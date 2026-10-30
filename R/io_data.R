# 讀檔、預覽與匯出

#' 讀取 CSV 或 SAV
#'
#' CSV 自動判斷編碼（UTF-8 → CP950/BIG5 → Latin1）；SAV 保留變項與數值標籤。
#' SAV 沒有標明編碼、中文讀成亂碼時（舊版 SPSS 存的 BIG5 檔），自動改用 CP950 重讀。
#' 讀不了的檔案會以白話中文錯誤訊息 kit_abort()，由呼叫端顯示給使用者。
#'
#' @param path 檔案路徑
#' @param name 原始檔名（用來判斷副檔名）
#' @param sav_encoding SAV 的編碼："auto"（依檔案標示，亂碼時改用 BIG5）、
#'   "file"（只依檔案標示）或 "CP950"（一律用 BIG5）
#' @return list(data, encoding, n_problems)；encoding 為 "UTF-8"、"BIG5"、
#'   "Latin1"（可能是亂碼）或 NA（SAV 依檔案標示讀取）
#' @noRd
read_data_file <- function(path, name, sav_encoding = "auto") {
  ext <- tolower(tools::file_ext(name))
  if (ext == "csv") {
    return(read_csv_auto(path))
  }
  if (ext %in% c("sav", "zsav")) {
    read_sav_as <- function(encoding) {
      tryCatch(
        haven::read_sav(path, encoding = encoding),
        error = function(e) {
          kit_abort(
            "無法讀取 ", name, "：檔案可能已損壞，或不是 SPSS 檔。",
            "請用 SPSS 開啟確認後重新存檔再上傳。"
          )
        }
      )
    }
    encoding <- NA_character_
    if (identical(sav_encoding, "CP950")) {
      data <- read_sav_as("CP950")
      encoding <- "BIG5"
    } else {
      data <- read_sav_as(NULL)
      if (!identical(sav_encoding, "file") &&
            looks_like_big5_mojibake(sav_text_sample(data))) {
        alt <- tryCatch(haven::read_sav(path, encoding = "CP950"), error = function(e) NULL)
        if (!is.null(alt) && looks_like_chinese(sav_text_sample(alt))) {
          data <- alt
          encoding <- "BIG5"
        }
      }
    }
    return(list(
      data = fix_utf8_df(data),
      encoding = encoding,
      n_problems = 0L
    ))
  }
  kit_abort(
    "無法讀取 ", name, "：目前只支援 CSV 與 SPSS（.sav）檔。",
    if (ext %in% c("xlsx", "xls")) {
      "Excel 檔請先在 Excel 用「另存新檔 → CSV UTF-8（逗號分隔）」存成 CSV 再上傳。"
    } else if (ext == "gz") {
      "壓縮過的 CSV（.csv.gz）請先到「⑧ 檔案格式轉換」解壓縮成 .csv 再上傳。"
    }
  )
}

#' 空檔、只有表頭的檔給白話錯誤（① 上傳用；不要默默變成 0 列的資料）
#' @noRd
check_not_empty <- function(df, name) {
  if (ncol(df) == 0) {
    kit_abort("無法讀取 ", name, "：檔案是空的（沒有任何欄位）。請確認選對檔案。")
  }
  if (nrow(df) == 0) {
    kit_abort(
      "無法讀取 ", name, "：檔案只有欄名，沒有任何資料列（0 列）。",
      "請確認匯出時有包含資料。"
    )
  }
  invisible(TRUE)
}

#' SAV 中用來判斷編碼的文字：欄名、變項標籤、數值標籤、文字欄前 200 個值
#' @noRd
sav_text_sample <- function(df, n = 200L) {
  text <- c(names(df), unlist(lapply(df, function(x) {
    c(
      attr(x, "label", exact = TRUE),
      names(attr(x, "labels", exact = TRUE)),
      if (is.character(x)) utils::head(x[!is.na(x)], n)
    )
  }), use.names = FALSE))
  text[!is.na(text)]
}

count_matches <- function(text, pattern) {
  sum(lengths(regmatches(text, gregexpr(pattern, text, perl = TRUE))))
}

#' 看起來是被當成 Latin1 解讀的 BIG5（例如「¼Ë¥»¬y¤ô¥N½X」）
#'
#' BIG5 每個中文字是 2 個位元組，被當成 Latin1 時會變成成串的 U+0080–U+00FF 字元；
#' 歐洲語言的 ä、é 則是零星夾在英文字母中間。不是合法 UTF-8 的文字也算可疑。
#' @noRd
looks_like_big5_mojibake <- function(text) {
  if (length(text) == 0) return(FALSE)
  if (any(!validUTF8(text))) return(TRUE)
  if (count_matches(text, "[\\x{3400}-\\x{9FFF}]") > 0) return(FALSE)
  high <- count_matches(text, "[\\x{80}-\\x{FF}]")
  if (high < 4) return(FALSE)
  paired <- 2 * count_matches(text, "[\\x{80}-\\x{FF}]{2}")
  letters <- count_matches(text, "[A-Za-z]")
  paired / high >= 0.3 && high / (high + letters) >= 0.3
}

#' 非 ASCII 的字大多是中文字或全形標點
#' @noRd
looks_like_chinese <- function(text) {
  text <- text[validUTF8(text)]
  non_ascii <- count_matches(text, "[^\\x{00}-\\x{7F}]")
  cjk <- count_matches(
    text, "[\\x{3000}-\\x{303F}\\x{3400}-\\x{9FFF}\\x{FF00}-\\x{FFEF}]"
  )
  cjk >= 2 && cjk / non_ascii >= 0.8
}

#' 預覽用：轉成方便閱讀的文字
#'
#' 整數不顯示小數點、標籤欄顯示「值 標籤」、日期顯示 yyyy-mm-dd。
#' @noRd
df_for_display <- function(df, n = NULL) {
  if (!is.null(n)) df <- utils::head(df, n)
  out <- lapply(df, function(x) {
    if (haven::is.labelled(x) && length(attr(x, "labels", exact = TRUE)) > 0) {
      return(as.character(haven::as_factor(x, levels = "both")))
    }
    if (haven::is.labelled(x)) x <- as.vector(unclass(x))
    if (inherits(x, c("Date", "POSIXt", "hms"))) return(format(x))
    if (is.numeric(x)) return(as.character(round(x, 4)))
    as.character(x)
  })
  tibble::as_tibble(out)
}

#' 預覽用：欄位過多時只取前 max_cols 欄（優先保留指定欄）
#' @noRd
preview_df_cols <- function(
    df,
    max_cols = KIT_PREVIEW_COLS,
    priority_cols = NULL) {
  cols <- names(df)
  if (length(cols) <= max_cols) return(df)
  priority_cols <- intersect(priority_cols, cols)
  keep <- unique(c(priority_cols, setdiff(cols, priority_cols)))
  df[, keep[seq_len(max_cols)], drop = FALSE]
}

#' 匯出用：將 key 欄位移到最前
#' @noRd
reorder_key_first <- function(df, key_cols) {
  key_cols <- intersect(key_cols, names(df))
  if (length(key_cols) == 0) return(df)
  df[, c(key_cols, setdiff(names(df), key_cols)), drop = FALSE]
}

#' 匯出 CSV：遺漏值一律寫成空白儲存格（SPSS／Excel 才不會把數值欄當成文字）
#' @noRd
write_kit_csv <- function(df, file) {
  readr::write_excel_csv(fix_utf8_df(df), file, na = "")
}

#' 匯出壓縮過的 CSV（.csv.gz）：遺漏值寫成空白；kitsUI 等工具可直接讀
#'
#' readr 看路徑結尾決定要不要壓縮，所以先寫到 .csv.gz 結尾的暫存檔再複製。
#' @noRd
write_kit_csv_gz <- function(df, file) {
  tmp <- tempfile(fileext = ".csv.gz")
  on.exit(unlink(tmp), add = TRUE)
  readr::write_csv(fix_utf8_df(df), tmp, na = "")
  file.copy(tmp, file, overwrite = TRUE)
  invisible(file)
}

#' 匯出 SAV：不合法的 SPSS 變項名稱自動改名
#' @noRd
write_kit_sav <- function(df, file) {
  df <- fix_utf8_df(df)
  names(df) <- sav_safe_names(names(df))
  haven::write_sav(df, file)
}

#' 匯出 SAV 時會被改名的欄位（給 UI 事先提醒）
#' @return 具名字元向量：名稱為原欄名、值為新欄名；沒有要改的時候長度 0
#' @noRd
sav_renamed_cols <- function(cols) {
  new <- sav_safe_names(cols)
  changed <- new != cols
  stats::setNames(new[changed], cols[changed])
}

# SPSS 保留字，不能當變項名稱（不分大小寫）
SPSS_RESERVED <- c(
  "ALL", "AND", "BY", "EQ", "GE", "GT", "LE", "LT", "NE", "NOT", "OR", "TO",
  "WITH"
)

#' 轉成合法的 SPSS 變項名稱
#'
#' 規則（以 haven::write_sav() 實測）：只能有字母、數字與 _ . @ # $；
#' 開頭須為字母或 @；不能以 . 結尾；不能是保留字；最長 64 bytes；
#' 不分大小寫不可重複。
#' @noRd
sav_safe_names <- function(cols) {
  out <- gsub("[^[:alnum:]_.@#$]", "_", cols)
  out <- sub("\\.+$", "", out)
  needs_prefix <- !grepl("^[[:alpha:]@]", out)
  out[needs_prefix] <- paste0("v_", out[needs_prefix])
  reserved <- toupper(out) %in% SPSS_RESERVED
  out[reserved] <- paste0(out[reserved], "_")
  out <- vapply(
    out,
    truncate_bytes,
    character(1),
    max_bytes = 64L,
    USE.NAMES = FALSE
  )

  # 不分大小寫的重複：第二個起加上 _2、_3…
  for (i in seq_along(out)) {
    taken <- toupper(out[seq_len(i - 1)])
    if (!toupper(out[i]) %in% taken) next
    k <- 2L
    repeat {
      tail <- paste0("_", k)
      candidate <- paste0(
        truncate_bytes(out[i], 64L - nchar(tail, "bytes")),
        tail
      )
      if (!toupper(candidate) %in% c(taken, toupper(out[-seq_len(i)]))) break
      k <- k + 1L
    }
    out[i] <- candidate
  }
  out
}

#' 依 UTF-8 位元組數截斷（不切斷中文字）
#' @noRd
truncate_bytes <- function(x, max_bytes) {
  while (nchar(x, "bytes") > max_bytes) x <- substr(x, 1, nchar(x) - 1)
  x
}


# ── 內部工具 ─────────────────────────────────────────────────────────────────

#' 依序嘗試 UTF-8、CP950（BIG5）；都不是合法中文時退回 Latin1 並標記
#' @noRd
read_csv_auto <- function(path) {
  try_enc <- function(enc) {
    # guess_max = Inf：用全部列判斷欄位型別，避免後段的文字值被默默變成 NA
    readr::read_csv(
      path,
      show_col_types = FALSE,
      guess_max = Inf,
      locale = readr::locale(encoding = enc),
      progress = FALSE
    )
  }
  # readr 以 UTF-8 讀非 UTF-8 檔不會報錯，需檢查結果是否為合法 UTF-8；
  # CP950 為 BIG5 超集（Windows 繁中 Excel 另存的 CSV）
  encodings <- c("UTF-8" = "UTF-8", BIG5 = "CP950")
  for (label in names(encodings)) {
    df <- tryCatch(try_enc(encodings[[label]]), error = function(e) NULL)
    if (!is.null(df) && is_valid_utf8_df(df)) {
      return(csv_result(df, label))
    }
  }
  df <- tryCatch(
    try_enc("Latin1"),
    error = function(e) {
      kit_abort(
        "無法讀取這個 CSV 檔，檔案可能不是文字格式的 CSV。",
        "請用 Excel 開啟確認後另存為「CSV UTF-8」再上傳。"
      )
    }
  )
  csv_result(df, "Latin1")
}

csv_result <- function(df, encoding) {
  n_problems <- nrow(readr::problems(df))
  list(
    data = fix_utf8_df(df),
    encoding = encoding,
    n_problems = as.integer(n_problems)
  )
}

is_valid_utf8_df <- function(df) {
  chr_ok <- vapply(df, function(x) {
    !is.character(x) || all(validUTF8(x[!is.na(x)]))
  }, logical(1))
  all(validUTF8(names(df))) && all(chr_ok)
}

fix_utf8_chr <- function(x) {
  if (!is.character(x)) return(x)
  iconv(x, from = "", to = "UTF-8", sub = "?")
}

#' 文字欄轉成 UTF-8；保留 SAV 的變項標籤與數值標籤
#' @noRd
fix_utf8_df <- function(df) {
  dplyr::mutate(df, dplyr::across(dplyr::everything(), function(x) {
    if (is.factor(x)) {
      label <- attr(x, "label", exact = TRUE)
      x <- fix_utf8_chr(as.character(x))
      attr(x, "label") <- label
      return(x)
    }
    if (!is.character(x)) return(x)
    # iconv() 會清掉屬性（含 haven 的 class 與標籤），轉完再放回去
    attrs <- attributes(x)
    x <- fix_utf8_chr(as.vector(unclass(x)))
    attributes(x) <- attrs
    x
  }))
}
