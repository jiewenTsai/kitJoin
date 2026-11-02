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
            # 原文：無法讀取  | ：檔案可能已損壞，或不是 SPSS 檔。
            "\u7121\u6cd5\u8b80\u53d6 ", name, "\uff1a\u6a94\u6848\u53ef\u80fd\u5df2\u640d\u58de\uff0c\u6216\u4e0d\u662f SPSS \u6a94\u3002",
            # 原文：請用 SPSS 開啟確認後重新存檔再上傳。
            "\u8acb\u7528 SPSS \u958b\u555f\u78ba\u8a8d\u5f8c\u91cd\u65b0\u5b58\u6a94\u518d\u4e0a\u50b3\u3002"
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
      data = kit_fix_utf8_df(data),
      encoding = encoding,
      n_problems = 0L
    ))
  }
  kit_abort(
    # 原文：無法讀取  | ：目前只支援 CSV 與 SPSS（.sav）檔。
    "\u7121\u6cd5\u8b80\u53d6 ", name, "\uff1a\u76ee\u524d\u53ea\u652f\u63f4 CSV \u8207 SPSS\uff08.sav\uff09\u6a94\u3002",
    if (ext %in% c("xlsx", "xls")) {
      # 原文：Excel 檔請先在 Excel 用「另存新檔 → CSV UTF-8（逗號分隔）」存成 CSV 再上傳。
      "Excel \u6a94\u8acb\u5148\u5728 Excel \u7528\u300c\u53e6\u5b58\u65b0\u6a94 \u2192 CSV UTF-8\uff08\u9017\u865f\u5206\u9694\uff09\u300d\u5b58\u6210 CSV \u518d\u4e0a\u50b3\u3002"
    } else if (ext == "gz") {
      # 原文：壓縮過的 CSV（.csv.gz）請先到「檔案格式轉換」解壓縮成 .csv 再上傳。
      "\u58d3\u7e2e\u904e\u7684 CSV\uff08.csv.gz\uff09\u8acb\u5148\u5230\u300c\u6a94\u6848\u683c\u5f0f\u8f49\u63db\u300d\u89e3\u58d3\u7e2e\u6210 .csv \u518d\u4e0a\u50b3\u3002"
    }
  )
}

#' 空檔、只有表頭的檔給白話錯誤（① 上傳用；不要默默變成 0 列的資料）
#' @noRd
check_not_empty <- function(df, name) {
  if (ncol(df) == 0) {
    # 原文：無法讀取  | ：檔案是空的（沒有任何欄位）。請確認選對檔案。
    kit_abort("\u7121\u6cd5\u8b80\u53d6 ", name, "\uff1a\u6a94\u6848\u662f\u7a7a\u7684\uff08\u6c92\u6709\u4efb\u4f55\u6b04\u4f4d\uff09\u3002\u8acb\u78ba\u8a8d\u9078\u5c0d\u6a94\u6848\u3002")
  }
  if (nrow(df) == 0) {
    kit_abort(
      # 原文：無法讀取  | ：檔案只有欄名，沒有任何資料列（0 列）。
      "\u7121\u6cd5\u8b80\u53d6 ", name, "\uff1a\u6a94\u6848\u53ea\u6709\u6b04\u540d\uff0c\u6c92\u6709\u4efb\u4f55\u8cc7\u6599\u5217\uff080 \u5217\uff09\u3002",
      # 原文：請確認匯出時有包含資料。
      "\u8acb\u78ba\u8a8d\u532f\u51fa\u6642\u6709\u5305\u542b\u8cc7\u6599\u3002"
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
  # (*UTF)：文字全是 ASCII 時 PCRE 不會自動切到 UTF-8 模式，\\x{9FFF} 會變成不合法
  hits <- gregexpr(paste0("(*UTF)", pattern), text, perl = TRUE)
  sum(vapply(hits, function(h) sum(h > 0), integer(1)))
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
  readr::write_excel_csv(kit_fix_utf8_df(df), file, na = "")
}

#' 匯出壓縮過的 CSV（.csv.gz）：遺漏值寫成空白；kitsUI 等工具可直接讀
#'
#' readr 看路徑結尾決定要不要壓縮，所以先寫到 .csv.gz 結尾的暫存檔再複製。
#' @noRd
write_kit_csv_gz <- function(df, file) {
  tmp <- tempfile(fileext = ".csv.gz")
  on.exit(unlink(tmp), add = TRUE)
  readr::write_csv(kit_fix_utf8_df(df), tmp, na = "")
  file.copy(tmp, file, overwrite = TRUE)
  invisible(file)
}

#' 匯出 SAV：不合法的 SPSS 變項名稱自動改名；寫完讀回來驗證
#'
#' haven 寫某些文字欄的數值標籤（例如值是「99 + 78 個空白 + 控制字元」）時，
#' 寫出的檔自己讀不回來。寫完先用 read_sav() 讀表頭；讀不回來就依序：
#' 1. 文字欄的數值標籤去掉控制字元與前後空白；
#' 2. 移除文字欄的數值標籤；
#' 3. 移除所有數值標籤；
#' 每一步重寫一次，讀得回來就停。
#'
#' @return invisible(list(fixed = 被修改數值標籤的欄（原欄名）, step = 0–3))
#' @noRd
write_kit_sav <- function(df, file) {
  df <- kit_fix_utf8_df(df)
  original <- names(df)
  names(df) <- kit_sav_safe_names(names(df))
  haven::write_sav(df, file)
  if (sav_readable(file)) return(invisible(list(fixed = character(0), step = 0L)))

  fixes <- list(clean_string_value_labels, drop_string_value_labels, drop_all_value_labels)
  for (step in seq_along(fixes)) {
    fixed <- fixes[[step]](df)
    haven::write_sav(fixed$df, file)
    if (sav_readable(file)) {
      return(invisible(list(fixed = original[match(fixed$cols, names(df))], step = step)))
    }
  }
  # 原文：SAV 檔寫出後無法讀回，可能是資料中有 SPSS 不支援的內容。請改下載 CSV。
  kit_abort("SAV \u6a94\u5beb\u51fa\u5f8c\u7121\u6cd5\u8b80\u56de\uff0c\u53ef\u80fd\u662f\u8cc7\u6599\u4e2d\u6709 SPSS \u4e0d\u652f\u63f4\u7684\u5167\u5bb9\u3002\u8acb\u6539\u4e0b\u8f09 CSV\u3002")
}

#' 寫出的 SAV 讀得回來嗎（只讀表頭與第 1 列；數值標籤在表頭）
#' @noRd
sav_readable <- function(file) {
  tryCatch({
    haven::read_sav(file, n_max = 1)
    TRUE
  }, error = function(e) FALSE)
}

#' 文字欄的數值標籤：值去掉控制字元與前後空白；去掉後重複或變空白的就移除
#' @return list(df, cols = 有改到的欄)
#' @noRd
clean_string_value_labels <- function(df) {
  cols <- character(0)
  for (col in names(df)) {
    x <- df[[col]]
    labels <- attr(x, "labels", exact = TRUE)
    if (!is.character(x) || is.null(labels)) next
    new <- trimws(gsub("[[:cntrl:]]", "", unclass(labels)))
    keep <- nzchar(new) & !duplicated(new)
    cleaned <- stats::setNames(new[keep], names(labels)[keep])
    if (identical(unname(cleaned), unname(unclass(labels))) &&
          identical(names(cleaned), names(labels))) next
    attr(x, "labels") <- if (length(cleaned) > 0) cleaned
    df[[col]] <- if (length(cleaned) > 0) x else haven::zap_labels(x)
    cols <- c(cols, col)
  }
  list(df = df, cols = cols)
}

#' 移除文字欄的數值標籤（保留變項標籤）
#' @noRd
drop_string_value_labels <- function(df) {
  cols <- names(df)[vapply(df, function(x) {
    is.character(x) && !is.null(attr(x, "labels", exact = TRUE))
  }, logical(1))]
  for (col in cols) df[[col]] <- haven::zap_labels(df[[col]])
  list(df = df, cols = cols)
}

#' 移除所有數值標籤（保留變項標籤）
#' @noRd
drop_all_value_labels <- function(df) {
  cols <- names(df)[vapply(df, function(x) {
    !is.null(attr(x, "labels", exact = TRUE))
  }, logical(1))]
  for (col in cols) df[[col]] <- haven::zap_labels(df[[col]])
  list(df = df, cols = cols)
}

#' SAV 修改了數值標籤時，給使用者看的說明；沒有修改時為 NULL
#' @noRd
sav_fix_message <- function(written) {
  cols <- written$fixed
  if (is.null(cols) || length(cols) == 0) return(NULL)
  what <- switch(as.character(written$step),
    # 原文：去掉了這些文字欄數值標籤裡的控制字元與多餘空白
    "1" = "\u53bb\u6389\u4e86\u9019\u4e9b\u6587\u5b57\u6b04\u6578\u503c\u6a19\u7c64\u88e1\u7684\u63a7\u5236\u5b57\u5143\u8207\u591a\u9918\u7a7a\u767d",
    # 原文：移除了這些文字欄的數值標籤
    "2" = "\u79fb\u9664\u4e86\u9019\u4e9b\u6587\u5b57\u6b04\u7684\u6578\u503c\u6a19\u7c64",
    # 原文：移除了這些欄的數值標籤
    "\u79fb\u9664\u4e86\u9019\u4e9b\u6b04\u7684\u6578\u503c\u6a19\u7c64"
  )
  paste0(
    # 原文：SAV 檔照原樣寫出後讀不回來（原檔的數值標籤有 SPSS 存不了的內容），
    "SAV \u6a94\u7167\u539f\u6a23\u5beb\u51fa\u5f8c\u8b80\u4e0d\u56de\u4f86\uff08\u539f\u6a94\u7684\u6578\u503c\u6a19\u7c64\u6709 SPSS \u5b58\u4e0d\u4e86\u7684\u5167\u5bb9\uff09\uff0c",
    # 原文：所以 | 再重新寫出：
    "\u6240\u4ee5", what, "\u518d\u91cd\u65b0\u5beb\u51fa\uff1a",
    # 原文：、
    paste(utils::head(cols, 10), collapse = "\u3001"),
    # 原文： 等  |  欄
    if (length(cols) > 10) paste0(" \u7b49 ", kit_num(length(cols)), " \u6b04") else "",
    # 原文：。資料值與變項標籤都沒有改。
    "\u3002\u8cc7\u6599\u503c\u8207\u8b8a\u9805\u6a19\u7c64\u90fd\u6c92\u6709\u6539\u3002"
  )
}

#' 匯出 SAV 時會被改名的欄位（給 UI 事先提醒）
#' @return 具名字元向量：名稱為原欄名、值為新欄名；沒有要改的時候長度 0
#' @noRd
sav_renamed_cols <- function(cols) {
  new <- kit_sav_safe_names(cols)
  changed <- new != cols
  stats::setNames(new[changed], cols[changed])
}


# ── 內部工具 ─────────────────────────────────────────────────────────────────

#' 依序嘗試 UTF-8、CP950（BIG5）；都不是合法中文時退回 Latin1 並標記
#' @noRd
read_csv_auto <- function(path) {
  delim <- guess_csv_delim(path)
  try_enc <- function(enc) {
    # guess_max = Inf：用全部列判斷欄位型別，避免後段的文字值被默默變成 NA
    readr::read_delim(
      path,
      delim = delim,
      trim_ws = TRUE,   # 和 read_csv() 相同（read_delim() 預設不去空白）
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
    if (!is.null(df) && kit_is_valid_utf8_df(df)) {
      return(csv_result(df, label, delim))
    }
  }
  df <- tryCatch(
    try_enc("Latin1"),
    error = function(e) {
      kit_abort(
        # 原文：無法讀取這個 CSV 檔，檔案可能不是文字格式的 CSV。
        "\u7121\u6cd5\u8b80\u53d6\u9019\u500b CSV \u6a94\uff0c\u6a94\u6848\u53ef\u80fd\u4e0d\u662f\u6587\u5b57\u683c\u5f0f\u7684 CSV\u3002",
        # 原文：請用 Excel 開啟確認後另存為「CSV UTF-8」再上傳。
        "\u8acb\u7528 Excel \u958b\u555f\u78ba\u8a8d\u5f8c\u53e6\u5b58\u70ba\u300cCSV UTF-8\u300d\u518d\u4e0a\u50b3\u3002"
      )
    }
  )
  csv_result(df, "Latin1", delim)
}

csv_result <- function(df, encoding, delim = ",") {
  n_problems <- nrow(readr::problems(df))
  list(
    data = kit_fix_utf8_df(df),
    encoding = encoding,
    n_problems = as.integer(n_problems),
    delim = delim
  )
}

#' CSV 的分隔符號：第一列（欄名）沒有逗號、但有分號或 Tab 時，改用那個符號
#'
#' 歐洲版 Excel 存的 CSV 用分號；從其他軟體複製的常是 Tab。
#' @noRd
guess_csv_delim <- function(path) {
  first <- tryCatch({
    con <- file(path, "rb")
    on.exit(close(con))
    line <- readLines(con, n = 1, warn = FALSE)
    if (length(line) == 0) "" else line
  }, error = function(e) "")
  count <- function(ch) lengths(regmatches(first, gregexpr(ch, first, fixed = TRUE, useBytes = TRUE)))
  if (count(",") > 0) return(",")
  others <- c(";" = count(";"), "\t" = count("\t"))
  if (max(others) == 0) return(",")
  names(others)[which.max(others)]
}

