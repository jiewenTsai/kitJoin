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
#' @param user_na SAV／DTA 的使用者自訂遺漏值（例如 99 = 拒答）：FALSE（預設）讀進來就是空白（NA）；
#'   TRUE 保留原來的值與遺漏值設定（haven 的 labelled_spss），到「遺漏值處理」再決定要不要轉成空白，
#'   匯出 SAV 時也會留著
#' @return list(data, encoding, n_problems)；encoding 為 "UTF-8"、"BIG5"、
#'   "Latin1"（可能是亂碼）或 NA（SAV、Excel、Stata 依檔案標示讀取）
#' @noRd
read_data_file <- function(path, name, sav_encoding = "auto", user_na = FALSE) {
  ext <- tolower(tools::file_ext(name))
  if (ext == "csv") {
    return(read_csv_auto(path))
  }
  if (ext %in% c("xlsx", "xls")) {
    return(read_excel_first(path, name))
  }
  if (ext == "dta") {
    data <- tryCatch(
      haven::read_dta(path),
      error = function(e) {
        # 原文：無法讀取  | ：檔案可能已損壞，或不是 Stata 檔。請用 Stata 開啟確認後重新存檔再上傳。
        kit_abort("\u7121\u6cd5\u8b80\u53d6 ", name, "\uff1a\u6a94\u6848\u53ef\u80fd\u5df2\u640d\u58de\uff0c\u6216\u4e0d\u662f Stata \u6a94\u3002\u8acb\u7528 Stata \u958b\u555f\u78ba\u8a8d\u5f8c\u91cd\u65b0\u5b58\u6a94\u518d\u4e0a\u50b3\u3002")
      }
    )
    if (!isTRUE(user_na)) data <- haven::zap_missing(data)
    return(list(data = kit_fix_utf8_df(data), encoding = NA_character_, n_problems = 0L))
  }
  if (ext %in% c("sav", "zsav")) {
    read_sav_as <- function(encoding) {
      tryCatch(
        haven::read_sav(path, encoding = encoding, user_na = isTRUE(user_na)),
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
        alt <- tryCatch(haven::read_sav(path, encoding = "CP950", user_na = isTRUE(user_na)),
                        error = function(e) NULL)
        if (!is.null(alt) && looks_like_chinese(sav_text_sample(alt))) {
          data <- alt
          encoding <- "BIG5"
        }
      }
    }
    return(list(
      # haven 不讀測量水準（名義、次序、尺度）：自己從表頭讀，掛在每一欄（R/sav_measure.R）
      data = sav_attach_measures(kit_fix_utf8_df(data), path),
      encoding = encoding,
      n_problems = 0L
    ))
  }
  kit_abort(
    # 原文：無法讀取  | ：目前只支援 CSV、Excel（.xlsx）、SPSS（.sav）與 Stata（.dta）檔。
    "\u7121\u6cd5\u8b80\u53d6 ", name, "\uff1a\u76ee\u524d\u53ea\u652f\u63f4 CSV\u3001Excel\uff08.xlsx\uff09\u3001SPSS\uff08.sav\uff09\u8207 Stata\uff08.dta\uff09\u6a94\u3002",
    if (ext == "gz") {
      # 原文：壓縮過的 CSV（.csv.gz）請先到「檔案格式轉換」解壓縮成 .csv 再上傳。
      "\u58d3\u7e2e\u904e\u7684 CSV\uff08.csv.gz\uff09\u8acb\u5148\u5230\u300c\u6a94\u6848\u683c\u5f0f\u8f49\u63db\u300d\u89e3\u58d3\u7e2e\u6210 .csv \u518d\u4e0a\u50b3\u3002"
    }
  )
}

#' 讀 Excel 的第 1 個工作表（欄位型別以全部列判斷）
#'
#' 有好幾個工作表時只讀第 1 個，提醒使用者；第 1 列當欄名。
#' @noRd
read_excel_first <- function(path, name) {
  sheets <- tryCatch(readxl::excel_sheets(path), error = function(e) {
    # 原文：無法讀取  | ：檔案可能已損壞，或不是 Excel 檔。請用 Excel 開啟確認後重新存檔再上傳。
    kit_abort("\u7121\u6cd5\u8b80\u53d6 ", name, "\uff1a\u6a94\u6848\u53ef\u80fd\u5df2\u640d\u58de\uff0c\u6216\u4e0d\u662f Excel \u6a94\u3002\u8acb\u7528 Excel \u958b\u555f\u78ba\u8a8d\u5f8c\u91cd\u65b0\u5b58\u6a94\u518d\u4e0a\u50b3\u3002")
  })
  data <- readxl::read_excel(path, sheet = 1, guess_max = 1e6, .name_repair = "unique_quiet")
  list(
    data = kit_fix_utf8_df(data),
    encoding = NA_character_,
    n_problems = 0L,
    sheets = sheets
  )
}

#' 匯出 Stata（.dta）：變項標籤、數值標籤、使用者自訂遺漏值都留著
#'
#' Stata 的欄名只能有英文字母、數字、底線，最多 32 個字元；不合的自動改名（和 SAV 一樣的規則再截短）。
#' @noRd
write_kit_dta <- function(df, file) {
  df <- kit_fix_utf8_df(df)
  names(df) <- dta_safe_names(names(df))
  # Stata 的數值標籤只能掛在整數上；文字欄的數值標籤拿掉
  for (col in names(df)) {
    x <- df[[col]]
    if (is.character(x) && !is.null(attr(x, "labels", exact = TRUE))) df[[col]] <- haven::zap_labels(x)
  }
  haven::write_dta(df, file, version = 15)
  invisible(file)
}

#' Codebook（資料字典）：每個欄位一列
#'
#' 欄位、變項標籤、型別、數值標籤（「1 = 是；2 = 否」）、使用者自訂遺漏值、有效筆數、遺漏筆數、
#' 不同的值有幾個、最小值、最大值。串接、疊加後的標籤都留著，所以可以拿來核對各波的設定。
#' @noRd
codebook_table <- function(df) {
  rows <- lapply(names(df), function(col) {
    x <- df[[col]]
    labels <- attr(x, "labels", exact = TRUE)
    na_values <- attr(x, "na_values", exact = TRUE)
    na_range <- attr(x, "na_range", exact = TRUE)
    raw <- if (haven::is.labelled(x)) as.vector(unclass(x)) else x
    missing <- is.na(x)
    num <- is.numeric(raw) && !inherits(x, c("Date", "POSIXt"))
    # 原文：日期
    type <- if (inherits(x, c("Date", "POSIXt"))) "\u65e5\u671f"
            # 原文：數字 | 是非 | 文字
            else if (num) "\u6578\u5b57" else if (is.logical(raw)) "\u662f\u975e" else "\u6587\u5b57"
    tibble::tibble(
      # 原文：欄位
      "\u6b04\u4f4d" = col,
      # 原文：變項標籤
      "\u8b8a\u9805\u6a19\u7c64" = as.character(attr(x, "label", exact = TRUE) %||% ""),
      # 原文：型別
      "\u578b\u5225" = type,
      # SPSS 的測量水準（讀 SAV 時記下的；其他檔案是空白）
      # 原文：測量水準
      "\u6e2c\u91cf\u6c34\u6e96" = if (is.na(col_measure(x))) "" else SAV_MEASURE_ZH[[col_measure(x)]],
      # 原文：數值標籤
      "\u6578\u503c\u6a19\u7c64" = if (length(labels) > 0) {
        # 原文：；
        paste(paste(num_to_text(unclass(labels)), "=", names(labels)), collapse = "\uff1b")
      } else "",
      # 原文：使用者自訂遺漏值
      "\u4f7f\u7528\u8005\u81ea\u8a02\u907a\u6f0f\u503c" = paste(c(
        # 原文：、
        if (length(na_values) > 0) paste(num_to_text(na_values), collapse = "\u3001"),
        # 原文：～
        if (length(na_range) == 2) paste0(num_to_text(na_range[1]), "\uff5e", num_to_text(na_range[2]))
      # 原文：、
      ), collapse = "\u3001"),
      # 原文：有效筆數
      "\u6709\u6548\u7b46\u6578" = sum(!missing),
      # 原文：遺漏筆數
      "\u907a\u6f0f\u7b46\u6578" = sum(missing),
      # 原文：不同的值
      "\u4e0d\u540c\u7684\u503c" = length(unique(raw[!missing])),
      # 原文：最小值
      "\u6700\u5c0f\u503c" = if (num && any(!missing)) num_to_text(min(raw[!missing])) else "",
      # 原文：最大值
      "\u6700\u5927\u503c" = if (num && any(!missing)) num_to_text(max(raw[!missing])) else ""
    )
  })
  dplyr::bind_rows(rows)
}

#' Stata 合法的欄名：英文字母或底線開頭，只有英數字與底線，最多 32 字元，不重複
#' @noRd
dta_safe_names <- function(cols) {
  new <- gsub("[^A-Za-z0-9_]", "_", cols)
  new <- ifelse(grepl("^[A-Za-z_]", new), new, paste0("v_", new))
  new <- substr(new, 1, 32)
  make.unique(new, sep = "_")
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
  # 測量水準：haven 寫完後再寫回去（讀進來時掛在欄上的 attr "measure"）
  measures <- purrr::map_chr(df, col_measure)
  # 文字欄在 SPSS 不能是尺度（例如疊加時數字和文字混在一起、統一成文字的欄）
  measures[purrr::map_lgl(df, is.character) & measures %in% "scale"] <- "nominal"
  haven::write_sav(df, file)
  if (sav_readable(file)) {
    sav_write_measures(file, measures)
    return(invisible(list(fixed = character(0), step = 0L)))
  }

  fixes <- list(clean_string_value_labels, drop_string_value_labels, drop_all_value_labels)
  for (step in seq_along(fixes)) {
    fixed <- fixes[[step]](df)
    haven::write_sav(fixed$df, file)
    if (sav_readable(file)) {
      sav_write_measures(file, measures)
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

