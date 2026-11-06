# 檔案格式轉換：CSV、CSV.GZ、SAV 互轉（不碰 Shiny）
#
# CSV 與 CSV.GZ 之間不解析成資料框：逐段複製位元組（BIG5 則逐段轉成 UTF-8），
# 大檔也只佔一小段記憶體。只有輸入或輸出是 SAV 時才把整個檔案讀成資料框。

# 三種格式：值為副檔名，名稱為給使用者看的說明
KIT_CONVERT_FORMATS <- c(
  # 原文：CSV（.csv，可用 Excel 開啟）
  "CSV\uff08.csv\uff0c\u53ef\u7528 Excel \u958b\u555f\uff09" = "csv",
  # 原文：CSV.GZ（.csv.gz，檔案較小，可上傳 kitsUI）
  "CSV.GZ\uff08.csv.gz\uff0c\u6a94\u6848\u8f03\u5c0f\uff0c\u53ef\u4e0a\u50b3 kitsUI\uff09" = "csv.gz",
  # 原文：SPSS（.sav）
  "SPSS\uff08.sav\uff09" = "sav"
)

# 逐段處理時每次讀多少：位元組數（直接複製）與列數（轉編碼、判斷編碼）
KIT_CONVERT_CHUNK_BYTES <- 1024^2
KIT_CONVERT_CHUNK_LINES <- 20000L

# UTF-8 的 BOM：輸出 .csv 時寫在開頭，Excel 才會用 UTF-8 開
KIT_UTF8_BOM <- as.raw(c(0xef, 0xbb, 0xbf))

#' 檔案格式：依檔名判斷 "csv"、"csv.gz"、"sav"；不支援的格式為 NA
#'
#' .gz 一律當成 CSV.GZ。
#' @noRd
convert_kind <- function(name) {
  n <- tolower(name)
  dplyr::case_when(
    grepl("[.]csv$", n) ~ "csv",
    grepl("[.]gz$", n) ~ "csv.gz",
    grepl("[.]z?sav$", n) ~ "sav",
    .default = NA_character_
  )
}

#' 可以轉成的格式：另外兩種（不列出原檔的格式）
#' @noRd
convert_targets <- function(kind) {
  if (length(kind) == 0 || is.na(kind)) return(KIT_CONVERT_FORMATS)
  KIT_CONVERT_FORMATS[KIT_CONVERT_FORMATS != kind]
}

#' 輸出檔名：去掉原本的副檔名，換成新的（wave1.csv.gz → wave1.sav）
#' @noRd
convert_out_name <- function(name, target) {
  base <- sub("[.](csv[.]gz|csv|gz|zsav|sav)$", "", basename(name), ignore.case = TRUE)
  if (!nzchar(base)) base <- "data"
  paste0(base, ".", target)
}

#' 檔案大小的白話寫法：850 B、12.3 KB、4.5 MB
#' @noRd
format_bytes <- function(bytes) {
  vapply(bytes, function(b) {
    # 原文：—
    if (is.na(b)) return("\u2014")
    units <- c("B", "KB", "MB", "GB")
    i <- min(max(floor(log(max(b, 1), 1024)), 0), length(units) - 1)
    digits <- if (i == 0) 0 else 1
    paste(formatC(b / 1024^i, format = "f", digits = digits), units[i + 1])
  }, character(1), USE.NAMES = FALSE)
}


# ── CSV ↔ CSV.GZ：逐段處理，不解析 ──────────────────────────────────────────

#' 開啟輸入檔：CSV.GZ 用 gzfile() 邊讀邊解壓縮
#' @noRd
convert_open_in <- function(path, kind, mode = "rb") {
  if (kind == "csv.gz") gzfile(path, mode) else file(path, mode)
}

#' 開啟輸出檔：CSV.GZ 用 gzfile() 邊寫邊壓縮
#' @noRd
convert_open_out <- function(path, target) {
  if (target == "csv.gz") gzfile(path, "wb") else file(path, "wb")
}

#' 判斷 CSV 編碼（逐段讀，不整個放進記憶體）
#'
#' 每一列都是合法 UTF-8 → "UTF-8"；否則每一列都能用 CP950 解讀 → "BIG5"；
#' 兩者都不是 → NA。只有英數字的檔案算 UTF-8。
#' @noRd
detect_csv_encoding <- function(path, kind) {
  con <- convert_open_in(path, kind, "r")
  on.exit(close(con))
  utf8 <- TRUE
  big5 <- TRUE
  repeat {
    lines <- readLines(con, n = KIT_CONVERT_CHUNK_LINES, warn = FALSE)
    if (length(lines) == 0) break
    if (utf8) utf8 <- all(validUTF8(lines))
    if (big5) big5 <- !anyNA(iconv(lines, "CP950", "UTF-8"))
    if (!utf8 && !big5) break
  }
  if (utf8) "UTF-8" else if (big5) "BIG5" else NA_character_
}

#' 逐段複製位元組：CSV ↔ CSV.GZ 只差在壓縮與否，內容一個位元組都不變
#'
#' @param bom 原檔開頭沒有 BOM 時是否補上（UTF-8 檔輸出 .csv 時補，Excel 開中文才不會亂碼）
#' @return 檔案的列數（換行字元數；最後一列沒有換行也算一列）
#' @noRd
stream_copy <- function(path, kind, out, target, bom = FALSE) {
  in_con <- convert_open_in(path, kind)
  on.exit(close(in_con))
  out_con <- convert_open_out(out, target)
  on.exit(close(out_con), add = TRUE)
  n_lines <- 0
  last <- NULL
  first <- TRUE
  repeat {
    chunk <- readBin(in_con, "raw", KIT_CONVERT_CHUNK_BYTES)
    if (length(chunk) == 0) break
    if (first) {
      first <- FALSE
      has_bom <- length(chunk) >= 3 && identical(chunk[1:3], KIT_UTF8_BOM)
      if (bom && !has_bom) writeBin(KIT_UTF8_BOM, out_con)
    }
    n_lines <- n_lines + sum(chunk == as.raw(10L))
    last <- chunk[length(chunk)]
    writeBin(chunk, out_con)
  }
  n_lines + (!is.null(last) && last != as.raw(10L))
}

#' 逐段把 BIG5 轉成 UTF-8（一次處理一段列，不解析欄位）
#'
#' @param bom 是否在開頭加 BOM（輸出 .csv 時加，Excel 開中文才不會亂碼）
#' @return 檔案的列數
#' @noRd
stream_big5_to_utf8 <- function(path, kind, out, target, bom = FALSE) {
  in_con <- convert_open_in(path, kind, "r")
  on.exit(close(in_con))
  out_con <- convert_open_out(out, target)
  on.exit(close(out_con), add = TRUE)
  if (bom) writeBin(KIT_UTF8_BOM, out_con)
  n_lines <- 0
  repeat {
    lines <- readLines(in_con, n = KIT_CONVERT_CHUNK_LINES, warn = FALSE)
    if (length(lines) == 0) break
    utf8 <- iconv(lines, "CP950", "UTF-8")
    writeBin(charToRaw(paste0(utf8, "\n", collapse = "")), out_con)
    n_lines <- n_lines + length(lines)
  }
  n_lines
}

#' 讀 CSV 的第一列（欄名）算欄數；不讀其他列
#' @noRd
csv_header_ncol <- function(path, kind, encoding) {
  length(csv_header_names(path, kind, encoding))
}

#' 讀 CSV 的第一列（欄名）；不讀其他列
#'
#' @param encoding "BIG5" 時轉成 UTF-8；NULL 時看這一列是不是合法 UTF-8，不是就當 BIG5
#' @noRd
csv_header_names <- function(path, kind, encoding = NULL) {
  con <- convert_open_in(path, kind, "r")
  on.exit(close(con))
  first <- readLines(con, n = 1, warn = FALSE)
  if (length(first) == 0) return(character(0))
  if (is.null(encoding)) encoding <- if (validUTF8(first)) "UTF-8" else "BIG5"
  if (identical(encoding, "BIG5")) first <- iconv(first, "CP950", "UTF-8")
  first <- sub("^\ufeff", "", first, useBytes = TRUE)
  names(readr::read_csv(I(paste0(first, "\n")), n_max = 0, show_col_types = FALSE,
                        name_repair = "minimal"))
}

#' CSV ↔ CSV.GZ
#'
#' UTF-8 檔逐段複製位元組；BIG5 檔逐段轉成 UTF-8。輸出 .csv 時開頭補 BOM
#' （原檔已有就不重複加）；編碼無法判斷時照原樣複製（不加 BOM）並提醒。
#' @return list(nrow, ncol, notes)
#' @noRd
convert_csv_stream <- function(path, kind, out, target) {
  enc <- detect_csv_encoding(path, kind)
  notes <- character(0)
  if (identical(enc, "BIG5")) {
    n_lines <- stream_big5_to_utf8(path, kind, out, target, bom = target == "csv")
    # 原文：原檔是 BIG5 編碼，已轉成 UTF-8。
    notes <- "\u539f\u6a94\u662f BIG5 \u7de8\u78bc\uff0c\u5df2\u8f49\u6210 UTF-8\u3002"
  } else {
    n_lines <- stream_copy(path, kind, out, target, bom = target == "csv" && identical(enc, "UTF-8"))
    if (is.na(enc)) {
      # 原文：無法判斷編碼（不是 UTF-8 也不是 BIG5），內容照原樣保留，中文可能是亂碼。
      notes <- "\u7121\u6cd5\u5224\u65b7\u7de8\u78bc\uff08\u4e0d\u662f UTF-8 \u4e5f\u4e0d\u662f BIG5\uff09\uff0c\u5167\u5bb9\u7167\u539f\u6a23\u4fdd\u7559\uff0c\u4e2d\u6587\u53ef\u80fd\u662f\u4e82\u78bc\u3002"
    }
  }
  list(
    nrow = max(n_lines - 1, 0),
    ncol = csv_header_ncol(path, kind, enc),
    notes = notes
  )
}


# ── 與 SAV 有關的轉換：要讀成資料框 ─────────────────────────────────────────

#' 讀成資料框再寫出（輸入或輸出是 SAV 時）
#'
#' CSV 與 CSV.GZ 用 ① 的讀法（UTF-8／BIG5 自動判斷，以 0 開頭的 ID 保留為文字）；
#' SAV 的 SPSS 自訂遺漏值讀成 NA。
#' @return list(nrow, ncol, notes)
#' @noRd
convert_df <- function(path, name, kind, out, target, labels_as_text = FALSE, dict = NULL) {
  read <- if (kind == "csv.gz") {
    read_csv_auto(kit_gz_path(path))
  } else {
    read_data_file(path, name, user_na = kind == "sav")
  }
  data <- read$data
  notes <- character(0)
  dict_out <- NULL
  if (kind == "sav") {
    # 變項檔要在自訂遺漏值變成空白之前做，才留得住 na_values
    dict_out <- file.path(dirname(out), convert_dict_name(basename(out)))
    write_kit_csv(sav_dictionary(data), dict_out)
    data <- haven::zap_missing(data)
  }
  if (!is.null(dict) && target == "sav") {
    applied <- apply_variables(data, read_variables_file(dict$path, dict$name), dict$name)
    data <- applied$data
    notes <- c(notes, applied$notes)
  }
  # 原文：原檔是 BIG5 編碼，已轉成 UTF-8。
  if (identical(read$encoding, "BIG5")) notes <- "\u539f\u6a94\u662f BIG5 \u7de8\u78bc\uff0c\u5df2\u8f49\u6210 UTF-8\u3002"
  if (identical(read$encoding, "Latin1")) {
    # 原文：無法判斷編碼（不是 UTF-8 也不是 BIG5），中文可能是亂碼。
    notes <- "\u7121\u6cd5\u5224\u65b7\u7de8\u78bc\uff08\u4e0d\u662f UTF-8 \u4e5f\u4e0d\u662f BIG5\uff09\uff0c\u4e2d\u6587\u53ef\u80fd\u662f\u4e82\u78bc\u3002"
  }
  if (kind == "sav") {
    data <- if (labels_as_text) {
      haven::as_factor(data, levels = "default")
    } else {
      haven::zap_labels(data)
    }
    notes <- c(notes, paste0(
      # 原文：CSV 本身不存標籤：變項標籤、數值標籤與自訂遺漏值寫在變項檔 
      "CSV \u672c\u8eab\u4e0d\u5b58\u6a19\u7c64\uff1a\u8b8a\u9805\u6a19\u7c64\u3001\u6578\u503c\u6a19\u7c64\u8207\u81ea\u8a02\u907a\u6f0f\u503c\u5beb\u5728\u8b8a\u9805\u6a94 ", basename(dict_out),
      # 原文：（labelled::generate_dictionary() 的格式），轉回 SAV 時一起上傳就能貼回去；資料檔裡自訂遺漏值已變成空白。
      "\uff08labelled::generate_dictionary() \u7684\u683c\u5f0f\uff09\uff0c\u8f49\u56de SAV \u6642\u4e00\u8d77\u4e0a\u50b3\u5c31\u80fd\u8cbc\u56de\u53bb\uff1b\u8cc7\u6599\u6a94\u88e1\u81ea\u8a02\u907a\u6f0f\u503c\u5df2\u8b8a\u6210\u7a7a\u767d\u3002"
    ))
  }
  if (target == "sav") {
    renamed <- sav_renamed_cols(names(data))
    if (length(renamed) > 0) {
      notes <- c(notes, paste0(
        # 原文：有  |  個直欄名稱不符合 SPSS 規則，已改名：
        "\u6709 ", kit_num(length(renamed)), " \u500b\u76f4\u6b04\u540d\u7a31\u4e0d\u7b26\u5408 SPSS \u898f\u5247\uff0c\u5df2\u6539\u540d\uff1a",
        # 原文：→ | 、 | 。
        paste(paste(names(renamed), "\u2192", renamed), collapse = "\u3001"), "\u3002"
      ))
    }
  }
  written <- switch(target,
    csv = write_kit_csv(data, out),
    csv.gz = write_kit_csv_gz(data, out),
    sav = write_kit_sav(data, out)
  )
  # SAV 寫完讀不回來而修改了數值標籤時，寫進說明
  if (target == "sav") notes <- c(notes, sav_fix_message(written))
  list(nrow = nrow(data), ncol = ncol(data), notes = notes, dict_path = dict_out)
}


# ── 主函式 ───────────────────────────────────────────────────────────────────

#' 轉換一個檔案
#'
#' CSV ↔ CSV.GZ 逐段處理不解析；有 SAV 的轉換讀成資料框。
#'
#' @param path 上傳檔的暫存路徑
#' @param name 原始檔名
#' @param target 輸出格式："csv"、"csv.gz" 或 "sav"（須與原檔格式不同）
#' @param out_dir 輸出資料夾
#' @param labels_as_text SAV 轉 CSV 時，數值標籤是否轉成文字（haven::as_factor()）；
#'   預設保留代碼
#' @param dict CSV 轉 SAV 時的變項檔：list(path, name)；NULL 表示不貼標籤
#' @return list(name, out_name, out_path, nrow, ncol, size_before, size_after, notes,
#'   dict_name, dict_path)；dict_* 是 SAV 轉 CSV 時一起輸出的變項檔（其他轉換為 NULL）
#' @noRd
convert_file <- function(path, name, target, out_dir, labels_as_text = FALSE, dict = NULL) {
  kind <- convert_kind(name)
  if (is.na(kind)) {
    kit_abort(
      # 原文：無法轉換  | ：只支援 CSV（.csv）、CSV.GZ（.csv.gz、.gz）與 SPSS（.sav）檔。
      "\u7121\u6cd5\u8f49\u63db ", name, "\uff1a\u53ea\u652f\u63f4 CSV\uff08.csv\uff09\u3001CSV.GZ\uff08.csv.gz\u3001.gz\uff09\u8207 SPSS\uff08.sav\uff09\u6a94\u3002",
      if (grepl("[.]xlsx?$", tolower(name))) {
        # 原文：Excel 檔請先在 Excel 用「另存新檔 → CSV UTF-8（逗號分隔）」存成 CSV。
        "Excel \u6a94\u8acb\u5148\u5728 Excel \u7528\u300c\u53e6\u5b58\u65b0\u6a94 \u2192 CSV UTF-8\uff08\u9017\u865f\u5206\u9694\uff09\u300d\u5b58\u6210 CSV\u3002"
      }
    )
  }
  if (!target %in% convert_targets(kind)) {
    # 原文：請選擇和原檔不同的格式。
    kit_abort("\u8acb\u9078\u64c7\u548c\u539f\u6a94\u4e0d\u540c\u7684\u683c\u5f0f\u3002")
  }
  out_name <- convert_out_name(name, target)
  out <- file.path(out_dir, out_name)
  info <- if (kind != "sav" && target != "sav") {
    convert_csv_stream(path, kind, out, target)
  } else {
    convert_df(path, name, kind, out, target, labels_as_text, dict)
  }
  list(
    name = name,
    out_name = out_name,
    out_path = out,
    nrow = info$nrow,
    ncol = info$ncol,
    size_before = file.size(path),
    size_after = file.size(out),
    notes = info$notes,
    dict_name = if (!is.null(info$dict_path)) basename(info$dict_path),
    dict_path = info$dict_path
  )
}

#' 結果表：檔案、轉成、列 × 欄、原始大小、轉換後大小（提醒另外顯示在狀態訊息）
#' @noRd
convert_summary_table <- function(res) {
  tibble::tibble(
    # 原文：檔案
    "\u6a94\u6848" = res$name,
    # 原文：轉成
    "\u8f49\u6210" = res$out_name,
    # 原文：橫列 × 直欄
    "\u6a6b\u5217 \u00d7 \u76f4\u6b04" = kit_dims(res$nrow, res$ncol),
    # 原文：原始大小
    "\u539f\u59cb\u5927\u5c0f" = format_bytes(res$size_before),
    # 原文：轉換後大小
    "\u8f49\u63db\u5f8c\u5927\u5c0f" = format_bytes(res$size_after)
  )
}
