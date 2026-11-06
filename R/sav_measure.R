# SPSS 的測量水準（名義、次序、尺度）
#
# haven 讀 SAV 時不讀測量水準，寫 SAV 時依型別決定（數字一律「尺度」，有數值標籤也一樣）。
# 所以 kitJoin 自己讀寫 SAV 檔表頭裡的那一段（第 7 類紀錄、子類 11「變項顯示參數」）：
#   讀檔：每一欄掛上 attr "measure"（"nominal"、"ordinal"、"scale"），之後的分頁照常處理；
#   寫檔：haven 寫完後，把每一欄的 measure 寫回同一段（只改那幾個位元組）。
# 讀不懂的檔（格式不是預期的樣子）不改任何東西：讀檔沒有 measure，寫檔維持 haven 的結果。
#
# SAV 表頭的格式見 PSPP 的說明文件「System File Format」。

# 測量水準的代碼（SAV 檔裡）與名稱
SAV_MEASURES <- c(nominal = 1L, ordinal = 2L, scale = 3L)
# 畫面與 codebook 用的中文
# 原文：名義 | 次序 | 尺度
SAV_MEASURE_ZH <- c(nominal = "\u540d\u7fa9", ordinal = "\u6b21\u5e8f", scale = "\u5c3a\u5ea6")

#' 讀 SAV 表頭：每個變項的名稱、測量水準，與測量水準在檔案裡的位置
#'
#' @param path .sav 或 .zsav（表頭不壓縮，兩種一樣）
#' @return tibble(name, measure, pos)：一個變項一列（長字串的延續紀錄不算）；
#'   name 是長變項名稱（沒有時是短名稱），measure 是 "nominal"/"ordinal"/"scale" 或 NA，
#'   pos 是那個整數在檔案裡的位置（0 起算；沒有時 NA）。讀不懂時 NULL。
#' @noRd
sav_header_vars <- function(path) {
  tryCatch(sav_header_parse(path), error = function(e) NULL)
}

sav_header_parse <- function(path) {
  size <- file.size(path)
  con <- file(path, "rb")
  on.exit(close(con))
  # 表頭最多讀到資料開始的地方；大檔不整個讀進來
  head <- readBin(con, "raw", min(size, 64L * 1024L * 1024L))
  magic <- rawToChar(head[1:4])
  if (!magic %in% c("$FL2", "$FL3")) return(NULL)
  int_at <- function(pos, endian) {
    readBin(head[(pos + 1):(pos + 4)], "integer", size = 4, endian = endian)
  }
  endian <- "little"
  if (!int_at(64, endian) %in% 2:3) endian <- "big"
  if (!int_at(64, endian) %in% 2:3) return(NULL)
  int <- function(pos) int_at(pos, endian)
  pos <- 176
  short <- character(0)
  long_map <- NULL
  disp <- NULL
  encoding <- NULL
  repeat {
    if (pos + 4 > length(head)) return(NULL)
    rec <- int(pos)
    pos <- pos + 4
    if (rec == 2L) {
      type <- int(pos)
      has_label <- int(pos + 4)
      n_missing <- int(pos + 8)
      name <- trimws(rawToChar(head[(pos + 21):(pos + 28)]))
      pos <- pos + 28
      if (has_label == 1L) {
        len <- int(pos)
        pos <- pos + 4 + 4 * ceiling(len / 4)
      }
      pos <- pos + 8 * abs(n_missing)
      if (type != -1L) short <- c(short, name)
    } else if (rec == 3L) {
      n <- int(pos)
      pos <- pos + 4
      for (i in seq_len(n)) {
        len <- as.integer(head[pos + 9])
        pos <- pos + 8 + 8 * ceiling((len + 1) / 8)
      }
      if (int(pos) != 4L) return(NULL)
      n_vars <- int(pos + 4)
      pos <- pos + 8 + 4 * n_vars
    } else if (rec == 6L) {
      pos <- pos + 4 + 80 * int(pos)
    } else if (rec == 7L) {
      sub <- int(pos)
      len <- int(pos + 4)
      count <- int(pos + 8)
      start <- pos + 12
      bytes <- len * count
      if (sub == 11L) disp <- list(start = start, count = count, len = len)
      if (sub == 13L) long_map <- head[(start + 1):(start + bytes)]
      if (sub == 20L) encoding <- rawToChar(head[(start + 1):(start + bytes)])
      pos <- start + bytes
    } else if (rec == 999L) {
      break
    } else {
      return(NULL)
    }
  }
  n <- length(short)
  measure <- rep(NA_character_, n)
  at <- rep(NA_real_, n)
  if (!is.null(disp) && disp$len == 4L && disp$count %in% c(2L * n, 3L * n)) {
    step <- disp$count / n
    at <- disp$start + (seq_len(n) - 1) * step * 4
    codes <- vapply(at, int, integer(1))
    measure <- names(SAV_MEASURES)[match(codes, SAV_MEASURES)]
  }
  name <- short
  if (!is.null(long_map)) {
    text <- rawToChar(long_map)
    enc <- if (is.null(encoding) || toupper(encoding) %in% c("UTF-8", "UTF8")) "UTF-8" else encoding
    text <- iconv(text, enc, "UTF-8", sub = "?")
    pairs <- strsplit(strsplit(text, "\t", fixed = TRUE)[[1]], "=", fixed = TRUE)
    pairs <- purrr::keep(pairs, \(p) length(p) == 2)
    keys <- toupper(purrr::map_chr(pairs, 1))
    vals <- purrr::map_chr(pairs, 2)
    hit <- match(toupper(short), keys)
    name[!is.na(hit)] <- vals[hit[!is.na(hit)]]
  }
  tibble::tibble(name = name, measure = measure, pos = at)
}

#' SAV 檔每個變項的測量水準
#' @return 具名文字向量（名稱是變項名稱）；讀不到時 NULL
#' @noRd
sav_read_measures <- function(path) {
  dict <- sav_header_vars(path)
  if (is.null(dict) || all(is.na(dict$measure))) return(NULL)
  dict <- dict[!is.na(dict$measure) & !duplicated(dict$name), ]
  stats::setNames(dict$measure, dict$name)
}

#' 每一欄掛上 SAV 檔裡的測量水準（attr "measure"）
#' @noRd
sav_attach_measures <- function(df, path) {
  m <- sav_read_measures(path)
  if (is.null(m)) return(df)
  for (col in intersect(names(df), names(m))) attr(df[[col]], "measure") <- m[[col]]
  df
}

#' 把測量水準寫進（haven 寫好的）SAV 檔：只改那幾個位元組
#'
#' @param measures 具名文字向量：名稱是檔案裡的變項名稱，值是 "nominal"/"ordinal"/"scale"
#' @return 改了的變項名稱（invisible）；讀不懂檔案時 character(0)
#' @noRd
sav_write_measures <- function(path, measures) {
  measures <- measures[!is.na(measures) & measures %in% names(SAV_MEASURES)]
  if (length(measures) == 0) return(invisible(character(0)))
  dict <- sav_header_vars(path)
  if (is.null(dict)) return(invisible(character(0)))
  endian <- if (sav_is_big_endian(path)) "big" else "little"
  hit <- which(dict$name %in% names(measures) & !is.na(dict$pos))
  if (length(hit) == 0) return(invisible(character(0)))
  con <- file(path, "r+b")
  on.exit(close(con))
  for (i in hit) {
    seek(con, dict$pos[i], rw = "write")
    writeBin(SAV_MEASURES[[measures[[dict$name[i]]]]], con, size = 4, endian = endian)
  }
  invisible(dict$name[hit])
}

sav_is_big_endian <- function(path) {
  con <- file(path, "rb")
  on.exit(close(con))
  head <- readBin(con, "raw", 68)
  !readBin(head[65:68], "integer", size = 4, endian = "little") %in% 2:3
}

#' 一欄的測量水準：attr "measure"；沒有時 NA
#' @noRd
col_measure <- function(x) {
  m <- attr(x, "measure", exact = TRUE)
  if (is.character(m) && length(m) == 1 && m %in% names(SAV_MEASURES)) m else NA_character_
}

#' 合併好幾個資料的欄（疊加、寬轉長）之後，把測量水準掛回去
#'
#' 同名的欄各檔的測量水準一樣時照用；不一樣時用第一個有設定的檔，並回傳這些欄
#' （畫面上提醒）。
#' @param out 合併後的資料；inputs 合併前的資料（list）
#' @param cols 要處理的欄（預設 out 的全部欄）；from 每一欄對應到 inputs 裡的哪些欄名
#' @return list(df, conflicts = tibble(column, measures))
#' @noRd
restore_measures <- function(out, inputs, cols = names(out), from = NULL) {
  conflicts <- list()
  for (col in cols) {
    src <- from[[col]] %||% col
    found <- unlist(purrr::map(inputs, function(df) {
      purrr::map_chr(intersect(src, names(df)), \(s) col_measure(df[[s]]))
    }))
    found <- found[!is.na(found)]
    if (length(found) == 0) next
    attr(out[[col]], "measure") <- found[[1]]
    if (length(unique(found)) > 1) {
      # 原文：、
      conflicts[[col]] <- paste(unique(SAV_MEASURE_ZH[found]), collapse = "\u3001")
    }
  }
  list(
    df = out,
    conflicts = tibble::tibble(column = names(conflicts), measures = unlist(conflicts) %||% character(0))
  )
}
