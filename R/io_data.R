#' 讀取 CSV 或 SAV 並修正 UTF-8
#' @return data.frame，已修正 UTF-8；SAV 標籤欄保留原型態
read_data_file <- function(path, name) {
  ext <- tolower(tools::file_ext(name))
  if (ext == "csv") {
    read_csv_auto(path)
  } else if (ext %in% c("sav", "zsav")) {
    fix_utf8_df(haven::read_sav(path))
  } else {
    stop("不支援的檔案格式：", name)
  }
}

#' 預覽用：將 haven 標籤欄轉成一般字元
df_for_display <- function(df, n = NULL) {
  out <- fix_utf8_df(df)
  if (!is.null(n)) out <- head(out, n)
  out
}

#' 預覽用：欄位過多時只取前 max_cols 欄（優先保留指定欄）
preview_df_cols <- function(df, max_cols = 20L, priority_cols = NULL) {
  cols <- names(df)
  if (length(cols) <= max_cols) return(df)
  if (is.null(priority_cols)) priority_cols <- character(0)
  priority_cols <- intersect(priority_cols, cols)
  keep <- unique(c(priority_cols, setdiff(cols, priority_cols)))
  keep <- keep[seq_len(max_cols)]
  df[, keep, drop = FALSE]
}

#' 匯出用：將 key 欄位移到最前
reorder_key_first <- function(df, key_cols) {
  key_cols <- intersect(key_cols, names(df))
  if (length(key_cols) == 0) return(df)
  df[, c(key_cols, setdiff(names(df), key_cols)), drop = FALSE]
}

prepare_export <- function(df, mode, by_vars) {
  df <- fix_utf8_df(df)
  if (is.null(by_vars)) by_vars <- character(0)
  key_cols <- if (identical(mode, "long")) {
    unique(c(intersect(by_vars, names(df)), intersect("wave", names(df))))
  } else {
    intersect(by_vars, names(df))
  }
  reorder_key_first(df, key_cols)
}

# ── 內部工具 ────────────────────────────────────────────────
read_csv_auto <- function(path) {
  try_enc <- function(enc) {
    readr::read_csv(
      path,
      show_col_types = FALSE,
      locale = readr::locale(encoding = enc)
    )
  }
  df <- tryCatch(try_enc("UTF-8"), error = function(e) NULL)
  if (is.null(df)) df <- tryCatch(try_enc("BIG5"), error = function(e) NULL)
  if (is.null(df)) df <- try_enc("Latin1")
  fix_utf8_df(df)
}

fix_utf8_chr <- function(x) {
  if (!is.character(x)) return(x)
  x <- as.character(x)
  iconv(x, from = "", to = "UTF-8", sub = "")
}

fix_utf8_df <- function(df) {
  dplyr::mutate(df, dplyr::across(dplyr::everything(), function(x) {
    if (inherits(x, "labelled") && is.numeric(x)) {
      x
    } else if (inherits(x, "labelled") || is.character(x) || is.factor(x)) {
      fix_utf8_chr(as.character(x))
    } else {
      x
    }
  }))
}
