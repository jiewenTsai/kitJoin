# 長/寬格式轉換的教學程式碼：只寫出和 tidyr 預設不同的參數（只有 tidyverse 版）

#' 單一字串轉成 R 程式碼（含跳脫）："a"
#' @noRd
r_chr1 <- function(x) encodeString(x, quote = '"')

#' tidyselect 的欄位寫法：一欄 x，多欄 c(x, y)
#' @noRd
r_cols <- function(x) {
  x <- r_col(x)
  if (length(x) == 1) x else paste0("c(", paste(x, collapse = ", "), ")")
}

#' 值轉成 R 程式碼：數字原樣、文字加引號、具名 list 寫成 list(a = 0)
#' @noRd
r_value <- function(x) {
  one <- function(v) {
    if (is.numeric(v)) format(v, scientific = FALSE, drop0trailing = TRUE)
    else r_chr1(v)
  }
  if (is.list(x)) {
    parts <- paste0(r_col(names(x)), " = ", vapply(x, one, character(1)))
    return(paste0("list(", paste(parts, collapse = ", "), ")"))
  }
  one(x)
}

#' 組出 pivot 呼叫：每個參數一行
#'
#' @param fn "pivot_wider" 或 "pivot_longer"
#' @param arg_code 具名字元向量：參數名 → 已寫好的 R 程式碼
#' @noRd
code_pivot_call <- function(fn, arg_code, data_name, result_name) {
  lines <- paste0("    ", names(arg_code), " = ", arg_code)
  lines <- paste0(lines, c(rep(",", length(lines) - 1), ""))
  c(
    paste0(result_name, " <- ", data_name, " |>"),
    paste0("  ", fn, "("),
    lines,
    "  )"
  )
}

#' pivot_wider() 的教學程式碼
#'
#' @param args wider_args() 的結果
#' @param data 要轉換的資料（判斷 id_cols 是否等於 tidyr 預設、values_fill 型別）
#' @param data_name 程式中資料物件的名稱，例如 data_t1
#' @return list(tidy = 字串, base = NULL)
#' @noRd
code_wider <- function(args, data, data_name = "data_t1") {
  nf <- if (length(args$names_from) == 1) args$names_from else "domain"
  vf <- if (length(args$values_from) > 0) args$values_from else "d_score"
  default_id <- setdiff(names(data), c(nf, vf))
  id_cols <- args$id_cols
  fill <- parse_values_fill(args$values_fill, data, vf)
  key_cols <- r_col(c(id_cols %||% default_id, nf))
  arg_code <- c(
    id_cols = if (!is.null(id_cols) && !identical(id_cols, default_id)) {
      r_cols(id_cols)
    },
    names_from = r_col(nf),
    values_from = r_cols(vf),
    names_prefix = if (nzchar(args$names_prefix)) r_chr1(args$names_prefix),
    names_sep = if (!identical(args$names_sep, "_")) r_chr1(args$names_sep),
    names_glue = if (!is.null(args$names_glue)) r_chr1(args$names_glue),
    names_vary = if (args$names_vary != "fastest") r_chr1(args$names_vary),
    names_sort = if (isTRUE(args$names_sort)) "TRUE",
    values_fill = if (!is.null(fill)) r_value(fill)
  )
  list(
    tidy = code_lines(
      "library(dplyr)",
      "library(tidyr)", "",
      # 原文：：「檔案上傳」讀進來的資料（讀檔方式見「檔案上傳」分頁的「用 R 怎麼做」）
      paste0("# ", data_name, "\uff1a\u300c\u6a94\u6848\u4e0a\u50b3\u300d\u8b80\u9032\u4f86\u7684\u8cc7\u6599\uff08\u8b80\u6a94\u65b9\u5f0f\u898b\u300c\u6a94\u6848\u4e0a\u50b3\u300d\u5206\u9801\u7684\u300c\u7528 R \u600e\u9ebc\u505a\u300d\uff09"),
      # 原文：# 先確認 id_cols × names_from 沒有重複（有重複時 pivot_wider 會產生 list 欄）
      "# \u5148\u78ba\u8a8d id_cols \u00d7 names_from \u6c92\u6709\u91cd\u8907\uff08\u6709\u91cd\u8907\u6642 pivot_wider \u6703\u7522\u751f list \u6b04\uff09",
      paste0(data_name, " |>"),
      "  count(",
      paste0("    ", strwrap(paste(key_cols, collapse = ", "), 70)),
      "  ) |>",
      "  filter(n > 1)",
      "",
      code_pivot_call("pivot_wider", arg_code, data_name, "wide"),
      "",
      # 原文：dim(wide)   # 列數、欄數
      "dim(wide)   # \u5217\u6578\u3001\u6b04\u6578"
    ),
    base = NULL
  )
}

#' pivot_longer() 的教學程式碼
#'
#' @param args mod_pivot_longer_server() 的參數 list（程式碼和 ⑥ 一樣由 pivot_longer_code() 產生）
#' @param col_names NULL 或資料的全部欄名（選到整組欄時寫 starts_with()）
#' @param labels NULL 或 pivot_longer_labels() 的結果（要先拿掉標籤時多寫一段）
#' @return list(tidy = 字串, base = NULL)
#' @noRd
code_longer <- function(args, data_name = "data_t1", col_names = NULL, labels = NULL) {
  zap <- pivot_longer_labels_code(labels, data_name)
  cols_code <- pivot_cols_code(args, col_names)
  note <- if (startsWith(cols_code, "!")) {
    # 原文：# cols = !c(...)：括號裡的欄不轉，其餘全部轉
    "# cols = !c(...)\uff1a\u62ec\u865f\u88e1\u7684\u6b04\u4e0d\u8f49\uff0c\u5176\u9918\u5168\u90e8\u8f49"
  } else if (grepl("starts_with(", cols_code, fixed = TRUE)) {
    first <- sub('^[^"]*"([^"]*)".*$', "\\1", cols_code)
    # 原文：\")：欄名以  |  開頭的欄（不分大小寫）
    paste0("# starts_with(\"", first, "\")\uff1a\u6b04\u540d\u4ee5 ", first, " \u958b\u982d\u7684\u6b04\uff08\u4e0d\u5206\u5927\u5c0f\u5beb\uff09")
  }
  list(
    tidy = code_lines(
      if (!is.null(zap)) "library(dplyr)", "library(tidyr)", "",
      # 原文：：「檔案上傳」讀進來的資料（讀檔方式見「檔案上傳」分頁的「用 R 怎麼做」）
      paste0("# ", data_name, "\uff1a\u300c\u6a94\u6848\u4e0a\u50b3\u300d\u8b80\u9032\u4f86\u7684\u8cc7\u6599\uff08\u8b80\u6a94\u65b9\u5f0f\u898b\u300c\u6a94\u6848\u4e0a\u50b3\u300d\u5206\u9801\u7684\u300c\u7528 R \u600e\u9ebc\u505a\u300d\uff09"),
      zap,
      note,
      paste0("long <- ", pivot_longer_code(args, data_name, cols_code)),
      "",
      # 原文：dim(long)   # 列數、欄數
      "dim(long)   # \u5217\u6578\u3001\u6b04\u6578"
    ),
    base = NULL
  )
}
