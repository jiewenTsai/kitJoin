# ⑦ 長寬互轉的教學程式碼：只寫出和 tidyr 預設不同的參數（只有 tidyverse 版）

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
    values_fill = if (!is.null(fill)) r_value(fill)
  )
  list(
    tidy = code_lines(
      "library(dplyr)",
      "library(tidyr)", "",
      paste0("# ", data_name, "：「① 上傳檔案」讀進來的資料（讀檔方式見 ① 分頁）"),
      "# 先確認 id_cols × names_from 沒有重複（有重複時 pivot_wider 會產生 list 欄）",
      paste0(data_name, " |>"),
      "  count(",
      paste0("    ", strwrap(paste(key_cols, collapse = ", "), 70)),
      "  ) |>",
      "  filter(n > 1)",
      "",
      code_pivot_call("pivot_wider", arg_code, data_name, "wide"),
      "",
      "dim(wide)   # 列數、欄數"
    ),
    base = NULL
  )
}

#' pivot_longer() 的教學程式碼
#'
#' @param args mod_pivot_longer_server() 的參數 list（程式碼和 ⑥ 一樣由 pivot_longer_code() 產生）
#' @param col_names NULL 或資料的全部欄名（選到整組欄時寫 starts_with()）
#' @return list(tidy = 字串, base = NULL)
#' @noRd
code_longer <- function(args, data_name = "data_t1", col_names = NULL) {
  cols_code <- pivot_cols_code(args, col_names)
  note <- if (startsWith(cols_code, "!")) {
    "# cols = !c(...)：括號裡的欄不轉，其餘全部轉"
  } else if (grepl("starts_with(", cols_code, fixed = TRUE)) {
    first <- sub('^[^"]*"([^"]*)".*$', "\\1", cols_code)
    paste0("# starts_with(\"", first, "\")：欄名以 ", first, " 開頭的欄（不分大小寫）")
  }
  list(
    tidy = code_lines(
      "library(tidyr)", "",
      paste0("# ", data_name, "：「① 上傳檔案」讀進來的資料（讀檔方式見 ① 分頁）"),
      note,
      paste0("long <- ", pivot_longer_code(args, data_name, cols_code)),
      "",
      "dim(long)   # 列數、欄數"
    ),
    base = NULL
  )
}
