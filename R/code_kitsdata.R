# ⑥ 轉成 kits 作答檔的教學程式碼（tidyverse 版）
#
# 和其他分頁一樣：每個檔案一個物件（data_t1、data_t2…），逐行寫出；
# 只列出勾了的進階調整，pivot_longer() 由 pivot_longer_code() 產生（只寫非預設的參數）。

#' ⑥ 的程式碼
#'
#' @param f code_files_from(files)
#' @param spec kits_spec()
#' @param item_file 題目表的檔名（NULL 表示沒有上傳）
#' @param rules kits_parse_rules()$rules
#' @param assigned tibble（column、domain）：手動指定領域的欄
#' @return 字串
#' @noRd
code_kitsdata <- function(f, spec, item_file = NULL, rules = NULL, assigned = NULL) {
  map <- spec$map
  items <- spec$items$column
  if (length(items) == 0) items <- c("lana01", "lana02", "bodyaf01")
  idx <- seq_along(f$obj)
  long <- paste0("long_", sub("^data_", "", f$obj))
  steps <- spec$steps
  per_file <- function(fun) paste(vapply(idx, fun, character(1)), collapse = "\n")

  code_lines(
    "library(dplyr)", "library(readr)", "library(tidyr)", "",
    grep("^data_.* <- read_", strsplit(code_read(f)$tidy, "\n")[[1]], value = TRUE),
    "",
    "# 題目欄（各檔的聯集；某個檔案沒有的題會略過）",
    paste0("items <- ", r_cols_vector(items, indent = "  ", quote = TRUE)),
    code_kitsdata_steps(f, steps, map, items),
    "",
    "# 轉成長格式：一列一個孩子的一個波次的一題",
    per_file(function(i) code_kitsdata_long(f$obj[i], long[i], spec$cohort[[i]], map)),
    "",
    code_kitsdata_domain(long, item_file, rules, assigned),
    "",
    code_kitsdata_finish(map),
    "",
    "# 檢查：同一列次同一題只能有一列（應該是 0 列）",
    "count(kits_long, visit_id, item_id) |> filter(n > 1)",
    "",
"# app 為了省記憶體，是一段一段（每 5,000 列寬資料）轉換、寫進 .csv.gz；結果和這裡相同",
        'write_csv(kits_long, "kits_long.csv.gz")   # 副檔名 .gz 會自動壓縮'
  )
}

# 勾了的進階調整（固定順序 1 → 2 → 3）
code_kitsdata_steps <- function(f, steps, map, items) {
  each <- function(expr) {
    paste(paste0(f$obj, " <- ", f$obj, " |>\n  mutate(", expr, ")"), collapse = "\n")
  }
  out <- character(0)
  if (!is.null(steps$na)) {
    extra <- c(map$wave_m, map$age_m)
    extra <- extra[nzchar(extra)]
    target <- if (length(extra) == 0) "any_of(items)" else
      paste0("c(any_of(items), ", paste(r_col(extra), collapse = ", "), ")")
    out <- c(out, "", "# 步驟 1：遺漏值代碼轉 NA",
             paste0("na_codes <- ", r_num(steps$na)),
             each(paste0("across(", target, ", \\(x) replace(x, x %in% na_codes, NA))")))
  }
  if (!is.null(steps$shift)) {
    shift <- if (setequal(steps$shift, items)) "items" else
      r_cols_vector(steps$shift, indent = "  ", quote = TRUE)
    out <- c(out, "", "# 步驟 2：分數減 1（kits 的分數要從 0 開始）",
             paste0("shift_items <- ", shift),
             each("across(any_of(shift_items), \\(x) x - 1)"))
  }
  rev <- steps$reverse
  if (!is.null(rev)) {
    max <- if (is.null(rev$max) || is.na(rev$max)) "最高分" else rev$max
    out <- c(out, "", "# 步驟 3：反向計分（0 起算的分數上，最高分 − 分數）",
             paste0("reverse_items <- ", r_cols_vector(rev$items, indent = "  ", quote = TRUE)),
             each(paste0("across(any_of(reverse_items), \\(x) ", max, " - x)")))
    if (isTRUE(rev$add_r)) {
      out <- c(out,
        "# 題號後面加 r",
        paste(paste0(f$obj, " <- rename_with(", f$obj,
                     ", \\(x) paste0(x, \"r\"), any_of(reverse_items))"), collapse = "\n"),
        "items <- c(setdiff(items, reverse_items), paste0(reverse_items, \"r\"))"
      )
    }
  }
  out
}

# 一個檔案：世代、visit_id、pivot_longer()
code_kitsdata_long <- function(obj, long, cohort, map) {
  co <- if (identical(cohort$type, "column")) {
    paste0("cohort = as.character(", r_col(cohort$value), ")")
  } else {
    paste0("cohort = ", r_str(if (nzchar(cohort$value %||% "")) cohort$value else "M03"))
  }
  rule <- kits_visit_rule(map)
  visit <- if (is.null(rule$parts)) {
    paste0("visit_id = as.character(", r_col(map$visit_id), ")")
  } else {
    subj <- if (nzchar(map$subj_id %||% "")) r_col(map$subj_id) else "subj_id"
    paste0("visit_id = paste(cohort, ", subj, ", ", r_col(rule$parts[3]), ", sep = \"_\")")
  }
  args <- list(names_to = "item_id", values_to = "item_score", values_drop_na = TRUE)
  piv <- pivot_longer_code(args, "", cols_code = "any_of(items)")
  code_lines(
    paste0(long, " <- ", obj, " |>"),
    paste0("  mutate(", co, ", ", visit, ") |>"),
    sub("^ \\|>\\n", "", piv)
  )
}

# 接上領域：題目表（結尾 r 的題查不到時查原題）、規則、手動指定
code_kitsdata_domain <- function(long, item_file, rules, assigned = NULL) {
  rules <- rules %||% tibble::tibble(pattern = character(0), domain = character(0))
  cases <- c(
    if (nrow(rules) > 0) {
      paste0("    grepl(", r_str_each(rules$pattern), ", item_id) ~ ", r_str_each(rules$domain))
    },
    if (!is.null(assigned) && nrow(assigned) > 0) {
      purrr::map_chr(split(assigned$column, assigned$domain), function(cols) {
        dom <- assigned$domain[match(cols[1], assigned$column)]
        paste0("    item_id %in% ", r_str(cols), " ~ ", r_str(dom), "   # 手動指定")
      })
    }
  )
  bind <- paste0("kits_long <- bind_rows(", paste(long, collapse = ", "), ")")
  out <- c("# 合併各檔，接上領域", bind)
  if (!is.null(item_file)) {
    out <- c(
      out,
      paste0("item_domain <- read_csv(", r_str(item_file), ")   # 題目表：item_id、domain"),
      "kits_long <- kits_long |>",
      "  mutate(item_base = if_else(item_id %in% item_domain$item_id, item_id,",
      "                             sub(\"r$\", \"\", item_id))) |>   # cogd01r 查 cogd01",
      "  left_join(item_domain, by = join_by(item_base == item_id))"
    )
  } else {
    out <- c(out, "kits_long <- mutate(kits_long, domain = NA_character_)")
  }
  if (length(cases) > 0) {
    out <- c(
      out,
      "kits_long <- kits_long |>",
      "  mutate(domain = coalesce(domain, case_when(",
      paste0(cases, c(rep(",", length(cases) - 1), "")),
      "  )))"
    )
  }
  code_lines(out)
}

# 每個值各自加引號
r_str_each <- function(x) paste0('"', gsub("\\\\", "\\\\\\\\", x), '"')

# 固定 9 欄、固定順序與型別（依 KITS_COLUMNS）；沒有對應的欄寫成空白
code_kitsdata_finish <- function(map) {
  cast <- c(c = "as.character", d = "as.numeric", i = "as.integer")
  empty <- c(c = "NA_character_", d = "NA_real_", i = "NA_integer_")
  exprs <- purrr::pmap_chr(KITS_COLUMNS, function(column, type, required, from, meaning) {
    if (from != "map") {
      return(if (type == "i") paste0(column, " = as.integer(", column, ")") else column)
    }
    col <- map[[column]] %||% ""
    if (column == "visit_id" && !nzchar(col)) return("visit_id")
    if (!nzchar(col)) return(paste0(column, " = ", empty[[type]]))
    value <- paste0(cast[[type]], "(", r_col(col), ")")
    if (type == "d" && col == column) column else paste0(column, " = ", value)
  })
  lines <- character(0)
  line <- ""
  for (x in exprs) {
    piece <- if (nzchar(line)) paste0(line, ", ", x) else x
    if (nchar(piece) > 80) {
      lines <- c(lines, paste0(line, ","))
      line <- x
    } else {
      line <- piece
    }
  }
  code_lines(
    "# 固定 9 欄（名稱、順序、型別和 kits 相同）；沒有對應的欄寫成空白",
    "kits_long <- kits_long |>",
    "  transmute(",
    paste0("    ", c(lines, line)),
    "  )"
  )
}
