#' 教學用程式碼產生器（tidyverse 版與 base R 版）
#'
#' 給初學者看：每個檔案一個物件（data_t1、data_t2…），逐行寫出，
#' 盡量少用進階觀念（不用 map()、自訂函式、where() 等）。
#' 程式碼依使用者在各分頁的設定產生；尚未上傳檔案時以範例值代入。
#' 每個 code_*() 回傳 list(tidy = 字串, base = 字串)。

#' 字串向量轉成 R 程式碼：c("a", "b")；單一值不加 c()
r_str <- function(x) {
  q <- paste0('"', x, '"')
  if (length(q) == 1) q else paste0("c(", paste(q, collapse = ", "), ")")
}

#' 數值向量轉成 R 程式碼：c(7777, 8888)
r_num <- function(x) {
  paste0("c(", paste(format(x, scientific = FALSE, trim = TRUE, drop0trailing = TRUE),
                     collapse = ", "), ")")
}

#' 欄位名稱在 tidyselect 中的寫法（非合法名稱加反引號）
r_col <- function(x) {
  ifelse(make.names(x) == x, x, paste0("`", x, "`"))
}

#' 排除欄位的 tidyselect 寫法：-id 或 -c(id, idno)
r_minus_cols <- function(x) {
  if (length(x) == 1) paste0("-", r_col(x)) else paste0("-c(", paste(r_col(x), collapse = ", "), ")")
}

#' 範例程式中的檔案資訊：檔名、波次、物件名稱、欄位
#'
#' @param file_names 上傳檔名（未上傳時為空）
#' @param waves 各檔波次名稱
#' @param cols 各檔欄位名稱（list；未知時為 NULL）
#' @param min_files 檔案數少於此數時改用範例檔（串接、疊加、流失需 2 個）
code_files <- function(file_names = character(0), waves = character(0), cols = NULL,
                       min_files = 1L) {
  if (length(file_names) < min_files) {
    file_names <- c("wave1.csv", "wave2.sav")
    waves <- c("t1", "t2")
    cols <- NULL
  }
  waves <- ifelse(is.na(waves) | !nzchar(waves), paste0("t", seq_along(file_names)), waves)
  obj <- make.unique(paste0("data_", gsub("[^A-Za-z0-9_]", "_", waves)), sep = "_")
  list(file = file_names, wave = waves, obj = obj, cols = cols)
}

code_read <- function(f) {
  is_csv <- tolower(tools::file_ext(f$file)) == "csv"
  tidy_lines <- paste0(f$obj, " <- ", ifelse(is_csv, "read_csv", "read_sav"),
                       '("', f$file, '")')
  base_lines <- paste0(f$obj, " <- ", ifelse(is_csv, "read.csv", "haven::read_sav"),
                       '("', f$file, '")',
                       ifelse(is_csv, "", "   # base R 不能讀 SAV，借用 haven"))
  list(
    tidy = paste0(
'library(readr)   # 讀 CSV
library(haven)   # 讀 SAV

', paste(tidy_lines, collapse = "\n"), '

head(', f$obj[1], ')   # 看前 6 列
'),
    base = paste0(
paste(base_lines, collapse = "\n"), '

head(', f$obj[1], ')   # 看前 6 列
')
  )
}

code_missing <- function(f, codes, exclude = character(0), match_text = TRUE) {
  if (length(codes) == 0) codes <- parse_na_codes(kit_na_codes_default())$codes
  if (is.null(exclude)) exclude <- character(0)
  # 各檔實際存在的排除欄位（欄位未知時直接使用）
  excl_of <- function(i) {
    if (is.null(f$cols)) exclude else intersect(exclude, f$cols[[i]])
  }

  tidy_one <- function(i) {
    ex <- excl_of(i)
    target <- if (length(ex) == 0) "everything()" else r_minus_cols(ex)
    if (!match_text) {
      target <- if (length(ex) == 0) {
        "where(is.numeric)"
      } else {
        paste0("where(is.numeric) & !", sub("^-", "", r_minus_cols(ex)))
      }
    }
    paste0(f$obj[i], " <- ", f$obj[i], " |>\n",
           "  mutate(across(", target, ", ~ replace(.x, .x %in% na_codes, NA)))")
  }
  base_one <- function(i) {
    ex <- excl_of(i)
    obj <- f$obj[i]
    cols_line <- if (length(ex) == 0) {
      paste0("cols <- names(", obj, ")")
    } else {
      paste0("cols <- setdiff(names(", obj, "), ", r_str(ex), ")")
    }
    if (!match_text) {
      cols_line <- paste0(cols_line, "\ncols <- cols[sapply(", obj, "[cols], is.numeric)]   # 只留數值欄")
    }
    paste0(cols_line, "\n",
           "for (col in cols) {\n",
           "  ", obj, "[[col]][", obj, "[[col]] %in% na_codes] <- NA\n",
           "}")
  }

  idx <- seq_along(f$obj)
  scope_note <- if (length(exclude) > 0) {
    paste0("# 不處理：", paste(exclude, collapse = "、"), "（如 ID 欄）\n")
  } else {
    ""
  }
  header <- paste0('na_codes <- ', r_num(codes), '

# %in% 會把數字和文字的 "9999" 都當成相同，所以不用另外寫文字版
', scope_note)

  list(
    tidy = paste0(
'library(dplyr)

', header, '
', paste(vapply(idx, tidy_one, character(1)), collapse = "\n\n"), '

colSums(is.na(', f$obj[1], '))   # 檢查：各欄 NA 數
'),
    base = paste0(
header, '
', paste(vapply(idx, base_one, character(1)), collapse = "\n\n"), '

colSums(is.na(', f$obj[1], '))   # 檢查：各欄 NA 數
')
  )
}

code_join <- function(f, by_vars, join_method) {
  by_vars <- if (length(by_vars) == 0) "release_id" else by_vars
  join_method <- if (length(join_method) == 0 || !nzchar(join_method)) "left_join" else join_method
  wide <- sub("^data_", "", f$obj)
  wide <- paste0(wide, "_wide")
  suffix <- vapply(f$wave, join_suffix, character(1))

  tidy_rename <- paste0(wide, " <- rename_with(", f$obj, ", ~ paste0(.x, \"", suffix, "\"), ",
                        r_minus_cols(by_vars), ")")
  tidy_join <- paste0("joined <- ", wide[1], " |>\n",
                      paste0("  ", join_method, "(", wide[-1], ", by = ", r_str(by_vars), ")",
                             collapse = " |>\n"))

  keep_test <- if (length(by_vars) == 1) {
    function(w) paste0("names(", w, ") != ", r_str(by_vars))
  } else {
    function(w) paste0("!names(", w, ") %in% ", r_str(by_vars))
  }
  base_rename <- paste0(
    wide, " <- ", f$obj, "\n",
    "to_rename <- ", vapply(wide, keep_test, character(1)), "\n",
    "names(", wide, ")[to_rename] <- paste0(names(", wide, ")[to_rename], \"", suffix, "\")"
  )
  merge_arg <- switch(join_method, left_join = ", all.x = TRUE", full_join = ", all = TRUE", "")
  base_join <- paste0(
    "joined <- merge(", wide[1], ", ", wide[2], ", by = ", r_str(by_vars), merge_arg, ")",
    if (length(wide) > 2) {
      paste0("\n", paste0("joined <- merge(joined, ", wide[-(1:2)], ", by = ", r_str(by_vars),
                          merge_arg, ")", collapse = "\n"))
    } else {
      ""
    }
  )
  merge_note <- switch(
    join_method,
    left_join = "# all.x = TRUE 相當於 left_join；merge() 會依 key 重新排序",
    full_join = "# all = TRUE 相當於 full_join；merge() 會依 key 重新排序",
    "# 不加 all 參數相當於 inner_join；merge() 會依 key 重新排序"
  )

  list(
    tidy = paste0(
'library(dplyr)

# 1. 非 key 欄位加上波次後綴，避免同名欄衝突
', paste(tidy_rename, collapse = "\n"), '

# 2. 依序合併
', tidy_join, '

dim(joined)   # 列數、欄數
'),
    base = paste0(
'# 1. 非 key 欄位加上波次後綴，避免同名欄衝突
', paste(base_rename, collapse = "\n\n"), '

# 2. 依序合併
', merge_note, '
', base_join, '

dim(joined)   # 列數、欄數
')
  )
}

code_stack <- function(f) {
  tidy_args <- paste0('  "', f$wave, '" = ', f$obj, collapse = ",\n")
  long <- paste0(sub("^data_", "", f$obj), "_long")
  base_copy <- paste0(long, " <- ", f$obj, "\n", long, '$wave <- "', f$wave, '"')
  list(
    tidy = paste0(
'library(dplyr)

# .id = "wave"：用前面的名稱（波次）建立 wave 欄
stacked <- bind_rows(
', tidy_args, ',
  .id = "wave"
)
# 若同名欄型別不同（一檔數值、一檔文字）會報錯，需先統一型別

count(stacked, wave)   # 各波次列數
'),
    base = paste0(
'# 1. 每個檔案加上 wave 欄
', paste(base_copy, collapse = "\n"), '

# 2. rbind() 需要欄位相同：先補上缺少的欄位（填 NA）
all_cols <- unique(c(', paste0("names(", long, ")", collapse = ", "), '))
', paste0(long, "[setdiff(all_cols, names(", long, "))] <- NA", collapse = "\n"), '

# 3. 上下疊起來
stacked <- rbind(', paste(long, collapse = ", "), ')

table(stacked$wave)   # 各波次列數
')
  )
}

code_attrition <- function(f, base_i, follow_i, id_vars) {
  id_vars <- if (length(id_vars) == 0) "release_id" else id_vars
  if (length(base_i) == 0 || is.na(base_i)) base_i <- 1L
  if (length(follow_i) == 0 || is.na(follow_i)) follow_i <- min(2L, length(f$obj))
  base_obj <- f$obj[base_i]
  follow_obj <- f$obj[follow_i]

  if (length(id_vars) == 1) {
    id <- r_col(id_vars)
    tidy_body <- paste0(
'attrition_df <- ', base_obj, ' |>
  mutate(attrition = if_else(', id, ' %in% ', follow_obj, '$', id, ', 0, 1))')
    base_body <- paste0(
'attrition_df <- ', base_obj, '
attrition_df$attrition <- ifelse(attrition_df$', id, ' %in% ', follow_obj, '$', id, ', 0, 1)')
    lead <- ""
  } else {
    key_of <- function(obj) paste0("paste(", paste0(obj, "$", r_col(id_vars), collapse = ", "), ")")
    lead <- paste0(
'# 多個 ID 欄位：先用 paste() 黏成一個比對用的鍵
followup_key <- ', key_of(follow_obj), '

')
    tidy_body <- paste0(
'attrition_df <- ', base_obj, ' |>
  mutate(attrition = if_else(paste(', paste(r_col(id_vars), collapse = ", "), ') %in% followup_key, 0, 1))')
    base_body <- paste0(
'attrition_df <- ', base_obj, '
attrition_df$attrition <- ifelse(', key_of("attrition_df"), ' %in% followup_key, 0, 1)')
  }
  rule <- paste0("# 前期（", base_obj, "）的 ID 有出現在後期（", follow_obj, "）→ 0 留存，否則 1 流失\n")

  list(
    tidy = paste0(
'library(dplyr)

', rule, lead, tidy_body, '

count(attrition_df, attrition)
'),
    base = paste0(
rule, lead, base_body, '

table(attrition_df$attrition)
')
  )
}
