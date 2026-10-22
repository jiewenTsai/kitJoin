#' 教學用程式碼產生器（tidyverse 版與 base R 版）
#'
#' 依使用者在各分頁的設定產生對應的簡化程式碼；未設定時以範例值代入。
#' 每個 code_*() 回傳 list(tidy = 字串, base = 字串)。

#' 將字元向量轉為 R 程式碼：c("a", "b")
r_chr_vec <- function(x) {
  if (length(x) == 0) return("character(0)")
  paste0("c(", paste0('"', x, '"', collapse = ", "), ")")
}

#' 將數值向量轉為 R 程式碼：c(7777, 8888)
r_num_vec <- function(x) {
  if (length(x) == 0) return("numeric(0)")
  paste0("c(", paste(format(x, scientific = FALSE, trim = TRUE, drop0trailing = TRUE),
                     collapse = ", "), ")")
}

or_default <- function(x, default) {
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x) == 0) default else x
}

code_read <- function(file_names, waves) {
  file_names <- or_default(file_names, c("wave1.csv", "wave2.sav"))
  waves <- or_default(waves, paste0("t", seq_along(file_names)))
  header <- paste0(
'files <- ', r_chr_vec(file_names), '   # 依上傳順序
waves <- ', r_chr_vec(waves), '
')
  list(
    tidy = paste0(
'library(readr)
library(haven)
library(purrr)

', header, '
read_one <- function(path) {
  if (tolower(tools::file_ext(path)) == "csv") read_csv(path) else read_sav(path)
}

dfs <- map(files, read_one) |> set_names(waves)

dim(dfs[[1]])
head(dfs[[1]])
'),
    base = paste0(
'# base R 沒有讀 SAV 的函式，仍需 haven::read_sav()
', header, '
read_one <- function(path) {
  if (tolower(tools::file_ext(path)) == "csv") {
    read.csv(path, fileEncoding = "UTF-8")   # BIG5 檔請改 "BIG5"
  } else {
    haven::read_sav(path)
  }
}

dfs <- setNames(lapply(files, read_one), waves)

dim(dfs[[1]])
head(dfs[[1]])
')
  )
}

code_missing <- function(codes, exclude, match_text = FALSE) {
  if (length(codes) == 0) codes <- parse_na_codes(kit_na_codes_default())$codes
  header <- paste0(
'na_codes  <- ', r_num_vec(codes), '
keep_cols <- ', r_chr_vec(exclude), '   # 不處理的欄位（如 ID）
')
  tidy_text <- if (isTRUE(match_text)) {
',
      across(
        where(is.character) & !any_of(keep_cols),
        \\(x) replace(x, trimws(x) %in% as.character(na_codes), NA)
      )'
  } else {
    ""
  }
  base_text <- if (isTRUE(match_text)) {
'
  txt_cols <- setdiff(names(df)[sapply(df, is.character)], keep_cols)
  for (col in txt_cols) {
    df[[col]][trimws(df[[col]]) %in% as.character(na_codes)] <- NA
  }'
  } else {
    ""
  }
  list(
    tidy = paste0(
'library(dplyr)
library(purrr)

', header, '
to_na <- function(df) {
  df |>
    mutate(
      across(
        where(is.numeric) & !any_of(keep_cols),
        \\(x) replace(x, x %in% na_codes, NA)
      )', tidy_text, '
    )
}

dfs_na <- map(dfs, to_na)

# 檢查：第一個檔案各欄有幾筆是遺漏值代碼
dfs[[1]] |>
  summarise(across(where(is.numeric), \\(x) sum(x %in% na_codes)))

# SAV 檔若要一併移除數值標籤，可再用 haven::zap_labels()
'),
    base = paste0(
header, '
to_na <- function(df) {
  num_cols <- setdiff(names(df)[sapply(df, is.numeric)], keep_cols)
  for (col in num_cols) {
    df[[col]][df[[col]] %in% na_codes] <- NA
  }', base_text, '
  df
}

dfs_na <- lapply(dfs, to_na)

# 檢查：第一個檔案各欄有幾筆是遺漏值代碼
num_cols <- names(dfs[[1]])[sapply(dfs[[1]], is.numeric)]
sapply(dfs[[1]][num_cols], function(x) sum(x %in% na_codes))
')
  )
}

code_join <- function(by_vars, join_method) {
  by_vars <- or_default(by_vars, "release_id")
  join_method <- or_default(join_method, "left_join")
  merge_args <- switch(
    join_method,
    left_join = ", all.x = TRUE",
    full_join = ", all = TRUE",
    ""
  )
  header <- paste0(
'# dfs：在「上傳與預覽」讀入、以波次命名的資料（若做了遺漏值處理則改用 dfs_na）
by_vars <- ', r_chr_vec(by_vars), '
')
  list(
    tidy = paste0(
'library(dplyr)
library(purrr)

', header, '
# 非 key 欄位加上波次後綴（t1 → _t1），避免同名欄衝突
dfs_suffix <- imap(dfs, \\(df, wave) {
  df |> rename_with(\\(x) paste0(x, "_", wave), .cols = !all_of(by_vars))
})

# 依序兩兩合併
joined <- reduce(dfs_suffix, \\(x, y) ', join_method, '(x, y, by = by_vars))

dim(joined)
'),
    base = paste0(
header, '
# 非 key 欄位加上波次後綴（t1 → _t1），避免同名欄衝突
dfs_suffix <- Map(function(df, wave) {
  other <- setdiff(names(df), by_vars)
  names(df)[match(other, names(df))] <- paste0(other, "_", wave)
  df
}, dfs, names(dfs))

# 依序兩兩合併（', join_method, ' 對應 merge()', if (nzchar(merge_args)) merge_args else " 預設", '）
# 注意：merge() 會依 key 重新排序列
joined <- Reduce(function(x, y) merge(x, y, by = by_vars', merge_args, '), dfs_suffix)

dim(joined)
')
  )
}

code_stack <- function() {
  header <- '# dfs：以波次命名的資料清單（若做了遺漏值處理則改用 dfs_na）
'
  list(
    tidy = paste0(
'library(dplyr)

', header, '# .id = "wave" 會用清單名稱（波次）建立 wave 欄
stacked <- bind_rows(dfs, .id = "wave")

# 若同名欄型別不一致（如一檔是數值、一檔是文字）會報錯，
# 可先把該欄統一轉成文字：
# dfs <- map(dfs, \\(df) mutate(df, across(any_of("某欄"), as.character)))

count(stacked, wave)
'),
    base = paste0(
header, '# 先補齊各檔缺少的欄位（填 NA），再加上 wave 欄
all_cols <- unique(unlist(lapply(dfs, names)))

dfs_full <- Map(function(df, wave) {
  df[setdiff(all_cols, names(df))] <- NA
  cbind(wave = wave, df[all_cols])
}, dfs, names(dfs))

stacked <- do.call(rbind, dfs_full)
rownames(stacked) <- NULL

table(stacked$wave)
')
  )
}

code_attrition <- function(base_wave, follow_wave, id_vars) {
  base_wave <- or_default(base_wave, "t1")
  follow_wave <- or_default(follow_wave, "t2")
  id_vars <- or_default(id_vars, "release_id")
  header <- paste0(
'baseline <- dfs[["', base_wave, '"]]   # 前期樣本（基準）
followup <- dfs[["', follow_wave, '"]]   # 後期樣本（比對）
')
  base_body <- if (length(id_vars) == 1) {
    paste0(
'
# 前期 ID 不在後期 → 流失（1）
baseline$attrition <- as.integer(!baseline$', id_vars, ' %in% followup$', id_vars, ')
')
  } else {
    paste0(
'id_vars  <- ', r_chr_vec(id_vars), '

# 多個 ID 欄位：先黏成一個比對鍵
make_key <- function(df) do.call(paste, c(df[id_vars], sep = "|"))

# 前期 ID 不在後期 → 流失（1）
baseline$attrition <- as.integer(!make_key(baseline) %in% make_key(followup))
')
  }
  list(
    tidy = paste0(
'library(dplyr)

', header, 'id_vars  <- ', r_chr_vec(id_vars), '

followup_ids <- followup |>
  distinct(across(all_of(id_vars))) |>
  mutate(.matched = 1L)

attrition_df <- baseline |>
  left_join(followup_ids, by = id_vars) |>
  mutate(attrition = if_else(is.na(.matched), 1L, 0L)) |>   # 1 = 流失
  select(-.matched)

count(attrition_df, attrition)
'),
    base = paste0(
header, base_body, '
table(baseline$attrition)
')
  )
}
