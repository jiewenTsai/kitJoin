#' 寬串接後綴：使用者輸入 t1，實際欄位後綴為 _t1
join_suffix <- function(label) {
  label <- trimws(if (is.null(label)) "" else label)
  if (!nzchar(label)) return("")
  if (startsWith(label, "_")) label else paste0("_", label)
}

#' 對非 by 欄位加後綴
apply_suffix <- function(df, suffix, by_vars) {
  if (is.null(suffix) || suffix == "") return(df)
  dplyr::rename_with(df, ~ paste0(.x, suffix), .cols = -dplyr::all_of(by_vars))
}

key_dup_stats <- function(df, by_vars) {
  key_df    <- dplyr::select(df, dplyr::all_of(by_vars))
  key_count <- dplyr::count(key_df, dplyr::across(dplyr::all_of(by_vars)))
  list(
    n_rows = nrow(df),
    n_unique_keys = nrow(dplyr::distinct(key_df)),
    n_dup_key_groups = sum(key_count$n > 1, na.rm = TRUE),
    max_rows_per_key = if (nrow(key_count) > 0) max(key_count$n) else 0L
  )
}

has_dup_keys <- function(df, by_vars) {
  key_dup_stats(df, by_vars)$n_dup_key_groups > 0
}

keys_in_all_files <- function(dfs, by_vars) {
  key_sets <- purrr::map(dfs, function(x) dplyr::distinct(dplyr::select(x, dplyr::all_of(by_vars))))
  nrow(purrr::reduce(key_sets, dplyr::inner_join, by = by_vars))
}

keys_in_any_file <- function(dfs, by_vars) {
  key_sets <- purrr::map(dfs, function(x) dplyr::distinct(dplyr::select(x, dplyr::all_of(by_vars))))
  nrow(dplyr::distinct(dplyr::bind_rows(key_sets)))
}

#' 串接後核對：各檔統計、逐步合併、最終列欄數是否符合規則
build_join_audit <- function(ordered_dfs, file_names, by_vars, join_method, result) {
  join_fn <- get(join_method, envir = asNamespace("dplyr"))
  n_by <- length(by_vars)
  any_dup <- any(purrr::map_lgl(ordered_dfs, ~ has_dup_keys(.x, by_vars)))

  file_stats <- purrr::imap_dfr(ordered_dfs, function(df, i) {
    st <- key_dup_stats(df, by_vars)
    tibble::tibble(
      順序 = i,
      檔案 = file_names[i],
      列數 = st$n_rows,
      欄位數 = ncol(df),
      新增欄位數 = ncol(df) - n_by,
      不重複鍵數 = st$n_unique_keys,
      重複鍵組數 = st$n_dup_key_groups,
      單鍵最多列數 = st$max_rows_per_key
    )
  })

  step_rows <- list()
  acc <- ordered_dfs[[1]]
  for (i in seq_along(ordered_dfs)[-1]) {
    right <- ordered_dfs[[i]]
    exp_ncol <- ncol(acc) + ncol(right) - n_by
    new_acc <- join_fn(acc, right, by = by_vars)
    step_rows[[length(step_rows) + 1]] <- tibble::tibble(
      步驟 = paste0("第 ", i, " 步：", file_names[i - 1], " ", join_method, " ", file_names[i]),
      左表列數 = nrow(acc),
      右表列數 = nrow(right),
      結果列數 = nrow(new_acc),
      預期欄位數 = exp_ncol,
      實際欄位數 = ncol(new_acc),
      欄位數符合 = exp_ncol == ncol(new_acc)
    )
    acc <- new_acc
  }
  join_steps <- dplyr::bind_rows(step_rows)

  exp_ncol_final <- n_by + sum(file_stats$新增欄位數)
  act_ncol <- ncol(result)
  act_nrow <- nrow(result)
  ncol_ok <- exp_ncol_final == act_ncol

  keys_all <- keys_in_all_files(ordered_dfs, by_vars)
  keys_any <- keys_in_any_file(ordered_dfs, by_vars)

  exp_nrow <- switch(
    join_method,
    left_join = nrow(ordered_dfs[[1]]),
    inner_join = keys_all,
    full_join = keys_any
  )

  exp_nrow_note <- switch(
    join_method,
    left_join = "以順序第 1 個檔案為主體；by 鍵在後續檔案皆唯一且無額外配對列時，列數應等於第 1 檔列數",
    inner_join = "僅保留所有檔案皆有的 by 鍵；by 鍵各檔皆唯一時，列數應等於各檔鍵的交集筆數",
    full_join = "保留任一方出現過的 by 鍵；by 鍵各檔皆唯一時，列數應等於各檔鍵的聯集筆數"
  )

  if (any_dup) {
    exp_nrow_note <- paste0(
      exp_nrow_note,
      "。（偵測到 by 鍵有重複列，列數可能因一對多而膨脹，以下「預期列數」僅供參考）"
    )
  }

  nrow_ok <- if (any_dup) {
    NA
  } else {
    act_nrow == exp_nrow
  }

  list(
    file_stats = file_stats,
    join_steps = join_steps,
    summary = tibble::tibble(
      檢查項目 = c(
        "結果欄位數",
        "預期欄位數（by 欄 + 各檔新增欄）",
        "欄位數是否符合",
        "結果列數",
        "預期列數（依 join 規則）",
        "列數是否符合",
        "各檔鍵交集筆數",
        "各檔鍵聯集筆數",
        "by 鍵是否有重複列"
      ),
      數值或狀態 = c(
        as.character(act_ncol),
        as.character(exp_ncol_final),
        if (ncol_ok) "符合" else "不符合",
        as.character(act_nrow),
        if (any_dup) "（有重複鍵，無單一預期值）" else as.character(exp_nrow),
        if (is.na(nrow_ok)) "需人工判讀（有重複鍵）" else if (nrow_ok) "符合" else "不符合",
        as.character(keys_all),
        as.character(keys_any),
        if (any_dup) "是" else "否"
      )
    ),
    rule_note = exp_nrow_note,
    ncol_ok = ncol_ok,
    nrow_ok = nrow_ok,
    any_dup = any_dup,
    join_method = join_method,
    by_vars = paste(by_vars, collapse = ", ")
  )
}
