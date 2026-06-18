col_bind_signature <- function(x) {
  if (inherits(x, "labelled")) {
    return(if (is.numeric(x)) "numeric" else "character")
  }
  if (is.factor(x) || is.character(x)) return("character")
  if (is.logical(x)) return("logical")
  if (is.integer(x) || is.double(x) || is.numeric(x)) return("numeric")
  "character"
}

coerce_for_bind <- function(x) {
  fix_utf8_chr(as.character(x))
}

harmonize_for_bind_rows <- function(dfs) {
  harmonized_cols <- character()
  all_cols <- unique(unlist(purrr::map(dfs, names)))
  for (col in all_cols) {
    present <- which(purrr::map_lgl(dfs, ~ col %in% names(.x)))
    if (length(present) < 2) next
    sigs <- unique(purrr::map_chr(dfs[present], ~ col_bind_signature(.x[[col]])))
    if (length(sigs) <= 1) next
    harmonized_cols <- c(harmonized_cols, col)
    dfs <- purrr::map(dfs, function(df) {
      if (!col %in% names(df)) return(df)
      df[[col]] <- coerce_for_bind(df[[col]])
      df
    })
  }
  list(dfs = dfs, harmonized_cols = unique(harmonized_cols))
}

#' 長格式疊加：同名欄對齊，不同欄新增，並標記 wave
stack_files_long <- function(dfs, wave_labels) {
  h <- harmonize_for_bind_rows(dfs)
  result <- purrr::map2(h$dfs, wave_labels, function(df, w) {
    dplyr::mutate(df, wave = w, .before = 1)
  }) %>% dplyr::bind_rows()
  list(result = result, harmonized_cols = h$harmonized_cols)
}

#' 長格式疊加後核對
build_stack_audit <- function(ordered_dfs, file_names, wave_labels, result,
                              harmonized_cols = character()) {
  file_stats <- tibble::tibble(
    順序 = seq_along(ordered_dfs),
    檔案 = file_names,
    wave = wave_labels,
    列數 = purrr::map_int(ordered_dfs, nrow),
    欄位數 = purrr::map_int(ordered_dfs, ncol)
  )

  exp_nrow <- sum(file_stats$列數)
  act_nrow <- nrow(result)
  nrow_ok <- exp_nrow == act_nrow

  union_cols <- unique(unlist(purrr::map(ordered_dfs, names)))
  exp_ncol <- length(union_cols) + 1L
  act_ncol <- ncol(result)
  ncol_ok <- act_ncol == exp_ncol

  wave_ok <- all(purrr::map_lgl(seq_along(wave_labels), function(i) {
    sum(result$wave == wave_labels[i], na.rm = TRUE) == nrow(ordered_dfs[[i]])
  }))

  rule_note <- paste0(
    "垂直疊加後列數應等於各檔列數之和（", exp_nrow, "）；",
    "欄位數應等 union(各檔欄位) + wave（", exp_ncol, "）。",
    if (length(harmonized_cols) > 0) {
      paste0(" 已將 ", length(harmonized_cols), " 個同名欄位統一轉為字元。")
    } else {
      ""
    }
  )

  list(
    file_stats = file_stats,
    harmonized_cols = harmonized_cols,
    summary = tibble::tibble(
      檢查項目 = c(
        "結果列數",
        "預期列數（各檔列數之和）",
        "列數是否符合",
        "結果欄位數",
        "預期欄位數（欄位聯集 + wave）",
        "欄位數是否符合",
        "各 wave 列數是否符合",
        "型別調整欄位數"
      ),
      數值或狀態 = c(
        as.character(act_nrow),
        as.character(exp_nrow),
        if (nrow_ok) "符合" else "不符合",
        as.character(act_ncol),
        as.character(exp_ncol),
        if (ncol_ok) "符合" else "不符合",
        if (wave_ok) "符合" else "不符合",
        as.character(length(harmonized_cols))
      )
    ),
    rule_note = rule_note,
    nrow_ok = nrow_ok,
    ncol_ok = ncol_ok,
    wave_ok = wave_ok
  )
}
