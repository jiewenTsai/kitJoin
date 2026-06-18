#' 以 left join 標記前期樣本的 attrition（流失）欄位
#'
#' @param baseline_df 前期完整資料框
#' @param followup_df 後期資料框（僅需含 id_vars，其餘欄位不使用）
#' @param id_vars 用於比對的 ID 欄位名稱（字元向量）
#' @return list(result, summary, followup_has_dup)
build_attrition <- function(baseline_df, followup_df, id_vars) {
  followup_ids <- followup_df |>
    dplyr::select(dplyr::all_of(id_vars)) |>
    dplyr::distinct()

  n_followup_uniq  <- nrow(followup_ids)
  followup_has_dup <- nrow(followup_df) != n_followup_uniq

  result <- dplyr::left_join(
    baseline_df,
    dplyr::mutate(followup_ids, .kit_matched = 1L),
    by = id_vars
  ) |>
    dplyr::mutate(
      attrition = dplyr::if_else(is.na(.kit_matched), 1L, 0L)
    ) |>
    dplyr::select(-.kit_matched)

  n_baseline  <- nrow(baseline_df)
  n_retained  <- sum(result$attrition == 0L)
  n_attrition <- sum(result$attrition == 1L)

  summary_tbl <- tibble::tibble(
    項目 = c(
      "前期樣本（基準）列數",
      "後期樣本不重複 ID 數",
      "留存（attrition = 0）",
      "流失（attrition = 1）",
      "後期 ID 是否有重複列",
      "輸出欄位數（前期全部欄位 + attrition）"
    ),
    數值或狀態 = c(
      as.character(n_baseline),
      as.character(n_followup_uniq),
      as.character(n_retained),
      as.character(n_attrition),
      if (followup_has_dup) "是（已 distinct 處理）" else "否",
      as.character(ncol(result))
    )
  )

  list(result = result, summary = summary_tbl, followup_has_dup = followup_has_dup)
}
