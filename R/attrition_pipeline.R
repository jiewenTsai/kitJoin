# 流失樣本：以前期樣本為準，標記後期是否還在

#' 標記前期樣本的流失（attrition）
#'
#' 前期的 ID 在後期出現 → 0（留存），沒出現 → 1（流失），
#' ID 空白無法比對 → NA。原檔已有 attrition 欄時，新欄改名為 kit_attrition。
#'
#' @param baseline_df 前期完整資料框
#' @param followup_df 後期資料框（只用 id_vars）
#' @param id_vars 用於比對的 ID 欄位
#' @return list(result, flag_col, summary, warnings, notes)
#' @noRd
build_attrition <- function(baseline_df, followup_df, id_vars) {
  if (length(id_vars) == 0) {
    kit_abort("請先選擇用來辨識同一個人的 ID 欄位（例如 release_id）。")
  }
  notes <- character(0)
  conflict <- key_type_conflict(
    list(baseline_df, followup_df),
    id_vars,
    c("前期", "後期")
  )
  if (!is.null(conflict)) {
    converted <- keys_to_text(list(baseline_df, followup_df), id_vars)
    baseline_df <- converted[[1]]
    followup_df <- converted[[2]]
    notes <- paste0(conflict, "已自動把 ID 轉成文字再比對。")
  }

  base_keys <- key_strings(baseline_df, id_vars)
  follow_keys <- key_strings(followup_df, id_vars)
  follow_keys <- follow_keys[!is.na(follow_keys)]

  flag_col <- unused_name("attrition", names(baseline_df))
  flag <- ifelse(
    is.na(base_keys),
    NA_integer_,
    ifelse(base_keys %in% follow_keys, 0L, 1L)
  )
  result <- baseline_df
  result[[flag_col]] <- flag

  n_na <- sum(is.na(flag))
  n_follow_dup <- sum(duplicated(follow_keys))
  summary_tbl <- tibble::tibble(
    項目 = c(
      "前期樣本列數",
      "後期樣本不重複 ID 數",
      paste0("留存（", flag_col, " = 0）"),
      paste0("流失（", flag_col, " = 1）"),
      paste0("ID 空白無法比對（", flag_col, " 空白）"),
      paste0("輸出欄位數（前期全部欄位 + ", flag_col, "）")
    ),
    數值 = c(
      nrow(baseline_df),
      length(unique(follow_keys)),
      sum(flag == 0L, na.rm = TRUE),
      sum(flag == 1L, na.rm = TRUE),
      n_na,
      ncol(result)
    )
  )

  list(
    result = result,
    flag_col = flag_col,
    summary = summary_tbl,
    warnings = c(
      if (n_na > 0) {
        paste0("前期有 ", n_na, " 列的 ID 是空白，無法判斷是否流失，", flag_col, " 欄留空。")
      },
      if (flag_col != "attrition") {
        paste0("前期檔案已經有 attrition 欄，為了不覆蓋原資料，結果放在新的 ", flag_col, " 欄。")
      }
    ),
    notes = c(
      notes,
      if (n_follow_dup > 0) {
        paste0("後期有 ", n_follow_dup, " 筆重複的 ID，比對時只看「有沒有出現」，不影響結果。")
      }
    )
  )
}
