# 流失樣本：以前期樣本為準，標記後期是否還在

# 前期 ID 的不重複比例低於這個值就停下（多半是選錯欄，例如出生年）
KIT_ID_MIN_UNIQUE <- 0.5

#' 標記前期樣本的流失（attrition）
#'
#' 前期的 ID 在後期出現 → 0（留存），沒出現 → 1（流失），
#' ID 空白無法比對 → NA。原檔已有 attrition 欄時，新欄改名為 kit_attrition。
#'
#' @param baseline_df 前期完整資料框
#' @param followup_df 後期資料框（只用 id_vars）
#' @param id_vars 前期用於比對的 ID 欄位
#' @param follow_id_vars 後期對應的 ID 欄位（依序對應 id_vars）；欄名相同時不用給
#' @return list(result, flag_col, summary, warnings, notes)
#' @noRd
build_attrition <- function(baseline_df, followup_df, id_vars,
                            follow_id_vars = id_vars) {
  if (length(id_vars) == 0) {
    kit_abort("請先選擇用來辨識同一個人的 ID 欄位。")
  }
  if (length(follow_id_vars) != length(id_vars)) {
    kit_abort(
      "前期與後期要選一樣多個 ID 欄：前期選了 ", length(id_vars), " 個，",
      "後期選了 ", length(follow_id_vars), " 個。"
    )
  }
  for (side in list(list("前期", baseline_df, id_vars), list("後期", followup_df, follow_id_vars))) {
    missing_by <- setdiff(side[[3]], names(side[[2]]))
    if (length(missing_by) > 0) {
      kit_abort(side[[1]], "樣本沒有 ", paste(missing_by, collapse = "、"), " 欄位。請重新選 ID 欄。")
    }
  }
  # 後期只用來比對 ID：只留 ID 欄並改成前期的欄名
  followup_df <- followup_df[follow_id_vars]
  names(followup_df) <- id_vars
  notes <- if (!identical(follow_id_vars, id_vars)) {
    paste0(
      "後期的 ", paste(follow_id_vars, collapse = "＋"), " 對應到前期的 ",
      paste(id_vars, collapse = "＋"), "。"
    )
  } else {
    character(0)
  }
  cleaned <- clean_keys(list(baseline_df, followup_df), id_vars)
  baseline_df <- cleaned[[1]]
  followup_df <- cleaned[[2]]
  conflict <- key_type_conflict(
    list(baseline_df, followup_df),
    id_vars,
    c("前期", "後期"),
    list(id_vars, follow_id_vars)
  )
  if (!is.null(conflict)) {
    converted <- keys_to_text(list(baseline_df, followup_df), id_vars)
    baseline_df <- converted[[1]]
    followup_df <- converted[[2]]
    notes <- c(notes, paste0(conflict, "已自動把 ID 轉成文字再比對。"))
  }

  base_keys <- key_strings(baseline_df, id_vars)
  follow_keys <- key_strings(followup_df, id_vars)
  follow_keys <- follow_keys[!is.na(follow_keys)]
  id_text <- paste(id_vars, collapse = "＋")

  # 前期 ID 要能辨識每一個人：大多數列重複時（例如選到出生年）結果沒有意義
  base_ok <- base_keys[!is.na(base_keys)]
  n_base_unique <- length(unique(base_ok))
  if (length(base_ok) > 0 && n_base_unique / length(base_ok) < KIT_ID_MIN_UNIQUE) {
    kit_abort(
      "前期的 ", id_text, " 有 ", length(base_ok), " 列，卻只有 ", n_base_unique,
      " 種值，看起來不是用來辨識每個人的 ID（例如出生年、權數）。",
      "請改選每個人都不同的編號欄。"
    )
  }
  n_base_dup <- sum(table(base_ok) > 1)

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
      if (n_base_dup > 0) {
        paste0(
          "前期有 ", n_base_dup, " 個 ID 出現不只一次（例：",
          paste(dup_key_examples(baseline_df, id_vars), collapse = "；"), "），",
          "所以「人數」其實是列數。請確認檔案是否重複匯出，或改選能唯一辨識每個人的欄位。"
        )
      },
      if (length(follow_keys) > 0 &&
            length(unique(follow_keys)) / length(follow_keys) < KIT_ID_MIN_UNIQUE) {
        paste0(
          "後期的 ", id_text, " 有 ", length(follow_keys), " 列，卻只有 ",
          length(unique(follow_keys)), " 種值，可能選錯了欄位。"
        )
      },
      if (length(base_ok) > 0 && sum(flag == 0L, na.rm = TRUE) == 0) {
        paste0(
          "前期和後期沒有任何相同的 ID，所以每個人都被標成流失。",
          "請確認兩個檔案的 ID 欄是同一套編號（格式也相同，例如都有或都沒有前導 0）。"
        )
      },
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
