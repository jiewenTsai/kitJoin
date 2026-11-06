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
    # 原文：請先選擇用來辨識同一個人的 ID 直欄。
    kit_abort("\u8acb\u5148\u9078\u64c7\u7528\u4f86\u8fa8\u8b58\u540c\u4e00\u500b\u4eba\u7684 ID \u76f4\u6b04\u3002")
  }
  if (length(follow_id_vars) != length(id_vars)) {
    kit_abort(
      # 原文：前期與後期要選一樣多個 ID 直欄：前期選了  |  個，
      "\u524d\u671f\u8207\u5f8c\u671f\u8981\u9078\u4e00\u6a23\u591a\u500b ID \u76f4\u6b04\uff1a\u524d\u671f\u9078\u4e86 ", length(id_vars), " \u500b\uff0c",
      # 原文：後期選了  |  個。
      "\u5f8c\u671f\u9078\u4e86 ", length(follow_id_vars), " \u500b\u3002"
    )
  }
  # 原文：前期 | 後期
  for (side in list(list("\u524d\u671f", baseline_df, id_vars), list("\u5f8c\u671f", followup_df, follow_id_vars))) {
    missing_by <- setdiff(side[[3]], names(side[[2]]))
    if (length(missing_by) > 0) {
      # 原文：樣本沒有  | 、 |  直欄。請重新選 ID 直欄。
      kit_abort(side[[1]], "\u6a23\u672c\u6c92\u6709 ", paste(missing_by, collapse = "\u3001"), " \u76f4\u6b04\u3002\u8acb\u91cd\u65b0\u9078 ID \u76f4\u6b04\u3002")
    }
  }
  # 後期只用來比對 ID：只留 ID 欄並改成前期的欄名
  followup_df <- followup_df[follow_id_vars]
  names(followup_df) <- id_vars
  notes <- if (!identical(follow_id_vars, id_vars)) {
    paste0(
      # 原文：後期的  | ＋ |  對應到前期的 
      "\u5f8c\u671f\u7684 ", paste(follow_id_vars, collapse = "\uff0b"), " \u5c0d\u61c9\u5230\u524d\u671f\u7684 ",
      # 原文：＋ | 。
      paste(id_vars, collapse = "\uff0b"), "\u3002"
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
    # 原文：前期 | 後期
    c("\u524d\u671f", "\u5f8c\u671f"),
    list(id_vars, follow_id_vars)
  )
  if (!is.null(conflict)) {
    converted <- keys_to_text(list(baseline_df, followup_df), id_vars)
    baseline_df <- converted[[1]]
    followup_df <- converted[[2]]
    # 原文：已自動把 ID 轉成文字再比對。
    notes <- c(notes, paste0(conflict, "\u5df2\u81ea\u52d5\u628a ID \u8f49\u6210\u6587\u5b57\u518d\u6bd4\u5c0d\u3002"))
  }

  base_keys <- key_strings(baseline_df, id_vars)
  follow_keys <- key_strings(followup_df, id_vars)
  follow_keys <- follow_keys[!is.na(follow_keys)]
  # 原文：＋
  id_text <- paste(id_vars, collapse = "\uff0b")

  # 前期 ID 要能辨識每一個人：大多數列重複時（例如選到出生年）結果沒有意義
  base_ok <- base_keys[!is.na(base_keys)]
  n_base_unique <- length(unique(base_ok))
  if (length(base_ok) > 0 && n_base_unique / length(base_ok) < KIT_ID_MIN_UNIQUE) {
    kit_abort(
      # 原文：前期的  |  有  |  橫列，卻只有 
      "\u524d\u671f\u7684 ", id_text, " \u6709 ", kit_num(length(base_ok)), " \u6a6b\u5217\uff0c\u537b\u53ea\u6709 ", kit_num(n_base_unique),
      # 原文： 種值，看起來不是用來辨識每個人的 ID（例如出生年、權數）。
      " \u7a2e\u503c\uff0c\u770b\u8d77\u4f86\u4e0d\u662f\u7528\u4f86\u8fa8\u8b58\u6bcf\u500b\u4eba\u7684 ID\uff08\u4f8b\u5982\u51fa\u751f\u5e74\u3001\u6b0a\u6578\uff09\u3002",
      # 原文：請改選每個人都不同的編號直欄。
      "\u8acb\u6539\u9078\u6bcf\u500b\u4eba\u90fd\u4e0d\u540c\u7684\u7de8\u865f\u76f4\u6b04\u3002"
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
    # 原文：項目
    "\u9805\u76ee" = c(
      # 原文：前期樣本橫列數
      "\u524d\u671f\u6a23\u672c\u6a6b\u5217\u6578",
      # 原文：後期樣本不重複 ID 數
      "\u5f8c\u671f\u6a23\u672c\u4e0d\u91cd\u8907 ID \u6578",
      # 原文：留存（ |  = 0）
      paste0("\u7559\u5b58\uff08", flag_col, " = 0\uff09"),
      # 原文：流失（ |  = 1）
      paste0("\u6d41\u5931\uff08", flag_col, " = 1\uff09"),
      # 原文：ID 空白無法比對（ |  空白）
      paste0("ID \u7a7a\u767d\u7121\u6cd5\u6bd4\u5c0d\uff08", flag_col, " \u7a7a\u767d\uff09"),
      # 原文：輸出直欄數（前期全部直欄 +  | ）
      paste0("\u8f38\u51fa\u76f4\u6b04\u6578\uff08\u524d\u671f\u5168\u90e8\u76f4\u6b04 + ", flag_col, "\uff09")
    ),
    # 原文：數值
    "\u6578\u503c" = c(
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
          # 原文：前期有  |  個 ID 出現不只一次（例：
          "\u524d\u671f\u6709 ", kit_num(n_base_dup), " \u500b ID \u51fa\u73fe\u4e0d\u53ea\u4e00\u6b21\uff08\u4f8b\uff1a",
          # 原文：； | ），
          paste(dup_key_examples(baseline_df, id_vars), collapse = "\uff1b"), "\uff09\uff0c",
          # 原文：所以「人數」其實是橫列數。請確認檔案是否重複匯出，或改選能唯一辨識每個人的直欄。
          "\u6240\u4ee5\u300c\u4eba\u6578\u300d\u5176\u5be6\u662f\u6a6b\u5217\u6578\u3002\u8acb\u78ba\u8a8d\u6a94\u6848\u662f\u5426\u91cd\u8907\u532f\u51fa\uff0c\u6216\u6539\u9078\u80fd\u552f\u4e00\u8fa8\u8b58\u6bcf\u500b\u4eba\u7684\u76f4\u6b04\u3002"
        )
      },
      if (length(follow_keys) > 0 &&
            length(unique(follow_keys)) / length(follow_keys) < KIT_ID_MIN_UNIQUE) {
        paste0(
          # 原文：後期的  |  有  |  橫列，卻只有 
          "\u5f8c\u671f\u7684 ", id_text, " \u6709 ", kit_num(length(follow_keys)), " \u6a6b\u5217\uff0c\u537b\u53ea\u6709 ",
          # 原文： 種值，可能選錯了直欄。
          kit_num(length(unique(follow_keys))), " \u7a2e\u503c\uff0c\u53ef\u80fd\u9078\u932f\u4e86\u76f4\u6b04\u3002"
        )
      },
      if (length(base_ok) > 0 && sum(flag == 0L, na.rm = TRUE) == 0) {
        paste0(
          # 原文：前期和後期沒有任何相同的 ID，所以每個人都被標成流失。
          "\u524d\u671f\u548c\u5f8c\u671f\u6c92\u6709\u4efb\u4f55\u76f8\u540c\u7684 ID\uff0c\u6240\u4ee5\u6bcf\u500b\u4eba\u90fd\u88ab\u6a19\u6210\u6d41\u5931\u3002",
          # 原文：請確認兩個檔案的 ID 直欄是同一套編號（格式也相同，例如都有或都沒有前導 0）。
          "\u8acb\u78ba\u8a8d\u5169\u500b\u6a94\u6848\u7684 ID \u76f4\u6b04\u662f\u540c\u4e00\u5957\u7de8\u865f\uff08\u683c\u5f0f\u4e5f\u76f8\u540c\uff0c\u4f8b\u5982\u90fd\u6709\u6216\u90fd\u6c92\u6709\u524d\u5c0e 0\uff09\u3002"
        )
      },
      if (n_na > 0) {
        # 原文：前期有  |  橫列的 ID 是空白，無法判斷是否流失， |  直欄留空。
        paste0("\u524d\u671f\u6709 ", kit_num(n_na), " \u6a6b\u5217\u7684 ID \u662f\u7a7a\u767d\uff0c\u7121\u6cd5\u5224\u65b7\u662f\u5426\u6d41\u5931\uff0c", flag_col, " \u76f4\u6b04\u7559\u7a7a\u3002")
      },
      if (flag_col != "attrition") {
        # 原文：前期檔案已經有 attrition 直欄，為了不覆蓋原資料，結果放在新的  |  直欄。
        paste0("\u524d\u671f\u6a94\u6848\u5df2\u7d93\u6709 attrition \u76f4\u6b04\uff0c\u70ba\u4e86\u4e0d\u8986\u84cb\u539f\u8cc7\u6599\uff0c\u7d50\u679c\u653e\u5728\u65b0\u7684 ", flag_col, " \u76f4\u6b04\u3002")
      }
    ),
    notes = c(
      notes,
      if (n_follow_dup > 0) {
        # 原文：後期有  |  筆重複的 ID，比對時只看「有沒有出現」，不影響結果。
        paste0("\u5f8c\u671f\u6709 ", kit_num(n_follow_dup), " \u7b46\u91cd\u8907\u7684 ID\uff0c\u6bd4\u5c0d\u6642\u53ea\u770b\u300c\u6709\u6c92\u6709\u51fa\u73fe\u300d\uff0c\u4e0d\u5f71\u97ff\u7d50\u679c\u3002")
      }
    )
  )
}
