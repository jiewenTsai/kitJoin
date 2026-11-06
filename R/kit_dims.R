# 列數、欄數的句子（「5 橫列 × 7 直欄」）
#
# kitDesign 的 kit_dims()、kit_rows_note() 寫「列」「欄」；kitJoin 的畫面一律寫「橫列」「直欄」，
# 使用者比較不會搞混。這裡是同名的 kitJoin 版本（套件自己的函式優先於 import(kitDesign) 的），
# 只改用字，其他和 kitDesign 0.8.0 相同。kitsUI 不受影響。

#' 「5 橫列 × 7 直欄」
#' @noRd
kit_dims <- function(nrow, ncol, preview_cols = NULL) {
  # 原文： 橫列 ×  |  直欄
  base <- paste0(kit_num(nrow), " \u6a6b\u5217 \u00d7 ", kit_num(ncol), " \u76f4\u6b04")
  if (!is.null(preview_cols) && preview_cols < ncol) {
    # 原文：（以下只預覽前  |  直欄）
    paste0(base, "\uff08\u4ee5\u4e0b\u53ea\u9810\u89bd\u524d ", preview_cols, " \u76f4\u6b04\uff09")
  } else {
    base
  }
}

#' 長表的說明：「共 3,418 橫列 × 25 直欄，以下前 10 橫列、前 20 直欄。」
#' @noRd
kit_rows_note <- function(nrow, ncol = NULL, shown_rows, shown_cols = NULL) {
  # 原文： 橫列
  total <- if (is.null(ncol)) paste0(kit_num(nrow), " \u6a6b\u5217") else kit_dims(nrow, ncol)
  cut_rows <- nrow > shown_rows
  cut_cols <- !is.null(ncol) && !is.null(shown_cols) && ncol > shown_cols
  # 原文：共  | ，以下是全部。
  if (!cut_rows && !cut_cols) return(paste0("\u5171 ", total, "\uff0c\u4ee5\u4e0b\u662f\u5168\u90e8\u3002"))
  # 原文：前  |  橫列 | 全部的橫列
  rows <- if (cut_rows) paste0("\u524d ", kit_num(shown_rows), " \u6a6b\u5217") else "\u5168\u90e8\u7684\u6a6b\u5217"
  # 原文：、前  |  直欄
  cols <- if (cut_cols) paste0("\u3001\u524d ", kit_num(shown_cols), " \u76f4\u6b04")
  # 原文：共  | ，以下 | 。
  paste0("\u5171 ", total, "\uff0c\u4ee5\u4e0b", rows, cols, "\u3002")
}
