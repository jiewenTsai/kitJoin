# 共用小工具

#' 給使用者看的錯誤
#'
#' 訊息需是白話中文、說明原因與怎麼修；UI 會原文顯示，不加技術細節。
#' 其他未預期的錯誤則由 UI 包成「發生未預期的錯誤」並把原始訊息收在技術細節裡。
#' @noRd
kit_abort <- function(...) {
  stop(structure(
    class = c("kit_user_error", "error", "condition"),
    list(message = paste0(...), call = NULL)
  ))
}

#' x 為 NULL 時改用 y（R 4.4 起 base 也有，這裡為了支援較舊的 R）
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x

#' 每列的 ID 合成一個比對用字串；任一 ID 欄缺漏的列回傳 NA
#' @noRd
key_strings <- function(df, by_vars) {
  keys <- lapply(df[by_vars], function(x) as.character(unclass(x)))
  out <- do.call(paste, c(keys, sep = "\r"))
  has_na <- Reduce(`|`, lapply(keys, is.na))
  out[has_na] <- NA_character_
  out
}

#' 「t1（wave1.csv）」這種給使用者看的檔案稱呼
#' @noRd
file_display_name <- function(wave_label, file_name) {
  ifelse(nzchar(wave_label), paste0(wave_label, "（", file_name, "）"), file_name)
}
