# 步驟分頁的進度（KIT 設計語言 3.2、4.2）
#
# 做完的分頁打 ✓；還不能用的分頁是灰色，滑過時說明要先做什麼；
# 結果過期（【要重新執行】）時 ✓ 拿掉。規則（每一頁要幾個檔案）是 kitJoin 的；
# 分頁列、狀態的計算與送到瀏覽器用 kitDesign：kit_navgroups()、kit_tab_states()、
# kit_watch_tab_states()（瀏覽器端是 kitDesign 的 kit.js）。

# 分頁列：三組、不編號。①② 是準備；③④⑤⑥ 選一個做；⑦⑧ 是獨立的小工具。
# 名稱是分頁的 value（和以前一樣，updateTabsetPanel() 與各頁的連結都用它）
KIT_TAB_GROUPS <- list(
  kitDesign::kit_navgroup("準備", c(upload = "檔案上傳", missing = "遺漏值處理")),
  kitDesign::kit_navgroup(
    "做出資料（選一個）",
    c(join = "橫向資料串接", stack = "直向波次疊加", attrition = "流失樣本", kitsdata = "kits 作答檔")
  ),
  kitDesign::kit_navgroup("其他工具", c(reshape = "長/寬格式轉換", convert = "檔案格式轉換"),
                          tools = TRUE)
)

# 每個分頁至少要幾個檔案才能用（⑧ 有自己的上傳，和 ① 無關）
KIT_TAB_NEED <- c(
  upload = 0L, missing = 1L, join = 2L, stack = 2L, attrition = 2L,
  kitsdata = 1L, reshape = 1L, convert = 0L
)

#' 每個分頁的狀態（kitJoin 的規則：檔案不夠的分頁變灰）
#'
#' @param n_files ① 已加入的檔案數
#' @param states 具名 list：每個分頁的狀態框狀態（kit_status() 的 state：
#'   "idle"、"success"、"warning"、"error"、"stale"）；沒有給的分頁當成 "idle"
#' @param tabset 分頁的 id（tabsetPanel(id = )）
#' @return kitDesign::kit_tab_states() 的結果：一個分頁一個 list(tabset, value, state, tip)；
#'   state 是 "done"（打 ✓）、"locked"（灰色）、"stale"（要重新執行）或 "ready"
#' @noRd
kitjoin_tab_states <- function(n_files, states = list(), tabset = "tabs") {
  tabs <- names(KIT_TAB_NEED)
  status <- stats::setNames(lapply(tabs, function(tab) states[[tab]] %||% "idle"), tabs)
  locked <- tabs[n_files < KIT_TAB_NEED]
  tips <- vapply(locked, function(tab) kit_tab_locked_tip(n_files, KIT_TAB_NEED[[tab]]), "")
  kit_tab_states(status, locked = locked, locked_tips = tips, tabset = tabset)
}

#' 還不能用的分頁：滑過時的說明
#' @noRd
kit_tab_locked_tip <- function(n_files, need) {
  if (need <= 1L) return("還不能用：先在「檔案上傳」加入檔案")
  if (n_files == 0) return(paste0("還不能用：先在「檔案上傳」加入至少 ", need, " 個檔案"))
  paste0("還不能用：這一頁要 ", need, " 個以上的檔案，先在「檔案上傳」再加入檔案")
}
