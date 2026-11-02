# 步驟分頁的進度（KIT 設計語言 3.2、4.2）
#
# 做完的分頁打 ✓；還不能用的分頁是灰色，滑過時說明要先做什麼；
# 結果過期（【要重新執行】）時 ✓ 拿掉。規則（每一頁要幾個檔案）是 kitJoin 的；
# 分頁列、狀態的計算與送到瀏覽器用 kitDesign：kit_navgroups()、kit_tab_states()、
# kit_watch_tab_states()（瀏覽器端是 kitDesign 的 kit.js）。

# 分頁列：三組、不編號。①② 是準備；③④⑤⑥ 選一個做；⑦⑧ 是獨立的小工具。
# 名稱是分頁的 value（和以前一樣，updateTabsetPanel() 與各頁的連結都用它）
KIT_TAB_GROUPS <- list(
  # 原文：準備 | 檔案上傳 | 遺漏值處理
  kitDesign::kit_navgroup("\u6e96\u5099", c(upload = "\u6a94\u6848\u4e0a\u50b3", missing = "\u907a\u6f0f\u503c\u8655\u7406")),
  kitDesign::kit_navgroup(
    # 原文：做出資料（選一個）
    "\u505a\u51fa\u8cc7\u6599\uff08\u9078\u4e00\u500b\uff09",
    # 原文：橫向資料串接 | 直向波次疊加 | 流失樣本 | kits 作答檔
    c(join = "\u6a6b\u5411\u8cc7\u6599\u4e32\u63a5", stack = "\u76f4\u5411\u6ce2\u6b21\u758a\u52a0", attrition = "\u6d41\u5931\u6a23\u672c", kitsdata = "kits \u4f5c\u7b54\u6a94")
  ),
  # 原文：其他工具 | 長/寬格式轉換 | 檔案格式轉換
  kitDesign::kit_navgroup("\u5176\u4ed6\u5de5\u5177", c(reshape = "\u9577/\u5bec\u683c\u5f0f\u8f49\u63db", convert = "\u6a94\u6848\u683c\u5f0f\u8f49\u63db"),
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
  # 原文：還不能用：先在「檔案上傳」加入檔案
  if (need <= 1L) return("\u9084\u4e0d\u80fd\u7528\uff1a\u5148\u5728\u300c\u6a94\u6848\u4e0a\u50b3\u300d\u52a0\u5165\u6a94\u6848")
  # 原文：還不能用：先在「檔案上傳」加入至少  |  個檔案
  if (n_files == 0) return(paste0("\u9084\u4e0d\u80fd\u7528\uff1a\u5148\u5728\u300c\u6a94\u6848\u4e0a\u50b3\u300d\u52a0\u5165\u81f3\u5c11 ", need, " \u500b\u6a94\u6848"))
  # 原文：還不能用：這一頁要  |  個以上的檔案，先在「檔案上傳」再加入檔案
  paste0("\u9084\u4e0d\u80fd\u7528\uff1a\u9019\u4e00\u9801\u8981 ", need, " \u500b\u4ee5\u4e0a\u7684\u6a94\u6848\uff0c\u5148\u5728\u300c\u6a94\u6848\u4e0a\u50b3\u300d\u518d\u52a0\u5165\u6a94\u6848")
}
