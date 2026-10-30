# 步驟分頁的進度（KIT 設計語言 3.2、4.2）
#
# 做完的分頁打 ✓；還不能用的分頁是灰色，滑過時說明要先做什麼；
# 結果過期（【要重新執行】）時 ✓ 拿掉。伺服器算好每個分頁的狀態，
# 用 kit-tab-states 訊息送到瀏覽器（kit_app_js()），由瀏覽器替分頁加 class。

# 分頁列：三組、不編號。①② 是準備；③④⑤⑥ 選一個做；⑦⑧ 是獨立的小工具。
# 名稱是分頁的 value（和以前一樣，updateTabsetPanel() 與各頁的連結都用它）
KIT_TAB_GROUPS <- list(
  list(label = "準備", class = NULL,
       tabs = c(upload = "上傳檔案", missing = "遺漏值轉 NA")),
  list(label = "做出資料（選一個）", class = NULL,
       tabs = c(join = "寬格式串接", stack = "長格式疊加", attrition = "流失樣本",
                kitsdata = "kits 作答檔")),
  list(label = "其他工具", class = "tools",
       tabs = c(reshape = "長寬互轉", convert = "檔案格式轉換"))
)

#' 分頁列（KIT 設計語言 3.2）：三組，每組上面一行灰色小字
#'
#' 按下去時由 kit_app_js() 切換藏起來的 tabsetPanel（id = tabset）；切換以後（包括
#' updateTabsetPanel()）再把目前的分頁標成 active。做完打 ✓、還不能用變灰也加在這些連結上。
#' @param tabset tabsetPanel 的 id
#' @param selected 一開始的分頁
#' @noRd
kit_navgroups <- function(tabset = "tabs", selected = "upload") {
  shiny::tags$nav(
    class = "kit-navgroups", `data-tabset` = tabset, `aria-label` = "分頁",
    lapply(KIT_TAB_GROUPS, function(g) {
      shiny::tags$div(
        class = paste(c("kit-navgroup", g$class), collapse = " "),
        shiny::tags$div(class = "kit-navgroup-label", g$label),
        shiny::tags$ul(
          class = "nav nav-tabs",
          lapply(names(g$tabs), function(value) {
            shiny::tags$li(
              class = "nav-item",
              shiny::tags$a(
                class = paste(c("nav-link", if (value == selected) "active"), collapse = " "),
                href = "#", `data-kit-tab` = value,
                `aria-current` = if (value == selected) "page",
                g$tabs[[value]]
              )
            )
          })
        )
      )
    })
  )
}

# 每個分頁至少要幾個檔案才能用（⑧ 有自己的上傳，和 ① 無關）
KIT_TAB_NEED <- c(
  upload = 0L, missing = 1L, join = 2L, stack = 2L, attrition = 2L,
  kitsdata = 1L, reshape = 1L, convert = 0L
)

#' 每個分頁的狀態
#'
#' @param n_files ① 已加入的檔案數
#' @param states 具名 list：每個分頁的狀態框狀態（kit_status() 的 state：
#'   "idle"、"success"、"warning"、"error"、"stale"）；沒有給的分頁當成 "idle"
#' @param tabset 分頁的 id（tabsetPanel(id = )）
#' @return list，一個分頁一個 list(tabset, value, state, tip)；
#'   state 是 "done"（打 ✓）、"locked"（灰色）、"stale"（要重新執行）或 "ready"
#' @noRd
kit_tab_states <- function(n_files, states = list(), tabset = "tabs") {
  purrr::map(names(KIT_TAB_NEED), function(tab) {
    need <- KIT_TAB_NEED[[tab]]
    st <- states[[tab]] %||% "idle"
    state <- if (n_files < need) {
      "locked"
    } else if (st %in% c("success", "warning")) {
      "done"
    } else if (identical(st, "stale")) {
      "stale"
    } else {
      "ready"
    }
    tip <- switch(state,
      locked = kit_tab_locked_tip(n_files, need),
      done = if (identical(st, "warning")) "已完成，但有要注意的地方" else "已完成",
      stale = "設定或檔案改過了：要重新執行",
      NULL
    )
    list(tabset = tabset, value = tab, state = state, tip = tip)
  })
}

#' 還不能用的分頁：滑過時的說明
#' @noRd
kit_tab_locked_tip <- function(n_files, need) {
  if (need <= 1L) return("還不能用：先在「上傳檔案」加入檔案")
  if (n_files == 0) return(paste0("還不能用：先在「上傳檔案」加入至少 ", need, " 個檔案"))
  paste0("還不能用：這一頁要 ", need, " 個以上的檔案，先在「上傳檔案」再加入檔案")
}

#' 把分頁狀態送到瀏覽器（狀態一變就送；瀏覽器端見 kit_app_js()）
#' @param states reactive：kit_tab_states() 的結果
#' @noRd
kit_watch_tab_states <- function(session, states) {
  shiny::observe({
    session$sendCustomMessage("kit-tab-states", states())
  })
}
