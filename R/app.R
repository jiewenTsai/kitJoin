# 組裝整個 app：雲端版（根目錄 app.R）、本機 run.bat、套件 run_kitjoin() 共用

#' 建立 kitJoin Shiny app 物件
#'
#' 三種啟動方式都呼叫這個函式，所以看到的畫面一致；
#' 雲端版會在上方與頁尾多顯示雲端使用須知。
#'
#' @return [shiny::shinyApp()] 物件
#' @export
kit_app <- function() {
  kit_set_options()
  shiny::shinyApp(ui = kit_ui, server = kit_server)
}

#' 啟動前的 Shiny 設定（上傳上限）
#' @noRd
kit_set_options <- function() {
  options(
    shiny.maxRequestSize = KIT_UPLOAD_MB * 1024^2,
    shiny.autoload.r = FALSE
  )
}

#' 在預設瀏覽器開啟網址
#'
#' Windows 上 browser = NULL 會用系統預設瀏覽器開啟（不受 RStudio 檢視器影響）。
#' @noRd
kit_open_browser <- function(url) {
  if (.Platform$OS.type == "windows") {
    utils::browseURL(url, browser = NULL)
  } else {
    utils::browseURL(url)
  }
  invisible(NULL)
}

#' 頁面（每次連線時產生，雲端判斷才會反映目前環境）
#' @noRd
kit_ui <- function(request) {
  bslib::page_fluid(
    theme = kit_theme(),
    title = "KIT 資料串接工具",
    shiny::tags$head(shiny::tags$style(kit_css())),
    kit_header(),
    if (kit_is_cloud()) kit_cloud_banner(),
    shiny::tabsetPanel(
      id = "tabs",
      shiny::tabPanel("① 上傳檔案", value = "upload", mod_upload_ui("upload")),
      shiny::tabPanel(
        "②（選用）遺漏值轉 NA",
        value = "missing",
        mod_missing_ui("missing")
      ),
      shiny::tabPanel("③ 寬格式串接", value = "join", mod_join_ui("join")),
      shiny::tabPanel("④ 長格式疊加", value = "stack", mod_stack_ui("stack")),
      shiny::tabPanel(
        "⑤ 流失樣本",
        value = "attrition",
        mod_attrition_ui("attrition")
      )
    ),
    if (kit_is_cloud()) kit_cloud_footer()
  )
}

#' 伺服器邏輯：各分頁模組之間只透過 files / working 兩組 reactive 傳資料
#' @noRd
kit_server <- function(input, output, session) {
  goto <- function(tab) {
    shiny::updateTabsetPanel(session, "tabs", selected = tab)
  }
  files <- mod_upload_server("upload", goto)
  working <- mod_missing_server("missing", files)
  mod_join_server("join", files, working, goto)
  mod_stack_server("stack", files, working, goto)
  mod_attrition_server("attrition", files, working, goto)
}
