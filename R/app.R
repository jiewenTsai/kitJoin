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

# 下載的 log 與 ASCII 備用檔名用的 app 名稱（kitDesign::kit_download_button()）
.onLoad <- function(libname, pkgname) {
  if (is.null(getOption("kitDesign.app"))) options(kitDesign.app = "kitJoin")
}

#' 啟動前的 Shiny 設定（上傳上限）
#' @noRd
kit_set_options <- function() {
  options(
    shiny.maxRequestSize = KIT_UPLOAD_MB * 1024^2,
    shiny.autoload.r = FALSE
  )
}

#' 頁面（每次連線時產生，雲端判斷才會反映目前環境）
#'
#' 主題、樣式、頁首、瀏覽器端的小程式都由 kitDesign::kit_page() 放好；
#' 這裡只加 kitJoin 自己的東西：雲端版提醒、分頁。
#' @noRd
kit_ui <- function(request) {
  meta <- kit_app_description("kitJoin")
  kit_page(
    "KIT 資料處理工具", "臺灣幼兒發展調查資料庫",
    tagline = "跨波次資料處理",
    version = meta$version, date = meta$date, package = "kitJoin",
    if (kit_is_cloud()) kit_cloud_banner(kit_cloud_banner_text()),
    # 分頁列：三組、不編號（kit_navgroups()）；下面是真的 Shiny 分頁，只是把它自己的分頁列藏起來，
    # 所以 input$tabs 與 updateTabsetPanel() 照常可以用
    kit_navgroups(KIT_TAB_GROUPS, "tabs", selected = "upload"),
    shiny::tabsetPanel(
      id = "tabs",
      type = "hidden",
      shiny::tabPanel("檔案上傳", value = "upload", mod_upload_ui("upload")),
      shiny::tabPanel("遺漏值處理", value = "missing", mod_missing_ui("missing")),
      shiny::tabPanel("橫向資料串接", value = "join", mod_join_ui("join")),
      shiny::tabPanel("直向波次疊加", value = "stack", mod_stack_ui("stack")),
      shiny::tabPanel("流失樣本", value = "attrition", mod_attrition_ui("attrition")),
      shiny::tabPanel("kits 作答檔", value = "kitsdata", mod_kitsdata_ui("kitsdata")),
      shiny::tabPanel("長/寬格式轉換", value = "reshape", mod_reshape_ui("reshape")),
      shiny::tabPanel("檔案格式轉換", value = "convert", mod_convert_ui("convert"))
    ),
    if (kit_is_cloud()) kitjoin_cloud_footer()
  )
}

#' 伺服器邏輯：各分頁模組之間只透過 files / working 兩組 reactive 傳資料
#'
#' 每個模組回傳自己的狀態（state）：步驟分頁依它打 ✓、變灰（kitjoin_tab_states()）。
#' @noRd
kit_server <- function(input, output, session) {
  goto <- function(tab) {
    shiny::updateTabsetPanel(session, "tabs", selected = tab)
  }
  files <- mod_upload_server("upload", goto)
  working <- mod_missing_server("missing", files, goto)
  join <- mod_join_server("join", files, working, goto)
  stack <- mod_stack_server("stack", files, working, goto)
  attrition <- mod_attrition_server("attrition", files, working, goto)
  # ⑥ 也用 ② 的代碼清單找疑似遺漏值代碼（只提醒、不改資料；不必先按 ② 的執行）
  kitsdata <- mod_kitsdata_server(
    "kitsdata", files, working, goto,
    # 打字時不要每個字都重算（約 1 秒）：停 0.8 秒再更新
    na_codes = shiny::debounce(shiny::reactive(
      parse_na_codes(input[["missing-codes"]] %||% kit_na_codes_default())$codes
    ), 800)
  )
  reshape <- mod_reshape_server("reshape", files, working, goto)
  # ⑧ 和其他分頁無關：不收 files / working
  convert <- mod_convert_server("convert")

  tab_states <- shiny::reactive(kitjoin_tab_states(
    n_files = length(files$keys()),
    states = list(
      upload = files$state(), missing = working$state(), join = join$state(),
      stack = stack$state(), attrition = attrition$state(), kitsdata = kitsdata$state(),
      reshape = reshape$state(), convert = convert$state()
    )
  ))
  kit_watch_tab_states(session, tab_states)
}
