# 入口：本機開發與 Posit Connect Cloud 部署共用
# 本機：shiny::runApp("app.R")（或 shiny::runApp(".")）
# Connect Cloud：以此檔為 primary file，回傳 Shiny app 物件（不可呼叫 runApp）
options(shiny.maxRequestSize = 500 * 1024^2)
options(shiny.autoload.r = FALSE)

open_kitjoin_browser <- function(url) {
  if (.Platform$OS.type == "windows") {
    tryCatch(utils::shell.exec(url), error = function(e) {
      shell(paste0("start \"\" ", shQuote(url, type = "cmd")), wait = FALSE)
    })
  } else {
    utils::browseURL(url)
  }
  invisible(NULL)
}

# Connect Cloud 以本檔為入口且為非互動執行：未設定 KIT_DEPLOY 時視為雲端版
# （本機 run.bat / run_kitjoin() 不經過本檔；在 R console 執行 runApp 為互動模式）
if (!nzchar(Sys.getenv("KIT_DEPLOY")) && !interactive()) {
  Sys.setenv(KIT_DEPLOY = "cloud")
}

# 僅本機 Windows 使用自訂開啟瀏覽器方式；伺服器（Linux）不受影響
if (.Platform$OS.type == "windows") {
  options(shiny.launch.browser = open_kitjoin_browser)
}

shiny::shinyAppDir("inst/shiny-app")
