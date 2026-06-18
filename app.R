# 開發用入口：從套件根目錄執行
# shiny::runApp("app.R")
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

shiny::runApp(
  appDir = "inst/shiny-app",
  launch.browser = open_kitjoin_browser
)
