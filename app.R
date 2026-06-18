# 開發用入口：從套件根目錄執行
# shiny::runApp("app.R")
options(shiny.maxRequestSize = 500 * 1024^2)
options(shiny.autoload.r = FALSE)
open_browser <- function(url) utils::browseURL(url)

shiny::runApp(
  appDir = "inst/shiny-app",
  launch.browser = open_browser
)
