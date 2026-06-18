# 開發用入口：從套件根目錄執行
# shiny::runApp("app.R")
options(shiny.maxRequestSize = 500 * 1024^2)
options(shiny.autoload.r = FALSE)
shiny::runApp(
  appDir = "inst/shiny-app",
  launch.browser = interactive()
)
