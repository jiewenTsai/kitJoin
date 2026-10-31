# 入口：Posit Connect Cloud（雲端版）與本機 run.bat 共用
#
# 雲端：Connect Cloud 以本檔為 primary file，回傳 Shiny app 物件（不可呼叫 runApp）。
# 本機：run.bat → scripts/run_app.R → shiny::runApp(本資料夾)。
# 套件：kitJoin::run_kitjoin() 不經過本檔，但同樣呼叫 kit_app()，畫面一致。

# 不讓 Shiny 自動 source R/：runApp() 在執行本檔之前就會載入 R/，所以真正擋住的是
# R/_disable_autoload.R；這裡再設一次，本檔改過重新載入時也一樣
options(shiny.autoload.r = FALSE)

# Connect Cloud 以非互動模式執行本檔：未設定 KIT_DEPLOY 時視為雲端版
# （run.bat 會先設定 KIT_DEPLOY = "local"）
if (!nzchar(Sys.getenv("KIT_DEPLOY")) && !interactive()) {
  Sys.setenv(KIT_DEPLOY = "cloud")
}

# 載入 R/ 的所有函式到獨立環境（不污染使用者的 global environment）。
# 共用的介面元件在 kitDesign：把它匯出的函式放在上一層（和套件的 import(kitDesign) 一樣），
# R/ 裡直接寫 kit_page()、kit_status() 就找得到
kit_design <- new.env(parent = globalenv())
for (n in getNamespaceExports("kitDesign")) {
  assign(n, getExportedValue("kitDesign", n), envir = kit_design)
}
kit_env <- new.env(parent = kit_design)
for (f in sort(list.files("R", pattern = "^[A-Za-z0-9].*[.]R$", full.names = TRUE))) {
  source(f, local = kit_env, encoding = "UTF-8")
}

options(shiny.launch.browser = kit_env$kit_open_browser)
kit_env$kit_app()
