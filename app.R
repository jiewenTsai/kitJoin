# 入口：Posit Connect Cloud（雲端版）與本機 run.bat 共用
#
# 雲端：Connect Cloud 以本檔為 primary file，回傳 Shiny app 物件（不可呼叫 runApp）。
# 本機：run.bat → scripts/run_app.R → shiny::runApp(本資料夾)。
# 套件：kitJoin::run_kitjoin() 不經過本檔，但同樣呼叫 kit_app()，畫面一致。

# 必須在載入 R/ 之前設定，避免 Shiny 自動 source R/ 到錯誤的環境
options(shiny.autoload.r = FALSE)

# Connect Cloud 以非互動模式執行本檔：未設定 KIT_DEPLOY 時視為雲端版
# （run.bat 會先設定 KIT_DEPLOY = "local"）
if (!nzchar(Sys.getenv("KIT_DEPLOY")) && !interactive()) {
  Sys.setenv(KIT_DEPLOY = "cloud")
}

# 載入 R/ 的所有函式到獨立環境（不污染使用者的 global environment）
kit_env <- new.env(parent = globalenv())
for (f in sort(list.files("R", pattern = "[.]R$", full.names = TRUE))) {
  source(f, local = kit_env, encoding = "UTF-8")
}

options(shiny.launch.browser = kit_env$kit_open_browser)
kit_env$kit_app()
