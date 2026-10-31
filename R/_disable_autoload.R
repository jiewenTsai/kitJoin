# 有這個檔，shiny::runApp() 就不會在執行 app.R 之前自動 source R/
# （app.R 裡的 options(shiny.autoload.r = FALSE) 來不及：否則 R/ 會載入兩次，
# 還會警告「appears to contain an R package」）。R/ 由 app.R 自己載入。
# 檔名以底線開頭：套件安裝與 app.R 都不會載入本檔；內容不會執行，只看檔案在不在。
