# 關閉 Shiny 自動載入套件 R/（避免函式進入錯誤環境）
options(shiny.autoload.r = FALSE)

source("load_backend.R", local = FALSE)
load_kitjoin_backend()
