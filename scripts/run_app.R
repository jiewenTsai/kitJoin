# Windows run.bat 啟動腳本：檢查相依套件並啟動 Shiny App
options(repos = c(CRAN = "https://cloud.r-project.org"))
options(shiny.maxRequestSize = 500 * 1024^2)
options(shiny.autoload.r = FALSE)

# 取得專案根目錄（優先用 run.bat 傳入的引數，避免 %~dp0 的引號 bug）
get_root <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) >= 1 && nzchar(args[1])) {
    p <- args[1]
    p <- gsub('^"+|"+$', "", p)       # 去除首尾引號
    p <- gsub('[/\\\\]+$', "", p)     # 去除尾端斜線
    return(normalizePath(p, winslash = "/", mustWork = TRUE))
  }
  file_arg <- sub(
    "^--file=", "",
    commandArgs(trailingOnly = FALSE)[
      grep("^--file=", commandArgs(trailingOnly = FALSE))
    ]
  )
  if (length(file_arg) >= 1 && nzchar(file_arg[1])) {
    return(normalizePath(dirname(file_arg[1]), winslash = "/"))
  }
  normalizePath(getwd(), winslash = "/")
}

root <- get_root()
setwd(root)

# 自動安裝缺少套件
pkgs <- c("shiny", "bslib", "dplyr", "readr", "haven", "purrr", "tibble")
missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) {
  message("正在安裝缺少的套件：", paste(missing, collapse = ", "))
  install.packages(missing, dependencies = TRUE)
  still <- missing[!vapply(missing, requireNamespace, logical(1), quietly = TRUE)]
  if (length(still) > 0) {
    stop("無法安裝以下套件：", paste(still, collapse = ", "), call. = FALSE)
  }
}

app_dir <- file.path(root, "inst", "shiny-app")
if (!dir.exists(app_dir)) {
  stop("找不到 inst/shiny-app，請確認已完整解壓縮 kitJoin。", call. = FALSE)
}

port <- suppressWarnings(as.integer(Sys.getenv("KITJOIN_PORT", "7600")))
if (is.na(port) || port <= 0L) port <- 7600L

message(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        "  啟動 kitJoin（http://127.0.0.1:", port, "）…")
message("  （關閉此視窗或按 Ctrl+C 可停止 App）")

# launch.browser = FALSE：由 run.bat 的 curl 輪詢負責開瀏覽器
shiny::runApp(
  appDir         = app_dir,
  port           = port,
  launch.browser = FALSE
)
