# Windows run.bat 啟動腳本：檢查相依套件並啟動 Shiny App
options(shiny.maxRequestSize = 500 * 1024^2)
options(shiny.autoload.r = FALSE)

cran <- "https://cloud.r-project.org"
pkgs <- c("shiny", "bslib", "dplyr", "readr", "haven", "purrr", "tibble")

missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) {
  message("正在安裝缺少的套件：", paste(missing, collapse = ", "))
  utils::install.packages(
    missing,
    repos = cran,
    dependencies = c("Depends", "Imports", "LinkingTo")
  )
  still_missing <- missing[!vapply(missing, requireNamespace, logical(1), quietly = TRUE)]
  if (length(still_missing) > 0) {
    stop(
      "無法安裝以下套件：", paste(still_missing, collapse = ", "),
      call. = FALSE
    )
  }
}

script_dir <- if (any(grepl("^--file=", args <- commandArgs(trailingOnly = FALSE)))) {
  dirname(sub("^--file=", "", args[grep("^--file=", args)][1]))
} else {
  file.path(getwd(), "scripts")
}

root <- normalizePath(file.path(script_dir, ".."), mustWork = FALSE)
app_dir <- file.path(root, "inst", "shiny-app")
if (!dir.exists(app_dir)) {
  stop("找不到 inst/shiny-app，請確認已完整解壓縮 kitJoin。", call. = FALSE)
}

port <- as.integer(Sys.getenv("KITJOIN_PORT", "7600"))
if (is.na(port) || port <= 0L) port <- 7600L

shiny::runApp(
  appDir = app_dir,
  port = port,
  launch.browser = FALSE
)
