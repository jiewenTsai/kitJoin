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

open_browser <- function(url) {
  message("開啟瀏覽器：", url)
  utils::browseURL(url)
}

root <- normalizePath(getwd(), mustWork = FALSE)
app_r <- file.path(root, "app.R")
if (!file.exists(app_r)) {
  stop("找不到 app.R，請從 kitJoin 根目錄執行 run.bat。", call. = FALSE)
}

shiny::runApp(app_r, launch.browser = open_browser)
