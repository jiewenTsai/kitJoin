# run.bat 呼叫的啟動腳本：安裝缺少的套件，再開啟根目錄的 app.R
options(repos = c(CRAN = "https://cloud.r-project.org"))

log_path <- NULL

log_msg <- function(...) {
  line <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "  ", paste0(...))
  message(line)
  if (!is.null(log_path)) cat(line, "\n", file = log_path, append = TRUE)
}

get_project_root <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) >= 1 && nzchar(args[1])) {
    p <- gsub('^"+|"+$', "", args[1])
    p <- gsub("[/\\\\]+$", "", p)
    return(normalizePath(p, winslash = "/", mustWork = TRUE))
  }
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg) >= 1) {
    return(dirname(dirname(
      normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/")
    )))
  }
  normalizePath(getwd(), winslash = "/")
}

# 剛裝好的 R 還沒有個人套件資料夾，非互動模式下 install.packages() 會直接失敗；
# 先建立資料夾並放到搜尋路徑最前面
use_user_library <- function() {
  lib <- Sys.getenv("R_LIBS_USER")
  if (!nzchar(lib)) return(invisible(NULL))
  dir.create(lib, recursive = TRUE, showWarnings = FALSE)
  .libPaths(c(lib, .libPaths()))
  invisible(lib)
}

install_if_missing <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) == 0) return(invisible(NULL))
  log_msg("第一次使用，正在安裝套件（約 3–10 分鐘，請勿關閉視窗）：", paste(missing, collapse = ", "))
  tryCatch(
    install.packages(missing, lib = .libPaths()[1]),
    error = function(e) stop("套件安裝失敗：", conditionMessage(e), call. = FALSE)
  )
  still <- missing[
    !vapply(missing, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(still) > 0) {
    stop(
      "無法載入套件：", paste(still, collapse = ", "),
      "。請確認電腦有連上網路後再執行一次 run.bat。",
      call. = FALSE
    )
  }
  invisible(NULL)
}

run_app <- function() {
  root <- get_project_root()
  log_path <<- file.path(root, "kitjoin_log.txt")
  cat("", file = log_path)

  log_msg("kitJoin 資料夾：", root)
  setwd(root)
  if (!file.exists("app.R")) stop("找不到 app.R，請重新解壓縮 ZIP 檔。", call. = FALSE)

  use_user_library()
  install_if_missing(c(
    "shiny", "bslib", "dplyr", "readr", "haven", "purrr", "tibble", "htmltools"
  ))

  log_msg("啟動中，瀏覽器會自動開啟 kitJoin。")
  log_msg("使用期間請不要關閉這個黑色視窗；用完直接關閉視窗即可結束。")
  Sys.setenv(KIT_DEPLOY = "local")
  shiny::runApp(root, launch.browser = TRUE)
}

status <- tryCatch(
  {
    run_app()
    0L
  },
  error = function(e) {
    if (is.null(log_path)) log_path <<- file.path(getwd(), "kitjoin_log.txt")
    log_msg("錯誤：", conditionMessage(e))
    1L
  }
)

quit(save = "no", status = status)
