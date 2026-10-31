# run.bat 呼叫的啟動腳本：照 DESCRIPTION 安裝缺少或版本太舊的套件，再開啟根目錄的 app.R
# （kitJoin 與 kitsUI 共用同一份；改的時候兩邊一起改）
#
# 要裝哪些套件、最低版本，都讀 DESCRIPTION 的 Imports；不在 CRAN 的（Remotes，例如 kits）
# 從 GitHub 裝。只看有沒有裝、不看版本時，舊版的 bslib 沒有 Brite 主題，app 一打開就壞掉。
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

# R 本身就有的套件：不用裝
BASE_PKGS <- c("stats", "tools", "utils", "methods", "graphics", "grDevices", "grid",
               "parallel", "splines", "tcltk", "compiler", "datasets")

#' DESCRIPTION 的 Imports（和 Depends 裡的套件）：套件名稱與最低版本（沒有寫時是 NA）
#' Remotes 寫的套件另外記下 GitHub 的位置（例如 kits = "jiewenTsai/kits"）
read_requirements <- function(desc_file = "DESCRIPTION") {
  d <- read.dcf(desc_file, fields = c("Imports", "Depends", "Remotes"))[1, ]
  items <- unlist(strsplit(paste(d[c("Imports", "Depends")][!is.na(d[c("Imports", "Depends")])],
                                 collapse = ","), ","))
  items <- trimws(gsub("\\s+", " ", items))
  items <- items[nzchar(items)]
  pkg <- sub("^([A-Za-z0-9.]+).*$", "\\1", items)
  min <- ifelse(grepl(">=", items), sub("^.*>=\\s*([0-9.-]+).*$", "\\1", items), NA_character_)
  keep <- !pkg %in% c("R", BASE_PKGS)
  remotes <- if (is.na(d[["Remotes"]])) character(0) else {
    r <- trimws(unlist(strsplit(d[["Remotes"]], ",")))
    r <- sub("@.*$", "", sub("^github::", "", r[nzchar(r)]))
    stats::setNames(r, basename(r))
  }
  data.frame(pkg = pkg[keep], min = min[keep],
             github = unname(remotes[pkg[keep]]), stringsAsFactors = FALSE)
}

#' 已經裝的版本（沒裝時 NA）。只讀 DESCRIPTION、不載入套件：Windows 上載入中的套件不能覆蓋
installed_version <- function(pkg) {
  path <- system.file(package = pkg)
  if (!nzchar(path)) return(NA_character_)
  as.character(utils::packageDescription(pkg, fields = "Version"))
}

#' 哪些要裝：沒裝的，或版本比 DESCRIPTION 寫的舊的
needs_install <- function(req, version_of = installed_version) {
  have <- vapply(req$pkg, version_of, character(1), USE.NAMES = FALSE)
  old <- !is.na(have) & !is.na(req$min) &
    mapply(function(h, m) utils::compareVersion(h, m) < 0, have, ifelse(is.na(req$min), "0", req$min))
  req$have <- have
  req$why <- ifelse(is.na(have), "沒有安裝", ifelse(old, paste0("版本太舊（", have, "，需要 ", req$min, " 以上）"), ""))
  req[is.na(have) | old, , drop = FALSE]
}

install_requirements <- function(req) {
  todo <- needs_install(req)
  if (nrow(todo) == 0) return(invisible(NULL))
  for (i in seq_len(nrow(todo))) log_msg("  ", todo$pkg[i], "：", todo$why[i])
  cran <- todo$pkg[is.na(todo$github)]
  github <- todo[!is.na(todo$github), , drop = FALSE]
  if (length(cran) > 0) {
    log_msg("正在安裝或更新套件（第一次約 3–10 分鐘，請勿關閉視窗）：", paste(cran, collapse = ", "))
    tryCatch(
      install.packages(cran, lib = .libPaths()[1]),
      error = function(e) stop("套件安裝失敗：", conditionMessage(e), call. = FALSE)
    )
  }
  if (nrow(github) > 0) {
    if (!nzchar(system.file(package = "remotes"))) install.packages("remotes", lib = .libPaths()[1])
    for (i in seq_len(nrow(github))) {
      log_msg("從 GitHub 安裝 ", github$pkg[i], "（第一次需要幾分鐘）……")
      tryCatch(
        remotes::install_github(github$github[i], lib = .libPaths()[1], upgrade = "never"),
        error = function(e) stop(github$pkg[i], " 安裝失敗：", conditionMessage(e), call. = FALSE)
      )
    }
  }
  still <- needs_install(req)
  if (nrow(still) > 0) {
    stop(
      "這些套件還是不能用：",
      paste0(still$pkg, "（", still$why, "）", collapse = "、"),
      "。請確認電腦有連上網路後再執行一次 run.bat；",
      "還是不行時，請把這個資料夾裡的 log 檔寄給我們。",
      call. = FALSE
    )
  }
  invisible(NULL)
}

run_app <- function() {
  root <- get_project_root()
  setwd(root)
  if (!file.exists("DESCRIPTION") || !file.exists("app.R")) {
    stop("找不到 app.R 或 DESCRIPTION，請重新解壓縮 ZIP 檔。", call. = FALSE)
  }
  info <- read.dcf("DESCRIPTION", fields = c("Package", "Title"))[1, ]
  log_path <<- file.path(root, paste0(tolower(info[["Package"]]), "_log.txt"))
  cat("", file = log_path)
  log_msg(info[["Package"]], " 資料夾：", root)

  use_user_library()
  install_requirements(read_requirements("DESCRIPTION"))

  log_msg("啟動中，瀏覽器會自動開啟 ", info[["Package"]], "。")
  log_msg("使用期間請不要關閉這個黑色視窗；用完直接關閉視窗即可結束。")
  Sys.setenv(KIT_DEPLOY = "local")
  shiny::runApp(root, launch.browser = TRUE)
}

# 測試時只載入上面的函式（KIT_LAUNCHER_TEST=1），不啟動 app
if (!nzchar(Sys.getenv("KIT_LAUNCHER_TEST"))) {
  status <- tryCatch(
    {
      run_app()
      0L
    },
    error = function(e) {
      if (is.null(log_path)) log_path <<- file.path(getwd(), "kit_log.txt")
      log_msg("錯誤：", conditionMessage(e))
      1L
    }
  )
  quit(save = "no", status = status)
}
