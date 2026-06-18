options(repos = c(CRAN = "https://cloud.r-project.org"))
options(shiny.maxRequestSize = 500 * 1024^2)
options(shiny.autoload.r = FALSE)

log_path <- NULL

log_msg <- function(...) {
  line <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "  ", paste(..., collapse = " "))
  message(line)
  if (!is.null(log_path)) cat(line, "\n", file = log_path, append = TRUE)
}

get_project_root <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) >= 1 && nzchar(args[1])) {
    p <- args[1]
    p <- gsub('^"+|"+$', "", p)
    p <- gsub('[/\\\\]+$', "", p)
    return(normalizePath(p, winslash = "/", mustWork = TRUE))
  }
  file_arg <- sub(
    "^--file=", "",
    commandArgs(trailingOnly = FALSE)[grep("^--file=", commandArgs(trailingOnly = FALSE))]
  )
  if (length(file_arg) >= 1 && nzchar(file_arg[1])) {
    return(dirname(normalizePath(file_arg[1], winslash = "/")))
  }
  normalizePath(getwd(), winslash = "/")
}

install_if_missing <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) == 0) return(invisible(NULL))
  log_msg("Installing missing packages: ", paste(missing, collapse = ", "))
  tryCatch(
    install.packages(missing, dependencies = TRUE),
    error = function(e) stop("Package install failed: ", conditionMessage(e), call. = FALSE)
  )
  still <- missing[!vapply(missing, requireNamespace, logical(1), quietly = TRUE)]
  if (length(still) > 0) stop("Could not load: ", paste(still, collapse = ", "), call. = FALSE)
  invisible(NULL)
}

run_app <- function() {
  root <- get_project_root()
  log_path <<- file.path(root, "kitjoin_log.txt")
  cat("", file = log_path)

  log_msg("Root: ", root)
  setwd(root)

  install_if_missing(c("shiny", "bslib", "dplyr", "readr", "haven", "purrr", "tibble"))

  app_dir <- file.path(root, "inst", "shiny-app")
  if (!dir.exists(app_dir)) stop("App dir not found: ", app_dir, call. = FALSE)

  log_msg("Starting kitJoin...")
  log_msg("(Close this window or press Ctrl+C to stop)")

  shiny::runApp(appDir = app_dir, launch.browser = TRUE)
}

status <- tryCatch(
  { run_app(); 0L },
  error = function(e) {
    if (is.null(log_path)) log_path <<- file.path(getwd(), "kitjoin_log.txt")
    log_msg("Error: ", conditionMessage(e))
    1L
  }
)

quit(save = "no", status = status)
