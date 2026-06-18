#' 啟動 KIT 資料串接 Shiny App
#'
#' @param launch.browser 是否在瀏覽器開啟
#' @param ... 傳遞給 [shiny::runApp()]
#' @return 不可見 NULL，side effect 啟動 app
#' @export
run_joinkit <- function(
    launch.browser = getOption("shiny.launch.browser", interactive()),
    ...) {
  options(shiny.maxRequestSize = 500 * 1024^2)
  options(shiny.autoload.r = FALSE)
  app_dir <- system.file("shiny-app", package = "kitJoin")
  if (!nzchar(app_dir) || !dir.exists(app_dir)) {
    stop("找不到 Shiny app。請嘗試：\n  shiny::runApp(\"app.R\")", call. = FALSE)
  }
  shiny::runApp(app_dir, launch.browser = launch.browser, ...)
  invisible(NULL)
}
