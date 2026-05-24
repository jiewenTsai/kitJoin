#' Launch the KIT data join Shiny application
#'
#' Opens a local Shiny app for uploading CSV or SAV files, joining them by
#' selected ID variables, validating row and column counts, and exporting
#' the merged dataset.
#'
#' @param launch.browser Logical; open the app in a web browser.
#' @param ... Additional arguments passed to [shiny::runApp()].
#'
#' @return Invisibly returns `NULL`. Called for side effects.
#'
#' @examples
#' \dontrun{
#' run_joinkit()
#' }
#'
#' @export
run_joinkit <- function(
    launch.browser = getOption("shiny.launch.browser", interactive()),
    ...) {
  app_dir <- system.file("shiny-app", package = "joinkit")
  if (!nzchar(app_dir) || !dir.exists(app_dir)) {
    stop(
      "Could not find the Shiny app. Try reinstalling joinkit:\n",
      "  remotes::install_github(\"YOUR_GITHUB_USER/joinkit\")",
      call. = FALSE
    )
  }
  shiny::runApp(app_dir, launch.browser = launch.browser, ...)
  invisible(NULL)
}
