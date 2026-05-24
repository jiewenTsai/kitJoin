#' Example data file paths (CSV and SAV mixed)
#'
#' Returns absolute paths to bundled example files for testing joins.
#' Files use a common \code{release_id} key across four waves.
#'
#' @return Named character vector of four file paths.
#' @export
#'
#' @examples
#' \dontrun{
#' example_paths()
#' }
example_paths <- function() {
  .ensure_example_sav()
  exdir <- system.file("extdata", "examples", package = "joinkit")
  if (!nzchar(exdir) || !dir.exists(exdir)) {
    stop("Example data not found. Reinstall joinkit.", call. = FALSE)
  }
  files <- c(
    wave1_csv = "example_wave1.csv",
    wave2_sav = "example_wave2.sav",
    wave3_csv = "example_wave3.csv",
    wave4_sav = "example_wave4.sav"
  )
  paths <- file.path(exdir, files)
  if (!all(file.exists(paths))) {
    stop(
      "Missing example files. From package root run:\n",
      "  Rscript scripts/create_examples.R",
      call. = FALSE
    )
  }
  stats::setNames(paths, names(files))
}
