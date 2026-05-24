.ensure_example_sav <- function() {
  exdir <- system.file("extdata", "examples", package = "joinkit")
  if (!nzchar(exdir) || !dir.exists(exdir)) {
    return(invisible(NULL))
  }

  pairs <- list(
    c("example_wave2.csv", "example_wave2.sav"),
    c("example_wave4.csv", "example_wave4.sav")
  )

  for (p in pairs) {
    csv_path <- file.path(exdir, p[[1]])
    sav_path <- file.path(exdir, p[[2]])
    if (file.exists(sav_path) || !file.exists(csv_path)) {
      next
    }
    tryCatch({
      df <- readr::read_csv(csv_path, show_col_types = FALSE)
      haven::write_sav(df, sav_path)
    }, error = function(e) {
      warning("Could not create ", p[[2]], ": ", conditionMessage(e), call. = FALSE)
    })
  }
  invisible(NULL)
}

.onAttach <- function(libname, pkgname) {
  .ensure_example_sav()
}

.onLoad <- function(libname, pkgname) {
  .ensure_example_sav()
}
