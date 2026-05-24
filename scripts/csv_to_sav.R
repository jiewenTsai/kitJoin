# 由 inst/extdata/examples 內的 CSV 產生對應 SAV（維護用）
pkg_root <- Sys.getenv("JOINKIT_ROOT", unset = ".")
exdir <- file.path(pkg_root, "inst", "extdata", "examples")
suppressPackageStartupMessages({
  library(readr)
  library(haven)
})
write_sav(read_csv(file.path(exdir, "example_wave2.csv"), show_col_types = FALSE),
          file.path(exdir, "example_wave2.sav"))
write_sav(read_csv(file.path(exdir, "example_wave4.csv"), show_col_types = FALSE),
          file.path(exdir, "example_wave4.sav"))
message("Wrote example_wave2.sav and example_wave4.sav")
