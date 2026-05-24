# 產生 inst/extdata/examples/ 內 2 個 CSV + 2 個 SAV（混合格式示範）
# 使用：JOINKIT_ROOT=/path/to/joinkit Rscript scripts/create_examples.R

pkg_root <- Sys.getenv("JOINKIT_ROOT", unset = ".")
exdir <- file.path(pkg_root, "inst", "extdata", "examples")
dir.create(exdir, recursive = TRUE, showWarnings = FALSE)

suppressPackageStartupMessages({
  library(readr)
  library(haven)
  library(dplyr)
})

set.seed(36)
ids <- paste0("R", sprintf("%06d", 1:50))

w1 <- tibble(
  release_id = ids,
  baby_sex = sample(1:2, 50, replace = TRUE),
  int_months = round(runif(50, 30, 40), 2),
  pfa0101 = sample(1:5, 50, replace = TRUE)
)
w2 <- tibble(
  release_id = ids,
  cogc01 = sample(1:4, 50, replace = TRUE),
  cogc02 = sample(1:4, 50, replace = TRUE),
  lanb01 = sample(1:4, 50, replace = TRUE)
)
w3 <- tibble(
  release_id = ids,
  socb01 = sample(1:4, 50, replace = TRUE),
  health01 = sample(1:3, 50, replace = TRUE),
  heigh = round(rnorm(50, 95, 8), 1)
)
w4 <- tibble(
  release_id = ids,
  weight = round(rnorm(50, 14, 2), 1),
  growheigh01 = sample(1:3, 50, replace = TRUE),
  famc01 = sample(1:4, 50, replace = TRUE)
)

write_csv(w1, file.path(exdir, "example_wave1.csv"))
write_sav(w2, file.path(exdir, "example_wave2.sav"))
write_csv(w3, file.path(exdir, "example_wave3.csv"))
write_sav(w4, file.path(exdir, "example_wave4.sav"))

message("Created in ", exdir, ": ", paste(list.files(exdir), collapse = ", "))
