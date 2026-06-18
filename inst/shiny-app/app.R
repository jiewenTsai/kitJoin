options(shiny.maxRequestSize = 500 * 1024^2)
options(shiny.autoload.r = FALSE)

library(shiny)
library(bslib)
library(dplyr)
library(readr)
library(haven)
library(purrr)

# global.R 已載入後端模組；若直接 source app.R 則在此補載
if (!exists("kit_theme", mode = "function")) {
  find_r_dir <- function() {
    candidates <- c(
      normalizePath(file.path(getwd(), "R"), mustWork = FALSE),
      normalizePath(file.path(getwd(), "..", "..", "R"), mustWork = FALSE),
      system.file("R", package = "kitJoin")
    )
    hits <- candidates[
      dir.exists(candidates) &
        file.exists(file.path(candidates, "kit_theme.R"))
    ]
    if (length(hits) == 0) stop("找不到 R/ 後端模組目錄")
    hits[1]
  }
  r_dir <- find_r_dir()
  for (f in c("kit_theme.R", "io_data.R", "join_pipeline.R",
              "stack_pipeline.R", "attrition_pipeline.R", "ui_helpers.R")) {
    source(file.path(r_dir, f), local = FALSE)
  }
}

source("ui.R", local = FALSE)
source("server.R", local = FALSE)

shinyApp(ui, server)
