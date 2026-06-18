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
  source("load_backend.R", local = FALSE)
  load_kitjoin_backend()
}

source("ui.R", local = FALSE)
source("server.R", local = FALSE)

shinyApp(ui, server)
