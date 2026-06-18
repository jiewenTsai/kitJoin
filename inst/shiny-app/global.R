# 關閉 Shiny 自動載入套件 R/（避免函式進入錯誤環境）
options(shiny.autoload.r = FALSE)

# 尋找後端模組目錄（支援從 inst/shiny-app 或套件根目錄執行）
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
  if (length(hits) == 0) {
    stop("找不到 R/ 後端模組目錄。請從 kitJoin 根目錄執行 shiny::runApp(\"app.R\")")
  }
  hits[1]
}

r_dir <- find_r_dir()
for (f in c("kit_theme.R", "io_data.R", "join_pipeline.R",
            "stack_pipeline.R", "attrition_pipeline.R", "ui_helpers.R")) {
  source(file.path(r_dir, f), local = FALSE)
}
