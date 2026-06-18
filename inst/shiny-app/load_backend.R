# 載入後端模組（開發：source R/；已安裝套件：從 kitJoin 命名空間匯入）
load_kitjoin_backend <- function() {
  if (exists("read_data_file", mode = "function")) {
    return(invisible(NULL))
  }

  if ("kitJoin" %in% loadedNamespaces()) {
    pkg_ns <- asNamespace("kitJoin")
    for (nm in ls(pkg_ns, all.names = TRUE)) {
      obj <- get(nm, envir = pkg_ns)
      if (is.function(obj)) {
        assign(nm, obj, envir = .GlobalEnv)
      }
    }
    return(invisible(NULL))
  }

  r_dir <- find_r_dir()
  for (f in c("kit_theme.R", "io_data.R", "join_pipeline.R",
              "stack_pipeline.R", "attrition_pipeline.R", "ui_helpers.R")) {
    source(file.path(r_dir, f), local = FALSE)
  }
  invisible(NULL)
}

find_r_dir <- function() {
  candidates <- c(
    normalizePath(file.path(getwd(), "R"), mustWork = FALSE),
    normalizePath(file.path(getwd(), "..", "..", "R"), mustWork = FALSE)
  )
  hits <- candidates[
    dir.exists(candidates) &
      file.exists(file.path(candidates, "kit_theme.R"))
  ]
  if (length(hits) == 0) {
    stop(
      "找不到 R/ 後端模組目錄。請從 kitJoin 根目錄執行 shiny::runApp(\"app.R\")",
      "，或透過 kitJoin::run_kitjoin() 啟動。",
      call. = FALSE
    )
  }
  hits[1]
}
