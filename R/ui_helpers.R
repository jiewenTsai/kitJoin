#' 維度描述：列在前、欄在後
format_dims <- function(nrow, ncol, preview_cols = NULL) {
  base <- sprintf("%d 列 × %d 欄", nrow, ncol)
  if (!is.null(preview_cols)) {
    paste0(base, "，以下僅預覽前 ", preview_cols, " 欄")
  } else {
    base
  }
}

#' 狀態訊息 UI（串接 / 疊加共用）
make_status_ui <- function(st) {
  if (is.null(st) || st$state == "idle") return(NULL)
  prefix <- switch(
    st$state,
    running = "【進行中】",
    success = "【成功】",
    error = "【失敗】",
    ""
  )
  cls <- switch(
    st$state,
    running = "alert alert-info",
    success = "alert alert-success",
    error = "alert alert-danger",
    "alert alert-secondary"
  )
  shiny::tags$div(
    class = cls,
    role = "alert",
    style = "margin-top: 8px;",
    shiny::tags$strong(prefix), " ", st$message
  )
}
