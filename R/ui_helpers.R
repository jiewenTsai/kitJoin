#' 分頁說明框（目標 / 步驟 / 原理 / 輸出）
#'
#' @param goal  本分頁目標（字串）
#' @param steps 操作步驟（字串或 tags$ol(...)）
#' @param principle 原理說明（字串）
#' @param output 輸出形式（字串）
#' @param note 附注（字串，可 NULL）
kit_tab_guide <- function(goal, steps, principle, output, note = NULL) {
  shiny::tags$div(
    class = "alert alert-success",
    shiny::tags$strong("【本分頁目標】"), " ", goal,
    shiny::tags$hr(style = "margin: 0.5rem 0;"),
    shiny::tags$strong("步驟："), steps,
    shiny::tags$br(),
    shiny::tags$strong("原理："), principle,
    shiny::tags$br(),
    shiny::tags$strong("輸出："), output,
    if (!is.null(note)) shiny::tagList(
      shiny::tags$br(),
      shiny::tags$small(class = "text-muted", note)
    )
  )
}

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
