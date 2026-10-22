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

#' 是否為雲端版（Posit Connect Cloud）
#'
#' 優先看環境變數 KIT_DEPLOY（"cloud" / "local"，於 Connect Cloud 設定）；
#' 未設定時，以 Posit Connect 預設的 R_CONFIG_ACTIVE = "rsconnect" 判斷。
kit_is_cloud <- function() {
  deploy <- tolower(Sys.getenv("KIT_DEPLOY"))
  if (nzchar(deploy)) return(identical(deploy, "cloud"))
  identical(Sys.getenv("R_CONFIG_ACTIVE"), "rsconnect")
}

#' 單檔上傳上限（MB，依 shiny.maxRequestSize）
kit_max_upload_mb <- function() {
  round(getOption("shiny.maxRequestSize", 5 * 1024^2) / 1024^2)
}

#' 雲端版提醒（資料安全與上傳上限）
kit_cloud_notice <- function() {
  shiny::tags$div(
    class = "alert alert-warning",
    role = "alert",
    shiny::tags$strong("【雲端版提醒】"),
    shiny::tags$ul(
      style = "margin: 0.3rem 0 0 1.2rem; padding: 0;",
      shiny::tags$li(
        "您上傳的資料會傳送至 Posit Connect Cloud 的伺服器處理。",
        "若資料含個人資料或受研究倫理審查（IRB）規範，請先確認可上傳至雲端；",
        "若不確定，請改用本機版。"
      ),
      shiny::tags$li(
        sprintf("單檔上傳上限為 %d MB；雲端運算資源有限，大型檔案建議使用本機版。",
                kit_max_upload_mb())
      ),
      shiny::tags$li(
        "本機版安裝方式請見 ",
        shiny::tags$a(
          href = "https://github.com/jiewenTsai/kitJoin",
          target = "_blank",
          "GitHub：jiewenTsai/kitJoin"
        ),
        "。"
      )
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
