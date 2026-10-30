# 檔案格式轉換：一個檔案進、一個檔案出
#
# 完全獨立的小工具：有自己的上傳，不讀也不改 ①～⑦ 的檔案或任何共用狀態。

#' @noRd
mod_convert_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    kit_tab_guide(
      goal = "把一個 CSV、CSV.GZ 或 SPSS（.sav）檔轉成另一種格式。",
      steps = kit_steps(
        "按「選擇檔案…」選一個檔案",
        "選要轉成哪一種格式",
        "按「開始轉換」，再按下載"
      ),
      output = paste0(
        "內容相同、格式不同的新檔。",
        "這一頁和其他分頁無關：這裡上傳的檔案不會出現在其他分頁。"
      ),
      technical = paste0(
        "CSV 與 CSV.GZ 互轉時逐段處理、不讀成表格，大檔也不會佔用太多記憶體；",
        "UTF-8 檔內容一個位元組都不變，BIG5 檔轉成 UTF-8。",
        "有 SPSS 檔的轉換要把整個檔案讀進來（haven），大檔建議用本機版。",
        "以 0 開頭的 ID（例如 0012）保留為文字。"
      )
    ),
    shiny::fileInput(
      ns("file"),
      "要轉換的檔案",
      multiple = FALSE,
      accept = c(".csv", ".csv.gz", ".gz", ".sav", ".zsav"),
      buttonLabel = "選擇檔案…",
      placeholder = "尚未選擇檔案",
      width = "100%"
    ),
    shiny::tags$small(
      class = "kit-hint",
      sprintf(
        "支援 CSV（.csv）、CSV.GZ（.csv.gz、.gz）與 SPSS（.sav）。單檔上限 %d MB。",
        kit_max_upload_mb()
      )
    ),
    shiny::uiOutput(ns("target_ui")),
    shiny::uiOutput(ns("settings_line")),
    kit_button(ns("run"), "開始轉換", "primary"),
    shiny::uiOutput(ns("status")),
    shiny::uiOutput(ns("result"))
  )
}

#' @noRd
mod_convert_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    result <- shiny::reactiveVal(NULL)
    status <- shiny::reactiveVal(kit_status())
    run_dir <- NULL

    # 每次轉換用新的暫存資料夾；舊的與離線時刪掉，避免大檔佔用伺服器空間
    new_run_dir <- function() {
      if (!is.null(run_dir)) unlink(run_dir, recursive = TRUE)
      run_dir <<- tempfile("kitconvert_")
      dir.create(run_dir)
      run_dir
    }
    session$onSessionEnded(function() {
      if (!is.null(run_dir)) unlink(run_dir, recursive = TRUE)
    })

    kind <- shiny::reactive({
      if (is.null(input$file)) NA_character_ else convert_kind(input$file$name)
    })

    # 只列出另外兩種格式
    output$target_ui <- shiny::renderUI({
      choices <- convert_targets(kind())
      shiny::tagList(
        shiny::radioButtons(
          ns("target"), "轉成",
          choices = choices, selected = choices[[1]], width = "100%"
        ),
        if (identical(kind(), "sav")) {
          kit_callout(
            shiny::checkboxInput(ns("labels_as_text"), "數值標籤轉成文字（例如 1 → 男）"),
            shiny::tags$small(
              class = "kit-hint",
              "CSV 不會保留 SPSS 的變項標籤與數值標籤；不勾選時寫出代碼（1、2…）。",
              "SPSS 自訂遺漏值會變成空白。"
            )
          )
        }
      )
    })

    # 設定摘要：「本次：w1.sav → CSV（數值標籤轉成文字）」
    output$settings_line <- shiny::renderUI({
      if (is.null(input$file)) return(NULL)
      if (!isTRUE(input$target %in% convert_targets(kind()))) return(NULL)
      kit_settings_line(paste0(
        input$file$name, " → ", toupper(input$target),
        if (identical(kind(), "sav") && isTRUE(input$labels_as_text)) "（數值標籤轉成文字）"
      ))
    })

    shiny::observeEvent(list(input$file, input$target, input$labels_as_text), {
      if (!is.null(result())) {
        result(NULL)
        status(kit_stale_status("開始轉換"))
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$run, {
      result(NULL)
      if (is.null(input$file)) {
        st <- kit_status("error", "請先按「選擇檔案…」選一個要轉換的檔案。")
        status(st)
        kit_notify(st)
        return()
      }
      st <- tryCatch(
        shiny::withProgress(message = "正在轉換…", value = 0.3, {
          res <- convert_file(
            input$file$datapath, input$file$name, input$target, new_run_dir(),
            labels_as_text = isTRUE(input$labels_as_text)
          )
          result(res)
          kit_done_status(
            paste0("轉換完成：", res$name, " → ", res$out_name, "。"),
            notes = res$notes
          )
        }),
        error = function(e) kit_error_status(e, "轉換")
      )
      status(st)
      kit_notify(st)
    })

    output$status <- shiny::renderUI(make_status_ui(status()))

    output$result <- shiny::renderUI({
      res <- result()
      if (is.null(res)) return(NULL)
      shiny::tagList(
        shiny::tags$div(
          class = "kit-table-scroll",
          shiny::tableOutput(ns("summary_table"))
        ),
        shiny::tags$div(
          class = "kit-downloads",
          kit_download_button(
            session, "download_file", paste0("下載 ", res$out_name), res$out_name,
            function(file) file.copy(res$out_path, file, overwrite = TRUE),
            class = KIT_BTN[["secondary"]]
          )
        )
      )
    })
    output$summary_table <- shiny::renderTable(
      convert_summary_table(shiny::req(result())),
      striped = TRUE, spacing = "s"
    )

    # 這一頁的狀態（步驟分頁打 ✓ 用）
    list(state = shiny::reactive(status()$state))
  })
}
