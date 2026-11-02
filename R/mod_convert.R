# 檔案格式轉換：一個檔案進、一個檔案出
#
# 完全獨立的小工具：有自己的上傳，不讀也不改 ①～⑦ 的檔案或任何共用狀態。

# 檔案格式轉換的頁面：骨架是 kitDesign::kit_run_tab_ui()；按鈕一直能按（沒有「還差」），沒有細節列
mod_convert_ui <- function(id) {
  ns <- shiny::NS(id)
  kit_run_tab_ui(
    # 原文：開始轉換
    id, "\u958b\u59cb\u8f49\u63db",
    guide = kit_tab_guide(
      # 原文：把一個 CSV、CSV.GZ 或 SPSS（.sav）檔轉成另一種格式。
      goal = "\u628a\u4e00\u500b CSV\u3001CSV.GZ \u6216 SPSS\uff08.sav\uff09\u6a94\u8f49\u6210\u53e6\u4e00\u7a2e\u683c\u5f0f\u3002",
      steps = kit_steps(
        # 原文：按「選擇檔案…」選一個檔案
        "\u6309\u300c\u9078\u64c7\u6a94\u6848\u2026\u300d\u9078\u4e00\u500b\u6a94\u6848",
        # 原文：選要轉成哪一種格式
        "\u9078\u8981\u8f49\u6210\u54ea\u4e00\u7a2e\u683c\u5f0f",
        # 原文：按「開始轉換」，再按下載
        "\u6309\u300c\u958b\u59cb\u8f49\u63db\u300d\uff0c\u518d\u6309\u4e0b\u8f09"
      ),
      output = paste0(
        # 原文：內容相同、格式不同的新檔。
        "\u5167\u5bb9\u76f8\u540c\u3001\u683c\u5f0f\u4e0d\u540c\u7684\u65b0\u6a94\u3002",
        # 原文：這一頁和其他分頁無關：這裡上傳的檔案不會出現在其他分頁。
        "\u9019\u4e00\u9801\u548c\u5176\u4ed6\u5206\u9801\u7121\u95dc\uff1a\u9019\u88e1\u4e0a\u50b3\u7684\u6a94\u6848\u4e0d\u6703\u51fa\u73fe\u5728\u5176\u4ed6\u5206\u9801\u3002"
      ),
      technical = paste0(
        # 原文：CSV 與 CSV.GZ 互轉時逐段處理、不讀成表格，大檔也不會佔用太多記憶體；
        "CSV \u8207 CSV.GZ \u4e92\u8f49\u6642\u9010\u6bb5\u8655\u7406\u3001\u4e0d\u8b80\u6210\u8868\u683c\uff0c\u5927\u6a94\u4e5f\u4e0d\u6703\u4f54\u7528\u592a\u591a\u8a18\u61b6\u9ad4\uff1b",
        # 原文：UTF-8 檔內容一個位元組都不變（轉成 CSV 時開頭補 BOM，Excel 開中文才不會亂碼），BIG5 檔轉成 UTF-8。
        "UTF-8 \u6a94\u5167\u5bb9\u4e00\u500b\u4f4d\u5143\u7d44\u90fd\u4e0d\u8b8a\uff08\u8f49\u6210 CSV \u6642\u958b\u982d\u88dc BOM\uff0cExcel \u958b\u4e2d\u6587\u624d\u4e0d\u6703\u4e82\u78bc\uff09\uff0cBIG5 \u6a94\u8f49\u6210 UTF-8\u3002",
        # 原文：有 SPSS 檔的轉換要把整個檔案讀進來（haven），大檔建議用本機版。
        "\u6709 SPSS \u6a94\u7684\u8f49\u63db\u8981\u628a\u6574\u500b\u6a94\u6848\u8b80\u9032\u4f86\uff08haven\uff09\uff0c\u5927\u6a94\u5efa\u8b70\u7528\u672c\u6a5f\u7248\u3002",
        # 原文：以 0 開頭的 ID（例如 0012）保留為文字。
        "\u4ee5 0 \u958b\u982d\u7684 ID\uff08\u4f8b\u5982 0012\uff09\u4fdd\u7559\u70ba\u6587\u5b57\u3002"
      )
    ),
    shiny::fileInput(
      ns("file"),
      # 原文：要轉換的檔案
      "\u8981\u8f49\u63db\u7684\u6a94\u6848",
      multiple = FALSE,
      accept = c(".csv", ".csv.gz", ".gz", ".sav", ".zsav"),
      # 原文：選擇檔案…
      buttonLabel = "\u9078\u64c7\u6a94\u6848\u2026",
      # 原文：尚未選擇檔案
      placeholder = "\u5c1a\u672a\u9078\u64c7\u6a94\u6848",
      width = "100%"
    ),
    shiny::tags$small(
      class = "kit-hint",
      sprintf(
        # 原文：支援 CSV（.csv）、CSV.GZ（.csv.gz、.gz）與 SPSS（.sav）。單檔上限 %d MB。
        "\u652f\u63f4 CSV\uff08.csv\uff09\u3001CSV.GZ\uff08.csv.gz\u3001.gz\uff09\u8207 SPSS\uff08.sav\uff09\u3002\u55ae\u6a94\u4e0a\u9650 %d MB\u3002",
        kit_max_upload_mb()
      )
    ),
    shiny::uiOutput(ns("target_ui")),
    gate = FALSE, result = TRUE, downloads = FALSE, details = FALSE, code = FALSE
  )
}

# 檔案格式轉換：狀態流在 kit_run_tab_server()；這裡是格式的選項、轉換、摘要表與下載
mod_convert_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
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
          # 原文：轉成
          ns("target"), "\u8f49\u6210",
          choices = choices, selected = choices[[1]], width = "100%"
        ),
        if (identical(kind(), "sav")) {
          kit_callout(
            # 原文：數值標籤轉成文字（例如 1 → 男）
            shiny::checkboxInput(ns("labels_as_text"), "\u6578\u503c\u6a19\u7c64\u8f49\u6210\u6587\u5b57\uff08\u4f8b\u5982 1 \u2192 \u7537\uff09"),
            shiny::tags$small(
              class = "kit-hint",
              # 原文：CSV 不會保留 SPSS 的變項標籤與數值標籤；不勾選時寫出代碼（1、2…）。
              "CSV \u4e0d\u6703\u4fdd\u7559 SPSS \u7684\u8b8a\u9805\u6a19\u7c64\u8207\u6578\u503c\u6a19\u7c64\uff1b\u4e0d\u52fe\u9078\u6642\u5beb\u51fa\u4ee3\u78bc\uff081\u30012\u2026\uff09\u3002",
              # 原文：SPSS 自訂遺漏值會變成空白。
              "SPSS \u81ea\u8a02\u907a\u6f0f\u503c\u6703\u8b8a\u6210\u7a7a\u767d\u3002"
            )
          )
        }
      )
    })

    tab <- kit_run_tab_server(
      # 原文：開始轉換
      input, output, session, "\u958b\u59cb\u8f49\u63db",
      # 設定摘要：「本次：w1.sav → CSV（數值標籤轉成文字）」
      settings = function() {
        if (is.null(input$file)) return(NULL)
        if (!isTRUE(input$target %in% convert_targets(kind()))) return(NULL)
        paste0(
          # 原文： → 
          input$file$name, " \u2192 ", toupper(input$target),
          # 原文：（數值標籤轉成文字）
          if (identical(kind(), "sav") && isTRUE(input$labels_as_text)) "\uff08\u6578\u503c\u6a19\u7c64\u8f49\u6210\u6587\u5b57\uff09"
        )
      },
      stale_on = function() list(input$file, input$target, input$labels_as_text),
      # 還沒選檔案：按鈕一直能按，所以在這裡說
      guard = function() {
        # 原文：請先按「選擇檔案…」選一個要轉換的檔案。
        if (is.null(input$file)) kit_status("error", "\u8acb\u5148\u6309\u300c\u9078\u64c7\u6a94\u6848\u2026\u300d\u9078\u4e00\u500b\u8981\u8f49\u63db\u7684\u6a94\u6848\u3002")
      },
      # 原文：正在轉換…
      progress = "\u6b63\u5728\u8f49\u63db\u2026",
      run = function() {
        convert_file(
          input$file$datapath, input$file$name, input$target, new_run_dir(),
          labels_as_text = isTRUE(input$labels_as_text)
        )
      },
      done = function(res) {
        # 原文：轉換完成： |  →  | 。
        kit_done_status(paste0("\u8f49\u63db\u5b8c\u6210\uff1a", res$name, " \u2192 ", res$out_name, "\u3002"), notes = res$notes)
      },
      # 原文：轉換
      error = function(e) kit_error_status(e, "\u8f49\u63db"),
      result_ui = function(res) {
        shiny::tagList(
          shiny::tags$div(
            class = "kit-table-scroll",
            shiny::tableOutput(ns("summary_table"))
          ),
          shiny::tags$div(
            class = "kit-downloads",
            kit_download_button(
              # 原文：下載 
              session, "download_file", paste0("\u4e0b\u8f09 ", res$out_name), res$out_name,
              function(file) file.copy(res$out_path, file, overwrite = TRUE),
              class = kit_btn_class("secondary")
            )
          )
        )
      }
    )
    result <- tab$result

    output$summary_table <- shiny::renderTable(
      convert_summary_table(shiny::req(result())),
      striped = TRUE, spacing = "s"
    )

    # 這一頁的狀態（步驟分頁打 ✓ 用）
    list(state = tab$state)
  })
}
