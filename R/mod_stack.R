# ④ 長格式疊加

#' @noRd
mod_stack_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    kit_tab_guide(
      goal = "把各波資料上下疊起來：每個人在每一波各佔一列，並用 wave 欄標示是哪一波。",
      steps = kit_steps(
        "確認「① 上傳檔案」中每個檔案的波次名稱（會填進 wave 欄）",
        "按「執行疊加」，看結果摘要與預覽",
        "下載 CSV 或 SAV"
      ),
      output = "長格式資料：列數等於各檔列數相加；欄位是各檔欄位的聯集，再加上 wave 欄。",
      technical = paste0(
        "以 dplyr::bind_rows() 垂直合併：同名欄對齊，只出現在部分檔案的欄位在其他波填 NA；",
        "同名欄型別不一致（例如一波數字、一波文字）時統一轉成文字。"
      )
    ),
    shiny::uiOutput(ns("source_note")),
    shiny::actionButton(ns("run"), "執行疊加", class = "btn-primary"),
    shiny::uiOutput(ns("status")),
    kit_download_ui(ns),
    shiny::uiOutput(ns("result")),
    kit_code_panel(ns("code"))
  )
}

#' @inheritParams mod_join_server
#' @noRd
mod_stack_server <- function(id, files, working, goto) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    result <- shiny::reactiveVal(NULL)
    status <- shiny::reactiveVal(kit_status())
    watch_goto_links(input, goto)

    output$source_note <- shiny::renderUI({
      kit_source_note_ui(
        length(files$keys()),
        working$source(),
        working$code_cells(),
        ns
      )
    })

    shiny::observeEvent(list(working$data(), files$labels()), {
      if (!is.null(result())) {
        result(NULL)
        status(kit_stale_status("執行疊加"))
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$run, {
      result(NULL)
      st <- tryCatch(
        shiny::withProgress(message = "正在疊加資料…", value = 0.3, {
          out <- stack_waves(working$data(), files$names(), files$labels())
          result(out)
          kit_done_status(
            paste0(
              "疊加完成！結果共 ",
              format_dims(nrow(out$result), ncol(out$result)),
              "。"
            ),
            warnings = out$warnings
          )
        }),
        error = function(e) kit_error_status(e, "疊加")
      )
      status(st)
      kit_notify(st)
    })

    output$status <- shiny::renderUI(make_status_ui(status()))

    # 匯出時 ID 與波次欄放最前面
    export_cols <- function(res) {
      c(guess_id_cols(names(res$result)), res$wave_col)
    }
    kit_download_server(
      session, output,
      data_fn = function() {
        res <- result()
        if (is.null(res)) {
          NULL
        } else {
          reorder_key_first(res$result, export_cols(res))
        }
      },
      filename_fn = timestamped_name("stacked"),
      run_label = "執行疊加"
    )

    output$result <- shiny::renderUI({
      res <- result()
      if (is.null(res)) return(NULL)
      shiny::tagList(
        kit_summary_ui(res$summary),
        shiny::h4("結果預覽"),
        shiny::textOutput(ns("preview_meta")),
        shiny::tags$div(
          class = "kit-table-scroll",
          shiny::tableOutput(ns("preview"))
        ),
        kit_audit_accordion(
          shiny::h6("最終核對"),
          shiny::tableOutput(ns("audit_checks")),
          shiny::h6("各檔案的列數"),
          shiny::tableOutput(ns("audit_files"))
        )
      )
    })

    output$preview_meta <- shiny::renderText({
      df <- result()$result
      paste0(
        format_dims(nrow(df), ncol(df), KIT_PREVIEW_COLS),
        "；以下為前 ", KIT_PREVIEW_ROWS, " 列。"
      )
    })
    output$preview <- shiny::renderTable({
      res <- result()
      kit_preview_table(res$result, priority_cols = export_cols(res))
    }, striped = TRUE, spacing = "s")
    output$audit_checks <- shiny::renderTable(
      result()$audit$checks,
      digits = 0,
      na = "—"
    )
    output$audit_files <- shiny::renderTable(result()$audit$files, digits = 0)

    output$code <- shiny::renderUI({
      kit_code_blocks(code_stack(code_files_from(files, 2L)))
    })
  })
}
