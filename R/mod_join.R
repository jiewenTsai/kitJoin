# ③ 寬格式串接

#' @noRd
mod_join_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    kit_tab_guide(
      goal = "把各波資料左右並排：每個人一列，各波的變項加上波次名稱（例如 score_t1、score_t2）。",
      steps = kit_steps(
        "選擇用來辨識同一個人的 ID 欄位",
        "選擇要保留哪些人",
        "按「執行串接」，看結果摘要與預覽",
        "下載 CSV 或 SAV"
      ),
      output = "寬格式資料：一人一列；欄位是 ID 加上各波的變項。",
      technical = paste0(
        "依序以 dplyr 的 left_join / inner_join / full_join 合併，ID 空白的列不互相配對；",
        "非 ID 欄位依波次名稱加後綴（t1 → _t1）避免同名衝突。"
      )
    ),
    shiny::uiOutput(ns("source_note")),
    shiny::selectizeInput(
      ns("by_vars"), "用哪個欄位辨識同一個人？（ID，可多選）",
      choices = NULL, multiple = TRUE,
      options = list(create = FALSE, placeholder = "請選擇 ID 欄位，例如 release_id")
    ),
    shiny::tags$small(
      class = "kit-hint",
      "只列出每個檔案都有的欄位。請選每個人唯一的編號，例如 release_id；",
      "不要用姓名，因為可能有人同名。"
    ),
    shiny::radioButtons(
      ns("method"), "要保留哪些人？",
      choices = KIT_JOIN_METHODS, selected = "left_join", width = "100%"
    ),
    join_method_example(),
    shiny::checkboxInput(
      ns("keys_as_text"),
      "ID 型別不一致時（例如一波是數字、一波是文字），自動轉成文字再比對",
      value = TRUE, width = "100%"
    ),
    shiny::actionButton(ns("run"), "執行串接", class = "btn-primary"),
    shiny::uiOutput(ns("status")),
    kit_download_ui(ns),
    shiny::uiOutput(ns("result")),
    kit_code_panel(ns("code"))
  )
}

#' @param files mod_upload_server() 的回傳值
#' @param working mod_missing_server() 的回傳值
#' @param goto function(tab) 切換分頁
#' @noRd
mod_join_server <- function(id, files, working, goto) {
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

    common_cols <- shiny::reactive({
      dfs <- files$data()
      if (length(dfs) == 0) return(character(0))
      purrr::reduce(purrr::map(dfs, names), intersect)
    })
    shiny::observeEvent(common_cols(), {
      common <- common_cols()
      selected <- intersect(input$by_vars %||% character(0), common)
      if (length(selected) == 0) {
        selected <- utils::head(guess_id_cols(common), 1)
      }
      shiny::updateSelectizeInput(
        session,
        "by_vars",
        choices = common,
        selected = selected
      )
    }, ignoreNULL = FALSE)

    # 檔案或設定變動後，舊結果失效
    shiny::observeEvent(
      list(
        working$data(),
        files$labels(),
        input$by_vars,
        input$method,
        input$keys_as_text
      ),
      {
        if (!is.null(result())) {
          result(NULL)
          status(kit_stale_status("執行串接"))
        }
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(input$run, {
      result(NULL)
      st <- tryCatch(
        shiny::withProgress(message = "正在串接資料…", value = 0.3, {
          out <- join_waves(
            working$data(), files$names(), files$labels(), input$by_vars,
            method = input$method, keys_as_text = isTRUE(input$keys_as_text)
          )
          out$by_vars <- input$by_vars
          result(out)
          kit_done_status(
            paste0(
              "串接完成！結果共 ",
              format_dims(nrow(out$result), ncol(out$result)),
              "。"
            ),
            warnings = out$warnings, notes = out$notes
          )
        }),
        error = function(e) kit_error_status(e, "串接")
      )
      status(st)
      kit_notify(st)
    })

    output$status <- shiny::renderUI(make_status_ui(status()))

    kit_download_server(
      output, ns,
      data_fn = function() {
        res <- result()
        if (is.null(res)) NULL else reorder_key_first(res$result, res$by_vars)
      },
      filename_fn = timestamped_name("joined"),
      run_label = "執行串接"
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
          shiny::tags$p(class = "text-muted", res$audit$rule),
          shiny::h6("最終核對"),
          shiny::tableOutput(ns("audit_checks")),
          shiny::h6("各檔案的列數與 ID"),
          shiny::tableOutput(ns("audit_files")),
          shiny::h6("逐步合併"),
          shiny::tableOutput(ns("audit_steps"))
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
    output$preview <- shiny::renderTable(
      kit_preview_table(result()$result, priority_cols = result()$by_vars),
      striped = TRUE, spacing = "s"
    )
    output$audit_checks <- shiny::renderTable(
      result()$audit$checks,
      digits = 0,
      na = "—"
    )
    output$audit_files <- shiny::renderTable(result()$audit$files, digits = 0)
    output$audit_steps <- shiny::renderTable(result()$audit$steps, digits = 0)

    output$code <- shiny::renderUI({
      kit_code_blocks(code_join(
        code_files_from(files, 2L),
        input$by_vars,
        input$method
      ))
    })
  })
}

#' 「詳細核對（進階）」收合區
#' @noRd
kit_audit_accordion <- function(...) {
  shiny::tags$div(
    class = "kit-audit",
    bslib::accordion(
      open = FALSE,
      bslib::accordion_panel(
        title = "詳細核對（進階）",
        icon = shiny::icon("list-check"),
        ...
      )
    )
  )
}

#' 各分頁的「到 ① 上傳檔案」「到 ② 遺漏值轉 NA」連結
#' @noRd
watch_goto_links <- function(input, goto) {
  shiny::observeEvent(input$goto_upload, goto("upload"))
  shiny::observeEvent(input$goto_missing, goto("missing"))
}
