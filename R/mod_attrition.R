# ⑤ 流失樣本

#' @noRd
mod_attrition_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    kit_tab_guide(
      goal = "以前期樣本為準，標記每個人在後期是否還有資料（有 = 留存，沒有 = 流失）。",
      steps = kit_steps(
        "選擇前期樣本（基準）與後期樣本（比對）",
        "選擇用來辨識同一個人的 ID 欄位",
        "按「建立流失樣本」，看摘要與預覽",
        "下載 CSV 或 SAV"
      ),
      output = "前期的全部資料，加上一個 attrition 欄：0 = 留存，1 = 流失。列數等於前期樣本列數。",
      note = "後期樣本只用來比對 ID，後期的其他變項不會出現在結果中。",
      technical = "前期的每一列，若其 ID 出現在後期樣本中則為留存；ID 空白的列無法比對，attrition 留空。"
    ),
    shiny::uiOutput(ns("source_note")),
    shiny::fluidRow(
      shiny::column(
        5,
        shiny::selectInput(ns("baseline"), "前期樣本（基準）", choices = NULL)
      ),
      shiny::column(
        5,
        shiny::selectInput(ns("followup"), "後期樣本（比對）", choices = NULL)
      )
    ),
    shiny::selectizeInput(
      ns("id_vars"), "用哪個欄位辨識同一個人？（ID，可多選）",
      choices = NULL, multiple = TRUE,
      options = list(create = FALSE, placeholder = "請選擇 ID 欄位，例如 release_id"),
      width = "60%"
    ),
    shiny::tags$small(class = "kit-hint", "只列出前期與後期樣本都有的欄位。"),
    shiny::br(),
    shiny::actionButton(ns("run"), "建立流失樣本", class = "btn-primary"),
    shiny::uiOutput(ns("status")),
    kit_download_ui(ns),
    shiny::uiOutput(ns("result")),
    kit_code_panel(ns("code"))
  )
}

#' @inheritParams mod_join_server
#' @noRd
mod_attrition_server <- function(id, files, working, goto) {
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

    # 前期預設第一個檔案、後期預設第二個；保留使用者原本的選擇
    shiny::observeEvent(list(files$keys(), files$names(), files$labels()), {
      choices <- stats::setNames(
        files$keys(),
        file_display_name(files$labels(), files$names())
      )
      keep <- function(prev, fallback) {
        if (!is.null(prev) && prev %in% choices) prev else fallback
      }
      base <- keep(input$baseline, unname(choices[1]))
      follow <- keep(input$followup, unname(setdiff(choices, base)[1]))
      shiny::updateSelectInput(
        session,
        "baseline",
        choices = choices,
        selected = base
      )
      shiny::updateSelectInput(
        session,
        "followup",
        choices = choices,
        selected = follow
      )
    }, ignoreNULL = FALSE)

    picked <- shiny::reactive({
      pos <- match(
        c(input$baseline %||% "", input$followup %||% ""),
        files$keys()
      )
      dfs <- working$data()
      list(
        pos = pos,
        baseline = if (!is.na(pos[1])) dfs[[pos[1]]],
        followup = if (!is.na(pos[2])) dfs[[pos[2]]]
      )
    })

    shiny::observeEvent(list(input$baseline, input$followup, files$data()), {
      p <- picked()
      cols <- if (is.null(p$baseline) || is.null(p$followup)) {
        character(0)
      } else {
        intersect(names(p$baseline), names(p$followup))
      }
      selected <- intersect(input$id_vars %||% character(0), cols)
      if (length(selected) == 0) selected <- utils::head(guess_id_cols(cols), 1)
      shiny::updateSelectizeInput(
        session,
        "id_vars",
        choices = cols,
        selected = selected
      )
    }, ignoreNULL = FALSE)

    shiny::observeEvent(
      list(working$data(), input$baseline, input$followup, input$id_vars),
      {
        if (!is.null(result())) {
          result(NULL)
          status(kit_stale_status("建立流失樣本"))
        }
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(input$run, {
      result(NULL)
      st <- tryCatch({
        if (length(files$keys()) < 2) {
          kit_abort("需要至少 2 個檔案。請到「① 上傳檔案」再加入一個波次的檔案。")
        }
        p <- picked()
        if (anyNA(p$pos)) kit_abort("請選擇前期與後期樣本。")
        if (p$pos[1] == p$pos[2]) {
          kit_abort("前期與後期選到同一個檔案了，請把後期樣本改成另一個波次。")
        }
        out <- build_attrition(p$baseline, p$followup, input$id_vars)
        out$id_vars <- input$id_vars
        result(out)
        n_lost <- sum(out$result[[out$flag_col]] == 1L, na.rm = TRUE)
        kit_done_status(
          paste0(
            "完成！前期 ", nrow(out$result), " 人中，留存 ",
            sum(out$result[[out$flag_col]] == 0L, na.rm = TRUE),
            " 人、流失 ", n_lost, " 人。"
          ),
          warnings = out$warnings, notes = out$notes
        )
      }, error = function(e) kit_error_status(e, "建立流失樣本"))
      status(st)
      kit_notify(st)
    })

    output$status <- shiny::renderUI(make_status_ui(status()))

    kit_download_server(
      output, ns,
      data_fn = function() {
        res <- result()
        if (is.null(res)) NULL else reorder_key_first(res$result, res$id_vars)
      },
      filename_fn = timestamped_name("attrition"),
      run_label = "建立流失樣本"
    )

    output$result <- shiny::renderUI({
      res <- result()
      if (is.null(res)) return(NULL)
      shiny::tagList(
        shiny::h4("摘要"),
        shiny::tableOutput(ns("summary")),
        shiny::h4("結果預覽"),
        shiny::tags$div(
          class = "kit-table-scroll",
          shiny::tableOutput(ns("preview"))
        )
      )
    })
    output$summary <- shiny::renderTable(result()$summary, digits = 0)
    output$preview <- shiny::renderTable({
      res <- result()
      kit_preview_table(
        res$result,
        priority_cols = c(res$id_vars, res$flag_col)
      )
    }, striped = TRUE, spacing = "s")

    output$code <- shiny::renderUI({
      f <- code_files_from(files, 2L)
      uploaded <- length(files$keys()) >= 2
      pos <- picked()$pos
      kit_code_blocks(code_attrition(
        f,
        if (uploaded) pos[1] else 1L,
        if (uploaded) pos[2] else 2L,
        input$id_vars
      ))
    })
  })
}
