# 流失樣本

#' @noRd
mod_attrition_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    kit_tab_guide(
      goal = "以前期樣本為準，標記每個人在後期是否還有資料（有 = 留存，沒有 = 流失）。",
      steps = kit_steps(
        "選擇前期樣本（基準）與後期樣本（比對）",
        "選擇用來辨識同一個人的 ID 欄位（前後期欄名不同時，可以分別指定）",
        "按「建立流失樣本」，看結果與預覽",
        "下載 CSV 或 SAV"
      ),
      output = "前期的全部資料，加上一個 attrition 欄：0 = 留存，1 = 流失。列數等於前期樣本列數。",
      note = "後期樣本只用來比對 ID，後期的其他變項不會出現在結果中。",
      technical = "前期的每一列，若其 ID 出現在後期樣本中則為留存；ID 空白的列無法比對，attrition 留空。",
      diagram = attrition_diagram()
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
    shiny::checkboxInput(
      ns("id_per_file"),
      "前期與後期的 ID 欄名不同（例如 w1_id、w4id），分別指定",
      value = FALSE, width = "100%"
    ),
    shiny::fluidRow(
      shiny::column(
        5,
        shiny::selectizeInput(
          ns("id_vars"), "用哪個欄位辨識同一個人？（ID，可多選）",
          choices = NULL, multiple = TRUE,
          options = list(create = FALSE, placeholder = "請選擇 ID 欄位，例如 id"),
          width = "100%"
        )
      ),
      shiny::column(
        5,
        shiny::conditionalPanel(
          "input.id_per_file", ns = ns,
          shiny::selectizeInput(
            ns("follow_id_vars"), "後期樣本的 ID 欄",
            choices = NULL, multiple = TRUE,
            options = list(create = FALSE, placeholder = "請選擇後期的 ID 欄"),
            width = "100%"
          )
        )
      )
    ),
    shiny::tags$small(
      class = "kit-hint",
      "沒有勾選上面的選項時，只列出前期與後期樣本都有的欄位。",
      "括號裡是「不重複的比例」，ID 應該接近 100%。"
    ),
    shiny::uiOutput(ns("settings_line")),
    shiny::uiOutput(ns("ready")),
    kit_button(ns("run"), "建立流失樣本", "primary", disabled = TRUE),
    shiny::uiOutput(ns("status")),
    shiny::uiOutput(ns("result")),
    kit_download_ui(ns),
    kit_details_row(
      shiny::uiOutput(ns("result_details"), inline = TRUE),
      kit_code_panel(ns("code"))
    )
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

    # 前期與後期沒有同名的 ID 欄、但各自都有像 ID 的欄時，自動改成分別指定
    shiny::observeEvent(list(input$baseline, input$followup, files$data()), {
      p <- picked()
      if (is.null(p$baseline) || is.null(p$followup)) return()
      common <- intersect(names(p$baseline), names(p$followup))
      if (length(guess_id_cols(common)) == 0 &&
            length(guess_id_cols(names(p$baseline))) > 0 &&
            length(guess_id_cols(names(p$followup))) > 0) {
        shiny::updateCheckboxInput(session, "id_per_file", value = TRUE)
      }
    })

    shiny::observeEvent(
      list(input$baseline, input$followup, files$data(), input$id_per_file),
      {
        p <- picked()
        has_both <- !is.null(p$baseline) && !is.null(p$followup)
        per_file <- isTRUE(input$id_per_file)
        update_ids <- function(input_id, df, cols) {
          choices <- if (length(cols) > 0) id_choices(list(df), cols) else character(0)
          selected <- intersect(input[[input_id]] %||% character(0), cols)
          if (length(selected) == 0) selected <- default_id_choice(choices)
          shiny::updateSelectizeInput(session, input_id, choices = choices, selected = selected)
        }
        base_cols <- if (!has_both) {
          character(0)
        } else if (per_file) {
          names(p$baseline)
        } else {
          intersect(names(p$baseline), names(p$followup))
        }
        update_ids("id_vars", p$baseline, base_cols)
        if (per_file) {
          update_ids("follow_id_vars", p$followup, if (has_both) names(p$followup) else character(0))
        }
      },
      ignoreNULL = FALSE
    )

    # 後期用的 ID 欄：沒有分別指定時和前期相同
    follow_ids <- shiny::reactive({
      if (isTRUE(input$id_per_file)) input$follow_id_vars %||% character(0) else input$id_vars
    })

    # 主要按鈕：檔案不夠、前後期同一個檔、還沒選 ID 時停用，上面一行寫還差什麼
    kit_run_gate(session, output, missing = shiny::reactive({
      need <- kit_need_files(length(files$keys()), 2L)
      if (!is.null(need)) return(need)
      pos <- picked()$pos
      if (anyNA(pos)) return("選前期與後期樣本")
      if (pos[1] == pos[2]) return("前期與後期要選不同的檔案")
      ids <- input$id_vars %||% character(0)
      if (length(ids) == 0) return("選 ID 欄")
      if (length(follow_ids() %||% character(0)) != length(ids)) {
        return("後期要選一樣多個 ID 欄")
      }
      character(0)
    }), link = function() {
      if (length(files$keys()) < 2) shiny::actionLink(ns("goto_upload"), "到「檔案上傳」加入檔案 →")
    })

    # 設定摘要：「本次：前期 t1（w1.sav）→ 後期 t2（w2.sav） · ID：w1_id → w2_id」
    output$settings_line <- shiny::renderUI({
      if (length(files$keys()) < 2) return(NULL)
      shown <- file_display_name(files$labels(), files$names())
      pos <- picked()$pos
      if (anyNA(pos)) return(NULL)
      ids <- input$id_vars %||% character(0)
      follow <- follow_ids() %||% character(0)
      id_text <- if (length(ids) == 0) {
        "ID：還沒選"
      } else if (isTRUE(input$id_per_file) && !identical(ids, follow)) {
        paste0("ID：", paste(ids, collapse = "＋"), " → ", paste(follow, collapse = "＋"))
      } else {
        paste0("ID：", paste(ids, collapse = "＋"))
      }
      kit_settings_line(c(
        paste0("前期 ", shown[pos[1]], " → 後期 ", shown[pos[2]]),
        id_text, kit_source_short(working$source())
      ))
    })

    shiny::observeEvent(
      list(working$data(), input$baseline, input$followup, input$id_vars, follow_ids()),
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
          kit_abort("需要至少 2 個檔案。請到「檔案上傳」再加入一個波次的檔案。")
        }
        p <- picked()
        if (anyNA(p$pos)) kit_abort("請選擇前期與後期樣本。")
        if (p$pos[1] == p$pos[2]) {
          kit_abort("前期與後期選到同一個檔案了，請把後期樣本改成另一個波次。")
        }
        out <- build_attrition(
          p$baseline, p$followup, input$id_vars,
          follow_id_vars = follow_ids() %||% character(0)
        )
        out$id_vars <- input$id_vars
        result(out)
        n_lost <- sum(out$result[[out$flag_col]] == 1L, na.rm = TRUE)
        kit_done_status(
          paste0(
            "流失樣本建立好了：前期 ", kit_num(nrow(out$result)), " 人中，留存 ",
            kit_num(sum(out$result[[out$flag_col]] == 0L, na.rm = TRUE)),
            " 人、流失 ", kit_num(n_lost), " 人。"
          ),
          warnings = out$warnings, notes = out$notes
        )
      }, error = function(e) {
        kit_error_status(e, "建立流失樣本", kit_upload_todo(ns, length(files$keys()), 2L))
      })
      status(st)
      kit_notify(st)
      kit_scroll_to("status")
    })

    output$status <- shiny::renderUI(make_status_ui(status()))

    kit_download_server(
      session, output,
      data_fn = function() {
        res <- result()
        if (is.null(res)) NULL else reorder_key_first(res$result, res$id_vars)
      },
      filename_fn = timestamped_name("attrition"),
      run_label = "建立流失樣本"
    )

    # 重點：狀態框下面一張人數表；預覽收在下面一排細節裡
    output$result <- shiny::renderUI({
      res <- result()
      if (is.null(res)) return(NULL)
      shiny::tagList(
        shiny::h5("各項人數"),
        shiny::tableOutput(ns("summary"))
      )
    })
    output$result_details <- shiny::renderUI({
      res <- result()
      if (is.null(res)) return(NULL)
      kit_detail(
        "結果預覽",
        shiny::tags$p(
          class = "kit-hint",
          kit_rows_note(nrow(res$result), ncol(res$result), KIT_PREVIEW_ROWS, KIT_PREVIEW_COLS)
        ),
        shiny::tags$div(class = "kit-table-scroll", shiny::tableOutput(ns("preview")))
      )
    })
    output$summary <- shiny::renderTable(
      result()$summary, digits = 0, format.args = list(big.mark = ",")
    )
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
        input$id_vars,
        follow_ids() %||% input$id_vars
      ))
    })

    # 這一頁的狀態（步驟分頁打 ✓ 用）
    list(state = shiny::reactive(status()$state))
  })
}

#' 頁首的圖解：前期的每個人，後期還有沒有資料
#' @noRd
attrition_diagram <- function() {
  kit_diagram(
    before = list(
      kit_dg_table(data.frame(id = c(1, 2, 3), score = c(5, 3, 4)), "前期（t1）"),
      kit_dg_table(data.frame(id = c(1, 3), score = c(6, 2)), "後期（t2）")
    ),
    after = kit_dg_table(
      data.frame(id = c(1, 2, 3), score = c(5, 3, 4), attrition = c("0 留存", "1 流失", "0 留存")),
      "結果", added = "attrition"
    ),
    caption = "以前期的人為準：後期還有資料的是留存（0），沒有的是流失（1）"
  )
}
