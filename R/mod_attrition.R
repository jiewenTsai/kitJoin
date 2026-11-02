# 流失樣本

# 流失樣本的頁面：骨架是 kitDesign::kit_run_tab_ui()，中間是前期、後期與 ID 的設定
mod_attrition_ui <- function(id) {
  ns <- shiny::NS(id)
  kit_run_tab_ui(
    # 原文：建立流失樣本
    id, "\u5efa\u7acb\u6d41\u5931\u6a23\u672c",
    guide = kit_tab_guide(
      # 原文：以前期樣本為準，標記每個人在後期是否還有資料（有 = 留存，沒有 = 流失）。
      goal = "\u4ee5\u524d\u671f\u6a23\u672c\u70ba\u6e96\uff0c\u6a19\u8a18\u6bcf\u500b\u4eba\u5728\u5f8c\u671f\u662f\u5426\u9084\u6709\u8cc7\u6599\uff08\u6709 = \u7559\u5b58\uff0c\u6c92\u6709 = \u6d41\u5931\uff09\u3002",
      steps = kit_steps(
        # 原文：選擇前期樣本（基準）與後期樣本（比對）
        "\u9078\u64c7\u524d\u671f\u6a23\u672c\uff08\u57fa\u6e96\uff09\u8207\u5f8c\u671f\u6a23\u672c\uff08\u6bd4\u5c0d\uff09",
        # 原文：選擇用來辨識同一個人的 ID 欄位（前後期欄名不同時，可以分別指定）
        "\u9078\u64c7\u7528\u4f86\u8fa8\u8b58\u540c\u4e00\u500b\u4eba\u7684 ID \u6b04\u4f4d\uff08\u524d\u5f8c\u671f\u6b04\u540d\u4e0d\u540c\u6642\uff0c\u53ef\u4ee5\u5206\u5225\u6307\u5b9a\uff09",
        # 原文：按「建立流失樣本」，看結果與預覽
        "\u6309\u300c\u5efa\u7acb\u6d41\u5931\u6a23\u672c\u300d\uff0c\u770b\u7d50\u679c\u8207\u9810\u89bd",
        # 原文：下載 CSV 或 SAV
        "\u4e0b\u8f09 CSV \u6216 SAV"
      ),
      # 原文：前期的全部資料，加上一個 attrition 欄：0 = 留存，1 = 流失。列數等於前期樣本列數。
      output = "\u524d\u671f\u7684\u5168\u90e8\u8cc7\u6599\uff0c\u52a0\u4e0a\u4e00\u500b attrition \u6b04\uff1a0 = \u7559\u5b58\uff0c1 = \u6d41\u5931\u3002\u5217\u6578\u7b49\u65bc\u524d\u671f\u6a23\u672c\u5217\u6578\u3002",
      # 原文：後期樣本只用來比對 ID，後期的其他變項不會出現在結果中。
      note = "\u5f8c\u671f\u6a23\u672c\u53ea\u7528\u4f86\u6bd4\u5c0d ID\uff0c\u5f8c\u671f\u7684\u5176\u4ed6\u8b8a\u9805\u4e0d\u6703\u51fa\u73fe\u5728\u7d50\u679c\u4e2d\u3002",
      # 原文：前期的每一列，若其 ID 出現在後期樣本中則為留存；ID 空白的列無法比對，attrition 留空。
      technical = "\u524d\u671f\u7684\u6bcf\u4e00\u5217\uff0c\u82e5\u5176 ID \u51fa\u73fe\u5728\u5f8c\u671f\u6a23\u672c\u4e2d\u5247\u70ba\u7559\u5b58\uff1bID \u7a7a\u767d\u7684\u5217\u7121\u6cd5\u6bd4\u5c0d\uff0cattrition \u7559\u7a7a\u3002",
      diagram = attrition_diagram()
    ),
    shiny::uiOutput(ns("source_note")),
    shiny::fluidRow(
      shiny::column(
        5,
        # 原文：前期樣本（基準）
        shiny::selectInput(ns("baseline"), "\u524d\u671f\u6a23\u672c\uff08\u57fa\u6e96\uff09", choices = NULL)
      ),
      shiny::column(
        5,
        # 原文：後期樣本（比對）
        shiny::selectInput(ns("followup"), "\u5f8c\u671f\u6a23\u672c\uff08\u6bd4\u5c0d\uff09", choices = NULL)
      )
    ),
    shiny::checkboxInput(
      ns("id_per_file"),
      # 原文：前期與後期的 ID 欄名不同（例如 w1_id、w4id），分別指定
      "\u524d\u671f\u8207\u5f8c\u671f\u7684 ID \u6b04\u540d\u4e0d\u540c\uff08\u4f8b\u5982 w1_id\u3001w4id\uff09\uff0c\u5206\u5225\u6307\u5b9a",
      value = FALSE, width = "100%"
    ),
    shiny::fluidRow(
      shiny::column(
        5,
        shiny::selectizeInput(
          # 原文：用哪個欄位辨識同一個人？（ID，可多選）
          ns("id_vars"), "\u7528\u54ea\u500b\u6b04\u4f4d\u8fa8\u8b58\u540c\u4e00\u500b\u4eba\uff1f\uff08ID\uff0c\u53ef\u591a\u9078\uff09",
          choices = NULL, multiple = TRUE,
          # 原文：請選擇 ID 欄位，例如 id
          options = list(create = FALSE, placeholder = "\u8acb\u9078\u64c7 ID \u6b04\u4f4d\uff0c\u4f8b\u5982 id"),
          width = "100%"
        )
      ),
      shiny::column(
        5,
        shiny::conditionalPanel(
          "input.id_per_file", ns = ns,
          shiny::selectizeInput(
            # 原文：後期樣本的 ID 欄
            ns("follow_id_vars"), "\u5f8c\u671f\u6a23\u672c\u7684 ID \u6b04",
            choices = NULL, multiple = TRUE,
            # 原文：請選擇後期的 ID 欄
            options = list(create = FALSE, placeholder = "\u8acb\u9078\u64c7\u5f8c\u671f\u7684 ID \u6b04"),
            width = "100%"
          )
        )
      )
    ),
    shiny::tags$small(
      class = "kit-hint",
      # 原文：沒有勾選上面的選項時，只列出前期與後期樣本都有的欄位。
      "\u6c92\u6709\u52fe\u9078\u4e0a\u9762\u7684\u9078\u9805\u6642\uff0c\u53ea\u5217\u51fa\u524d\u671f\u8207\u5f8c\u671f\u6a23\u672c\u90fd\u6709\u7684\u6b04\u4f4d\u3002",
      # 原文：括號裡是「不重複的比例」，ID 應該接近 100%。
      "\u62ec\u865f\u88e1\u662f\u300c\u4e0d\u91cd\u8907\u7684\u6bd4\u4f8b\u300d\uff0cID \u61c9\u8a72\u63a5\u8fd1 100%\u3002"
    ),
    result = TRUE
  )
}

# 流失樣本（參數同 mod_join_server()）。狀態流在 kit_run_tab_server()；這裡是前期、後期、ID 的選擇，
# 建立流失樣本、人數表、預覽與下載
mod_attrition_server <- function(id, files, working, goto) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
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

    tab <- kit_run_tab_server(
      # 原文：建立流失樣本
      input, output, session, "\u5efa\u7acb\u6d41\u5931\u6a23\u672c",
      # 主要按鈕：檔案不夠、前後期同一個檔、還沒選 ID 時停用
      ready = shiny::reactive({
        need <- kit_need_files(length(files$keys()), 2L)
        if (!is.null(need)) return(need)
        pos <- picked()$pos
        # 原文：選前期與後期樣本
        if (anyNA(pos)) return("\u9078\u524d\u671f\u8207\u5f8c\u671f\u6a23\u672c")
        # 原文：前期與後期要選不同的檔案
        if (pos[1] == pos[2]) return("\u524d\u671f\u8207\u5f8c\u671f\u8981\u9078\u4e0d\u540c\u7684\u6a94\u6848")
        ids <- input$id_vars %||% character(0)
        # 原文：選 ID 欄
        if (length(ids) == 0) return("\u9078 ID \u6b04")
        if (length(follow_ids() %||% character(0)) != length(ids)) {
          # 原文：後期要選一樣多個 ID 欄
          return("\u5f8c\u671f\u8981\u9078\u4e00\u6a23\u591a\u500b ID \u6b04")
        }
        character(0)
      }),
      ready_link = function() {
        # 原文：到「檔案上傳」加入檔案 →
        if (length(files$keys()) < 2) shiny::actionLink(ns("goto_upload"), "\u5230\u300c\u6a94\u6848\u4e0a\u50b3\u300d\u52a0\u5165\u6a94\u6848 \u2192")
      },
      # 設定摘要：「本次：前期 t1（w1.sav）→ 後期 t2（w2.sav） · ID：w1_id → w2_id」
      settings = function() {
        if (length(files$keys()) < 2) return(NULL)
        shown <- file_display_name(files$labels(), files$names())
        pos <- picked()$pos
        if (anyNA(pos)) return(NULL)
        ids <- input$id_vars %||% character(0)
        follow <- follow_ids() %||% character(0)
        id_text <- if (length(ids) == 0) {
          # 原文：ID：還沒選
          "ID\uff1a\u9084\u6c92\u9078"
        } else if (isTRUE(input$id_per_file) && !identical(ids, follow)) {
          # 原文：ID： | ＋ |  →  | ＋
          paste0("ID\uff1a", paste(ids, collapse = "\uff0b"), " \u2192 ", paste(follow, collapse = "\uff0b"))
        } else {
          # 原文：ID： | ＋
          paste0("ID\uff1a", paste(ids, collapse = "\uff0b"))
        }
        # 原文：前期  |  → 後期 
        c(paste0("\u524d\u671f ", shown[pos[1]], " \u2192 \u5f8c\u671f ", shown[pos[2]]), id_text,
          kit_source_short(working$source()))
      },
      stale_on = function() {
        list(working$data(), input$baseline, input$followup, input$id_vars, follow_ids())
      },
      run = function() {
        if (length(files$keys()) < 2) {
          # 原文：需要至少 2 個檔案。請到「檔案上傳」再加入一個波次的檔案。
          kit_abort("\u9700\u8981\u81f3\u5c11 2 \u500b\u6a94\u6848\u3002\u8acb\u5230\u300c\u6a94\u6848\u4e0a\u50b3\u300d\u518d\u52a0\u5165\u4e00\u500b\u6ce2\u6b21\u7684\u6a94\u6848\u3002")
        }
        p <- picked()
        # 原文：請選擇前期與後期樣本。
        if (anyNA(p$pos)) kit_abort("\u8acb\u9078\u64c7\u524d\u671f\u8207\u5f8c\u671f\u6a23\u672c\u3002")
        if (p$pos[1] == p$pos[2]) {
          # 原文：前期與後期選到同一個檔案了，請把後期樣本改成另一個波次。
          kit_abort("\u524d\u671f\u8207\u5f8c\u671f\u9078\u5230\u540c\u4e00\u500b\u6a94\u6848\u4e86\uff0c\u8acb\u628a\u5f8c\u671f\u6a23\u672c\u6539\u6210\u53e6\u4e00\u500b\u6ce2\u6b21\u3002")
        }
        out <- build_attrition(
          p$baseline, p$followup, input$id_vars,
          follow_id_vars = follow_ids() %||% character(0)
        )
        out$id_vars <- input$id_vars
        out
      },
      done = function(out) {
        flag <- out$result[[out$flag_col]]
        kit_done_status(
          paste0(
            # 原文：流失樣本建立好了：前期  |  人中，留存 
            "\u6d41\u5931\u6a23\u672c\u5efa\u7acb\u597d\u4e86\uff1a\u524d\u671f ", kit_num(nrow(out$result)), " \u4eba\u4e2d\uff0c\u7559\u5b58 ",
            kit_num(sum(flag == 0L, na.rm = TRUE)),
            # 原文： 人、流失  |  人。
            " \u4eba\u3001\u6d41\u5931 ", kit_num(sum(flag == 1L, na.rm = TRUE)), " \u4eba\u3002"
          ),
          warnings = out$warnings, notes = out$notes
        )
      },
      error = function(e) {
        # 原文：建立流失樣本
        kit_error_status(e, "\u5efa\u7acb\u6d41\u5931\u6a23\u672c", kit_upload_todo(ns, length(files$keys()), 2L))
      },
      # 重點：狀態框下面一張人數表；預覽收在細節列
      result_ui = function(res) {
        shiny::tagList(
          # 原文：各項人數
          shiny::h5("\u5404\u9805\u4eba\u6578"),
          shiny::tableOutput(ns("summary"))
        )
      },
      details_ui = function(res) {
        kit_detail(
          # 原文：結果預覽
          "\u7d50\u679c\u9810\u89bd",
          shiny::tags$p(
            class = "kit-hint",
            kit_rows_note(nrow(res$result), ncol(res$result), KIT_PREVIEW_ROWS, KIT_PREVIEW_COLS)
          ),
          shiny::tags$div(class = "kit-table-scroll", shiny::tableOutput(ns("preview")))
        )
      },
      code = function() {
        f <- code_files_from(files, 2L)
        uploaded <- length(files$keys()) >= 2
        pos <- picked()$pos
        code_attrition(
          f,
          if (uploaded) pos[1] else 1L,
          if (uploaded) pos[2] else 2L,
          input$id_vars,
          follow_ids() %||% input$id_vars
        )
      },
      code_note = KIT_CODE_NOTE_JOIN
    )
    result <- tab$result

    kit_download_server(
      session, output,
      data_fn = function() {
        res <- result()
        if (is.null(res)) NULL else reorder_key_first(res$result, res$id_vars)
      },
      filename_fn = timestamped_name("attrition"),
      # 原文：建立流失樣本
      run_label = "\u5efa\u7acb\u6d41\u5931\u6a23\u672c"
    )

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

    # 這一頁的狀態（步驟分頁打 ✓ 用）
    list(state = tab$state)
  })
}

# 頁首的圖解：前期的每個人，後期還有沒有資料
attrition_diagram <- function() {
  kit_diagram(
    before = list(
      # 原文：前期（t1）
      kit_dg_table(data.frame(id = c(1, 2, 3), score = c(5, 3, 4)), "\u524d\u671f\uff08t1\uff09"),
      # 原文：後期（t2）
      kit_dg_table(data.frame(id = c(1, 3), score = c(6, 2)), "\u5f8c\u671f\uff08t2\uff09")
    ),
    after = kit_dg_table(
      # 原文：0 留存 | 1 流失 | 0 留存
      data.frame(id = c(1, 2, 3), score = c(5, 3, 4), attrition = c("0 \u7559\u5b58", "1 \u6d41\u5931", "0 \u7559\u5b58")),
      # 原文：結果
      "\u7d50\u679c", added = "attrition"
    ),
    # 原文：以前期的人為準：後期還有資料的是留存（0），沒有的是流失（1）
    caption = "\u4ee5\u524d\u671f\u7684\u4eba\u70ba\u6e96\uff1a\u5f8c\u671f\u9084\u6709\u8cc7\u6599\u7684\u662f\u7559\u5b58\uff080\uff09\uff0c\u6c92\u6709\u7684\u662f\u6d41\u5931\uff081\uff09"
  )
}
