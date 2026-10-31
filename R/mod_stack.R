# 直向波次疊加

# 直向波次疊加的頁面：骨架是 kitDesign::kit_run_tab_ui()，這一頁沒有自己的設定
mod_stack_ui <- function(id) {
  ns <- shiny::NS(id)
  kit_run_tab_ui(
    id, "執行疊加",
    guide = kit_tab_guide(
      goal = "把各波資料上下疊起來：每個人在每一波各佔一列，並用 wave 欄標示是哪一波。",
      steps = kit_steps(
        "確認「檔案上傳」中每個檔案的波次名稱（會填進 wave 欄）",
        "按「執行疊加」，看結果與預覽",
        "下載 CSV 或 SAV"
      ),
      output = "長格式資料：列數等於各檔列數相加；欄位是各檔欄位的聯集，再加上 wave 欄。",
      technical = paste0(
        "以 dplyr::bind_rows() 垂直合併：同名欄對齊，只出現在部分檔案的欄位在其他波填 NA；",
        "同名欄型別不一致（例如一波數字、一波文字）時統一轉成文字。"
      ),
      diagram = stack_diagram()
    ),
    shiny::uiOutput(ns("source_note"))
  )
}

# 直向波次疊加：狀態流在 kit_run_tab_server()，這裡只有疊加、狀態框的句子、預覽與下載
mod_stack_server <- function(id, files, working, goto) {
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

    tab <- kit_run_tab_server(
      input, output, session, "執行疊加",
      # 主要按鈕：不到 2 個檔案時停用
      ready = shiny::reactive(kit_need_files(length(files$keys()), 2L)),
      ready_link = function() shiny::actionLink(ns("goto_upload"), "到「檔案上傳」加入檔案 →"),
      # 設定摘要：「本次：5 個檔案 · wave 欄填 t1、t2… · 原始資料」
      settings = function() {
        n <- length(files$keys())
        if (n < 2) return(NULL)
        labels <- files$labels()
        c(
          paste0(kit_num(n), " 個檔案"),
          paste0("wave 欄填 ", paste(utils::head(labels, 6), collapse = "、"),
                 if (length(labels) > 6) "…"),
          kit_source_short(working$source())
        )
      },
      stale_on = function() list(working$data(), files$labels()),
      progress = "正在疊加資料…",
      run = function() stack_waves(working$data(), files$names(), files$labels()),
      done = function(out) {
        kit_done_status(
          paste0("疊加完成：結果共 ", kit_dims(nrow(out$result), ncol(out$result)), "。"),
          warnings = out$warnings, points = out$summary
        )
      },
      error = function(e) kit_error_status(e, "疊加", kit_upload_todo(ns, length(files$keys()), 2L)),
      # 重點都在狀態框裡（各檔列數、wave 欄、只在部分波次的欄）；預覽與詳細核對收在細節列
      details_ui = function(res) {
        shiny::tagList(
          kit_detail(
            "結果預覽",
            shiny::tags$p(class = "kit-hint", shiny::textOutput(ns("preview_meta"), inline = TRUE)),
            shiny::tags$div(class = "kit-table-scroll", shiny::tableOutput(ns("preview")))
          ),
          kit_audit_detail(
            shiny::h6("最終核對"),
            shiny::tableOutput(ns("audit_checks")),
            shiny::h6("各檔案的列數"),
            shiny::tags$div(class = "kit-table-scroll", shiny::tableOutput(ns("audit_files")))
          )
        )
      },
      code = function() code_stack(code_files_from(files, 2L)),
      code_note = KIT_CODE_NOTE_JOIN
    )
    result <- tab$result

    # 匯出時 ID 與波次欄放最前面
    export_cols <- function(res) {
      c(guess_id_cols(names(res$result)), res$wave_col)
    }
    kit_download_server(
      session, output,
      data_fn = function() {
        res <- result()
        if (is.null(res)) NULL else reorder_key_first(res$result, export_cols(res))
      },
      filename_fn = timestamped_name("stacked"),
      run_label = "執行疊加"
    )

    output$preview_meta <- shiny::renderText({
      df <- result()$result
      kit_rows_note(nrow(df), ncol(df), KIT_PREVIEW_ROWS, KIT_PREVIEW_COLS)
    })
    output$preview <- shiny::renderTable({
      res <- result()
      kit_preview_table(res$result, priority_cols = export_cols(res))
    }, striped = TRUE, spacing = "s")
    output$audit_checks <- shiny::renderTable(
      result()$audit$checks,
      digits = 0,
      na = "—",
      format.args = list(big.mark = ",")
    )
    output$audit_files <- shiny::renderTable(
      result()$audit$files, digits = 0, format.args = list(big.mark = ",")
    )

    # 這一頁的狀態（步驟分頁打 ✓ 用）
    list(state = tab$state)
  })
}

# 頁首的圖解：各波上下疊起來，加上 wave 欄
stack_diagram <- function() {
  kit_diagram(
    before = list(
      kit_dg_table(data.frame(id = c(1, 2), score = c(5, 3)), "第 1 波（t1）"),
      kit_dg_table(data.frame(id = c(1, 3), score = c(6, 4)), "第 2 波（t2）")
    ),
    after = kit_dg_table(
      data.frame(wave = c("t1", "t1", "t2", "t2"), id = c(1, 2, 1, 3), score = c(5, 3, 6, 4)),
      "結果", added = "wave"
    ),
    caption = "上下疊起來：每人每一波一列，新的 wave 欄標示是哪一波"
  )
}
