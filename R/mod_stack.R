# 直向波次疊加

# 直向波次疊加的頁面：骨架是 kitDesign::kit_run_tab_ui()，這一頁沒有自己的設定
mod_stack_ui <- function(id) {
  ns <- shiny::NS(id)
  kit_run_tab_ui(
    # 原文：執行疊加
    id, "\u57f7\u884c\u758a\u52a0",
    guide = kit_tab_guide(
      # 原文：把各波資料上下疊起來：每個人在每一波各佔一列，並用 wave 欄標示是哪一波。
      goal = "\u628a\u5404\u6ce2\u8cc7\u6599\u4e0a\u4e0b\u758a\u8d77\u4f86\uff1a\u6bcf\u500b\u4eba\u5728\u6bcf\u4e00\u6ce2\u5404\u4f54\u4e00\u5217\uff0c\u4e26\u7528 wave \u6b04\u6a19\u793a\u662f\u54ea\u4e00\u6ce2\u3002",
      steps = kit_steps(
        # 原文：確認「檔案上傳」中每個檔案的波次名稱（會填進 wave 欄）
        "\u78ba\u8a8d\u300c\u6a94\u6848\u4e0a\u50b3\u300d\u4e2d\u6bcf\u500b\u6a94\u6848\u7684\u6ce2\u6b21\u540d\u7a31\uff08\u6703\u586b\u9032 wave \u6b04\uff09",
        # 原文：按「執行疊加」，看結果與預覽
        "\u6309\u300c\u57f7\u884c\u758a\u52a0\u300d\uff0c\u770b\u7d50\u679c\u8207\u9810\u89bd",
        # 原文：下載 CSV 或 SAV
        "\u4e0b\u8f09 CSV \u6216 SAV"
      ),
      # 原文：長格式資料：列數等於各檔列數相加；欄位是各檔欄位的聯集，再加上 wave 欄。
      output = "\u9577\u683c\u5f0f\u8cc7\u6599\uff1a\u5217\u6578\u7b49\u65bc\u5404\u6a94\u5217\u6578\u76f8\u52a0\uff1b\u6b04\u4f4d\u662f\u5404\u6a94\u6b04\u4f4d\u7684\u806f\u96c6\uff0c\u518d\u52a0\u4e0a wave \u6b04\u3002",
      technical = paste0(
        # 原文：以 dplyr::bind_rows() 垂直合併：同名欄對齊，只出現在部分檔案的欄位在其他波填 NA；
        "\u4ee5 dplyr::bind_rows() \u5782\u76f4\u5408\u4f75\uff1a\u540c\u540d\u6b04\u5c0d\u9f4a\uff0c\u53ea\u51fa\u73fe\u5728\u90e8\u5206\u6a94\u6848\u7684\u6b04\u4f4d\u5728\u5176\u4ed6\u6ce2\u586b NA\uff1b",
        # 原文：同名欄型別不一致（例如一波數字、一波文字）時統一轉成文字。
        "\u540c\u540d\u6b04\u578b\u5225\u4e0d\u4e00\u81f4\uff08\u4f8b\u5982\u4e00\u6ce2\u6578\u5b57\u3001\u4e00\u6ce2\u6587\u5b57\uff09\u6642\u7d71\u4e00\u8f49\u6210\u6587\u5b57\u3002"
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
      # 原文：執行疊加
      input, output, session, "\u57f7\u884c\u758a\u52a0",
      # 主要按鈕：不到 2 個檔案時停用
      ready = shiny::reactive(kit_need_files(length(files$keys()), 2L)),
      # 原文：到「檔案上傳」加入檔案 →
      ready_link = function() shiny::actionLink(ns("goto_upload"), "\u5230\u300c\u6a94\u6848\u4e0a\u50b3\u300d\u52a0\u5165\u6a94\u6848 \u2192"),
      # 設定摘要：「本次：5 個檔案 · wave 欄填 t1、t2… · 原始資料」
      settings = function() {
        n <- length(files$keys())
        if (n < 2) return(NULL)
        labels <- files$labels()
        c(
          # 原文： 個檔案
          paste0(kit_num(n), " \u500b\u6a94\u6848"),
          # 原文：wave 欄填  | 、
          paste0("wave \u6b04\u586b ", paste(utils::head(labels, 6), collapse = "\u3001"),
                 # 原文：…
                 if (length(labels) > 6) "\u2026"),
          kit_source_short(working$source())
        )
      },
      stale_on = function() list(working$data(), files$labels()),
      # 原文：正在疊加資料…
      progress = "\u6b63\u5728\u758a\u52a0\u8cc7\u6599\u2026",
      run = function() stack_waves(working$data(), files$names(), files$labels()),
      done = function(out) {
        kit_done_status(
          # 原文：疊加完成：結果共  | 。
          paste0("\u758a\u52a0\u5b8c\u6210\uff1a\u7d50\u679c\u5171 ", kit_dims(nrow(out$result), ncol(out$result)), "\u3002"),
          warnings = out$warnings, points = out$summary
        )
      },
      # 原文：疊加
      error = function(e) kit_error_status(e, "\u758a\u52a0", kit_upload_todo(ns, length(files$keys()), 2L)),
      # 重點都在狀態框裡（各檔列數、wave 欄、只在部分波次的欄）；預覽與詳細核對收在細節列
      details_ui = function(res) {
        shiny::tagList(
          kit_detail(
            # 原文：結果預覽
            "\u7d50\u679c\u9810\u89bd",
            shiny::tags$p(class = "kit-hint", shiny::textOutput(ns("preview_meta"), inline = TRUE)),
            shiny::tags$div(class = "kit-table-scroll", shiny::tableOutput(ns("preview")))
          ),
          kit_audit_detail(
            # 原文：最終核對
            shiny::h6("\u6700\u7d42\u6838\u5c0d"),
            shiny::tableOutput(ns("audit_checks")),
            # 原文：各檔案的列數
            shiny::h6("\u5404\u6a94\u6848\u7684\u5217\u6578"),
            shiny::tags$div(class = "kit-table-scroll", shiny::tableOutput(ns("audit_files")))
          )
        )
      },
      code = page_code <- function() code_stack(code_files_from(files, 2L)),
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
      # 原文：執行疊加
      run_label = "\u57f7\u884c\u758a\u52a0"
    )

    output$preview_meta <- shiny::renderText({
      df <- shiny::req(result())$result   # 結果被清掉（例如 ② 重新執行）時不顯示
      kit_rows_note(nrow(df), ncol(df), KIT_PREVIEW_ROWS, KIT_PREVIEW_COLS)
    })
    output$preview <- shiny::renderTable({
      res <- result()
      kit_preview_table(res$result, priority_cols = export_cols(res))
    }, striped = TRUE, spacing = "s")
    output$audit_checks <- shiny::renderTable(
      result()$audit$checks,
      digits = 0,
      # 原文：—
      na = "\u2014",
      format.args = list(big.mark = ",")
    )
    output$audit_files <- shiny::renderTable(
      result()$audit$files, digits = 0, format.args = list(big.mark = ",")
    )

    # 這一頁的狀態（步驟分頁打 ✓ 用）
    list(state = tab$state, result = tab$result, code = page_code)
  })
}

# 頁首的圖解：各波上下疊起來，加上 wave 欄
stack_diagram <- function() {
  kit_diagram(
    before = list(
      # 原文：第 1 波（t1）
      kit_dg_table(data.frame(id = c(1, 2), score = c(5, 3)), "\u7b2c 1 \u6ce2\uff08t1\uff09"),
      # 原文：第 2 波（t2）
      kit_dg_table(data.frame(id = c(1, 3), score = c(6, 4)), "\u7b2c 2 \u6ce2\uff08t2\uff09")
    ),
    after = kit_dg_table(
      data.frame(wave = c("t1", "t1", "t2", "t2"), id = c(1, 2, 1, 3), score = c(5, 3, 6, 4)),
      # 原文：結果
      "\u7d50\u679c", added = "wave"
    ),
    # 原文：上下疊起來：每人每一波一列，新的 wave 欄標示是哪一波
    caption = "\u4e0a\u4e0b\u758a\u8d77\u4f86\uff1a\u6bcf\u4eba\u6bcf\u4e00\u6ce2\u4e00\u5217\uff0c\u65b0\u7684 wave \u6b04\u6a19\u793a\u662f\u54ea\u4e00\u6ce2"
  )
}
