# 橫向資料串接

# 橫向資料串接的頁面：骨架是 kitDesign::kit_run_tab_ui()，中間是 ID 與串接方法的設定
mod_join_ui <- function(id) {
  ns <- shiny::NS(id)
  kit_run_tab_ui(
    # 原文：執行串接
    id, "\u57f7\u884c\u4e32\u63a5",
    guide = kit_tab_guide(
      # 原文：把各波資料左右並排：每個人一列，各波的變項加上波次名稱（例如 score_t1、score_t2）。
      goal = "\u628a\u5404\u6ce2\u8cc7\u6599\u5de6\u53f3\u4e26\u6392\uff1a\u6bcf\u500b\u4eba\u4e00\u5217\uff0c\u5404\u6ce2\u7684\u8b8a\u9805\u52a0\u4e0a\u6ce2\u6b21\u540d\u7a31\uff08\u4f8b\u5982 score_t1\u3001score_t2\uff09\u3002",
      steps = kit_steps(
        # 原文：選擇用來辨識同一個人的 ID 欄位（各檔欄名不同時，可以分別指定）
        "\u9078\u64c7\u7528\u4f86\u8fa8\u8b58\u540c\u4e00\u500b\u4eba\u7684 ID \u6b04\u4f4d\uff08\u5404\u6a94\u6b04\u540d\u4e0d\u540c\u6642\uff0c\u53ef\u4ee5\u5206\u5225\u6307\u5b9a\uff09",
        # 原文：選擇要保留哪些人
        "\u9078\u64c7\u8981\u4fdd\u7559\u54ea\u4e9b\u4eba",
        # 原文：按「執行串接」，看結果與預覽
        "\u6309\u300c\u57f7\u884c\u4e32\u63a5\u300d\uff0c\u770b\u7d50\u679c\u8207\u9810\u89bd",
        # 原文：下載 CSV 或 SAV
        "\u4e0b\u8f09 CSV \u6216 SAV"
      ),
      # 原文：寬格式資料：一人一列；欄位是 ID 加上各波的變項。
      output = "\u5bec\u683c\u5f0f\u8cc7\u6599\uff1a\u4e00\u4eba\u4e00\u5217\uff1b\u6b04\u4f4d\u662f ID \u52a0\u4e0a\u5404\u6ce2\u7684\u8b8a\u9805\u3002",
      technical = paste0(
        # 原文：依序以 dplyr 的 left_join / inner_join / full_join 合併，ID 空白的列不互相配對；
        "\u4f9d\u5e8f\u4ee5 dplyr \u7684 left_join / inner_join / full_join \u5408\u4f75\uff0cID \u7a7a\u767d\u7684\u5217\u4e0d\u4e92\u76f8\u914d\u5c0d\uff1b",
        # 原文：非 ID 欄位依波次名稱加後綴（t1 → _t1）避免同名衝突。
        "\u975e ID \u6b04\u4f4d\u4f9d\u6ce2\u6b21\u540d\u7a31\u52a0\u5f8c\u7db4\uff08t1 \u2192 _t1\uff09\u907f\u514d\u540c\u540d\u885d\u7a81\u3002"
      ),
      diagram = join_diagram()
    ),
    shiny::uiOutput(ns("source_note")),
    shiny::checkboxInput(
      ns("id_per_file"),
      # 原文：各檔的 ID 欄名不同（例如 w1_id、w2_id、w3id），分別指定每個檔案的 ID 欄
      "\u5404\u6a94\u7684 ID \u6b04\u540d\u4e0d\u540c\uff08\u4f8b\u5982 w1_id\u3001w2_id\u3001w3id\uff09\uff0c\u5206\u5225\u6307\u5b9a\u6bcf\u500b\u6a94\u6848\u7684 ID \u6b04",
      value = FALSE, width = "100%"
    ),
    shiny::conditionalPanel(
      "!input.id_per_file", ns = ns,
      shiny::selectizeInput(
        # 原文：用哪個欄位辨識同一個人？（ID，可多選）
        ns("by_vars"), "\u7528\u54ea\u500b\u6b04\u4f4d\u8fa8\u8b58\u540c\u4e00\u500b\u4eba\uff1f\uff08ID\uff0c\u53ef\u591a\u9078\uff09",
        choices = NULL, multiple = TRUE,
        # 原文：請選擇 ID 欄位，例如 id
        options = list(create = FALSE, placeholder = "\u8acb\u9078\u64c7 ID \u6b04\u4f4d\uff0c\u4f8b\u5982 id")
      ),
      shiny::tags$small(
        class = "kit-hint",
        # 原文：只列出每個檔案都有的欄位；括號裡是「不重複的比例」，ID 應該接近 100%。
        "\u53ea\u5217\u51fa\u6bcf\u500b\u6a94\u6848\u90fd\u6709\u7684\u6b04\u4f4d\uff1b\u62ec\u865f\u88e1\u662f\u300c\u4e0d\u91cd\u8907\u7684\u6bd4\u4f8b\u300d\uff0cID \u61c9\u8a72\u63a5\u8fd1 100%\u3002",
        # 原文：請選每個人唯一的編號（例如受訪者編號）；不要用姓名，因為可能有人同名。
        "\u8acb\u9078\u6bcf\u500b\u4eba\u552f\u4e00\u7684\u7de8\u865f\uff08\u4f8b\u5982\u53d7\u8a2a\u8005\u7de8\u865f\uff09\uff1b\u4e0d\u8981\u7528\u59d3\u540d\uff0c\u56e0\u70ba\u53ef\u80fd\u6709\u4eba\u540c\u540d\u3002"
      )
    ),
    shiny::conditionalPanel(
      "input.id_per_file", ns = ns,
      shiny::uiOutput(ns("per_file_ids")),
      shiny::tags$small(
        class = "kit-hint",
        # 原文：每個檔案選自己的 ID 欄；比對前會改成第 1 個檔案的欄名，結果的 ID 欄也用這個名稱。
        "\u6bcf\u500b\u6a94\u6848\u9078\u81ea\u5df1\u7684 ID \u6b04\uff1b\u6bd4\u5c0d\u524d\u6703\u6539\u6210\u7b2c 1 \u500b\u6a94\u6848\u7684\u6b04\u540d\uff0c\u7d50\u679c\u7684 ID \u6b04\u4e5f\u7528\u9019\u500b\u540d\u7a31\u3002",
        # 原文：括號裡是「不重複的比例」。用多個 ID 欄時，各檔請依相同順序選。
        "\u62ec\u865f\u88e1\u662f\u300c\u4e0d\u91cd\u8907\u7684\u6bd4\u4f8b\u300d\u3002\u7528\u591a\u500b ID \u6b04\u6642\uff0c\u5404\u6a94\u8acb\u4f9d\u76f8\u540c\u9806\u5e8f\u9078\u3002"
      )
    ),
    shiny::uiOutput(ns("id_check")),
    shiny::radioButtons(
      # 原文：要保留哪些人？
      ns("method"), "\u8981\u4fdd\u7559\u54ea\u4e9b\u4eba\uff1f",
      choices = KIT_JOIN_METHODS, selected = "left_join", width = "100%"
    ),
    join_method_example(),
    kit_advanced(
      shiny::checkboxInput(
        ns("keys_as_text"),
        # 原文：ID 型別不一致時（例如一波是數字、一波是文字），自動轉成文字再比對
        "ID \u578b\u5225\u4e0d\u4e00\u81f4\u6642\uff08\u4f8b\u5982\u4e00\u6ce2\u662f\u6578\u5b57\u3001\u4e00\u6ce2\u662f\u6587\u5b57\uff09\uff0c\u81ea\u52d5\u8f49\u6210\u6587\u5b57\u518d\u6bd4\u5c0d",
        value = TRUE, width = "100%"
      ),
      shiny::checkboxGroupInput(
        # 原文：ID 比對前先標準化（編號寫法不一致時）
        ns("id_normalize"), "ID \u6bd4\u5c0d\u524d\u5148\u6a19\u6e96\u5316\uff08\u7de8\u865f\u5beb\u6cd5\u4e0d\u4e00\u81f4\u6642\uff09",
        choices = c(
          # 原文：ID 比對時去掉前導零（001 和 1 當成同一人）
          "ID \u6bd4\u5c0d\u6642\u53bb\u6389\u524d\u5c0e\u96f6\uff08001 \u548c 1 \u7576\u6210\u540c\u4e00\u4eba\uff09" = "zeros",
          # 原文：ID 比對時不分大小寫（a01 和 A01 當成同一人）
          "ID \u6bd4\u5c0d\u6642\u4e0d\u5206\u5927\u5c0f\u5beb\uff08a01 \u548c A01 \u7576\u6210\u540c\u4e00\u4eba\uff09" = "case"
        ),
        width = "100%"
      ),
      shiny::selectizeInput(
        # 原文：不分波次的欄位（性別、出生日期…）：只留一欄、不加後綴
        ns("constant_cols"), "\u4e0d\u5206\u6ce2\u6b21\u7684\u6b04\u4f4d\uff08\u6027\u5225\u3001\u51fa\u751f\u65e5\u671f\u2026\uff09\uff1a\u53ea\u7559\u4e00\u6b04\u3001\u4e0d\u52a0\u5f8c\u7db4",
        choices = NULL, multiple = TRUE, width = "100%",
        # 原文：不選就每一波都留，例如 gender_t1、gender_t2
        options = list(create = FALSE, placeholder = "\u4e0d\u9078\u5c31\u6bcf\u4e00\u6ce2\u90fd\u7559\uff0c\u4f8b\u5982 gender_t1\u3001gender_t2")
      ),
      shiny::tags$small(
        class = "kit-hint",
        # 原文：選了的欄位取最早有值的那一波；各波的值不一樣時，完成後會列出幾個人不一樣。
        "\u9078\u4e86\u7684\u6b04\u4f4d\u53d6\u6700\u65e9\u6709\u503c\u7684\u90a3\u4e00\u6ce2\uff1b\u5404\u6ce2\u7684\u503c\u4e0d\u4e00\u6a23\u6642\uff0c\u5b8c\u6210\u5f8c\u6703\u5217\u51fa\u5e7e\u500b\u4eba\u4e0d\u4e00\u6a23\u3002"
      )
    ),
    result = TRUE
  )
}

# 橫向資料串接。files：mod_upload_server()；working：mod_missing_server()；goto(tab)：換分頁。
# 狀態流在 kit_run_tab_server()；這裡是 ID 的選擇、串接、結果太大時的確認視窗、預覽與下載
mod_join_server <- function(id, files, working, goto) {
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

    common_cols <- shiny::reactive({
      dfs <- files$data()
      if (length(dfs) == 0) return(character(0))
      purrr::reduce(purrr::map(dfs, names), intersect)
    })
    shiny::observeEvent(common_cols(), {
      common <- common_cols()
      choices <- id_choices(files$data(), common)
      selected <- intersect(input$by_vars %||% character(0), common)
      if (length(selected) == 0) selected <- default_id_choice(choices)
      shiny::updateSelectizeInput(
        session,
        "by_vars",
        choices = choices,
        selected = selected
      )
    }, ignoreNULL = FALSE)

    # 不分波次的欄位：任何一個檔案有的非 ID 欄
    shiny::observeEvent(list(files$data(), id_spec()$by_vars), {
      dfs <- files$data()
      cols <- setdiff(unique(unlist(purrr::map(dfs, names))), id_spec()$by_vars)
      shiny::updateSelectizeInput(
        session, "constant_cols", choices = cols,
        selected = intersect(input$constant_cols %||% character(0), cols)
      )
    }, ignoreNULL = FALSE)

    # 各檔的 ID 欄名不同（共同欄裡沒有像 ID 的欄，但每個檔都有）時，自動改成分別指定
    shiny::observeEvent(files$keys(), {
      dfs <- files$data()
      if (length(dfs) < 2) return()
      each_has_id <- all(purrr::map_lgl(dfs, function(df) {
        length(guess_id_cols(names(df))) > 0
      }))
      if (length(guess_id_cols(common_cols())) == 0 && each_has_id) {
        shiny::updateCheckboxInput(session, "id_per_file", value = TRUE)
      }
    })

    # 每個檔案各自的 ID 下拉選單（id_<檔案 key>）
    output$per_file_ids <- shiny::renderUI({
      dfs <- files$data()
      if (length(dfs) == 0) return(NULL)
      keys <- files$keys()
      names_shown <- file_display_name(files$labels(), files$names())
      shiny::tags$div(
        class = "kit-per-file-ids",
        purrr::map(seq_along(dfs), function(i) {
          id <- paste0("id_", keys[i])
          cols <- names(dfs[[i]])
          choices <- id_choices(dfs[i], cols)
          prev <- intersect(shiny::isolate(input[[id]]) %||% character(0), cols)
          # 寬度跟著內容（約 28rem）：不拉滿整頁；手機上由 CSS 的 max-width: 100% 收窄
          shiny::selectizeInput(
            # 原文：的 ID 欄
            ns(id), paste0(names_shown[i], "\u7684 ID \u6b04"),
            choices = choices,
            selected = if (length(prev) > 0) prev else default_id_choice(choices),
            multiple = TRUE, width = "28rem",
            # 原文：請選擇這個檔案的 ID 欄
            options = list(create = FALSE, placeholder = "\u8acb\u9078\u64c7\u9019\u500b\u6a94\u6848\u7684 ID \u6b04")
          )
        })
      )
    })

    # 目前的 ID 設定：by_vars 是結果中的 ID 欄名；id_cols 是各檔自己的 ID 欄（NULL = 同名）
    id_spec <- shiny::reactive({
      if (!isTRUE(input$id_per_file)) {
        return(list(by_vars = input$by_vars %||% character(0), id_cols = NULL))
      }
      cols <- purrr::map(files$keys(), function(k) input[[paste0("id_", k)]] %||% character(0))
      list(by_vars = if (length(cols) > 0) cols[[1]] else character(0), id_cols = cols)
    })

    # 選好 ID 後，先看各檔的 ID 有沒有重複、空白
    output$id_check <- shiny::renderUI({
      spec <- id_spec()
      by_vars <- spec$by_vars
      dfs <- files$data()
      if (length(dfs) < 2 || length(by_vars) == 0) return(NULL)
      id_cols <- spec$id_cols %||% rep(list(by_vars), length(dfs))
      ok <- purrr::map2_lgl(dfs, id_cols, function(df, cols) {
        length(cols) == length(by_vars) && all(cols %in% names(df))
      })
      if (!all(ok)) return(NULL)
      names_shown <- file_display_name(files$labels(), files$names())
      tbl <- id_check_table(dfs, names_shown, by_vars, id_cols)
      shiny::tags$details(
        class = "kit-example",
        # 原文：重複的ID數
        open = if (any(tbl[["\u91cd\u8907\u7684ID\u6578"]] > 0)) NA,
        shiny::tags$summary(
          # 原文：重複的ID數 | ⚠ ID 檢查：有檔案的 ID 重複 | ID 檢查：各檔的 ID 都不重複
          if (any(tbl[["\u91cd\u8907\u7684ID\u6578"]] > 0)) "\u26a0 ID \u6aa2\u67e5\uff1a\u6709\u6a94\u6848\u7684 ID \u91cd\u8907" else "ID \u6aa2\u67e5\uff1a\u5404\u6a94\u7684 ID \u90fd\u4e0d\u91cd\u8907"
        ),
        shiny::tags$div(class = "kit-table-scroll", kit_static_table(tbl)),
        # 原文：重複的ID數
        if (any(tbl[["\u91cd\u8907\u7684ID\u6578"]] > 0)) {
          shiny::tags$small(
            class = "kit-hint",
            # 原文：ID 重複時，同一個人在結果中會佔多列；重複很多時結果會暴增。
            "ID \u91cd\u8907\u6642\uff0c\u540c\u4e00\u500b\u4eba\u5728\u7d50\u679c\u4e2d\u6703\u4f54\u591a\u5217\uff1b\u91cd\u8907\u5f88\u591a\u6642\u7d50\u679c\u6703\u66b4\u589e\u3002",
            # 原文：請確認選的是每個人都不同的編號。
            "\u8acb\u78ba\u8a8d\u9078\u7684\u662f\u6bcf\u500b\u4eba\u90fd\u4e0d\u540c\u7684\u7de8\u865f\u3002"
          )
        }
      )
    })

    tab <- kit_run_tab_server(
      # 原文：執行串接
      input, output, session, "\u57f7\u884c\u4e32\u63a5",
      # 主要按鈕：檔案不夠或還沒選 ID 時停用
      ready = shiny::reactive({
        n <- length(files$keys())
        need <- kit_need_files(n, 2L)
        if (!is.null(need)) return(need)
        spec <- id_spec()
        if (length(spec$by_vars) == 0) {
          # 原文：選第 1 個檔案的 ID 欄 | 選 ID 欄
          return(if (isTRUE(input$id_per_file)) "\u9078\u7b2c 1 \u500b\u6a94\u6848\u7684 ID \u6b04" else "\u9078 ID \u6b04")
        }
        if (!is.null(spec$id_cols) && any(lengths(spec$id_cols) != length(spec$by_vars))) {
          # 原文：每個檔案選一樣多個 ID 欄
          return("\u6bcf\u500b\u6a94\u6848\u9078\u4e00\u6a23\u591a\u500b ID \u6b04")
        }
        character(0)
      }),
      ready_link = function() {
        # 原文：到「檔案上傳」加入檔案 →
        if (length(files$keys()) < 2) shiny::actionLink(ns("goto_upload"), "\u5230\u300c\u6a94\u6848\u4e0a\u50b3\u300d\u52a0\u5165\u6a94\u6848 \u2192")
      },
      # 設定摘要：「本次：5 個檔案 · ID：各檔各自 · 全勤樣本 · 原始資料」
      settings = function() {
        n <- length(files$keys())
        if (n < 2) return(NULL)
        spec <- id_spec()
        id_text <- if (isTRUE(input$id_per_file)) {
          # 原文：ID：各檔各自指定
          "ID\uff1a\u5404\u6a94\u5404\u81ea\u6307\u5b9a"
        } else if (length(spec$by_vars) > 0) {
          # 原文：ID： | ＋
          paste0("ID\uff1a", paste(spec$by_vars, collapse = "\uff0b"))
        } else {
          # 原文：ID：還沒選
          "ID\uff1a\u9084\u6c92\u9078"
        }
        c(
          # 原文： 個檔案
          paste0(kit_num(n), " \u500b\u6a94\u6848"), id_text, kit_join_method_short(input$method),
          kit_source_short(working$source())
        )
      },
      stale_on = function() {
        list(working$data(), files$labels(), id_spec(), input$method, input$keys_as_text,
             input$id_normalize, input$constant_cols)
      },
      # 原文：正在串接資料…
      progress = "\u6b63\u5728\u4e32\u63a5\u8cc7\u6599\u2026",
      run = function(confirm_large = FALSE) {
        spec <- id_spec()
        out <- join_waves(
          working$data(), files$names(), files$labels(), spec$by_vars,
          method = input$method, keys_as_text = isTRUE(input$keys_as_text),
          confirm_large = confirm_large, id_cols = spec$id_cols,
          id_normalize = input$id_normalize %||% character(0),
          constant_cols = input$constant_cols %||% character(0)
        )
        out$by_vars <- spec$by_vars
        out
      },
      done = function(out) {
        kit_done_status(
          # 原文：串接完成：結果共  | 。
          paste0("\u4e32\u63a5\u5b8c\u6210\uff1a\u7d50\u679c\u5171 ", kit_dims(nrow(out$result), ncol(out$result)), "\u3002"),
          warnings = out$warnings, notes = out$notes, points = out$summary
        )
      },
      # 原文：串接
      error = function(e) kit_error_status(e, "\u4e32\u63a5", kit_upload_todo(ns, length(files$keys()), 2L)),
      # 結果會暴增：跳出確認視窗，使用者按「仍要執行」才繼續（tab$run(confirm_large = TRUE)）
      confirm = list(kit_join_too_large = function(e) {
        shiny::showModal(shiny::modalDialog(
          # 原文：串接結果會非常大
          title = "\u4e32\u63a5\u7d50\u679c\u6703\u975e\u5e38\u5927",
          conditionMessage(e),
          footer = shiny::tagList(
            # 原文：取消，回去改 ID
            shiny::modalButton("\u53d6\u6d88\uff0c\u56de\u53bb\u6539 ID"),
            # 原文：仍要執行
            kit_button(ns("run_confirm"), "\u4ecd\u8981\u57f7\u884c", "danger")
          ),
          easyClose = TRUE
        ))
        kit_status("error", conditionMessage(e))
      }),
      # 重點（狀態框下面）：各檔案的人數
      result_ui = function(res) {
        shiny::tagList(
          # 原文：各檔案的人數
          shiny::h5("\u5404\u6a94\u6848\u7684\u4eba\u6578"),
          shiny::tags$div(class = "kit-table-scroll", shiny::tableOutput(ns("people")))
        )
      },
      # 預覽與詳細核對收在細節列
      details_ui = function(res) {
        shiny::tagList(
          kit_detail(
            # 原文：結果預覽
            "\u7d50\u679c\u9810\u89bd",
            shiny::tags$p(class = "kit-hint", shiny::textOutput(ns("preview_meta"), inline = TRUE)),
            shiny::tags$div(class = "kit-table-scroll", shiny::tableOutput(ns("preview")))
          ),
          kit_audit_detail(
            shiny::tags$p(class = "kit-hint", res$audit$rule),
            # 原文：最終核對
            shiny::h6("\u6700\u7d42\u6838\u5c0d"),
            shiny::tableOutput(ns("audit_checks")),
            # 原文：各檔案的列數與 ID
            shiny::h6("\u5404\u6a94\u6848\u7684\u5217\u6578\u8207 ID"),
            shiny::tags$div(class = "kit-table-scroll", shiny::tableOutput(ns("audit_files"))),
            # 原文：逐步合併
            shiny::h6("\u9010\u6b65\u5408\u4f75"),
            shiny::tags$div(class = "kit-table-scroll", shiny::tableOutput(ns("audit_steps")))
          )
        )
      },
      # page_code：同一個函式也交給 app 的流程紀錄（pipeline_log_server()）
      code = page_code <- function() {
        spec <- id_spec()
        code_join(code_files_from(files, 2L), spec$by_vars, input$method, id_cols = spec$id_cols,
                  id_normalize = input$id_normalize %||% character(0),
                  constant_cols = input$constant_cols %||% character(0))
      },
      code_note = KIT_CODE_NOTE_JOIN
    )
    result <- tab$result
    shiny::observeEvent(input$run_confirm, {
      shiny::removeModal()
      tab$run(confirm_large = TRUE)
    })

    kit_download_server(
      session, output,
      data_fn = function() {
        res <- result()
        if (is.null(res)) NULL else reorder_key_first(res$result, res$by_vars)
      },
      filename_fn = timestamped_name("joined"),
      # 原文：執行串接
      run_label = "\u57f7\u884c\u4e32\u63a5"
    )

    output$people <- shiny::renderTable(
      result()$people, digits = 0, format.args = list(big.mark = ",")
    )
    output$preview_meta <- shiny::renderText({
      df <- shiny::req(result())$result   # 結果被清掉（例如 ② 重新執行）時不顯示
      kit_rows_note(nrow(df), ncol(df), KIT_PREVIEW_ROWS, KIT_PREVIEW_COLS)
    })
    output$preview <- shiny::renderTable(
      kit_preview_table(result()$result, priority_cols = result()$by_vars),
      striped = TRUE, spacing = "s"
    )
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
    output$audit_steps <- shiny::renderTable(
      result()$audit$steps, digits = 0, format.args = list(big.mark = ",")
    )

    # 這一頁的狀態（步驟分頁打 ✓ 用）
    list(state = tab$state, result = tab$result, code = page_code)
  })
}

# 「詳細核對」：結果下面那一排細節的一個（預設收起）
kit_audit_detail <- function(...) {
  # 原文：詳細核對（進階）
  kit_detail("\u8a73\u7d30\u6838\u5c0d\uff08\u9032\u968e\uff09", ...)
}

# 設定摘要用的串接方法簡稱：「全勤樣本」
kit_join_method_short <- function(method) {
  label <- names(KIT_JOIN_METHODS)[match(method %||% "", KIT_JOIN_METHODS)]
  if (is.na(label)) return(NULL)
  # 原文：：.*$
  sub("\uff1a.*$", "", label)
}

# 設定摘要用的資料來源：「原始資料」或「② 處理後的資料」
kit_source_short <- function(source) {
  # 原文：遺漏值處理後的資料 | 原始資料
  if (identical(source, "na")) "\u907a\u6f0f\u503c\u8655\u7406\u5f8c\u7684\u8cc7\u6599" else "\u539f\u59cb\u8cc7\u6599"
}

# 各分頁的「到 檔案上傳」「到 遺漏值處理」連結
watch_goto_links <- function(input, goto) {
  shiny::observeEvent(input$goto_upload, goto("upload"))
  shiny::observeEvent(input$goto_upload_status, goto("upload"))
  shiny::observeEvent(input$goto_missing, goto("missing"))
}

# 頁首的圖解：預設（向後追蹤樣本）怎麼左右接起來
join_diagram <- function() {
  kit_diagram(
    before = list(
      # 原文：第 1 波（t1）
      kit_dg_table(data.frame(id = c(1, 2), score = c(5, 3)), "\u7b2c 1 \u6ce2\uff08t1\uff09"),
      # 原文：第 2 波（t2）
      kit_dg_table(data.frame(id = c(1, 3), score = c(6, 4)), "\u7b2c 2 \u6ce2\uff08t2\uff09")
    ),
    after = kit_dg_table(
      # 原文：結果
      data.frame(id = c(1, 2), score_t1 = c(5, 3), score_t2 = c(6, NA)), "\u7d50\u679c"
    ),
    # 原文：依 id 左右接起來：每人一列，各波的變項加上波次名稱；預設只留第 1 波有的人
    caption = "\u4f9d id \u5de6\u53f3\u63a5\u8d77\u4f86\uff1a\u6bcf\u4eba\u4e00\u5217\uff0c\u5404\u6ce2\u7684\u8b8a\u9805\u52a0\u4e0a\u6ce2\u6b21\u540d\u7a31\uff1b\u9810\u8a2d\u53ea\u7559\u7b2c 1 \u6ce2\u6709\u7684\u4eba"
  )
}
