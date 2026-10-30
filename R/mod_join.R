# ③ 寬格式串接

#' @noRd
mod_join_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    kit_tab_guide(
      goal = "把各波資料左右並排：每個人一列，各波的變項加上波次名稱（例如 score_t1、score_t2）。",
      steps = kit_steps(
        "選擇用來辨識同一個人的 ID 欄位（各檔欄名不同時，可以分別指定）",
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
    shiny::checkboxInput(
      ns("id_per_file"),
      "各檔的 ID 欄名不同（例如 w1_id、w2_id、w3id），分別指定每個檔案的 ID 欄",
      value = FALSE, width = "100%"
    ),
    shiny::conditionalPanel(
      "!input.id_per_file", ns = ns,
      shiny::selectizeInput(
        ns("by_vars"), "用哪個欄位辨識同一個人？（ID，可多選）",
        choices = NULL, multiple = TRUE,
        options = list(create = FALSE, placeholder = "請選擇 ID 欄位，例如 release_id")
      ),
      shiny::tags$small(
        class = "kit-hint",
        "只列出每個檔案都有的欄位；括號裡是「不重複的比例」，ID 應該接近 100%。",
        "請選每個人唯一的編號，例如 release_id；不要用姓名，因為可能有人同名。"
      )
    ),
    shiny::conditionalPanel(
      "input.id_per_file", ns = ns,
      shiny::uiOutput(ns("per_file_ids")),
      shiny::tags$small(
        class = "kit-hint",
        "每個檔案選自己的 ID 欄；比對前會改成第 1 個檔案的欄名，結果的 ID 欄也用這個名稱。",
        "括號裡是「不重複的比例」。用多個 ID 欄時，各檔請依相同順序選。"
      )
    ),
    shiny::uiOutput(ns("id_check")),
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
          shiny::selectizeInput(
            ns(id), paste0(names_shown[i], "的 ID 欄"),
            choices = choices,
            selected = if (length(prev) > 0) prev else default_id_choice(choices),
            multiple = TRUE, width = "100%",
            options = list(create = FALSE, placeholder = "請選擇這個檔案的 ID 欄")
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
        open = if (any(tbl$重複的ID數 > 0)) NA,
        shiny::tags$summary(
          if (any(tbl$重複的ID數 > 0)) "⚠ ID 檢查：有檔案的 ID 重複" else "ID 檢查：各檔的 ID 都不重複"
        ),
        shiny::tags$div(class = "kit-table-scroll", kit_static_table(tbl)),
        if (any(tbl$重複的ID數 > 0)) {
          shiny::tags$small(
            class = "kit-hint",
            "ID 重複時，同一個人在結果中會佔多列；重複很多時結果會暴增。",
            "請確認選的是每個人都不同的編號。"
          )
        }
      )
    })

    # 檔案或設定變動後，舊結果失效
    shiny::observeEvent(
      list(
        working$data(),
        files$labels(),
        id_spec(),
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

    run_join <- function(confirm_large) {
      result(NULL)
      asked <- FALSE
      st <- tryCatch(
        shiny::withProgress(message = "正在串接資料…", value = 0.3, {
          spec <- id_spec()
          out <- join_waves(
            working$data(), files$names(), files$labels(), spec$by_vars,
            method = input$method, keys_as_text = isTRUE(input$keys_as_text),
            confirm_large = confirm_large, id_cols = spec$id_cols
          )
          out$by_vars <- spec$by_vars
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
        # 結果會暴增：跳出確認視窗，使用者按「仍要執行」才繼續
        kit_join_too_large = function(e) {
          shiny::showModal(shiny::modalDialog(
            title = "串接結果會非常大",
            conditionMessage(e),
            footer = shiny::tagList(
              shiny::modalButton("取消，回去改 ID"),
              shiny::actionButton(ns("run_confirm"), "仍要執行", class = "btn-danger")
            ),
            easyClose = TRUE
          ))
          asked <<- TRUE
          kit_status("error", conditionMessage(e))
        },
        error = function(e) kit_error_status(e, "串接")
      )
      status(st)
      if (!asked) kit_notify(st)
    }
    shiny::observeEvent(input$run, run_join(confirm_large = FALSE))
    shiny::observeEvent(input$run_confirm, {
      shiny::removeModal()
      run_join(confirm_large = TRUE)
    })

    output$status <- shiny::renderUI(make_status_ui(status()))

    kit_download_server(
      session, output,
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
      spec <- id_spec()
      kit_code_blocks(code_join(
        code_files_from(files, 2L),
        spec$by_vars,
        input$method,
        id_cols = spec$id_cols
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
