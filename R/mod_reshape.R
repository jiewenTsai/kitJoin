# ⑦ 長寬互轉：tidyr::pivot_wider() / pivot_longer()

#' 控制項標籤：中文說明 + tidyr 參數名
#' @noRd
arg_label <- function(text, arg) {
  shiny::tagList(text, " ", shiny::tags$code(arg))
}

#' @noRd
mod_reshape_ui <- function(id) {
  ns <- shiny::NS(id)
  is_wider <- "input.direction == 'wider'"
  shiny::tagList(
    kit_tab_guide(
      goal = paste0(
        "把一個檔案在長格式與寬格式之間互轉。",
        "例如 kitsUI 分數表（每次施測 × 每個領域一列）轉成每次施測一列、各領域並排。"
      ),
      steps = kit_steps(
        "選要轉換的檔案與方向",
        "設定欄位（每個控制項旁邊是對應的 tidyr 參數名）",
        "按「執行轉換」，看結果摘要、檢查與預覽",
        "下載 CSV 或 SAV"
      ),
      output = "轉換後的一個資料檔；轉換前後的列數 × 欄數會並列在摘要中。",
      technical = paste0(
        "長轉寬用 tidyr::pivot_wider()，寬轉長用 tidyr::pivot_longer()，預設值同 tidyr。",
        "長轉寬不做 values_fn：id_cols × names_from 有重複時停下並列出重複列，",
        "不會默默彙總，也不會產生 list 欄。"
      )
    ),
    shiny::uiOutput(ns("source_note")),
    shiny::fluidRow(
      shiny::column(
        5,
        shiny::selectInput(ns("file"), "要轉換的檔案", choices = NULL)
      ),
      shiny::column(
        7,
        shiny::radioButtons(
          ns("direction"), "方向",
          choices = c(
            "長轉寬（pivot_wider）" = "wider",
            "寬轉長（pivot_longer）" = "longer"
          ),
          selected = "wider", inline = TRUE
        )
      )
    ),
    shiny::uiOutput(ns("guess")),
    shiny::conditionalPanel(is_wider, ns = ns, wider_controls_ui(ns)),
    shiny::conditionalPanel(
      "input.direction == 'longer'", ns = ns,
      mod_pivot_longer_ui(ns("longer"))
    ),
    shiny::uiOutput(ns("direction_note")),
    shiny::uiOutput(ns("settings_line")),
    kit_button(ns("run"), "執行轉換", "primary"),
    shiny::uiOutput(ns("status")),
    shiny::uiOutput(ns("blocked")),
    shiny::uiOutput(ns("result")),
    kit_download_ui(ns),
    kit_details_row(
      shiny::uiOutput(ns("result_details"), inline = TRUE),
      kit_code_panel(ns("code"))
    )
  )
}

#' 長轉寬的控制項（參數一對一對應 tidyr::pivot_wider()）
#' @noRd
wider_controls_ui <- function(ns) {
  shiny::tagList(
    shiny::uiOutput(ns("preset")),
    shiny::fluidRow(
      shiny::column(
        5,
        shiny::selectizeInput(
          ns("names_from"), arg_label("新欄名來自哪一欄", "names_from"),
          choices = NULL,
          options = list(placeholder = "例如 domain")
        )
      ),
      shiny::column(
        7,
        shiny::selectizeInput(
          ns("values_from"), arg_label("要攤開的值（可多選）", "values_from"),
          choices = NULL, multiple = TRUE,
          options = list(placeholder = "例如 d_score")
        )
      )
    ),
    shiny::checkboxInput(
      ns("id_auto"),
      arg_label("哪些欄是 id：用 tidyr 預設（其餘全部欄位）", "id_cols = NULL"),
      value = TRUE, width = "100%"
    ),
    shiny::conditionalPanel(
      "!input.id_auto", ns = ns,
      shiny::selectizeInput(
        ns("id_cols"), arg_label("哪些欄是 id（可多選）", "id_cols"),
        choices = NULL, multiple = TRUE, width = "100%"
      )
    ),
    shiny::uiOutput(ns("id_note")),
    kit_advanced(
      id = ns("advanced"),
      title = "進階設定：欄名格式（分隔符號）、欄位排列、缺的格子",
      wider_advanced_ui(ns)
    )
  )
}

#' 長轉寬的進階控制項（預設收起）
#' @noRd
wider_advanced_ui <- function(ns) {
  shiny::tagList(
    shiny::radioButtons(
      ns("name_mode"), "新欄名怎麼組？",
      choices = c(
        "值欄名 + 分隔字 + 欄名來源的值（names_sep / names_prefix）" = "sep",
        "自訂格式（names_glue）" = "glue"
      ),
      selected = "sep", width = "100%"
    ),
    shiny::conditionalPanel(
      "input.name_mode == 'sep'", ns = ns,
      shiny::fluidRow(
        shiny::column(
          4,
          shiny::textInput(ns("names_sep"), arg_label("分隔字", "names_sep"), "_")
        ),
        shiny::column(
          4,
          shiny::textInput(
            ns("names_prefix"), arg_label("欄名前綴", "names_prefix"), ""
          )
        )
      )
    ),
    shiny::conditionalPanel(
      "input.name_mode == 'glue'", ns = ns,
      shiny::textInput(
        ns("names_glue"), arg_label("欄名格式", "names_glue"), "",
        placeholder = "{.value}_{domain}"
      ),
      shiny::tags$small(
        class = "kit-hint",
        "{.value} 是值欄的名稱（如 d_score），{domain} 是欄名來源欄的值（如 lan）。"
      )
    ),
    shiny::radioButtons(
      ns("names_vary"), arg_label("欄位排列", "names_vary"),
      choices = c(
        "fastest：同一個值欄放在一起（d_score_lan、d_score_cog、scale_lan…）" = "fastest",
        "slowest：同一個領域放在一起（d_score_lan、scale_lan、d_score_cog…）" = "slowest"
      ),
      selected = "fastest", width = "100%"
    ),
    shiny::textInput(
      ns("values_fill"), arg_label("缺的格子填什麼", "values_fill"), "",
      placeholder = "空白 = 不填（NA）"
    ),
    shiny::tags$small(
      class = "kit-hint",
      "只填「沒有這一列」造成的空格；原本就空白的值不會被填。",
      "數字只填進數值欄、文字只填進文字欄。"
    )
  )
}

#' @inheritParams mod_join_server
#' @noRd
mod_reshape_server <- function(id, files, working, goto) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    result <- shiny::reactiveVal(NULL)
    status <- shiny::reactiveVal(kit_status())
    blocked <- shiny::reactiveVal(NULL)
    watch_goto_links(input, goto)

    output$source_note <- shiny::renderUI({
      reshape_source_note_ui(
        length(files$keys()), working$source(), working$code_cells(), ns
      )
    })

    shiny::observeEvent(list(files$keys(), files$names(), files$labels()), {
      choices <- stats::setNames(
        files$keys(),
        file_display_name(files$labels(), files$names())
      )
      selected <- if (isTRUE(input$file %in% choices)) input$file else choices[1]
      shiny::updateSelectInput(
        session, "file", choices = choices, selected = unname(selected)
      )
    }, ignoreNULL = FALSE)

    picked <- shiny::reactive({
      pos <- match(input$file %||% "", files$keys())
      if (is.na(pos)) return(NULL)
      list(
        data = working$data()[[pos]],
        obj = code_files_from(files)$obj[pos]
      )
    })
    src <- shiny::reactive(picked()$data)
    cols <- shiny::reactive(names(src()))

    # 換檔案時更新欄位選項；保留仍存在的選擇
    shiny::observeEvent(cols(), {
      all <- cols()
      keep <- function(x, fallback = character(0)) {
        x <- intersect(x %||% character(0), all)
        if (length(x) == 0) intersect(fallback, all) else x
      }
      shiny::updateSelectizeInput(
        session, "names_from", choices = all,
        selected = keep(input$names_from, "name")
      )
      shiny::updateSelectizeInput(
        session, "values_from", choices = all,
        selected = keep(input$values_from, "value")
      )
      shiny::updateSelectizeInput(
        session, "id_cols", choices = all, selected = keep(input$id_cols)
      )
    }, ignoreNULL = FALSE)

    # 取消「用預設」時，把目前的預設 id 欄填進去，方便從那裡刪減
    from_preset <- FALSE
    shiny::observeEvent(input$id_auto, {
      if (from_preset) {
        from_preset <<- FALSE
        return()
      }
      if (isTRUE(input$id_auto) || length(input$id_cols) > 0) return()
      shiny::updateSelectizeInput(
        session, "id_cols",
        selected = setdiff(cols(), c(input$names_from, input$values_from))
      )
    }, ignoreInit = TRUE)

    wider <- shiny::reactive({
      glue <- identical(input$name_mode, "glue")
      wider_args(
        id_cols = if (isTRUE(input$id_auto)) NULL else input$id_cols,
        names_from = input$names_from[nzchar(input$names_from %||% "")],
        values_from = input$values_from,
        names_prefix = if (glue) "" else input$names_prefix,
        names_sep = if (glue) "_" else input$names_sep,
        names_glue = if (glue) input$names_glue,
        names_vary = input$names_vary,
        values_fill = input$values_fill
      )
    })
    # 寬轉長的設定：和 ⑥ 共用的元件（mod_pivot_longer.R）
    longer <- mod_pivot_longer_server("longer", data = src)

    # 轉換前掃一下：看起來是長格式還是寬格式（只提示，不自動換方向）
    guess <- shiny::reactive({
      df <- src()
      if (is.null(df)) return(NULL)
      id <- input$guess_id
      reshape_guess_format(df, if (isTRUE(id %in% names(df))) id)
    })
    output$guess <- shiny::renderUI({
      all <- cols()
      if (length(all) == 0) return(NULL)
      kit_callout(
        shiny::uiOutput(ns("guess_text")),
        shiny::tags$div(
          class = "d-flex flex-wrap align-items-end gap-3 mt-2",
          shiny::selectizeInput(
            ns("guess_id"), "用哪一欄當 ID", choices = c("（請選）" = "", all),
            selected = reshape_guess_id(all) %||% "", width = "16em"
          ),
          shiny::tags$div(class = "mb-3", shiny::uiOutput(ns("guess_flip")))
        )
      )
    })
    output$guess_text <- shiny::renderUI({
      g <- guess()
      if (is.null(g)) return(NULL)
      shiny::tags$div(reshape_guess_text(g))
    })
    output$guess_flip <- shiny::renderUI({
      to <- guess()$suggest
      if (is.null(to) || identical(to, input$direction)) return(NULL)
      label <- c(wider = "長轉寬", longer = "寬轉長")[[to]]
      kit_button(ns("guess_flip_btn"), paste0("改選「", label, "」"))
    })
    shiny::observeEvent(input$guess_flip_btn, {
      to <- guess()$suggest
      if (!is.null(to)) shiny::updateRadioButtons(session, "direction", selected = to)
    })
    # 方向和猜測相反時，在執行鈕旁邊提醒（不擋）
    output$direction_note <- shiny::renderUI({
      note <- reshape_direction_note(guess()$format, input$direction)
      if (is.null(note)) return(NULL)
      kit_callout(warn = TRUE, note)
    })

    output$id_note <- shiny::renderUI({
      df <- src()
      if (is.null(df)) return(NULL)
      ids <- wider_id_cols(df, wider())
      shiny::tags$small(
        class = "kit-hint",
        paste0(
          "目前的 id 欄（", length(ids), " 欄）：",
          paste(utils::head(ids, 12), collapse = "、"),
          if (length(ids) > 12) " …" else "", "。",
          "轉換後每一組 id 欄的值是一列。"
        )
      )
    })

    # kitsUI 分數表：顯示套用預設的按鈕（只填控制項，不自動執行）
    output$preset <- shiny::renderUI({
      if (!is_kitsui_score_table(cols())) return(NULL)
      kit_callout(
        "這個檔案看起來是 kitsUI 的分數表（一列 = 一次施測 × 一個領域）。",
        kit_button(ns("apply_preset"), "套用 kitsUI 分數表預設"),
        shiny::tags$div(
          class = "kit-hint",
          "只會填好下方設定，不會自動執行；values_from 多放 scale，",
          "d_score 空白時才分得出是沒測這個領域，還是有測但還沒放上 D 量尺。"
        )
      )
    })
    shiny::observeEvent(input$apply_preset, {
      p <- kitsui_score_preset(cols())
      if (is.null(p)) return()
      from_preset <<- !identical(input$id_auto, FALSE)
      shiny::updateCheckboxInput(session, "id_auto", value = FALSE)
      shiny::updateSelectizeInput(session, "id_cols", selected = p$id_cols)
      shiny::updateSelectizeInput(session, "names_from", selected = p$names_from)
      shiny::updateSelectizeInput(session, "values_from", selected = p$values_from)
      shiny::updateRadioButtons(session, "name_mode", selected = "glue")
      shiny::updateTextInput(session, "names_glue", value = p$names_glue)
      shiny::updateRadioButtons(session, "names_vary", selected = p$names_vary)
      session$sendCustomMessage("kit-open-details", ns("advanced"))
      shiny::showNotification(
        "已填入 kitsUI 分數表預設。請確認後按「執行轉換」。",
        type = "message", duration = KIT_NOTIFY_SECONDS
      )
    })

    # 設定摘要：「本次：t1（scores.csv） · 長轉寬 · names_from：domain · values_from：d_score」
    output$settings_line <- shiny::renderUI({
      p <- picked()
      if (is.null(p)) return(NULL)
      shown <- file_display_name(files$labels(), files$names())
      pos <- match(input$file %||% "", files$keys())
      args <- if (identical(input$direction, "longer")) {
        a <- longer()
        n <- length(a$cols)
        c("寬轉長", if (n > 0) {
          paste0(if (isTRUE(a$cols_exclude)) "不轉 " else "轉 ", n, " 欄")
        } else {
          "還沒選要轉的欄"
        })
      } else {
        w <- wider()
        c("長轉寬",
          if (length(w$names_from) > 0) paste0("names_from：", paste(w$names_from, collapse = "、")),
          if (length(w$values_from) > 0) {
            paste0("values_from：", paste(w$values_from, collapse = "、"))
          })
      }
      kit_settings_line(c(shown[pos], args))
    })

    # 檔案或設定變動後，舊結果失效
    shiny::observeEvent(
      list(src(), input$direction, wider(), longer()),
      {
        blocked(NULL)
        if (!is.null(result())) {
          result(NULL)
          status(kit_stale_status("執行轉換"))
        }
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(input$run, {
      result(NULL)
      blocked(NULL)
      st <- tryCatch(
        shiny::withProgress(message = "正在轉換資料…", value = 0.3, {
          df <- src()
          if (is.null(df)) kit_abort("請先到「上傳檔案」加入檔案。")
          out <- if (identical(input$direction, "longer")) {
            reshape_longer(df, longer())
          } else {
            run_wider(df, wider(), blocked)
          }
          out$direction <- input$direction
          out$before <- c(nrow(df), ncol(df))
          result(out)
          kit_done_status(
            paste0(
              "轉換完成：結果共 ",
              format_dims(nrow(out$result), ncol(out$result)), "。"
            ),
            warnings = out$warnings
          )
        }),
        error = function(e) {
          kit_error_status(e, "轉換", kit_upload_todo(ns, length(files$keys())))
        }
      )
      status(st)
      kit_notify(st)
    })

    output$status <- shiny::renderUI(make_status_ui(status()))
    output$blocked <- shiny::renderUI({
      dup <- blocked()
      if (is.null(dup)) return(NULL)
      shiny::tagList(
        shiny::h5(paste0("重複的列（", kit_rows_note(nrow(dup$rows), NULL, 50), "）")),
        shiny::tags$div(
          class = "kit-table-scroll",
          shiny::tableOutput(ns("blocked_rows"))
        )
      )
    })
    output$blocked_rows <- shiny::renderTable(
      df_for_display(blocked()$rows, 50),
      striped = TRUE, spacing = "s"
    )

    kit_download_server(
      session, output,
      data_fn = function() result()$result,
      filename_fn = function(ext) {
        dir <- if (identical(result()$direction, "longer")) "long" else "wide"
        timestamped_name(dir)(ext)
      },
      run_label = "執行轉換"
    )

    # 重點：結果摘要；空白格、預覽、id 欄不一致的列收在下面一排細節裡
    output$result <- shiny::renderUI({
      res <- result()
      if (is.null(res)) return(NULL)
      kit_summary_ui(res$summary)
    })
    output$result_details <- shiny::renderUI({
      res <- result()
      if (is.null(res)) return(NULL)
      is_wider <- identical(res$direction, "wider")
      shiny::tagList(
        kit_detail(
          "結果預覽",
          shiny::tags$p(class = "kit-hint", shiny::textOutput(ns("preview_meta"), inline = TRUE)),
          shiny::tags$div(class = "kit-table-scroll", shiny::tableOutput(ns("preview")))
        ),
        if (is_wider) {
          kit_detail(
            "空白格從哪裡來？",
            shiny::tags$p(
              class = "kit-hint",
              "轉成寬格式後，下面兩種情況都會是空白：",
              "「沒有這一列」＝原檔沒有這組 id 欄 × ", names(res$empty)[1], " 的列；",
              "「有列但空白」＝有這一列，但值本來就是空的。"
            ),
            shiny::tags$div(class = "kit-table-scroll", shiny::tableOutput(ns("empty_table")))
          )
        },
        if (is_wider && !res$id_check$ok) {
          kit_audit_detail(
            shiny::h6(paste0(
              "id 欄不一致的列（", kit_rows_note(nrow(res$id_check$rows), NULL, 50), "）"
            )),
            shiny::tags$div(class = "kit-table-scroll", shiny::tableOutput(ns("id_rows")))
          )
        }
      )
    })
    output$empty_table <- shiny::renderTable(
      result()$empty, digits = 0, format.args = list(big.mark = ",")
    )
    output$id_rows <- shiny::renderTable(
      df_for_display(result()$id_check$rows, 50),
      striped = TRUE, spacing = "s"
    )
    output$preview_meta <- shiny::renderText({
      res <- shiny::req(result())
      paste0(
        "轉換前 ", format_dims(res$before[1], res$before[2]), "；轉換後",
        kit_rows_note(
          nrow(res$result), ncol(res$result), KIT_RESHAPE_PREVIEW_ROWS, KIT_PREVIEW_COLS
        )
      )
    })
    output$preview <- shiny::renderTable({
      res <- result()
      kit_preview_table(
        res$result, priority_cols = res$id_cols, n = KIT_RESHAPE_PREVIEW_ROWS
      )
    }, striped = TRUE, spacing = "s")

    output$code <- shiny::renderUI({
      p <- picked()
      obj <- p$obj %||% "data_t1"
      snips <- if (identical(input$direction, "longer")) {
        args <- utils::modifyList(PIVOT_LONGER_DEFAULTS, longer())
        labels <- if (!is.null(p$data)) {
          pivot_longer_labels(p$data, pivot_longer_cols(p$data, args))
        }
        code_longer(longer(), obj, names(p$data), labels)
      } else {
        code_wider(wider(), p$data %||% tibble::tibble(), obj)
      }
      kit_code_blocks(snips)
    })

    # 這一頁的狀態（步驟分頁打 ✓ 用）
    list(state = shiny::reactive(status()$state))
  })
}

#' 執行長轉寬；id × names_from 重複時把重複列存進 blocked 再停下
#' @noRd
run_wider <- function(df, args, blocked) {
  if (length(args$names_from) == 1 && length(args$values_from) > 0) {
    keys <- c(wider_id_cols(df, args), args$names_from)
    dup <- check_key_unique(df, intersect(keys, names(df)))
    if (!dup$ok) blocked(dup)
  }
  reshape_wider(df, args)
}

#' 方向和資料格式的猜測相反時的提醒；沒有問題時 NULL
#' @noRd
reshape_direction_note <- function(format, direction) {
  if (identical(format, "long") && identical(direction, "longer")) {
    return("資料看起來已經是長格式，確定要再轉成長格式嗎？")
  }
  if (identical(format, "wide") && identical(direction, "wider")) {
    return("資料看起來已經是寬格式，確定要再轉成寬格式嗎？")
  }
  NULL
}

#' 目前使用哪一份資料；只要 1 個檔案就能用
#' @noRd
reshape_source_note_ui <- function(n_files, source, code_cells, ns) {
  if (n_files == 0) {
    return(kit_callout(
      warn = TRUE,
      "還沒有檔案。",
      shiny::actionLink(ns("goto_upload"), "到「上傳檔案」加入檔案")
    ))
  }
  kit_source_note_ui(max(n_files, 2L), source, code_cells, ns)
}
