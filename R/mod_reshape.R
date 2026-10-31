# 長/寬格式轉換：tidyr::pivot_wider() / pivot_longer()
#
# 主畫面只寫白話（「哪一欄的值變成新的欄名」）；tidyr 的函式名與參數名
# 收在「進階設定」與「用 R 怎麼做」裡（KIT 設計語言 1.7 白話先行）。

#' 進階設定裡的控制項標籤：中文說明 + tidyr 參數名
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
        "選要轉換的檔案與方向（會先依資料的樣子幫你選好）",
        "設定欄位（R 的參數名稱在「進階設定」與「用 R 怎麼做」裡）",
        "按「執行轉換」，看結果、檢查與預覽",
        "下載 CSV 或 SAV"
      ),
      output = "轉換後的一個資料檔；轉換前後的列數 × 欄數會並列在摘要中。",
      technical = paste0(
        "長轉寬用 tidyr::pivot_wider()，寬轉長用 tidyr::pivot_longer()，預設值同 tidyr。",
        "長轉寬不做 values_fn：id_cols × names_from 有重複時停下並列出重複列，",
        "不會默默彙總，也不會產生 list 欄。"
      ),
      diagram = reshape_diagram()
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
            "長轉寬（每人一列、各題並排）" = "wider",
            "寬轉長（每人每題一列）" = "longer"
          ),
          selected = "wider", inline = TRUE
        )
      )
    ),
    # 一個提示：資料看起來是長或寬格式；和方向相反時是黃色，「改選…」的按鈕在框外面
    shiny::uiOutput(ns("guess")),
    kit_advanced(
      title = "判斷錯了？換一欄當 ID 再看一次",
      shiny::selectizeInput(
        ns("guess_id"), "判斷長、寬格式時，用哪一欄當 ID",
        choices = c("（請選）" = ""), width = "16rem"
      )
    ),
    shiny::conditionalPanel(is_wider, ns = ns, wider_controls_ui(ns)),
    shiny::conditionalPanel(
      "input.direction == 'longer'", ns = ns,
      mod_pivot_longer_ui(ns("longer"))
    ),
    shiny::uiOutput(ns("settings_line")),
    shiny::uiOutput(ns("ready")),
    kit_button(ns("run"), "執行轉換", "primary", disabled = TRUE),
    shiny::uiOutput(ns("status")),
    shiny::uiOutput(ns("blocked")),
    kit_download_ui(ns),
    kit_details_row(
      shiny::uiOutput(ns("result_details"), inline = TRUE),
      kit_code_panel(ns("code"))
    )
  )
}

# 長轉寬控制項的白話名稱（主畫面、設定摘要、錯誤訊息用同一套）
WIDER_LABELS <- c(
  names_from = "哪一欄的值變成新的欄名",
  values_from = "要填進新欄的值",
  id_cols = "辨認同一列的欄"
)

#' 長轉寬的控制項（參數一對一對應 tidyr::pivot_wider()；參數名在進階設定裡）
#' @noRd
wider_controls_ui <- function(ns) {
  shiny::tagList(
    shiny::uiOutput(ns("preset")),
    shiny::fluidRow(
      shiny::column(
        5,
        shiny::selectizeInput(
          ns("names_from"), WIDER_LABELS[["names_from"]],
          choices = NULL,
          options = list(placeholder = "例如 wave 或 item")
        )
      ),
      shiny::column(
        7,
        shiny::selectizeInput(
          ns("values_from"), paste0(WIDER_LABELS[["values_from"]], "（可多選）"),
          choices = NULL, multiple = TRUE,
          options = list(placeholder = "例如 score")
        )
      )
    ),
    shiny::checkboxInput(
      ns("id_auto"),
      "其餘的欄都用來辨認同一列（預設）",
      value = TRUE, width = "100%"
    ),
    shiny::conditionalPanel(
      "!input.id_auto", ns = ns,
      shiny::selectizeInput(
        ns("id_cols"), paste0(WIDER_LABELS[["id_cols"]], "（可多選）"),
        choices = NULL, multiple = TRUE, width = "100%"
      )
    ),
    shiny::uiOutput(ns("id_note")),
    kit_advanced(
      id = ns("advanced"),
      title = "進階設定：欄名格式、欄位排列、缺的格子、R 的參數名稱",
      wider_advanced_ui(ns)
    )
  )
}

#' 長轉寬的進階控制項（預設收起）：最上面一行對照 tidyr 的參數名
#' @noRd
wider_advanced_ui <- function(ns) {
  shiny::tagList(
    shiny::tags$p(
      class = "kit-hint",
      "對照 R 的 ", shiny::tags$code("tidyr::pivot_wider()"), "：",
      "「", WIDER_LABELS[["names_from"]], "」是 ", shiny::tags$code("names_from"), "；",
      "「", WIDER_LABELS[["values_from"]], "」是 ", shiny::tags$code("values_from"), "；",
      "「", WIDER_LABELS[["id_cols"]], "」是 ", shiny::tags$code("id_cols"),
      "（勾「其餘的欄」時是 ", shiny::tags$code("id_cols = NULL"), "）。"
    ),
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
        placeholder = "{.value}_{wave}"
      ),
      shiny::tags$small(
        class = "kit-hint",
        "{.value} 是值欄的名稱（如 score），{wave} 是欄名來源欄的值（如 t1）。"
      )
    ),
    shiny::radioButtons(
      ns("names_vary"), arg_label("欄位排列", "names_vary"),
      choices = c(
        "fastest：同一個值欄放在一起（score_t1、score_t2、se_t1…）" = "fastest",
        "slowest：同一個欄名來源的值放在一起（score_t1、se_t1、score_t2…）" = "slowest"
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
      guess_sel <- if (isTRUE(input$guess_id %in% all)) input$guess_id else reshape_guess_id(all)
      shiny::updateSelectizeInput(
        session, "guess_id", choices = c("（請選）" = "", all), selected = guess_sel %||% ""
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
    # 換檔案時：方向預設跟著判斷走（看起來是寬格式 → 寬轉長）；之後使用者可以自己改
    shiny::observeEvent(input$file, {
      to <- shiny::isolate(guess())$suggest
      if (!is.null(to) && !identical(to, input$direction)) {
        shiny::updateRadioButtons(session, "direction", selected = to)
      }
    }, ignoreInit = TRUE)

    # 一個提示框：判斷的結果；方向和判斷相反時是黃色，框外面一顆「改選…」
    output$guess <- shiny::renderUI({
      g <- guess()
      if (is.null(g)) return(NULL)
      to <- g$suggest
      against <- !is.null(to) && !identical(to, input$direction)
      names_dir <- c(wider = "長轉寬", longer = "寬轉長")
      shiny::tagList(
        kit_callout(
          warn = against,
          paste0(
            reshape_guess_text(g),
            if (against) paste0("現在選的是「", names_dir[[input$direction]], "」，確定嗎？")
          )
        ),
        if (against) {
          shiny::tags$div(
            class = "mb-3",
            kit_button(ns("guess_flip_btn"), paste0("改選「", names_dir[[to]], "」"))
          )
        }
      )
    })
    shiny::observeEvent(input$guess_flip_btn, {
      to <- guess()$suggest
      if (!is.null(to)) shiny::updateRadioButtons(session, "direction", selected = to)
    })

    output$id_note <- shiny::renderUI({
      df <- src()
      if (is.null(df)) return(NULL)
      ids <- wider_id_cols(df, wider())
      shiny::tags$small(
        class = "kit-hint",
        paste0(
          "目前用來辨認同一列的欄（", kit_num(length(ids)), " 欄）：",
          paste(utils::head(ids, 12), collapse = "、"),
          if (length(ids) > 12) " …" else "", "。",
          "轉換後，這些欄的每一組值是一列。"
        )
      )
    })

    # kitsUI 分數表：一行小字加一顆按鈕（只填控制項，不自動執行；不放進提示框）
    output$preset <- shiny::renderUI({
      if (!is_kitsui_score_table(cols())) return(NULL)
      shiny::tags$div(
        class = "kit-preset mb-3",
        shiny::tags$p(
          class = "kit-hint",
          "這個檔案看起來是 kitsUI 的分數表（一列 = 一次施測 × 一個領域）。",
          "按下面的按鈕只會填好設定，不會自動執行；要填的值會多放 scale，",
          "d_score 空白時才分得出是沒測這個領域，還是有測但還沒放上 D 量尺。"
        ),
        kit_button(ns("apply_preset"), "套用 kitsUI 分數表預設")
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

    # 主要按鈕：沒有檔案、還沒選欄時停用，上面一行寫還差什麼
    kit_run_gate(session, output, missing = shiny::reactive({
      if (length(files$keys()) == 0) return(kit_need_files(0L, 1L))
      if (is.null(picked())) return("選要轉換的檔案")
      if (identical(input$direction, "longer")) {
        a <- longer()
        if (length(a$cols) == 0 && !isTRUE(a$cols_exclude)) return("選要轉成長格式的欄")
        return(character(0))
      }
      w <- wider()
      c(
        if (length(w$names_from) == 0) paste0("選「", WIDER_LABELS[["names_from"]], "」"),
        if (length(w$values_from) == 0) paste0("選「", WIDER_LABELS[["values_from"]], "」")
      )
    }), link = function() {
      if (length(files$keys()) == 0) shiny::actionLink(ns("goto_upload"), "到「檔案上傳」加入檔案 →")
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
          paste0(if (isTRUE(a$cols_exclude)) "不轉 " else "轉 ", kit_num(n), " 欄")
        } else {
          "還沒選要轉的欄"
        })
      } else {
        w <- wider()
        c("長轉寬",
          if (length(w$names_from) > 0) paste0("新欄名來自 ", paste(w$names_from, collapse = "、")),
          if (length(w$values_from) > 0) {
            paste0("填入 ", paste(w$values_from, collapse = "、"))
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
          if (is.null(df)) kit_abort("請先到「檔案上傳」加入檔案。")
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
            warnings = out$warnings, points = out$summary
          )
        }),
        error = function(e) {
          kit_error_status(e, "轉換", kit_upload_todo(ns, length(files$keys())))
        }
      )
      status(st)
      kit_notify(st)
      kit_scroll_to("status")
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

    # 重點都在狀態框裡；空白格、預覽、id 欄不一致的列收在下面一排細節裡
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
              "「沒有這一列」＝原檔沒有這組（辨認同一列的欄 × ", names(res$empty)[1], "）的列；",
              "「有列但空白」＝有這一列，但值本來就是空的。"
            ),
            shiny::tags$div(class = "kit-table-scroll", shiny::tableOutput(ns("empty_table")))
          )
        },
        if (is_wider && !res$id_check$ok) {
          kit_audit_detail(
            shiny::h6(paste0(
              "辨認同一列的欄不一致的列（", kit_rows_note(nrow(res$id_check$rows), NULL, 50), "）"
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

#' 目前使用哪一份資料；只要 1 個檔案就能用
#' @noRd
reshape_source_note_ui <- function(n_files, source, code_cells, ns) {
  # 還沒有檔案時不另開提示框：主要按鈕上面一行「還差」已經說了
  if (n_files == 0) return(NULL)
  kit_source_note_ui(max(n_files, 2L), source, code_cells, ns)
}

#' 頁首的圖解：長轉寬（預設的方向）
#' @noRd
reshape_diagram <- function() {
  # 教育、心理的資料多半是寬格式，所以寬格式在左；兩個方向都能轉，用雙箭頭
  kit_diagram(
    before = kit_dg_table(data.frame(id = c(1, 2), q1 = c(4, 3), q2 = c(5, NA)), "寬格式"),
    after = kit_dg_table(
      data.frame(id = c(1, 1, 2, 2), item = c("q1", "q2", "q1", "q2"), score = c(4, 5, 3, NA)),
      "長格式"
    ),
    arrow = "⇄",
    caption = paste0(
      "寬轉長：每人一列、各題並排，變成每人每題一列（空白的值也留一列）；",
      "長轉寬則倒過來"
    )
  )
}
