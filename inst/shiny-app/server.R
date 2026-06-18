server <- function(input, output, session) {

  # ── 上傳 slot 狀態 ─────────────────────────────────────────────────────────
  n_created  <- reactiveVal(0L)        # 累計建立的 slot 數（只增不減，用於產生唯一 ID）
  slot_order <- reactiveVal(integer(0)) # 已上傳檔案的 slot ID（使用者排序）

  slot_file  <- function(i) input[[paste0("file_", i)]]
  slot_label <- function(i) {
    v <- input[[paste0("label_", i)]]
    trimws(if (is.null(v)) "" else v)
  }
  slot_name  <- function(i) {
    fi <- slot_file(i)
    if (is.null(fi)) NA_character_ else fi$name
  }

  # 新增一個上傳 slot（insertUI + 註冊觀察器）
  add_slot <- function() {
    id <- n_created() + 1L
    n_created(id)

    insertUI(
      selector  = "#file_slots_container",
      where     = "beforeEnd",
      immediate = TRUE,
      ui = shiny::div(
        id    = paste0("file_slot_", id),
        style = paste0(
          "border: 1px solid #e0e0e0; border-radius: 4px;",
          " padding: 0.75rem; margin-bottom: 0.5rem;"
        ),
        shiny::fluidRow(
          shiny::column(
            6,
            shiny::fileInput(
              paste0("file_", id),
              label   = paste0("檔案 ", id),
              multiple = FALSE,
              accept  = c(".csv", ".sav", ".zsav"),
              width   = "100%"
            )
          ),
          shiny::column(
            4,
            shiny::textInput(
              paste0("label_", id),
              "後綴或波次名稱",
              value = paste0("t", id),
              width = "100%"
            )
          ),
          shiny::column(
            2,
            shiny::br(),
            shiny::actionButton(
              paste0("remove_slot_", id), "移除",
              class = "btn-sm btn-outline-danger"
            )
          )
        )
      )
    )

    # 當此 slot 上傳檔案時，加入 slot_order
    shiny::observeEvent(input[[paste0("file_", id)]], {
      ord <- slot_order()
      if (!id %in% ord) slot_order(c(ord, id))
      reset_results()
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # 上移按鈕：在 slot_order 中往前移
    shiny::observeEvent(input[[paste0("up_slot_", id)]], {
      ord <- slot_order()
      pos <- match(id, ord)
      if (!is.na(pos) && pos > 1L) {
        ord[c(pos - 1L, pos)] <- ord[c(pos, pos - 1L)]
        slot_order(ord)
      }
    }, ignoreInit = TRUE)

    # 下移按鈕：在 slot_order 中往後移
    shiny::observeEvent(input[[paste0("down_slot_", id)]], {
      ord <- slot_order()
      pos <- match(id, ord)
      if (!is.na(pos) && pos < length(ord)) {
        ord[c(pos, pos + 1L)] <- ord[c(pos + 1L, pos)]
        slot_order(ord)
      }
    }, ignoreInit = TRUE)

    # 移除按鈕：從 DOM 與 slot_order 移除
    shiny::observeEvent(input[[paste0("remove_slot_", id)]], {
      removeUI(selector = paste0("#file_slot_", id))
      slot_order(setdiff(slot_order(), id))
      reset_results()
    }, ignoreInit = TRUE, once = TRUE)
  }

  # 初始化：session 就緒後建立第一個 slot
  shiny::observeEvent(TRUE, { add_slot() }, once = TRUE)

  # 按「+ 新增上傳框」
  shiny::observeEvent(input$add_file_slot, { add_slot() })


  # ── 結果狀態 ───────────────────────────────────────────────────────────────
  join_status      <- reactiveVal(list(state = "idle", message = ""))
  stack_status     <- reactiveVal(list(state = "idle", message = ""))
  join_audit       <- reactiveVal(NULL)
  stack_audit      <- reactiveVal(NULL)
  result_data      <- reactiveVal(NULL)
  result_mode      <- reactiveVal(NULL)
  attrition_result <- reactiveVal(NULL)
  attrition_status <- reactiveVal(list(state = "idle", message = ""))

  reset_results <- function() {
    join_status(list(state = "idle", message = ""))
    stack_status(list(state = "idle", message = ""))
    join_audit(NULL)
    stack_audit(NULL)
    result_data(NULL)
    result_mode(NULL)
  }


  # ── 讀取上傳的資料 ─────────────────────────────────────────────────────────
  loaded_data <- reactive({
    ord <- slot_order()
    req(length(ord) >= 1)
    purrr::map(ord, function(i) {
      fi <- slot_file(i)
      req(!is.null(fi))
      read_data_file(fi$datapath, fi$name)
    })
  })

  loaded_names <- reactive({
    ord <- slot_order()
    req(length(ord) >= 1)
    purrr::map_chr(ord, function(i) {
      fi <- slot_file(i)
      if (is.null(fi)) "" else fi$name
    })
  })

  get_file_labels <- reactive({
    ord <- slot_order()
    req(length(ord) >= 1)
    purrr::map_chr(ord, function(i) {
      v <- input[[paste0("label_", i)]]
      trimws(if (is.null(v)) "" else v)
    })
  })


  # ── 已上傳檔案的排序 UI ────────────────────────────────────────────────────
  output$file_order_ui <- renderUI({
    ord <- slot_order()
    if (length(ord) == 0) return(NULL)

    shiny::tagList(
      shiny::h4("已上傳檔案（串接 / 疊加順序）"),
      shiny::tags$small(
        class = "text-muted",
        "後綴或波次名稱：寬串接時自動加底線（t1 → _t1）；",
        "長格式疊加時直接作為 wave 欄位值。"
      ),
      shiny::br(),
      lapply(seq_along(ord), function(pos) {
        slot_i <- ord[pos]
        shiny::fluidRow(
          shiny::column(
            5,
            shiny::strong(slot_name(slot_i)),
            shiny::br(),
            shiny::tags$small(class = "text-muted", paste0("順序：", pos))
          ),
          shiny::column(
            4,
            shiny::tags$span(
              class = "text-muted",
              "波次：", slot_label(slot_i)
            )
          ),
          shiny::column(
            3,
            shiny::br(),
            shiny::actionButton(
              paste0("up_slot_", slot_i), "↑ 上移", class = "btn-sm"
            ),
            shiny::actionButton(
              paste0("down_slot_", slot_i), "↓ 下移", class = "btn-sm"
            )
          )
        )
      })
    )
  })


  # ── 各檔共有欄位（供串接 by_vars 使用）────────────────────────────────────
  common_cols <- reactive({
    dfs <- loaded_data()
    req(length(dfs) >= 1)
    purrr::reduce(purrr::map(dfs, names), intersect)
  })

  shiny::observeEvent(common_cols(), {
    common   <- common_cols()
    selected <- input$by_vars
    if (is.null(selected)) selected <- character(0)
    selected <- intersect(selected, common)
    if (length(selected) == 0 && "release_id" %in% common) {
      selected <- "release_id"
    }
    shiny::updateSelectizeInput(session, "by_vars",
      choices = common, selected = selected)
  }, ignoreNULL = FALSE)


  # ── 預覽第一個檔案 ─────────────────────────────────────────────────────────
  output$preview_meta <- renderText({
    req(length(slot_order()) >= 1)
    df <- loaded_data()[[1]]
    n_show <- min(ncol(df), 20L)
    format_dims(nrow(df), ncol(df), n_show)
  })

  output$preview_table <- renderTable({
    req(length(slot_order()) >= 1)
    df <- loaded_data()[[1]]
    df <- preview_df_cols(df, priority_cols = input$by_vars)
    df_for_display(df, 5)
  }, striped = TRUE, spacing = "s")


  # ── 寬格式串接 ─────────────────────────────────────────────────────────────
  shiny::observeEvent(input$run_join, {
    stack_audit(NULL)
    result_data(NULL)
    result_mode(NULL)
    join_audit(NULL)
    join_status(list(state = "running", message = "串接進行中，請稍候…"))

    prog <- Progress$new(session, min = 0, max = 1)
    prog$set(message = "正在串接資料", value = 0)

    by_vars  <- input$by_vars
    n_loaded <- length(slot_order())

    if (n_loaded < 2) {
      msg <- "串接失敗：請至少上傳 2 個檔案。"
      join_status(list(state = "error", message = msg))
      showNotification(msg, type = "error", duration = 8)
      prog$close()
      return()
    }
    if (length(common_cols()) == 0) {
      msg <- "串接失敗：各檔沒有共同欄位，無法串接。"
      join_status(list(state = "error", message = msg))
      showNotification(msg, type = "error", duration = 8)
      prog$close()
      return()
    }
    if (is.null(by_vars) || length(by_vars) == 0) {
      msg <- "串接失敗：請至少選擇一個串接 key。"
      join_status(list(state = "error", message = msg))
      showNotification(msg, type = "error", duration = 8)
      prog$close()
      return()
    }

    tryCatch({
      prog$set(value = 0.05, detail = "讀取與套用後綴…")
      dfs        <- loaded_data()
      file_names <- loaded_names()
      labels     <- get_file_labels()
      join_fn    <- get(input$join_method, envir = asNamespace("dplyr"))

      ordered_dfs <- purrr::map(seq_along(dfs), function(i) {
        suffix <- join_suffix(labels[i])
        apply_suffix(dfs[[i]], suffix, by_vars)
      })

      n_files <- length(ordered_dfs)
      result  <- ordered_dfs[[1]]

      if (n_files >= 2) {
        for (i in 2:n_files) {
          prog$set(
            value  = 0.05 + 0.65 * (i - 1) / (n_files - 1),
            detail = paste0("合併第 ", i, " / ", n_files,
                            " 個檔案（", file_names[i], "）")
          )
          result <- join_fn(result, ordered_dfs[[i]], by = by_vars)
        }
      }

      prog$set(value = 0.75, detail = "核對列欄數…")
      audit <- build_join_audit(ordered_dfs, file_names, by_vars,
                                input$join_method, result)

      prog$set(value = 0.85, detail = "更新檢核結果…")
      join_audit(audit)
      result_data(result)
      result_mode("wide")

      check_bits <- c(
        if (audit$ncol_ok) "欄位數符合" else "欄位數不符",
        if (is.na(audit$nrow_ok)) "列數需人工確認（by 鍵有重複）"
        else if (audit$nrow_ok)   "列數符合"
        else                      "列數不符"
      )
      success_msg <- paste0(
        "串接成功！共 ", n_files, " 個檔案；",
        format_dims(nrow(result), ncol(result)),
        "（", paste(check_bits, collapse = "；"), "）"
      )

      prog$set(value = 0.92, detail = "產生預覽…")
      session$onFlushed(function() {
        join_status(list(state = "success", message = success_msg))
        showNotification("串接成功", type = "message", duration = 5)
        prog$set(value = 1, detail = "完成")
        prog$close()
      }, once = TRUE)
    }, error = function(e) {
      msg <- paste0("串接失敗：", conditionMessage(e))
      join_status(list(state = "error", message = msg))
      showNotification(msg, type = "error", duration = 10)
      prog$close()
    })
  })

  output$join_status_ui <- renderUI(make_status_ui(join_status()))

  output$join_summary <- renderText({
    req(result_data(), result_mode() == "wide")
    format_dims(nrow(result_data()), ncol(result_data()))
  })

  output$audit_verdict_ui <- renderUI({
    audit <- join_audit()
    req(audit)
    all_ok  <- isTRUE(audit$ncol_ok) && (isTRUE(audit$nrow_ok) || is.na(audit$nrow_ok))
    partial <- isTRUE(audit$ncol_ok) && isFALSE(audit$nrow_ok)

    if (all_ok && !audit$any_dup) {
      cls <- "alert alert-success"
      msg <- "核對通過：欄位數與列數皆符合目前串接規則（by 鍵各檔皆唯一）。"
    } else if (all_ok && audit$any_dup) {
      cls <- "alert alert-warning"
      msg <- paste0(
        "欄位數符合規則；列數無法自動比對單一預期值（by 鍵：",
        audit$by_vars,
        " 存在重複列，可能為一對多合併）。請參考下方各檔「重複鍵組數」。"
      )
    } else if (partial) {
      cls <- "alert alert-warning"
      msg <- "欄位數符合，但列數與依 join 方法推算的預期值不一致，請檢查 by 變項或 join 方法是否正確。"
    } else {
      cls <- "alert alert-danger"
      msg <- "核對未通過：欄位數或列數與預期不符，請檢查後綴設定、by 變項與串接順序。"
    }
    shiny::tags$div(
      class = cls, role = "alert",
      shiny::tags$strong("【核對】"), " ", msg,
      shiny::tags$br(),
      shiny::tags$small(
        "串接方法：", audit$join_method, "　by 變項：", audit$by_vars
      )
    )
  })

  output$audit_rule_note <- renderText({
    req(join_audit())
    join_audit()$rule_note
  })

  output$audit_files   <- renderTable({ req(join_audit()); join_audit()$file_stats })
  output$audit_steps   <- renderTable({
    audit <- join_audit(); req(audit)
    if (nrow(audit$join_steps) == 0)
      return(data.frame(說明 = "僅一個檔案，無逐步合併"))
    audit$join_steps
  })
  output$audit_summary <- renderTable({ req(join_audit()); join_audit()$summary })

  output$join_preview_meta <- renderText({
    req(result_data(), result_mode() == "wide")
    df <- result_data()
    format_dims(nrow(df), ncol(df), min(ncol(df), 20L))
  })

  output$join_preview <- renderTable({
    req(result_data(), result_mode() == "wide")
    df <- preview_df_cols(result_data(), priority_cols = input$by_vars)
    df_for_display(df, 10)
  }, striped = TRUE, spacing = "s")


  # ── 長格式疊加 ─────────────────────────────────────────────────────────────
  shiny::observeEvent(input$run_stack, {
    join_audit(NULL)
    stack_audit(NULL)
    result_data(NULL)
    result_mode(NULL)
    stack_status(list(state = "running", message = "疊加進行中，請稍候…"))

    prog <- Progress$new(session, min = 0, max = 1)
    prog$set(message = "正在疊加資料", value = 0)

    n_loaded <- length(slot_order())
    if (n_loaded < 1) {
      msg <- "疊加失敗：請至少上傳 1 個檔案。"
      stack_status(list(state = "error", message = msg))
      showNotification(msg, type = "error", duration = 8)
      prog$close()
      return()
    }

    wave_labels <- get_file_labels()
    if (any(!nzchar(wave_labels))) {
      msg <- "疊加失敗：請為每個檔案填寫後綴或波次名稱。"
      stack_status(list(state = "error", message = msg))
      showNotification(msg, type = "error", duration = 8)
      prog$close()
      return()
    }
    if (any(duplicated(wave_labels))) {
      msg <- "疊加失敗：後綴或波次名稱不可重複。"
      stack_status(list(state = "error", message = msg))
      showNotification(msg, type = "error", duration = 8)
      prog$close()
      return()
    }

    tryCatch({
      prog$set(value = 0.1, detail = "讀取檔案…")
      dfs        <- loaded_data()
      file_names <- loaded_names()
      n_files    <- length(dfs)

      prog$set(value = 0.25, detail = "檢查欄位型別…")
      stacked <- stack_files_long(dfs, wave_labels)
      result  <- stacked$result

      prog$set(value = 0.65, detail = "核對列欄數…")
      audit <- build_stack_audit(dfs, file_names, wave_labels,
                                 result, stacked$harmonized_cols)

      prog$set(value = 0.85, detail = "更新檢核結果…")
      stack_audit(audit)
      result_data(result)
      result_mode("long")

      check_bits <- c(
        if (audit$nrow_ok) "列數符合" else "列數不符",
        if (audit$ncol_ok) "欄位數符合" else "欄位數不符",
        if (audit$wave_ok) "wave 列數符合" else "wave 列數不符"
      )
      success_msg <- paste0(
        "疊加成功！共 ", n_files, " 個檔案；",
        format_dims(nrow(result), ncol(result)),
        "（", paste(check_bits, collapse = "；"), "）"
      )

      prog$set(value = 0.92, detail = "產生預覽…")
      session$onFlushed(function() {
        stack_status(list(state = "success", message = success_msg))
        showNotification("疊加成功", type = "message", duration = 5)
        prog$set(value = 1, detail = "完成")
        prog$close()
      }, once = TRUE)
    }, error = function(e) {
      msg <- paste0("疊加失敗：", conditionMessage(e))
      stack_status(list(state = "error", message = msg))
      showNotification(msg, type = "error", duration = 10)
      prog$close()
    })
  })

  output$stack_status_ui <- renderUI(make_status_ui(stack_status()))

  output$stack_summary <- renderText({
    req(result_data(), result_mode() == "long")
    format_dims(nrow(result_data()), ncol(result_data()))
  })

  output$stack_audit_verdict_ui <- renderUI({
    audit <- stack_audit()
    req(audit)
    all_ok <- isTRUE(audit$nrow_ok) && isTRUE(audit$ncol_ok) && isTRUE(audit$wave_ok)

    if (all_ok) {
      cls <- "alert alert-success"
      msg <- "核對通過：列數、欄位數與各 wave 列數皆符合疊加規則。"
    } else if (isTRUE(audit$nrow_ok) && isTRUE(audit$ncol_ok)) {
      cls <- "alert alert-warning"
      msg <- "列數與欄位數符合，但部分 wave 列數與來源檔案不一致，請檢查。"
    } else {
      cls <- "alert alert-danger"
      msg <- "核對未通過：列數或欄位數與預期不符，請檢查上傳檔案與波次名稱。"
    }
    shiny::tags$div(
      class = cls, role = "alert",
      shiny::tags$strong("【核對】"), " ", msg,
      if (length(audit$harmonized_cols) > 0) {
        shiny::tagList(
          shiny::tags$br(),
          shiny::tags$small("已統一型別欄位：",
                            paste(audit$harmonized_cols, collapse = ", "))
        )
      }
    )
  })

  output$stack_audit_rule_note <- renderText({ req(stack_audit()); stack_audit()$rule_note })
  output$stack_audit_files     <- renderTable({ req(stack_audit()); stack_audit()$file_stats })
  output$stack_audit_summary   <- renderTable({ req(stack_audit()); stack_audit()$summary })

  output$stack_preview_meta <- renderText({
    req(result_data(), result_mode() == "long")
    df <- result_data()
    format_dims(nrow(df), ncol(df), min(ncol(df), 20L))
  })

  output$stack_preview <- renderTable({
    req(result_data(), result_mode() == "long")
    df <- preview_df_cols(result_data(), priority_cols = "wave")
    df_for_display(df, 10)
  }, striped = TRUE, spacing = "s")


  # ── Attrition ──────────────────────────────────────────────────────────────

  # 更新前期 / 後期選單（有 >= 1 個已上傳檔案時啟用）
  shiny::observe({
    ord  <- slot_order()
    nms  <- if (length(ord) >= 1) loaded_names() else character(0)
    ch   <- setNames(as.character(ord), nms)
    prev_base   <- input$attrition_baseline
    prev_follow <- input$attrition_followup
    shiny::updateSelectInput(session, "attrition_baseline",
      choices  = ch,
      selected = if (!is.null(prev_base) && prev_base %in% ch) prev_base else ch[1]
    )
    shiny::updateSelectInput(session, "attrition_followup",
      choices  = ch,
      selected = if (!is.null(prev_follow) && prev_follow %in% ch) prev_follow else ch[min(2, length(ch))]
    )
  })

  # 更新 ID 欄位選單（前後期共有欄位）
  attrition_common_cols <- reactive({
    ord <- slot_order()
    req(length(ord) >= 2)
    base_slot   <- suppressWarnings(as.integer(input$attrition_baseline))
    follow_slot <- suppressWarnings(as.integer(input$attrition_followup))
    req(!is.na(base_slot), !is.na(follow_slot), base_slot != follow_slot)
    dfs         <- loaded_data()
    base_pos    <- match(base_slot, ord)
    follow_pos  <- match(follow_slot, ord)
    req(!is.na(base_pos), !is.na(follow_pos))
    intersect(names(dfs[[base_pos]]), names(dfs[[follow_pos]]))
  })

  shiny::observeEvent(
    list(input$attrition_baseline, input$attrition_followup, slot_order()),
    {
      cols <- tryCatch(attrition_common_cols(), error = function(e) character(0))
      prev <- input$attrition_id_vars
      if (is.null(prev)) prev <- character(0)
      selected <- intersect(prev, cols)
      if (length(selected) == 0 && "release_id" %in% cols) selected <- "release_id"
      shiny::updateSelectizeInput(session, "attrition_id_vars",
        choices = cols, selected = selected)
    },
    ignoreNULL = FALSE
  )

  # 執行 Attrition
  shiny::observeEvent(input$run_attrition, {
    attrition_result(NULL)
    attrition_status(list(state = "running", message = "Attrition 計算中，請稍候…"))

    id_vars     <- input$attrition_id_vars
    base_slot   <- suppressWarnings(as.integer(input$attrition_baseline))
    follow_slot <- suppressWarnings(as.integer(input$attrition_followup))
    ord         <- slot_order()

    if (is.na(base_slot) || is.na(follow_slot)) {
      msg <- "失敗：請選擇前期與後期樣本。"
      attrition_status(list(state = "error", message = msg))
      showNotification(msg, type = "error", duration = 8)
      return()
    }
    if (base_slot == follow_slot) {
      msg <- "失敗：前期與後期樣本不可選擇同一個檔案。"
      attrition_status(list(state = "error", message = msg))
      showNotification(msg, type = "error", duration = 8)
      return()
    }
    if (is.null(id_vars) || length(id_vars) == 0) {
      msg <- "失敗：請至少選擇一個 ID 欄位。"
      attrition_status(list(state = "error", message = msg))
      showNotification(msg, type = "error", duration = 8)
      return()
    }

    base_pos   <- match(base_slot, ord)
    follow_pos <- match(follow_slot, ord)
    if (is.na(base_pos) || is.na(follow_pos)) {
      msg <- "失敗：選定的檔案已移除，請重新選擇。"
      attrition_status(list(state = "error", message = msg))
      showNotification(msg, type = "error", duration = 8)
      return()
    }

    tryCatch({
      dfs         <- loaded_data()
      baseline_df <- dfs[[base_pos]]
      followup_df <- dfs[[follow_pos]]

      res <- build_attrition(baseline_df, followup_df, id_vars)
      attrition_result(res)

      msg <- paste0(
        "完成！前期 ", nrow(baseline_df), " 列；",
        "留存 ", sum(res$result$attrition == 0L), " 筆，",
        "流失 ", sum(res$result$attrition == 1L), " 筆。",
        if (res$followup_has_dup) "（後期 ID 有重複列，已 distinct 處理）" else ""
      )
      attrition_status(list(state = "success", message = msg))
      showNotification("Attrition 完成", type = "message", duration = 5)
    }, error = function(e) {
      msg <- paste0("失敗：", conditionMessage(e))
      attrition_status(list(state = "error", message = msg))
      showNotification(msg, type = "error", duration = 10)
    })
  })

  output$attrition_status_ui <- renderUI(make_status_ui(attrition_status()))

  output$attrition_summary_table <- renderTable({
    res <- attrition_result()
    req(res)
    res$summary
  })

  output$attrition_preview <- renderTable({
    res <- attrition_result()
    req(res)
    df <- preview_df_cols(res$result, priority_cols = c(input$attrition_id_vars, "attrition"))
    df_for_display(df, 10)
  }, striped = TRUE, spacing = "s")

  output$download_attrition_csv <- downloadHandler(
    filename = function() {
      paste0("attrition_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      res <- attrition_result()
      req(res)
      readr::write_csv(fix_utf8_df(res$result), file)
    }
  )


  # ── 匯出（寬格式串接 / 長格式疊加）────────────────────────────────────────
  export_filename <- function(ext) {
    prefix <- if (identical(result_mode(), "long")) "stacked_" else "joined_"
    paste0(prefix, format(Sys.time(), "%Y%m%d_%H%M%S"), ".", ext)
  }

  output$download_csv <- downloadHandler(
    filename = function() export_filename("csv"),
    content = function(file) {
      req(result_data())
      df <- prepare_export(result_data(), result_mode(), input$by_vars)
      readr::write_csv(df, file)
    }
  )

  output$download_sav <- downloadHandler(
    filename = function() export_filename("sav"),
    content = function(file) {
      req(result_data())
      df <- prepare_export(result_data(), result_mode(), input$by_vars)
      haven::write_sav(df, file)
    }
  )
}
