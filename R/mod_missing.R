# ②（選用）遺漏值轉 NA

# 「依數值標籤」轉換前的預覽表最多列幾個欄位
KIT_LABEL_NA_ROWS <- 200L

#' @noRd
mod_missing_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    kit_tab_guide(
      goal = paste0(
        "把代表遺漏的代碼（例如 7777、9999，或 SPSS 數值標籤寫著「遺漏值」「跳答」的 98、99）",
        "改成真正的遺漏值（NA），避免它們在分析時被當成數字。這一步是選用的。"
      ),
      steps = kit_steps(
        "選擇判斷方式：用一組代碼套用到所有欄，或依 SPSS 數值標籤逐欄判斷",
        "確認代碼（已預填 KIT 常用代碼）或遺漏值標籤的關鍵字",
        "確認不要處理的欄位（ID 已預先排除）",
        "按「執行轉換」，檢查每個欄位替換了幾格",
        "只想改遺漏值：在「下載處理後的檔案」下載就完成了（1 個檔案就可以）",
        "要接著做：③～⑦ 分頁會自動使用處理後的資料"
      ),
      output = "每個檔案各自轉換；列數與欄位不變，只有遺漏值代碼變成空白（NA）。可以直接下載處理後的檔案。",
      note = paste0(
        "轉換後無法再區分遺漏原因（例如 7777 不適用、9999 拒答）；分析需要區分時請保留原檔。",
        "SAV 檔中 SPSS 已設定為遺漏的值，讀入時就已經是 NA。"
      ),
      technical = paste0(
        "依代碼：數值欄（含 SAV 標籤欄）中等於任一代碼的值改為 NA，SAV 標籤欄同時移除這些代碼的數值標籤；",
        "勾選「文字欄也比對」時，文字欄中完全等於代碼的值也會改為 NA。",
        "依數值標籤：每一欄只轉換自己的數值標籤含關鍵字（遺漏、跳答、不適用…）的值，",
        "而且代碼要是負數或大於其他選項的值（例如 1–5 分的 98、99），",
        "所以量表中間的「不清楚」或剛好等於 99 的真實數值（例如體重 99 公斤）不會被轉換。"
      )
    ),
    shiny::radioButtons(
      ns("na_mode"), "怎麼判斷遺漏值？",
      choices = c(
        "依代碼清單：同一組代碼套用到所有欄（例如 KIT 的 7777、9999）" = "codes",
        "依 SPSS 數值標籤：每一欄只轉換標籤寫著「遺漏、跳答、不適用…」的值" = "labels"
      ),
      selected = "codes", width = "100%"
    ),
    shiny::uiOutput(ns("mode_note")),
    shiny::conditionalPanel(
      "input.na_mode != 'labels'", ns = ns,
      shiny::textInput(
        ns("codes"), "遺漏值代碼（以逗號分隔）",
        value = kit_na_codes_default(), width = "60%"
      ),
      shiny::uiOutput(ns("codes_parsed"))
    ),
    shiny::conditionalPanel(
      "input.na_mode == 'labels'", ns = ns,
      shiny::textInput(
        ns("keywords"), "數值標籤含這些字的值，視為遺漏值（以逗號分隔）",
        value = kit_na_label_keywords_default(), width = "100%"
      ),
      shiny::uiOutput(ns("label_found"))
    ),
    shiny::selectizeInput(
      ns("exclude_cols"), "不要處理的欄位（例如 ID，可多選）",
      choices = NULL, multiple = TRUE, options = list(create = FALSE),
      width = "60%"
    ),
    shiny::tags$small(
      class = "kit-hint",
      "已預先排除名稱像 ID 的欄位（例如 id、release_id、w3id、學號），因為 ID 可能剛好等於 7777 或 9999。"
    ),
    shiny::conditionalPanel(
      "input.na_mode != 'labels'", ns = ns,
      shiny::checkboxInput(
        ns("match_text"), "文字欄也比對（例如 CSV 中被讀成文字的 \"9999\"）",
        value = TRUE, width = "100%"
      )
    ),
    shiny::checkboxInput(
      ns("use_downstream"), "③～⑦ 分頁使用處理後的資料",
      value = TRUE, width = "100%"
    ),
    shiny::actionButton(ns("run"), "執行轉換", class = "btn-primary"),
    shiny::uiOutput(ns("status")),
    shiny::uiOutput(ns("result")),
    kit_code_panel(ns("code"))
  )
}

#' @param files mod_upload_server() 的回傳值
#' @return list(data, source, code_cells)：後續分頁要用的資料
#' @noRd
mod_missing_server <- function(id, files) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    result <- shiny::reactiveVal(NULL)
    status <- shiny::reactiveVal(kit_status())

    codes <- shiny::reactive(parse_na_codes(input$codes))
    keywords <- shiny::reactive(
      parse_na_keywords(input$keywords %||% kit_na_label_keywords_default())
    )
    mode <- shiny::reactive(if (identical(input$na_mode, "labels")) "labels" else "codes")

    # 新的一批檔案：KIT 代碼一格都沒有、但 SPSS 數值標籤標了遺漏值時，改用「依數值標籤」
    auto_note <- shiny::reactiveVal(NULL)
    shiny::observeEvent(files$keys(), {
      dfs <- files$data()
      auto_note(NULL)
      if (length(dfs) == 0) return()
      kit_cells <- count_code_cells(dfs)
      label_cells <- count_code_cells(dfs, codes = numeric(0), keywords = keywords())
      if (kit_cells == 0 && label_cells > 0 && mode() == "codes") {
        shiny::updateRadioButtons(session, "na_mode", selected = "labels")
        auto_note(paste0(
          "資料中沒有 7777、9999 這類 KIT 代碼，但 SPSS 數值標籤標示了 ", label_cells,
          " 格遺漏值（例如「遺漏值」「跳答」），所以已改用「依 SPSS 數值標籤」。",
          "要用自己的代碼清單，請改選「依代碼清單」。"
        ))
      }
    })
    output$mode_note <- shiny::renderUI({
      note <- auto_note()
      if (is.null(note)) return(NULL)
      shiny::tags$div(class = "kit-callout", note)
    })

    # 依數值標籤：轉換前先列出每個欄位會轉換哪些代碼
    label_found <- shiny::reactive({
      label_na_table(
        files$data(), files$names(), keywords(),
        exclude = input$exclude_cols %||% character(0)
      )
    })
    output$label_found <- shiny::renderUI({
      if (mode() != "labels" || length(files$keys()) == 0) return(NULL)
      tbl <- label_found()
      if (nrow(tbl) == 0) {
        return(shiny::tags$div(
          class = "kit-callout kit-callout-warning",
          "沒有找到數值標籤含這些字的值。CSV 檔沒有數值標籤，請改用「依代碼清單」。"
        ))
      }
      shown <- utils::head(tbl, KIT_LABEL_NA_ROWS)
      shiny::tags$details(
        class = "kit-example",
        shiny::tags$summary(paste0(
          "偵測到 ", nrow(tbl), " 個欄位有遺漏值標籤，資料中共 ", sum(tbl$資料中的格數),
          " 格會轉成 NA（按這裡看每個欄位）"
        )),
        shiny::tags$small(
          class = "kit-hint",
          "某個欄位不該轉換（例如「不知道」在這題是有效答案）時，把它加到下方「不要處理的欄位」。"
        ),
        shiny::tags$div(class = "kit-table-scroll", kit_static_table(shown)),
        if (nrow(tbl) > nrow(shown)) {
          shiny::tags$small(
            class = "kit-hint",
            paste0("只列出前 ", nrow(shown), " 個欄位；轉換後的「各欄位替換格數」會列出全部。")
          )
        }
      )
    })
    output$codes_parsed <- shiny::renderUI({
      p <- codes()
      shiny::tags$div(
        class = "kit-codes-parsed",
        if (length(p$codes) > 0) {
          shiny::tags$small(
            class = "text-muted d-block",
            "會替換的代碼：",
            paste(
              format(
                p$codes,
                scientific = FALSE,
                trim = TRUE,
                drop0trailing = TRUE
              ),
              collapse = "、"
            )
          )
        },
        if (length(p$invalid) > 0) {
          shiny::tags$small(
            class = "text-danger d-block",
            "這些不是數字，會略過：", paste(p$invalid, collapse = "、")
          )
        }
      )
    })

    # 不處理的欄位：選項為各檔欄位聯集；新出現的欄位若像 ID 則預設選取
    all_cols <- shiny::reactive(unique(unlist(purrr::map(files$data(), names))))
    known_cols <- character(0)
    shiny::observeEvent(all_cols(), {
      cols <- all_cols()
      prev <- input$exclude_cols %||% character(0)
      selected <- union(
        intersect(prev, cols),
        guess_id_cols(setdiff(cols, known_cols))
      )
      known_cols <<- cols
      shiny::updateSelectizeInput(
        session,
        "exclude_cols",
        choices = cols,
        selected = selected
      )
    }, ignoreNULL = FALSE)

    # 舊結果失效時記下來：③～⑦ 才能告訴使用者「② 要重新執行」，而不是默默改用原始資料
    stale <- shiny::reactiveVal(FALSE)
    invalidate <- function() {
      if (is.null(result())) return()
      result(NULL)
      stale(TRUE)
      status(kit_stale_status("執行轉換"))
    }

    # 檔案變動：只是移除或調整順序時，其他檔案的轉換結果仍然有效，直接保留；
    # 加入新檔案時舊結果失效（新檔案還沒轉換）
    shiny::observeEvent(files$data(), {
      res <- result()
      if (is.null(res)) return()
      keys <- files$keys()
      if (length(keys) == 0) {
        result(NULL)
        stale(FALSE)
        status(kit_status())
        return()
      }
      if (!all(keys %in% res$keys)) {
        invalidate()
        return()
      }
      idx <- match(keys, res$keys)
      if (identical(idx, seq_along(res$keys))) return()
      result(list(
        keys = res$keys[idx],
        names = res$names[idx],
        data = res$data[idx],
        detail = res$detail[idx],
        summary = res$summary[idx, , drop = FALSE],
        settings = res$settings
      ))
      status(kit_done_status(paste0(
        "檔案已移除或調整順序；其他 ", length(keys), " 個檔案的轉換結果保留，不用重新執行。"
      )))
    }, ignoreInit = TRUE)

    # 實際影響轉換的設定（另一種判斷方式的輸入框改了不算）
    current_settings <- function() {
      list(
        mode = mode(),
        codes = if (mode() == "codes") codes()$codes,
        match_text = if (mode() == "codes") isTRUE(input$match_text),
        keywords = if (mode() == "labels") keywords(),
        exclude = sort(input$exclude_cols %||% character(0))
      )
    }
    same_settings <- function(a, b) {
      # 排除欄只比對目前檔案裡有的欄：移除檔案時，它的 ID 從清單消失不算改設定
      cols <- all_cols()
      a$exclude <- intersect(a$exclude, cols)
      b$exclude <- intersect(b$exclude, cols)
      identical(a, b)
    }

    # 代碼、判斷方式、關鍵字、排除欄、文字比對改了：舊結果失效
    shiny::observeEvent(
      list(
        input$na_mode, input$codes, input$keywords, input$exclude_cols,
        input$match_text, all_cols()
      ),
      {
        res <- result()
        if (!is.null(res) && !same_settings(res$settings, current_settings())) invalidate()
      },
      ignoreInit = TRUE, ignoreNULL = FALSE
    )

    shiny::observeEvent(input$run, {
      stale(FALSE)
      result(NULL)
      dfs <- files$data()
      st <- tryCatch({
        if (length(dfs) == 0) kit_abort("請先到「① 上傳檔案」加入檔案。")
        exclude <- input$exclude_cols %||% character(0)
        converted <- if (mode() == "labels") {
          if (length(keywords()) == 0) {
            kit_abort("請至少輸入一個遺漏值標籤的關鍵字，例如 遺漏。")
          }
          purrr::map(dfs, replace_na_by_labels, keywords = keywords(), exclude = exclude)
        } else {
          if (length(codes()$codes) == 0) kit_abort("請至少輸入一個遺漏值代碼，例如 9999。")
          purrr::map(
            dfs, replace_na_codes,
            codes = codes()$codes,
            exclude = exclude,
            match_text = isTRUE(input$match_text)
          )
        }
        result(list(
          keys = files$keys(),
          names = files$names(),
          data = purrr::map(converted, "data"),
          detail = purrr::map(converted, "detail"),
          summary = na_summary_table(files$names(), dfs, converted),
          settings = current_settings()
        ))
        n_cells <- sum(purrr::map_int(converted, function(x) {
          if (is.null(x$detail)) 0L else as.integer(sum(x$detail$合計))
        }))
        kit_done_status(paste0(
          "轉換完成！", length(dfs), " 個檔案共替換 ", n_cells, " 格為 NA。",
          if (isTRUE(input$use_downstream)) "③～⑦ 分頁會使用處理後的資料。"
          else "③～⑦ 分頁仍使用原始資料（上方選項未勾選）。"
        ))
      }, error = function(e) kit_error_status(e, "轉換"))
      status(st)
      kit_notify(st)
    })

    output$status <- shiny::renderUI(make_status_ui(status()))

    output$result <- shiny::renderUI({
      res <- result()
      if (is.null(res)) return(NULL)
      choices <- stats::setNames(seq_along(res$names), res$names)
      shiny::tagList(
        shiny::hr(),
        shiny::h4("轉換結果"),
        shiny::h5("各檔案替換摘要"),
        shiny::tableOutput(ns("summary")),
        shiny::h5("下載處理後的檔案"),
        shiny::tags$p(
          class = "kit-hint",
          "一次下載一個檔案。只想改遺漏值的話，下載後就完成了，不用到後面的分頁。"
        ),
        shiny::selectInput(ns("view_file"), "要下載與檢視的檔案", choices = choices),
        kit_download_ui(ns),
        shiny::h5("各欄位替換格數"),
        shiny::tags$div(
          class = "kit-table-scroll",
          shiny::tableOutput(ns("detail"))
        ),
        shiny::h5("轉換後預覽"),
        shiny::textOutput(ns("preview_meta")),
        shiny::tags$div(
          class = "kit-table-scroll",
          shiny::tableOutput(ns("preview"))
        )
      )
    })

    output$summary <- shiny::renderTable(result()$summary, digits = 0)

    view <- shiny::reactive({
      res <- result()
      i <- suppressWarnings(as.integer(input$view_file))
      if (is.null(res) || length(i) == 0 || is.na(i) || i > length(res$data)) {
        return(NULL)
      }
      list(
        data = res$data[[i]],
        detail = res$detail[[i]],
        name = res$names[[i]]
      )
    })

    output$detail <- shiny::renderTable({
      v <- view()
      shiny::req(v)
      if (is.null(v$detail)) return(data.frame(說明 = "這個檔案沒有符合遺漏值代碼的值"))
      v$detail
    }, striped = TRUE, spacing = "s", digits = 0)

    output$preview_meta <- shiny::renderText({
      v <- view()
      shiny::req(v)
      format_dims(nrow(v$data), ncol(v$data), KIT_PREVIEW_COLS)
    })

    output$preview <- shiny::renderTable({
      v <- view()
      shiny::req(v)
      replaced <- if (is.null(v$detail)) character(0) else v$detail$欄位
      kit_preview_table(v$data, priority_cols = c(input$exclude_cols, replaced))
    }, striped = TRUE, spacing = "s")

    kit_download_server(
      session, output,
      data_fn = function() view()$data,
      filename_fn = function(ext) na_export_name(view()$name, ext),
      run_label = "執行轉換"
    )

    output$code <- shiny::renderUI({
      kit_code_blocks(code_missing(
        code_files_from(files),
        codes()$codes,
        input$exclude_cols,
        isTRUE(input$match_text),
        mode = mode(),
        keywords = keywords()
      ))
    })

    use_processed <- shiny::reactive({
      res <- result()
      isTRUE(input$use_downstream) && !is.null(res) &&
        identical(res$keys, files$keys())
    })

    list(
      data = shiny::reactive(
        if (use_processed()) result()$data else files$data()
      ),
      # "stale"：② 的結果因為設定或檔案改了而清除，還沒重新執行（後面的分頁用原始資料並提醒）
      source = shiny::reactive({
        if (use_processed()) {
          "na"
        } else if (stale() && isTRUE(input$use_downstream)) {
          "stale"
        } else {
          "raw"
        }
      }),
      code_cells = shiny::reactive(count_code_cells(files$data(), keywords = keywords()))
    )
  })
}

#' 各檔案替換摘要表
#' @noRd
na_summary_table <- function(file_names, dfs, converted) {
  tibble::tibble(
    檔案 = file_names,
    列數 = purrr::map_int(dfs, nrow),
    欄位數 = purrr::map_int(dfs, ncol),
    有替換的欄位數 = purrr::map_int(converted, function(x) {
      if (is.null(x$detail)) 0L else nrow(x$detail)
    }),
    替換為NA的格數 = purrr::map_int(converted, function(x) {
      if (is.null(x$detail)) 0L else as.integer(sum(x$detail$合計))
    })
  )
}
