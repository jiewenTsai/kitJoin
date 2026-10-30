# ②（選用）遺漏值轉 NA

#' @noRd
mod_missing_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    kit_tab_guide(
      goal = paste0(
        "把代表遺漏的代碼（例如 7777、9999）改成真正的遺漏值（NA），",
        "避免它們在分析時被當成數字。這一步是選用的。"
      ),
      steps = kit_steps(
        "確認遺漏值代碼（已預填 KIT 常用代碼）",
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
        "數值欄（含 SAV 標籤欄）中等於任一代碼的值改為 NA，SAV 標籤欄同時移除這些代碼的數值標籤；",
        "勾選「文字欄也比對」時，文字欄中完全等於代碼的值也會改為 NA。"
      )
    ),
    shiny::textInput(
      ns("codes"), "遺漏值代碼（以逗號分隔）",
      value = kit_na_codes_default(), width = "60%"
    ),
    shiny::uiOutput(ns("codes_parsed")),
    shiny::selectizeInput(
      ns("exclude_cols"), "不要處理的欄位（例如 ID，可多選）",
      choices = NULL, multiple = TRUE, options = list(create = FALSE),
      width = "60%"
    ),
    shiny::tags$small(
      class = "kit-hint",
      "已預先排除名稱像 ID 的欄位（例如 id、release_id），因為 ID 可能剛好等於 7777 或 9999。"
    ),
    shiny::checkboxInput(
      ns("match_text"), "文字欄也比對（例如 CSV 中被讀成文字的 \"9999\"）",
      value = TRUE, width = "100%"
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

    # 檔案變動後，舊的轉換結果失效
    shiny::observeEvent(files$data(), {
      if (!is.null(result())) {
        result(NULL)
        status(kit_stale_status("執行轉換"))
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$run, {
      result(NULL)
      dfs <- files$data()
      st <- tryCatch({
        if (length(dfs) == 0) kit_abort("請先到「① 上傳檔案」加入檔案。")
        if (length(codes()$codes) == 0) kit_abort("請至少輸入一個遺漏值代碼，例如 9999。")
        converted <- purrr::map(
          dfs, replace_na_codes,
          codes = codes()$codes,
          exclude = input$exclude_cols %||% character(0),
          match_text = isTRUE(input$match_text)
        )
        result(list(
          keys = files$keys(),
          names = files$names(),
          data = purrr::map(converted, "data"),
          detail = purrr::map(converted, "detail"),
          summary = na_summary_table(files$names(), dfs, converted)
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
        isTRUE(input$match_text)
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
      source = shiny::reactive(if (use_processed()) "na" else "raw"),
      code_cells = shiny::reactive(count_code_cells(files$data()))
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
