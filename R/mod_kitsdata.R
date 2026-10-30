# ⑥ 轉成 kits 作答檔

#' @noRd
mod_kitsdata_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    kit_tab_guide(
      goal = paste0(
        "把寬格式的資料（一列一個孩子的一個波次、一題一欄）轉成 kits 與 kitsUI 要的作答檔：",
        "一列一個孩子的一個波次的一題。"
      ),
      steps = kit_steps(
        "替每個檔案填世代（例如 M03），或選世代所在的欄",
        "確認孩子編號、波次、月齡等欄位的對應",
        "上傳題目表（item_id、domain），或輸入規則；沒有領域的欄逐一排除或指定領域",
        "（選用）勾選進階調整：遺漏值代碼、分數減 1、反向計分",
        "按「執行轉換」，看檢查結果，下載 .csv.gz"
      ),
      output = paste0(
        "一個作答檔（多個檔案合併成一個），固定 9 欄：",
        paste(KITS_COLUMNS$column, collapse = "、"), "。可以直接上傳到 kitsUI。"
      ),
      note = paste0(
        "沒有作答的題不會放進作答檔。原檔的波次名稱（t1、t2）這一頁不會用到；",
        "多個檔案時，每個檔案可以是不同世代、各有好幾個波次。"
      ),
      technical = paste0(
        "每個檔案先做勾選的進階調整（固定順序：1 遺漏值代碼轉 NA → 2 分數減 1 → 3 反向計分），",
        "再用 tidyr::pivot_longer() 把題目欄轉成 item_id、item_score，最後接上領域並合併。",
        "檢查都在寬資料上做；長格式在下載時每 5,000 列寬資料轉一段、寫進 .csv.gz，不會整張放進記憶體。",
        "visit_id 由 {cohort}_{subj_id}_{波次標籤} 組成；沒有波次標籤欄時改用 wave_m。"
      )
    ),
    shiny::uiOutput(ns("source_note")),
    kitsdata_spec_ui(),

    shiny::h4("1. 世代（每個檔案）"),
    shiny::uiOutput(ns("cohort_table")),

    shiny::h4("2. 對應欄位"),
    shiny::tags$small(
      class = "kit-hint",
      "已依欄名預先猜好，可以改。對應的欄會套用到每個檔案（各檔欄名要相同）。"
    ),
    shiny::tags$div(class = "kit-table-scroll", kitsdata_map_ui(ns)),
    shiny::uiOutput(ns("visit_note")),

    shiny::h4("3. 題目與領域"),
    shiny::fileInput(
      ns("item_file"), "題目表（item_id、domain 兩欄；CSV 或 SAV）",
      accept = c(".csv", ".sav"), buttonLabel = "選擇檔案…",
      placeholder = "沒有題目表", width = "60%"
    ),
    shiny::uiOutput(ns("item_file_note")),
    shiny::textAreaInput(
      ns("rules"), "或輸入規則（題目表找不到的題才用；一行一條「規則 = 領域」，規則是正規表示式）",
      placeholder = "^lan = lan\n^body = motor\n^cog = cog", rows = 3, width = "60%"
    ),
    shiny::uiOutput(ns("rules_note")),
    shiny::uiOutput(ns("domain_summary")),
    shiny::uiOutput(ns("undecided")),

    shiny::h4("4. 進階調整（選用）"),
    shiny::tags$small(
      class = "kit-hint",
      "勾了才做；不管勾的順序，都依 1 → 2 → 3 的順序執行。「用 R 怎麼做？」只會列出勾了的步驟。"
    ),
    kitsdata_steps_ui(ns),

    shiny::h4("5. 轉成長格式（pivot_longer）"),
    shiny::tags$small(
      class = "kit-hint",
      "kits 的作答檔格式固定，這些參數不能改；⑦ 長寬互轉可以自己設定。"
    ),
    mod_pivot_longer_ui(ns("pivot")),

    shiny::actionButton(ns("run"), "執行轉換", class = "btn-primary"),
    shiny::uiOutput(ns("status")),
    shiny::uiOutput(ns("result")),
    kit_code_panel(ns("code"), title = "用 R 怎麼做？（tidyverse 版）")
  )
}

# 對應欄位的表：kits 的欄 | 說明 | 用哪一欄
kitsdata_map_ui <- function(ns) {
  rows <- purrr::imap(KITS_MAP_FIELDS, function(meaning, field) {
    none <- if (field == "visit_id") "（自動組成）" else "（不用）"
    shiny::tags$tr(
      shiny::tags$td(shiny::tags$code(field)),
      shiny::tags$td(meaning, if (field == "wave_m") shiny::tags$strong("（必填）")),
      shiny::tags$td(shiny::selectInput(
        ns(paste0("map_", field)), NULL, choices = stats::setNames("", none), width = "14rem"
      ))
    )
  })
  shiny::tags$table(
    class = "table table-sm align-middle kit-file-table",
    shiny::tags$thead(shiny::tags$tr(
      shiny::tags$th("kits 的欄"), shiny::tags$th("意思"), shiny::tags$th("用檔案裡的哪一欄")
    )),
    shiny::tags$tbody(rows)
  )
}

# 進階調整：三個勾選框，勾了才出現各自的設定
kitsdata_steps_ui <- function(ns) {
  cond <- function(id) paste0("input['", ns(id), "']")
  shiny::tagList(
    shiny::checkboxInput(ns("step_na"), "步驟 1：遺漏值代碼轉 NA", FALSE, width = "100%"),
    shiny::conditionalPanel(
      cond("step_na"),
      shiny::textInput(
        ns("na_codes"), "遺漏值代碼（以逗號分隔；題目、wave_m、age_m 都會處理）",
        value = kit_na_codes_default(), width = "60%"
      ),
      shiny::uiOutput(ns("na_note"))
    ),
    shiny::checkboxInput(
      ns("step_shift"), "步驟 2：分數減 1（例如 1–5 計分改成 0–4；kits 的分數要從 0 開始）",
      FALSE, width = "100%"
    ),
    shiny::conditionalPanel(
      cond("step_shift"),
      shiny::radioButtons(
        ns("shift_scope"), NULL, inline = TRUE,
        choices = c("全部題目" = "all", "只有選的題目" = "pick")
      ),
      shiny::conditionalPanel(
        paste0(cond("shift_scope"), " == 'pick'"),
        shiny::selectizeInput(ns("shift_items"), "要減 1 的題目", NULL, multiple = TRUE,
                              width = "60%")
      )
    ),
    shiny::uiOutput(ns("shift_suggest")),
    shiny::checkboxInput(
      ns("step_reverse"), "步驟 3：反向計分（最高分 − 分數，在 0 起算的分數上做）",
      FALSE, width = "100%"
    ),
    shiny::conditionalPanel(
      cond("step_reverse"),
      shiny::selectizeInput(ns("reverse_items"), "要反向的題目", NULL, multiple = TRUE,
                            width = "60%"),
      shiny::numericInput(
        ns("reverse_max"), "最高分（0 起算；例如 0–4 計分填 4）", value = NA, min = 1,
        step = 1, width = "20rem"
      ),
      shiny::checkboxInput(ns("reverse_add_r"), "題號後面加 r（例如 cogd01 → cogd01r）", FALSE),
      shiny::tags$small(
        class = "kit-hint",
        "KIT 的資料已經反向過、也已經用 r 命名（例如 cogd01r），通常不用勾這一步。"
      )
    )
  )
}

#' @param files mod_upload_server() 的回傳值
#' @param working mod_missing_server() 的回傳值
#' @noRd
mod_kitsdata_server <- function(id, files, working) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    result <- shiny::reactiveVal(NULL)
    status <- shiny::reactiveVal(kit_status())

    output$source_note <- shiny::renderUI({
      n <- length(files$keys())
      if (n == 0) {
        return(shiny::tags$div(
          class = "kit-callout kit-callout-warning",
          "請先到「① 上傳檔案」加入寬格式的資料檔（一個或多個）。"
        ))
      }
      shiny::tags$p(
        class = "kit-source-note",
        "目前使用：",
        if (identical(working$source(), "na")) "「② 遺漏值轉 NA」處理後的資料" else "原始資料",
        "，共 ", n, " 個檔案。"
      )
    })

    all_cols <- shiny::reactive(unique(unlist(purrr::map(working$data(), names))))

    # 1. 世代 ----
    output$cohort_table <- shiny::renderUI({
      keys <- files$keys()
      if (length(keys) == 0) return(NULL)
      dfs <- working$data()
      rows <- purrr::pmap(list(keys, files$names(), dfs), function(key, name, df) {
        src_id <- paste0("cohort_src_", key)
        txt_id <- paste0("cohort_txt_", key)
        src <- shiny::isolate(input[[src_id]]) %||% "__text__"
        if (!src %in% c("__text__", names(df))) src <- "__text__"
        shiny::tags$tr(
          shiny::tags$td(shiny::tags$strong(name), shiny::tags$br(), shiny::tags$small(
            class = "text-muted", format_dims(nrow(df), ncol(df))
          )),
          shiny::tags$td(shiny::selectInput(
            ns(src_id), NULL, width = "12rem", selected = src,
            choices = list(
              "自己填" = "__text__",
              "檔案裡的欄" = as.list(stats::setNames(names(df), names(df)))
            )
          )),
          shiny::tags$td(shiny::conditionalPanel(
            paste0("input['", ns(src_id), "'] == '__text__'"),
            shiny::textInput(ns(txt_id), NULL, value = shiny::isolate(input[[txt_id]]) %||% "",
                             placeholder = "例如 M03", width = "10rem")
          ))
        )
      })
      shiny::tagList(
        shiny::tags$div(
          class = "kit-table-scroll",
          shiny::tags$table(
            class = "table table-sm align-middle kit-file-table",
            shiny::tags$thead(shiny::tags$tr(
              shiny::tags$th("檔案"), shiny::tags$th("世代從哪裡來"), shiny::tags$th("世代")
            )),
            shiny::tags$tbody(rows)
          )
        ),
        shiny::tags$small(
          class = "kit-hint",
          "世代一定要給（例如 M03、M36）；空白時不能轉換。這一頁不會從檔名猜世代。"
        )
      )
    })

    cohorts <- shiny::reactive({
      purrr::map(files$keys(), function(key) {
        src <- input[[paste0("cohort_src_", key)]] %||% "__text__"
        if (identical(src, "__text__")) {
          list(type = "text", value = trimws(input[[paste0("cohort_txt_", key)]] %||% ""))
        } else {
          list(type = "column", value = src)
        }
      })
    })

    # 2. 對應欄位：欄位變動時更新選項；保留還在的選擇，否則用猜的
    shiny::observeEvent(all_cols(), {
      cols <- all_cols()
      guess <- kits_guess_map(cols)
      for (field in names(KITS_MAP_FIELDS)) {
        id <- paste0("map_", field)
        prev <- input[[id]]
        selected <- if (!is.null(prev) && prev %in% cols) prev else guess[[field]]
        none <- if (field == "visit_id") "（自動組成）" else "（不用）"
        shiny::updateSelectInput(
          session, id, choices = c(stats::setNames("", none), cols), selected = selected
        )
      }
    })

    map <- shiny::reactive({
      purrr::map(stats::setNames(nm = names(KITS_MAP_FIELDS)), function(field) {
        input[[paste0("map_", field)]] %||% ""
      })
    })

    output$visit_note <- shiny::renderUI({
      dfs <- working$data()
      if (length(dfs) == 0) return(NULL)
      rule <- kits_visit_rule(map())
      example <- kitsdata_visit_example(dfs[[1]], cohorts()[[1]], map())
      cls <- if (is.null(rule$parts) || rule$parts[3] != "wave_m") "kit-source-note" else
        "kit-callout kit-callout-warning"
      shiny::tags$div(
        class = cls, rule$note,
        if (!is.null(example)) shiny::tags$span("例子：", shiny::tags$code(example))
      )
    })

    # 3. 題目與領域 ----
    item_table <- shiny::reactive({
      f <- input$item_file
      if (is.null(f)) return(NULL)
      tryCatch(
        kits_item_table(read_data_file(f$datapath, f$name)$data),
        error = function(e) e
      )
    })
    output$item_file_note <- shiny::renderUI({
      tab <- item_table()
      if (is.null(tab)) return(NULL)
      if (inherits(tab, "error")) {
        return(shiny::tags$div(class = "text-danger small", conditionMessage(tab)))
      }
      doms <- dplyr::count(tab, .data$domain)
      shiny::tags$small(
        class = "kit-hint",
        "題目表共 ", nrow(tab), " 題：", paste0(doms$domain, " ", doms$n, " 題", collapse = "、"), "。"
      )
    })
    rules <- shiny::reactive(kits_parse_rules(input$rules))
    output$rules_note <- shiny::renderUI({
      bad <- rules()$invalid
      if (length(bad) == 0) return(NULL)
      shiny::tags$div(
        class = "text-danger small kit-codes-parsed",
        "看不懂這幾行（格式是「規則 = 領域」），會略過：", paste(bad, collapse = "；")
      )
    })

    cohort_cols <- shiny::reactive({
      purrr::map_chr(purrr::keep(cohorts(), function(x) x$type == "column"), "value")
    })
    candidates <- shiny::reactive({
      kits_candidate_cols(working$data(), c(kits_mapped_cols(map()), cohort_cols()))
    })
    domains <- shiny::reactive({
      tab <- item_table()
      if (inherits(tab, "error")) tab <- NULL
      kits_item_domains(candidates()$numeric, tab, rules()$rules)
    })
    no_domain <- shiny::reactive(domains()$column[is.na(domains()$domain)])
    choice_id <- function(col) paste0("nodom_", paste(as.character(charToRaw(col)), collapse = ""))

    output$domain_summary <- shiny::renderUI({
      d <- domains()
      if (length(working$data()) == 0) return(NULL)
      found <- dplyr::filter(d, !is.na(.data$domain))
      by_src <- dplyr::count(found, src = ifelse(startsWith(.data$source, "規則"), "規則", "題目表"))
      text_cols <- candidates()$text
      shiny::tags$div(
        class = "kit-source-note",
        shiny::tags$p(
          found |> nrow(), " 欄有領域",
          if (nrow(found) > 0) paste0("（", paste0(by_src$src, " ", by_src$n, collapse = "、"), "）"),
          "；", length(no_domain()), " 欄沒有領域。",
          if (nrow(found) > 0) {
            dc <- dplyr::count(found, .data$domain)
            paste0("各領域：", paste0(dc$domain, " ", dc$n, " 題", collapse = "、"), "。")
          }
        ),
        if (length(text_cols) > 0) {
          shiny::tags$p(
            class = "small",
            "文字欄不能當題目，已排除：", paste(utils::head(text_cols, 10), collapse = "、"),
            if (length(text_cols) > 10) "…" else "", "。"
          )
        }
      )
    })

    output$undecided <- shiny::renderUI({
      cols <- no_domain()
      if (length(cols) == 0) return(NULL)
      known <- sort(unique(stats::na.omit(domains()$domain)))
      rows <- purrr::map(cols, function(col) {
        id <- choice_id(col)
        shiny::tags$tr(
          shiny::tags$td(shiny::tags$code(col)),
          shiny::tags$td(shiny::selectizeInput(
            ns(id), NULL, width = "14rem",
            choices = c("（還沒決定）" = "", "排除（不是題目）" = "__exclude__", known),
            selected = shiny::isolate(input[[id]]) %||% "",
            options = list(create = TRUE, placeholder = "排除，或選／輸入領域")
          ))
        )
      })
      shiny::tagList(
        shiny::h5("沒有領域的欄"),
        shiny::tags$small(
          class = "kit-hint",
          "每一欄都要決定：不是題目就排除；是題目就選或輸入它的領域。"
        ),
        shiny::actionButton(ns("exclude_rest"), "其餘還沒決定的全部排除",
                            class = "btn-sm btn-outline-secondary mb-2"),
        shiny::tags$div(
          class = "kit-table-scroll kit-undecided",
          shiny::tags$table(
            class = "table table-sm align-middle kit-file-table",
            shiny::tags$thead(shiny::tags$tr(shiny::tags$th("欄"), shiny::tags$th("怎麼處理"))),
            shiny::tags$tbody(rows)
          )
        )
      )
    })
    shiny::observeEvent(input$exclude_rest, {
      for (col in no_domain()) {
        id <- choice_id(col)
        if (!nzchar(input[[id]] %||% "")) {
          shiny::updateSelectizeInput(session, id, selected = "__exclude__")
        }
      }
    })

    decided <- shiny::reactive({
      cols <- no_domain()
      vals <- purrr::map_chr(cols, function(col) input[[choice_id(col)]] %||% "")
      list(
        undecided = cols[!nzchar(vals)],
        assigned = tibble::tibble(column = cols, domain = vals) |>
          dplyr::filter(nzchar(.data$domain), .data$domain != "__exclude__")
      )
    })
    items <- shiny::reactive({
      found <- dplyr::filter(domains(), !is.na(.data$domain)) |>
        dplyr::select("column", "domain")
      dplyr::bind_rows(found, decided()$assigned)
    })

    # 4. 進階調整 ----
    shiny::observeEvent(items()$column, {
      cols <- items()$column
      for (id in c("shift_items", "reverse_items")) {
        keep <- intersect(input[[id]] %||% character(0), cols)
        shiny::updateSelectizeInput(session, id, choices = cols, selected = keep, server = TRUE)
      }
    })
    na_codes <- shiny::reactive(parse_na_codes(input$na_codes)$codes)
    output$na_note <- shiny::renderUI({
      if (!identical(working$source(), "na")) return(NULL)
      shiny::tags$small(
        class = "kit-hint",
        "「② 遺漏值轉 NA」已經處理過這些資料；這一步可以不勾（勾了也不會重複替換）。"
      )
    })
    steps <- shiny::reactive({
      cols <- items()$column
      list(
        na = if (isTRUE(input$step_na)) na_codes(),
        shift = if (isTRUE(input$step_shift)) {
          if (identical(input$shift_scope, "pick")) {
            intersect(input$shift_items %||% character(0), cols)
          } else {
            cols
          }
        },
        reverse = if (isTRUE(input$step_reverse)) {
          list(
            items = intersect(input$reverse_items %||% character(0), cols),
            max = input$reverse_max, add_r = isTRUE(input$reverse_add_r)
          )
        }
      )
    })
    output$shift_suggest <- shiny::renderUI({
      if (isTRUE(input$step_shift) || length(items()$column) == 0) return(NULL)
      dfs <- working$data()
      if (isTRUE(input$step_na)) {
        dfs <- purrr::map(dfs, function(df) kits_step_na(df, items()$column, na_codes())$data)
      }
      r <- kits_item_ranges(dfs, items()$column)
      r <- r[r$n > 0, ]
      if (nrow(r) == 0 || any(r$min != 1)) return(NULL)
      shiny::tags$div(
        class = "kit-callout",
        shiny::tags$strong("建議："), "每一題的最低分都是 1，分數可能是從 1 開始計分。",
        "kits 的分數要從 0 開始；如果是這樣，請勾「步驟 2：分數減 1」（不會自動幫您勾）。"
      )
    })

    # 5. pivot_longer：參數全部固定 ----
    pivot_args <- mod_pivot_longer_server(
      "pivot", data = shiny::reactive(NULL),
      fixed = list(
        cols = shiny::reactive(items()$column), names_to = "item_id",
        values_to = "item_score", names_prefix = NULL, names_pattern = NULL,
        values_drop_na = TRUE
      )
    )

    spec <- shiny::reactive({
      kits_spec(cohorts(), map(), items(), decided()$undecided, steps())
    })

    # 設定或資料變動後，舊結果失效
    shiny::observeEvent(list(spec(), working$data()), {
      if (!is.null(result())) {
        result(NULL)
        status(kit_stale_status("執行轉換"))
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$run, {
      result(NULL)
      st <- tryCatch(
        shiny::withProgress(message = "正在轉換…", value = 0.3, {
          out <- kits_build(working$data(), files$names(), spec(), pivot_args())
          if (length(out$problems) > 0) {
            kit_status(
              "error",
              paste0("有 ", length(out$problems), " 個問題要先處理，處理後再按一次「執行轉換」："),
              warnings = out$problems, notes = out$notes
            )
          } else {
            # 長格式不在這裡做出來：下載時才一段一段寫進 .csv.gz（省記憶體）
            result(out)
            kit_done_status(
              paste0(
                "檢查完成！作答檔會有 ", format(out$n_rows, big.mark = ","), " 列、",
                format(out$n_visits, big.mark = ","), " 個列次、", out$n_items, " 題。",
                "請按下方按鈕下載。"
              ),
              warnings = out$warnings, notes = out$notes
            )
          }
        }),
        error = function(e) kit_error_status(e, "轉換")
      )
      status(st)
      kit_notify(st)
    })

    output$status <- shiny::renderUI(make_status_ui(status()))

    output$result <- shiny::renderUI({
      res <- result()
      if (is.null(res)) return(NULL)
      shiny::tagList(
        shiny::hr(),
        shiny::h4("結果"),
        shiny::tags$div(
          class = "kit-downloads",
          shiny::downloadButton(ns("download_gz"), "下載作答檔（.csv.gz）", class = "btn-primary me-2"),
          shiny::downloadButton(ns("download_csv"), "下載未壓縮的 CSV", class = "btn-outline-primary"),
          shiny::tags$small(
            class = "kit-hint",
            paste0("未壓縮約 ", kits_size_text(res$csv_mb * 1024^2), "；壓縮檔通常小很多。"),
            "按下載後才一段一段寫出檔案，資料大時要等一下。",
            "kitsUI 可以直接讀 .csv.gz（單檔上限 ", KITSUI_UPLOAD_MB, " MB）。"
          )
        ),
        shiny::h5("各世代 × 領域"),
        shiny::tableOutput(ns("summary")),
        shiny::h5("作答檔預覽"),
        shiny::tags$small(class = "kit-hint", paste0("前 ", KIT_PREVIEW_ROWS * 2, " 列。")),
        shiny::tags$div(class = "kit-table-scroll", shiny::tableOutput(ns("preview")))
      )
    })
    output$summary <- shiny::renderTable(result()$summary, digits = 0)
    output$preview <- shiny::renderTable(
      kit_preview_table(result()$preview, n = KIT_PREVIEW_ROWS * 2),
      striped = TRUE, spacing = "s"
    )
    output$download_gz <- shiny::downloadHandler(
      filename = function() timestamped_name("kits_long")("csv.gz"),
      content = function(file) kitsdata_download(result()$prep, file, write_kits_long_gz)
    )
    output$download_csv <- shiny::downloadHandler(
      filename = function() timestamped_name("kits_long")("csv"),
      content = function(file) kitsdata_download(result()$prep, file, write_kits_long_csv)
    )

    output$code <- shiny::renderUI({
      kit_code_blocks(list(tidy = code_kitsdata(
        code_files_from(files), spec(), item_file = input$item_file$name,
        rules = rules()$rules, assigned = decided()$assigned
      ), base = NULL))
    })
  })
}

#' 第一個檔案第一列的 visit_id 例子（看得出怎麼組）
#' @noRd
kitsdata_visit_example <- function(df, cohort, map) {
  if (nrow(df) == 0 || is.null(cohort) || !nzchar(cohort$value %||% "")) return(NULL)
  if (identical(cohort$type, "column") && !cohort$value %in% names(df)) return(NULL)
  needed <- kits_mapped_cols(map)
  if (!all(needed %in% names(df))) return(NULL)
  one <- df[1, , drop = FALSE]
  co <- if (identical(cohort$type, "column")) as.character(unclass(one[[cohort$value]])) else
    cohort$value
  persons <- tibble::tibble(
    cohort = co,
    subj_id = if (nzchar(map$subj_id %||% "")) as.character(unclass(one[[map$subj_id]])) else NA,
    wave_m = if (nzchar(map$wave_m %||% "")) as.numeric(unclass(one[[map$wave_m]])) else NA_real_
  )
  out <- kits_visit_ids(persons, one, map)
  if (is.na(out)) NULL else out
}

#' kits 作答檔的欄位表（欄位、必填、意思），放在頁面上方
#' @noRd
kitsdata_spec_ui <- function() {
  rows <- purrr::pmap(KITS_COLUMNS, function(column, type, required, from, meaning) {
    shiny::tags$tr(
      shiny::tags$td(shiny::tags$code(column)),
      shiny::tags$td(if (required) "是" else "否"),
      shiny::tags$td(c(c = "文字", d = "數字", i = "整數")[[type]]),
      shiny::tags$td(meaning)
    )
  })
  shiny::tags$details(
    class = "kit-example", open = NA,
    shiny::tags$summary("kits 作答檔的 9 個欄位（名稱、順序、型別固定）"),
    shiny::tags$table(
      class = "table table-sm kit-example-table",
      shiny::tags$thead(shiny::tags$tr(
        shiny::tags$th("欄位"), shiny::tags$th("必填"), shiny::tags$th("型別"),
        shiny::tags$th("意思")
      )),
      shiny::tags$tbody(rows)
    ),
    shiny::tags$small(
      class = "kit-hint",
      "不是必填、也沒有對應的欄（subj_id、sex、age_m）仍會輸出，內容是空白。"
    )
  )
}

#' 檔案大小的文字：小於 1 MB 時用 KB
#' @noRd
kits_size_text <- function(bytes) {
  if (is.na(bytes)) return("—")
  if (bytes < 1024^2) sprintf("%.0f KB", bytes / 1024) else sprintf("%.1f MB", bytes / 1024^2)
}

# 下載：一段一段寫出長格式，顯示進度
kitsdata_download <- function(prep, file, writer) {
  shiny::withProgress(message = "正在寫出作答檔…", value = 0, {
    writer(prep = prep, file = file, progress = function(p) shiny::setProgress(p))
  })
}
