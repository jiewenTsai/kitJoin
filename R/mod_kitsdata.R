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
        "確認孩子編號、波次、時間點等欄位的對應（各檔欄名不同時，每個檔案各選一欄）",
        "在「3. 題目設定」給每一題領域（可以上傳題目表），不是題目的欄排除；需要時調整計分對照",
        "按「執行轉換」，看檢查結果，下載 .csv.gz"
      ),
      output = shiny::tagList(
        paste0(
          "一個作答檔（多個檔案合併成一個），固定 9 欄：",
          paste(KITS_COLUMNS$column, collapse = "、"), "。可以直接上傳到 kitsUI。"
        ),
        " ",
        htmltools::tagAppendAttributes(kit_kitsui_button("開啟 kitsUI 上傳"), class = "btn-sm")
      ),
      note = paste0(
        "沒有作答的題不會放進作答檔。原檔的波次名稱（t1、t2）這一頁不會用到；",
        "多個檔案時，每個檔案可以是不同世代、各有好幾個波次。"
      ),
      technical = paste0(
        "每個檔案先把每一題的原分數換成計分（計分對照），",
        "再用 tidyr::pivot_longer() 把題目欄轉成 item_id、item_score，最後接上領域並合併。",
        "檢查都在寬資料上做；長格式在下載時每 5,000 列寬資料轉一段、寫進 .csv.gz，不會整張放進記憶體。",
        "visit_id 由 {cohort}_{subj_id}_{波次標籤} 組成；沒有波次標籤欄時改用 wave_m。",
        "這一頁不把任何值改成 NA：疑似遺漏值代碼（值標籤是「遺漏值」「跳答」這類、② 的代碼、",
        "7777 以上、和分數明顯分開的值）只提醒，不在計分對照裡，照原樣寫進作答檔；",
        "要當成遺漏值，請先用「② 遺漏值轉 NA」。"
      )
    ),
    shiny::tags$div(
      class = "kit-callout",
      shiny::tags$strong("多個檔案會合併成一個作答檔。"),
      "只要一個波次或一個領域時，請在 ① 只上傳那一個檔案。"
    ),
    shiny::uiOutput(ns("source_note")),
    shiny::uiOutput(ns("codes_note")),
    kitsdata_spec_ui(),

    shiny::h4("1. 世代（每個檔案）"),
    shiny::uiOutput(ns("cohort_table")),

    shiny::h4("2. 對應欄位"),
    shiny::tags$small(
      class = "kit-hint",
      "已依欄名預先猜好，可以改。選「某一欄」時，同一個欄名套用到每個檔案；",
      "各檔欄名不同時（例如 w1_id、w2_id、w3id），選「各檔各選一欄」，",
      "再到上面「1.」的表裡替每個檔案選一欄。"
    ),
    shiny::tags$div(class = "kit-table-scroll", kitsdata_map_ui(ns)),
    shiny::selectInput(
      ns("wave_unit"), "wave_m（時間點）的單位", choices = KITS_WAVE_UNITS, selected = "month",
      width = "16rem"
    ),
    shiny::uiOutput(ns("wave_unit_note")),
    shiny::uiOutput(ns("visit_note")),

    shiny::h4("3. 題目設定"),
    shiny::tags$small(
      class = "kit-hint",
      paste0(
        "一列一個題目欄（「2.」對應到的欄不算）。先勾選要改的列，再按上面的按鈕一次改好。",
        "「計分對照」上一列是原分數、下一列是計分；粗體是改過的。"
      )
    ),
    shiny::fileInput(
      ns("item_file"),
      "上傳題目表（item_id、domain，可以再加 item_id_out）或題目設定檔（之前下載的；CSV 或 SAV）",
      accept = c(".csv", ".sav"), buttonLabel = "選擇檔案…", placeholder = "沒有上傳",
      width = "60%"
    ),
    shiny::uiOutput(ns("item_file_note")),
    shiny::selectInput(
      ns("reverse_rule"), "反向題的欄名（題目表查不到時，改用原題的領域）",
      choices = KITS_REVERSE_RULES, selected = "suffix", width = "24rem"
    ),
    shiny::tags$div(
      class = "mb-2",
      shiny::uiOutput(ns("settings_dl_ui"), inline = TRUE),
      shiny::tags$small(
        class = "text-muted ms-2",
        "欄位：item_id、include、domain、map_raw_to_score（計分對照，寫成「原分數=計分」，",
        "例如 1=0;2=1）、item_id_out（輸出的題號）。",
        "下一波上傳這個檔，就會套用同樣的設定。"
      )
    ),
    shiny::uiOutput(ns("settings_summary")),
    shiny::uiOutput(ns("shift_suggest")),
    kitsdata_bulk_ui(ns),
    shiny::uiOutput(ns("settings_table")),
    kitsdata_filter_js(ns),

    shiny::h4("4. 轉成長格式（pivot_longer）"),
    shiny::tags$small(
      class = "kit-hint",
      "kits 的作答檔格式固定，這些參數不能改；⑦ 長寬互轉可以自己設定。"
    ),
    mod_pivot_longer_ui(ns("pivot")),

    shiny::actionButton(ns("run"), "執行轉換", class = "btn-primary"),
    shiny::uiOutput(ns("status")),
    shiny::uiOutput(ns("result")),
    kit_code_panel(ns("code"))
  )
}

# 對應欄位的表：kits 的欄 | 說明 | 用哪一欄
kitsdata_map_ui <- function(ns) {
  rows <- purrr::pmap(KITS_MAP_ROWS, function(field, type, required, meaning, typed, per_file,
                                               label) {
    none <- if (field == "visit_id") "（自動組成）" else "（還沒選）"
    col_select <- shiny::selectInput(
      ns(paste0("map_", field)), NULL, choices = stats::setNames("", none), width = "14rem"
    )
    has_src <- typed || per_file
    src <- if (has_src) {
      shiny::selectInput(
        ns(paste0("src_", field)), NULL, width = "10rem",
        choices = c("某一欄" = "column", if (per_file) c("各檔各選一欄" = "per_file"),
                    if (typed) c("自己填" = "typed"), if (typed && !required) c("不用" = "none"))
      )
    }
    cond <- function(value) paste0("input['", ns(paste0("src_", field)), "'] == '", value, "'")
    hint <- function(value, text) {
      shiny::conditionalPanel(cond(value), shiny::tags$small(class = "text-muted", text))
    }
    shiny::tags$tr(
      shiny::tags$td(shiny::tags$code(field)),
      shiny::tags$td(meaning, if (required) shiny::tags$strong("（必填）")),
      shiny::tags$td(src),
      shiny::tags$td(
        if (has_src) shiny::conditionalPanel(cond("column"), col_select) else col_select,
        if (typed) hint("typed", "在上面「1.」的表裡，每個檔案各填一個值。"),
        if (per_file) hint("per_file", "在上面「1.」的表裡，每個檔案各選一欄。")
      )
    )
  })
  shiny::tags$table(
    class = "table table-sm align-middle kit-file-table",
    shiny::tags$thead(shiny::tags$tr(
      shiny::tags$th("kits 的欄"), shiny::tags$th("意思"), shiny::tags$th("從哪裡來"),
      shiny::tags$th("用檔案裡的哪一欄")
    )),
    shiny::tags$tbody(rows)
  )
}

# 「3. 題目設定」上方的按鈕列：選取、對選取的列做批次動作
kitsdata_bulk_ui <- function(ns) {
  btn <- function(id, label, cls = "btn-outline-secondary") {
    shiny::actionButton(ns(id), label, class = paste("btn-sm", cls))
  }
  row <- function(...) shiny::tags$div(class = "kit-bulk-row", ...)
  shiny::tags$div(
    class = "kit-bulk",
    shiny::tags$style(shiny::HTML(paste(
      ".kit-bulk { background: #f8f9fa; border: 1px solid #dee2e6; border-radius: .375rem;",
      "  padding: .5rem .75rem; margin-bottom: .5rem; }",
      ".kit-bulk-row { display: flex; flex-wrap: wrap; gap: .4rem; align-items: center;",
      "  margin: .25rem 0; }",
      ".kit-bulk-row .form-group { margin-bottom: 0; }",
      ".kit-bulk-row .shiny-input-container:not(.shiny-input-container-inline) {",
      "  width: auto; }",
      ".kit-map { border-collapse: collapse; font-size: .8rem; }",
      ".kit-map th, .kit-map td { border: 1px solid #dee2e6; padding: 0 .35rem;",
      "  text-align: center; }",
      ".kit-map th { background: #f1f3f5; font-weight: normal; white-space: nowrap; }",
      ".kit-map td.changed { font-weight: bold; background: #fff3cd; }",
      ".kit-settings-scroll { max-height: 32rem; overflow: auto; }",
      ".kit-settings tr.excluded td { color: #adb5bd; }",
      ".kit-settings th { white-space: nowrap; }"
    ))),
    row(
      shiny::tags$strong("選取："), btn("sel_all", "全選"), btn("sel_none", "全不選"),
      shiny::textInput(ns("sel_filter"), NULL, placeholder = "依欄名篩選，例如 lan",
                       width = "12rem"),
      btn("sel_filtered", "選取篩選出來的列"),
      shiny::tags$span(
        class = "text-muted small", shiny::textOutput(ns("sel_count"), inline = TRUE)
      )
    ),
    row(
      shiny::tags$strong("對選取的列："),
      shiny::selectizeInput(ns("bulk_domain"), NULL, choices = NULL, width = "11rem",
                            options = list(create = TRUE, placeholder = "選或輸入領域")),
      btn("do_domain", "設領域"),
      shiny::tags$span("｜平移"),
      shiny::numericInput(ns("bulk_shift"), NULL, value = -1, step = 1, width = "5.5rem"),
      btn("do_shift", "平移"),
      shiny::tags$span("｜反向：最高分"),
      shiny::textInput(ns("bulk_max"), NULL, placeholder = "空白＝各題目前最高計分",
                       width = "12rem"),
      btn("do_reverse", "反向")
    ),
    row(
      shiny::tags$span("合併：原分數"),
      shiny::textInput(ns("merge_from"), NULL, placeholder = "例如 4、5", width = "7rem"),
      shiny::tags$span("→ 計分"),
      shiny::numericInput(ns("merge_to"), NULL, value = NA, step = 1, width = "5.5rem"),
      btn("do_merge", "合併"),
      shiny::tags$span("｜"),
      btn("do_restore", "還原（計分＝原分數）"),
      btn("do_exclude", "排除", "btn-outline-danger"),
      btn("do_include", "納入", "btn-outline-success")
    ),
    row(
      shiny::tags$span("輸出題號：欄名去掉"),
      shiny::textInput(ns("out_pattern"), NULL, value = "^w[0-9]+", width = "8rem"),
      btn("do_out_prefix", "去掉"),
      btn("do_out_reset", "還原成欄名")
    ),
    shiny::tags$small(
      class = "text-muted",
      "平移、反向都在目前的計分上做（例如先合併再平移）；合併是把指定的原分數都改成同一個計分。",
      "遺漏值代碼（例如 98、99）不要用合併接起來，請先到「② 遺漏值轉 NA」改成 NA。",
      "輸出題號是作答檔的 item_id：各波次欄名不同、其實是同一題時，給它們同一個題號",
      "（例如 w1d5_1、w2d5_1 去掉 ^w[0-9]+ 都是 d5_1；題號換了的，用題目表的 item_id_out 欄）。"
    )
  )
}

# 依欄名篩選在瀏覽器做（不重畫表格）：打字時藏起不符合的列，表格重畫後再套一次。
# 規則和 kitsdata_filter_cols() 一樣：去掉前後空白、不分大小寫、包含就顯示。
kitsdata_filter_js <- function(ns) {
  shiny::tags$script(shiny::HTML(sprintf(
    "(function() {
  var box = '%s', out = '%s';
  function apply() {
    var el = document.getElementById(box), tab = document.getElementById(out);
    if (!el || !tab) return;
    var f = el.value.replace(/^[ \\t\\r\\n]+|[ \\t\\r\\n]+$/g, '').toLowerCase();
    tab.querySelectorAll('tr[data-col]').forEach(function(tr) {
      var hit = !f || tr.getAttribute('data-col').toLowerCase().indexOf(f) >= 0;
      tr.style.display = hit ? '' : 'none';
    });
  }
  $(document).on('input change', '#' + box, apply);
  $(document).on('shiny:value', function(e) {
    if (e.name === out) setTimeout(apply, 0);
  });
})();",
    ns("sel_filter"), ns("settings_table")
  )))
}

#' @param files mod_upload_server() 的回傳值
#' @param working mod_missing_server() 的回傳值
#' @param goto function(tab) 切換分頁（到 ② 處理遺漏值代碼）
#' @param na_codes NULL 或 reactive：② 的代碼清單（數字向量），用來找疑似遺漏值代碼（只提醒）
#' @noRd
mod_kitsdata_server <- function(id, files, working, goto, na_codes = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    result <- shiny::reactiveVal(NULL)
    status <- shiny::reactiveVal(kit_status())
    codes_2 <- shiny::reactive(if (is.null(na_codes)) NULL else na_codes())

    output$source_note <- shiny::renderUI({
      n <- length(files$keys())
      if (n == 0) {
        return(shiny::tags$div(
          class = "kit-callout kit-callout-warn",
          "請先到「① 上傳檔案」加入寬格式的資料檔（一個或多個）。"
        ))
      }
      src <- working$source() %||% "raw"
      shiny::tagList(
        shiny::tags$p(
          class = "kit-source-note",
          "目前使用：",
          if (identical(src, "na")) "「② 遺漏值轉 NA」處理後的資料" else "原始資料",
          "，共 ", n, " 個檔案。"
        ),
        # ② 的設定或檔案改過、還沒重新執行（fix-io 分支的 source = "stale"）
        if (identical(src, "stale")) {
          shiny::tags$div(
            class = "kit-callout kit-callout-warn",
            "② 的設定改過了，請",
            shiny::actionLink(ns("goto_missing_stale"), "重新執行「② 遺漏值轉 NA」"),
            "，這一頁才會用到新的結果。"
          )
        }
      )
    })

    # 疑似遺漏值代碼還在資料裡：⑥ 不改、照原樣輸出；提醒先到 ② 改成 NA 再回來（不擋轉換）
    shiny::observeEvent(input$goto_missing, goto("missing"))
    shiny::observeEvent(input$goto_missing_stale, goto("missing"))
    shiny::observeEvent(input$goto_upload_status, goto("upload"))
    output$codes_note <- shiny::renderUI({
      if (length(working$data()) == 0) return(NULL)
      s <- settings()
      n_items <- sum(s$n_codes[s$include] %||% 0)
      n_side <- kits_code_cells(working$data(), unlist(kits_side_cols(map(), per_file())),
                                codes_2())
      n <- n_items + n_side
      if (n == 0) return(NULL)
      shiny::tags$div(
        class = "kit-callout kit-callout-warn",
        paste0(
          "題目、性別、wave_m 或 age_m 有 ", format(n, big.mark = ","), " 格疑似遺漏值代碼",
          "（值標籤是「遺漏值」「跳答」「不適用」這類、② 的代碼、7777 以上，",
          "或和分數明顯分開的值，例如 1–5 分以外的 95–99）。",
          "這一頁不會改它們，會照原樣當成分數寫進作答檔；每一題的代碼列在「3. 題目設定」的提醒欄。",
          "如果它們是遺漏值，請先"
        ),
        shiny::actionLink(ns("goto_missing"), "到「② 遺漏值轉 NA」"),
        "把它們改成 NA，再回來轉換。"
      )
    })

    all_cols <- shiny::reactive(unique(unlist(purrr::map(working$data(), names))))

    # 1. 世代 ----
    output$cohort_table <- shiny::renderUI({
      keys <- files$keys()
      if (length(keys) == 0) return(NULL)
      dfs <- working$data()
      fields <- typed_fields()
      pf <- pf_fields()
      guess <- kits_guess_per_file(dfs)
      labels <- files$labels()
      rows <- purrr::pmap(list(keys, files$names(), dfs, labels, seq_along(keys)),
                          function(key, name, df, wl, k) {
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
          )),
          purrr::map(pf, function(field) {
            id <- pf_id(field, key)
            prev <- shiny::isolate(input[[id]])
            selected <- if (!is.null(prev) && prev %in% names(df)) prev else guess[[field]][k]
            shiny::tags$td(shiny::selectInput(
              ns(id), NULL, width = "10rem", selected = selected,
              choices = c(stats::setNames("", "（沒有）"), names(df))
            ))
          }),
          purrr::map(fields, function(field) {
            id <- typed_id(field, key)
            shiny::tags$td(shiny::textInput(
              ns(id), NULL, width = "9rem",
              value = shiny::isolate(input[[id]]) %||% typed_default(field, wl),
              placeholder = if (field == "wave_m") kitsdata_wave_example(wave_unit()) else
                if (field == "age_m") "例如 18"
            ))
          })
        )
      })
      shiny::tagList(
        shiny::tags$div(
          class = "kit-table-scroll",
          shiny::tags$table(
            class = "table table-sm align-middle kit-file-table",
            shiny::tags$thead(shiny::tags$tr(
              shiny::tags$th("檔案"), shiny::tags$th("世代從哪裡來"), shiny::tags$th("世代"),
              purrr::map(pf, function(field) {
                shiny::tags$th(KITS_MAP_ROWS$label[KITS_MAP_ROWS$field == field], "的欄",
                               shiny::tags$code(field))
              }),
              purrr::map(fields, function(field) {
                shiny::tags$th(KITS_MAP_ROWS$label[KITS_MAP_ROWS$field == field],
                               shiny::tags$code(field))
              })
            )),
            shiny::tags$tbody(rows)
          )
        ),
        shiny::tags$small(
          class = "kit-hint",
          "世代一定要給（例如 M03、M36）；空白時不能轉換。這一頁不會從檔名猜世代。",
          if (length(fields) > 0) "「2. 對應欄位」選「自己填」的欄，每個檔案都要填一個值。",
          if (length(pf) > 0) "「2. 對應欄位」選「各檔各選一欄」的欄，每個檔案選自己的欄。"
        ),
        shiny::radioButtons(
          ns("value_labels"), "世代欄、性別欄有值標籤時（SPSS 檔），作答檔寫：",
          choices = c("代碼（例如 1、2）" = "code", "標籤（例如 男、女）" = "label"),
          selected = shiny::isolate(input$value_labels) %||% "code", inline = TRUE
        ),
        shiny::uiOutput(ns("typed_note"))
      )
    })
    output$typed_note <- shiny::renderUI({
      bad <- kits_typed_problems(files$names(), typed())
      if (length(bad) == 0) return(NULL)
      shiny::tags$div(class = "text-danger small", lapply(bad, shiny::tags$div))
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
      sources <- kits_guess_sources(working$data())
      for (field in KITS_MAP_ROWS$field) {
        id <- paste0("map_", field)
        prev <- input[[id]]
        selected <- if (!is.null(prev) && prev %in% cols) prev else guess[[field]]
        none <- if (field == "visit_id") "（自動組成）" else "（還沒選）"
        shiny::updateSelectInput(
          session, id, choices = c(stats::setNames("", none), cols), selected = selected
        )
        # 來源第一次的預設：各檔都有同一欄時是「某一欄」；欄名各檔不同、每個檔案都猜得到時是
        # 「各檔各選一欄」；猜不到的選填欄是「不用」（不會自動選自己填）
        row <- KITS_MAP_ROWS[KITS_MAP_ROWS$field == field, ]
        if ((row$typed || row$per_file) && is.null(input[[paste0("src_", field)]])) {
          shiny::updateSelectInput(session, paste0("src_", field), selected = sources[[field]])
        }
      }
    })

    # 對應欄位的來源：某一欄（column）、自己填（typed）、不用（none）；沒有選時是某一欄
    src_of <- function(field) {
      row <- KITS_MAP_ROWS[KITS_MAP_ROWS$field == field, ]
      if (!row$typed && !row$per_file) return("column")
      input[[paste0("src_", field)]] %||% "column"
    }
    map <- shiny::reactive({
      purrr::map(stats::setNames(nm = KITS_MAP_ROWS$field), function(field) {
        if (src_of(field) != "column") return("")
        input[[paste0("map_", field)]] %||% ""
      })
    })
    typed_fields <- shiny::reactive({
      KITS_MAP_ROWS$field[purrr::map_lgl(KITS_MAP_ROWS$field, function(f) src_of(f) == "typed")]
    })
    pf_fields <- shiny::reactive({
      KITS_MAP_ROWS$field[purrr::map_lgl(KITS_MAP_ROWS$field, function(f) src_of(f) == "per_file")]
    })
    # 各檔各選一欄：每個檔案一個欄名（還沒顯示選單時用每個檔案自己猜的欄）
    per_file <- shiny::reactive({
      keys <- files$keys()
      fields <- pf_fields()
      if (length(fields) == 0) return(list())
      guess <- kits_guess_per_file(working$data())
      stats::setNames(purrr::map(fields, function(field) {
        purrr::map2_chr(keys, seq_along(keys), function(key, k) {
          input[[pf_id(field, key)]] %||% guess[[field]][k]
        })
      }), fields)
    })
    # 自己填的值：每個檔案一個（還沒顯示輸入框時用預設值：波次標籤是 ① 的波次名稱）
    typed <- shiny::reactive({
      keys <- files$keys()
      labels <- files$labels()
      fields <- typed_fields()
      stats::setNames(purrr::map(fields, function(field) {
        purrr::map2_chr(keys, labels, function(key, wl) {
          trimws(input[[typed_id(field, key)]] %||% typed_default(field, wl))
        })
      }), fields)
    })

    wave_unit <- shiny::reactive(input$wave_unit %||% "month")
    output$wave_unit_note <- shiny::renderUI({
      if (identical(wave_unit(), "month")) return(NULL)
      shiny::tags$div(
        class = "kit-callout kit-callout-warn",
        "作答檔的 wave_m 只放數字，單位不會寫進去。kitsUI 目前把 wave_m 當成月：",
        "圖的時間軸、依 wave_m 借題庫參數的規則、age50 這類以月計算的結果，",
        "用年或波次序號時要自己換算或解讀。"
      )
    })

    output$visit_note <- shiny::renderUI({
      dfs <- working$data()
      if (length(dfs) == 0) return(NULL)
      first <- kits_apply_typed(dfs[1], list(map = map(), typed = purrr::map(typed(), 1)))
      rule <- kits_visit_rule(first$spec$map, per_file())
      example <- kitsdata_visit_example(first$dfs[[1]], cohorts()[[1]],
                                        kits_file_map(first$spec$map, per_file(), 1))
      cls <- if (is.null(rule$parts) || rule$parts[3] != "wave_m") "kit-source-note" else
        "kit-callout kit-callout-warn"
      shiny::tags$div(
        class = cls, rule$note,
        if (!is.null(example)) shiny::tags$span("例子：", shiny::tags$code(example))
      )
    })

    # 3. 題目設定 ----
    # 設定存在伺服器：一列一個候選欄（納入、領域、計分對照）；資料變了沿用舊設定
    settings <- shiny::reactiveVal(kits_settings_empty())
    upload <- shiny::reactiveVal(NULL)
    cohort_cols <- shiny::reactive({
      purrr::map_chr(purrr::keep(cohorts(), function(x) x$type == "column"), "value")
    })
    candidates <- shiny::reactive({
      kits_candidate_cols(working$data(), c(kits_mapped_cols(map(), per_file()), cohort_cols()))
    })
    shiny::observe({
      new <- kits_item_settings(working$data(), candidates()$numeric, codes_2())
      up <- shiny::isolate(upload())
      if (inherits(up, "error")) up <- NULL
      settings(kits_settings_sync(shiny::isolate(settings()), new, up, reverse_rule()))
    })
    reverse_rule <- shiny::reactive(input$reverse_rule %||% "suffix")
    # 換規則：拿掉用舊規則從原題來的領域，再用新規則套一次題目表
    shiny::observeEvent(input$reverse_rule, {
      up <- upload()
      if (is.null(up) || inherits(up, "error")) return()
      s <- settings()
      old <- !is.na(s$source) & startsWith(s$source, "題目表（原題")
      s$domain[old] <- NA_character_
      s$source[old] <- NA_character_
      settings(kits_settings_apply(s, up, reverse = reverse_rule()))
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$item_file, {
      f <- input$item_file
      up <- tryCatch(
        kits_settings_import(read_data_file(f$datapath, f$name)$data),
        error = function(e) e
      )
      upload(up)
      if (!inherits(up, "error")) {
        settings(kits_settings_apply(settings(), up, reverse = reverse_rule()))
      }
    })
    output$item_file_note <- shiny::renderUI({
      up <- upload()
      if (is.null(up)) return(NULL)
      if (inherits(up, "error")) {
        return(shiny::tags$div(class = "text-danger small", conditionMessage(up)))
      }
      what <- if (up$type == "settings") "題目設定檔" else "題目表"
      no_dom <- sum(is.na(up$data$domain))
      n_out <- sum(!is.na(up$data[["item_id_out"]] %||% NA))
      shiny::tags$small(
        class = "kit-hint",
        paste0("已套用", what, "（", nrow(up$data), " 題）",
               if (up$type == "settings") "：納入、領域與計分對照。" else "：領域。",
               if (n_out > 0) paste0(n_out, " 題有輸出題號（item_id_out）。"),
               if (no_dom > 0) paste0(no_dom, " 題沒有給領域，這些題的領域不變。"))
      )
    })
    # 檔名固定，按鈕只畫一次；內容是按下載當下的設定
    output$settings_dl_ui <- shiny::renderUI(kit_download_button(
      session, "settings_dl", "下載題目設定（CSV）", KITS_SETTINGS_FILE,
      function(file) readr::write_excel_csv(kits_settings_export(settings()), file, na = ""),
      class = "btn-sm btn-outline-primary"
    ))
    items <- shiny::reactive(kits_settings_items(settings()))

    output$settings_summary <- shiny::renderUI({
      s <- settings()
      if (length(working$data()) == 0) return(NULL)
      inc <- s[s$include, ]
      dc <- dplyr::count(inc[!is.na(inc$domain), ], .data$domain)
      text_cols <- candidates()$text
      items_now <- kits_settings_items(s)
      rename <- kits_item_rename(items_now)
      clash <- kits_rename_problems(working$data(), files$names(), items_now, rename)
      shiny::tags$div(
        class = "kit-source-note",
        if (length(rename) > 0) {
          shiny::tags$p(paste0(
            length(rename), " 題改了輸出題號，合成 ", length(unique(rename)), " 個題號。"
          ))
        },
        if (length(clash) > 0) {
          shiny::tags$div(class = "text-danger small", lapply(clash, shiny::tags$div))
        },
        shiny::tags$p(paste0(
          nrow(s), " 欄：納入 ", nrow(inc), "、排除 ", nrow(s) - nrow(inc), "；",
          "納入但沒有領域 ", sum(is.na(inc$domain)), " 題",
          if (nrow(dc) > 0) paste0("；各領域：", paste0(dc$domain, " ", dc$n, " 題", collapse = "、")),
          "。"
        )),
        if (length(text_cols) > 0) {
          shiny::tags$p(class = "small", paste0(
            "文字欄不能當題目，不列在表裡：", paste(utils::head(text_cols, 10), collapse = "、"),
            if (length(text_cols) > 10) "…", "。"
          ))
        },
        kitsdata_mixed_note(candidates()$mixed, files$names())
      )
    })

    # 選取：存在伺服器；全選、全不選、選取篩選出來的列
    sel <- shiny::reactiveVal(character(0))
    set_sel <- function(rows) {
      sel(rows)
      shiny::updateCheckboxGroupInput(session, "sel", selected = rows)
    }
    shiny::observeEvent(input$sel, sel(input$sel %||% character(0)), ignoreNULL = FALSE)
    shiny::observeEvent(input$sel_all, set_sel(settings()$column))
    shiny::observeEvent(input$sel_none, set_sel(character(0)))
    filtered <- shiny::reactive(kitsdata_filter_cols(settings()$column, input$sel_filter))
    shiny::observeEvent(input$sel_filtered, set_sel(filtered()))
    output$sel_count <- shiny::renderText({
      paste0("已選取 ", length(intersect(sel(), settings()$column)), " 列")
    })
    shiny::observe({
      doms <- sort(unique(stats::na.omit(settings()$domain)))
      shiny::updateSelectizeInput(session, "bulk_domain", choices = doms,
                                  selected = shiny::isolate(input$bulk_domain))
    })

    # 批次動作：只改選取的列
    act <- function(fun) {
      rows <- intersect(sel(), settings()$column)
      if (length(rows) == 0) {
        shiny::showNotification("請先在表裡勾選要改的列。", type = "warning")
        return(invisible())
      }
      out <- tryCatch(fun(settings(), rows), error = function(e) e)
      if (inherits(out, "error")) {
        shiny::showNotification(conditionMessage(out), type = "error")
      } else {
        settings(out)
      }
    }
    shiny::observeEvent(input$do_domain, act(function(s, rows) {
      d <- trimws(input$bulk_domain %||% "")
      if (!nzchar(d)) kit_abort("請先選或輸入領域，例如 lan。")
      kits_bulk_domain(s, rows, d)
    }))
    shiny::observeEvent(input$do_shift, act(function(s, rows) {
      by <- input$bulk_shift
      if (is.null(by) || is.na(by) || by != round(by) || by == 0) {
        kit_abort("平移量要是不等於 0 的整數，例如 -1。")
      }
      kits_bulk_shift(s, rows, by)
    }))
    shiny::observeEvent(input$do_reverse, act(function(s, rows) {
      txt <- trimws(input$bulk_max %||% "")
      top <- if (nzchar(txt)) suppressWarnings(as.numeric(txt)) else NA_real_
      if (nzchar(txt) && is.na(top)) kit_abort("最高分要是數字，或留空白（用各題目前的最高計分）。")
      kits_bulk_reverse(s, rows, top)
    }))
    shiny::observeEvent(input$do_merge, act(function(s, rows) {
      from <- kits_parse_values(input$merge_from)
      to <- input$merge_to
      if (length(from) == 0) kit_abort("請填要合併的原分數，例如 4、5。")
      if (is.null(to) || is.na(to)) kit_abort("請填合併後的計分，例如 3。")
      kits_bulk_merge(s, rows, from, to)
    }))
    shiny::observeEvent(input$do_restore, act(kits_bulk_restore))
    shiny::observeEvent(input$do_out_prefix, act(function(s, rows) {
      kits_bulk_out_prefix(s, rows, trimws(input$out_pattern %||% ""))
    }))
    shiny::observeEvent(input$do_out_reset, act(kits_bulk_out_reset))
    shiny::observeEvent(input$do_exclude, act(function(s, rows) kits_bulk_include(s, rows, FALSE)))
    shiny::observeEvent(input$do_include, act(function(s, rows) kits_bulk_include(s, rows, TRUE)))

    # 建議：所有納入的題都從 m 開始 → 一鍵選取全部、平移 −m（使用者按了才做）
    output$shift_suggest <- shiny::renderUI({
      m <- kits_settings_shift_hint(settings())
      if (is.null(m)) return(NULL)
      shiny::tags$div(
        class = "kit-callout",
        shiny::tags$strong("建議："),
        paste0("所有題目都從 ", m, " 開始，要選取全部、平移 ", kits_shift_text(-m), " 嗎？",
               "（kits 的分數要從 0 開始）"),
        shiny::actionButton(ns("apply_hint"), paste0("選取全部並平移 ", kits_shift_text(-m)),
                            class = "btn-sm btn-primary ms-2")
      )
    })
    shiny::observeEvent(input$apply_hint, {
      m <- kits_settings_shift_hint(settings())
      if (is.null(m)) return()
      rows <- settings()$column[settings()$include]
      set_sel(rows)
      settings(kits_bulk_shift(settings(), rows, -m))
    })

    # 篩選不是這張表的依賴：瀏覽器自己藏列（kitsdata_filter_js()），重畫時才帶入目前的篩選
    output$settings_table <- shiny::renderUI({
      kitsdata_settings_table(
        ns("sel"), settings(), shiny::isolate(sel()), shiny::isolate(filtered())
      )
    })

    # 4. pivot_longer：參數全部固定 ----
    pivot_args <- mod_pivot_longer_server(
      "pivot", data = shiny::reactive(NULL),
      fixed = list(
        cols = shiny::reactive(items()$column), names_to = "item_id",
        values_to = "item_score", names_prefix = NULL, names_pattern = NULL,
        values_drop_na = TRUE
      )
    )

    spec <- shiny::reactive({
      kits_spec(cohorts(), map(), items(), kits_settings_undecided(settings()), typed(),
                codes_2(), per_file(), input$value_labels %||% "code")
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
              paste0(
                "還不能轉換：有 ", length(out$problems), " 個問題要先處理，",
                "處理後再按一次「執行轉換」。"
              ),
              warnings = out$problems, notes = out$notes
            )
          } else {
            # 長格式不在這裡做出來：下載時才一段一段寫進 .csv.gz（省記憶體）
            result(out)
            kit_done_status(
              paste0(
                "檢查完成：作答檔會有 ", kit_num(out$n_rows), " 列、",
                kit_num(out$n_visits), " 個列次、", kit_num(out$n_items), " 題。"
              ),
              warnings = out$warnings, notes = out$notes
            )
          }
        }),
        error = function(e) {
          kit_error_status(e, "轉換", kit_upload_todo(ns, length(files$keys())))
        }
      )
      status(st)
      kit_notify(st)
    })

    output$status <- shiny::renderUI(make_status_ui(status()))

    output$result <- shiny::renderUI({
      res <- result()
      if (is.null(res)) return(NULL)
      kits_name <- timestamped_name("kits_long")
      shiny::tagList(
        shiny::hr(),
        shiny::h4("結果"),
        shiny::tags$div(
          class = "kit-downloads",
          kit_download_button(
            session, "download_gz", "下載作答檔（.csv.gz）", kits_name("csv.gz"),
            function(file) kitsdata_download(res$prep, file, write_kits_long_gz),
            class = "btn-primary me-2"
          ),
          kit_download_button(
            session, "download_csv", "下載未壓縮的 CSV", kits_name("csv"),
            function(file) kitsdata_download(res$prep, file, write_kits_long_csv),
            class = "btn-outline-primary"
          ),
          shiny::tags$small(
            class = "kit-hint",
            paste0("未壓縮約 ", kits_size_text(res$csv_mb * 1024^2), "；壓縮檔通常小很多。"),
            "按下載後才一段一段寫出檔案，資料大時要等一下。",
            "kitsUI 可以直接讀 .csv.gz（單檔上限 ", KITSUI_UPLOAD_MB, " MB）。"
          ),
          shiny::tags$div(class = "mt-2", kit_kitsui_button("開啟 kitsUI 上傳作答檔"))
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

    output$code <- shiny::renderUI({
      text_fix <- kits_text_fix(working$data(), items()$column)
      kit_code_blocks(list(tidy = code_kitsdata(
        code_files_from(files), spec(), na_applied = identical(working$source(), "na"),
        text_fix = text_fix
      ), base = NULL))
    })

    # 這一頁的狀態（步驟分頁打 ✓ 用）
    list(state = shiny::reactive(status()$state))
  })
}

#' 在某些檔案是文字的題目欄：哪個檔案、哪一欄（那個檔案的這一題不轉）
#' @noRd
kitsdata_mixed_note <- function(mixed, file_names) {
  if (is.null(mixed) || nrow(mixed) == 0) return(NULL)
  shown <- paste0(mixed$column, "（", file_names[mixed$file], "）")
  shiny::tags$p(class = "small text-warning", paste0(
    nrow(mixed), " 個題目欄在部分檔案是文字：", paste(utils::head(shown, 8), collapse = "、"),
    if (nrow(mixed) > 8) "…", "。這些檔案的這一題不轉，其他檔案照常轉。"
  ))
}

#' 第一個檔案第一列的 visit_id 例子（看得出怎麼組）
#' @noRd
kitsdata_visit_example <- function(df, cohort, map) {
  if (nrow(df) == 0 || is.null(cohort) || !nzchar(cohort$value %||% "")) return(NULL)
  if (identical(cohort$type, "column") && !cohort$value %in% names(df)) return(NULL)
  needed <- kits_mapped_cols(map)
  if (!all(needed %in% names(df))) return(NULL)
  one <- df[1, , drop = FALSE]
  co <- if (identical(cohort$type, "column")) kits_id_text(one[[cohort$value]]) else
    cohort$value
  persons <- tibble::tibble(
    cohort = co,
    subj_id = if (nzchar(map$subj_id %||% "")) kits_id_text(one[[map$subj_id]]) else NA,
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

# 自己填的輸入框 id：typed_<欄>_<檔案>
typed_id <- function(field, key) paste0("typed_", field, "_", key)

# 各檔各選一欄的選單 id：pf_<欄>_<檔案>
pf_id <- function(field, key) paste0("pf_", field, "_", key)

# wave_m 自己填的例子（依單位）
kitsdata_wave_example <- function(unit) {
  switch(unit, year = "例如 1", index = "例如 2", other = "例如 0", "例如 18")
}

# 自己填的預設值：波次標籤用 ① 的波次名稱（t1、t2…），其他空白
typed_default <- function(field, wave_label) {
  if (identical(field, "wave")) wave_label %||% "" else ""
}

#' 「3. 題目設定」的表
#'
#' 選取欄用 Shiny 的 checkboxGroupInput 格式（容器 class 與 name = id），所以 input[[id]]
#' 是選取的題號，updateCheckboxGroupInput() 也能用。篩選掉的列只是藏起來（選取不會不見）。
#' 計分對照用兩列的小表：上一列原分數、下一列計分（改過的粗體）。
#' 題目多時（150–300 題）用 htmltools 的標籤太慢：整張表一次用字串組好，回傳 HTML()；
#' 標記和以前用標籤組的一樣（只少了標籤之間的換行與縮排），每一列多一個 data-col（給篩選用）。
#' @param id 已經加上 namespace 的 input id
#' @param shown 篩選出來的題號（其他列藏起來）
#' @noRd
kitsdata_settings_table <- function(id, settings, selected = character(0),
                                    shown = settings$column) {
  if (nrow(settings) == 0) {
    return(shiny::tags$small(class = "kit-hint", "還沒有題目欄：請先在 ① 上傳資料，並完成「2.」。"))
  }
  esc <- function(x) htmltools::htmlEscape(x)
  att <- function(x) htmltools::htmlEscape(x, attribute = TRUE)
  col <- settings$column
  inc <- settings$include
  map <- kitsdata_map_cells(settings$raw, settings$score)
  domain <- ifelse(
    is.na(settings$domain), "<span class=\"text-danger\">（沒有）</span>", esc(settings$domain)
  )
  source <- ifelse(
    is.na(settings$source), "",
    paste0("<div class=\"text-muted small\">", esc(settings$source), "</div>")
  )
  out <- settings[["item_id_out"]] %||% rep(NA_character_, nrow(settings))
  out_id <- ifelse(
    is.na(out), "", paste0("<div class=\"small\">→ 題號 <code>", esc(out), "</code></div>")
  )
  rows <- paste0(
    "<tr data-col=\"", att(col), "\"",
    ifelse(inc, "", " class=\"excluded\""),
    ifelse(col %in% shown, "", " style=\"display: none;\""), ">",
    "<td><input type=\"checkbox\" name=\"", att(id), "\" value=\"", att(col), "\"",
    ifelse(col %in% selected, " checked=\"checked\"", ""), "/></td>",
    "<td>", ifelse(inc, "納入", "排除"), "</td>",
    "<td><code>", esc(col), "</code>", out_id, "</td>",
    "<td>", domain, source, "</td>",
    "<td>", map$raw_text, "</td><td>", map$table, "</td><td>", map$after_text, "</td>",
    "<td class=\"small text-danger\">", esc(kits_settings_alerts(settings)), "</td></tr>"
  )
  head <- c("選取", "納入", "欄位（item_id）", "領域", "原分數",
            "計分對照（原分數 → 計分）", "計分後", "提醒")
  htmltools::HTML(paste0(
    "<div id=\"", att(id), "\" class=\"form-group shiny-input-checkboxgroup ",
    "shiny-input-container\" style=\"width: 100%;\">",
    "<div class=\"shiny-options-group kit-settings-scroll\">",
    "<table class=\"table table-sm align-middle kit-settings\">",
    "<thead><tr>", paste0("<th>", head, "</th>", collapse = ""), "</tr></thead>",
    "<tbody>", paste(rows, collapse = ""), "</tbody></table></div></div>"
  ))
}

#' 每一列的原分數、計分對照小表、計分後（三個字元向量，一列一個）
#'
#' 所有列的分數先攤平成一個向量一起處理，再依列號接回去。
#' 原分數很多種（例如體重這種連續的欄）：只寫幾種、範圍，對照表收起來。
#' @noRd
kitsdata_map_cells <- function(raw, score) {
  n <- length(raw)
  len <- lengths(raw)
  # 計分和原分數一樣長（不夠的補 NA，和以前 score[k] 的行為一樣）
  if (!identical(lengths(score), len)) {
    score <- Map(function(r, s) s[seq_along(r)], raw, score)
  }
  row <- factor(rep.int(seq_len(n), len), levels = seq_len(n))
  r <- as.numeric(unlist(raw, use.names = FALSE))
  s <- as.numeric(unlist(score, use.names = FALSE))
  r_txt <- kits_num_text(r)
  s_cell <- ifelse(
    is.na(s), "<td class=\"changed text-danger\">？</td>",
    paste0("<td", ifelse(s != r, " class=\"changed\"", ""), ">", kits_num_text(s), "</td>")
  )
  join <- function(x, by, sep = "") {
    unname(vapply(split(x, by), paste, character(1), collapse = sep))
  }
  table <- paste0(
    "<table class=\"kit-map\"><tr><th>原分數</th>", join(paste0("<td>", r_txt, "</td>"), row),
    "</tr><tr><th>計分</th>", join(s_cell, row), "</tr></table>"
  )
  many <- len > KITS_MAP_SHOW_MAX
  table[many] <- paste0("<details><summary>展開計分對照</summary>", table[many], "</details>")
  table[len == 0] <- "<span class=\"text-muted\">（沒有作答）</span>"

  # 計分後：每列去掉 NA、排序、去重複
  ok <- !is.na(s)
  a_row <- as.integer(row)[ok]
  a <- s[ok]
  o <- order(a_row, a)
  a_row <- a_row[o]
  a <- a[o]
  k <- length(a)
  keep <- k > 0 & c(TRUE, a_row[-1] != a_row[-k] | a[-1] != a[-k])
  a_row <- factor(a_row[keep], levels = seq_len(n))
  a <- a[keep]
  list(
    raw_text = kitsdata_values_text(r, r_txt, row),
    table = table,
    after_text = kitsdata_values_text(a, kits_num_text(a), a_row)
  )
}

#' 每列的分數寫成一段文字：「0 1 2」；超過 KITS_MAP_SHOW_MAX 種時寫「29 種（8.1–11.9）」
#'
#' 有小數的列用整個向量一起 format()（小數位數一致，和以前一樣）。
#' @param x 攤平的分數；txt 每個分數單獨的文字；row 列號（factor）
#' @noRd
kitsdata_values_text <- function(x, txt, row) {
  n <- nlevels(row)
  out <- unname(vapply(split(txt, row), paste, character(1), collapse = " "))
  frac <- which(tabulate(as.integer(row)[!kits_num_is_int(x)], n) > 0)
  if (length(frac) > 0) {
    out[frac] <- vapply(split(x, row)[frac], function(v) {
      paste(kits_num_format(v), collapse = " ")
    }, character(1))
  }
  cnt <- tabulate(as.integer(row), n)
  many <- which(cnt > KITS_MAP_SHOW_MAX)
  if (length(many) > 0) {
    lo <- vapply(split(x, row)[many], min, numeric(1))
    hi <- vapply(split(x, row)[many], max, numeric(1))
    out[many] <- paste0(cnt[many], " 種（", kits_num_text(lo), "–", kits_num_text(hi), "）")
  }
  out
}

# 數字的文字（和以前一樣：format(trim, drop0trailing, scientific = FALSE)）
kits_num_format <- function(x) {
  format(x, trim = TRUE, drop0trailing = TRUE, scientific = FALSE)
}

# 整數（不會被寫成科學記號）的分數
kits_num_is_int <- function(x) is.finite(x) & x == round(x) & abs(x) < 1e15

#' 每個數字單獨寫成文字，和 kits_num_format() 一個一個寫的結果相同
#'
#' 整數用 sprintf() 一次寫完（-0 寫成 0）；有小數的才一個一個 format()。
#' @noRd
kits_num_text <- function(x) {
  out <- sprintf("%.0f", x)
  out[!is.na(x) & x == 0] <- "0"
  slow <- which(!kits_num_is_int(x))
  out[slow] <- vapply(x[slow], kits_num_format, character(1))
  out
}

# 依欄名篩選：去掉前後空白、不分大小寫、包含就算（和 kitsdata_filter_js() 一樣）
kitsdata_filter_cols <- function(cols, text) {
  f <- tolower(trimws(text %||% ""))
  if (!nzchar(f)) cols else cols[grepl(f, tolower(cols), fixed = TRUE)]
}

# 計分對照最多直接顯示幾種原分數（再多就收起來）
KITS_MAP_SHOW_MAX <- 12
