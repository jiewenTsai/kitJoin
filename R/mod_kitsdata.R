# 轉成 kits 作答檔
#
# 三個步驟，做完一步收成一行（KIT 設計語言：步驟卡）：
#   1. 每個檔案是哪個世代、哪個波次（一列一個檔案）
#   2. 哪些欄是題目（勾組、上傳題目表，或用欄名挑；一開始一題都不納入）。只挑題目，不設領域
#   3. 題目表：領域與計分。先列檢查清單（只列出要決定的事，旁邊就是決定的按鈕），
#      下面直接是題目表（逐題的領域與計分，可以下載，下次在第 2 步上傳）
# 三步都好了才能按「轉換成作答檔」；轉換完，主角換成「下載作答檔」。
# 純函式在 kitsdata_steps.R；轉換與檢查在 kitsdata_pipeline.R。

#' @noRd
mod_kitsdata_ui <- function(id) {
  ns <- shiny::NS(id)
  open_is <- function(k) paste0("output.open == '", k, "'")
  shiny::tagList(
    kit_tab_guide(
      goal = "把各波次的資料轉成 kitsUI 要的作答檔。",
      steps = kit_steps(
        "說清楚每個檔案是哪個世代、哪個波次",
        "挑出哪些欄是題目（有題目表就上傳）",
        "在題目表看領域與計分，決定要不要改",
        "按「轉換成作答檔」，下載"
      ),
      output = shiny::tagList(
        "一個作答檔（.csv.gz；多個檔案合併成一個），可以直接上傳到 kitsUI。",
        " ", kit_kitsui_link("開啟 kitsUI", kind = "secondary", class = "btn-sm")
      ),
      note = paste0(
        "只要一個波次或一個領域時，請在「檔案上傳」只上傳那一個檔案。",
        "沒有作答的題不會放進作答檔。"
      ),
      technical = paste0(
        "每個檔案先把每一題的原分數換成計分（計分對照），",
        "再用 tidyr::pivot_longer() 把題目欄轉成 item_id、item_score，最後接上領域並合併。",
        "檢查都在寬資料上做；長格式在下載時每 5,000 列寬資料轉一段、寫進 .csv.gz，不會整張放進記憶體。",
        "visit_id 由 {cohort}_{subj_id}_{波次標籤} 組成；沒有波次標籤欄時改用 wave_m。",
        "這一頁不把任何值改成 NA：疑似遺漏值代碼（值標籤是「遺漏值」「跳答」這類、",
        "「遺漏值處理」的代碼、7777 以上、和分數明顯分開的值）要在第 3 步決定：",
        "先用「遺漏值處理」改成 NA，或照原樣當成分數。"
      )
    ),
    shiny::uiOutput(ns("source_note")),
    shiny::conditionalPanel(
      "output.has_files == 'yes'", ns = ns,

      # 第 1 步 ----
      kit_step_card(
        shiny::uiOutput(ns("head1")),
        shiny::conditionalPanel(
          open_is(1), ns = ns, kit_step_body(
          shiny::tags$p("已經照欄名猜好了（淡黃色），請看一遍，不對就改。",
                        "每一格可以選檔案裡的一欄，也可以直接打字填一個值（例如 2009）。"),
          shiny::uiOutput(ns("files_table")),
          shiny::actionLink(ns("fill_cohort"), "第一列的世代套用到每一列"),
          shiny::tags$div(
            class = "kit-inline",
            shiny::selectInput(ns("wave_unit"), "波次的單位", choices = KITS_WAVE_UNITS,
                               selected = "month", width = "15rem"),
            shiny::uiOutput(ns("visit_note"))
          ),
          shiny::uiOutput(ns("wave_unit_note")),
          kit_detail(
            "還有性別、實際月齡、波次標籤（不一定要）",
            shiny::uiOutput(ns("more_table")),
            shiny::radioButtons(
              ns("value_labels"), "世代欄、性別欄有值標籤時（SPSS 檔），作答檔寫：",
              choices = c("代碼（例如 1、2）" = "code", "標籤（例如 男、女）" = "label"),
              selected = "code", inline = TRUE
            )
          ),
          shiny::uiOutput(ns("step1_problems")),
          shiny::actionButton(ns("ok1"), "對，下一步", class = "btn-default")
        )),
        title_id = ns("steptitle1")
      ),

      # 第 2 步 ----
      kit_step_card(
        shiny::uiOutput(ns("head2")),
        shiny::conditionalPanel(
          open_is(2), ns = ns, kit_step_body(
          shiny::uiOutput(ns("intro2")),
          shiny::tags$div(
            class = "kit-inline",
            shiny::textInput(ns("grp_search"), NULL, width = "20rem",
                             placeholder = "找欄名或題目內容，例如 d5 或 緊張")
          ),
          shiny::tags$div(
            class = "kit-pager",
            shiny::actionButton(ns("grp_prev"), "上一頁", class = "btn-sm btn-default"),
            shiny::textOutput(ns("grp_page"), inline = TRUE),
            shiny::actionButton(ns("grp_next"), "下一頁", class = "btn-sm btn-default")
          ),
          shiny::uiOutput(ns("groups_table")),
          shiny::uiOutput(ns("merge_ui")),
          shiny::uiOutput(ns("items_status")),
          kit_details_row(
            kit_detail(
              "有題目表就上傳",
              shiny::fileInput(
                ns("item_file"), "上傳題目表（之前下載的題目表，或 item_id、domain 兩欄的 CSV/SAV）",
                accept = c(".csv", ".sav"), buttonLabel = "選擇檔案…", placeholder = "還沒有上傳",
                width = "34rem"
              ),
              shiny::tags$p(class = "kit-hint",
                "一列一題：item_id（資料裡的欄名）、domain（領域）；",
                "可以再加 item_id_out（作答檔的題號：各波次欄名不同、其實是同一題時，給同一個題號）。",
                "之前在第 3 步下載的題目表，會連領域、計分對照一起套用。"),
              shiny::selectInput(
                ns("reverse_rule"), "反向題的欄名（題目表查不到時，用原題的領域）",
                choices = KITS_REVERSE_RULES, selected = "suffix", width = "24rem"
              )
            ),
            kit_detail(
              "用欄名一次挑很多欄",
              shiny::tags$div(
                class = "kit-inline",
                shiny::textInput(ns("pick_prefix"), "欄名開頭是", width = "16rem",
                                 placeholder = "例如 w1d5_、w2d5_"),
                shiny::actionButton(ns("pick_add"), "加入這些欄", class = "btn-default")
              ),
              shiny::uiOutput(ns("pick_note")),
              shiny::actionLink(ns("pick_clear"), "全部清掉，重新挑")
            ),
            kit_detail("看挑出來的題目", shiny::uiOutput(ns("items_list"))),
            kit_detail("沒有列在上面的欄", shiny::uiOutput(ns("text_cols")))
          ),
          shiny::actionButton(ns("ok2"), "對，下一步", class = "btn-default")
        )),
        title_id = ns("steptitle2")
      ),

      # 第 3 步 ----
      kit_step_card(
        shiny::uiOutput(ns("head3")),
        shiny::conditionalPanel(
          open_is(3), ns = ns, kit_step_body(
          shiny::tags$p("kits 要的分數是 0、1、2…。轉換前，看下面每一件事："),
          shiny::uiOutput(ns("checks")),
          shiny::tags$div(
            id = ns("item_table"), class = "kit-itemtable",
            shiny::tags$h5(class = "kit-itemtable-title",
                           shiny::textOutput(ns("items_title"), inline = TRUE)),
            shiny::tags$p(class = "kit-hint",
              "一列一欄。勾選幾列，再用表下面的「對勾選的列」改領域或計分。"),
            kitsdata_items_tools(ns),
            shiny::uiOutput(ns("items_table")),
            kitsdata_bulk_tools(ns),
            shiny::tags$div(class = "kit-downloads kit-itemtable-dl",
                            shiny::uiOutput(ns("table_dl_ui"), inline = TRUE))
          )
        )),
        title_id = ns("steptitle3")
      ),

      shiny::uiOutput(ns("ready")),
      shiny::uiOutput(ns("run_ui")),
      shiny::uiOutput(ns("status")),
      shiny::uiOutput(ns("result"))
    )
  )
}

# 題目表上面：找欄名、要不要顯示沒挑的欄、換頁、勾選
kitsdata_items_tools <- function(ns) {
  shiny::tagList(
    shiny::tags$div(
      class = "kit-inline",
      shiny::textInput(ns("tbl_search"), NULL, placeholder = "找欄名，例如 d5", width = "14rem"),
      shiny::checkboxInput(ns("tbl_all"), "也顯示沒挑的欄", value = FALSE)
    ),
    shiny::tags$div(
      class = "kit-pager",
      shiny::actionButton(ns("tbl_prev"), "上一頁", class = "btn-sm btn-default"),
      shiny::textOutput(ns("tbl_page"), inline = TRUE),
      shiny::actionButton(ns("tbl_next"), "下一頁", class = "btn-sm btn-default")
    )
  )
}

# 題目表下面：對勾選的列做一件事
KITS_BULK_ACTIONS <- c(
  "設領域" = "domain", "計分加減（例如 -1）" = "shift", "反向計分" = "reverse",
  "合併原分數" = "merge", "還原（計分＝原分數）" = "restore", "不要這幾欄" = "exclude",
  "要這幾欄" = "include", "輸出題號：去掉欄名的一段" = "out_prefix", "輸出題號：還原成欄名" = "out_reset"
)
kitsdata_bulk_tools <- function(ns) {
  when <- function(value, ...) {
    shiny::conditionalPanel(paste0("input.bulk_action == '", value, "'"), ns = ns, ...)
  }
  shiny::tags$div(
    class = "kit-bulk",
    shiny::tags$div(
      class = "kit-inline",
      shiny::actionLink(ns("tbl_sel_all"), "勾選全部符合的列"),
      shiny::actionLink(ns("tbl_sel_none"), "全部不勾"),
      shiny::textOutput(ns("sel_count"), inline = TRUE)
    ),
    shiny::tags$div(
      class = "kit-inline",
      shiny::selectInput(ns("bulk_action"), "對勾選的列", choices = KITS_BULK_ACTIONS,
                         width = "17rem"),
      when("domain", shiny::selectizeInput(
        ns("bulk_domain"), "領域", choices = NULL, width = "12rem",
        options = list(create = TRUE, placeholder = "選或輸入領域")
      )),
      when("shift", shiny::numericInput(ns("bulk_shift"), "加減", value = -1, step = 1,
                                        width = "7rem")),
      when("reverse", shiny::textInput(ns("bulk_max"), "最高分", placeholder = "空白＝各題最高分",
                                       width = "11rem")),
      when("merge",
           shiny::textInput(ns("merge_from"), "原分數", placeholder = "例如 4、5", width = "8rem"),
           shiny::numericInput(ns("merge_to"), "都變成", value = NA, step = 1, width = "7rem")),
      when("out_prefix", shiny::textInput(ns("out_pattern"), "去掉（規則）", value = "^w[0-9]+",
                                          width = "9rem")),
      shiny::actionButton(ns("bulk_apply"), "套用", class = "btn-default")
    ),
    shiny::tags$small(
      class = "kit-hint",
      "加減、反向都在目前的計分上做。遺漏值代碼（例如 98、99）不要用合併接起來，",
      "請先用「遺漏值處理」改成 NA。輸出題號是作答檔的 item_id：各波次欄名不同、其實是同一題時，",
      "給它們同一個題號（例如 w1d5_1、w2d5_1 去掉 ^w[0-9]+ 都是 d5_1）。"
    )
  )
}

#' @param files mod_upload_server() 的回傳值
#' @param working mod_missing_server() 的回傳值
#' @param goto function(tab) 切換分頁（到「遺漏值處理」）
#' @param na_codes NULL 或 reactive：「遺漏值處理」的代碼清單（只用來找疑似遺漏值代碼）
#' @noRd
mod_kitsdata_server <- function(id, files, working, goto, na_codes = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    codes_2 <- shiny::reactive(if (is.null(na_codes)) NULL else na_codes())
    dfs <- shiny::reactive(working$data())
    has_files <- shiny::reactive(length(files$keys()) > 0)

    # 步驟：現在打開哪一步（0 = 都收起來，轉換完成時），第 1、2 步按過「對，下一步」沒有
    open <- shiny::reactiveVal(1L)
    ok <- shiny::reactiveValues(s1 = FALSE, s2 = FALSE)
    output$open <- shiny::renderText(as.character(open()))
    output$has_files <- shiny::renderText(if (has_files()) "yes" else "no")
    shiny::outputOptions(output, "open", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "has_files", suspendWhenHidden = FALSE)
    # 換了檔案：從第 1 步重來
    shiny::observeEvent(files$keys(), {
      ok$s1 <- FALSE
      ok$s2 <- FALSE
      open(1L)
    }, ignoreInit = TRUE)
    # 「改」連結在標頭裡，標頭重畫時連結也重畫（值從 0 開始）：只有真的按了（> 0）才打開
    clicked <- function(x) isTRUE(x > 0)
    shiny::observeEvent(input$edit1, if (clicked(input$edit1)) open(1L))
    shiny::observeEvent(input$edit2, if (clicked(input$edit2) && ok$s1) open(2L))
    shiny::observeEvent(input$edit3, if (clicked(input$edit3) && ok$s1 && ok$s2) open(3L))

    output$source_note <- shiny::renderUI({
      if (!has_files()) {
        return(shiny::tags$div(
          class = "kit-callout kit-callout-warn",
          "請先到「檔案上傳」加入寬格式的資料檔（一個或多個）。"
        ))
      }
      if (identical(working$source(), "na")) {
        return(shiny::tags$p(class = "kit-hint", "用的是「遺漏值處理」處理後的資料。"))
      }
      # 「遺漏值處理」的設定或檔案改過、還沒重新執行
      if (identical(working$source(), "stale")) {
        shiny::tags$div(
          class = "kit-callout kit-callout-warn",
          "「遺漏值處理」的設定改過了，請",
          shiny::actionLink(ns("goto_missing_stale"), "重新執行「遺漏值處理」"),
          "，這一頁才會用到新的結果。"
        )
      }
    })
    shiny::observeEvent(input$goto_missing, goto("missing"))
    shiny::observeEvent(input$goto_missing_stale, goto("missing"))
    shiny::observeEvent(input$goto_upload_status, goto("upload"))

    all_cols <- shiny::reactive(unique(unlist(purrr::map(dfs(), names))))

    # 第 1 步 ----
    guess <- shiny::reactive(kits_step1_guess(dfs(), files$names(), files$labels()))
    cell_id <- function(field, key) paste0("cell_", field, "_", key)
    cells <- shiny::reactive({
      keys <- files$keys()
      g <- guess()
      fields <- c(KITS_STEP1_MAIN, KITS_STEP1_MORE)
      stats::setNames(purrr::map(fields, function(field) {
        purrr::map2_chr(keys, seq_along(keys), function(key, k) {
          v <- input[[cell_id(field, key)]]
          if (is.null(v)) g[[field]][k] %||% "" else trimws(paste(v, collapse = ""))
        })
      }), fields)
    })
    step1 <- shiny::reactive(kits_step1_spec(dfs(), files$names(), cells()))

    # 一格：選檔案裡的一欄，或打字填一個值
    cell_input <- function(field, key, df, k, width = "11rem", fname = "") {
      id <- cell_id(field, key)
      prev <- shiny::isolate(input[[id]])
      g <- guess()[[field]][k] %||% ""
      value <- prev %||% g
      choices <- c("", names(df))
      if (nzchar(value) && !value %in% choices) choices <- c(choices, value)
      box <- shiny::selectizeInput(
        ns(id), NULL, choices = choices, selected = value, width = width,
        options = list(
          create = TRUE, createOnBlur = TRUE, persist = FALSE,
          # 選單掛在整頁上：表格外框會左右捲動（overflow），選單放在裡面會被切掉
          dropdownParent = "body",
          placeholder = if (field == "cohort") "例如 M03" else "選一欄或打字",
          # 表格裡的選單沒有看得到的標籤：給螢幕閱讀器「檔名 的 世代」
          onInitialize = I(sprintf("function() { this.$control_input.attr('aria-label', %s); }",
                                   jsonlite::toJSON(paste0(fname, " 的", KITS_STEP1_LABELS[[field]]),
                                                    auto_unbox = TRUE))),
          render = I("{option_create: function(d, esc) {
            return '<div class=\"create\">填入「' + esc(d.input) + '」</div>'; }}")
        )
      )
      shiny::tags$td(class = if (is.null(prev) && nzchar(g)) "kit-guess",
                     `data-label` = KITS_STEP1_LABELS[[field]], box)
    }
    file_rows <- function(fields) {
      keys <- files$keys()
      purrr::pmap(list(keys, files$names(), dfs(), seq_along(keys)), function(key, name, df, k) {
        shiny::tags$tr(
          shiny::tags$td(shiny::tags$strong(name), shiny::tags$br(),
                         shiny::tags$small(class = "kit-hint", kit_dims(nrow(df), ncol(df)))),
          purrr::map(fields, function(field) cell_input(field, key, df, k, fname = name))
        )
      })
    }
    file_table <- function(fields) {
      shiny::tags$div(
        class = "kit-table-scroll",
        shiny::tags$table(
          class = "table kit-file-table",
          shiny::tags$thead(shiny::tags$tr(
            shiny::tags$th("檔案"),
            purrr::map(fields, function(f) shiny::tags$th(KITS_STEP1_LABELS[[f]]))
          )),
          shiny::tags$tbody(file_rows(fields))
        )
      )
    }
    output$files_table <- shiny::renderUI({
      if (!has_files()) return(NULL)
      file_table(KITS_STEP1_MAIN)
    })
    output$more_table <- shiny::renderUI({
      if (!has_files()) return(NULL)
      shiny::tagList(
        file_table(KITS_STEP1_MORE),
        shiny::tags$small(class = "kit-hint",
          "實際月齡：kitsUI 的題目表通過年齡要用它。波次標籤（例如 m108g03）不會輸出，",
          "只用來組列次編號；列次編號通常讓它自動組成。")
      )
    })

    wave_unit <- shiny::reactive(input$wave_unit %||% "month")
    output$wave_unit_note <- shiny::renderUI({
      if (identical(wave_unit(), "month")) return(NULL)
      shiny::tags$small(
        class = "kit-hint",
        "作答檔的 wave_m 只放數字，單位不會寫進去。kitsUI 目前把 wave_m 當成月：",
        "圖的時間軸、依 wave_m 借題庫參數的規則、age50 這類以月計算的結果，",
        "用年或波次序號時要自己換算或解讀。"
      )
    })
    output$visit_note <- shiny::renderUI({
      s <- step1()
      if (!has_files() || length(s$problems) > 0) return(NULL)
      first <- kits_apply_typed(dfs()[1], list(map = s$map, typed = purrr::map(s$typed, 1)))
      example <- kitsdata_visit_example(first$dfs[[1]], s$cohort[[1]],
                                        kits_file_map(first$spec$map, s$per_file, 1))
      if (is.null(example)) return(NULL)
      shiny::tags$small(class = "kit-hint", "作答檔裡的列次編號會長這樣：", shiny::tags$code(example))
    })
    # 第一列的世代套用到每一列（在瀏覽器裡改選單；testServer 不會改 input）
    shiny::observeEvent(input$fill_cohort, {
      v <- cells()$cohort[1]
      if (!nzchar(v)) {
        shiny::showNotification("第一列的世代還沒有填。", type = "warning")
        return()
      }
      keys <- files$keys()
      for (k in seq_along(keys)[-1]) {
        shiny::updateSelectizeInput(session, cell_id("cohort", keys[k]),
                                    choices = c("", names(dfs()[[k]]), v), selected = v)
      }
    })

    step1_msg <- shiny::reactiveVal(NULL)
    output$step1_problems <- shiny::renderUI({
      msg <- step1_msg()
      if (length(msg) == 0) return(NULL)
      kit_problems(msg, head = "還差：")
    })
    shiny::observeEvent(cells(), step1_msg(NULL), ignoreInit = TRUE)
    shiny::observeEvent(input$ok1, {
      p <- step1()$problems
      if (length(p) > 0) {
        step1_msg(p)
        return()
      }
      step1_msg(NULL)
      ok$s1 <- TRUE
      open(if (ok$s2) 3L else 2L)
    })

    # 第 2 步：哪些欄是題目（一開始一題都不納入） ----
    # 題目設定：每次設定都補上預設領域（組名），所以轉換不用先設領域
    settings_rv <- shiny::reactiveVal(kits_settings_empty())
    settings <- function(value) {
      if (missing(value)) settings_rv() else settings_rv(kits_domain_defaults(value))
    }
    upload <- shiny::reactiveVal(NULL)
    cohort_cols <- shiny::reactive({
      purrr::map_chr(purrr::keep(step1()$cohort, function(x) x$type == "column"), "value")
    })
    # 候選欄與每一欄的題目設定，只在資料或代碼清單變了時算一次（2,000 欄約 1 秒）；
    # 第 1 步改對應欄位時只是從裡面拿掉對應到的欄（不用每打一格就重算）
    all_cand <- shiny::reactive(kits_candidate_cols(dfs(), character(0)))
    all_settings <- shiny::reactive({
      kits_item_settings(dfs(), all_cand()$numeric, codes_2(), include = FALSE)
    })
    mapped_cols <- shiny::reactive({
      s <- step1()
      sort(unique(c(kits_mapped_cols(s$map, s$per_file), cohort_cols())))
    })
    candidates <- shiny::reactive({
      a <- all_cand()
      m <- mapped_cols()
      list(numeric = setdiff(a$numeric, m), text = setdiff(a$text, m),
           mixed = a$mixed[!a$mixed$column %in% m, ])
    })
    reverse_rule <- shiny::reactive(input$reverse_rule %||% "suffix")
    shiny::observe({
      a <- all_settings()
      new <- a[a$column %in% candidates()$numeric, ]
      old <- shiny::isolate(settings())
      # 欄沒變（例如第 1 步只改了世代的值）：不用重新同步
      if (identical(new$column, old$column) && identical(new$raw, old$raw) &&
          identical(new$codes, old$codes)) return()
      up <- shiny::isolate(upload())
      if (inherits(up, "error")) up <- NULL
      settings(kits_settings_sync(old, new, up, shiny::isolate(reverse_rule())))
    })
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
        error = function(e) {
          # 只留最裡面那一句中文（不要 rlang 的「In index: 37. Caused by error」）
          while (inherits(e$parent, "condition")) e <- e$parent
          simpleError(conditionMessage(e))
        }
      )
      if (!inherits(up, "error")) up$name <- f$name
      upload(up)
      if (!inherits(up, "error")) {
        settings(kits_settings_apply(settings(), up, reverse = reverse_rule()))
      }
    })
    items <- shiny::reactive(kits_settings_items(settings()))
    counts <- shiny::reactive(kits_step2_counts(settings()))

    output$intro2 <- shiny::renderUI({
      shiny::tagList(
        shiny::tags$p(shiny::tags$strong(
          "先挑出哪些欄是題目；領域和計分在下一步的題目表設定，之後都還可以改。"
        )),
        shiny::tags$p(class = "kit-hint",
          "下面是看起來像題目的欄（分數是 0–20 的整數），欄名開頭一樣的放在一組。勾選要轉的組。")
      )
    })

    # 像題目的欄一組一列：組的結構只在欄變了時重算（勾選、領域改了不重畫）
    groups_all <- shiny::reactiveVal(kits_item_groups(kits_settings_empty(), list()))
    shiny::observe({
      s <- settings()
      g <- kits_item_groups(s, dfs())
      old <- shiny::isolate(groups_all())
      if (!identical(g$stem, old$stem) || !identical(g$cols, old$cols)) groups_all(g)
    })
    grp_page <- shiny::reactiveVal(1L)
    grp_shown <- shiny::reactive({
      g <- groups_all()
      g$k <- seq_len(nrow(g))
      kits_groups_filter(g, input$grp_search)
    })
    grp_pages <- shiny::reactive(max(1L, ceiling(nrow(grp_shown()) / KITS_GROUPS_PAGE)))
    shiny::observeEvent(input$grp_search, grp_page(1L), ignoreInit = TRUE)
    shiny::observeEvent(input$grp_prev, grp_page(max(1L, grp_page() - 1L)))
    shiny::observeEvent(input$grp_next, grp_page(min(grp_pages(), grp_page() + 1L)))
    grp_rows <- shiny::reactive({
      g <- grp_shown()
      p <- min(grp_page(), grp_pages())
      i <- seq_len(nrow(g))
      g[i > (p - 1) * KITS_GROUPS_PAGE & i <= p * KITS_GROUPS_PAGE, ]
    })
    output$grp_page <- shiny::renderText({
      paste0(kit_num(nrow(grp_shown())), " 組 · 第 ", min(grp_page(), grp_pages()), " / ",
             grp_pages(), " 頁")
    })
    # 勾選框的值是組名（不用列號：組的清單變了也不會對錯組）
    grp_sel_prev <- shiny::reactiveVal(character(0))
    group_on <- function(s, g) {
      g$stem[vapply(g$cols, function(c) all(s$include[match(c, s$column)]), logical(1))]
    }
    output$groups_table <- shiny::renderUI({
      g <- grp_rows()
      if (nrow(g) == 0) {
        return(shiny::tags$p(class = "kit-hint", if (nrow(groups_all()) == 0)
          "沒有看起來像題目的欄。可以上傳題目表，或用欄名挑（下面）。" else "沒有符合的組。"))
      }
      s <- shiny::isolate(settings())
      shiny::isolate(grp_sel_prev(group_on(s, g)))
      rows <- purrr::pmap(g, function(stem, cols, n, range, label, labels, n_in, domain, k) {
        on <- all(s$include[match(cols, s$column)])
        shiny::tags$tr(
          shiny::tags$td(shiny::tags$input(
            type = "checkbox", name = ns("grp_sel"), value = stem,
            checked = if (on) NA, `aria-label` = paste("要", stem)
          )),
          shiny::tags$td(shiny::tags$strong(kits_group_cols_text(cols)), shiny::tags$br(),
                         shiny::tags$small(class = "kit-hint", paste0("分數 ", range))),
          shiny::tags$td(
            class = "kit-group-label",
            if (nzchar(label)) label else shiny::tags$span(class = "kit-muted", "（沒有題目內容）"),
            # 一組不只一題：可以打開看每一題（同一組裡也可能混了別的概念）
            if (n > 1 && any(nzchar(labels))) shiny::tags$details(
              class = "kit-group-more",
              shiny::tags$summary(paste0("看全部 ", n, " 題")),
              shiny::tags$ol(lapply(seq_along(cols), function(j) shiny::tags$li(
                shiny::tags$code(cols[j]), " ", if (nzchar(labels[j])) labels[j] else "（沒有題目內容）"
              )))
            )
          )
        )
      })
      shiny::tags$div(
        id = ns("grp_sel"), class = "form-group shiny-input-checkboxgroup shiny-input-container",
        style = "width: auto; max-width: 100%;",
        shiny::tags$div(
          class = "shiny-options-group kit-table-scroll",
          shiny::tags$table(
            class = "table kit-groups",
            shiny::tags$thead(shiny::tags$tr(
              shiny::tags$th("要"), shiny::tags$th("欄"), shiny::tags$th("題目內容（第一題）")
            )),
            shiny::tags$tbody(rows)
          )
        )
      )
    })
    # 各波欄名不同、其實是同一份量表：勾選的組依題目順序合成同一組題號（bsrs1、bsrs2…）
    # 選單只在「納入了哪些組」變了才重畫（打題號時不會被打斷）
    included_groups <- shiny::reactiveVal(character(0))
    shiny::observe({
      g <- groups_all()
      s <- settings()
      on <- if (nrow(g) == 0) character(0) else group_on(s, g)
      if (!identical(on, shiny::isolate(included_groups()))) included_groups(on)
    })
    output$merge_ui <- shiny::renderUI({
      on <- included_groups()
      if (length(on) < 2) return(NULL)
      g <- shiny::isolate(groups_all())
      g <- g[g$stem %in% on, ]
      shiny::tags$div(
        class = "kit-merge",
        shiny::tags$p(shiny::tags$strong("各波欄名不同、其實是同一份量表？"),
                      "把每一波的那一組選進來，依題目順序合成同一組題號，kits 才能把各波連起來。"),
        shiny::tags$div(
          class = "kit-inline",
          shiny::selectizeInput(
            ns("merge_groups"), "要合成的組（每一波一組）", multiple = TRUE, width = "26rem",
            choices = stats::setNames(g$stem, paste0(g$stem, "（", g$n, " 題）")),
            selected = kits_merge_suggest(g)
          ),
          shiny::textInput(ns("merge_name"), "題號的開頭", width = "10rem", placeholder = "例如 bsrs"),
          shiny::actionButton(ns("merge_apply"), "合成同一組題號", class = "btn-default")
        ),
        shiny::tags$small(class = "kit-hint",
          "例如 w1d5_1、w2d5_1、w3c5_1 都會變成題號 bsrs1。已經合成的題號在「看挑出來的題目」裡看得到。")
      )
    })
    shiny::observeEvent(input$merge_apply, {
      out <- tryCatch(
        kits_merge_groups(settings(), groups_all(), input$merge_groups, input$merge_name),
        error = function(e) e
      )
      if (inherits(out, "error")) {
        shiny::showNotification(conditionMessage(out), type = "error")
        return()
      }
      settings(out)
      shiny::showNotification(paste0("已經合成 ", length(input$merge_groups), " 組的題號。"),
                              type = "message")
    })

    # 勾選：只改使用者這次勾或取消的組（和上一次的值比），其他組不動
    shiny::observeEvent(input$grp_sel, {
      g <- grp_rows()
      new <- input$grp_sel %||% character(0)
      prev <- grp_sel_prev()
      changed <- intersect(union(setdiff(new, prev), setdiff(prev, new)), g$stem)
      grp_sel_prev(new)
      if (length(changed) == 0) return()
      s <- settings()
      for (stem in changed) {
        cols <- g$cols[[match(stem, g$stem)]]
        s <- kits_bulk_include(s, cols, stem %in% new)
      }
      if (!identical(s, settings())) settings(s)
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
    # 設定在別的地方改了（題目表、用欄名挑、第 3 步的題目表）：把這一頁的勾選跟上
    shiny::observe({
      on <- group_on(settings(), grp_rows())
      if (!setequal(on, shiny::isolate(grp_sel_prev()))) {
        grp_sel_prev(on)
        shiny::updateCheckboxGroupInput(session, "grp_sel", selected = if (length(on)) on else character(0))
      }
    })
    # 用欄名挑
    picked <- shiny::reactive(kits_pick_cols(settings()$column, input$pick_prefix))
    output$pick_note <- shiny::renderUI({
      if (!nzchar(trimws(input$pick_prefix %||% ""))) return(NULL)
      shiny::tags$small(class = "kit-hint", paste0("符合 ", length(picked()), " 欄",
        if (length(picked()) > 0) paste0("：", paste(utils::head(picked(), 6), collapse = "、"),
                                        if (length(picked()) > 6) "…"), "。"))
    })
    shiny::observeEvent(input$pick_add, {
      rows <- picked()
      if (length(rows) == 0) {
        shiny::showNotification("沒有符合的欄：請填欄名的開頭，例如 w1d5_。", type = "warning")
        return()
      }
      settings(kits_bulk_include(settings(), rows, TRUE))
    })
    shiny::observeEvent(input$pick_clear, {
      settings(kits_bulk_include(settings(), settings()$column, FALSE))
    })
    shiny::observe({
      doms <- sort(unique(stats::na.omit(settings()$domain)))
      for (id in c("bulk_domain", "fix_domain")) {
        shiny::updateSelectizeInput(session, id, choices = doms,
                                    selected = shiny::isolate(input[[id]]))
      }
    })

    output$items_status <- shiny::renderUI({
      up <- upload()
      cnt <- counts()
      s <- settings()
      rename <- kits_item_rename(items())
      clash <- kits_rename_problems(dfs(), files$names(), items(), rename)
      up_line <- if (inherits(up, "error")) {
        kit_check("todo", "題目表讀不進來", conditionMessage(up))
      } else if (!is.null(up)) {
        miss <- setdiff(tolower(up$data$item_id), tolower(s$column))
        kit_check(
          if (cnt$n_cols > 0) "ok" else "todo",
          paste0(up$name %||% "題目表", "：", cnt$pick_text),
          paste0(
            if (up$type == "settings") "已套用題目表（納入、領域、計分對照）。",
            if (length(rename) > 0) paste0("各波欄名不同的同一題，用 item_id_out 合成一題。"),
            if (length(miss) > 0) paste0("題目表有 ", length(miss), " 題在資料裡找不到（",
                                         paste(utils::head(miss, 5), collapse = "、"),
                                         if (length(miss) > 5) "…", "）。")
          )
        )
      } else if (cnt$n_cols > 0) {
        kit_check("ok", cnt$pick_text, "")
      }
      shiny::tagList(
        if (!is.null(up_line)) kit_checklist(up_line),
        kit_problems(clash, bullets = FALSE),
        kitsdata_mixed_note(candidates()$mixed, files$names())
      )
    })
    output$items_list <- shiny::renderUI({
      it <- items()
      if (nrow(it) == 0) return(shiny::tags$p(class = "kit-hint", "還沒有挑題目。"))
      out_id <- ifelse(is.na(it$item_id_out), it$column, it$item_id_out)
      tab <- dplyr::summarise(
        dplyr::group_by(tibble::tibble(item = out_id, col = it$column), .data$item),
        cols = paste(.data$col, collapse = "、"), .groups = "drop"
      )
      shiny::tags$div(class = "kit-table-scroll", shiny::tags$table(
        class = "table",
        shiny::tags$thead(shiny::tags$tr(shiny::tags$th("題號"), shiny::tags$th("來自哪幾欄"))),
        shiny::tags$tbody(purrr::pmap(tab, function(item, cols) shiny::tags$tr(
          shiny::tags$td(shiny::tags$code(item)), shiny::tags$td(cols)
        )))
      ))
    })
    output$text_cols <- shiny::renderUI({
      tx <- candidates()$text
      shiny::tags$p(class = "kit-hint", if (length(tx) == 0) "沒有。" else paste0(
        "文字的欄不能當題目（", length(tx), " 欄）：", paste(utils::head(tx, 12), collapse = "、"),
        if (length(tx) > 12) "…", "。"
      ))
    })
    shiny::observeEvent(input$ok2, {
      if (counts()$n_cols == 0) {
        shiny::showNotification("還沒有挑題目：請上傳題目表，或用欄名挑。", type = "warning")
        return()
      }
      ok$s2 <- TRUE
      open(3L)
    })

    # 第 3 步：題目表：領域與計分 ----
    ack <- shiny::reactiveValues(codes = NULL, shift = NULL, shifted = 0, common = FALSE,
                                 dom_default = NULL)
    side_cells <- shiny::reactive({
      s <- step1()
      kits_code_cells(dfs(), unlist(kits_side_cols(s$map, s$per_file)), codes_2())
    })
    side_codes <- shiny::reactive({
      s <- step1()
      kits_side_codes(dfs(), kits_side_cols(s$map, s$per_file), codes_2())
    })
    checks <- shiny::reactive({
      kits_step3_checks(
        settings(), side_cells(), na_applied = identical(working$source(), "na"),
        ack = list(codes = ack$codes, shift = ack$shift, shifted = ack$shifted, common = ack$common,
                   dom_default = ack$dom_default),
        reverse_used = !is.null(upload()) && !inherits(upload(), "error"),
        side_codes = side_codes(),
        file_cols = lapply(dfs(), names)
      )
    })
    n_todo <- shiny::reactive(sum(vapply(checks(), function(x) x$state == "todo", logical(1))))

    output$checks <- shiny::renderUI({
      ch <- checks()
      if (length(ch) == 0) return(shiny::tags$p(class = "kit-hint", "還沒有挑題目。"))
      btn <- function(id, label) shiny::actionButton(ns(id), label, class = "btn-default btn-choice")
      actions <- list(
        codes = function(x) if (x$state == "todo") list(
          btn("goto_missing", "到「遺漏值處理」改掉（建議）"),
          btn("ack_codes", "它們是真的分數")
        ) else if (!is.null(ack$codes)) list(shiny::actionLink(ns("unack_codes"), "改回來")),
        shift = function(x) if (x$state == "todo") list(
          btn("do_shift", paste0("全部減 ", kits_id_text(kits_settings_shift_hint(settings())),
                                 "（建議）")),
          btn("ack_shift", "不用改")
        ) else if (!is.null(ack$shift)) list(shiny::actionLink(ns("unack_shift"), "改回來")),
        domain = function(x) if (x$state == "todo") list(
          shiny::selectizeInput(ns("fix_domain"), NULL, width = "12rem",
                                choices = sort(unique(stats::na.omit(settings()$domain))),
                                options = list(create = TRUE, placeholder = "選或輸入領域")),
          btn("do_fix_domain", "這幾欄都設成這個領域")
        ),
        dom_default = function(x) if (x$state == "todo") list(
          btn("go_dom_table", "在下面的題目表改領域"),
          btn("ack_dom", "這樣就好")
        ) else if (!is.null(ack$dom_default)) list(shiny::actionLink(ns("unack_dom"), "改回來")),
        frac = function(x) if (x$state == "todo") list(btn("do_drop_frac", "不要這幾欄（建議）")),
        common = function(x) if (x$state == "todo") list(
          btn("go_merge", "回第 2 步合成同一組題號（建議）"),
          btn("ack_common", "它們不是同一份量表")
        ) else if (isTRUE(ack$common)) list(shiny::actionLink(ns("unack_common"), "改回來"))
      )
      kit_checklist(lapply(ch, function(x) {
        kit_check(x$state, x$title, x$text, if (!is.null(actions[[x$id]])) actions[[x$id]](x))
      }))
    })
    shiny::observeEvent(input$ack_codes, {
      ch <- checks()$codes
      n <- kits_codes_total(settings(), side_cells())
      if (!is.null(ch)) ack$codes <- n
    })
    shiny::observeEvent(input$unack_codes, ack$codes <- NULL)
    shiny::observeEvent(input$ack_common, ack$common <- TRUE)
    # 領域用組名：「這樣就好」記下當時的欄與領域；之後領域改了，就要再決定一次
    shiny::observeEvent(input$ack_dom, {
      s <- settings()
      ack$dom_default <- kits_domain_default_key(s[s$include & lengths(s$raw) > 0, ])
    })
    shiny::observeEvent(input$unack_dom, ack$dom_default <- NULL)
    # 「在下面的題目表改領域」：捲到題目表，「對勾選的列」先選好「設領域」
    shiny::observeEvent(input$go_dom_table, {
      shiny::updateSelectInput(session, "bulk_action", selected = "domain")
      kit_scroll_to("item_table")
    })
    shiny::observeEvent(input$unack_common, ack$common <- FALSE)
    shiny::observeEvent(input$go_merge, if (ok$s1) open(2L))
    shiny::observeEvent(input$do_shift, {
      m <- kits_settings_shift_hint(settings())
      if (is.null(m)) return()
      settings(kits_bulk_shift(settings(), settings()$column[settings()$include], -m))
      ack$shifted <- m
    })
    shiny::observeEvent(input$ack_shift, ack$shift <- kits_settings_shift_hint(settings()))
    shiny::observeEvent(input$unack_shift, ack$shift <- NULL)
    shiny::observeEvent(input$do_fix_domain, {
      d <- trimws(input$fix_domain %||% "")
      if (!nzchar(d)) {
        shiny::showNotification("請先選或輸入領域，例如 distress。", type = "warning")
        return()
      }
      settings(kits_bulk_domain(settings(), kits_settings_undecided(settings()), d))
    })
    shiny::observeEvent(input$do_drop_frac, {
      s <- settings()
      frac <- s$include & purrr::map_lgl(s$raw, function(r) any(r != round(r)))
      settings(kits_bulk_include(s, s$column[frac], FALSE))
    })

    # 題目表：一頁 25 列；勾選存在伺服器（換頁不會不見）
    sel <- shiny::reactiveVal(character(0))
    page <- shiny::reactiveVal(1L)
    tbl_rows <- shiny::reactive({
      s <- settings()
      if (!isTRUE(input$tbl_all)) s <- s[s$include, ]
      s[s$column %in% kitsdata_filter_cols(s$column, input$tbl_search), ]
    })
    n_pages <- shiny::reactive(max(1L, ceiling(nrow(tbl_rows()) / KITS_ITEMS_PAGE)))
    shiny::observeEvent(list(input$tbl_search, input$tbl_all), page(1L), ignoreInit = TRUE)
    shiny::observeEvent(input$tbl_prev, page(max(1L, page() - 1L)))
    shiny::observeEvent(input$tbl_next, page(min(n_pages(), page() + 1L)))
    page_rows <- shiny::reactive({
      r <- tbl_rows()
      p <- min(page(), n_pages())
      r[seq_len(nrow(r)) > (p - 1) * KITS_ITEMS_PAGE & seq_len(nrow(r)) <= p * KITS_ITEMS_PAGE, ]
    })
    output$tbl_page <- shiny::renderText(paste0("第 ", min(page(), n_pages()), " / ", n_pages(), " 頁"))
    output$items_title <- shiny::renderText({
      cnt <- counts()
      dom <- cnt$by_domain
      paste0("題目表（", kit_num(cnt$n_cols), " 欄",
             if (length(dom) > 0) paste0(" · ", paste0(names(dom), " ", as.integer(dom), " 題",
                                                       collapse = "、")), "）")
    })
    # 題目表的下載（轉換前就可以下載；結果區也有一個，內容一樣）：按下載當下的設定
    table_csv <- function(file) {
      readr::write_excel_csv(kits_settings_export(settings()), file, na = "")
    }
    output$table_dl_ui <- shiny::renderUI(kit_download_button(
      session, "table_dl", "下載題目表（CSV）", KITS_SETTINGS_FILE, table_csv,
      class = "btn-default btn-sm"
    ))
    output$items_table <- shiny::renderUI({
      kits_items_page_html(ns("sel"), page_rows(), shiny::isolate(sel()))
    })
    shiny::observeEvent(input$sel, {
      here <- page_rows()$column
      sel(union(setdiff(sel(), here), intersect(input$sel %||% character(0), here)))
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
    set_sel <- function(rows) {
      sel(rows)
      shiny::updateCheckboxGroupInput(session, "sel", selected = intersect(rows, page_rows()$column))
    }
    shiny::observeEvent(input$tbl_sel_all, set_sel(tbl_rows()$column))
    shiny::observeEvent(input$tbl_sel_none, set_sel(character(0)))
    output$sel_count <- shiny::renderText(paste0("已勾選 ", length(intersect(sel(), settings()$column)), " 列"))

    shiny::observeEvent(input$bulk_apply, {
      rows <- intersect(sel(), settings()$column)
      if (length(rows) == 0) {
        shiny::showNotification("請先在表裡勾選要改的列。", type = "warning")
        return()
      }
      s <- settings()
      out <- tryCatch(switch(
        input$bulk_action %||% "domain",
        domain = {
          d <- trimws(input$bulk_domain %||% "")
          if (!nzchar(d)) kit_abort("請先選或輸入領域，例如 lan。")
          kits_bulk_domain(s, rows, d)
        },
        shift = {
          by <- input$bulk_shift
          if (is.null(by) || is.na(by) || by != round(by) || by == 0) {
            kit_abort("加減要是不等於 0 的整數，例如 -1。")
          }
          kits_bulk_shift(s, rows, by)
        },
        reverse = {
          txt <- trimws(input$bulk_max %||% "")
          top <- if (nzchar(txt)) suppressWarnings(as.numeric(txt)) else NA_real_
          if (nzchar(txt) && is.na(top)) kit_abort("最高分要是數字，或留空白（用各題目前的最高分）。")
          kits_bulk_reverse(s, rows, top)
        },
        merge = {
          from <- kits_parse_values(input$merge_from)
          to <- input$merge_to
          if (length(from) == 0) kit_abort("請填要合併的原分數，例如 4、5。")
          if (is.null(to) || is.na(to)) kit_abort("請填合併後的計分，例如 3。")
          kits_bulk_merge(s, rows, from, to)
        },
        restore = kits_bulk_restore(s, rows),
        exclude = kits_bulk_include(s, rows, FALSE),
        include = kits_bulk_include(s, rows, TRUE),
        out_prefix = kits_bulk_out_prefix(s, rows, trimws(input$out_pattern %||% "")),
        out_reset = kits_bulk_out_reset(s, rows)
      ), error = function(e) e)
      if (inherits(out, "error")) {
        shiny::showNotification(conditionMessage(out), type = "error")
      } else {
        settings(out)
      }
    })

    # 步驟標頭 ----
    # 畫法是 kitDesign 的 kit_step_head()；哪一步打開的規則留在這裡（第 3 步沒有「對，下一步」、
    # 轉換完全部收起、第 1 步要先過檢查），所以不用 kit_stepcards_server()
    head_ui <- function(k, title, summary) {
      state <- if (open() == k) "current" else if (k == 1 && ok$s1 || k == 2 && ok$s2 ||
                                                  k == 3 && open() == 0L) "done" else "locked"
      can_edit <- state == "done" && (k == 1 || ok$s1 && (k == 2 || ok$s2))
      kit_step_head(k, title, state, summary = summary,
                    edit_id = if (can_edit) ns(paste0("edit", k)),
                    title_id = ns(paste0("steptitle", k)))
    }
    output$head1 <- shiny::renderUI(head_ui(
      1, "每個檔案是哪個世代、哪個波次",
      kits_step1_summary(cells(), length(files$keys()), wave_unit(), step1())
    ))
    output$head2 <- shiny::renderUI(head_ui(2, "哪些欄是題目", counts()$pick_text))
    output$head3 <- shiny::renderUI(head_ui(3, "題目表：領域與計分", kits_step3_summary(checks())))

    # 轉換 ----
    spec <- shiny::reactive({
      s <- step1()
      kits_spec(s$cohort, s$map, items(), kits_settings_undecided(settings()), s$typed,
                codes_2(), s$per_file, input$value_labels %||% "code")
    })
    missing_steps <- shiny::reactive({
      c(if (!ok$s1) "第 1 步確認",
        if (!ok$s2) (if (counts()$n_cols > 0) "第 2 步確認" else "第 2 步挑題目"),
        if (ok$s1 && ok$s2 && n_todo() > 0) paste0("第 3 步有 ", n_todo(), " 件事要決定"))
    })
    output$ready <- shiny::renderUI({
      if (!is.null(result())) return(NULL)
      m <- missing_steps()
      shiny::tags$p(
        class = paste("kit-ready", if (length(m) > 0) "todo"),
        if (length(m) > 0) paste0("還差：", paste(m, collapse = "、"), "。") else "三步都好了。"
      )
    })
    output$run_ui <- shiny::renderUI({
      if (!is.null(result())) return(NULL)
      b <- shiny::actionButton(ns("run"), "轉換成作答檔", class = "btn-primary")
      if (length(missing_steps()) > 0) b <- htmltools::tagAppendAttributes(b, disabled = NA)
      b
    })

    # 主要按鈕、狀態框、通知、結果過期：kit_run_tab_server()（「還差」與按鈕是上面這一頁自己的）
    tab <- kit_run_tab_server(
      input, output, session, "轉換成作答檔",
      stale_on = function() list(spec(), dfs()),
      guard = function() {
        m <- missing_steps()
        if (length(m) > 0) kit_status("error", paste0("還不能轉換：", paste(m, collapse = "、"), "。"))
      },
      progress = "正在轉換…",
      run = function() {
        problems <- step1()$problems
        if (length(problems) > 0) return(kit_status("error", "第 1 步還有問題：", warnings = problems))
        out <- kits_build(dfs(), files$names(), spec())
        if (length(out$problems) > 0) {
          return(kit_status(
            "error",
            paste0("有 ", length(out$problems), " 個問題要先處理，處理後再按一次「轉換成作答檔」："),
            warnings = out$problems, notes = out$notes
          ))
        }
        # 長格式不在這裡做出來：下載時才一段一段寫進 .csv.gz（省記憶體）
        out
      },
      done = function(out) {
        open(0L)
        kit_done_status(
          paste0(
            "作答檔 ", kit_num(out$n_rows), " 列：",
            kit_num(out$n_visits), " 個列次、", out$n_items, " 題。"
          ),
          # 第 3 步已經按了「它們是真的分數」：遺漏值代碼不用再提醒一次
          warnings = if (!is.null(ack$codes)) {
            out$warnings[!grepl("題有疑似遺漏值代碼", out$warnings, fixed = TRUE)]
          } else out$warnings,
          notes = out$notes
        )
      },
      error = function(e) kit_error_status(e, "轉換", kit_upload_todo(ns, length(files$keys()))),
      result_ui = function(res) {
        kits_name <- timestamped_name("kits_long")
        shiny::tagList(
          shiny::tags$div(
            class = "kit-downloads",
            kit_download_button(
              session, "download_gz", "下載作答檔（.csv.gz）", kits_name("csv.gz"),
              function(file) kitsdata_download(res$prep, file, write_kits_long_gz),
              class = "btn-primary"
            ),
            kit_kitsui_link("開啟 kitsUI，上傳這個檔 →", class = "kit-next-link")
          ),
          shiny::h5("每個世代、每個領域有幾列"),
          kit_table_scroll(shiny::tableOutput(ns("summary"))),
          kit_details_row(
            kit_detail(
              "作答檔預覽",
              shiny::tags$small(class = "kit-hint", paste0("前 ", KIT_PREVIEW_ROWS * 2, " 列。")),
              shiny::tags$div(class = "kit-table-scroll", shiny::tableOutput(ns("preview")))
            ),
            kit_detail(
              "這次的題目表（下次在第 2 步上傳）",
              shiny::tags$p(class = "kit-hint",
                "下一波在第 2 步上傳這個檔，就會套用同樣的題目、領域與計分對照。"),
              shiny::uiOutput(ns("settings_dl_ui"), inline = TRUE)
            ),
            kit_detail(
              "其他下載",
              kit_download_button(
                session, "download_csv", "下載未壓縮的 CSV", kits_name("csv"),
                function(file) kitsdata_download(res$prep, file, write_kits_long_csv),
                class = "btn-default btn-sm"
              ),
              shiny::tags$small(
                class = "kit-hint",
                paste0(" 未壓縮約 ", kits_size_text(res$csv_mb * 1024^2), "。"),
                "kitsUI 可以直接讀 .csv.gz（單檔上限 ", KITSUI_UPLOAD_MB, " MB）。"
              )
            ),
            kit_detail("kits 作答檔的格式", kitsdata_spec_ui()),
            kit_code_panel(ns("code"))
          )
        )
      },
      code = function() {
        text_fix <- kits_text_fix(dfs(), items()$column)
        list(tidy = code_kitsdata(
          code_files_from(files), spec(), na_applied = identical(working$source(), "na"),
          text_fix = text_fix
        ), base = NULL)
      },
      code_note = KIT_CODE_NOTE_JOIN
    )
    result <- tab$result

    output$summary <- shiny::renderTable(result()$summary, digits = 0, format.args = list(big.mark = ","))
    output$preview <- shiny::renderTable(
      kit_preview_table(result()$preview, n = KIT_PREVIEW_ROWS * 2),
      striped = TRUE, spacing = "s"
    )
    # 檔名固定；內容是按下載當下的設定
    output$settings_dl_ui <- shiny::renderUI(kit_download_button(
      session, "settings_dl", "下載題目表（CSV）", KITS_SETTINGS_FILE, table_csv,
      class = "btn-default btn-sm"
    ))

    # 這一頁的狀態（步驟分頁打 ✓ 用）
    list(state = tab$state)
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

#' kits 作答檔的欄位表（欄位、必填、意思），放在結果下面的「kits 作答檔的格式」
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
  shiny::tagList(
    shiny::tags$p(class = "kit-hint", "作答檔固定 9 個欄位（名稱、順序、型別都固定）："),
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
