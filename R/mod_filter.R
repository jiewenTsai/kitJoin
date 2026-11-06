# 篩選與摘要
#
# 上半是篩選（保留哪些欄、哪些列、排序），結果往後傳給串接、疊加等分頁；
# 下半是分組摘要（人數、平均數…），只用來看，不往後傳。
# 每個檔案各自設定：輸入的 id 後面接檔案代號（keep_cols_f1），條件一列一個編號（c3_col）。
# 換檔案時畫面重畫，初始值從 input 拿（isolate），所以每個檔案的設定都留著。

# 選欄框的標題（跟著模式換）
FILTER_KEEP_LABEL <- c(
  # 原文：要保留的欄（點選；沒挑＝全部保留）
  include = "\u8981\u4fdd\u7559\u7684\u6b04\uff08\u9ede\u9078\uff1b\u6c92\u6311\uff1d\u5168\u90e8\u4fdd\u7559\uff09",
  # 原文：要拿掉的欄（點選；其餘全部保留）
  exclude = "\u8981\u62ff\u6389\u7684\u6b04\uff08\u9ede\u9078\uff1b\u5176\u9918\u5168\u90e8\u4fdd\u7559\uff09"
)

# 摘要的表在畫面上最多列幾列（下載的檔案是全部）
SUMMARY_SHOW_ROWS <- 500L

# 篩選與摘要的頁面：骨架是 kitDesign::kit_run_tab_ui()，下面另外一塊是摘要
mod_filter_ui <- function(id) {
  ns <- shiny::NS(id)
  section <- function(title) shiny::tags$h5(class = "mt-3", title)
  shiny::tagList(
    kit_run_tab_ui(
      # 原文：執行篩選
      id, "\u57f7\u884c\u7be9\u9078",
      guide = kit_tab_guide(
        goal = paste0(
          # 原文：依條件留下需要的列與欄（例如只留男生、只留有語言分數的人），也可以排序；
          "\u4f9d\u689d\u4ef6\u7559\u4e0b\u9700\u8981\u7684\u5217\u8207\u6b04\uff08\u4f8b\u5982\u53ea\u7559\u7537\u751f\u3001\u53ea\u7559\u6709\u8a9e\u8a00\u5206\u6578\u7684\u4eba\uff09\uff0c\u4e5f\u53ef\u4ee5\u6392\u5e8f\uff1b",
          # 原文：下面還可以看各組的人數、平均數。這一步是選用的。
          "\u4e0b\u9762\u9084\u53ef\u4ee5\u770b\u5404\u7d44\u7684\u4eba\u6578\u3001\u5e73\u5747\u6578\u3002\u9019\u4e00\u6b65\u662f\u9078\u7528\u7684\u3002"
        ),
        steps = kit_steps(
          # 原文：有好幾個檔案時，先選要設定的檔案（每個檔案各自設定；沒設定的檔案原樣往後傳）
          "\u6709\u597d\u5e7e\u500b\u6a94\u6848\u6642\uff0c\u5148\u9078\u8981\u8a2d\u5b9a\u7684\u6a94\u6848\uff08\u6bcf\u500b\u6a94\u6848\u5404\u81ea\u8a2d\u5b9a\uff1b\u6c92\u8a2d\u5b9a\u7684\u6a94\u6848\u539f\u6a23\u5f80\u5f8c\u50b3\uff09",
          # 原文：選要保留的欄（不選＝全部保留）
          "\u9078\u8981\u4fdd\u7559\u7684\u6b04\uff08\u4e0d\u9078\uff1d\u5168\u90e8\u4fdd\u7559\uff09",
          # 原文：按「加一個條件」：選欄 → 選比較方式 → 選或填值
          "\u6309\u300c\u52a0\u4e00\u500b\u689d\u4ef6\u300d\uff1a\u9078\u6b04 \u2192 \u9078\u6bd4\u8f03\u65b9\u5f0f \u2192 \u9078\u6216\u586b\u503c",
          # 原文：（選用）選排序的欄
          "\uff08\u9078\u7528\uff09\u9078\u6392\u5e8f\u7684\u6b04",
          # 原文：按「執行篩選」，看每個檔案留下幾列、有幾列是因為空白而排除
          "\u6309\u300c\u57f7\u884c\u7be9\u9078\u300d\uff0c\u770b\u6bcf\u500b\u6a94\u6848\u7559\u4e0b\u5e7e\u5217\u3001\u6709\u5e7e\u5217\u662f\u56e0\u70ba\u7a7a\u767d\u800c\u6392\u9664",
          # 原文：最下面的「摘要」可以算各組的人數、平均數；摘要只用來看，不會傳到後面的分頁
          "\u6700\u4e0b\u9762\u7684\u300c\u6458\u8981\u300d\u53ef\u4ee5\u7b97\u5404\u7d44\u7684\u4eba\u6578\u3001\u5e73\u5747\u6578\uff1b\u6458\u8981\u53ea\u7528\u4f86\u770b\uff0c\u4e0d\u6703\u50b3\u5230\u5f8c\u9762\u7684\u5206\u9801"
        ),
        output = paste0(
          # 原文：每個檔案各自篩選。後面的分頁（串接、疊加、流失樣本、kits 作答檔、長/寬格式轉換）
          "\u6bcf\u500b\u6a94\u6848\u5404\u81ea\u7be9\u9078\u3002\u5f8c\u9762\u7684\u5206\u9801\uff08\u4e32\u63a5\u3001\u758a\u52a0\u3001\u6d41\u5931\u6a23\u672c\u3001kits \u4f5c\u7b54\u6a94\u3001\u9577/\u5bec\u683c\u5f0f\u8f49\u63db\uff09",
          # 原文：會用篩選後的資料。也可以直接下載篩選後的檔案。
          "\u6703\u7528\u7be9\u9078\u5f8c\u7684\u8cc7\u6599\u3002\u4e5f\u53ef\u4ee5\u76f4\u63a5\u4e0b\u8f09\u7be9\u9078\u5f8c\u7684\u6a94\u6848\u3002"
        ),
        note = paste0(
          # 原文：條件用到的欄是空白時，「大於」「等於」這類比較判斷不了，那一列不會留下；
          "\u689d\u4ef6\u7528\u5230\u7684\u6b04\u662f\u7a7a\u767d\u6642\uff0c\u300c\u5927\u65bc\u300d\u300c\u7b49\u65bc\u300d\u9019\u985e\u6bd4\u8f03\u5224\u65b7\u4e0d\u4e86\uff0c\u90a3\u4e00\u5217\u4e0d\u6703\u7559\u4e0b\uff1b",
          # 原文：狀態框會寫出有幾列是這樣排除的。要留下空白的列，加一個「是空白」的條件，
          "\u72c0\u614b\u6846\u6703\u5beb\u51fa\u6709\u5e7e\u5217\u662f\u9019\u6a23\u6392\u9664\u7684\u3002\u8981\u7559\u4e0b\u7a7a\u767d\u7684\u5217\uff0c\u52a0\u4e00\u500b\u300c\u662f\u7a7a\u767d\u300d\u7684\u689d\u4ef6\uff0c",
          # 原文：再選「任一條件符合」。
          "\u518d\u9078\u300c\u4efb\u4e00\u689d\u4ef6\u7b26\u5408\u300d\u3002"
        ),
        technical = paste0(
          # 原文：照 dplyr 的順序：filter() 篩列（全部符合用逗號，任一符合用 |；條件算出 NA 的列不留下），
          "\u7167 dplyr \u7684\u9806\u5e8f\uff1afilter() \u7be9\u5217\uff08\u5168\u90e8\u7b26\u5408\u7528\u9017\u865f\uff0c\u4efb\u4e00\u7b26\u5408\u7528 |\uff1b\u689d\u4ef6\u7b97\u51fa NA \u7684\u5217\u4e0d\u7559\u4e0b\uff09\uff0c",
          # 原文：arrange() 排序（desc() 是由大到小，空白排最後），select() 選欄。
          "arrange() \u6392\u5e8f\uff08desc() \u662f\u7531\u5927\u5230\u5c0f\uff0c\u7a7a\u767d\u6392\u6700\u5f8c\uff09\uff0cselect() \u9078\u6b04\u3002",
          # 原文：「包含文字」用 grepl(fixed = TRUE)，照字面比對，不是正規表示式。
          "\u300c\u5305\u542b\u6587\u5b57\u300d\u7528 grepl(fixed = TRUE)\uff0c\u7167\u5b57\u9762\u6bd4\u5c0d\uff0c\u4e0d\u662f\u6b63\u898f\u8868\u793a\u5f0f\u3002",
          # 原文：「是其中之一」用 %in%：空白不屬於任何一個值，所以「不是其中之一」會留下空白的列。
          "\u300c\u662f\u5176\u4e2d\u4e4b\u4e00\u300d\u7528 %in%\uff1a\u7a7a\u767d\u4e0d\u5c6c\u65bc\u4efb\u4f55\u4e00\u500b\u503c\uff0c\u6240\u4ee5\u300c\u4e0d\u662f\u5176\u4e2d\u4e4b\u4e00\u300d\u6703\u7559\u4e0b\u7a7a\u767d\u7684\u5217\u3002",
          # 原文：SPSS 的數值標籤、變項標籤會保留。摘要用 group_by() 與 summarise()，na.rm = TRUE。
          "SPSS \u7684\u6578\u503c\u6a19\u7c64\u3001\u8b8a\u9805\u6a19\u7c64\u6703\u4fdd\u7559\u3002\u6458\u8981\u7528 group_by() \u8207 summarise()\uff0cna.rm = TRUE\u3002"
        ),
        diagram = filter_diagram()
      ),
      shiny::uiOutput(ns("source_note")),
      shiny::uiOutput(ns("file_ui")),
      shiny::uiOutput(ns("file_note")),
      # 原文：1. 保留哪些欄
      section("1. \u4fdd\u7559\u54ea\u4e9b\u6b04"),
      shiny::uiOutput(ns("keep_ui")),
      shiny::uiOutput(ns("keep_quick")),
      shiny::uiOutput(ns("keep_count")),
      # 原文：2. 保留哪些列
      section("2. \u4fdd\u7559\u54ea\u4e9b\u5217"),
      shiny::uiOutput(ns("conds_hint")),
      # 條件一列一列用 insertUI() 加進各檔案的區塊（加、拿掉條件時，其他條件不重畫，選好的值不會不見）
      shiny::tags$div(id = ns("conds_area")),
      # 原文：加一個條件
      shiny::tags$div(class = "mb-2", kit_button(ns("add_cond"), "\u52a0\u4e00\u500b\u689d\u4ef6", class = "btn-sm")),
      shiny::uiOutput(ns("combine_ui")),
      # 原文：3. 排序（選用）
      section("3. \u6392\u5e8f\uff08\u9078\u7528\uff09"),
      shiny::uiOutput(ns("sort_ui")),
      shiny::checkboxInput(
        # 原文：後面的分頁使用篩選後的資料
        ns("use_downstream"), "\u5f8c\u9762\u7684\u5206\u9801\u4f7f\u7528\u7be9\u9078\u5f8c\u7684\u8cc7\u6599", value = TRUE, width = "100%"
      ),
      result = TRUE
    ),
    filter_summary_ui(ns)
  )
}

# 摘要那一塊：分組欄、變項、統計量、計算按鈕、結果
filter_summary_ui <- function(ns) {
  shiny::tags$div(
    class = "kit-filter-summary mt-4 pt-3 border-top",
    # 原文：摘要：各組的人數與統計量
    shiny::tags$h4("\u6458\u8981\uff1a\u5404\u7d44\u7684\u4eba\u6578\u8207\u7d71\u8a08\u91cf"),
    shiny::tags$p(
      class = "kit-hint",
      # 原文：用上面選的檔案。摘要只用來看，不會改資料，也不會傳到後面的分頁。
      "\u7528\u4e0a\u9762\u9078\u7684\u6a94\u6848\u3002\u6458\u8981\u53ea\u7528\u4f86\u770b\uff0c\u4e0d\u6703\u6539\u8cc7\u6599\uff0c\u4e5f\u4e0d\u6703\u50b3\u5230\u5f8c\u9762\u7684\u5206\u9801\u3002"
    ),
    shiny::uiOutput(ns("sum_source")),
    shiny::selectizeInput(
      # 原文：分組欄（可以不選；選好幾欄就是交叉分組）
      ns("sum_groups"), "\u5206\u7d44\u6b04\uff08\u53ef\u4ee5\u4e0d\u9078\uff1b\u9078\u597d\u5e7e\u6b04\u5c31\u662f\u4ea4\u53c9\u5206\u7d44\uff09",
      choices = NULL, multiple = TRUE, width = "100%",
      # 原文：不分組
      options = list(placeholder = "\u4e0d\u5206\u7d44", plugins = list("remove_button"))
    ),
    shiny::selectizeInput(
      # 原文：要摘要的變項（數值欄，可多選）
      ns("sum_vars"), "\u8981\u6458\u8981\u7684\u8b8a\u9805\uff08\u6578\u503c\u6b04\uff0c\u53ef\u591a\u9078\uff09",
      choices = NULL, multiple = TRUE, width = "100%",
      # 原文：點這裡選變項
      options = list(placeholder = "\u9ede\u9019\u88e1\u9078\u8b8a\u9805", plugins = list("remove_button"))
    ),
    shiny::uiOutput(ns("sum_quick")),
    shiny::checkboxGroupInput(
      # 原文：統計量（人數 n 一定會算）
      ns("sum_stats"), "\u7d71\u8a08\u91cf\uff08\u4eba\u6578 n \u4e00\u5b9a\u6703\u7b97\uff09",
      choices = stats::setNames(names(SUMMARY_STATS), SUMMARY_STATS),
      selected = SUMMARY_STATS_DEFAULT, inline = TRUE, width = "100%"
    ),
    # 原文：計算摘要
    kit_button(ns("sum_run"), "\u8a08\u7b97\u6458\u8981"),
    shiny::uiOutput(ns("sum_result"))
  )
}

# 篩選與摘要。files：mod_upload_server()；working：mod_missing_server()；goto：NULL 或
# function(tab)。回傳和 working 一樣的 list（data、source、code_cells、state、result、code），
# 另外有 filtered()：後面的分頁拿到的是不是篩選後的資料；filter_state()："on"、"stale"、"off"
mod_filter_server <- function(id, files, working, goto = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    if (!is.null(goto)) watch_goto_links(input, goto)
    fid <- function(name, key) paste0(name, "_", key)
    cid <- function(n, name) paste0("c", n, "_", name)

    output$source_note <- shiny::renderUI({
      reshape_source_note_ui(length(files$keys()), working$source(), working$code_cells(), ns)
    })

    # 各檔資料，名稱是檔案代號
    dfs <- shiny::reactive({
      keys <- files$keys()
      d <- working$data()
      if (length(d) != length(keys)) return(list())
      stats::setNames(d, keys)
    })
    shown_names <- shiny::reactive(file_display_name(files$labels(), files$names()))
    shown_of <- function(keys) shown_names()[match(keys, files$keys())]
    # 各檔欄名：沒變就不重畫設定
    cols_rv <- shiny::reactiveVal(list())
    shiny::observe({
      cols <- purrr::map(dfs(), names)
      if (!identical(cols, shiny::isolate(cols_rv()))) cols_rv(cols)
    })
    cur <- shiny::reactive({
      keys <- files$keys()
      if (length(keys) == 0) return(NULL)
      if (isTRUE(input$file %in% keys)) input$file else keys[1]
    })
    cur_cols <- shiny::reactive({
      key <- cur()
      if (is.null(key)) character(0) else cols_rv()[[key]] %||% character(0)
    })

    output$file_ui <- shiny::renderUI({
      keys <- files$keys()
      if (length(keys) < 2) return(NULL)
      shiny::selectInput(
        # 原文：要設定的檔案（每個檔案各自設定）
        ns("file"), "\u8981\u8a2d\u5b9a\u7684\u6a94\u6848\uff08\u6bcf\u500b\u6a94\u6848\u5404\u81ea\u8a2d\u5b9a\uff09",
        choices = stats::setNames(keys, shown_names()), selected = shiny::isolate(cur())
      )
    })

    # ── 1. 保留哪些欄 ──────────────────────────────────────────────────────────
    output$keep_ui <- shiny::renderUI({
      key <- cur()
      if (is.null(key)) {
        # 原文：先到「檔案上傳」加入檔案。
        return(shiny::tags$p(class = "kit-hint", "\u5148\u5230\u300c\u6a94\u6848\u4e0a\u50b3\u300d\u52a0\u5165\u6a94\u6848\u3002"))
      }
      mode <- shiny::isolate(input[[fid("keep_mode", key)]]) %||% "include"
      shiny::tagList(
        shiny::radioButtons(
          ns(fid("keep_mode", key)), NULL,
          # 原文：挑要保留的欄 | 挑要拿掉的欄
          choices = c("\u6311\u8981\u4fdd\u7559\u7684\u6b04" = "include", "\u6311\u8981\u62ff\u6389\u7684\u6b04" = "exclude"),
          selected = mode, inline = TRUE
        ),
        shiny::selectizeInput(
          ns(fid("keep_cols", key)), FILTER_KEEP_LABEL[[mode]], choices = cur_cols(),
          selected = shiny::isolate(input[[fid("keep_cols", key)]]),
          multiple = TRUE, width = "100%",
          # 原文：沒挑＝全部保留
          options = list(placeholder = "\u6c92\u6311\uff1d\u5168\u90e8\u4fdd\u7559", plugins = list("remove_button"))
        )
      )
    })
    output$keep_quick <- shiny::renderUI({
      if (is.null(cur())) return(NULL)
      pivot_quick_ui(ns("keep_pick"), pivot_name_groups(cur_cols()))
    })
    output$keep_count <- shiny::renderUI({
      key <- cur()
      if (is.null(key)) return(NULL)
      shiny::tags$div(class = "small mb-3", filter_keep_count_text(
        cur_cols(), input[[fid("keep_mode", key)]], input[[fid("keep_cols", key)]]
      ))
    })
    # 快速選欄：全選、清除、加入一組（和寬轉長的選欄一樣）
    shiny::observeEvent(input$keep_pick, {
      key <- cur()
      if (is.null(key)) return()
      id <- fid("keep_cols", key)
      sel <- pivot_quick_pick(
        input$keep_pick, input[[id]], cur_cols(), pivot_name_groups(cur_cols())
      )
      shiny::updateSelectizeInput(session, id, selected = sel)
    })
    # 換模式：標題跟著換；已挑的欄換成另一半，保留的欄不變
    last_mode <- new.env()
    shiny::observe({
      key <- cur()
      mode <- if (is.null(key)) NULL else input[[fid("keep_mode", key)]]
      shiny::isolate({
        prev <- if (is.null(key)) NULL else get0(key, envir = last_mode, inherits = FALSE)
        if (!is.null(mode) && !identical(mode, prev %||% "include")) {
          assign(key, mode, envir = last_mode)
          now <- input[[fid("keep_cols", key)]] %||% character(0)
          flip <- if (length(now) > 0) pivot_order_cols(setdiff(cur_cols(), now), cur_cols())
          shiny::updateSelectizeInput(
            session, fid("keep_cols", key), label = FILTER_KEEP_LABEL[[mode]],
            selected = flip %||% character(0)
          )
        }
      })
    })

    # ── 2. 保留哪些列 ──────────────────────────────────────────────────────────
    # 每個檔案的條件編號（檔案代號 → 編號）；編號不重複，所以各檔的輸入不會混在一起
    cond_ids <- shiny::reactiveVal(list())
    counter <- 0L
    shiny::observeEvent(input$add_cond, {
      key <- cur()
      if (is.null(key)) return()
      counter <<- counter + 1L
      n <- counter
      ids <- cond_ids()
      ids[[key]] <- c(ids[[key]], n)
      cond_ids(ids)
      shiny::insertUI(
        paste0("#", ns(fid("conds", key))), "beforeEnd", cond_row_ui(n, cur_cols())
      )
      make_cond_row(n, key)
    })

    # 一個條件的比較方式與值（跟著欄的種類換）、拿掉按鈕
    make_cond_row <- function(n, key) {
      col_x <- function() {
        col <- input[[cid(n, "col")]] %||% ""
        df <- dfs()[[key]]
        if (!nzchar(col) || is.null(df) || !col %in% names(df)) return(NULL)
        df[[col]]
      }
      op_of <- function(x, ops) {
        op <- input[[cid(n, "op")]]
        if (isTRUE(op %in% ops)) op else unname(ops[1])
      }
      output[[cid(n, "op_ui")]] <- shiny::renderUI({
        x <- col_x()
        # 原文：先選欄
        if (is.null(x)) return(shiny::tags$span(class = "kit-hint d-inline-block pt-2", "\u5148\u9078\u6b04"))
        ops <- filter_ops_for(filter_col_kind(x), many = is.null(filter_value_choices(x)))
        shiny::selectInput(
          ns(cid(n, "op")), NULL, choices = ops,
          selected = shiny::isolate(op_of(x, ops)), width = "13rem"
        )
      })
      output[[cid(n, "value_ui")]] <- shiny::renderUI({
        x <- col_x()
        if (is.null(x)) return(NULL)
        op <- op_of(x, filter_ops_for(filter_col_kind(x)))
        now <- shiny::isolate(list(
          num = input[[cid(n, "num")]], lo = input[[cid(n, "lo")]], hi = input[[cid(n, "hi")]],
          values = input[[cid(n, "values")]], text = input[[cid(n, "text")]]
        ))
        filter_value_ui(function(name) ns(cid(n, name)), op, filter_value_choices(x), now)
      })
      shiny::observeEvent(input[[cid(n, "rm")]], {
        ids <- cond_ids()
        ids[[key]] <- setdiff(ids[[key]], n)
        cond_ids(ids)
        shiny::removeUI(paste0("#", ns(cid(n, "row"))))
      }, ignoreInit = TRUE, once = TRUE)
    }

    # 各檔案的條件區塊：第一次看到這個檔案時加進畫面，之後只顯示或藏起來（不重畫）
    output$cur_file <- shiny::renderText(cur() %||% "")
    shiny::outputOptions(output, "cur_file", suspendWhenHidden = FALSE)
    made <- character(0)
    shiny::observe({
      key <- cur()
      if (is.null(key) || key %in% made) return()
      made <<- c(made, key)
      shiny::insertUI(
        paste0("#", ns("conds_area")), "beforeEnd",
        shiny::conditionalPanel(
          sprintf("output['%s'] == '%s'", ns("cur_file"), key),
          shiny::tags$div(id = ns(fid("conds", key)))
        )
      )
    })
    # 一個條件的一列：欄、比較方式、值、拿掉
    cond_row_ui <- function(n, cols) {
      shiny::tags$div(
        id = ns(cid(n, "row")),
        class = "kit-filter-cond d-flex flex-wrap align-items-start gap-2",
        shiny::selectizeInput(
          # 原文：（選欄）
          # 原文：（選欄）
          ns(cid(n, "col")), NULL, choices = c("\uff08\u9078\u6b04\uff09" = "", cols), selected = "",
          width = "14rem"
        ),
        shiny::uiOutput(ns(cid(n, "op_ui"))),
        shiny::uiOutput(ns(cid(n, "value_ui"))),
        # 原文：拿掉
        # 原文：拿掉
        kit_button(ns(cid(n, "rm")), "\u62ff\u6389", class = "btn-sm")
      )
    }
    # 欄名改了（例如 ② 重新執行）：各條件的選欄跟著換，還在的欄保留
    shiny::observeEvent(cols_rv(), {
      ids <- cond_ids()
      for (key in names(ids)) {
        cols <- cols_rv()[[key]] %||% character(0)
        for (n in ids[[key]]) {
          col <- input[[cid(n, "col")]] %||% ""
          shiny::updateSelectizeInput(
            # 原文：（選欄）
            # 原文：（選欄）
            session, cid(n, "col"), choices = c("\uff08\u9078\u6b04\uff09" = "", cols),
            selected = if (col %in% cols) col else ""
          )
        }
      }
    }, ignoreInit = TRUE)
    output$conds_hint <- shiny::renderUI({
      key <- cur()
      if (is.null(key) || length(cond_ids()[[key]] %||% integer(0)) > 0) return(NULL)
      # 原文：還沒有條件：全部的列都保留。
      # 原文：還沒有條件：全部的列都保留。
      shiny::tags$p(class = "kit-hint", "\u9084\u6c92\u6709\u689d\u4ef6\uff1a\u5168\u90e8\u7684\u5217\u90fd\u4fdd\u7559\u3002")
    })
    output$combine_ui <- shiny::renderUI({
      key <- cur()
      if (is.null(key) || length(cond_ids()[[key]]) < 2) return(NULL)
      shiny::radioButtons(
        # 原文：條件怎麼合起來
        ns(fid("combine", key)), "\u689d\u4ef6\u600e\u9ebc\u5408\u8d77\u4f86",
        # 原文：全部條件都符合（而且） | 任一條件符合（或）
        choices = c("\u5168\u90e8\u689d\u4ef6\u90fd\u7b26\u5408\uff08\u800c\u4e14\uff09" = "and", "\u4efb\u4e00\u689d\u4ef6\u7b26\u5408\uff08\u6216\uff09" = "or"),
        selected = shiny::isolate(input[[fid("combine", key)]]) %||% "and", inline = TRUE
      )
    })

    # ── 3. 排序 ────────────────────────────────────────────────────────────────
    output$sort_ui <- shiny::renderUI({
      key <- cur()
      if (is.null(key)) return(NULL)
      shiny::tagList(
        shiny::selectizeInput(
          # 原文：依哪些欄排序（照點選的順序；不選＝不排序）
          ns(fid("sort_cols", key)), "\u4f9d\u54ea\u4e9b\u6b04\u6392\u5e8f\uff08\u7167\u9ede\u9078\u7684\u9806\u5e8f\uff1b\u4e0d\u9078\uff1d\u4e0d\u6392\u5e8f\uff09",
          choices = cur_cols(), selected = shiny::isolate(input[[fid("sort_cols", key)]]),
          multiple = TRUE, width = "100%",
          # 原文：不排序
          options = list(placeholder = "\u4e0d\u6392\u5e8f", plugins = list("remove_button"))
        ),
        shiny::uiOutput(ns("sort_desc_ui"))
      )
    })
    output$sort_desc_ui <- shiny::renderUI({
      key <- cur()
      sort_cols <- if (is.null(key)) NULL else input[[fid("sort_cols", key)]]
      if (length(sort_cols) == 0) return(NULL)
      shiny::checkboxGroupInput(
        # 原文：由大到小排的欄（沒勾的由小到大；空白一律排最後）
        ns(fid("sort_desc", key)), "\u7531\u5927\u5230\u5c0f\u6392\u7684\u6b04\uff08\u6c92\u52fe\u7684\u7531\u5c0f\u5230\u5927\uff1b\u7a7a\u767d\u4e00\u5f8b\u6392\u6700\u5f8c\uff09",
        choices = sort_cols,
        selected = intersect(shiny::isolate(input[[fid("sort_desc", key)]]), sort_cols),
        inline = TRUE
      )
    })

    # ── 設定 ──────────────────────────────────────────────────────────────────
    raw_spec <- function(key) {
      list(
        keep_mode = input[[fid("keep_mode", key)]],
        keep_cols = input[[fid("keep_cols", key)]],
        conds = purrr::map(cond_ids()[[key]] %||% integer(0), function(n) {
          list(
            col = input[[cid(n, "col")]], op = input[[cid(n, "op")]],
            num = input[[cid(n, "num")]], lo = input[[cid(n, "lo")]], hi = input[[cid(n, "hi")]],
            values = input[[cid(n, "values")]], text = input[[cid(n, "text")]]
          )
        }),
        combine = input[[fid("combine", key)]],
        sort_cols = input[[fid("sort_cols", key)]],
        sort_desc = input[[fid("sort_desc", key)]]
      )
    }
    specs_now <- shiny::reactive({
      purrr::imap(dfs(), function(df, key) filter_spec_clean(raw_spec(key), df))
    })
    # 設定真的改了才算（reactiveVal 遇到一樣的值不會通知）：舊結果失效用
    specs <- shiny::reactiveVal(list())
    shiny::observe(specs(specs_now()))

    output$file_note <- shiny::renderUI({
      s <- specs()
      if (length(files$keys()) < 2 || length(s) == 0) return(NULL)
      on <- names(s)[purrr::map_lgl(s, "active")]
      shiny::tags$p(
        class = "kit-hint",
        if (length(on) == 0) {
          # 原文：還沒有檔案有設定。
          "\u9084\u6c92\u6709\u6a94\u6848\u6709\u8a2d\u5b9a\u3002"
        } else {
          # 原文：有設定的檔案： | 、 | 。
          paste0("\u6709\u8a2d\u5b9a\u7684\u6a94\u6848\uff1a", paste(shown_of(on), collapse = "\u3001"), "\u3002")
        },
        # 原文：沒有設定的檔案原樣往後傳。
        "\u6c92\u6709\u8a2d\u5b9a\u7684\u6a94\u6848\u539f\u6a23\u5f80\u5f8c\u50b3\u3002"
      )
    })

    tab <- kit_run_tab_server(
      # 原文：執行篩選
      input, output, session, "\u57f7\u884c\u7be9\u9078",
      ready = shiny::reactive({
        if (length(files$keys()) == 0) return(kit_need_files(0L, 1L))
        s <- specs()
        probs <- unlist(purrr::imap(s, function(sp, key) {
          # 原文：：
          if (isTRUE(sp$active) && length(sp$problems) > 0) paste0(shown_of(key), "\uff1a", sp$problems)
        }))
        if (length(probs) > 0) return(utils::head(unname(probs), 3))
        # 原文：選要保留的欄、加一個條件，或選排序的欄
        if (!any(purrr::map_lgl(s, "active"))) return("\u9078\u8981\u4fdd\u7559\u7684\u6b04\u3001\u52a0\u4e00\u500b\u689d\u4ef6\uff0c\u6216\u9078\u6392\u5e8f\u7684\u6b04")
        character(0)
      }),
      ready_link = function() {
        if (length(files$keys()) == 0) {
          # 原文：到「檔案上傳」加入檔案 →
          shiny::actionLink(ns("goto_upload"), "\u5230\u300c\u6a94\u6848\u4e0a\u50b3\u300d\u52a0\u5165\u6a94\u6848 \u2192")
        }
      },
      # 設定摘要：「本次：t1（w1.csv） · 2 個條件（全部符合） · 保留 3／5 欄 · 後面的分頁用篩選後的資料」
      settings = function() {
        s <- specs()
        on <- names(s)[purrr::map_lgl(s, "active")]
        if (length(on) == 0) return(NULL)
        c(
          # 原文：、
          paste(shown_of(on), collapse = "\u3001"),
          if (length(on) == 1) filter_spec_short(s[[on]], length(cols_rv()[[on]])),
          # 原文：後面的分頁用篩選後的資料 | 後面的分頁不用篩選後的資料
          if (use_downstream()) "\u5f8c\u9762\u7684\u5206\u9801\u7528\u7be9\u9078\u5f8c\u7684\u8cc7\u6599" else "\u5f8c\u9762\u7684\u5206\u9801\u4e0d\u7528\u7be9\u9078\u5f8c\u7684\u8cc7\u6599"
        )
      },
      stale_on = function() list(working$data(), specs()),
      # 原文：正在篩選…
      progress = "\u6b63\u5728\u7be9\u9078\u2026",
      run = function() {
        keys <- files$keys()
        d <- dfs()
        # 原文：請先到「檔案上傳」加入檔案。
        if (length(d) == 0) kit_abort("\u8acb\u5148\u5230\u300c\u6a94\u6848\u4e0a\u50b3\u300d\u52a0\u5165\u6a94\u6848\u3002")
        s <- specs_now()[keys]
        out <- filter_files(unname(d[keys]), unname(s), shown_names())
        c(out, list(keys = keys, names = shown_names(), files = files$names(), specs = s))
      },
      done = function(res) {
        kept <- sum(purrr::map_int(res$counts[res$active], "rows_out"))
        kit_done_status(
          paste0(
            # 原文：篩選完成： |  個檔案有設定，共保留  |  列。
            "\u7be9\u9078\u5b8c\u6210\uff1a", kit_num(sum(res$active)), " \u500b\u6a94\u6848\u6709\u8a2d\u5b9a\uff0c\u5171\u4fdd\u7559 ", kit_num(kept), " \u5217\u3002"
          ),
          warnings = res$warnings, points = res$points,
          notes = if (use_downstream()) {
            # 原文：後面的分頁會使用篩選後的資料。
            "\u5f8c\u9762\u7684\u5206\u9801\u6703\u4f7f\u7528\u7be9\u9078\u5f8c\u7684\u8cc7\u6599\u3002"
          } else {
            # 原文：後面的分頁仍使用篩選前的資料（上方選項沒有勾選）。
            "\u5f8c\u9762\u7684\u5206\u9801\u4ecd\u4f7f\u7528\u7be9\u9078\u524d\u7684\u8cc7\u6599\uff08\u4e0a\u65b9\u9078\u9805\u6c92\u6709\u52fe\u9078\uff09\u3002"
          }
        )
      },
      # 原文：篩選
      error = function(e) kit_error_status(e, "\u7be9\u9078", kit_upload_todo(ns, length(files$keys()))),
      result_ui = function(res) {
        shiny::tagList(
          # 原文：各檔案篩選結果
          shiny::h4("\u5404\u6a94\u6848\u7be9\u9078\u7d50\u679c"),
          shiny::tags$div(class = "kit-table-scroll", shiny::tableOutput(ns("report"))),
          shiny::tags$p(
            class = "kit-hint",
            # 原文：「被條件排除的列數」已經包含「因為空白（判斷不了）而排除」的列。
            "\u300c\u88ab\u689d\u4ef6\u6392\u9664\u7684\u5217\u6578\u300d\u5df2\u7d93\u5305\u542b\u300c\u56e0\u70ba\u7a7a\u767d\uff08\u5224\u65b7\u4e0d\u4e86\uff09\u800c\u6392\u9664\u300d\u7684\u5217\u3002",
            # 原文：下面的下載、預覽是上面選的檔案。
            "\u4e0b\u9762\u7684\u4e0b\u8f09\u3001\u9810\u89bd\u662f\u4e0a\u9762\u9078\u7684\u6a94\u6848\u3002"
          )
        )
      },
      details_ui = function(res) {
        shiny::tagList(
          kit_detail(
            # 原文：結果預覽（上面選的檔案）
            "\u7d50\u679c\u9810\u89bd\uff08\u4e0a\u9762\u9078\u7684\u6a94\u6848\uff09",
            shiny::tags$p(class = "kit-hint", shiny::textOutput(ns("preview_meta"), inline = TRUE)),
            shiny::tags$div(class = "kit-table-scroll", shiny::tableOutput(ns("preview")))
          ),
          kit_detail(
            # 原文：每個條件各有幾列符合（上面選的檔案）
            "\u6bcf\u500b\u689d\u4ef6\u5404\u6709\u5e7e\u5217\u7b26\u5408\uff08\u4e0a\u9762\u9078\u7684\u6a94\u6848\uff09",
            shiny::tags$div(class = "kit-table-scroll", shiny::tableOutput(ns("cond_table")))
          )
        )
      },
      # page_code：同一個函式也交給 app 的流程紀錄（pipeline_log_server()）
      code = page_code <- function() {
        f <- code_files_from(files)
        keys <- files$keys()
        in_place <- use_downstream()
        filt <- if (length(keys) > 0) {
          code_filter(f, unname(specs_now()[keys]), shown_names(), in_place)
        }
        summ <- summary_code_now(f, in_place)
        if (is.null(filt) && is.null(summ)) return(NULL)
        list(
          tidy = code_lines(
            "library(dplyr)", if (!is.null(summ)) "library(tidyr)   # pivot_longer()", "",
            filt, summ
          ),
          base = NULL
        )
      },
      code_note = KIT_CODE_NOTE_JOIN
    )
    result <- tab$result
    status <- tab$status

    # 檔案全部移除：清掉結果與狀態
    shiny::observeEvent(files$keys(), {
      if (length(files$keys()) == 0 && !is.null(result())) {
        result(NULL)
        status(kit_status())
      }
    }, ignoreInit = TRUE)

    use_downstream <- shiny::reactive(!identical(input$use_downstream, FALSE))

    output$report <- shiny::renderTable(
      result()$report, digits = 0, format.args = list(big.mark = ",")
    )
    view <- shiny::reactive({
      res <- result()
      key <- cur()
      pos <- if (is.null(res) || is.null(key)) NA else match(key, res$keys)
      if (is.na(pos)) return(NULL)
      list(data = res$data[[pos]], conds = res$conds[[pos]], file = res$files[[pos]],
           active = res$active[[pos]])
    })
    output$preview_meta <- shiny::renderText({
      v <- shiny::req(view())
      kit_rows_note(nrow(v$data), ncol(v$data), KIT_PREVIEW_ROWS, KIT_PREVIEW_COLS)
    })
    output$preview <- shiny::renderTable({
      v <- shiny::req(view())
      kit_preview_table(v$data)
    }, striped = TRUE, spacing = "s")
    output$cond_table <- shiny::renderTable({
      v <- shiny::req(view())
      # 原文：說明 | 這個檔案沒有條件（所有的列都保留）
      v$conds %||% data.frame("\u8aaa\u660e" = "\u9019\u500b\u6a94\u6848\u6c92\u6709\u689d\u4ef6\uff08\u6240\u6709\u7684\u5217\u90fd\u4fdd\u7559\uff09", check.names = FALSE)
    }, digits = 0, format.args = list(big.mark = ","))

    kit_download_server(
      session, output,
      data_fn = function() view()$data,
      filename_fn = function(ext) {
        paste0(tools::file_path_sans_ext(view()$file), "_filtered.", ext)
      },
      # 原文：執行篩選
      run_label = "\u57f7\u884c\u7be9\u9078"
    )

    # ── 摘要 ──────────────────────────────────────────────────────────────────
    # 上面選的檔案：有篩選結果時用篩選後的資料，不然用篩選前的資料
    sum_src <- shiny::reactive({
      key <- cur()
      if (is.null(key)) return(NULL)
      v <- view()
      if (!is.null(v) && isTRUE(v$active)) {
        return(list(data = v$data, filtered = TRUE, key = key))
      }
      list(data = dfs()[[key]], filtered = FALSE, key = key)
    })
    output$sum_source <- shiny::renderUI({
      src <- sum_src()
      if (is.null(src) || is.null(src$data)) return(NULL)
      dims <- kit_dims(nrow(src$data), ncol(src$data))
      pending <- isTRUE(specs()[[src$key]]$active)
      shiny::tags$p(
        class = "kit-source-note",
        if (src$filtered) {
          # 原文：摘要用的是：上面篩選後的資料（ | ）。
          paste0("\u6458\u8981\u7528\u7684\u662f\uff1a\u4e0a\u9762\u7be9\u9078\u5f8c\u7684\u8cc7\u6599\uff08", dims, "\uff09\u3002")
        } else if (pending) {
          # 原文：摘要用的是：篩選前的資料（ | ）。這個檔案有篩選設定，按「執行篩選」後才會用篩選後的資料。
          paste0("\u6458\u8981\u7528\u7684\u662f\uff1a\u7be9\u9078\u524d\u7684\u8cc7\u6599\uff08", dims, "\uff09\u3002\u9019\u500b\u6a94\u6848\u6709\u7be9\u9078\u8a2d\u5b9a\uff0c\u6309\u300c\u57f7\u884c\u7be9\u9078\u300d\u5f8c\u624d\u6703\u7528\u7be9\u9078\u5f8c\u7684\u8cc7\u6599\u3002")
        } else {
          # 原文：摘要用的是：篩選前的資料（ | ；這個檔案沒有篩選設定）。
          paste0("\u6458\u8981\u7528\u7684\u662f\uff1a\u7be9\u9078\u524d\u7684\u8cc7\u6599\uff08", dims, "\uff1b\u9019\u500b\u6a94\u6848\u6c92\u6709\u7be9\u9078\u8a2d\u5b9a\uff09\u3002")
        }
      )
    })
    sum_cols <- shiny::reactive(names(sum_src()$data) %||% character(0))
    sum_num_cols <- shiny::reactive({
      df <- sum_src()$data
      if (is.null(df)) character(0) else summary_numeric_cols(df)
    })
    shiny::observeEvent(list(sum_cols(), sum_num_cols()), {
      shiny::updateSelectizeInput(
        session, "sum_groups", choices = sum_cols(),
        selected = intersect(input$sum_groups %||% character(0), sum_cols())
      )
      shiny::updateSelectizeInput(
        session, "sum_vars", choices = sum_num_cols(),
        selected = intersect(input$sum_vars %||% character(0), sum_num_cols())
      )
    }, ignoreNULL = FALSE)
    output$sum_quick <- shiny::renderUI({
      if (length(sum_num_cols()) == 0) return(NULL)
      pivot_quick_ui(ns("sum_pick"), pivot_name_groups(sum_num_cols()))
    })
    shiny::observeEvent(input$sum_pick, {
      all <- sum_num_cols()
      sel <- pivot_quick_pick(input$sum_pick, input$sum_vars, all, pivot_name_groups(all))
      shiny::updateSelectizeInput(session, "sum_vars", selected = sel)
    })

    sum_res <- shiny::reactiveVal(NULL)
    sum_err <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$sum_run, {
      src <- sum_src()
      out <- tryCatch({
        # 原文：請先到「檔案上傳」加入檔案。
        if (is.null(src$data)) kit_abort("\u8acb\u5148\u5230\u300c\u6a94\u6848\u4e0a\u50b3\u300d\u52a0\u5165\u6a94\u6848\u3002")
        groups <- input$sum_groups %||% character(0)
        vars <- setdiff(input$sum_vars %||% character(0), groups)
        # 大資料（幾十萬列、幾百個變項）要幾秒鐘：顯示進度
        # 原文：正在計算摘要…
        tbl <- shiny::withProgress(message = "\u6b63\u5728\u8a08\u7b97\u6458\u8981\u2026", value = 0.3, {
          summary_table(src$data, groups, vars, input$sum_stats %||% character(0))
        })
        list(table = tbl, filtered = src$filtered)
      }, error = function(e) e)
      if (inherits(out, "error")) {
        sum_res(NULL)
        sum_err(conditionMessage(out))
      } else {
        sum_res(out)
        sum_err(NULL)
      }
    })
    # 資料或設定改了：舊的摘要清掉
    shiny::observeEvent(
      list(sum_src(), input$sum_groups, input$sum_vars, input$sum_stats),
      {
        sum_res(NULL)
        sum_err(NULL)
      },
      ignoreInit = TRUE
    )
    output$sum_result <- shiny::renderUI({
      err <- sum_err()
      if (!is.null(err)) return(shiny::tags$div(class = "mt-3", kit_callout(warn = TRUE, err)))
      r <- sum_res()
      if (is.null(r)) {
        # 原文：選好變項後按「計算摘要」。
        return(shiny::tags$p(class = "kit-hint mt-2", "\u9078\u597d\u8b8a\u9805\u5f8c\u6309\u300c\u8a08\u7b97\u6458\u8981\u300d\u3002"))
      }
      n <- nrow(r$table)
      shiny::tags$div(
        class = "mt-3",
        shiny::tags$p(
          class = "kit-hint",
          paste0(
            # 原文：共  |  列（一列 = 一組 × 一個變項）
            "\u5171 ", kit_num(n), " \u5217\uff08\u4e00\u5217 = \u4e00\u7d44 \u00d7 \u4e00\u500b\u8b8a\u9805\uff09",
            # 原文：，畫面上只列出前  |  列
            if (n > SUMMARY_SHOW_ROWS) paste0("\uff0c\u756b\u9762\u4e0a\u53ea\u5217\u51fa\u524d ", kit_num(SUMMARY_SHOW_ROWS), " \u5217"),
            # 原文：。n 是這一組的列數；n_valid 是有效值個數；n_missing 是遺漏數。
            "\u3002n \u662f\u9019\u4e00\u7d44\u7684\u5217\u6578\uff1bn_valid \u662f\u6709\u6548\u503c\u500b\u6578\uff1bn_missing \u662f\u907a\u6f0f\u6578\u3002",
            # 原文：畫面上四捨五入到小數 3 位，下載的檔案是完整的數字。
            "\u756b\u9762\u4e0a\u56db\u6368\u4e94\u5165\u5230\u5c0f\u6578 3 \u4f4d\uff0c\u4e0b\u8f09\u7684\u6a94\u6848\u662f\u5b8c\u6574\u7684\u6578\u5b57\u3002"
          )
        ),
        shiny::tags$div(class = "kit-table-scroll", shiny::tableOutput(ns("sum_table"))),
        shiny::tags$div(
          class = "kit-downloads",
          kit_download_button(
            # 原文：下載摘要 CSV
            session, "sum_csv", "\u4e0b\u8f09\u6458\u8981 CSV", timestamped_name("summary")("csv"),
            function(file) write_kit_csv(r$table, file), kit_btn_class("secondary")
          ),
          if (requireNamespace("writexl", quietly = TRUE)) {
            kit_download_button(
              # 原文：下載摘要 Excel
              session, "sum_xlsx", "\u4e0b\u8f09\u6458\u8981 Excel", timestamped_name("summary")("xlsx"),
              function(file) writexl::write_xlsx(summary_plain(r$table), file),
              kit_btn_class("secondary")
            )
          }
        )
      )
    })
    output$sum_table <- shiny::renderTable({
      r <- shiny::req(sum_res())
      summary_plain(utils::head(r$table, SUMMARY_SHOW_ROWS)) |>
        summary_display_ints()
    # 原文：—
    }, striped = TRUE, spacing = "s", digits = 3, na = "\u2014")

    # 摘要的程式碼：上面選的檔案、目前的分組與變項
    summary_code_now <- function(f, in_place) {
      src <- sum_src()
      vars <- setdiff(input$sum_vars %||% character(0), input$sum_groups %||% character(0))
      if (is.null(src) || length(vars) == 0) return(NULL)
      pos <- match(src$key, files$keys())
      groups <- input$sum_groups %||% character(0)
      df <- src$data
      labelled <- groups[purrr::map_lgl(groups, function(g) haven::is.labelled(df[[g]]))]
      filtered <- isTRUE(specs_now()[[src$key]]$active)
      obj <- if (filtered) filter_target_obj(f$obj[pos], in_place) else f$obj[pos]
      code_summary(
        obj, sub("^data_", "summary_", f$obj[pos]), groups, vars,
        input$sum_stats %||% character(0), labelled, name = shown_of(src$key)
      )
    }

    use_filtered <- shiny::reactive({
      res <- result()
      !is.null(res) && use_downstream() && identical(res$keys, files$keys())
    })

    list(
      result = tab$result,
      code = page_code,
      data = shiny::reactive(if (use_filtered()) result()$data else working$data()),
      source = working$source,
      code_cells = working$code_cells,
      filtered = use_filtered,
      # "stale"：篩選的設定或資料改了、舊結果已清除（後面的分頁用沒篩選的資料並提醒）
      filter_state = shiny::reactive({
        if (use_filtered()) {
          "on"
        } else if (identical(tab$state(), "stale") && use_downstream()) {
          "stale"
        } else {
          "off"
        }
      }),
      # 這一頁的狀態（步驟分頁打 ✓ 用）
      state = tab$state
    )
  })
}

#' 一個條件的值：依比較方式換成數字框、兩個數字框、選值、輸入文字
#' @param id function(name) 回傳輸入的完整 id
#' @param choices filter_value_choices() 的結果（NULL：值太多，自己輸入）
#' @param now 目前的值（換欄、換比較方式時保留）
#' @noRd
filter_value_ui <- function(id, op, choices, now) {
  num <- function(name) {
    shiny::numericInput(id(name), NULL, value = now[[name]] %||% NA, width = "9rem")
  }
  hint <- function(text) shiny::tags$small(class = "kit-hint d-block", text)
  if (op %in% c("eq", "ne", "gt", "ge", "lt", "le")) return(num("num"))
  if (op == "between") {
    return(shiny::tags$div(
      class = "d-flex gap-2 align-items-start",
      # 原文：到
      num("lo"), shiny::tags$span(class = "pt-2", "\u5230"), num("hi")
    ))
  }
  if (op %in% c("in", "not_in")) {
    widget <- if (is.null(choices)) {
      shiny::textInput(
        id("text"), NULL, value = now$text %||% "", width = "16rem",
        # 原文：輸入一個值（要完全一樣）
        placeholder = "\u8f38\u5165\u4e00\u500b\u503c\uff08\u8981\u5b8c\u5168\u4e00\u6a23\uff09"
      )
    } else {
      shiny::selectizeInput(
        id("values"), NULL, choices = choices, selected = now$values, multiple = TRUE,
        width = "20rem",
        # 原文：點這裡選值
        options = list(placeholder = "\u9ede\u9019\u88e1\u9078\u503c", plugins = list("remove_button"))
      )
    }
    return(shiny::tags$div(
      widget,
      # 原文：空白的列不算符合。 | 空白的列會留下（空白不屬於任何一個值）。
      hint(if (op == "in") "\u7a7a\u767d\u7684\u5217\u4e0d\u7b97\u7b26\u5408\u3002" else "\u7a7a\u767d\u7684\u5217\u6703\u7559\u4e0b\uff08\u7a7a\u767d\u4e0d\u5c6c\u65bc\u4efb\u4f55\u4e00\u500b\u503c\uff09\u3002")
    ))
  }
  if (op == "contains") {
    return(shiny::tags$div(
      shiny::textInput(
        id("text"), NULL, value = now$text %||% "", width = "16rem",
        # 原文：例如 臺北
        placeholder = "\u4f8b\u5982 \u81fa\u5317"
      ),
      # 原文：照字面比對，要一模一樣（全形、半形不同）；空白的列不算符合。
      hint("\u7167\u5b57\u9762\u6bd4\u5c0d\uff0c\u8981\u4e00\u6a21\u4e00\u6a23\uff08\u5168\u5f62\u3001\u534a\u5f62\u4e0d\u540c\uff09\uff1b\u7a7a\u767d\u7684\u5217\u4e0d\u7b97\u7b26\u5408\u3002")
    ))
  }
  NULL
}

#' 選欄的效果：「保留 3 欄、拿掉 2 欄（city、age_m）」
#' @noRd
filter_keep_count_text <- function(all, mode, picked) {
  # 原文：還沒有資料。
  if (length(all) == 0) return("\u9084\u6c92\u6709\u8cc7\u6599\u3002")
  kept <- filter_keep_cols(all, mode, intersect(picked %||% character(0), all))
  dropped <- setdiff(all, kept)
  # 原文：每一欄都拿掉了，至少要保留一欄。
  if (length(kept) == 0) return("\u6bcf\u4e00\u6b04\u90fd\u62ff\u6389\u4e86\uff0c\u81f3\u5c11\u8981\u4fdd\u7559\u4e00\u6b04\u3002")
  # 原文：全部  |  欄都保留。
  if (length(dropped) == 0) return(paste0("\u5168\u90e8 ", kit_num(length(all)), " \u6b04\u90fd\u4fdd\u7559\u3002"))
  paste0(
    # 原文：保留  |  欄，拿掉  |  欄（
    "\u4fdd\u7559 ", kit_num(length(kept)), " \u6b04\uff0c\u62ff\u6389 ", kit_num(length(dropped)), " \u6b04\uff08",
    # 原文：、 | … | ）。
    paste(utils::head(dropped, 6), collapse = "\u3001"), if (length(dropped) > 6) "\u2026", "\uff09\u3002"
  )
}

#' 摘要表給畫面與 Excel：類別欄轉成文字
#' @noRd
summary_plain <- function(tbl) {
  tbl[] <- lapply(tbl, function(x) if (is.factor(x)) as.character(x) else x)
  tbl
}

# 頁首的圖解：留下符合條件的列與要的欄
filter_diagram <- function() {
  kit_diagram(
    before = kit_dg_table(
      # 原文：篩選前
      data.frame(id = c(1, 2, 3), sex = c(1, 2, 1), score = c(52, NA, 61)), "\u7be9\u9078\u524d"
    ),
    # 原文：sex 等於 1，拿掉 sex
    after = kit_dg_table(data.frame(id = c(1, 3), score = c(52, 61)), "sex \u7b49\u65bc 1\uff0c\u62ff\u6389 sex"),
    # 原文：留下符合條件的列與要的欄；沒有設定的檔案原樣往後傳
    caption = "\u7559\u4e0b\u7b26\u5408\u689d\u4ef6\u7684\u5217\u8207\u8981\u7684\u6b04\uff1b\u6c92\u6709\u8a2d\u5b9a\u7684\u6a94\u6848\u539f\u6a23\u5f80\u5f8c\u50b3"
  )
}

#' 畫面上的摘要表：整數的欄（分組欄的代碼、人數）顯示成整數，不要寫成 1.000
#' @noRd
summary_display_ints <- function(tab) {
  whole <- function(x) is.double(x) && all(is.na(x) | (is.finite(x) & x == round(x)))
  dplyr::mutate(tab, dplyr::across(dplyr::where(whole), as.integer))
}
