# 長/寬格式轉換：tidyr::pivot_wider() / pivot_longer()
#
# 主畫面只寫白話（「哪一欄的值變成新的欄名」）；tidyr 的函式名與參數名
# 收在「進階設定」與「用 R 怎麼做」裡（KIT 設計語言 1.7 白話先行）。

# 進階設定裡的控制項標籤：中文說明 + tidyr 參數名
arg_label <- function(text, arg) {
  shiny::tagList(text, " ", shiny::tags$code(arg))
}

# 長/寬格式轉換的頁面：骨架是 kitDesign::kit_run_tab_ui()；狀態框下面是重複的列（output blocked）
mod_reshape_ui <- function(id) {
  ns <- shiny::NS(id)
  is_wider <- "input.direction == 'wider'"
  kit_run_tab_ui(
    # 原文：執行轉換
    id, "\u57f7\u884c\u8f49\u63db",
    guide = kit_tab_guide(
      goal = paste0(
        # 原文：把一個檔案在長格式與寬格式之間互轉。
        "\u628a\u4e00\u500b\u6a94\u6848\u5728\u9577\u683c\u5f0f\u8207\u5bec\u683c\u5f0f\u4e4b\u9593\u4e92\u8f49\u3002",
        # 原文：例如 kitsUI 分數表（每次施測 × 每個領域一列）轉成每次施測一列、各領域並排。
        "\u4f8b\u5982 kitsUI \u5206\u6578\u8868\uff08\u6bcf\u6b21\u65bd\u6e2c \u00d7 \u6bcf\u500b\u9818\u57df\u4e00\u5217\uff09\u8f49\u6210\u6bcf\u6b21\u65bd\u6e2c\u4e00\u5217\u3001\u5404\u9818\u57df\u4e26\u6392\u3002"
      ),
      steps = kit_steps(
        # 原文：選要轉換的檔案與方向（會先依資料的樣子幫你選好）
        "\u9078\u8981\u8f49\u63db\u7684\u6a94\u6848\u8207\u65b9\u5411\uff08\u6703\u5148\u4f9d\u8cc7\u6599\u7684\u6a23\u5b50\u5e6b\u4f60\u9078\u597d\uff09",
        # 原文：設定欄位（R 的參數名稱在「進階設定」與「用 R 怎麼做」裡）
        "\u8a2d\u5b9a\u6b04\u4f4d\uff08R \u7684\u53c3\u6578\u540d\u7a31\u5728\u300c\u9032\u968e\u8a2d\u5b9a\u300d\u8207\u300c\u7528 R \u600e\u9ebc\u505a\u300d\u88e1\uff09",
        # 原文：按「執行轉換」，看結果、檢查與預覽
        "\u6309\u300c\u57f7\u884c\u8f49\u63db\u300d\uff0c\u770b\u7d50\u679c\u3001\u6aa2\u67e5\u8207\u9810\u89bd",
        # 原文：下載 CSV 或 SAV
        "\u4e0b\u8f09 CSV \u6216 SAV"
      ),
      # 原文：轉換後的一個資料檔；轉換前後的列數 × 欄數會並列在摘要中。
      output = "\u8f49\u63db\u5f8c\u7684\u4e00\u500b\u8cc7\u6599\u6a94\uff1b\u8f49\u63db\u524d\u5f8c\u7684\u5217\u6578 \u00d7 \u6b04\u6578\u6703\u4e26\u5217\u5728\u6458\u8981\u4e2d\u3002",
      technical = paste0(
        # 原文：長轉寬用 tidyr::pivot_wider()，寬轉長用 tidyr::pivot_longer()，預設值同 tidyr。
        "\u9577\u8f49\u5bec\u7528 tidyr::pivot_wider()\uff0c\u5bec\u8f49\u9577\u7528 tidyr::pivot_longer()\uff0c\u9810\u8a2d\u503c\u540c tidyr\u3002",
        # 原文：長轉寬不做 values_fn：id_cols × names_from 有重複時停下並列出重複列，
        "\u9577\u8f49\u5bec\u4e0d\u505a values_fn\uff1aid_cols \u00d7 names_from \u6709\u91cd\u8907\u6642\u505c\u4e0b\u4e26\u5217\u51fa\u91cd\u8907\u5217\uff0c",
        # 原文：不會默默彙總，也不會產生 list 欄。
        "\u4e0d\u6703\u9ed8\u9ed8\u5f59\u7e3d\uff0c\u4e5f\u4e0d\u6703\u7522\u751f list \u6b04\u3002"
      ),
      diagram = reshape_diagram()
    ),
    shiny::uiOutput(ns("source_note")),
    shiny::fluidRow(
      shiny::column(
        5,
        # 原文：要轉換的檔案
        shiny::selectInput(ns("file"), "\u8981\u8f49\u63db\u7684\u6a94\u6848", choices = NULL)
      ),
      shiny::column(
        7,
        shiny::radioButtons(
          # 原文：方向
          ns("direction"), "\u65b9\u5411",
          choices = c(
            # 原文：長轉寬（每人一列、各題並排）
            "\u9577\u8f49\u5bec\uff08\u6bcf\u4eba\u4e00\u5217\u3001\u5404\u984c\u4e26\u6392\uff09" = "wider",
            # 原文：寬轉長（每人每題一列）
            "\u5bec\u8f49\u9577\uff08\u6bcf\u4eba\u6bcf\u984c\u4e00\u5217\uff09" = "longer"
          ),
          selected = "wider", inline = TRUE
        )
      )
    ),
    # 一個提示：資料看起來是長或寬格式；和方向相反時是黃色，「改選…」的按鈕在框外面
    shiny::uiOutput(ns("guess")),
    kit_advanced(
      # 原文：判斷錯了？換一欄當 ID 再看一次
      title = "\u5224\u65b7\u932f\u4e86\uff1f\u63db\u4e00\u6b04\u7576 ID \u518d\u770b\u4e00\u6b21",
      shiny::selectizeInput(
        # 原文：判斷長、寬格式時，用哪一欄當 ID
        ns("guess_id"), "\u5224\u65b7\u9577\u3001\u5bec\u683c\u5f0f\u6642\uff0c\u7528\u54ea\u4e00\u6b04\u7576 ID",
        # 原文：（請選）
        choices = c("\uff08\u8acb\u9078\uff09" = ""), width = "16rem"
      )
    ),
    shiny::conditionalPanel(is_wider, ns = ns, wider_controls_ui(ns)),
    shiny::conditionalPanel(
      "input.direction == 'longer'", ns = ns,
      mod_pivot_longer_ui(ns("longer"))
    ),
    result = shiny::uiOutput(ns("blocked"))
  )
}

# 長轉寬控制項的白話名稱（主畫面、設定摘要、錯誤訊息用同一套）
WIDER_LABELS <- c(
  # 原文：哪一欄的值變成新的欄名
  names_from = "\u54ea\u4e00\u6b04\u7684\u503c\u8b8a\u6210\u65b0\u7684\u6b04\u540d",
  # 原文：要填進新欄的值
  values_from = "\u8981\u586b\u9032\u65b0\u6b04\u7684\u503c",
  # 原文：辨認同一列的欄
  id_cols = "\u8fa8\u8a8d\u540c\u4e00\u5217\u7684\u6b04"
)

# 長轉寬的控制項（參數一對一對應 tidyr::pivot_wider()；參數名在進階設定裡）
wider_controls_ui <- function(ns) {
  shiny::tagList(
    shiny::uiOutput(ns("preset")),
    shiny::fluidRow(
      shiny::column(
        5,
        shiny::selectizeInput(
          ns("names_from"), WIDER_LABELS[["names_from"]],
          choices = NULL,
          # 原文：例如 wave 或 item
          options = list(placeholder = "\u4f8b\u5982 wave \u6216 item")
        )
      ),
      shiny::column(
        7,
        shiny::selectizeInput(
          # 原文：（可多選）
          ns("values_from"), paste0(WIDER_LABELS[["values_from"]], "\uff08\u53ef\u591a\u9078\uff09"),
          choices = NULL, multiple = TRUE,
          # 原文：例如 score
          options = list(placeholder = "\u4f8b\u5982 score")
        )
      )
    ),
    shiny::checkboxInput(
      ns("id_auto"),
      # 原文：其餘的欄都用來辨認同一列（預設）
      "\u5176\u9918\u7684\u6b04\u90fd\u7528\u4f86\u8fa8\u8a8d\u540c\u4e00\u5217\uff08\u9810\u8a2d\uff09",
      value = TRUE, width = "100%"
    ),
    shiny::conditionalPanel(
      "!input.id_auto", ns = ns,
      shiny::selectizeInput(
        # 原文：（可多選）
        ns("id_cols"), paste0(WIDER_LABELS[["id_cols"]], "\uff08\u53ef\u591a\u9078\uff09"),
        choices = NULL, multiple = TRUE, width = "100%"
      )
    ),
    shiny::uiOutput(ns("id_note")),
    kit_advanced(
      id = ns("advanced"),
      # 原文：進階設定：欄名格式、欄位排列、缺的格子、R 的參數名稱
      title = "\u9032\u968e\u8a2d\u5b9a\uff1a\u6b04\u540d\u683c\u5f0f\u3001\u6b04\u4f4d\u6392\u5217\u3001\u7f3a\u7684\u683c\u5b50\u3001R \u7684\u53c3\u6578\u540d\u7a31",
      wider_advanced_ui(ns)
    )
  )
}

# 長轉寬的進階控制項（預設收起）：最上面一行對照 tidyr 的參數名
wider_advanced_ui <- function(ns) {
  shiny::tagList(
    shiny::tags$p(
      class = "kit-hint",
      # 原文：對照 R 的  | ：
      "\u5c0d\u7167 R \u7684 ", shiny::tags$code("tidyr::pivot_wider()"), "\uff1a",
      # 原文：「 | 」是  | ；
      "\u300c", WIDER_LABELS[["names_from"]], "\u300d\u662f ", shiny::tags$code("names_from"), "\uff1b",
      # 原文：「 | 」是  | ；
      "\u300c", WIDER_LABELS[["values_from"]], "\u300d\u662f ", shiny::tags$code("values_from"), "\uff1b",
      # 原文：「 | 」是 
      "\u300c", WIDER_LABELS[["id_cols"]], "\u300d\u662f ", shiny::tags$code("id_cols"),
      # 原文：（勾「其餘的欄」時是  | ）。
      "\uff08\u52fe\u300c\u5176\u9918\u7684\u6b04\u300d\u6642\u662f ", shiny::tags$code("id_cols = NULL"), "\uff09\u3002"
    ),
    shiny::radioButtons(
      # 原文：新欄名怎麼組？
      ns("name_mode"), "\u65b0\u6b04\u540d\u600e\u9ebc\u7d44\uff1f",
      choices = c(
        # 原文：值欄名 + 分隔字 + 欄名來源的值（names_sep / names_prefix）
        "\u503c\u6b04\u540d + \u5206\u9694\u5b57 + \u6b04\u540d\u4f86\u6e90\u7684\u503c\uff08names_sep / names_prefix\uff09" = "sep",
        # 原文：自訂格式（names_glue）
        "\u81ea\u8a02\u683c\u5f0f\uff08names_glue\uff09" = "glue"
      ),
      selected = "sep", width = "100%"
    ),
    shiny::conditionalPanel(
      "input.name_mode == 'sep'", ns = ns,
      shiny::fluidRow(
        shiny::column(
          4,
          # 原文：分隔字
          shiny::textInput(ns("names_sep"), arg_label("\u5206\u9694\u5b57", "names_sep"), "_")
        ),
        shiny::column(
          4,
          shiny::textInput(
            # 原文：欄名前綴
            ns("names_prefix"), arg_label("\u6b04\u540d\u524d\u7db4", "names_prefix"), ""
          )
        )
      )
    ),
    shiny::conditionalPanel(
      "input.name_mode == 'glue'", ns = ns,
      shiny::textInput(
        # 原文：欄名格式
        ns("names_glue"), arg_label("\u6b04\u540d\u683c\u5f0f", "names_glue"), "",
        placeholder = "{.value}_{wave}"
      ),
      shiny::tags$small(
        class = "kit-hint",
        # 原文：{.value} 是值欄的名稱（如 score），{wave} 是欄名來源欄的值（如 t1）。
        "{.value} \u662f\u503c\u6b04\u7684\u540d\u7a31\uff08\u5982 score\uff09\uff0c{wave} \u662f\u6b04\u540d\u4f86\u6e90\u6b04\u7684\u503c\uff08\u5982 t1\uff09\u3002"
      )
    ),
    shiny::radioButtons(
      # 原文：欄位排列
      ns("names_vary"), arg_label("\u6b04\u4f4d\u6392\u5217", "names_vary"),
      choices = c(
        # 原文：fastest：同一個值欄放在一起（score_t1、score_t2、se_t1…）
        "fastest\uff1a\u540c\u4e00\u500b\u503c\u6b04\u653e\u5728\u4e00\u8d77\uff08score_t1\u3001score_t2\u3001se_t1\u2026\uff09" = "fastest",
        # 原文：slowest：同一個欄名來源的值放在一起（score_t1、se_t1、score_t2…）
        "slowest\uff1a\u540c\u4e00\u500b\u6b04\u540d\u4f86\u6e90\u7684\u503c\u653e\u5728\u4e00\u8d77\uff08score_t1\u3001se_t1\u3001score_t2\u2026\uff09" = "slowest"
      ),
      selected = "fastest", width = "100%"
    ),
    shiny::checkboxInput(
      # 原文：新欄照欄名來源的值排序
      ns("names_sort"), arg_label("\u65b0\u6b04\u7167\u6b04\u540d\u4f86\u6e90\u7684\u503c\u6392\u5e8f", "names_sort"),
      value = FALSE, width = "100%"
    ),
    shiny::tags$small(
      class = "kit-hint",
      # 原文：不勾（tidyr 的預設）：新欄照資料裡第一次出現的順序，資料的列換了順序，欄的順序也會跟著換；
      #       勾了：照值排序（數字照大小；文字照字母，t10 排在 t2 前面）。
      "\u4e0d\u52fe\uff08tidyr \u7684\u9810\u8a2d\uff09\uff1a\u65b0\u6b04\u7167\u8cc7\u6599\u88e1\u7b2c\u4e00\u6b21\u51fa\u73fe\u7684\u9806\u5e8f\uff0c\u8cc7\u6599\u7684\u5217\u63db\u4e86\u9806\u5e8f\uff0c\u6b04\u7684\u9806\u5e8f\u4e5f\u6703\u8ddf\u8457\u63db\uff1b",
      "\u52fe\u4e86\uff1a\u7167\u503c\u6392\u5e8f\uff08\u6578\u5b57\u7167\u5927\u5c0f\uff1b\u6587\u5b57\u7167\u5b57\u6bcd\uff0ct10 \u6392\u5728 t2 \u524d\u9762\uff09\u3002"
    ),
    shiny::textInput(
      # 原文：缺的格子填什麼
      ns("values_fill"), arg_label("\u7f3a\u7684\u683c\u5b50\u586b\u4ec0\u9ebc", "values_fill"), "",
      # 原文：空白 = 不填（NA）
      placeholder = "\u7a7a\u767d = \u4e0d\u586b\uff08NA\uff09"
    ),
    shiny::tags$small(
      class = "kit-hint",
      # 原文：只填「沒有這一列」造成的空格；原本就空白的值不會被填。
      "\u53ea\u586b\u300c\u6c92\u6709\u9019\u4e00\u5217\u300d\u9020\u6210\u7684\u7a7a\u683c\uff1b\u539f\u672c\u5c31\u7a7a\u767d\u7684\u503c\u4e0d\u6703\u88ab\u586b\u3002",
      # 原文：數字只填進數值欄、文字只填進文字欄。
      "\u6578\u5b57\u53ea\u586b\u9032\u6578\u503c\u6b04\u3001\u6587\u5b57\u53ea\u586b\u9032\u6587\u5b57\u6b04\u3002"
    )
  )
}

# 長/寬格式轉換（參數同 mod_join_server()）。狀態流在 kit_run_tab_server()；這裡是檔案與方向、
# 長或寬的判斷、兩個方向的設定、轉換、重複的列、預覽與下載
mod_reshape_server <- function(id, files, working, goto) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    blocked <- shiny::reactiveVal(NULL)
    watch_goto_links(input, goto)

    output$source_note <- shiny::renderUI({
      reshape_source_note_ui(
        length(files$keys()), working$source(), working$code_cells(), ns,
        filter = kit_filter_state(working)
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
        # 原文：（請選）
        session, "guess_id", choices = c("\uff08\u8acb\u9078\uff09" = "", all), selected = guess_sel %||% ""
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
        values_fill = input$values_fill,
        names_sort = input$names_sort
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
      # 原文：長轉寬 | 寬轉長
      names_dir <- c(wider = "\u9577\u8f49\u5bec", longer = "\u5bec\u8f49\u9577")
      shiny::tagList(
        kit_callout(
          warn = against,
          paste0(
            reshape_guess_text(g),
            # 原文：現在選的是「 | 」，確定嗎？
            if (against) paste0("\u73fe\u5728\u9078\u7684\u662f\u300c", names_dir[[input$direction]], "\u300d\uff0c\u78ba\u5b9a\u55ce\uff1f")
          )
        ),
        if (against) {
          shiny::tags$div(
            class = "mb-3",
            # 原文：改選「 | 」
            kit_button(ns("guess_flip_btn"), paste0("\u6539\u9078\u300c", names_dir[[to]], "\u300d"))
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
          # 原文：目前用來辨認同一列的欄（ |  欄）：
          "\u76ee\u524d\u7528\u4f86\u8fa8\u8a8d\u540c\u4e00\u5217\u7684\u6b04\uff08", kit_num(length(ids)), " \u6b04\uff09\uff1a",
          # 原文：、
          paste(utils::head(ids, 12), collapse = "\u3001"),
          # 原文： … | 。
          if (length(ids) > 12) " \u2026" else "", "\u3002",
          # 原文：轉換後，這些欄的每一組值是一列。
          "\u8f49\u63db\u5f8c\uff0c\u9019\u4e9b\u6b04\u7684\u6bcf\u4e00\u7d44\u503c\u662f\u4e00\u5217\u3002"
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
          # 原文：這個檔案看起來是 kitsUI 的分數表（一列 = 一次施測 × 一個領域）。
          "\u9019\u500b\u6a94\u6848\u770b\u8d77\u4f86\u662f kitsUI \u7684\u5206\u6578\u8868\uff08\u4e00\u5217 = \u4e00\u6b21\u65bd\u6e2c \u00d7 \u4e00\u500b\u9818\u57df\uff09\u3002",
          # 原文：按下面的按鈕只會填好設定，不會自動執行；要填的值會多放 scale，
          "\u6309\u4e0b\u9762\u7684\u6309\u9215\u53ea\u6703\u586b\u597d\u8a2d\u5b9a\uff0c\u4e0d\u6703\u81ea\u52d5\u57f7\u884c\uff1b\u8981\u586b\u7684\u503c\u6703\u591a\u653e scale\uff0c",
          # 原文：d_score 空白時才分得出是沒測這個領域，還是有測但還沒放上 D 量尺。
          "d_score \u7a7a\u767d\u6642\u624d\u5206\u5f97\u51fa\u662f\u6c92\u6e2c\u9019\u500b\u9818\u57df\uff0c\u9084\u662f\u6709\u6e2c\u4f46\u9084\u6c92\u653e\u4e0a D \u91cf\u5c3a\u3002"
        ),
        # 原文：套用 kitsUI 分數表預設
        kit_button(ns("apply_preset"), "\u5957\u7528 kitsUI \u5206\u6578\u8868\u9810\u8a2d")
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
        # 原文：已填入 kitsUI 分數表預設。請確認後按「執行轉換」。
        "\u5df2\u586b\u5165 kitsUI \u5206\u6578\u8868\u9810\u8a2d\u3002\u8acb\u78ba\u8a8d\u5f8c\u6309\u300c\u57f7\u884c\u8f49\u63db\u300d\u3002",
        type = "message", duration = KIT_NOTIFY_SECONDS
      )
    })

    # 檔案或設定變動：舊結果失效（kit_run_tab_server()），重複的列也不再顯示
    changes <- function() list(src(), input$direction, wider(), longer())
    shiny::observeEvent(changes(), blocked(NULL), ignoreInit = TRUE)

    tab <- kit_run_tab_server(
      # 原文：執行轉換
      input, output, session, "\u57f7\u884c\u8f49\u63db",
      # 主要按鈕：沒有檔案、還沒選欄時停用
      ready = shiny::reactive({
        if (length(files$keys()) == 0) return(kit_need_files(0L, 1L))
        # 原文：選要轉換的檔案
        if (is.null(picked())) return("\u9078\u8981\u8f49\u63db\u7684\u6a94\u6848")
        if (identical(input$direction, "longer")) {
          a <- longer()
          # 原文：選要轉成長格式的欄
          if (length(a$cols) == 0 && !isTRUE(a$cols_exclude)) return("\u9078\u8981\u8f49\u6210\u9577\u683c\u5f0f\u7684\u6b04")
          return(character(0))
        }
        w <- wider()
        c(
          # 原文：選「 | 」
          if (length(w$names_from) == 0) paste0("\u9078\u300c", WIDER_LABELS[["names_from"]], "\u300d"),
          # 原文：選「 | 」
          if (length(w$values_from) == 0) paste0("\u9078\u300c", WIDER_LABELS[["values_from"]], "\u300d")
        )
      }),
      ready_link = function() {
        # 原文：到「檔案上傳」加入檔案 →
        if (length(files$keys()) == 0) shiny::actionLink(ns("goto_upload"), "\u5230\u300c\u6a94\u6848\u4e0a\u50b3\u300d\u52a0\u5165\u6a94\u6848 \u2192")
      },
      # 設定摘要：「本次：t1（scores.csv） · 長轉寬 · names_from：domain · values_from：d_score」
      settings = function() {
        p <- picked()
        if (is.null(p)) return(NULL)
        shown <- file_display_name(files$labels(), files$names())
        pos <- match(input$file %||% "", files$keys())
        args <- if (identical(input$direction, "longer")) {
          a <- longer()
          n <- length(a$cols)
          # 原文：寬轉長
          c("\u5bec\u8f49\u9577", if (n > 0) {
            # 原文：不轉  | 轉  |  欄
            paste0(if (isTRUE(a$cols_exclude)) "\u4e0d\u8f49 " else "\u8f49 ", kit_num(n), " \u6b04")
          } else {
            # 原文：還沒選要轉的欄
            "\u9084\u6c92\u9078\u8981\u8f49\u7684\u6b04"
          })
        } else {
          w <- wider()
          # 原文：長轉寬
          c("\u9577\u8f49\u5bec",
            # 原文：新欄名來自  | 、
            if (length(w$names_from) > 0) paste0("\u65b0\u6b04\u540d\u4f86\u81ea ", paste(w$names_from, collapse = "\u3001")),
            if (length(w$values_from) > 0) {
              # 原文：填入  | 、
              paste0("\u586b\u5165 ", paste(w$values_from, collapse = "\u3001"))
            })
        }
        c(shown[pos], args)
      },
      stale_on = changes,
      # 原文：正在轉換資料…
      progress = "\u6b63\u5728\u8f49\u63db\u8cc7\u6599\u2026",
      run = function() {
        blocked(NULL)
        df <- src()
        # 原文：請先到「檔案上傳」加入檔案。
        if (is.null(df)) kit_abort("\u8acb\u5148\u5230\u300c\u6a94\u6848\u4e0a\u50b3\u300d\u52a0\u5165\u6a94\u6848\u3002")
        out <- if (identical(input$direction, "longer")) {
          reshape_longer(df, longer())
        } else {
          run_wider(df, wider(), blocked)
        }
        out$direction <- input$direction
        out$before <- c(nrow(df), ncol(df))
        out
      },
      done = function(out) {
        kit_done_status(
          # 原文：轉換完成：結果共  | 。
          paste0("\u8f49\u63db\u5b8c\u6210\uff1a\u7d50\u679c\u5171 ", kit_dims(nrow(out$result), ncol(out$result)), "\u3002"),
          warnings = out$warnings, points = out$summary
        )
      },
      # 原文：轉換
      error = function(e) kit_error_status(e, "\u8f49\u63db", kit_upload_todo(ns, length(files$keys()))),
      # 重點都在狀態框裡；空白格、預覽、id 欄不一致的列收在細節列
      details_ui = function(res) {
        is_wider <- identical(res$direction, "wider")
        shiny::tagList(
          kit_detail(
            # 原文：結果預覽
            "\u7d50\u679c\u9810\u89bd",
            shiny::tags$p(class = "kit-hint", shiny::textOutput(ns("preview_meta"), inline = TRUE)),
            shiny::tags$div(class = "kit-table-scroll", shiny::tableOutput(ns("preview")))
          ),
          if (is_wider) {
            kit_detail(
              # 原文：空白格從哪裡來？
              "\u7a7a\u767d\u683c\u5f9e\u54ea\u88e1\u4f86\uff1f",
              shiny::tags$p(
                class = "kit-hint",
                # 原文：轉成寬格式後，下面兩種情況都會是空白：
                "\u8f49\u6210\u5bec\u683c\u5f0f\u5f8c\uff0c\u4e0b\u9762\u5169\u7a2e\u60c5\u6cc1\u90fd\u6703\u662f\u7a7a\u767d\uff1a",
                # 原文：「沒有這一列」＝原檔沒有這組（辨認同一列的欄 ×  | ）的列；
                "\u300c\u6c92\u6709\u9019\u4e00\u5217\u300d\uff1d\u539f\u6a94\u6c92\u6709\u9019\u7d44\uff08\u8fa8\u8a8d\u540c\u4e00\u5217\u7684\u6b04 \u00d7 ", names(res$empty)[1], "\uff09\u7684\u5217\uff1b",
                # 原文：「有列但空白」＝有這一列，但值本來就是空的。
                "\u300c\u6709\u5217\u4f46\u7a7a\u767d\u300d\uff1d\u6709\u9019\u4e00\u5217\uff0c\u4f46\u503c\u672c\u4f86\u5c31\u662f\u7a7a\u7684\u3002"
              ),
              shiny::tags$div(class = "kit-table-scroll", shiny::tableOutput(ns("empty_table")))
            )
          },
          if (is_wider && !res$id_check$ok) {
            kit_audit_detail(
              shiny::h6(paste0(
                # 原文：辨認同一列的欄不一致的列（ | ）
                "\u8fa8\u8a8d\u540c\u4e00\u5217\u7684\u6b04\u4e0d\u4e00\u81f4\u7684\u5217\uff08", kit_rows_note(nrow(res$id_check$rows), NULL, 50), "\uff09"
              )),
              shiny::tags$div(class = "kit-table-scroll", shiny::tableOutput(ns("id_rows")))
            )
          }
        )
      },
      # page_code：同一個函式也交給 app 的流程紀錄（pipeline_log_server()）
      code = page_code <- function() {
        p <- picked()
        obj <- p$obj %||% "data_t1"
        if (identical(input$direction, "longer")) {
          args <- utils::modifyList(PIVOT_LONGER_DEFAULTS, longer())
          labels <- if (!is.null(p$data)) {
            pivot_longer_labels(p$data, pivot_longer_cols(p$data, args))
          }
          code_longer(longer(), obj, names(p$data), labels)
        } else {
          code_wider(wider(), p$data %||% tibble::tibble(), obj)
        }
      },
      code_note = KIT_CODE_NOTE_JOIN
    )
    result <- tab$result

    output$blocked <- shiny::renderUI({
      dup <- blocked()
      if (is.null(dup)) return(NULL)
      shiny::tagList(
        # 原文：重複的列（ | ）
        shiny::h5(paste0("\u91cd\u8907\u7684\u5217\uff08", kit_rows_note(nrow(dup$rows), NULL, 50), "\uff09")),
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
      # 原文：執行轉換
      run_label = "\u57f7\u884c\u8f49\u63db"
    )

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
        # 原文：轉換前  | ；轉換後
        "\u8f49\u63db\u524d ", kit_dims(res$before[1], res$before[2]), "\uff1b\u8f49\u63db\u5f8c",
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

    # 這一頁的狀態（步驟分頁打 ✓ 用）
    list(state = tab$state, result = tab$result, code = page_code)
  })
}

# 執行長轉寬；id × names_from 重複時把重複列存進 blocked 再停下
run_wider <- function(df, args, blocked) {
  if (length(args$names_from) == 1 && length(args$values_from) > 0) {
    keys <- c(wider_id_cols(df, args), args$names_from)
    dup <- check_key_unique(df, intersect(keys, names(df)))
    if (!dup$ok) blocked(dup)
  }
  reshape_wider(df, args)
}

# 目前使用哪一份資料；只要 1 個檔案就能用
reshape_source_note_ui <- function(n_files, source, code_cells, ns, filter = "off") {
  # 還沒有檔案時不另開提示框：主要按鈕上面一行「還差」已經說了
  if (n_files == 0) return(NULL)
  kit_source_note_ui(max(n_files, 2L), source, code_cells, ns, filter = filter)
}

# 頁首的圖解：長轉寬（預設的方向）
reshape_diagram <- function() {
  # 教育、心理的資料多半是寬格式，所以寬格式在左；兩個方向都能轉，用雙箭頭
  kit_diagram(
    # 原文：寬格式
    before = kit_dg_table(data.frame(id = c(1, 2), q1 = c(4, 3), q2 = c(5, NA)), "\u5bec\u683c\u5f0f"),
    after = kit_dg_table(
      data.frame(id = c(1, 1, 2, 2), item = c("q1", "q2", "q1", "q2"), score = c(4, 5, 3, NA)),
      # 原文：長格式
      "\u9577\u683c\u5f0f"
    ),
    # 原文：⇄
    arrow = "\u21c4",
    caption = paste0(
      # 原文：寬轉長：每人一列、各題並排，變成每人每題一列（空白的值也留一列）；
      "\u5bec\u8f49\u9577\uff1a\u6bcf\u4eba\u4e00\u5217\u3001\u5404\u984c\u4e26\u6392\uff0c\u8b8a\u6210\u6bcf\u4eba\u6bcf\u984c\u4e00\u5217\uff08\u7a7a\u767d\u7684\u503c\u4e5f\u7559\u4e00\u5217\uff09\uff1b",
      # 原文：長轉寬則倒過來
      "\u9577\u8f49\u5bec\u5247\u5012\u904e\u4f86"
    )
  )
}
