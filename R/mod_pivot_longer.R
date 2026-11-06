# 共用元件：pivot_longer() 的設定（寬轉長）
#
# 每個控制項對應 tidyr::pivot_longer() 的一個參數，畫面上寫中文與參數名稱；
# 預設值和 tidyr 相同。轉成 kits 作答檔用它（參數全部固定），長/寬格式轉換也用它。
#
# 介面：
#   mod_pivot_longer_ui(id)
#   mod_pivot_longer_server(id, data, fixed = list())  → reactive：參數 list
#   pivot_longer_code(args, data_name, cols_code = NULL, col_names = NULL) → 程式碼字串
#   pivot_longer_run(df, args)                          → 長格式資料
#
# cols 一律用點選：挑要轉的欄（cols = c(a, b)），或挑不轉的欄（cols = !c(id, sex)）。
# 欄名怎麼變成值也用點選（去掉開頭、結尾，切成幾段；pivot_names.R），app 寫出跳脫好的規則。
# 會寫正規表示式的人可以在進階設定自己寫 names_prefix / names_pattern；填了就以那裡為準。

# tidyr::pivot_longer() 的預設值（cols 沒有預設，一定要給）
# cols_exclude = TRUE 時 cols 是「不轉的欄」，其餘全部轉
PIVOT_LONGER_DEFAULTS <- list(
  cols = character(0),
  cols_exclude = FALSE,
  names_to = "name",
  values_to = "value",
  names_prefix = NULL,
  names_sep = NULL,
  names_pattern = NULL,
  names_transform = NULL,
  values_drop_na = FALSE
)

# 控制項的中文名稱
PIVOT_LONGER_LABELS <- c(
  # 原文：要轉成長格式的欄
  cols = "\u8981\u8f49\u6210\u9577\u683c\u5f0f\u7684\u6b04",
  # 原文：原本的欄名放到哪一欄
  names_to = "\u539f\u672c\u7684\u6b04\u540d\u653e\u5230\u54ea\u4e00\u6b04",
  # 原文：原本的值放到哪一欄
  values_to = "\u539f\u672c\u7684\u503c\u653e\u5230\u54ea\u4e00\u6b04",
  # 原文：欄名要去掉的開頭（正規表示式）
  names_prefix = "\u6b04\u540d\u8981\u53bb\u6389\u7684\u958b\u982d\uff08\u6b63\u898f\u8868\u793a\u5f0f\uff09",
  # 原文：從欄名取出部分的規則（正規表示式）
  names_pattern = "\u5f9e\u6b04\u540d\u53d6\u51fa\u90e8\u5206\u7684\u898f\u5247\uff08\u6b63\u898f\u8868\u793a\u5f0f\uff09",
  # 原文：去掉空白的值
  values_drop_na = "\u53bb\u6389\u7a7a\u767d\u7684\u503c"
)

# 主畫面上的控制項（只寫中文）；其他的在「進階設定」裡，標題後面附參數名稱
PIVOT_LONGER_MAIN <- c("cols", "names_to", "values_to")

#' 控制項的標題：主畫面只有中文；進階設定裡是「中文（參數名稱）」
#' @noRd
pivot_label <- function(arg, labels = PIVOT_LONGER_LABELS) {
  if (arg %in% PIVOT_LONGER_MAIN) return(labels[[arg]])
  # 原文：（ | ）
  shiny::tagList(labels[[arg]], shiny::tags$code(paste0("\uff08", arg, "\uff09")))
}

#' 進階設定最上面一行：主畫面的控制項對應 tidyr::pivot_longer() 的哪個參數
#' @noRd
pivot_longer_arg_map <- function(labels = PIVOT_LONGER_LABELS) {
  shiny::tags$p(
    class = "kit-hint",
    # 原文：對照 R 的  | ：
    "\u5c0d\u7167 R \u7684 ", shiny::tags$code("tidyr::pivot_longer()"), "\uff1a",
    lapply(seq_along(PIVOT_LONGER_MAIN), function(i) {
      a <- PIVOT_LONGER_MAIN[i]
      shiny::tagList(
        # 原文：「 | 」是 
        "\u300c", labels[[a]], "\u300d\u662f ", shiny::tags$code(a),
        # 原文：； | 。
        if (i < length(PIVOT_LONGER_MAIN)) "\uff1b" else "\u3002"
      )
    })
  )
}

#' pivot_longer() 設定的畫面
#' @param id 模組 id
#' @noRd
mod_pivot_longer_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tags$div(class = "kit-pivot", shiny::uiOutput(ns("controls")))
}

#' pivot_longer() 設定的伺服器
#'
#' @param id 模組 id
#' @param data reactive：要轉換的資料（給 cols 的選項；NULL 時沒有選項）
#' @param fixed 具名 list：要固定的參數（畫面上只顯示、不能改），例如
#'   `list(names_to = "item_id")`。值可以是 reactive（例如 cols 由別的設定決定）。
#' @return reactive：參數 list（names(PIVOT_LONGER_DEFAULTS)），沒改的用 tidyr 的預設。
#'   用點選設定欄名時多兩個欄位：names_by_app（規則是 app 寫的）、names_problem
#'   （點選的設定有問題時給使用者看的文字；pivot_longer_run() 會擋下來）
#' @noRd
mod_pivot_longer_server <- function(id, data, fixed = list()) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    value_of <- function(x) if (is.function(x)) x() else x
    locked <- function(arg) arg %in% names(fixed)

    # 欄名怎麼變成值：開頭、規則沒有固定時才用點選（⑥ 全部固定，沒有這一塊）
    names_click <- !locked("names_prefix") && !locked("names_pattern")

    output$controls <- shiny::renderUI({
      text <- function(arg, placeholder = NULL) {
        if (locked(arg)) return(pivot_locked_ui(ns, arg))
        shiny::textInput(
          ns(arg), pivot_label(arg),
          value = PIVOT_LONGER_DEFAULTS[[arg]] %||% "", placeholder = placeholder
        )
      }
      # 切成幾段時，每段的欄名在下面填；這一格先收起來（自己寫規則時還是要用）
      show_names_to <- sprintf(
        "!input['%s'] || (input['%s'] || '').trim() || (input['%s'] || '').trim()",
        ns("name_split"), ns("names_prefix"), ns("names_pattern")
      )
      shiny::tagList(
        if (locked("cols")) {
          pivot_locked_ui(ns, "cols")
        } else {
          pivot_cols_ui(ns, shiny::isolate(all_cols()), shiny::isolate(input$cols))
        },
        shiny::tags$div(
          class = "kit-pivot-row",
          shiny::conditionalPanel(show_names_to, text("names_to")),
          text("values_to")
        ),
        if (names_click) pivot_names_panel_ui(ns),
        kit_advanced(
          # 原文：進階設定：空白的值、自己寫規則、R 的參數名稱
          title = "\u9032\u968e\u8a2d\u5b9a\uff1a\u7a7a\u767d\u7684\u503c\u3001\u81ea\u5df1\u5beb\u898f\u5247\u3001R \u7684\u53c3\u6578\u540d\u7a31",
          open = any(c("names_prefix", "names_pattern", "values_drop_na") %in% names(fixed)),
          pivot_longer_arg_map(),
          if (locked("values_drop_na")) {
            pivot_locked_ui(ns, "values_drop_na")
          } else {
            shiny::checkboxInput(ns("values_drop_na"), pivot_label("values_drop_na"), FALSE)
          },
          # 原文：自己寫規則（會寫正規表示式的人才需要）
          shiny::tags$div(class = "fw-semibold mt-2", "\u81ea\u5df1\u5beb\u898f\u5247\uff08\u6703\u5beb\u6b63\u898f\u8868\u793a\u5f0f\u7684\u4eba\u624d\u9700\u8981\uff09"),
          shiny::tags$p(
            class = "kit-hint",
            # 原文：這兩格照 tidyr 的寫法，裡面是正規表示式。. ( ) 這類符號有特別的意思。
            "\u9019\u5169\u683c\u7167 tidyr \u7684\u5beb\u6cd5\uff0c\u88e1\u9762\u662f\u6b63\u898f\u8868\u793a\u5f0f\u3002. ( ) \u9019\u985e\u7b26\u865f\u6709\u7279\u5225\u7684\u610f\u601d\u3002",
            # 原文：填了以後，上面「欄名怎麼變成值」的點選就不用。
            "\u586b\u4e86\u4ee5\u5f8c\uff0c\u4e0a\u9762\u300c\u6b04\u540d\u600e\u9ebc\u8b8a\u6210\u503c\u300d\u7684\u9ede\u9078\u5c31\u4e0d\u7528\u3002",
            # 原文：要切成好幾欄時，「原本的欄名放到哪一欄」用逗號分開寫好幾個名字。
            "\u8981\u5207\u6210\u597d\u5e7e\u6b04\u6642\uff0c\u300c\u539f\u672c\u7684\u6b04\u540d\u653e\u5230\u54ea\u4e00\u6b04\u300d\u7528\u9017\u865f\u5206\u958b\u5beb\u597d\u5e7e\u500b\u540d\u5b57\u3002"
          ),
          # 原文：例如 score_
          text("names_prefix", "\u4f8b\u5982 score_"),
          # 原文：例如 (.*)_t(\\d)
          text("names_pattern", "\u4f8b\u5982 (.*)_t(\\d)")
        )
      )
    })

    # 放在收起來的分頁裡也先畫好，選欄框才收得到欄名
    shiny::outputOptions(output, "controls", suspendWhenHidden = FALSE)

    # 固定的參數：顯示目前的值
    for (arg in names(fixed)) {
      local({
        a <- arg
        output[[paste0("locked_", a)]] <- shiny::renderText(
          pivot_locked_text(a, value_of(fixed[[a]]))
        )
      })
    }

    # cols 的選項跟著資料變；保留還在的選擇
    all_cols <- shiny::reactive({
      df <- data()
      if (is.null(df)) character(0) else names(df)
    })
    shiny::observe({
      if (locked("cols")) return()
      cols <- all_cols()
      keep <- intersect(shiny::isolate(input$cols) %||% character(0), cols)
      shiny::updateSelectizeInput(session, "cols", choices = cols, selected = keep)
    })
    groups <- shiny::reactive(pivot_name_groups(all_cols()))

    # 換模式：標題跟著換；已選的欄換成另一半，轉的欄不變
    last_mode <- "include"
    shiny::observeEvent(input$cols_mode, {
      mode <- input$cols_mode
      if (identical(mode, last_mode)) return()
      last_mode <<- mode
      now <- input$cols %||% character(0)
      flip <- if (length(now) > 0) pivot_order_cols(setdiff(all_cols(), now), all_cols())
      shiny::updateSelectizeInput(
        session, "cols", label = PIVOT_COLS_PICK[[mode]], selected = flip
      )
    })

    # 快速選欄的按鈕：全選、清除、加入一組
    shiny::observeEvent(input$cols_pick, {
      sel <- pivot_quick_pick(input$cols_pick, input$cols, all_cols(), groups())
      shiny::updateSelectizeInput(session, "cols", selected = sel)
    })
    output$cols_quick <- shiny::renderUI(pivot_quick_ui(ns("cols_pick"), groups()))
    output$cols_count <- shiny::renderUI({
      shiny::tags$div(
        class = "small mb-3",
        pivot_cols_count_text(all_cols(), input$cols, identical(input$cols_mode, "exclude"))
      )
    })

    typed <- function(arg) {
      v <- input[[arg]]
      if (is.null(v) || !nzchar(trimws(v))) NULL else trimws(v)
    }

    # ── 欄名怎麼變成值（點選）───────────────────────────────────────────
    # 實際要轉的欄（依資料的欄位順序）
    conv_cols <- shiny::reactive({
      if (locked("cols")) return(value_of(fixed$cols) %||% character(0))
      picked <- pivot_order_cols(input$cols %||% character(0), all_cols())
      if (identical(input$cols_mode, "exclude")) setdiff(all_cols(), picked) else picked
    })
    # 進階設定裡自己寫了規則：以那裡為準
    expert <- shiny::reactive(!is.null(typed("names_prefix")) || !is.null(typed("names_pattern")))
    starts <- shiny::reactive(pivot_name_starts(conv_cols()))
    ends <- shiny::reactive(pivot_name_ends(conv_cols()))
    # 選項變了，舊的選擇不在新的選項裡時，當作「不去掉」
    start <- shiny::reactive(pivot_keep_choice(input$name_start, starts()))
    end <- shiny::reactive(pivot_keep_choice(input$name_end, ends()))
    middles <- shiny::reactive(pivot_name_middles(conv_cols(), start(), end()))
    split_choices <- shiny::reactive({
      m <- middles()
      if (locked("names_to") || !is.null(m$problem)) return(character(0))
      pivot_name_split_choices(m$middles)
    })
    split <- shiny::reactive(pivot_keep_choice(input$name_split, split_choices()))
    split_info <- shiny::reactive({
      s <- split()
      if (!nzchar(s)) NULL else pivot_name_split(middles()$middles, s)
    })

    output$names_cut <- shiny::renderUI({
      cols <- conv_cols()
      if (length(cols) == 0) {
        return(shiny::tags$p(
          # 原文：選了要轉的欄以後，這裡會列出每個欄名都有的開頭、結尾。
          class = "kit-hint", "\u9078\u4e86\u8981\u8f49\u7684\u6b04\u4ee5\u5f8c\uff0c\u9019\u88e1\u6703\u5217\u51fa\u6bcf\u500b\u6b04\u540d\u90fd\u6709\u7684\u958b\u982d\u3001\u7d50\u5c3e\u3002"
        ))
      }
      shiny::tagList(
        pivot_cut_radio(
          # 原文：去掉每個欄名都有的開頭
          ns("name_start"), "\u53bb\u6389\u6bcf\u500b\u6b04\u540d\u90fd\u6709\u7684\u958b\u982d", starts(),
          # 原文：每個欄名的開頭不一樣，沒有可以去掉的。
          shiny::isolate(input$name_start), "\u6bcf\u500b\u6b04\u540d\u7684\u958b\u982d\u4e0d\u4e00\u6a23\uff0c\u6c92\u6709\u53ef\u4ee5\u53bb\u6389\u7684\u3002"
        ),
        pivot_cut_radio(
          # 原文：去掉每個欄名都有的結尾
          ns("name_end"), "\u53bb\u6389\u6bcf\u500b\u6b04\u540d\u90fd\u6709\u7684\u7d50\u5c3e", ends(),
          # 原文：每個欄名的結尾不一樣，沒有可以去掉的。
          shiny::isolate(input$name_end), "\u6bcf\u500b\u6b04\u540d\u7684\u7d50\u5c3e\u4e0d\u4e00\u6a23\uff0c\u6c92\u6709\u53ef\u4ee5\u53bb\u6389\u7684\u3002"
        )
      )
    })
    output$names_split <- shiny::renderUI({
      if (locked("names_to") || length(conv_cols()) == 0) return(NULL)
      choices <- split_choices()
      if (length(choices) == 0) {
        return(shiny::tags$p(
          # 原文：每個欄名裡沒有共同的分隔符號，也不是文字接數字，所以不能切。
          class = "kit-hint", "\u6bcf\u500b\u6b04\u540d\u88e1\u6c92\u6709\u5171\u540c\u7684\u5206\u9694\u7b26\u865f\uff0c\u4e5f\u4e0d\u662f\u6587\u5b57\u63a5\u6578\u5b57\uff0c\u6240\u4ee5\u4e0d\u80fd\u5207\u3002"
        ))
      }
      shiny::radioButtons(
        # 原文：把欄名切成幾段
        ns("name_split"), "\u628a\u6b04\u540d\u5207\u6210\u5e7e\u6bb5",
        # 原文：不切
        choiceNames = unname(c("\u4e0d\u5207", PIVOT_SPLIT_LABELS[choices])),
        choiceValues = c("", choices),
        selected = pivot_keep_choice(shiny::isolate(input$name_split), choices),
        inline = TRUE
      )
    })

    # 每段放到哪一欄：換了切法才換成預設的名字；段數和切法一樣時保留已經填的
    last_parts <- ""
    output$names_parts <- shiny::renderUI({
      info <- split_info()
      m <- middles()
      can_num <- if (is.null(info)) pivot_middles_numeric(m$middles) else any(info$numeric)
      boxes <- NULL
      if (!is.null(info) && !is.na(info$n)) {
        key <- paste(split(), info$n)
        keep <- identical(key, last_parts)
        last_parts <<- key
        defaults <- pivot_part_default_names(info$digit)
        examples <- purrr::map_chr(seq_len(info$n), function(i) info$parts[[1]][i])
        boxes <- purrr::map(seq_len(info$n), function(i) {
          id <- paste0("part_", i)
          now <- if (keep) shiny::isolate(input[[id]])
          shiny::textInput(
            # 原文：第  |  段放到哪一欄（例如  | ）
            ns(id), paste0("\u7b2c ", i, " \u6bb5\u653e\u5230\u54ea\u4e00\u6b04\uff08\u4f8b\u5982 ", examples[i], "\uff09"),
            value = now %||% defaults[i]
          )
        })
      }
      shiny::tagList(
        if (length(boxes) > 0) shiny::tags$div(class = "kit-pivot-row", boxes),
        if (isTRUE(can_num)) {
          shiny::checkboxInput(
            # 原文：數字的部分轉成數字（例如 01 → 1）
            ns("names_num"), "\u6578\u5b57\u7684\u90e8\u5206\u8f49\u6210\u6578\u5b57\uff08\u4f8b\u5982 01 \u2192 1\uff09",
            shiny::isolate(isTRUE(input$names_num))
          )
        }
      )
    })

    # 點選的設定 → tidyr 參數；有問題時 problem 是給使用者看的文字
    click <- shiny::reactive({
      m <- middles()
      info <- split_info()
      if (!is.null(m$problem)) return(list(args = NULL, problem = m$problem))
      if (!is.null(info$problem)) return(list(args = NULL, problem = info$problem))
      if (is.null(info)) {
        names_to <- pivot_names_to_typed(typed("names_to"))
        numeric <- pivot_middles_numeric(m$middles)
      } else {
        defaults <- pivot_part_default_names(info$digit)
        names_to <- purrr::map_chr(seq_len(info$n), function(i) {
          typed(paste0("part_", i)) %||% defaults[i]
        })
        numeric <- info$numeric
      }
      if (anyDuplicated(names_to) > 0) {
        return(list(args = NULL, problem = paste0(
          # 原文：每一段要放到不同名字的欄；現在有重複的名字：
          "\u6bcf\u4e00\u6bb5\u8981\u653e\u5230\u4e0d\u540c\u540d\u5b57\u7684\u6b04\uff1b\u73fe\u5728\u6709\u91cd\u8907\u7684\u540d\u5b57\uff1a",
          # 原文：。
          kit_name_list(unique(names_to[duplicated(names_to)])), "\u3002"
        )))
      }
      to_integer <- isTRUE(input$names_num) & numeric
      args <- pivot_names_args(start(), end(), split(), info, names_to, to_integer)
      list(args = args, problem = NULL)
    })

    out <- shiny::reactive({
      args <- PIVOT_LONGER_DEFAULTS
      if (!locked("cols")) {
        # 依資料的欄位順序（和 starts_with()、!c() 選出來的順序一樣）
        args$cols <- pivot_order_cols(input$cols %||% character(0), all_cols())
        args$cols_exclude <- identical(input$cols_mode, "exclude")
      }
      names_to <- typed("names_to")
      if (!is.null(names_to)) args$names_to <- pivot_names_to_typed(names_to)
      args$values_to <- typed("values_to") %||% args$values_to
      args$names_prefix <- typed("names_prefix")
      args$names_pattern <- typed("names_pattern")
      args$values_drop_na <- isTRUE(input$values_drop_na)
      if (names_click && !expert() && length(conv_cols()) > 0) {
        p <- click()
        if (!is.null(p$args)) {
          args[names(p$args)] <- p$args
          by_app <- c(args$names_prefix, args$names_sep, args$names_pattern)
          args$names_by_app <- length(by_app) > 0
        }
        args["names_problem"] <- list(p$problem)
      }
      for (a in names(fixed)) args[[a]] <- value_of(fixed[[a]])
      args
    })

    # 預覽：每個欄名會變成什麼（用 tidyr 自己算）；有問題時說明，並擋住執行
    output$names_preview <- shiny::renderUI({
      cols <- conv_cols()
      if (length(cols) == 0) return(NULL)
      a <- utils::modifyList(PIVOT_LONGER_DEFAULTS, out())
      empty <- tibble::as_tibble(
        stats::setNames(rep(list(logical(0)), length(all_cols())), all_cols()),
        .name_repair = "minimal"
      )
      problem <- a$names_problem %||% pivot_longer_problem(empty, cols, a, names_check = FALSE)
      chk <- if (is.null(problem)) pivot_names_check(cols, a)
      problem <- problem %||% chk$problem
      shiny::tagList(
        if (expert()) {
          shiny::tags$p(
            class = "small text-warning",
            # 原文：進階設定的「自己寫規則」有填，所以用那裡的規則；上面的點選暫時不用。
            "\u9032\u968e\u8a2d\u5b9a\u7684\u300c\u81ea\u5df1\u5beb\u898f\u5247\u300d\u6709\u586b\uff0c\u6240\u4ee5\u7528\u90a3\u88e1\u7684\u898f\u5247\uff1b\u4e0a\u9762\u7684\u9ede\u9078\u66ab\u6642\u4e0d\u7528\u3002"
          )
        },
        if (!is.null(chk$preview)) {
          shiny::tagList(
            # 原文：預覽：欄名會變成這樣
            shiny::tags$div(class = "small fw-semibold", "\u9810\u89bd\uff1a\u6b04\u540d\u6703\u8b8a\u6210\u9019\u6a23"),
            shiny::tags$div(
              class = "kit-table-scroll",
              kit_static_table(utils::head(chk$preview, PIVOT_NAMES_PREVIEW))
            ),
            if (length(cols) > PIVOT_NAMES_PREVIEW) {
              shiny::tags$p(class = "kit-hint", paste0(
                # 原文：共  |  欄，只列前  |  欄。
                "\u5171 ", kit_num(length(cols)), " \u6b04\uff0c\u53ea\u5217\u524d ", PIVOT_NAMES_PREVIEW, " \u6b04\u3002"
              ))
            }
          )
        },
        if (length(chk$unmatched) > 0) {
          shiny::tags$p(class = "small text-warning", paste0(
            # 原文：這些欄名對不上規則，值會是空白： | 。
            "\u9019\u4e9b\u6b04\u540d\u5c0d\u4e0d\u4e0a\u898f\u5247\uff0c\u503c\u6703\u662f\u7a7a\u767d\uff1a", kit_name_list(chk$unmatched), "\u3002"
          ))
        },
        if (!is.null(problem)) shiny::tags$p(class = "small text-danger", problem)
      )
    })

    out
  })
}

# 預覽表最多列幾欄
PIVOT_NAMES_PREVIEW <- 10L

# 切法的說明（代號 → 選項文字）
PIVOT_SPLIT_LABELS <- c(
  # 原文：用底線 _ 切
  "_" = "\u7528\u5e95\u7dda _ \u5207",
  # 原文：用點 . 切
  "." = "\u7528\u9ede . \u5207",
  # 原文：用減號 - 切
  "-" = "\u7528\u6e1b\u865f - \u5207",
  # 原文：用空白切
  " " = "\u7528\u7a7a\u767d\u5207",
  # 原文：文字和數字分開
  digits = "\u6587\u5b57\u548c\u6578\u5b57\u5206\u958b"
)

# 「原本的欄名放到哪一欄」填的字：逗號分開的好幾個名字
pivot_names_to_typed <- function(x) {
  if (is.null(x)) return("name")
  trimws(strsplit(x, ",")[[1]])
}

# 選項變了：舊的選擇還在就留著，不在就用 ""（不去掉、不切）
pivot_keep_choice <- function(now, choices) {
  if (!is.null(now) && nzchar(now) && now %in% choices) now else ""
}

#' 「欄名怎麼變成值」的畫面（放在主畫面，不收在進階設定）
#' @noRd
pivot_names_panel_ui <- function(ns) {
  shiny::tags$div(
    class = "kit-pivot-names mb-3",
    # 原文：欄名怎麼變成值
    shiny::tags$div(class = "fw-semibold", "\u6b04\u540d\u600e\u9ebc\u8b8a\u6210\u503c"),
    shiny::tags$p(
      class = "kit-hint",
      # 原文：原本的欄名會變成「原本的欄名放到哪一欄」裡的值。
      "\u539f\u672c\u7684\u6b04\u540d\u6703\u8b8a\u6210\u300c\u539f\u672c\u7684\u6b04\u540d\u653e\u5230\u54ea\u4e00\u6b04\u300d\u88e1\u7684\u503c\u3002",
      # 原文：可以去掉每個欄名都有的開頭、結尾，也可以把欄名切成幾段。都用點的，不用寫規則。
      "\u53ef\u4ee5\u53bb\u6389\u6bcf\u500b\u6b04\u540d\u90fd\u6709\u7684\u958b\u982d\u3001\u7d50\u5c3e\uff0c\u4e5f\u53ef\u4ee5\u628a\u6b04\u540d\u5207\u6210\u5e7e\u6bb5\u3002\u90fd\u7528\u9ede\u7684\uff0c\u4e0d\u7528\u5beb\u898f\u5247\u3002"
    ),
    shiny::uiOutput(ns("names_cut")),
    shiny::uiOutput(ns("names_split")),
    shiny::uiOutput(ns("names_parts")),
    shiny::uiOutput(ns("names_preview"))
  )
}

#' 去掉開頭 / 結尾的選項：「不去掉」加上每個候選（用 <code> 顯示，空白也看得到）
#' @noRd
pivot_cut_radio <- function(id, label, candidates, selected, none_text) {
  if (length(candidates) == 0) return(shiny::tags$p(class = "kit-hint", none_text))
  shiny::radioButtons(
    id, label,
    # 原文：不去掉
    choiceNames = c(list("\u4e0d\u53bb\u6389"), purrr::map(candidates, shiny::tags$code)),
    choiceValues = c("", candidates),
    selected = pivot_keep_choice(selected, candidates),
    inline = TRUE
  )
}

# 選欄框的標題（跟著模式換）
PIVOT_COLS_PICK <- c(
  # 原文：要轉的欄（點選，可多選）
  include = "\u8981\u8f49\u7684\u6b04\uff08\u9ede\u9078\uff0c\u53ef\u591a\u9078\uff09",
  # 原文：不轉的欄（通常是 id、性別這類每人一個值的欄；其餘全部轉）
  exclude = "\u4e0d\u8f49\u7684\u6b04\uff08\u901a\u5e38\u662f id\u3001\u6027\u5225\u9019\u985e\u6bcf\u4eba\u4e00\u500b\u503c\u7684\u6b04\uff1b\u5176\u9918\u5168\u90e8\u8f49\uff09"
)

# cols：全部用點選。挑要轉的欄，或挑不轉的欄（其餘全部轉）
pivot_cols_ui <- function(ns, choices = NULL, selected = NULL) {
  shiny::tagList(
    shiny::radioButtons(
      ns("cols_mode"), pivot_label("cols"),
      # 原文：挑要轉的欄 | 挑不轉的欄（其餘全部轉）
      choices = c("\u6311\u8981\u8f49\u7684\u6b04" = "include", "\u6311\u4e0d\u8f49\u7684\u6b04\uff08\u5176\u9918\u5168\u90e8\u8f49\uff09" = "exclude"),
      inline = TRUE
    ),
    shiny::selectizeInput(
      ns("cols"), PIVOT_COLS_PICK[["include"]], choices = choices, selected = selected,
      multiple = TRUE,
      width = "100%",
      # 原文：點這裡選欄
      options = list(placeholder = "\u9ede\u9019\u88e1\u9078\u6b04", plugins = list("remove_button"))
    ),
    shiny::uiOutput(ns("cols_quick")),
    shiny::uiOutput(ns("cols_count"))
  )
}

# 快速選欄的按鈕；按下時送出 cols_pick = "all"、"none" 或 "g1"、"g2"…
pivot_quick_ui <- function(input_id, groups) {
  btn <- function(key, label) {
    shiny::tags$button(
      type = "button", class = paste("btn", kit_btn_class("secondary")), style = "text-transform: none;",
      onclick = sprintf(
        "Shiny.setInputValue('%s', '%s', {priority: 'event'})", input_id, key
      ),
      label
    )
  }
  group_btns <- purrr::map(seq_len(nrow(groups)), function(i) {
    # 原文： 開頭（ |  欄）
    label <- paste0(groups$prefix[i], " \u958b\u982d\uff08", kit_num(groups$n[i]), " \u6b04\uff09")
    btn(paste0("g", i), label)
  })
  shiny::tags$div(
    class = "d-flex flex-wrap align-items-center gap-1 mb-2",
    # 原文：快速選：
    shiny::tags$span(class = "small text-muted me-1", "\u5feb\u901f\u9078\uff1a"),
    # 原文：全選 | 清除
    btn("all", "\u5168\u9078"), btn("none", "\u6e05\u9664"),
    if (length(group_btns) > 0) {
      # 原文：加入一組：
      shiny::tags$span(class = "small text-muted ms-2 me-1", "\u52a0\u5165\u4e00\u7d44\uff1a")
    },
    group_btns
  )
}

#' 快速選欄：按鈕代號 → 新的選擇（依資料的欄位順序）
#' @param key "all"、"none" 或 "g1"、"g2"…（第幾組）
#' @param now 目前的選擇
#' @param all 資料的全部欄名
#' @param groups pivot_name_groups() 的結果
#' @noRd
pivot_quick_pick <- function(key, now, all, groups) {
  now <- now %||% character(0)
  i <- suppressWarnings(as.integer(sub("^g", "", key)))
  sel <- if (identical(key, "all")) {
    all
  } else if (identical(key, "none")) {
    character(0)
  } else if (!is.na(i) && i >= 1 && i <= nrow(groups)) {
    union(now, groups$cols[[i]])
  } else {
    now
  }
  pivot_order_cols(sel, all)
}

# 依資料的欄位順序排好（只留資料裡有的欄）
pivot_order_cols <- function(cols, all) {
  if (length(all) == 0) return(cols %||% character(0))
  all[all %in% cols]
}

#' 選欄的效果：「會轉 N 欄，保留 M 欄不動」（文字）
#' @noRd
pivot_cols_count_text <- function(all, picked, exclude) {
  picked <- picked %||% character(0)
  # 原文：還沒有資料。
  if (length(all) == 0) return("\u9084\u6c92\u6709\u8cc7\u6599\u3002")
  conv <- if (exclude) setdiff(all, picked) else intersect(all, picked)
  keep <- setdiff(all, conv)
  if (length(conv) == 0) {
    # 原文：每一欄都挑成不轉了，沒有欄可以轉。 | 還沒選要轉的欄。
    return(if (exclude) "\u6bcf\u4e00\u6b04\u90fd\u6311\u6210\u4e0d\u8f49\u4e86\uff0c\u6c92\u6709\u6b04\u53ef\u4ee5\u8f49\u3002" else "\u9084\u6c92\u9078\u8981\u8f49\u7684\u6b04\u3002")
  }
  few <- function(x, n) {
    # 原文：（ | 、 | … | ）
    paste0("\uff08", paste(utils::head(x, n), collapse = "\u3001"), if (length(x) > n) "\u2026", "\uff09")
  }
  paste0(
    # 原文：會轉  |  欄 | ，
    "\u6703\u8f49 ", kit_num(length(conv)), " \u6b04", few(conv, 4), "\uff0c",
    # 原文：保留  |  欄不動 | 。
    "\u4fdd\u7559 ", kit_num(length(keep)), " \u6b04\u4e0d\u52d5", if (length(keep) > 0) few(keep, 6), "\u3002"
  )
}

#' 欄名分組：開頭相同的欄（例如 lan01…lan10、score_lan、score_cog）
#'
#' 每欄的開頭 = 第一個數字之前，或到第一個「_」為止（含 _），取先到的。
#' 一組 = 欄名以這個開頭開始的所有欄（不分大小寫，和 tidyselect 的 starts_with() 一樣）。
#' 只留 2 欄以上的組；成員一樣的組只留一個；欄多的在前（一樣多時照欄位順序）。
#' @param cols 欄名
#' @param max_groups 最多幾組
#' @return tibble(prefix, n, cols)；cols 是 list，成員依原本的欄位順序
#' @noRd
pivot_name_groups <- function(cols, max_groups = 8L) {
  empty <- tibble::tibble(prefix = character(0), n = integer(0), cols = list())
  heads <- sub("^([^0-9_]*_?).*$", "\\1", cols)
  prefixes <- unique(heads[nzchar(heads) & heads != cols])
  prefixes <- prefixes[!duplicated(tolower(prefixes))]
  if (length(prefixes) == 0) return(empty)
  low <- tolower(cols)
  members <- purrr::map(prefixes, function(p) cols[startsWith(low, tolower(p))])
  n <- lengths(members)
  keep <- n >= 2 & !duplicated(members)
  if (!any(keep)) return(empty)
  out <- tibble::tibble(prefix = prefixes[keep], n = n[keep], cols = members[keep])
  out <- out[order(-out$n, seq_len(nrow(out))), ]
  utils::head(out, max_groups)
}

# 固定的參數：中文標題 + 目前的值（不能改）
pivot_locked_ui <- function(ns, arg) {
  shiny::tags$div(
    class = "kit-pivot-locked",
    shiny::tags$label(pivot_label(arg)),
    shiny::tags$div(
      class = "text-muted",
      shiny::textOutput(ns(paste0("locked_", arg)), inline = TRUE)
    )
  )
}

# 固定參數的顯示文字
pivot_locked_text <- function(arg, value) {
  if (arg == "cols") {
    # 原文：（還沒有題目欄）
    if (length(value) == 0) return("\uff08\u9084\u6c92\u6709\u984c\u76ee\u6b04\uff09")
    # 原文：…（共  |  欄）
    more <- if (length(value) > 6) paste0("\u2026\uff08\u5171 ", kit_num(length(value)), " \u6b04\uff09") else ""
    # 原文：、 | 　固定
    return(paste0(paste(utils::head(value, 6), collapse = "\u3001"), more, "\u3000\u56fa\u5b9a"))
  }
  # 原文：（不用）　固定
  if (is.null(value)) return("\uff08\u4e0d\u7528\uff09\u3000\u56fa\u5b9a")
  shown <- if (is.character(value)) paste0("\"", value, "\"", collapse = ", ") else
    paste(value, collapse = ", ")
  # 原文：　固定
  paste0(shown, "\u3000\u56fa\u5b9a")
}


# ── 執行與程式碼 ─────────────────────────────────────────────────────────────

#' 依參數執行 tidyr::pivot_longer()
#'
#' cols_exclude = TRUE 時 cols 是不轉的欄。轉換前先檢查：沒有欄、新欄名和留下的欄撞名、
#' 好幾個 names_to 卻沒有 names_pattern。要轉的欄的標籤先整理（pivot_longer_labels()）：
#' 數值標籤不一樣時拿掉，不會把第一欄的標籤套到每一題。tidyr 的錯誤改成中文；
#' tidyr 的警告存在結果的 "kit_warnings" 屬性（reshape_longer() 會顯示）。
#' @noRd
pivot_longer_run <- function(df, args) {
  args <- utils::modifyList(PIVOT_LONGER_DEFAULTS, args)
  cols <- pivot_longer_cols(df, args)
  problem <- pivot_longer_problem(df, cols, args)
  if (!is.null(problem)) kit_abort(problem)
  df <- pivot_longer_labels(df, cols)$data
  caught <- character(0)
  out <- tryCatch(
    withCallingHandlers(
      tidyr::pivot_longer(
        df, cols = dplyr::all_of(cols), names_to = args$names_to, values_to = args$values_to,
        names_prefix = args$names_prefix, names_sep = args$names_sep,
        names_pattern = args$names_pattern, names_transform = args$names_transform,
        values_drop_na = isTRUE(args$values_drop_na)
      ),
      warning = function(w) {
        caught <<- c(caught, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      msg <- conditionMessage(e)
      hint <- if (grepl("combine|incompatible|type", msg, ignore.case = TRUE)) {
        # 原文：（常見原因：選到的欄有的是數字、有的是文字。請只選同一種型別的欄。）
        "\uff08\u5e38\u898b\u539f\u56e0\uff1a\u9078\u5230\u7684\u6b04\u6709\u7684\u662f\u6578\u5b57\u3001\u6709\u7684\u662f\u6587\u5b57\u3002\u8acb\u53ea\u9078\u540c\u4e00\u7a2e\u578b\u5225\u7684\u6b04\u3002\uff09"
      }
      # 原文：寬轉長沒有成功：
      kit_abort("\u5bec\u8f49\u9577\u6c92\u6709\u6210\u529f\uff1a", msg, hint)
    }
  )
  if (length(caught) > 0) attr(out, "kit_warnings") <- unique(caught)
  # 測量水準（SPSS）：合在一起的值欄沿用原本那些欄的（不同時用第一欄的）
  if (args$values_to %in% names(out)) {
    from <- stats::setNames(list(cols), args$values_to)
    out <- restore_measures(out, list(df), args$values_to, from)$df
  }
  out
}

#' 寬轉長前整理標籤（SPSS 檔的值標籤、變項標籤）
#'
#' 要轉的欄放進同一個值欄，只能有一套標籤：
#' - 數值標籤：每一欄都一樣時保留；不一樣（或有的欄有、有的沒有）時全部拿掉，值不變
#'   （不然 tidyr 會把第一欄的標籤套到每一題）。
#' - 變項標籤（label）：每一欄都一樣時保留；不一樣時拿掉（值欄不是任何一題）。
#' @return list(data, conflict = 數值標籤不一樣時 TRUE, zap_var = 拿掉變項標籤時 TRUE,
#'   cols = 有標籤的欄, warning = 給使用者看的提醒（沒有時 NULL）, examples)
#' @noRd
pivot_longer_labels <- function(df, cols) {
  cols <- intersect(cols, names(df))
  vlab <- lapply(df[cols], function(x) attr(x, "labels", exact = TRUE))
  var_lab <- lapply(df[cols], function(x) attr(x, "label", exact = TRUE))
  has_v <- !vapply(vlab, is.null, logical(1))
  has_l <- !vapply(var_lab, is.null, logical(1))
  out <- list(data = df, conflict = FALSE, zap_var = FALSE, cols = character(0),
              warning = NULL)
  if (!any(has_v) && !any(has_l)) return(out)
  same <- function(x) all(vapply(x, identical, logical(1), x[[1]]))
  if (any(has_v) && !(all(has_v) && same(vlab))) {
    out$conflict <- TRUE
    out$cols <- cols[has_v]
    df[cols[has_v]] <- lapply(df[cols[has_v]], haven::zap_labels)
    sets <- unique(vlab[has_v])
    first <- cols[has_v][match(sets, vlab[has_v])]
    show <- function(lab) {
      txt <- paste0(format(unname(lab), trim = TRUE, scientific = FALSE), "=", names(lab))
      # 原文：、 | …
      paste0(paste(utils::head(txt, 3), collapse = "\u3001"), if (length(txt) > 3) "\u2026")
    }
    # 原文：（ | ）
    examples <- paste0(utils::head(first, 2), "\uff08", vapply(utils::head(sets, 2), show, ""), "\uff09")
    out$warning <- paste0(
      # 原文：要轉的  |  欄有  |  種不同的數值標籤（例如 
      "\u8981\u8f49\u7684 ", length(cols), " \u6b04\u6709 ", length(sets) + (!all(has_v)), " \u7a2e\u4e0d\u540c\u7684\u6578\u503c\u6a19\u7c64\uff08\u4f8b\u5982 ",
      # 原文：； | ；有的欄沒有數值標籤 | ）。
      paste(examples, collapse = "\uff1b"), if (!all(has_v)) "\uff1b\u6709\u7684\u6b04\u6c92\u6709\u6578\u503c\u6a19\u7c64", "\uff09\u3002",
      # 原文：放進同一欄會把第一欄的標籤套到每一題，所以值欄不放數值標籤（值不變）。
      "\u653e\u9032\u540c\u4e00\u6b04\u6703\u628a\u7b2c\u4e00\u6b04\u7684\u6a19\u7c64\u5957\u5230\u6bcf\u4e00\u984c\uff0c\u6240\u4ee5\u503c\u6b04\u4e0d\u653e\u6578\u503c\u6a19\u7c64\uff08\u503c\u4e0d\u8b8a\uff09\u3002",
      # 原文：需要標籤的話，請每次只轉標籤相同的欄。
      "\u9700\u8981\u6a19\u7c64\u7684\u8a71\uff0c\u8acb\u6bcf\u6b21\u53ea\u8f49\u6a19\u7c64\u76f8\u540c\u7684\u6b04\u3002"
    )
  }
  if (any(has_l) && !(all(has_l) && same(var_lab))) {
    out$zap_var <- TRUE
    out$cols <- union(out$cols, cols[has_l])
    df[cols[has_l]] <- lapply(df[cols[has_l]], function(x) {
      attr(x, "label") <- NULL
      x
    })
  }
  out$data <- df
  out
}

#' 實際要轉的欄：cols_exclude = TRUE 時是 cols 以外的全部欄（依資料順序），
#' 否則是 cols（只留資料裡有的，順序照 cols）
#' @noRd
pivot_longer_cols <- function(df, args) {
  if (isTRUE(args$cols_exclude)) {
    setdiff(names(df), args$cols)
  } else {
    intersect(args$cols, names(df))
  }
}

#' 轉換前的檢查；沒問題時 NULL
#'
#' 有用欄名規則（names_prefix、names_sep、names_pattern）時，names_check = TRUE 會再查
#' 兩個不同的欄是不是變成一樣的值（一樣的值分不出原本是哪一欄）。
#' @noRd
pivot_longer_problem <- function(df, cols, args, names_check = TRUE) {
  # 原文：請選要轉成長格式的欄。
  if (length(cols) == 0) return("\u8acb\u9078\u8981\u8f49\u6210\u9577\u683c\u5f0f\u7684\u6b04\u3002")
  if (!is.null(args$names_problem)) return(args$names_problem)
  new_cols <- c(args$names_to, args$values_to)
  if (any(!nzchar(new_cols))) {
    return(paste0(
      # 原文：「 | 」與「
      "\u300c", PIVOT_LONGER_LABELS[["names_to"]], "\u300d\u8207\u300c", PIVOT_LONGER_LABELS[["values_to"]],
      # 原文：」不能空白。
      "\u300d\u4e0d\u80fd\u7a7a\u767d\u3002"
    ))
  }
  clash <- intersect(new_cols, setdiff(names(df), cols))
  if (length(clash) > 0) {
    # 原文：新的欄名  | 、 |  和留下來的欄撞名了，請換一個名字。
    return(paste0("\u65b0\u7684\u6b04\u540d ", paste(clash, collapse = "\u3001"), " \u548c\u7559\u4e0b\u4f86\u7684\u6b04\u649e\u540d\u4e86\uff0c\u8acb\u63db\u4e00\u500b\u540d\u5b57\u3002"))
  }
  if (length(args$names_to) > 1 && is.null(args$names_pattern) && is.null(args$names_sep)) {
    return(paste0(
      # 原文：「 | 」填了好幾個名稱時，
      "\u300c", PIVOT_LONGER_LABELS[["names_to"]], "\u300d\u586b\u4e86\u597d\u5e7e\u500b\u540d\u7a31\u6642\uff0c",
      # 原文：要在「欄名怎麼變成值」選一種切法，
      "\u8981\u5728\u300c\u6b04\u540d\u600e\u9ebc\u8b8a\u6210\u503c\u300d\u9078\u4e00\u7a2e\u5207\u6cd5\uff0c",
      # 原文：或在進階設定填「從欄名取出部分的規則」（names_pattern）。
      "\u6216\u5728\u9032\u968e\u8a2d\u5b9a\u586b\u300c\u5f9e\u6b04\u540d\u53d6\u51fa\u90e8\u5206\u7684\u898f\u5247\u300d\uff08names_pattern\uff09\u3002"
    ))
  }
  uses_rule <- !is.null(args$names_prefix) || !is.null(args$names_sep) ||
    !is.null(args$names_pattern)
  if (names_check && uses_rule) return(pivot_names_check(cols, args)$problem)
  NULL
}

#' 寬轉長前拿掉標籤的程式碼（pivot_longer_labels() 有拿掉時；沒有時 NULL）
#' @param labels pivot_longer_labels() 的結果
#' @noRd
pivot_longer_labels_code <- function(labels, data_name) {
  if (is.null(labels) || length(labels$cols) == 0) return(NULL)
  fun <- if (labels$conflict && labels$zap_var) {
    "\\(x) haven::zap_label(haven::zap_labels(x))"
  } else if (labels$conflict) {
    "haven::zap_labels"
  } else {
    "haven::zap_label"
  }
  code_lines(
    if (labels$conflict) {
      # 原文：# 要轉的欄數值標籤不一樣：先拿掉，才不會把第一欄的標籤套到每一題（值不變）
      "# \u8981\u8f49\u7684\u6b04\u6578\u503c\u6a19\u7c64\u4e0d\u4e00\u6a23\uff1a\u5148\u62ff\u6389\uff0c\u624d\u4e0d\u6703\u628a\u7b2c\u4e00\u6b04\u7684\u6a19\u7c64\u5957\u5230\u6bcf\u4e00\u984c\uff08\u503c\u4e0d\u8b8a\uff09"
    } else {
      # 原文：# 要轉的欄變項標籤不一樣：先拿掉（值欄不是任何一題）
      "# \u8981\u8f49\u7684\u6b04\u8b8a\u9805\u6a19\u7c64\u4e0d\u4e00\u6a23\uff1a\u5148\u62ff\u6389\uff08\u503c\u6b04\u4e0d\u662f\u4efb\u4f55\u4e00\u984c\uff09"
    },
    paste0(data_name, " <- ", data_name, " |>"),
    paste0("  mutate(across(", r_cols_vector(labels$cols, indent = "      "), ", ", fun, "))"),
    ""
  )
}

#' pivot_longer() 的程式碼：只寫和 tidyr 預設不同的參數（cols 一定寫）
#'
#' @param args mod_pivot_longer_server() 的參數 list
#' @param data_name 資料物件名稱
#' @param cols_code NULL 或 cols 的寫法（例如 "any_of(items)"）；NULL 時依 args 產生
#' @param col_names NULL 或資料的全部欄名；有給時，選到的剛好是整組欄就寫 starts_with()
#' @return 字串（多行）
#' @noRd
pivot_longer_code <- function(args, data_name, cols_code = NULL, col_names = NULL) {
  args <- utils::modifyList(PIVOT_LONGER_DEFAULTS, args)
  d <- PIVOT_LONGER_DEFAULTS
  cols_code <- cols_code %||% pivot_cols_code(args, col_names)
  str_arg <- function(name) {
    v <- args[[name]]
    if (is.null(v) || identical(v, d[[name]])) return(NULL)
    paste0(name, " = ", r_str(v))
  }
  rules <- c("names_prefix", "names_sep", "names_pattern")
  rule_parts <- purrr::map(rules, str_arg)
  transform <- names(args$names_transform)
  parts <- c(
    paste0("cols = ", cols_code),
    str_arg("names_to"), str_arg("values_to"),
    unlist(rule_parts),
    if (length(transform) > 0) {
      paste0("names_transform = list(", paste0(r_col(transform), " = as.integer",
                                               collapse = ", "), ")")
    },
    if (isTRUE(args$values_drop_na)) "values_drop_na = TRUE"
  )
  lines <- paste0("    ", parts, c(rep(",", length(parts) - 1), ""))
  # app 依點選寫出的規則：上面加一行說明
  used <- rules[lengths(rule_parts) > 0]
  if (isTRUE(args$names_by_app) && length(used) > 0) {
    at <- match(unlist(rule_parts)[1], parts)
    escaped <- any(grepl("\\", unlist(args[used]), fixed = TRUE))
    note <- paste0(
      # 原文：、 |  是 app 依你點的設定寫出來的
      "    # ", paste(used, collapse = "\u3001"), " \u662f app \u4f9d\u4f60\u9ede\u7684\u8a2d\u5b9a\u5beb\u51fa\u4f86\u7684",
      # 原文：；\\\\. 這種寫法是符號本身
      if (escaped) "\uff1b\\\\. \u9019\u7a2e\u5beb\u6cd5\u662f\u7b26\u865f\u672c\u8eab"
    )
    lines <- append(lines, note, after = at - 1)
  }
  code_lines(
    paste0(data_name, " |>"),
    "  pivot_longer(",
    lines,
    "  )"
  )
}

#' cols 的寫法
#'
#' 不轉的欄：!c(id, sex)（沒挑就是 everything()）。要轉的欄：剛好是整組欄時寫
#' starts_with("score_")，否則逐欄列出 c(a, b)。starts_with() 先用資料的欄名試選一次，
#' 選出來的欄和順序都一樣才用。
#' @noRd
pivot_cols_code <- function(args, col_names = NULL) {
  cols <- args$cols %||% character(0)
  if (!is.null(col_names)) cols <- intersect(cols, col_names)
  if (isTRUE(args$cols_exclude)) {
    if (length(cols) == 0) return("everything()")
    return(paste0("!", if (length(cols) == 1) r_col(cols) else r_cols_vector(cols)))
  }
  pivot_cols_by_group(cols, col_names) %||% r_cols_vector(cols)
}

# 選到的欄剛好是幾組整組欄時：starts_with() 的寫法；不是時 NULL
pivot_cols_by_group <- function(cols, col_names) {
  if (is.null(col_names) || length(cols) == 0) return(NULL)
  groups <- pivot_name_groups(col_names, max_groups = Inf)
  groups <- groups[purrr::map_lgl(groups$cols, function(g) all(g %in% cols)), ]
  picked <- integer(0)
  covered <- character(0)
  for (i in seq_len(nrow(groups))) {
    if (length(setdiff(groups$cols[[i]], covered)) == 0) next
    picked <- c(picked, i)
    covered <- union(covered, groups$cols[[i]])
  }
  if (length(picked) == 0 || !setequal(covered, cols)) return(NULL)
  # 照每組第一欄在資料中的位置排
  first <- purrr::map_int(groups$cols[picked], function(g) match(g[1], col_names))
  calls <- paste0("starts_with(", r_str_each(groups$prefix[picked[order(first)]]), ")")
  code <- if (length(calls) == 1) calls else r_wrap_c(calls)
  # 用資料的欄名試選一次：欄和順序都一樣才用
  empty <- stats::setNames(rep(list(logical(0)), length(col_names)), col_names)
  template <- tibble::as_tibble(empty, .name_repair = "minimal")
  got <- tryCatch(
    names(dplyr::select(template, !!str2lang(code))),
    error = function(e) NULL
  )
  if (identical(got, cols)) code else NULL
}

# 每個字串各自寫成 "x"
r_str_each <- function(x) vapply(x, r_str, character(1), USE.NAMES = FALSE)

#' 欄名向量的寫法：c(a, b, c)，太長時換行（每行不超過 100 字）
#' @param quote TRUE 時寫成文字 c("a", "b")
#' @noRd
r_cols_vector <- function(cols, indent = "      ", quote = FALSE) {
  if (length(cols) == 0) return("c()")
  items <- if (quote) paste0('"', cols, '"') else r_col(cols)
  r_wrap_c(items, indent)
}

# c(a, b, c)：太長時換行
r_wrap_c <- function(items, indent = "      ") {
  one <- paste0("c(", paste(items, collapse = ", "), ")")
  if (nchar(one) <= 70) return(one)
  lines <- character(0)
  line <- ""
  for (x in items) {
    piece <- if (nzchar(line)) paste0(line, ", ", x) else x
    if (nchar(piece) > 90 - nchar(indent)) {
      lines <- c(lines, paste0(line, ","))
      line <- x
    } else {
      line <- piece
    }
  }
  lines <- c(lines, line)
  paste0("c(\n", paste0(indent, lines, collapse = "\n"), "\n", substr(indent, 3, 100), ")")
}
