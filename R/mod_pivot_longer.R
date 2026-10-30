# 共用元件：pivot_longer() 的設定（寬轉長）
#
# 每個控制項對應 tidyr::pivot_longer() 的一個參數，畫面上寫中文與參數名稱；
# 預設值和 tidyr 相同。⑥ 轉成 kits 作答檔用它（參數全部固定），⑦ 長寬互轉也用它。
#
# 介面：
#   mod_pivot_longer_ui(id)
#   mod_pivot_longer_server(id, data, fixed = list())  → reactive：參數 list
#   pivot_longer_code(args, data_name, cols_code = NULL) → 程式碼字串
#   pivot_longer_run(df, args)                          → 長格式資料

# tidyr::pivot_longer() 的預設值（cols 沒有預設，一定要給）
PIVOT_LONGER_DEFAULTS <- list(
  cols = character(0),
  cols_pattern = NULL,
  names_to = "name",
  values_to = "value",
  names_prefix = NULL,
  names_pattern = NULL,
  values_drop_na = FALSE
)

# 控制項的中文名稱
PIVOT_LONGER_LABELS <- c(
  cols = "要轉成長格式的欄",
  names_to = "原本的欄名放到哪一欄",
  values_to = "原本的值放到哪一欄",
  names_prefix = "欄名要去掉的開頭",
  names_pattern = "從欄名取出部分的規則",
  values_drop_na = "去掉空白的值"
)

#' 控制項的標題：「中文（參數名稱）」
#' @noRd
pivot_label <- function(arg, labels = PIVOT_LONGER_LABELS) {
  shiny::tagList(labels[[arg]], shiny::tags$code(paste0("（", arg, "）")))
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
#' @return reactive：參數 list（names(PIVOT_LONGER_DEFAULTS)），沒改的用 tidyr 的預設
#' @noRd
mod_pivot_longer_server <- function(id, data, fixed = list()) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    value_of <- function(x) if (is.function(x)) x() else x
    locked <- function(arg) arg %in% names(fixed)

    output$controls <- shiny::renderUI({
      text <- function(arg, placeholder = NULL) {
        if (locked(arg)) return(pivot_locked_ui(ns, arg))
        shiny::textInput(
          ns(arg), pivot_label(arg),
          value = PIVOT_LONGER_DEFAULTS[[arg]] %||% "", placeholder = placeholder
        )
      }
      shiny::tagList(
        if (locked("cols")) pivot_locked_ui(ns, "cols") else pivot_cols_ui(ns),
        shiny::tags$div(class = "kit-pivot-row", text("names_to"), text("values_to")),
        shiny::tags$details(
          class = "kit-pivot-advanced",
          open = if (any(c("names_prefix", "names_pattern", "values_drop_na") %in% names(fixed))) {
            NA
          },
          shiny::tags$summary("進階參數"),
          text("names_prefix", "例如 score_"),
          text("names_pattern", "例如 (.*)_t(\\d)"),
          if (locked("values_drop_na")) {
            pivot_locked_ui(ns, "values_drop_na")
          } else {
            shiny::checkboxInput(ns("values_drop_na"), pivot_label("values_drop_na"), FALSE)
          }
        )
      )
    })

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
    shiny::observe({
      if (locked("cols")) return()
      df <- data()
      cols <- if (is.null(df)) character(0) else names(df)
      keep <- intersect(shiny::isolate(input$cols) %||% character(0), cols)
      shiny::updateSelectizeInput(session, "cols", choices = cols, selected = keep)
    })

    shiny::reactive({
      typed <- function(arg) {
        v <- input[[arg]]
        if (is.null(v) || !nzchar(trimws(v))) NULL else trimws(v)
      }
      args <- PIVOT_LONGER_DEFAULTS
      if (identical(input$cols_mode, "pattern")) {
        args$cols_pattern <- typed("cols_pattern")
      } else {
        args$cols <- input$cols %||% character(0)
      }
      names_to <- typed("names_to")
      if (!is.null(names_to)) args$names_to <- trimws(strsplit(names_to, ",")[[1]])
      args$values_to <- typed("values_to") %||% args$values_to
      args$names_prefix <- typed("names_prefix")
      args$names_pattern <- typed("names_pattern")
      args$values_drop_na <- isTRUE(input$values_drop_na)
      for (a in names(fixed)) args[[a]] <- value_of(fixed[[a]])
      args
    })
  })
}

# cols：逐欄挑選，或用欄名規則（tidyselect 的 matches()）
pivot_cols_ui <- function(ns) {
  shiny::tagList(
    shiny::radioButtons(
      ns("cols_mode"), pivot_label("cols"),
      choices = c("逐欄挑選" = "list", "用欄名規則挑（matches()）" = "pattern"),
      inline = TRUE
    ),
    shiny::conditionalPanel(
      "input.cols_mode == 'list'", ns = ns,
      shiny::selectizeInput(ns("cols"), NULL, choices = NULL, multiple = TRUE, width = "100%")
    ),
    shiny::conditionalPanel(
      "input.cols_mode == 'pattern'", ns = ns,
      shiny::textInput(ns("cols_pattern"), NULL, placeholder = "例如 ^score_", width = "60%")
    )
  )
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
    if (length(value) == 0) return("（還沒有題目欄）")
    more <- if (length(value) > 6) paste0("…（共 ", length(value), " 欄）") else ""
    return(paste0(paste(utils::head(value, 6), collapse = "、"), more, "　固定"))
  }
  if (is.null(value)) return("（不用）　固定")
  shown <- if (is.character(value)) paste0("\"", value, "\"", collapse = ", ") else
    paste(value, collapse = ", ")
  paste0(shown, "　固定")
}


# ── 執行與程式碼 ─────────────────────────────────────────────────────────────

#' 依參數執行 tidyr::pivot_longer()
#'
#' cols_pattern 有值時用欄名規則選欄。轉換前先檢查：沒有欄、新欄名和留下的欄撞名、
#' 好幾個 names_to 卻沒有 names_pattern。tidyr 的錯誤（例如數字與文字混在一起）改成中文。
#' @noRd
pivot_longer_run <- function(df, args) {
  args <- utils::modifyList(PIVOT_LONGER_DEFAULTS, args)
  cols <- pivot_longer_cols(df, args)
  problem <- pivot_longer_problem(df, cols, args)
  if (!is.null(problem)) kit_abort(problem)
  tryCatch(
    tidyr::pivot_longer(
      df, cols = dplyr::all_of(cols), names_to = args$names_to, values_to = args$values_to,
      names_prefix = args$names_prefix, names_pattern = args$names_pattern,
      values_drop_na = isTRUE(args$values_drop_na)
    ),
    error = function(e) {
      kit_abort(
        "寬轉長沒有成功：", conditionMessage(e),
        "（常見原因：選到的欄有的是數字、有的是文字。請只選同一種型別的欄。）"
      )
    }
  )
}

#' 實際要轉的欄：cols_pattern 有值時用欄名規則，否則用 cols（只留資料裡有的）
#' @noRd
pivot_longer_cols <- function(df, args) {
  if (!is.null(args$cols_pattern)) {
    names(df)[grepl(args$cols_pattern, names(df))]
  } else {
    intersect(args$cols, names(df))
  }
}

# 轉換前的檢查；沒問題時 NULL
pivot_longer_problem <- function(df, cols, args) {
  if (length(cols) == 0) return("請選要轉成長格式的欄（cols）。")
  new_cols <- c(args$names_to, args$values_to)
  if (any(!nzchar(new_cols))) return("names_to 與 values_to 不能空白。")
  clash <- intersect(new_cols, setdiff(names(df), cols))
  if (length(clash) > 0) {
    return(paste0("新的欄名 ", paste(clash, collapse = "、"), " 和留下來的欄撞名了，請換一個名字。"))
  }
  if (length(args$names_to) > 1 && is.null(args$names_pattern)) {
    return("names_to 有好幾個名稱時，要用 names_pattern 說明怎麼從欄名切出各部分。")
  }
  NULL
}

#' pivot_longer() 的程式碼：只寫和 tidyr 預設不同的參數（cols 一定寫）
#'
#' @param args mod_pivot_longer_server() 的參數 list
#' @param data_name 資料物件名稱
#' @param cols_code NULL 或 cols 的寫法（例如 "any_of(items)"）；NULL 時依 args 產生
#' @return 字串（多行）
#' @noRd
pivot_longer_code <- function(args, data_name, cols_code = NULL) {
  args <- utils::modifyList(PIVOT_LONGER_DEFAULTS, args)
  d <- PIVOT_LONGER_DEFAULTS
  cols_code <- cols_code %||% if (!is.null(args$cols_pattern)) {
    paste0("matches(", r_str(args$cols_pattern), ")")
  } else {
    r_cols_vector(args$cols)
  }
  str_arg <- function(name) {
    v <- args[[name]]
    if (is.null(v) || identical(v, d[[name]])) return(NULL)
    paste0(name, " = ", r_str(v))
  }
  parts <- c(
    paste0("cols = ", cols_code),
    str_arg("names_to"), str_arg("values_to"),
    str_arg("names_prefix"), str_arg("names_pattern"),
    if (isTRUE(args$values_drop_na)) "values_drop_na = TRUE"
  )
  code_lines(
    paste0(data_name, " |>"),
    "  pivot_longer(",
    paste0("    ", parts, c(rep(",", length(parts) - 1), "")),
    "  )"
  )
}

#' 欄名向量的寫法：c(a, b, c)，太長時換行（每行不超過 100 字）
#' @param quote TRUE 時寫成文字 c("a", "b")
#' @noRd
r_cols_vector <- function(cols, indent = "      ", quote = FALSE) {
  if (length(cols) == 0) return("c()")
  items <- if (quote) paste0('"', cols, '"') else r_col(cols)
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
