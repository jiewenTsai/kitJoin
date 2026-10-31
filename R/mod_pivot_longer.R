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

# tidyr::pivot_longer() 的預設值（cols 沒有預設，一定要給）
# cols_exclude = TRUE 時 cols 是「不轉的欄」，其餘全部轉
PIVOT_LONGER_DEFAULTS <- list(
  cols = character(0),
  cols_exclude = FALSE,
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

# 主畫面上的控制項（只寫中文）；其他的在「進階設定」裡，標題後面附參數名稱
PIVOT_LONGER_MAIN <- c("cols", "names_to", "values_to")

#' 控制項的標題：主畫面只有中文；進階設定裡是「中文（參數名稱）」
#' @noRd
pivot_label <- function(arg, labels = PIVOT_LONGER_LABELS) {
  if (arg %in% PIVOT_LONGER_MAIN) return(labels[[arg]])
  shiny::tagList(labels[[arg]], shiny::tags$code(paste0("（", arg, "）")))
}

#' 進階設定最上面一行：主畫面的控制項對應 tidyr::pivot_longer() 的哪個參數
#' @noRd
pivot_longer_arg_map <- function(labels = PIVOT_LONGER_LABELS) {
  shiny::tags$p(
    class = "kit-hint",
    "對照 R 的 ", shiny::tags$code("tidyr::pivot_longer()"), "：",
    lapply(seq_along(PIVOT_LONGER_MAIN), function(i) {
      a <- PIVOT_LONGER_MAIN[i]
      shiny::tagList(
        "「", labels[[a]], "」是 ", shiny::tags$code(a),
        if (i < length(PIVOT_LONGER_MAIN)) "；" else "。"
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
        if (locked("cols")) {
          pivot_locked_ui(ns, "cols")
        } else {
          pivot_cols_ui(ns, shiny::isolate(all_cols()), shiny::isolate(input$cols))
        },
        shiny::tags$div(class = "kit-pivot-row", text("names_to"), text("values_to")),
        kit_advanced(
          title = "進階設定：欄名的開頭、規則、空白的值、R 的參數名稱",
          open = any(c("names_prefix", "names_pattern", "values_drop_na") %in% names(fixed)),
          pivot_longer_arg_map(),
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

    shiny::reactive({
      typed <- function(arg) {
        v <- input[[arg]]
        if (is.null(v) || !nzchar(trimws(v))) NULL else trimws(v)
      }
      args <- PIVOT_LONGER_DEFAULTS
      if (!locked("cols")) {
        # 依資料的欄位順序（和 starts_with()、!c() 選出來的順序一樣）
        args$cols <- pivot_order_cols(input$cols %||% character(0), all_cols())
        args$cols_exclude <- identical(input$cols_mode, "exclude")
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

# 選欄框的標題（跟著模式換）
PIVOT_COLS_PICK <- c(
  include = "要轉的欄（點選，可多選）",
  exclude = "不轉的欄（通常是 id、性別這類每人一個值的欄；其餘全部轉）"
)

# cols：全部用點選。挑要轉的欄，或挑不轉的欄（其餘全部轉）
pivot_cols_ui <- function(ns, choices = NULL, selected = NULL) {
  shiny::tagList(
    shiny::radioButtons(
      ns("cols_mode"), pivot_label("cols"),
      choices = c("挑要轉的欄" = "include", "挑不轉的欄（其餘全部轉）" = "exclude"),
      inline = TRUE
    ),
    shiny::selectizeInput(
      ns("cols"), PIVOT_COLS_PICK[["include"]], choices = choices, selected = selected,
      multiple = TRUE,
      width = "100%",
      options = list(placeholder = "點這裡選欄", plugins = list("remove_button"))
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
    label <- paste0(groups$prefix[i], " 開頭（", kit_num(groups$n[i]), " 欄）")
    btn(paste0("g", i), label)
  })
  shiny::tags$div(
    class = "d-flex flex-wrap align-items-center gap-1 mb-2",
    shiny::tags$span(class = "small text-muted me-1", "快速選："),
    btn("all", "全選"), btn("none", "清除"),
    if (length(group_btns) > 0) {
      shiny::tags$span(class = "small text-muted ms-2 me-1", "加入一組：")
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
  if (length(all) == 0) return("還沒有資料。")
  conv <- if (exclude) setdiff(all, picked) else intersect(all, picked)
  keep <- setdiff(all, conv)
  if (length(conv) == 0) {
    return(if (exclude) "每一欄都挑成不轉了，沒有欄可以轉。" else "還沒選要轉的欄。")
  }
  few <- function(x, n) {
    paste0("（", paste(utils::head(x, n), collapse = "、"), if (length(x) > n) "…", "）")
  }
  paste0(
    "會轉 ", kit_num(length(conv)), " 欄", few(conv, 4), "，",
    "保留 ", kit_num(length(keep)), " 欄不動", if (length(keep) > 0) few(keep, 6), "。"
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
    if (length(value) == 0) return("（還沒有題目欄）")
    more <- if (length(value) > 6) paste0("…（共 ", kit_num(length(value)), " 欄）") else ""
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
        names_prefix = args$names_prefix, names_pattern = args$names_pattern,
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
        "（常見原因：選到的欄有的是數字、有的是文字。請只選同一種型別的欄。）"
      }
      kit_abort("寬轉長沒有成功：", msg, hint)
    }
  )
  if (length(caught) > 0) attr(out, "kit_warnings") <- unique(caught)
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
      paste0(paste(utils::head(txt, 3), collapse = "、"), if (length(txt) > 3) "…")
    }
    examples <- paste0(utils::head(first, 2), "（", vapply(utils::head(sets, 2), show, ""), "）")
    out$warning <- paste0(
      "要轉的 ", length(cols), " 欄有 ", length(sets) + (!all(has_v)), " 種不同的數值標籤（例如 ",
      paste(examples, collapse = "；"), if (!all(has_v)) "；有的欄沒有數值標籤", "）。",
      "放進同一欄會把第一欄的標籤套到每一題，所以值欄不放數值標籤（值不變）。",
      "需要標籤的話，請每次只轉標籤相同的欄。"
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

# 轉換前的檢查；沒問題時 NULL
pivot_longer_problem <- function(df, cols, args) {
  if (length(cols) == 0) return("請選要轉成長格式的欄。")
  new_cols <- c(args$names_to, args$values_to)
  if (any(!nzchar(new_cols))) {
    return(paste0(
      "「", PIVOT_LONGER_LABELS[["names_to"]], "」與「", PIVOT_LONGER_LABELS[["values_to"]],
      "」不能空白。"
    ))
  }
  clash <- intersect(new_cols, setdiff(names(df), cols))
  if (length(clash) > 0) {
    return(paste0("新的欄名 ", paste(clash, collapse = "、"), " 和留下來的欄撞名了，請換一個名字。"))
  }
  if (length(args$names_to) > 1 && is.null(args$names_pattern)) {
    return(paste0(
      "「", PIVOT_LONGER_LABELS[["names_to"]], "」填了好幾個名稱時，要在進階設定填「",
      PIVOT_LONGER_LABELS[["names_pattern"]], "（names_pattern）」，說明怎麼從欄名切出各部分。"
    ))
  }
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
      "# 要轉的欄數值標籤不一樣：先拿掉，才不會把第一欄的標籤套到每一題（值不變）"
    } else {
      "# 要轉的欄變項標籤不一樣：先拿掉（值欄不是任何一題）"
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
