# 0.4.2 前用 htmltools 標籤組的「3. 題目設定」表：只給測試比對用，
# 確認一次組好的 HTML（kitsdata_settings_table()）和舊的標記相同。

kitsdata_settings_table_old <- function(id, settings, selected = character(0),
                                        shown = settings$column) {
  if (nrow(settings) == 0) {
    return(shiny::tags$small(class = "kit-hint", "還沒有題目欄：請先在 ① 上傳資料，並完成「2.」。"))
  }
  num <- function(x) format(x, trim = TRUE, drop0trailing = TRUE, scientific = FALSE)
  alerts <- kits_settings_alerts(settings)
  rows <- lapply(seq_len(nrow(settings)), function(i) {
    col <- settings$column[i]
    raw <- settings$raw[[i]]
    score <- settings$score[[i]]
    inc <- settings$include[i]
    many <- length(raw) > KITS_MAP_SHOW_MAX
    map_tbl <- if (length(raw) == 0) {
      shiny::tags$span(class = "text-muted", "（沒有作答）")
    } else {
      shiny::tags$table(
        class = "kit-map",
        shiny::tags$tr(shiny::tags$th("原分數"), lapply(raw, function(r) shiny::tags$td(num(r)))),
        shiny::tags$tr(
          shiny::tags$th("計分"),
          lapply(seq_along(raw), function(k) {
            sc <- score[k]
            if (is.na(sc)) return(shiny::tags$td(class = "changed text-danger", "？"))
            shiny::tags$td(class = if (sc != raw[k]) "changed", num(sc))
          })
        )
      )
    }
    # 原分數很多種（例如體重這種連續的欄）：只寫幾種、範圍，對照表收起來
    raw_text <- if (many) {
      paste0(length(raw), " 種（", num(min(raw)), "–", num(max(raw)), "）")
    } else {
      paste(num(raw), collapse = " ")
    }
    if (many) {
      map_tbl <- shiny::tags$details(shiny::tags$summary("展開計分對照"), map_tbl)
    }
    after <- sort(unique(score[!is.na(score)]))
    after_text <- if (length(after) > KITS_MAP_SHOW_MAX) {
      paste0(length(after), " 種（", num(min(after)), "–", num(max(after)), "）")
    } else {
      paste(num(after), collapse = " ")
    }
    shiny::tags$tr(
      class = if (!inc) "excluded",
      style = if (!col %in% shown) "display: none;",
      shiny::tags$td(shiny::tags$input(
        type = "checkbox", name = id, value = col, checked = if (col %in% selected) "checked"
      )),
      shiny::tags$td(if (inc) "納入" else "排除"),
      shiny::tags$td(shiny::tags$code(col)),
      shiny::tags$td(
        if (is.na(settings$domain[i])) shiny::tags$span(class = "text-danger", "（沒有）") else
          settings$domain[i],
        if (!is.na(settings$source[i])) {
          shiny::tags$div(class = "text-muted small", settings$source[i])
        }
      ),
      shiny::tags$td(raw_text),
      shiny::tags$td(map_tbl),
      shiny::tags$td(after_text),
      shiny::tags$td(class = "small text-danger", alerts[i])
    )
  })
  shiny::tags$div(
    id = id, class = "form-group shiny-input-checkboxgroup shiny-input-container",
    style = "width: 100%;",
    shiny::tags$div(
      class = "shiny-options-group kit-settings-scroll",
      shiny::tags$table(
        class = "table table-sm align-middle kit-settings",
        shiny::tags$thead(shiny::tags$tr(
          shiny::tags$th("選取"), shiny::tags$th("納入"), shiny::tags$th("欄位（item_id）"),
          shiny::tags$th("領域"), shiny::tags$th("原分數"),
          shiny::tags$th("計分對照（原分數 → 計分）"), shiny::tags$th("計分後"),
          shiny::tags$th("提醒")
        )),
        shiny::tags$tbody(rows)
      )
    )
  )
}
