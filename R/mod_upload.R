# ① 上傳檔案：加入、排序、命名波次、預覽

#' @noRd
mod_upload_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    kit_tab_guide(
      goal = "加入各波次的資料檔，替每個檔案取一個波次名稱，並確認資料讀得正確。",
      steps = kit_steps(
        "按「選擇檔案…」加入各波次的 CSV 或 SAV 檔（可一次選多個，之後也可以再加）",
        "在清單中替每個檔案填波次名稱，例如 t1、t2、t3",
        "用 ↑ ↓ 調整順序（第一個檔案是寬格式串接與流失樣本的基準）",
        "在下方預覽確認中文、數字顯示正常"
      ),
      output = "檔案只是載入，還沒有合併。之後到 ③ ④ ⑤ 分頁做串接、疊加或建立流失樣本。",
      technical = paste0(
        "CSV 會依序嘗試 UTF-8 與 BIG5（CP950）編碼；都不符合時以 Latin1 讀入並提醒可能是亂碼。",
        "欄位型別以整個檔案判斷，SAV 檔保留變項標籤與數值標籤。"
      )
    ),
    shiny::fileInput(
      ns("files"),
      "加入檔案（可一次選多個）",
      multiple = TRUE,
      accept = c(".csv", ".sav", ".zsav"),
      buttonLabel = "選擇檔案…",
      placeholder = "尚未選擇檔案",
      width = "100%"
    ),
    shiny::tags$small(
      class = "kit-hint",
      sprintf(
        "支援 CSV 與 SPSS（.sav）。單檔上限 %d MB。Excel 檔請先另存為「CSV UTF-8」。",
        kit_max_upload_mb()
      )
    ),
    shiny::uiOutput(ns("file_list")),
    shiny::uiOutput(ns("label_check")),
    shiny::uiOutput(ns("next_step")),
    shiny::uiOutput(ns("preview")),
    kit_code_panel(ns("code"))
  )
}

#' @param goto function(tab) 切換分頁
#' @return list of reactives：keys、names、labels、data、encodings
#' @noRd
mod_upload_server <- function(id, goto) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # 依使用者排序的檔案：每個元素 list(key, name, data, encoding, n_problems)
    records <- shiny::reactiveVal(list())
    # 波次名稱另外存，打字時才不會讓檔案清單重畫
    labels <- shiny::reactiveValues()
    n_added <- 0L

    keys <- shiny::reactive(purrr::map_chr(records(), "key"))
    current_labels <- shiny::reactive({
      purrr::map_chr(keys(), function(k) labels[[k]] %||% "")
    })

    next_label <- function(taken) {
      k <- length(records()) + 1L
      while (paste0("t", k) %in% taken) k <- k + 1L
      paste0("t", k)
    }

    move <- function(key, step) {
      recs <- records()
      pos <- match(key, purrr::map_chr(recs, "key"))
      to <- pos + step
      if (is.na(pos) || to < 1 || to > length(recs)) return()
      recs[c(pos, to)] <- recs[c(to, pos)]
      records(recs)
    }

    watch_row <- function(key) {
      # 一定要先 force：observer 晚一點才求值，否則同一批上傳的列
      # 都會讀到 for 迴圈最後的 key（只有最後一個檔案能改名、排序、移除）
      force(key)
      shiny::observeEvent(input[[paste0("label_", key)]], {
        value <- trimws(input[[paste0("label_", key)]])
        if (!identical(shiny::isolate(labels[[key]]), value)) {
          labels[[key]] <- value
        }
      }, ignoreInit = TRUE)
      shiny::observeEvent(input[[paste0("up_", key)]], move(key, -1L))
      shiny::observeEvent(input[[paste0("down_", key)]], move(key, 1L))
      shiny::observeEvent(input[[paste0("remove_", key)]], {
        records(purrr::discard(records(), function(r) r$key == key))
      })
    }

    # 上傳：逐檔讀取，讀不了的檔案只顯示錯誤，不影響其他檔案
    shiny::observeEvent(input$files, {
      uploads <- input$files
      recs <- records()
      for (i in seq_len(nrow(uploads))) {
        name <- uploads$name[i]
        read <- tryCatch(
          read_data_file(uploads$datapath[i], name),
          error = function(e) e
        )
        if (inherits(read, "error")) {
          msg <- conditionMessage(read)
          if (!inherits(read, "kit_user_error")) {
            msg <- paste0("無法讀取 ", name, "：", msg)
          }
          shiny::showNotification(
            msg,
            type = "error",
            duration = KIT_NOTIFY_ERROR_SECONDS
          )
          next
        }
        n_added <<- n_added + 1L
        key <- paste0("f", n_added)
        taken <- purrr::map_chr(recs, function(r) {
          shiny::isolate(labels[[r$key]]) %||% ""
        })
        labels[[key]] <- next_label(taken)
        recs[[length(recs) + 1L]] <- c(list(key = key, name = name), read)
        watch_row(key)
      }
      records(recs)
    })

    output$file_list <- shiny::renderUI({
      recs <- records()
      if (length(recs) == 0) return(NULL)
      n <- length(recs)
      rows <- purrr::imap(recs, function(r, pos) {
        btn <- function(prefix, label, show) {
          if (!show) return(NULL)
          shiny::actionButton(
            ns(paste0(prefix, r$key)), label,
            class = "btn-sm btn-outline-secondary"
          )
        }
        shiny::tags$tr(
          shiny::tags$td(pos),
          shiny::tags$td(
            shiny::tags$strong(r$name), shiny::tags$br(),
            shiny::tags$small(
              class = "text-muted",
              format_dims(nrow(r$data), ncol(r$data))
            ),
            file_read_notes(r)
          ),
          shiny::tags$td(
            shiny::textInput(
              ns(paste0("label_", r$key)), NULL,
              value = shiny::isolate(labels[[r$key]]), width = "8rem"
            )
          ),
          shiny::tags$td(
            class = "kit-row-buttons",
            btn("up_", "↑ 上移", pos > 1),
            btn("down_", "↓ 下移", pos < n)
          ),
          shiny::tags$td(
            shiny::actionButton(
              ns(paste0("remove_", r$key)), "移除",
              class = "btn-sm btn-outline-danger"
            )
          )
        )
      })
      shiny::tagList(
        shiny::h4("已加入的檔案"),
        shiny::tags$div(
          class = "kit-table-scroll",
          shiny::tags$table(
            class = "table table-sm align-middle kit-file-table",
            shiny::tags$thead(shiny::tags$tr(
              shiny::tags$th("順序"), shiny::tags$th("檔案"),
              shiny::tags$th("波次名稱"), shiny::tags$th("調整順序"), shiny::tags$th("")
            )),
            shiny::tags$tbody(rows)
          )
        ),
        shiny::tags$small(
          class = "kit-hint",
          "波次名稱：寬格式串接時加在欄名後面（score → score_t1）；",
          "長格式疊加時填進 wave 欄。每個檔案要不同。"
        )
      )
    })

    output$label_check <- shiny::renderUI({
      recs <- records()
      if (length(recs) == 0) return(NULL)
      problem <- check_wave_labels(
        current_labels(),
        purrr::map_chr(recs, "name")
      )
      if (is.null(problem)) return(NULL)
      shiny::tags$div(class = "alert alert-danger kit-label-check", problem)
    })

    output$next_step <- shiny::renderUI({
      n <- length(records())
      if (n == 0) return(NULL)
      if (n == 1) {
        return(shiny::tags$div(
          class = "kit-callout",
          "已加入 1 個檔案。串接、疊加與流失樣本都需要至少 2 個波次的檔案，請再加入一個檔案。",
          "要轉成 kits 作答檔時，1 個檔案就可以 → ",
          shiny::actionLink(ns("goto_kitsdata"), "⑥ 轉成 kits 作答檔")
        ))
      }
      link <- function(id, text) shiny::actionLink(ns(id), text)
      shiny::tags$div(
        class = "kit-callout kit-next-step",
        shiny::tags$strong("下一步："),
        shiny::tags$ul(
          if (count_code_cells(purrr::map(records(), "data")) > 0) {
            shiny::tags$li(
              "資料中有 9999 等遺漏值代碼 → 建議先到 ",
              link("goto_missing", "② 遺漏值轉 NA"), "（選用）"
            )
          },
          shiny::tags$li("要「每人一列、各波變項並排」→ 到 ", link("goto_join", "③ 寬格式串接")),
          shiny::tags$li("要「每人每波一列」→ 到 ", link("goto_stack", "④ 長格式疊加")),
          shiny::tags$li("要標記誰在後期沒有繼續參加 → 到 ", link("goto_attrition", "⑤ 流失樣本")),
          shiny::tags$li("要做 kits 或 kitsUI 的作答檔 → 到 ", link("goto_kitsdata", "⑥ 轉成 kits 作答檔"))
        )
      )
    })
    for (tab in c("missing", "join", "stack", "attrition", "kitsdata")) {
      local({
        target <- tab
        shiny::observeEvent(input[[paste0("goto_", target)]], goto(target))
      })
    }

    output$preview <- shiny::renderUI({
      recs <- records()
      if (length(recs) == 0) return(NULL)
      choices <- stats::setNames(
        purrr::map_chr(recs, "key"),
        purrr::map_chr(recs, "name")
      )
      selected <- shiny::isolate(input$preview_key)
      if (is.null(selected) || !selected %in% choices) selected <- choices[[1]]
      shiny::tagList(
        shiny::hr(),
        shiny::h4("預覽"),
        shiny::selectInput(
          ns("preview_key"),
          "要預覽的檔案",
          choices = choices,
          selected = selected
        ),
        shiny::textOutput(ns("preview_meta")),
        shiny::tags$div(
          class = "kit-table-scroll",
          shiny::tableOutput(ns("preview_table"))
        )
      )
    })

    preview_record <- shiny::reactive({
      recs <- records()
      shiny::req(length(recs) > 0, input$preview_key)
      rec <- purrr::detect(recs, function(r) r$key == input$preview_key)
      shiny::req(rec)
      rec
    })
    output$preview_meta <- shiny::renderText({
      df <- preview_record()$data
      paste0(
        format_dims(nrow(df), ncol(df), KIT_PREVIEW_COLS),
        "；以下為前 ", KIT_PREVIEW_ROWS, " 列。"
      )
    })
    output$preview_table <- shiny::renderTable(
      kit_preview_table(preview_record()$data),
      striped = TRUE, spacing = "s"
    )

    files <- list(
      keys = keys,
      names = shiny::reactive(purrr::map_chr(records(), "name")),
      labels = current_labels,
      data = shiny::reactive(purrr::map(records(), "data")),
      encodings = shiny::reactive(purrr::map_chr(records(), "encoding"))
    )

    output$code <- shiny::renderUI(
      kit_code_blocks(code_read(code_files_from(files)))
    )

    files
  })
}

#' 檔案清單中的讀檔提醒：編碼、格式異常的列
#' @noRd
file_read_notes <- function(record) {
  enc <- record$encoding
  shiny::tagList(
    if (!is.na(enc) && enc == "Latin1") {
      shiny::tags$div(
        class = "text-danger small",
        "⚠ 中文可能是亂碼：請用 Excel 另存為「CSV UTF-8」後重新上傳。"
      )
    } else if (!is.na(enc)) {
      shiny::tags$small(class = "text-muted", paste0("　編碼：", enc))
    },
    if (record$n_problems > 0) {
      shiny::tags$div(
        class = "text-danger small",
        paste0("⚠ 有 ", record$n_problems, " 處資料格式異常（例如欄位數不對），請用 Excel 檢查原檔。")
      )
    }
  )
}
