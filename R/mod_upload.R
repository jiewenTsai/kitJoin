# 檔案上傳：加入、排序、命名波次、預覽

#' @noRd
mod_upload_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    kit_tab_guide(
      goal = "加入各波次的資料檔，替每個檔案取一個波次名稱，並確認資料讀得正確。",
      steps = kit_steps(
        "按「選擇檔案…」加入各波次的 CSV 或 SAV 檔（可一次選多個，之後也可以再加）",
        "在清單中替每個檔案填波次名稱，例如 t1、t2、t3",
        "用 ↑ ↓ 調整順序（第一個檔案是橫向資料串接與流失樣本的基準）",
        "在下方預覽確認中文、數字顯示正常"
      ),
      output = paste0(
        "檔案只是載入，還沒有合併。1 個檔案可以用遺漏值處理、kits 作答檔、長/寬格式轉換；",
        "橫向資料串接、直向波次疊加、流失樣本需要 2 個以上。"
      ),
      technical = paste0(
        "CSV 會依序嘗試 UTF-8 與 BIG5（CP950）編碼；都不符合時以 Latin1 讀入並提醒可能是亂碼。",
        "欄位型別以整個檔案判斷，SAV 檔保留變項標籤與數值標籤。"
      )
    ),
    # 這一頁的主要動作是「選擇檔案…」：用 kit-file-primary 畫成主要按鈕
    shiny::tags$div(
      class = "kit-file-primary",
      shiny::fileInput(
        ns("files"),
        "加入檔案（可一次選多個）",
        multiple = TRUE,
        accept = c(".csv", ".sav", ".zsav"),
        buttonLabel = "選擇檔案…",
        placeholder = "尚未選擇檔案",
        width = "100%"
      )
    ),
    shiny::tags$small(
      class = "kit-hint",
      sprintf(
        "支援 CSV 與 SPSS（.sav）。單檔上限 %d MB。Excel 檔請先另存為「CSV UTF-8」。",
        kit_max_upload_mb()
      )
    ),
    kit_advanced(
      title = "進階設定：SPSS 檔的中文編碼",
      shiny::radioButtons(
        ns("sav_encoding"), NULL,
        choices = c(
          "自動判斷（建議）：依檔案標示；中文變成亂碼時改用 BIG5 重讀" = "auto",
          "只依檔案標示" = "file",
          "一律用 BIG5（CP950）：舊版 SPSS 在繁體中文 Windows 存的檔" = "CP950"
        ),
        selected = "auto", width = "100%"
      ),
      shiny::tags$small(
        class = "kit-hint",
        "只影響 SAV 檔。改了以後，請把檔案移除再重新加入才會套用。"
      )
    ),
    shiny::uiOutput(ns("status")),
    shiny::uiOutput(ns("file_list")),
    kit_details_row(
      kit_detail("預覽檔案內容（確認中文、數字顯示正常）", shiny::uiOutput(ns("preview"))),
      kit_code_panel(ns("code"))
    ),
    shiny::uiOutput(ns("next_step"))
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
    # 最近一次加入時讀不了的檔案（狀態框列出來；下次加入時清掉）
    failed <- shiny::reactiveVal(character(0))

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
      bad <- character(0)
      for (i in seq_len(nrow(uploads))) {
        name <- uploads$name[i]
        read <- tryCatch({
          out <- read_data_file(
            uploads$datapath[i], name,
            sav_encoding = input$sav_encoding %||% "auto"
          )
          check_not_empty(out$data, name)
          out
        }, error = function(e) e)
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
          bad <- c(bad, msg)
          next
        }
        # 同一個檔案加兩次通常是不小心：照樣加入，但提醒
        same <- purrr::detect(recs, function(r) {
          r$name == name || identical(r$data, read$data)
        })
        if (!is.null(same)) {
          shiny::showNotification(
            paste0(
              name, if (same$name == name) " 已經加入過了（檔名相同）" else paste0(" 的內容和已加入的 ", same$name, " 完全相同"),
              "。如果是不小心重複加入，請在清單中按「移除」。"
            ),
            type = "warning",
            duration = KIT_NOTIFY_ERROR_SECONDS
          )
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
      failed(bad)
      records(recs)
    })

    output$file_list <- shiny::renderUI({
      recs <- records()
      if (length(recs) == 0) return(NULL)
      n <- length(recs)
      rows <- purrr::imap(recs, function(r, pos) {
        btn <- function(prefix, label, show) {
          if (!show) return(NULL)
          kit_button(ns(paste0(prefix, r$key)), label)
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
            kit_button(ns(paste0("remove_", r$key)), "移除", "danger")
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
          "波次名稱：橫向資料串接時加在欄名後面（score → score_t1）；",
          "直向波次疊加時填進 wave 欄。每個檔案要不同。"
        )
      )
    })

    # 狀態框：加入了幾個檔案；讀不了的檔案、亂碼、格式異常要注意；波次名稱有問題時停下來
    status <- shiny::reactive({
      recs <- records()
      bad <- failed()
      if (length(recs) == 0) {
        if (length(bad) == 0) return(kit_status())
        return(kit_status("error", "沒有加入任何檔案：檔案讀不了。", warnings = bad))
      }
      problem <- check_wave_labels(current_labels(), purrr::map_chr(recs, "name"))
      if (!is.null(problem)) {
        return(kit_status("error", "波次名稱要修改，改好以後才能串接或疊加。", warnings = problem))
      }
      n_rows <- sum(purrr::map_int(recs, function(r) nrow(r$data)))
      warns <- c(
        bad,
        purrr::map_chr(purrr::keep(recs, function(r) identical(r$encoding, "Latin1")), function(r) {
          paste0(r$name, "：中文可能是亂碼，請用 Excel 另存為「CSV UTF-8」後重新加入。")
        }),
        purrr::map_chr(purrr::keep(recs, function(r) r$n_problems > 0), function(r) {
          paste0(r$name, "：有 ", kit_num(r$n_problems), " 處資料格式異常（例如欄位數不對）。")
        })
      )
      kit_done_status(
        paste0("已加入 ", length(recs), " 個檔案，共 ", kit_num(n_rows), " 列。"),
        warnings = warns
      )
    })
    output$status <- shiny::renderUI(make_status_ui(status()))

    # 最下面的「下一步」：資料裡有遺漏值代碼時建議先到 ②；2 個以上的檔案時到 ③；
    # 下面列出每一頁適合做什麼
    has_codes <- shiny::reactive({
      count_code_cells(
        purrr::map(records(), "data"),
        keywords = parse_na_keywords(kit_na_label_keywords_default())
      ) > 0
    })
    next_target <- shiny::reactive({
      n <- length(records())
      if (n == 0) NULL else if (has_codes()) "missing" else if (n >= 2) "join"
    })
    output$next_step <- shiny::renderUI({
      n <- length(records())
      if (n == 0) return(NULL)
      link <- function(id, text) shiny::actionLink(ns(id), text)
      uses <- if (n == 1) {
        shiny::tagList(
          shiny::tags$strong("已加入 1 個檔案。可以用："),
          shiny::tags$ul(
            shiny::tags$li("只改遺漏值代碼 → 到 ", link("goto_missing", "遺漏值處理"),
                           "，處理後直接下載"),
            shiny::tags$li("要做 kits 或 kitsUI 的作答檔 → 到 ",
                           link("goto_kitsdata", "kits 作答檔")),
            shiny::tags$li("要長轉寬或寬轉長 → 到 ", link("goto_reshape", "長/寬格式轉換"))
          ),
          "橫向資料串接、直向波次疊加與流失樣本需要至少 2 個波次的檔案。"
        )
      } else {
        shiny::tagList(
          shiny::tags$strong("接下來可以："),
          shiny::tags$ul(
            if (has_codes()) {
              shiny::tags$li(
                "資料中有遺漏值代碼（例如 9999，或數值標籤寫著「遺漏值」的值）→ 建議先到 ",
                link("goto_missing", "遺漏值處理"), "（選用）"
              )
            },
            shiny::tags$li("要「每人一列、各波變項並排」→ 到 ", link("goto_join", "橫向資料串接")),
            shiny::tags$li("要「每人每波一列」→ 到 ", link("goto_stack", "直向波次疊加")),
            shiny::tags$li("要標記誰在後期沒有繼續參加 → 到 ", link("goto_attrition", "流失樣本")),
            shiny::tags$li("要做 kits 或 kitsUI 的作答檔 → 到 ", link("goto_kitsdata", "kits 作答檔")),
            shiny::tags$li("要長轉寬或寬轉長 → 到 ", link("goto_reshape", "長/寬格式轉換"))
          )
        )
      }
      if (n == 1 && has_codes()) {
        uses <- shiny::tagList(uses, " 資料中有遺漏值代碼，建議先到「遺漏值處理」。")
      }
      target <- next_target()
      kit_next_step(
        text = uses,
        button_id = if (!is.null(target)) ns("go_next"),
        label = switch(target %||% "", missing = "下一步：遺漏值處理",
                       join = "下一步：橫向資料串接", NULL)
      )
    })
    shiny::observeEvent(input$go_next, {
      target <- next_target()
      if (!is.null(target)) goto(target)
    })
    for (tab in c("missing", "join", "stack", "attrition", "kitsdata", "reshape")) {
      local({
        target <- tab
        shiny::observeEvent(input[[paste0("goto_", target)]], goto(target))
      })
    }

    output$preview <- shiny::renderUI({
      recs <- records()
      if (length(recs) == 0) return(shiny::tags$p(class = "kit-hint", "還沒有加入檔案。"))
      choices <- stats::setNames(
        purrr::map_chr(recs, "key"),
        purrr::map_chr(recs, "name")
      )
      selected <- shiny::isolate(input$preview_key)
      if (is.null(selected) || !selected %in% choices) selected <- choices[[1]]
      shiny::tagList(
        shiny::selectInput(
          ns("preview_key"),
          "要預覽的檔案",
          choices = choices,
          selected = selected
        ),
        shiny::tags$p(class = "kit-hint", shiny::textOutput(ns("preview_meta"), inline = TRUE)),
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
      kit_rows_note(nrow(df), ncol(df), KIT_PREVIEW_ROWS, KIT_PREVIEW_COLS)
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

    # 這一頁的狀態（步驟分頁打 ✓ 用）
    files$state <- shiny::reactive(status()$state)
    files
  })
}

#' 檔案清單中的讀檔提醒：編碼、格式異常的列
#' @noRd
file_read_notes <- function(record) {
  enc <- record$encoding
  is_sav <- tolower(tools::file_ext(record$name)) %in% c("sav", "zsav")
  shiny::tagList(
    if (is_sav && identical(enc, "BIG5")) {
      shiny::tags$div(
        class = "text-muted small",
        "　編碼：BIG5。這個 SPSS 檔沒有標明正確的編碼，中文原本會變成亂碼，",
        "已改用 BIG5（CP950）讀取。請在下方預覽確認中文正常。"
      )
    } else if (!is.na(enc) && enc == "Latin1") {
      shiny::tags$div(
        class = "text-danger small",
        "⚠ 中文可能是亂碼：請用 Excel 另存為「CSV UTF-8」後重新上傳。"
      )
    } else if (!is.na(enc)) {
      shiny::tags$small(class = "text-muted", paste0("　編碼：", enc))
    },
    if (!is.null(record$delim) && record$delim != ",") {
      shiny::tags$div(
        class = "text-muted small",
        paste0(
          "　分隔符號：", if (record$delim == ";") "分號（;）" else "Tab",
          "。這個檔案的欄位不是用逗號分隔，已自動改用", if (record$delim == ";") "分號" else " Tab", "讀取。"
        )
      )
    },
    if (record$n_problems > 0) {
      shiny::tags$div(
        class = "text-danger small",
        paste0("⚠ 有 ", record$n_problems, " 處資料格式異常（例如欄位數不對），請用 Excel 檢查原檔。")
      )
    }
  )
}
