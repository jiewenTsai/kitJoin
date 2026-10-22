# 各分頁共用的 UI 元件與狀態訊息

#' 分頁說明框（目標 / 步驟 / 輸出，技術說明預設收起）
#'
#' @param goal 本分頁目標
#' @param steps 操作步驟（字串或 tags$ol(...)）
#' @param output 輸出形式
#' @param technical 技術說明（給想了解原理的人，預設收起；可 NULL）
#' @param note 附注（可 NULL）
#' @noRd
kit_tab_guide <- function(goal, steps, output, technical = NULL, note = NULL) {
  shiny::tags$div(
    class = "kit-guide",
    shiny::tags$p(shiny::tags$strong("這一頁做什麼："), goal),
    shiny::tags$div(shiny::tags$strong("步驟："), steps),
    shiny::tags$p(shiny::tags$strong("會得到："), output),
    if (!is.null(note)) shiny::tags$p(class = "kit-guide-note", note),
    if (!is.null(technical)) {
      shiny::tags$details(
        shiny::tags$summary("技術說明（給想了解原理的人）"),
        shiny::tags$p(technical)
      )
    }
  )
}

#' 編號步驟清單
#' @noRd
kit_steps <- function(...) {
  shiny::tags$ol(class = "kit-steps", lapply(c(...), shiny::tags$li))
}

#' 單檔上傳上限（MB）
#' @noRd
kit_max_upload_mb <- function() {
  round(getOption("shiny.maxRequestSize", KIT_UPLOAD_MB * 1024^2) / 1024^2)
}

#' 維度描述：「5 列 × 7 欄」；欄位多於預覽欄數時補一句只預覽前幾欄
#' @noRd
format_dims <- function(nrow, ncol, preview_cols = NULL) {
  base <- sprintf("%d 列 × %d 欄", as.integer(nrow), as.integer(ncol))
  if (!is.null(preview_cols) && preview_cols < ncol) {
    paste0(base, "（以下只預覽前 ", preview_cols, " 欄）")
  } else {
    base
  }
}

#' 串接方法的小例子：第 1 波 A B C、第 2 波 B C D
#' @noRd
join_method_example <- function() {
  row <- function(method, kept) {
    shiny::tags$tr(shiny::tags$td(method), shiny::tags$td(kept))
  }
  shiny::tags$details(
    class = "kit-example",
    shiny::tags$summary("看例子：三種方法會留下誰？"),
    shiny::tags$p("假設第 1 波有 A、B、C 三人，第 2 波有 B、C、D 三人："),
    shiny::tags$table(
      class = "table table-sm kit-example-table",
      shiny::tags$thead(shiny::tags$tr(
        shiny::tags$th("方法"),
        shiny::tags$th("結果留下")
      )),
      shiny::tags$tbody(
        row("向後追蹤樣本（left join）", "A、B、C（A 的第 2 波欄位是空白；D 不在第 1 波，不納入）"),
        row("全勤樣本（inner join）", "B、C"),
        row("全樣本（full join）", "A、B、C、D（缺席波次的欄位是空白）")
      )
    )
  )
}


# ── 狀態訊息 ─────────────────────────────────────────────────────────────────

#' 狀態物件
#'
#' @param state "idle"、"stale"、"success"、"warning" 或 "error"
#' @param message 主要訊息
#' @param warnings 需要使用者注意的事（黃色條列）
#' @param notes 補充說明（灰色小字）
#' @param details 技術細節（收起）
#' @noRd
kit_status <- function(state = "idle", message = "", warnings = character(0),
                       notes = character(0), details = NULL) {
  list(
    state = state,
    message = message,
    warnings = warnings,
    notes = notes,
    details = details
  )
}

#' 執行結果的狀態：有提醒時為 warning，否則為 success
#' @noRd
kit_done_status <- function(
    message,
    warnings = character(0),
    notes = character(0)) {
  state <- if (length(warnings) > 0) "warning" else "success"
  kit_status(state, message, warnings = warnings, notes = notes)
}

#' 執行失敗的狀態：白話錯誤直接顯示；未預期的錯誤附上技術細節
#' @noRd
kit_error_status <- function(e, action) {
  if (inherits(e, "kit_user_error")) {
    return(kit_status("error", conditionMessage(e)))
  }
  kit_status(
    "error",
    paste0(
      action, "時發生未預期的錯誤。請確認檔案內容與設定後再試一次；",
      "如果一直發生，請把下方「技術細節」回報給開發者。"
    ),
    details = conditionMessage(e)
  )
}

#' 檔案或設定變更後，舊結果失效
#' @noRd
kit_stale_status <- function(run_label) {
  kit_status("stale", paste0("檔案或設定已變更，舊的結果已清除。請重新按「", run_label, "」。"))
}

#' 狀態訊息 UI
#' @noRd
make_status_ui <- function(st) {
  if (is.null(st) || st$state == "idle") return(NULL)
  look <- switch(st$state,
    stale = list("alert alert-info", "【需要重新執行】"),
    success = list("alert alert-success", "【完成】"),
    warning = list("alert alert-warning", "【完成，但請注意】"),
    error = list("alert alert-danger", "【沒有完成】")
  )
  shiny::tags$div(
    class = paste(look[[1]], "kit-status"),
    role = "alert",
    shiny::tags$strong(look[[2]]), " ", st$message,
    if (length(st$warnings) > 0) {
      shiny::tags$ul(lapply(st$warnings, shiny::tags$li))
    },
    if (length(st$notes) > 0) {
      shiny::tags$div(
        class = "kit-status-notes",
        lapply(st$notes, shiny::tags$div)
      )
    },
    if (!is.null(st$details)) {
      shiny::tags$details(
        shiny::tags$summary("技術細節"),
        shiny::tags$code(st$details)
      )
    }
  )
}

#' 顯示通知：失敗用紅色、其他用一般樣式
#' @noRd
kit_notify <- function(st) {
  if (st$state == "error") {
    shiny::showNotification(
      st$message,
      type = "error",
      duration = KIT_NOTIFY_ERROR_SECONDS
    )
  } else {
    shiny::showNotification(
      st$message,
      type = "message",
      duration = KIT_NOTIFY_SECONDS
    )
  }
}

#' 白話摘要框
#' @noRd
kit_summary_ui <- function(sentences, title = "結果摘要") {
  shiny::tags$div(
    class = "kit-summary",
    shiny::tags$strong(title),
    shiny::tags$ul(lapply(sentences, shiny::tags$li))
  )
}

#' 目前使用哪一份資料（原始 / 遺漏值處理後），以及檔案數不足的提醒
#'
#' @param n_files 已上傳檔案數
#' @param source "raw" 或 "na"
#' @param code_cells 原始資料中等於遺漏值代碼的格數
#' @param ns 模組的 namespace 函式（連結按鈕用）
#' @noRd
kit_source_note_ui <- function(n_files, source, code_cells, ns) {
  if (n_files < 2) {
    return(shiny::tags$div(
      class = "kit-callout kit-callout-warning",
      "這個功能需要至少 2 個波次的檔案，目前只有 ", n_files, " 個。",
      shiny::actionLink(ns("goto_upload"), "到「① 上傳檔案」加入檔案")
    ))
  }
  if (identical(source, "na")) {
    return(shiny::tags$p(
      class = "kit-source-note",
      "目前使用：「② 遺漏值轉 NA」處理後的資料。"
    ))
  }
  if (code_cells > 0) {
    return(shiny::tags$div(
      class = "kit-callout kit-callout-warning",
      "目前使用：原始資料。資料中有 ", code_cells, " 格是 7777、9999 等常見的遺漏值代碼，",
      "若它們代表遺漏，請先",
      shiny::actionLink(ns("goto_missing"), "到「② 遺漏值轉 NA」處理"),
      "，否則會被當成一般數值。"
    ))
  }
  shiny::tags$p(class = "kit-source-note", "目前使用：原始資料。")
}


# ── 下載 ─────────────────────────────────────────────────────────────────────

#' 下載區（CSV + SAV）
#'
#' 沒有結果時顯示停用的按鈕與「先執行」提示；有結果時顯示下載按鈕、
#' 檔名與 SAV 自動改名提醒。
#'
#' @param output,ns 模組的 output 與 namespace 函式
#' @param data_fn 回傳要匯出的資料框；沒有結果時回傳 NULL
#' @param filename_fn function(ext) 回傳下載檔名
#' @param run_label 產生結果的按鈕名稱（給提示用）
#' @noRd
kit_download_server <- function(output, ns, data_fn, filename_fn, run_label) {
  output$download_csv <- shiny::downloadHandler(
    filename = function() filename_fn("csv"),
    content = function(file) write_kit_csv(data_fn(), file)
  )
  output$download_sav <- shiny::downloadHandler(
    filename = function() filename_fn("sav"),
    content = function(file) write_kit_sav(data_fn(), file)
  )
  output$downloads <- shiny::renderUI({
    df <- data_fn()
    if (is.null(df)) {
      disabled <- function(label) {
        shiny::tags$button(
          type = "button",
          class = "btn btn-outline-secondary me-2",
          disabled = NA,
          shiny::icon("download"),
          label
        )
      }
      return(shiny::tags$div(
        class = "kit-downloads",
        disabled("下載 CSV"), disabled("下載 SAV"),
        shiny::tags$small(
          class = "kit-hint",
          paste0("先按上方「", run_label, "」，才能下載結果。")
        )
      ))
    }
    renamed <- sav_renamed_cols(names(df))
    shiny::tags$div(
      class = "kit-downloads",
      shiny::downloadButton(
        ns("download_csv"),
        "下載 CSV",
        class = "btn-primary me-2"
      ),
      shiny::downloadButton(
        ns("download_sav"),
        "下載 SAV（SPSS）",
        class = "btn-outline-primary"
      ),
      shiny::tags$small(
        class = "kit-hint",
        "CSV 的遺漏值為空白儲存格，可直接用 Excel 開啟。",
        if (length(renamed) > 0) {
          paste0(
            "匯出 SAV 時，有 ", length(renamed), " 個欄名不符合 SPSS 規則，會自動改名：",
            paste(
              utils::head(paste(names(renamed), "→", renamed), 5),
              collapse = "、"
            ),
            if (length(renamed) > 5) " …" else "", "。"
          )
        }
      )
    )
  })
}

#' 下載區的位置（搭配 kit_download_server()）
#' @noRd
kit_download_ui <- function(ns) {
  shiny::uiOutput(ns("downloads"))
}

#' 結果檔名：joined_20261022_153000.csv
#' @noRd
timestamped_name <- function(prefix) {
  function(ext) {
    paste0(prefix, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", ext)
  }
}

#' 預覽表（共用格式）
#' @noRd
kit_preview_table <- function(df, priority_cols = NULL, n = KIT_PREVIEW_ROWS) {
  df_for_display(preview_df_cols(df, priority_cols = priority_cols), n)
}


# ── 教學：用 R 怎麼做？ ──────────────────────────────────────────────────────

#' 教學區塊（預設收起）
#'
#' @param output_id 由 server 以 `kit_code_blocks()` 填入內容的 uiOutput id
#' @noRd
kit_code_panel <- function(output_id) {
  shiny::tags$div(
    class = "kit-code-panel",
    bslib::accordion(
      open = FALSE,
      bslib::accordion_panel(
        title = "用 R 怎麼做？（tidyverse 版與 base R 版）",
        value = output_id,
        icon = shiny::icon("code"),
        shiny::uiOutput(output_id)
      )
    )
  )
}

#' 教學區塊內容：說明 + tidyverse 版 + base R 版（附複製按鈕）
#'
#' @param snips list(tidy = 字串, base = 字串)
#' @noRd
kit_code_blocks <- function(snips) {
  block <- function(label, code) {
    shiny::tags$div(
      class = "kit-code-block",
      shiny::tags$div(
        class = "kit-code-head",
        shiny::tags$strong(label),
        shiny::tags$button(
          type = "button",
          class = "btn btn-sm btn-outline-secondary",
          onclick = paste0(
            "var b=this;navigator.clipboard.writeText(",
            "b.parentElement.nextElementSibling.innerText).then(function(){",
            "b.textContent='已複製';",
            "setTimeout(function(){b.textContent='複製';},1500);});"
          ),
          "複製"
        )
      ),
      # 直接組 HTML：避免 tags$pre() 排版時在程式碼前插入縮排空白
      shiny::HTML(paste0(
        "<pre><code>",
        htmltools::htmlEscape(sub("\\s+$", "", code)),
        "</code></pre>"
      ))
    )
  }
  shiny::tagList(
    shiny::tags$p(
      class = "text-muted small",
      "以下程式碼依您目前在本分頁的設定產生，為簡化示範；",
      "app 實際處理另含編碼修正、型別檢查與核對等步驟。"
    ),
    block("tidyverse 版", snips$tidy),
    block("base R 版", snips$base)
  )
}
