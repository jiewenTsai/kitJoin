# 各分頁共用的 UI 元件與狀態訊息

#' 分頁說明框（預設收起：只看到「這一頁做什麼」一句；按「看步驟」才展開步驟、輸出、技術說明）
#'
#' @param goal 本分頁目標
#' @param steps 操作步驟（字串或 tags$ol(...)）
#' @param output 輸出形式
#' @param technical 技術說明（給想了解原理的人，預設收起；可 NULL）
#' @param note 附注（可 NULL）
#' @noRd
kit_tab_guide <- function(goal, steps, output, technical = NULL, note = NULL) {
  shiny::tags$details(
    class = "kit-guide kit-guide-page",
    shiny::tags$summary(
      shiny::tags$strong("這一頁做什麼："), goal,
      shiny::tags$span(class = "kit-guide-more")
    ),
    shiny::tags$div(
      class = "kit-guide-body",
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
  )
}

#' 開啟 kitsUI 雲端版的按鈕（新分頁）
#' @param kind 按鈕種類（KIT_BTN）：平常是次要按鈕；⑥ 最下面的「下一步」是主要按鈕
#' @noRd
kit_kitsui_button <- function(label = "開啟 kitsUI", kind = "secondary") {
  shiny::tags$a(
    href = KITSUI_URL,
    target = "_blank",
    rel = "noopener noreferrer",
    class = paste("btn kit-ext-btn", KIT_BTN[[kind]]),
    shiny::icon("arrow-up-right-from-square"),
    label
  )
}


# ── 頁面骨架的元件（KIT 設計語言 3、4.1；和 kitsUI 的 ui_* 同樣的標記與 class）──────
#
# 每一頁由上到下：這一頁做什麼 → 設定（進階設定收起）→ 設定摘要 → 一個主要按鈕 →
# 狀態框 → 重點（一張表或摘要）→ 一排收起來的細節 → 下一步。

# 按鈕的寫法（KIT 設計語言 3.4）：主要按鈕一頁一個（加上最下面的「下一步」）；
# 次要按鈕（下載、全選、上移…）白底、小一號；危險按鈕（移除、排除、仍要執行）鮭紅底
KIT_BTN <- c(
  primary = "btn-primary",
  secondary = "btn-sm btn-outline-secondary",
  danger = "btn-sm btn-danger"
)

#' 一個按鈕（shiny::actionButton()），class 照 KIT_BTN
#' @param kind "primary"、"secondary" 或 "danger"
#' @noRd
kit_button <- function(id, label, kind = "secondary", ..., class = NULL) {
  shiny::actionButton(id, label, class = paste(c(KIT_BTN[[kind]], class), collapse = " "), ...)
}

#' 提示框（KIT 設計語言 3.7）：天藍的資訊；warn = TRUE 時是黃色的注意
#' @noRd
kit_callout <- function(..., warn = FALSE, class = NULL) {
  shiny::tags$div(
    class = paste(c("kit-callout", if (warn) "kit-callout-warn", class), collapse = " "),
    ...
  )
}

#' 進階設定：預設收起（文字欄比對、ID 型別轉換、反向題規則、分隔符號…）
#' @param title 收合標題
#' @noRd
kit_advanced <- function(..., title = "進階設定", open = FALSE, id = NULL) {
  shiny::tags$details(
    id = id, class = "kit-advanced", open = if (open) NA,
    shiny::tags$summary(title),
    shiny::tags$div(class = "kit-advanced-body", ...)
  )
}

#' 一排細節：結果下面一排收起來的區塊（詳細核對、預覽、用 R 怎麼做…）
#' @param ... kit_detail()
#' @noRd
kit_details_row <- function(...) {
  shiny::tags$div(class = "kit-details", ...)
}

#' 一個細節：預設收起，標題一行小字
#'
#' 收起來的 output 要打開才算出來：kit_app_js() 在打開時通知 Shiny 重新檢查。
#' @noRd
kit_detail <- function(title, ..., open = FALSE) {
  shiny::tags$details(
    class = "kit-detail", open = if (open) NA,
    shiny::tags$summary(title),
    shiny::tags$div(class = "kit-detail-body", ...)
  )
}

#' 設定摘要：主要按鈕上面一行小字，例如「本次：5 個檔案 · ID：各檔各自 · 全勤樣本」
#' @param parts 各段文字（空字串與 NULL 略過），用「 · 」接起來
#' @noRd
kit_settings_line <- function(parts) {
  parts <- unlist(parts)
  parts <- parts[!is.na(parts) & nzchar(parts)]
  if (length(parts) == 0) return(NULL)
  shiny::tags$p(class = "kit-settings-line", paste0("本次：", paste(parts, collapse = " · ")))
}

#' 下一步：頁面最下面，一句小字與一個切換分頁的按鈕（和主要按鈕同一種樣式，靠右）
#' @param label 「下一步：橫向資料串接」；後面自動加「 →」
#' @param button 不用 actionButton 時直接給按鈕（例如開啟 kitsUI 的連結）；
#'   button 與 button_id 都是 NULL 時只有那句小字
#' @noRd
kit_next_step <- function(text = NULL, button_id = NULL, label = NULL, button = NULL) {
  if (is.null(button) && !is.null(button_id)) {
    button <- kit_button(button_id, paste0(label, " →"), "primary")
  }
  shiny::tags$div(
    class = "kit-next",
    if (!is.null(text)) shiny::tags$div(class = "kit-hint", text),
    button
  )
}

#' 長表的說明：「共 3,418 列 × 25 欄，以下前 10 列、前 20 欄。」
#' @param shown_rows,shown_cols 畫面上顯示幾列、幾欄（NULL：不提欄）
#' @noRd
kit_rows_note <- function(nrow, ncol = NULL, shown_rows, shown_cols = NULL) {
  total <- if (is.null(ncol)) paste0(kit_num(nrow), " 列") else format_dims(nrow, ncol)
  cut_rows <- nrow > shown_rows
  cut_cols <- !is.null(ncol) && !is.null(shown_cols) && ncol > shown_cols
  if (!cut_rows && !cut_cols) return(paste0("共 ", total, "，以下是全部。"))
  rows <- if (cut_rows) paste0("前 ", kit_num(shown_rows), " 列") else "全部的列"
  cols <- if (cut_cols) paste0("、前 ", kit_num(shown_cols), " 欄")
  paste0("共 ", total, "，以下", rows, cols, "。")
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

#' 整數加千分位：3418 → "3,418"（KIT 設計語言 5：四位數以上加千分位）
#' @noRd
kit_num <- function(x) {
  formatC(as.numeric(x), format = "d", big.mark = ",")
}

#' 維度描述：「5 列 × 7 欄」；欄位多於預覽欄數時補一句只預覽前幾欄
#' @noRd
format_dims <- function(nrow, ncol, preview_cols = NULL) {
  base <- paste0(kit_num(nrow), " 列 × ", kit_num(ncol), " 欄")
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
#
# KIT 設計語言 3.6：按了主要按鈕以後第一眼看的框，只有四種標籤。
# 第一行是標籤＋一句結論（帶數字與單位）；接著最多 3 點要注意的事；
# 停下來時寫怎麼辦，原始的錯誤訊息收在「原始訊息」裡。

# 狀態 → 框的顏色與標籤（idle 不顯示）
KIT_STATUS_LOOK <- list(
  success = c(class = "alert-success", label = "【完成】"),
  warning = c(class = "alert-warning", label = "【完成，但請注意】"),
  error = c(class = "alert-danger", label = "【停下來】"),
  stale = c(class = "alert-warning", label = "【要重新執行】")
)

# 狀態框裡直接列出幾點；更多的收在「還有 N 點」裡
KIT_STATUS_MAX_POINTS <- 3L

#' 狀態物件
#'
#' @param state "idle"、"stale"、"success"、"warning" 或 "error"
#' @param message 第一行的結論（一句話）
#' @param warnings 需要注意的事（條列，最多直接顯示 3 點）
#' @param notes 補充說明（灰色小字）
#' @param details 原始訊息（收起）
#' @param action 怎麼辦的按鈕或連結（tag；可 NULL）
#' @noRd
kit_status <- function(state = "idle", message = "", warnings = character(0),
                       notes = character(0), details = NULL, action = NULL) {
  list(
    state = state,
    message = message,
    warnings = warnings,
    notes = notes,
    details = details,
    action = action
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

#' 執行失敗的狀態（停下來）：白話錯誤直接顯示；未預期的錯誤寫怎麼辦，原始訊息收起來
#' @param action 動作名稱，例如「串接」
#' @param todo 怎麼辦的按鈕或連結（tag；可 NULL）
#' @noRd
kit_error_status <- function(e, action, todo = NULL) {
  if (inherits(e, "kit_user_error")) {
    return(kit_status("error", conditionMessage(e), action = todo))
  }
  kit_status(
    "error",
    paste0(action, "時發生沒有預期到的問題，沒有做完。"),
    warnings = paste0(
      "怎麼辦：確認檔案內容與設定後再試一次；一直發生的話，",
      "請把下面的「原始訊息」回報給開發者。"
    ),
    details = conditionMessage(e),
    action = todo
  )
}

#' 檔案或設定變更後，舊結果失效
#' @noRd
kit_stale_status <- function(run_label) {
  kit_status(
    "stale",
    paste0("檔案或設定改過了，舊的結果已清除：請再按一次「", run_label, "」。")
  )
}

#' 狀態框 UI
#' @noRd
make_status_ui <- function(st) {
  if (is.null(st) || identical(st$state, "idle")) return(NULL)
  look <- KIT_STATUS_LOOK[[st$state]]
  points <- st$warnings
  shown <- utils::head(points, KIT_STATUS_MAX_POINTS)
  more <- points[-seq_along(shown)]
  shiny::tags$div(
    class = paste("alert", look[["class"]], "kit-status"),
    role = "alert",
    shiny::tags$div(
      class = "kit-status-line",
      shiny::tags$strong(look[["label"]]), " ", st$message
    ),
    if (length(shown) > 0) shiny::tags$ul(lapply(shown, shiny::tags$li)),
    if (length(more) > 0) {
      shiny::tags$details(
        class = "kit-status-more",
        shiny::tags$summary(paste0("還有 ", length(more), " 點")),
        shiny::tags$ul(lapply(more, shiny::tags$li))
      )
    },
    if (!is.null(st$action)) shiny::tags$div(class = "kit-status-action", st$action),
    if (length(st$notes) > 0) {
      shiny::tags$div(
        class = "kit-status-notes",
        lapply(st$notes, shiny::tags$div)
      )
    },
    if (!is.null(st$details)) {
      shiny::tags$details(
        class = "kit-status-raw",
        shiny::tags$summary("原始訊息"),
        shiny::tags$code(st$details)
      )
    }
  )
}

#' 停下來時「怎麼辦」的連結：檔案不夠時到 ① 加入檔案（夠了就是 NULL）
#'
#' id 是 goto_upload_status（頁面上方的提示框已經用了 goto_upload，id 不能重複）。
#' @param need 這一頁至少要幾個檔案
#' @noRd
kit_upload_todo <- function(ns, n_files, need = 1L) {
  if (n_files >= need) return(NULL)
  shiny::actionLink(ns("goto_upload_status"), "到「檔案上傳」加入檔案 →")
}

#' 顯示通知：停下來用紅色、其他用一般樣式
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
#' @param source "raw"、"na" 或 "stale"（② 的結果已清除、等待重新執行）
#' @param code_cells 原始資料中等於遺漏值代碼的格數
#' @param ns 模組的 namespace 函式（連結按鈕用）
#' @noRd
kit_source_note_ui <- function(n_files, source, code_cells, ns) {
  if (n_files < 2) {
    return(kit_callout(
      warn = TRUE,
      "這個功能需要至少 2 個波次的檔案，目前只有 ", n_files, " 個。",
      shiny::actionLink(ns("goto_upload"), "到「檔案上傳」加入檔案")
    ))
  }
  if (identical(source, "na")) {
    return(shiny::tags$p(
      class = "kit-source-note",
      "目前使用：「遺漏值處理」處理後的資料。"
    ))
  }
  if (identical(source, "stale")) {
    return(kit_callout(
      warn = TRUE,
      "目前使用：原始資料。「遺漏值處理」的代碼、設定或檔案改過了，舊的轉換結果已清除，",
      shiny::actionLink(ns("goto_missing"), "請回「遺漏值處理」重新按「執行轉換」"),
      "；在那之前，這裡用的是還沒轉換的原始資料。"
    ))
  }
  if (code_cells > 0) {
    return(kit_callout(
      warn = TRUE,
      "目前使用：原始資料。資料中有 ", code_cells, " 格看起來是遺漏值代碼",
      "（7777、9999 等，或 SPSS 數值標籤寫著「遺漏值」「跳答」的值），",
      "若它們代表遺漏，請先",
      shiny::actionLink(ns("goto_missing"), "到「遺漏值處理」處理"),
      "，否則會被當成一般數值。"
    ))
  }
  shiny::tags$p(class = "kit-source-note", "目前使用：原始資料。")
}


# ── 下載 ─────────────────────────────────────────────────────────────────────

#' 下載區（CSV + CSV.GZ + SAV）
#'
#' 沒有結果時顯示停用的按鈕與「先執行」提示；有結果時顯示下載按鈕、
#' 檔名與 SAV 自動改名提醒。
#'
#' 按鈕用 kit_download_button()（R/download.R）：檔名在畫出按鈕時決定，
#' 檔案內容就是畫面上這份結果（結果更新時按鈕會重畫、網址跟著換）。
#'
#' @param session,output 模組的 session 與 output
#' @param data_fn 回傳要匯出的資料框；沒有結果時回傳 NULL
#' @param filename_fn function(ext) 回傳下載檔名
#' @param run_label 產生結果的按鈕名稱（給提示用）
#' @noRd
kit_download_server <- function(session, output, data_fn, filename_fn, run_label) {
  output$downloads <- shiny::renderUI({
    df <- data_fn()
    if (is.null(df)) {
      disabled <- function(label) {
        shiny::tags$button(
          type = "button",
          class = paste("btn", KIT_BTN[["secondary"]]),
          disabled = NA,
          shiny::icon("download"),
          label
        )
      }
      return(shiny::tags$div(
        class = "kit-downloads",
        disabled("下載 CSV"), disabled("下載 CSV.GZ"), disabled("下載 SAV"),
        shiny::tags$small(
          class = "kit-hint",
          paste0("先按上方「", run_label, "」，才能下載結果。")
        )
      ))
    }
    renamed <- sav_renamed_cols(names(df))
    button <- function(id, label, ext, writer, class) {
      kit_download_button(
        session, id, label, filename_fn(ext), function(file) writer(df, file), class
      )
    }
    # SAV 寫完讀不回來而修改了數值標籤時，下載當下跳出通知
    write_sav_notify <- function(df, file) {
      written <- write_kit_sav(df, file)
      msg <- sav_fix_message(written)
      if (!is.null(msg)) {
        try(shiny::showNotification(
          msg, type = "warning", duration = KIT_NOTIFY_ERROR_SECONDS, session = session
        ), silent = TRUE)
      }
      invisible(written)
    }
    shiny::tags$div(
      class = "kit-downloads",
      button("download_csv", "下載 CSV", "csv", write_kit_csv, KIT_BTN[["secondary"]]),
      button("download_csvgz", "下載 CSV.GZ", "csv.gz", write_kit_csv_gz, KIT_BTN[["secondary"]]),
      button("download_sav", "下載 SAV（SPSS）", "sav", write_sav_notify, KIT_BTN[["secondary"]]),
      shiny::tags$small(
        class = "kit-hint",
        "CSV 的遺漏值為空白儲存格，可直接用 Excel 開啟。",
        "CSV.GZ 是壓縮過的 CSV，檔案較小，可直接上傳 kitsUI；Excel 不能直接開。",
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

#' 小表格（直接放在 renderUI() 裡，不用另開 output）
#' @noRd
kit_static_table <- function(df) {
  cells <- df_for_display(df)
  shiny::tags$table(
    class = "table table-sm table-striped",
    shiny::tags$thead(shiny::tags$tr(lapply(names(cells), shiny::tags$th))),
    shiny::tags$tbody(lapply(seq_len(nrow(cells)), function(i) {
      shiny::tags$tr(lapply(cells[i, ], function(v) shiny::tags$td(v %||% "")))
    }))
  )
}

#' 預覽表（共用格式）
#' @noRd
kit_preview_table <- function(df, priority_cols = NULL, n = KIT_PREVIEW_ROWS) {
  df_for_display(preview_df_cols(df, priority_cols = priority_cols), n)
}


# ── 教學：用 R 怎麼做？ ──────────────────────────────────────────────────────

#' 「用 R 怎麼做」：一行小字的收合標題（和 kitsUI 的 ui_detail() 同樣的標記），預設收起
#'
#' 放在結果下面那一排細節的最後；打開後只有程式碼本身有框。
#' @param output_id 由 server 以 `kit_code_blocks()` 填入內容的 uiOutput id
#' @noRd
kit_code_panel <- function(output_id) {
  kit_detail("用 R 怎麼做", shiny::uiOutput(output_id))
}

#' 教學區塊內容：說明 + tidyverse 版 + base R 版（附複製按鈕）
#'
#' @param snips list(tidy = 字串, base = 字串)；base 為 NULL 時只顯示 tidyverse 版
#' @noRd
kit_code_blocks <- function(snips) {
  block <- function(label, code) {
    shiny::tagList(
      shiny::tags$div(
        class = "kit-code-head",
        shiny::tags$strong(label),
        shiny::tags$button(
          type = "button",
          class = paste("btn", KIT_BTN[["secondary"]]),
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
      class = "kit-hint",
      "以下程式碼依你目前在這一頁的設定產生，是簡化的示範；",
      "app 實際處理另外還有編碼修正、型別檢查與核對等步驟。"
    ),
    block("tidyverse 版", snips$tidy),
    if (!is.null(snips$base)) block("base R 版", snips$base)
  )
}


# ── 瀏覽器端的小程式 ─────────────────────────────────────────────────────────

#' app 的 JavaScript（放在頁面最後，shiny.js 已經載入）
#'
#' 1. 收合區塊（<details>）打開時，通知 Shiny 重新檢查裡面的 output 看不看得到；
#'    不然收起來時暫停的 output 打開後還是空的。
#' 2. 分頁列（kit_navgroups()）：按下去時切換藏起來的 tabsetPanel；分頁換了以後
#'    （包括 updateTabsetPanel()）把目前的分頁標成 active。
#' 3. kit-tab-states：依伺服器算好的狀態，替分頁列的連結加上 class 與滑過時的說明
#'    （kit-tab-done 打 ✓、kit-tab-locked 灰色、kit-tab-stale 拿掉 ✓）。
#' 4. kit-open-details：打開指定 id 的收合區塊（例如套用預設後打開進階設定）。
#' @noRd
kit_app_js <- function() {
  shiny::tags$script(shiny::HTML("
(function() {
  var $ = window.jQuery;
  document.addEventListener('toggle', function(e) {
    var d = e.target;
    if (!d || d.tagName !== 'DETAILS' || !$) return;
    $(d).trigger(d.open ? 'shown' : 'hidden');
  }, true);
  function navLinks(tabset, value) {
    var sel = '.kit-navgroups[data-tabset=\"' + tabset + '\"] a[data-kit-tab' +
      (value ? '=\"' + value + '\"' : '') + ']';
    return document.querySelectorAll(sel);
  }
  if ($) {
    $(document).on('click', '.kit-navgroups a[data-kit-tab]', function(e) {
      e.preventDefault();
      var bar = this.closest('.kit-navgroups');
      var tab = document.querySelector('#' + bar.getAttribute('data-tabset') +
        ' a[data-value=\"' + this.getAttribute('data-kit-tab') + '\"]');
      if (tab) $(tab).tab('show');
    });
    $(document).on('shown.bs.tab', 'ul.shiny-tab-input a[data-value]', function() {
      var ul = this.closest('ul'), value = this.getAttribute('data-value');
      navLinks(ul.id).forEach(function(a) {
        var on = a.getAttribute('data-kit-tab') === value;
        a.classList.toggle('active', on);
        if (on) a.setAttribute('aria-current', 'page'); else a.removeAttribute('aria-current');
      });
    });
  }
  if (!window.Shiny || !Shiny.addCustomMessageHandler) return;
  Shiny.addCustomMessageHandler('kit-tab-states', function(states) {
    states.forEach(function(s) {
      navLinks(s.tabset, s.value).forEach(function(a) {
        a.classList.remove('kit-tab-done', 'kit-tab-locked', 'kit-tab-stale');
        if (s.state !== 'ready') a.classList.add('kit-tab-' + s.state);
        if (s.tip) a.setAttribute('title', s.tip); else a.removeAttribute('title');
      });
    });
  });
  Shiny.addCustomMessageHandler('kit-open-details', function(id) {
    var d = document.getElementById(id);
    if (d) d.open = true;
  });
})();
"))
}
