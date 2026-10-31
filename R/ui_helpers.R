# kitJoin 自己的 UI 元件（共用的介面元件在 kitDesign：kit_status()、kit_button()、kit_callout()…）

#' 到 kitsUI：依本機版、雲端版給不同的東西（一個 tag）
#'
#' - 雲端版（kit_is_cloud()）：開新分頁連到 KITSUI_URL，後面一行小字說明開的是雲端版。
#' - 本機版：不放連結（連結會把人帶到雲端版，作答檔就上傳到雲端了），
#'   改成一行說明：在 R 執行 kitsUI::run_kitsui()。
#'
#' @param label 連結文字，例如「開啟 kitsUI」「開啟 kitsUI，上傳這個檔 →」
#' @param kind "link"（文字連結）或按鈕種類（kit_btn_class()：「secondary」、「primary」）
#' @param class 連結另外加的 class（例如 "btn-sm"、"kit-next-link"）
#' @param cloud 是不是雲端版（預設 kit_is_cloud()；測試用）
#' @return 雲端版：<span class="kit-app-link"><a …>label</a> <small>…</small></span>；
#'   本機版：<span class="kit-hint kit-kitsui-local">本機版：…</span>
#' @noRd
kit_kitsui_link <- function(label = "開啟 kitsUI", kind = "link", class = NULL,
                            cloud = kit_is_cloud()) {
  if (!isTRUE(cloud)) {
    return(shiny::tags$span(
      class = "kit-hint kit-kitsui-local",
      "本機版：在 R 執行 ", shiny::tags$code("kitsUI::run_kitsui()"),
      " 開啟 kitsUI，作答檔不會離開這台電腦。"
    ))
  }
  # 連結本身是 kitDesign 的 kit_app_link()（外層 <span class="kit-app-link">）
  kit_app_link(label, KITSUI_URL, kind = kind, class = class,
               note = "（開啟網路上的雲端版 kitsUI）")
}

#' 開啟 kitsUI 的按鈕（舊名，和 kit_kitsui_link(kind = "secondary") 一樣）
#' @param kind 按鈕種類（kit_btn_class()）
#' @noRd
kit_kitsui_button <- function(label = "開啟 kitsUI", kind = "secondary") {
  kit_kitsui_link(label, kind = kind)
}

#' 單檔上傳上限（MB）
#' @noRd
kit_max_upload_mb <- function() {
  round(getOption("shiny.maxRequestSize", KIT_UPLOAD_MB * 1024^2) / 1024^2)
}

#' 串接方法的小比較：和頁首圖解同一個例子（第 1 波 1、2 號；第 2 波 1、3 號），三種方式各留下誰
#' @noRd
join_method_example <- function() {
  shiny::tags$div(
    class = "kit-dg-compare",
    kit_dg_table(
      data.frame(
        "方式" = c("向後追蹤樣本（預設）", "全勤樣本", "全樣本"),
        "留下的人" = c("1、2 號", "1 號", "1、2、3 號"),
        "空白的格子" = c("2 號的 t2", "沒有", "2 號的 t2、3 號的 t1"),
        check.names = FALSE
      ),
      title = "三種方式各留下誰（第 1 波有 1、2 號，第 2 波有 1、3 號）",
      row_header = TRUE
    )
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

#' 目前使用哪一份資料（原始 / 遺漏值處理後），以及檔案數不足的提醒
#'
#' @param n_files 已上傳檔案數（不到 2 個時回傳 NULL：由主要按鈕上面的「還差」說）
#' @param source "raw"、"na" 或 "stale"（② 的結果已清除、等待重新執行）
#' @param code_cells 原始資料中等於遺漏值代碼的格數
#' @param ns 模組的 namespace 函式（連結按鈕用）
#' @noRd
kit_source_note_ui <- function(n_files, source, code_cells, ns) {
  # 檔案不夠時不另開提示框：主要按鈕停用，上面一行「還差」（kit_run_gate()）已經說了
  if (n_files < 2) return(NULL)
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
      "目前使用：原始資料。資料中有 ", kit_num(code_cells), " 格看起來是遺漏值代碼",
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
#' 按鈕用 kitDesign::kit_download_button()：檔名在畫出按鈕時決定，
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
          class = paste("btn", kit_btn_class("secondary")),
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
      button("download_csv", "下載 CSV", "csv", write_kit_csv, kit_btn_class("secondary")),
      button("download_csvgz", "下載 CSV.GZ", "csv.gz", write_kit_csv_gz, kit_btn_class("secondary")),
      button("download_sav", "下載 SAV（SPSS）", "sav", write_sav_notify, kit_btn_class("secondary")),
      shiny::tags$small(
        class = "kit-hint",
        "CSV 的遺漏值為空白儲存格，可直接用 Excel 開啟。",
        "CSV.GZ 是壓縮過的 CSV，檔案較小，可直接上傳 kitsUI；Excel 不能直接開。",
        if (length(renamed) > 0) {
          paste0(
            "匯出 SAV 時，有 ", kit_num(length(renamed)), " 個欄名不符合 SPSS 規則，會自動改名：",
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
