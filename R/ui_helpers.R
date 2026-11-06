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
# 原文：開啟 kitsUI
kit_kitsui_link <- function(label = "\u958b\u555f kitsUI", kind = "link", class = NULL,
                            cloud = kit_is_cloud()) {
  if (!isTRUE(cloud)) {
    return(shiny::tags$span(
      class = "kit-hint kit-kitsui-local",
      # 原文：本機版：在 R 執行 
      "\u672c\u6a5f\u7248\uff1a\u5728 R \u57f7\u884c ", shiny::tags$code("kitsUI::run_kitsui()"),
      # 原文： 開啟 kitsUI，作答檔不會離開這台電腦。
      " \u958b\u555f kitsUI\uff0c\u4f5c\u7b54\u6a94\u4e0d\u6703\u96e2\u958b\u9019\u53f0\u96fb\u8166\u3002"
    ))
  }
  # 連結本身是 kitDesign 的 kit_app_link()（外層 <span class="kit-app-link">）
  kit_app_link(label, KITSUI_URL, kind = kind, class = class,
               # 原文：（開啟網路上的雲端版 kitsUI）
               note = "\uff08\u958b\u555f\u7db2\u8def\u4e0a\u7684\u96f2\u7aef\u7248 kitsUI\uff09")
}

#' 開啟 kitsUI 的按鈕（舊名，和 kit_kitsui_link(kind = "secondary") 一樣）
#' @param kind 按鈕種類（kit_btn_class()）
#' @noRd
# 原文：開啟 kitsUI
kit_kitsui_button <- function(label = "\u958b\u555f kitsUI", kind = "secondary") {
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
        # 原文：方式 | 向後追蹤樣本（預設） | 全勤樣本 | 全樣本
        "\u65b9\u5f0f" = c("\u5411\u5f8c\u8ffd\u8e64\u6a23\u672c\uff08\u9810\u8a2d\uff09", "\u5168\u52e4\u6a23\u672c", "\u5168\u6a23\u672c"),
        # 原文：留下的人 | 1、2 號 | 1 號 | 1、2、3 號
        "\u7559\u4e0b\u7684\u4eba" = c("1\u30012 \u865f", "1 \u865f", "1\u30012\u30013 \u865f"),
        # 原文：空白的格子 | 2 號的 t2 | 沒有 | 2 號的 t2、3 號的 t1
        "\u7a7a\u767d\u7684\u683c\u5b50" = c("2 \u865f\u7684 t2", "\u6c92\u6709", "2 \u865f\u7684 t2\u30013 \u865f\u7684 t1"),
        check.names = FALSE
      ),
      # 原文：三種方式各留下誰（第 1 波有 1、2 號，第 2 波有 1、3 號）
      title = "\u4e09\u7a2e\u65b9\u5f0f\u5404\u7559\u4e0b\u8ab0\uff08\u7b2c 1 \u6ce2\u6709 1\u30012 \u865f\uff0c\u7b2c 2 \u6ce2\u6709 1\u30013 \u865f\uff09",
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
  # 原文：到「檔案上傳」加入檔案 →
  shiny::actionLink(ns("goto_upload_status"), "\u5230\u300c\u6a94\u6848\u4e0a\u50b3\u300d\u52a0\u5165\u6a94\u6848 \u2192")
}

#' 目前使用哪一份資料（原始 / 遺漏值處理後 / 再經過篩選），以及檔案數不足的提醒
#'
#' @param n_files 已上傳檔案數（不到 2 個時回傳 NULL：由主要按鈕上面的「還差」說）
#' @param source "raw"、"na" 或 "stale"（② 的結果已清除、等待重新執行）
#' @param code_cells 原始資料中等於遺漏值代碼的格數
#' @param ns 模組的 namespace 函式（連結按鈕用）
#' @param filter 「篩選與摘要」：「on」（用篩選後的資料）、「stale」（篩選結果已清除、
#'   等待重新執行）或「off」（kit_filter_state()）
#' @noRd
kit_source_note_ui <- function(n_files, source, code_cells, ns, filter = "off") {
  # 檔案不夠時不另開提示框：主要按鈕停用，上面一行「還差」（kit_run_gate()）已經說了
  if (n_files < 2) return(NULL)
  on <- identical(filter, "on")
  if (on && identical(source, "na")) {
    return(shiny::tags$p(
      class = "kit-source-note",
      # 原文：目前使用：「遺漏值處理」處理後、再經過「篩選與摘要」的資料。
      "\u76ee\u524d\u4f7f\u7528\uff1a\u300c\u907a\u6f0f\u503c\u8655\u7406\u300d\u8655\u7406\u5f8c\u3001\u518d\u7d93\u904e\u300c\u7be9\u9078\u8207\u6458\u8981\u300d\u7684\u8cc7\u6599\u3002"
    ))
  }
  if (on && identical(source, "raw") && code_cells == 0) {
    return(shiny::tags$p(
      # 原文：目前使用：原始資料，再經過「篩選與摘要」的資料。
      class = "kit-source-note", "\u76ee\u524d\u4f7f\u7528\uff1a\u539f\u59cb\u8cc7\u6599\uff0c\u518d\u7d93\u904e\u300c\u7be9\u9078\u8207\u6458\u8981\u300d\u7684\u8cc7\u6599\u3002"
    ))
  }
  extra <- if (on) {
    # 原文：這份資料也經過「篩選與摘要」篩選。
    shiny::tags$p(class = "kit-source-note", "\u9019\u4efd\u8cc7\u6599\u4e5f\u7d93\u904e\u300c\u7be9\u9078\u8207\u6458\u8981\u300d\u7be9\u9078\u3002")
  } else if (identical(filter, "stale")) {
    kit_callout(
      warn = TRUE,
      # 原文：「篩選與摘要」的設定或資料改過了，舊的篩選結果已清除，
      "\u300c\u7be9\u9078\u8207\u6458\u8981\u300d\u7684\u8a2d\u5b9a\u6216\u8cc7\u6599\u6539\u904e\u4e86\uff0c\u820a\u7684\u7be9\u9078\u7d50\u679c\u5df2\u6e05\u9664\uff0c",
      # 原文：請回「篩選與摘要」重新按「執行篩選」
      shiny::actionLink(ns("goto_filter"), "\u8acb\u56de\u300c\u7be9\u9078\u8207\u6458\u8981\u300d\u91cd\u65b0\u6309\u300c\u57f7\u884c\u7be9\u9078\u300d"),
      # 原文：；在那之前，這裡用的是沒有篩選的資料。
      "\uff1b\u5728\u90a3\u4e4b\u524d\uff0c\u9019\u88e1\u7528\u7684\u662f\u6c92\u6709\u7be9\u9078\u7684\u8cc7\u6599\u3002"
    )
  }
  note <- kit_source_note_missing(source, code_cells, ns)
  if (is.null(extra)) note else shiny::tagList(note, extra)
}

#' 目前使用哪一份資料：原始 / 遺漏值處理後（kit_source_note_ui() 用）
#' @noRd
kit_source_note_missing <- function(source, code_cells, ns) {
  if (identical(source, "na")) {
    return(shiny::tags$p(
      class = "kit-source-note",
      # 原文：目前使用：「遺漏值處理」處理後的資料。
      "\u76ee\u524d\u4f7f\u7528\uff1a\u300c\u907a\u6f0f\u503c\u8655\u7406\u300d\u8655\u7406\u5f8c\u7684\u8cc7\u6599\u3002"
    ))
  }
  if (identical(source, "stale")) {
    return(kit_callout(
      warn = TRUE,
      # 原文：目前使用：原始資料。「遺漏值處理」的代碼、設定或檔案改過了，舊的轉換結果已清除，
      "\u76ee\u524d\u4f7f\u7528\uff1a\u539f\u59cb\u8cc7\u6599\u3002\u300c\u907a\u6f0f\u503c\u8655\u7406\u300d\u7684\u4ee3\u78bc\u3001\u8a2d\u5b9a\u6216\u6a94\u6848\u6539\u904e\u4e86\uff0c\u820a\u7684\u8f49\u63db\u7d50\u679c\u5df2\u6e05\u9664\uff0c",
      # 原文：請回「遺漏值處理」重新按「執行轉換」
      shiny::actionLink(ns("goto_missing"), "\u8acb\u56de\u300c\u907a\u6f0f\u503c\u8655\u7406\u300d\u91cd\u65b0\u6309\u300c\u57f7\u884c\u8f49\u63db\u300d"),
      # 原文：；在那之前，這裡用的是還沒轉換的原始資料。
      "\uff1b\u5728\u90a3\u4e4b\u524d\uff0c\u9019\u88e1\u7528\u7684\u662f\u9084\u6c92\u8f49\u63db\u7684\u539f\u59cb\u8cc7\u6599\u3002"
    ))
  }
  if (code_cells > 0) {
    return(kit_callout(
      warn = TRUE,
      # 原文：目前使用：原始資料。資料中有  |  格看起來是遺漏值代碼
      "\u76ee\u524d\u4f7f\u7528\uff1a\u539f\u59cb\u8cc7\u6599\u3002\u8cc7\u6599\u4e2d\u6709 ", kit_num(code_cells), " \u683c\u770b\u8d77\u4f86\u662f\u907a\u6f0f\u503c\u4ee3\u78bc",
      # 原文：（7777、9999 等，或 SPSS 數值標籤寫著「遺漏值」「跳答」的值），
      "\uff087777\u30019999 \u7b49\uff0c\u6216 SPSS \u6578\u503c\u6a19\u7c64\u5beb\u8457\u300c\u907a\u6f0f\u503c\u300d\u300c\u8df3\u7b54\u300d\u7684\u503c\uff09\uff0c",
      # 原文：若它們代表遺漏，請先
      "\u82e5\u5b83\u5011\u4ee3\u8868\u907a\u6f0f\uff0c\u8acb\u5148",
      # 原文：到「遺漏值處理」處理
      shiny::actionLink(ns("goto_missing"), "\u5230\u300c\u907a\u6f0f\u503c\u8655\u7406\u300d\u8655\u7406"),
      # 原文：，否則會被當成一般數值。
      "\uff0c\u5426\u5247\u6703\u88ab\u7576\u6210\u4e00\u822c\u6578\u503c\u3002"
    ))
  }
  # 原文：目前使用：原始資料。
  shiny::tags$p(class = "kit-source-note", "\u76ee\u524d\u4f7f\u7528\uff1a\u539f\u59cb\u8cc7\u6599\u3002")
}


# ── 下載 ─────────────────────────────────────────────────────────────────────

#' 下載區（CSV + CSV.GZ + SAV），畫在 kit_run_tab_ui(downloads = TRUE) 的 output downloads
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
        # 原文：下載 CSV | 下載 CSV.GZ | 下載 SAV
        disabled("\u4e0b\u8f09 CSV"), disabled("\u4e0b\u8f09 CSV.GZ"), disabled("\u4e0b\u8f09 SAV"),
        # 原文：下載 DTA（Stata） | 下載 codebook
        disabled("\u4e0b\u8f09 DTA\uff08Stata\uff09"), disabled("\u4e0b\u8f09 codebook"),
        shiny::tags$small(
          class = "kit-hint",
          # 原文：先按上方「 | 」，才能下載結果。
          paste0("\u5148\u6309\u4e0a\u65b9\u300c", run_label, "\u300d\uff0c\u624d\u80fd\u4e0b\u8f09\u7d50\u679c\u3002")
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
      # 原文：下載 CSV
      button("download_csv", "\u4e0b\u8f09 CSV", "csv", write_kit_csv, kit_btn_class("secondary")),
      # 原文：下載 CSV.GZ
      button("download_csvgz", "\u4e0b\u8f09 CSV.GZ", "csv.gz", write_kit_csv_gz, kit_btn_class("secondary")),
      # 原文：下載 SAV（SPSS）
      button("download_sav", "\u4e0b\u8f09 SAV\uff08SPSS\uff09", "sav", write_sav_notify, kit_btn_class("secondary")),
      # 原文：下載 DTA（Stata）
      button("download_dta", "\u4e0b\u8f09 DTA\uff08Stata\uff09", "dta", write_kit_dta, kit_btn_class("secondary")),
      kit_download_button(
        # 原文：下載 codebook
        session, "download_codebook", "\u4e0b\u8f09 codebook",
        sub("\\.csv$", "_codebook.csv", filename_fn("csv")),
        function(file) write_kit_csv(codebook_table(df), file), kit_btn_class("secondary")
      ),
      shiny::tags$small(
        class = "kit-hint",
        # 原文：CSV 的遺漏值為空白儲存格，可直接用 Excel 開啟。
        "CSV \u7684\u907a\u6f0f\u503c\u70ba\u7a7a\u767d\u5132\u5b58\u683c\uff0c\u53ef\u76f4\u63a5\u7528 Excel \u958b\u555f\u3002",
        # 原文：CSV.GZ 是壓縮過的 CSV，檔案較小，可直接上傳 kitsUI；Excel 不能直接開。
        "CSV.GZ \u662f\u58d3\u7e2e\u904e\u7684 CSV\uff0c\u6a94\u6848\u8f03\u5c0f\uff0c\u53ef\u76f4\u63a5\u4e0a\u50b3 kitsUI\uff1bExcel \u4e0d\u80fd\u76f4\u63a5\u958b\u3002",
        # 原文：codebook 是資料字典：每個直欄一個橫列，有變項標籤、數值標籤、遺漏值設定與有效筆數（CSV，Excel 可以開）。
        "codebook \u662f\u8cc7\u6599\u5b57\u5178\uff1a\u6bcf\u500b\u76f4\u6b04\u4e00\u500b\u6a6b\u5217\uff0c\u6709\u8b8a\u9805\u6a19\u7c64\u3001\u6578\u503c\u6a19\u7c64\u3001\u907a\u6f0f\u503c\u8a2d\u5b9a\u8207\u6709\u6548\u7b46\u6578\uff08CSV\uff0cExcel \u53ef\u4ee5\u958b\uff09\u3002",
        if (length(renamed) > 0) {
          paste0(
            # 原文：匯出 SAV 時，有  |  個直欄名稱不符合 SPSS 規則，會自動改名：
            "\u532f\u51fa SAV \u6642\uff0c\u6709 ", kit_num(length(renamed)), " \u500b\u76f4\u6b04\u540d\u7a31\u4e0d\u7b26\u5408 SPSS \u898f\u5247\uff0c\u6703\u81ea\u52d5\u6539\u540d\uff1a",
            paste(
              # 原文：→
              utils::head(paste(names(renamed), "\u2192", renamed), 5),
              # 原文：、
              collapse = "\u3001"
            ),
            # 原文： … | 。
            if (length(renamed) > 5) " \u2026" else "", "\u3002"
          )
        }
      )
    )
  })
}

#' 「篩選與摘要」的狀態："on"、"stale"、"off"（working 沒有這一項時是 "off"）
#' @param working mod_filter_server() 或 mod_missing_server() 的回傳
#' @noRd
kit_filter_state <- function(working) {
  if (is.function(working$filter_state)) working$filter_state() else "off"
}

#' 結果檔名：joined_20261022_153000.csv
#'
#' 一律用臺灣時間（雲端主機是 UTC）。
#' @noRd
timestamped_name <- function(prefix) {
  function(ext) {
    paste0(prefix, "_", format(Sys.time(), "%Y%m%d_%H%M%S", tz = "Asia/Taipei"), ".", ext)
  }
}
