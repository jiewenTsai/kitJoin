# 流程紀錄：這次在 app 裡做了哪些步驟，可以下載成一份完整的 R 腳本與 JSON 紀錄
#
# 每一頁按執行、有結果時記一筆：哪一頁、什麼時間、用了哪些檔案、結果幾列幾欄，以及那一頁
# 「用 R 怎麼做」的程式碼（就是這一頁當時的設定）。
#   R 腳本：讀檔的程式碼，接著每一頁最後一次執行的程式碼，依執行的先後排；
#   JSON：每一次執行都留著（包括重做的），加上輸入檔的指紋（MD5），之後可以核對是不是同一批檔案。

#' 流程紀錄的畫面：放在所有分頁下面
#' @noRd
pipeline_log_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("log"))
}

#' 流程紀錄
#'
#' @param files mod_upload_server() 的回傳
#' @param pages 具名 list：名稱是分頁名稱，值是模組回傳的 list（要有 result、code）
#' @return reactive：紀錄（list，每次執行一筆）
#' @noRd
pipeline_log_server <- function(id, files, pages) {
  shiny::moduleServer(id, function(input, output, session) {
    entries <- shiny::reactiveVal(list())

    for (page in names(pages)) {
      local({
        label <- page
        mod <- pages[[label]]
        shiny::observeEvent(mod$result(), {
          res <- mod$result()
          if (is.null(res)) return()
          code <- tryCatch(code_tidy_text(mod$code()), error = function(e) NULL)
          dims <- result_dims(res)
          entries(c(entries(), list(list(
            page = label,
            time = format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "Asia/Taipei"),
            files = shiny::isolate(files$names()),
            rows = dims[1], cols = dims[2],
            code = code
          ))))
        }, ignoreNULL = TRUE)
      })
    }

    output$log <- shiny::renderUI({
      log <- entries()
      if (length(log) == 0) return(NULL)
      tbl <- pipeline_log_table(log)
      shiny::tags$details(
        class = "kit-example",
        # 原文：流程紀錄：這次做了  |  個步驟（下載完整的 R 腳本與紀錄）
        shiny::tags$summary(paste0("\u6d41\u7a0b\u7d00\u9304\uff1a\u9019\u6b21\u505a\u4e86 ", kit_num(length(log)), " \u500b\u6b65\u9a5f\uff08\u4e0b\u8f09\u5b8c\u6574\u7684 R \u8173\u672c\u8207\u7d00\u9304\uff09")),
        shiny::tags$div(class = "kit-table-scroll", kit_static_table(tbl)),
        shiny::tags$div(
          class = "kit-downloads",
          kit_download_button(
            # 原文：下載 R 腳本
            session, "script", "\u4e0b\u8f09 R \u8173\u672c",
            timestamped_name("kitjoin_pipeline")("R"),
            function(file) {
              writeLines(enc2utf8(pipeline_script(shiny::isolate(entries()), shiny::isolate(code_files_from(files)))),
                         file, useBytes = TRUE)
            },
            kit_btn_class("secondary")
          ),
          kit_download_button(
            # 原文：下載紀錄（JSON）
            session, "json", "\u4e0b\u8f09\u7d00\u9304\uff08JSON\uff09",
            timestamped_name("kitjoin_log")("json"),
            function(file) {
              jsonlite::write_json(
                pipeline_log_json(shiny::isolate(entries()), shiny::isolate(files$names()), shiny::isolate(files$data())),
                file, pretty = TRUE, auto_unbox = TRUE, null = "null"
              )
            },
            kit_btn_class("secondary")
          ),
          shiny::tags$small(
            class = "kit-hint",
            # 原文：R 腳本：讀檔，接著每一頁最後一次執行的程式碼，依執行的先後排；是示範，app 另外還有編碼修正與核對。
            "R \u8173\u672c\uff1a\u8b80\u6a94\uff0c\u63a5\u8457\u6bcf\u4e00\u9801\u6700\u5f8c\u4e00\u6b21\u57f7\u884c\u7684\u7a0b\u5f0f\u78bc\uff0c\u4f9d\u57f7\u884c\u7684\u5148\u5f8c\u6392\uff1b\u662f\u793a\u7bc4\uff0capp \u53e6\u5916\u9084\u6709\u7de8\u78bc\u4fee\u6b63\u8207\u6838\u5c0d\u3002",
            # 原文：紀錄（JSON）：每一次執行都留著，加上輸入檔的指紋（MD5），之後可以核對是不是同一批檔案。
            "\u7d00\u9304\uff08JSON\uff09\uff1a\u6bcf\u4e00\u6b21\u57f7\u884c\u90fd\u7559\u8457\uff0c\u52a0\u4e0a\u8f38\u5165\u6a94\u7684\u6307\u7d0b\uff08MD5\uff09\uff0c\u4e4b\u5f8c\u53ef\u4ee5\u6838\u5c0d\u662f\u4e0d\u662f\u540c\u4e00\u6279\u6a94\u6848\u3002"
          )
        )
      )
    })

    entries
  })
}

#' 「用 R 怎麼做」的程式碼取 tidyverse 版本（list(tidy, base) 或一段文字）
#' @noRd
code_tidy_text <- function(code) {
  if (is.null(code)) return(NULL)
  if (is.list(code)) code <- code$tidy %||% code[[1]]
  paste(unlist(code), collapse = "\n")
}

#' 結果的列數、欄數：結果裡第一個資料框（result、data 或 data 的第 1 個檔案）
#' @noRd
result_dims <- function(res) {
  cand <- list(res$result, res$data, if (is.list(res$data) && !is.data.frame(res$data)) res$data[[1]], res)
  for (x in cand) if (is.data.frame(x)) return(c(nrow(x), ncol(x)))
  c(NA_integer_, NA_integer_)
}

#' 畫面上的紀錄表
#' @noRd
pipeline_log_table <- function(log) {
  tibble::tibble(
    # 原文：順序
    "\u9806\u5e8f" = seq_along(log),
    # 原文：時間
    "\u6642\u9593" = vapply(log, `[[`, "", "time"),
    # 原文：分頁
    "\u5206\u9801" = vapply(log, `[[`, "", "page"),
    # 原文：檔案數
    "\u6a94\u6848\u6578" = vapply(log, function(e) length(e$files), integer(1)),
    # 原文：結果
    "\u7d50\u679c" = vapply(log, function(e) {
      # 原文： 列 ×  |  欄
      if (is.na(e$rows)) "" else paste0(kit_num(e$rows), " \u5217 \u00d7 ", kit_num(e$cols), " \u6b04")
    }, character(1))
  )
}

#' 完整的 R 腳本：讀檔，接著每一頁最後一次執行的程式碼（依執行先後）
#' @noRd
pipeline_script <- function(log, f) {
  last <- log[!duplicated(vapply(log, `[[`, "", "page"), fromLast = TRUE)]
  read <- code_read(f)$tidy
  steps <- unlist(lapply(last, function(e) {
    c("",
      # 原文：# ──  | （ | ） | ─
      paste0("# \u2500\u2500 ", e$page, "\uff08", e$time, "\uff09", strrep("\u2500", 20)),
      # 原文：# （這一頁沒有程式碼）
      e$code %||% "# \uff08\u9019\u4e00\u9801\u6c92\u6709\u7a0b\u5f0f\u78bc\uff09")
  }))
  c(
    # 原文： 的流程紀錄（
    paste0("# kitJoin ", kitjoin_version(), " \u7684\u6d41\u7a0b\u7d00\u9304\uff08",
           # 原文：）
           format(Sys.time(), "%Y-%m-%d %H:%M", tz = "Asia/Taipei"), "\uff09"),
    # 原文：# 讀檔，接著每一頁最後一次執行的程式碼，依執行的先後排。
    "# \u8b80\u6a94\uff0c\u63a5\u8457\u6bcf\u4e00\u9801\u6700\u5f8c\u4e00\u6b21\u57f7\u884c\u7684\u7a0b\u5f0f\u78bc\uff0c\u4f9d\u57f7\u884c\u7684\u5148\u5f8c\u6392\u3002",
    # 原文：# 各頁的程式碼是簡化的示範；app 實際處理另外還有編碼修正、型別檢查與核對等步驟。
    "# \u5404\u9801\u7684\u7a0b\u5f0f\u78bc\u662f\u7c21\u5316\u7684\u793a\u7bc4\uff1bapp \u5be6\u969b\u8655\u7406\u53e6\u5916\u9084\u6709\u7de8\u78bc\u4fee\u6b63\u3001\u578b\u5225\u6aa2\u67e5\u8207\u6838\u5c0d\u7b49\u6b65\u9a5f\u3002",
    "",
    read,
    steps
  )
}

#' JSON 紀錄：輸入檔（檔名、列數、欄數、MD5）與每一次執行
#' @noRd
pipeline_log_json <- function(log, file_names, dfs) {
  list(
    app = "kitJoin",
    version = as.character(kitjoin_version()),
    created = format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "Asia/Taipei"),
    timezone = "Asia/Taipei",
    files = lapply(seq_along(file_names), function(i) list(
      name = file_names[i], rows = nrow(dfs[[i]]), cols = ncol(dfs[[i]]), md5 = data_md5(dfs[[i]])
    )),
    steps = lapply(log, function(e) e[c("page", "time", "files", "rows", "cols", "code")])
  )
}

#' kitJoin 的版本（雲端版用 runApp(資料夾) 跑、套件沒有安裝，也讀得到 DESCRIPTION）
#' @noRd
kitjoin_version <- function() {
  as.character(kit_app_description("kitJoin")$version)
}

#' 資料框的 MD5（同樣的資料讀進來會一樣；改了任何一格就不一樣）
#' @noRd
data_md5 <- function(df) {
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(df, tmp, version = 3, compress = FALSE)
  unname(tools::md5sum(tmp))
}
