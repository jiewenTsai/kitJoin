# 檔案上傳：加入、排序、命名波次、預覽

#' @noRd
mod_upload_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    kit_tab_guide(
      # 原文：加入各波次的資料檔，替每個檔案取一個波次名稱，並確認資料讀得正確。
      goal = "\u52a0\u5165\u5404\u6ce2\u6b21\u7684\u8cc7\u6599\u6a94\uff0c\u66ff\u6bcf\u500b\u6a94\u6848\u53d6\u4e00\u500b\u6ce2\u6b21\u540d\u7a31\uff0c\u4e26\u78ba\u8a8d\u8cc7\u6599\u8b80\u5f97\u6b63\u78ba\u3002",
      steps = kit_steps(
        # 原文：按「選擇檔案…」加入各波次的 CSV 或 SAV 檔（可一次選多個，之後也可以再加）
        "\u6309\u300c\u9078\u64c7\u6a94\u6848\u2026\u300d\u52a0\u5165\u5404\u6ce2\u6b21\u7684 CSV \u6216 SAV \u6a94\uff08\u53ef\u4e00\u6b21\u9078\u591a\u500b\uff0c\u4e4b\u5f8c\u4e5f\u53ef\u4ee5\u518d\u52a0\uff09",
        # 原文：在清單中替每個檔案填波次名稱，例如 t1、t2、t3
        "\u5728\u6e05\u55ae\u4e2d\u66ff\u6bcf\u500b\u6a94\u6848\u586b\u6ce2\u6b21\u540d\u7a31\uff0c\u4f8b\u5982 t1\u3001t2\u3001t3",
        # 原文：用 ↑ ↓ 調整順序（第一個檔案是橫向資料串接與流失樣本的基準）
        "\u7528 \u2191 \u2193 \u8abf\u6574\u9806\u5e8f\uff08\u7b2c\u4e00\u500b\u6a94\u6848\u662f\u6a6b\u5411\u8cc7\u6599\u4e32\u63a5\u8207\u6d41\u5931\u6a23\u672c\u7684\u57fa\u6e96\uff09",
        # 原文：在下方預覽確認中文、數字顯示正常
        "\u5728\u4e0b\u65b9\u9810\u89bd\u78ba\u8a8d\u4e2d\u6587\u3001\u6578\u5b57\u986f\u793a\u6b63\u5e38"
      ),
      output = paste0(
        # 原文：檔案只是載入，還沒有合併。1 個檔案可以用遺漏值處理、kits 作答檔、長/寬格式轉換；
        "\u6a94\u6848\u53ea\u662f\u8f09\u5165\uff0c\u9084\u6c92\u6709\u5408\u4f75\u30021 \u500b\u6a94\u6848\u53ef\u4ee5\u7528\u907a\u6f0f\u503c\u8655\u7406\u3001kits \u4f5c\u7b54\u6a94\u3001\u9577/\u5bec\u683c\u5f0f\u8f49\u63db\uff1b",
        # 原文：橫向資料串接、直向波次疊加、流失樣本需要 2 個以上。
        "\u6a6b\u5411\u8cc7\u6599\u4e32\u63a5\u3001\u76f4\u5411\u6ce2\u6b21\u758a\u52a0\u3001\u6d41\u5931\u6a23\u672c\u9700\u8981 2 \u500b\u4ee5\u4e0a\u3002"
      ),
      technical = paste0(
        # 原文：CSV 會依序嘗試 UTF-8 與 BIG5（CP950）編碼；都不符合時以 Latin1 讀入並提醒可能是亂碼。
        "CSV \u6703\u4f9d\u5e8f\u5617\u8a66 UTF-8 \u8207 BIG5\uff08CP950\uff09\u7de8\u78bc\uff1b\u90fd\u4e0d\u7b26\u5408\u6642\u4ee5 Latin1 \u8b80\u5165\u4e26\u63d0\u9192\u53ef\u80fd\u662f\u4e82\u78bc\u3002",
        # 原文：直欄型別以整個檔案判斷，SAV 檔保留變項標籤與數值標籤。
        "\u76f4\u6b04\u578b\u5225\u4ee5\u6574\u500b\u6a94\u6848\u5224\u65b7\uff0cSAV \u6a94\u4fdd\u7559\u8b8a\u9805\u6a19\u7c64\u8207\u6578\u503c\u6a19\u7c64\u3002"
      )
    ),
    # 這一頁的主要動作是「選擇檔案…」：用 kit-file-primary 畫成主要按鈕
    shiny::tags$div(
      class = "kit-file-primary",
      shiny::fileInput(
        ns("files"),
        # 原文：加入檔案（可一次選多個）
        "\u52a0\u5165\u6a94\u6848\uff08\u53ef\u4e00\u6b21\u9078\u591a\u500b\uff09",
        multiple = TRUE,
        accept = c(".csv", ".sav", ".zsav", ".xlsx", ".xls", ".dta"),
        # 原文：選擇檔案…
        buttonLabel = "\u9078\u64c7\u6a94\u6848\u2026",
        # 原文：尚未選擇檔案
        placeholder = "\u5c1a\u672a\u9078\u64c7\u6a94\u6848",
        width = "34rem"
      )
    ),
    shiny::tags$small(
      class = "kit-hint",
      sprintf(
        # 原文：支援 CSV、Excel（.xlsx，讀第 1 個工作表）、SPSS（.sav）與 Stata（.dta）。單檔上限 %d MB。
        "\u652f\u63f4 CSV\u3001Excel\uff08.xlsx\uff0c\u8b80\u7b2c 1 \u500b\u5de5\u4f5c\u8868\uff09\u3001SPSS\uff08.sav\uff09\u8207 Stata\uff08.dta\uff09\u3002\u55ae\u6a94\u4e0a\u9650 %d MB\u3002",
        kit_max_upload_mb()
      )
    ),
    kit_advanced(
      # 原文：進階設定：SPSS 檔的中文編碼
      title = "\u9032\u968e\u8a2d\u5b9a\uff1aSPSS \u6a94\u7684\u4e2d\u6587\u7de8\u78bc",
      shiny::radioButtons(
        ns("sav_encoding"), NULL,
        choices = c(
          # 原文：自動判斷（建議）：依檔案標示；中文變成亂碼時改用 BIG5 重讀
          "\u81ea\u52d5\u5224\u65b7\uff08\u5efa\u8b70\uff09\uff1a\u4f9d\u6a94\u6848\u6a19\u793a\uff1b\u4e2d\u6587\u8b8a\u6210\u4e82\u78bc\u6642\u6539\u7528 BIG5 \u91cd\u8b80" = "auto",
          # 原文：只依檔案標示
          "\u53ea\u4f9d\u6a94\u6848\u6a19\u793a" = "file",
          # 原文：一律用 BIG5（CP950）：舊版 SPSS 在繁體中文 Windows 存的檔
          "\u4e00\u5f8b\u7528 BIG5\uff08CP950\uff09\uff1a\u820a\u7248 SPSS \u5728\u7e41\u9ad4\u4e2d\u6587 Windows \u5b58\u7684\u6a94" = "CP950"
        ),
        selected = "auto", width = "100%"
      ),
      shiny::tags$small(
        class = "kit-hint",
        # 原文：只影響 SAV 檔。改了以後，請把檔案移除再重新加入才會套用。
        "\u53ea\u5f71\u97ff SAV \u6a94\u3002\u6539\u4e86\u4ee5\u5f8c\uff0c\u8acb\u628a\u6a94\u6848\u79fb\u9664\u518d\u91cd\u65b0\u52a0\u5165\u624d\u6703\u5957\u7528\u3002"
      ),
      shiny::checkboxInput(
        ns("user_na"),
        # 原文：保留 SPSS／Stata 的使用者自訂遺漏值（例如 99 = 拒答）：讀進來時不先變成空白
        "\u4fdd\u7559 SPSS\uff0fStata \u7684\u4f7f\u7528\u8005\u81ea\u8a02\u907a\u6f0f\u503c\uff08\u4f8b\u5982 99 = \u62d2\u7b54\uff09\uff1a\u8b80\u9032\u4f86\u6642\u4e0d\u5148\u8b8a\u6210\u7a7a\u767d",
        value = FALSE, width = "100%"
      ),
      shiny::tags$small(
        class = "kit-hint",
        # 原文：勾了以後原來的代碼和遺漏值設定都留著：到「遺漏值處理」再決定要不要轉成空白；
        "\u52fe\u4e86\u4ee5\u5f8c\u539f\u4f86\u7684\u4ee3\u78bc\u548c\u907a\u6f0f\u503c\u8a2d\u5b9a\u90fd\u7559\u8457\uff1a\u5230\u300c\u907a\u6f0f\u503c\u8655\u7406\u300d\u518d\u6c7a\u5b9a\u8981\u4e0d\u8981\u8f49\u6210\u7a7a\u767d\uff1b",
        # 原文：直接下載 SAV 時，SPSS 裡也還是使用者自訂遺漏值。改了以後，請把檔案移除再重新加入。
        "\u76f4\u63a5\u4e0b\u8f09 SAV \u6642\uff0cSPSS \u88e1\u4e5f\u9084\u662f\u4f7f\u7528\u8005\u81ea\u8a02\u907a\u6f0f\u503c\u3002\u6539\u4e86\u4ee5\u5f8c\uff0c\u8acb\u628a\u6a94\u6848\u79fb\u9664\u518d\u91cd\u65b0\u52a0\u5165\u3002"
      )
    ),
    shiny::uiOutput(ns("status")),
    shiny::uiOutput(ns("file_list")),
    kit_details_row(
      # 原文：預覽檔案內容（確認中文、數字顯示正常）
      kit_detail("\u9810\u89bd\u6a94\u6848\u5167\u5bb9\uff08\u78ba\u8a8d\u4e2d\u6587\u3001\u6578\u5b57\u986f\u793a\u6b63\u5e38\uff09", shiny::uiOutput(ns("preview"))),
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
            sav_encoding = input$sav_encoding %||% "auto",
            user_na = isTRUE(input$user_na)
          )
          check_not_empty(out$data, name)
          out
        }, error = function(e) e)
        if (inherits(read, "error")) {
          msg <- conditionMessage(read)
          if (!inherits(read, "kit_user_error")) {
            # 原文：無法讀取  | ：
            msg <- paste0("\u7121\u6cd5\u8b80\u53d6 ", name, "\uff1a", msg)
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
              # 原文： 已經加入過了（檔名相同） |  的內容和已加入的  |  完全相同
              name, if (same$name == name) " \u5df2\u7d93\u52a0\u5165\u904e\u4e86\uff08\u6a94\u540d\u76f8\u540c\uff09" else paste0(" \u7684\u5167\u5bb9\u548c\u5df2\u52a0\u5165\u7684 ", same$name, " \u5b8c\u5168\u76f8\u540c"),
              # 原文：。如果是不小心重複加入，請在清單中按「移除」。
              "\u3002\u5982\u679c\u662f\u4e0d\u5c0f\u5fc3\u91cd\u8907\u52a0\u5165\uff0c\u8acb\u5728\u6e05\u55ae\u4e2d\u6309\u300c\u79fb\u9664\u300d\u3002"
            ),
            type = "warning",
            duration = KIT_NOTIFY_ERROR_SECONDS
          )
        }
        if (length(read$sheets) > 1) {
          shiny::showNotification(
            # 原文： 有  |  個工作表，只讀了第 1 個（
            paste0(name, " \u6709 ", length(read$sheets), " \u500b\u5de5\u4f5c\u8868\uff0c\u53ea\u8b80\u4e86\u7b2c 1 \u500b\uff08", read$sheets[1],
                   # 原文：）。要用其他工作表，請在 Excel 把它移到最前面再加入。
                   "\uff09\u3002\u8981\u7528\u5176\u4ed6\u5de5\u4f5c\u8868\uff0c\u8acb\u5728 Excel \u628a\u5b83\u79fb\u5230\u6700\u524d\u9762\u518d\u52a0\u5165\u3002"),
            type = "warning", duration = KIT_NOTIFY_ERROR_SECONDS
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
        # 每一列固定兩格（上移、下移）：用不到的那一格放看不見的佔位，按鈕才不會左右跳
        btn <- function(prefix, label, show) {
          if (show) return(kit_button(ns(paste0(prefix, r$key)), label))
          shiny::tags$button(
            type = "button", class = paste("btn invisible", kit_btn_class("secondary")),
            disabled = NA, tabindex = "-1", `aria-hidden` = "true", label
          )
        }
        shiny::tags$tr(
          # 原文：順序
          shiny::tags$td(`data-label` = "\u9806\u5e8f", pos),
          shiny::tags$td(
            # 原文：檔案
            `data-label` = "\u6a94\u6848",
            shiny::tags$strong(r$name), shiny::tags$br(),
            shiny::tags$small(
              class = "text-muted",
              kit_dims(nrow(r$data), ncol(r$data))
            ),
            file_read_notes(r)
          ),
          shiny::tags$td(
            # 原文：波次名稱
            `data-label` = "\u6ce2\u6b21\u540d\u7a31",
            shiny::textInput(
              ns(paste0("label_", r$key)), NULL,
              value = shiny::isolate(labels[[r$key]]), width = "8rem"
            )
          ),
          shiny::tags$td(
            # 原文：調整順序
            class = "kit-row-buttons", `data-label` = "\u8abf\u6574\u9806\u5e8f",
            # 原文：↑ 上移
            btn("up_", "\u2191 \u4e0a\u79fb", pos > 1),
            # 原文：↓ 下移
            btn("down_", "\u2193 \u4e0b\u79fb", pos < n)
          ),
          shiny::tags$td(
            # 原文：移除
            kit_button(ns(paste0("remove_", r$key)), "\u79fb\u9664", "danger")
          )
        )
      })
      shiny::tagList(
        # 原文：已加入的檔案
        shiny::h4("\u5df2\u52a0\u5165\u7684\u6a94\u6848"),
        shiny::tags$div(
          class = "kit-table-scroll",
          shiny::tags$table(
            class = "table table-sm align-middle kit-file-table",
            shiny::tags$thead(shiny::tags$tr(
              # 原文：順序 | 檔案
              shiny::tags$th("\u9806\u5e8f"), shiny::tags$th("\u6a94\u6848"),
              # 原文：波次名稱 | 調整順序
              shiny::tags$th("\u6ce2\u6b21\u540d\u7a31"), shiny::tags$th("\u8abf\u6574\u9806\u5e8f"), shiny::tags$th("")
            )),
            shiny::tags$tbody(rows)
          )
        ),
        shiny::tags$small(
          class = "kit-hint",
          # 原文：波次名稱：橫向資料串接時加在直欄名稱後面（score → score_t1）；
          "\u6ce2\u6b21\u540d\u7a31\uff1a\u6a6b\u5411\u8cc7\u6599\u4e32\u63a5\u6642\u52a0\u5728\u76f4\u6b04\u540d\u7a31\u5f8c\u9762\uff08score \u2192 score_t1\uff09\uff1b",
          # 原文：直向波次疊加時填進 wave 直欄。每個檔案要不同。
          "\u76f4\u5411\u6ce2\u6b21\u758a\u52a0\u6642\u586b\u9032 wave \u76f4\u6b04\u3002\u6bcf\u500b\u6a94\u6848\u8981\u4e0d\u540c\u3002"
        )
      )
    })

    # 狀態框：加入了幾個檔案；讀不了的檔案、亂碼、格式異常要注意；波次名稱有問題時停下來
    status <- shiny::reactive({
      recs <- records()
      bad <- failed()
      if (length(recs) == 0) {
        if (length(bad) == 0) return(kit_status())
        # 原文：沒有加入任何檔案：檔案讀不了。
        return(kit_status("error", "\u6c92\u6709\u52a0\u5165\u4efb\u4f55\u6a94\u6848\uff1a\u6a94\u6848\u8b80\u4e0d\u4e86\u3002", warnings = bad))
      }
      problem <- check_wave_labels(current_labels(), purrr::map_chr(recs, "name"))
      if (!is.null(problem)) {
        # 原文：波次名稱要修改，改好以後才能串接或疊加。
        return(kit_status("error", "\u6ce2\u6b21\u540d\u7a31\u8981\u4fee\u6539\uff0c\u6539\u597d\u4ee5\u5f8c\u624d\u80fd\u4e32\u63a5\u6216\u758a\u52a0\u3002", warnings = problem))
      }
      n_rows <- sum(purrr::map_int(recs, function(r) nrow(r$data)))
      warns <- c(
        bad,
        purrr::map_chr(purrr::keep(recs, function(r) identical(r$encoding, "Latin1")), function(r) {
          # 原文：：中文可能是亂碼，請用 Excel 另存為「CSV UTF-8」後重新加入。
          paste0(r$name, "\uff1a\u4e2d\u6587\u53ef\u80fd\u662f\u4e82\u78bc\uff0c\u8acb\u7528 Excel \u53e6\u5b58\u70ba\u300cCSV UTF-8\u300d\u5f8c\u91cd\u65b0\u52a0\u5165\u3002")
        }),
        purrr::map_chr(purrr::keep(recs, function(r) r$n_problems > 0), function(r) {
          # 原文：：有  |  處資料格式異常（例如直欄數不對）。
          paste0(r$name, "\uff1a\u6709 ", kit_num(r$n_problems), " \u8655\u8cc7\u6599\u683c\u5f0f\u7570\u5e38\uff08\u4f8b\u5982\u76f4\u6b04\u6578\u4e0d\u5c0d\uff09\u3002")
        })
      )
      kit_done_status(
        # 原文：已加入  |  個檔案，共  |  橫列。
        paste0("\u5df2\u52a0\u5165 ", length(recs), " \u500b\u6a94\u6848\uff0c\u5171 ", kit_num(n_rows), " \u6a6b\u5217\u3002"),
        warnings = warns
      )
    })
    output$status <- shiny::renderUI(kit_status_ui(status()))

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
          # 原文：已加入 1 個檔案。可以用：
          shiny::tags$strong("\u5df2\u52a0\u5165 1 \u500b\u6a94\u6848\u3002\u53ef\u4ee5\u7528\uff1a"),
          shiny::tags$ul(
            # 原文：只改遺漏值代碼 → 到  | 遺漏值處理
            shiny::tags$li("\u53ea\u6539\u907a\u6f0f\u503c\u4ee3\u78bc \u2192 \u5230 ", link("goto_missing", "\u907a\u6f0f\u503c\u8655\u7406"),
                           # 原文：，處理後直接下載
                           "\uff0c\u8655\u7406\u5f8c\u76f4\u63a5\u4e0b\u8f09"),
            # 原文：要做 kits 或 kitsUI 的作答檔 → 到 
            shiny::tags$li("\u8981\u505a kits \u6216 kitsUI \u7684\u4f5c\u7b54\u6a94 \u2192 \u5230 ",
                           # 原文：kits 作答檔
                           link("goto_kitsdata", "kits \u4f5c\u7b54\u6a94")),
            # 原文：要長轉寬或寬轉長 → 到  | 長/寬格式轉換
            shiny::tags$li("\u8981\u9577\u8f49\u5bec\u6216\u5bec\u8f49\u9577 \u2192 \u5230 ", link("goto_reshape", "\u9577/\u5bec\u683c\u5f0f\u8f49\u63db"))
          ),
          # 原文：橫向資料串接、直向波次疊加與流失樣本需要至少 2 個波次的檔案。
          "\u6a6b\u5411\u8cc7\u6599\u4e32\u63a5\u3001\u76f4\u5411\u6ce2\u6b21\u758a\u52a0\u8207\u6d41\u5931\u6a23\u672c\u9700\u8981\u81f3\u5c11 2 \u500b\u6ce2\u6b21\u7684\u6a94\u6848\u3002"
        )
      } else {
        shiny::tagList(
          # 原文：接下來可以：
          shiny::tags$strong("\u63a5\u4e0b\u4f86\u53ef\u4ee5\uff1a"),
          shiny::tags$ul(
            if (has_codes()) {
              shiny::tags$li(
                # 原文：資料中有遺漏值代碼（例如 9999，或數值標籤寫著「遺漏值」的值）→ 建議先到 
                "\u8cc7\u6599\u4e2d\u6709\u907a\u6f0f\u503c\u4ee3\u78bc\uff08\u4f8b\u5982 9999\uff0c\u6216\u6578\u503c\u6a19\u7c64\u5beb\u8457\u300c\u907a\u6f0f\u503c\u300d\u7684\u503c\uff09\u2192 \u5efa\u8b70\u5148\u5230 ",
                # 原文：遺漏值處理 | （選用）
                link("goto_missing", "\u907a\u6f0f\u503c\u8655\u7406"), "\uff08\u9078\u7528\uff09"
              )
            },
            # 原文：要「每人一個橫列、各波變項並排」→ 到  | 橫向資料串接
            shiny::tags$li("\u8981\u300c\u6bcf\u4eba\u4e00\u500b\u6a6b\u5217\u3001\u5404\u6ce2\u8b8a\u9805\u4e26\u6392\u300d\u2192 \u5230 ", link("goto_join", "\u6a6b\u5411\u8cc7\u6599\u4e32\u63a5")),
            # 原文：要「每人每波一個橫列」→ 到  | 直向波次疊加
            shiny::tags$li("\u8981\u300c\u6bcf\u4eba\u6bcf\u6ce2\u4e00\u500b\u6a6b\u5217\u300d\u2192 \u5230 ", link("goto_stack", "\u76f4\u5411\u6ce2\u6b21\u758a\u52a0")),
            # 原文：要標記誰在後期沒有繼續參加 → 到  | 流失樣本
            shiny::tags$li("\u8981\u6a19\u8a18\u8ab0\u5728\u5f8c\u671f\u6c92\u6709\u7e7c\u7e8c\u53c3\u52a0 \u2192 \u5230 ", link("goto_attrition", "\u6d41\u5931\u6a23\u672c")),
            # 原文：要做 kits 或 kitsUI 的作答檔 → 到  | kits 作答檔
            shiny::tags$li("\u8981\u505a kits \u6216 kitsUI \u7684\u4f5c\u7b54\u6a94 \u2192 \u5230 ", link("goto_kitsdata", "kits \u4f5c\u7b54\u6a94")),
            # 原文：要長轉寬或寬轉長 → 到  | 長/寬格式轉換
            shiny::tags$li("\u8981\u9577\u8f49\u5bec\u6216\u5bec\u8f49\u9577 \u2192 \u5230 ", link("goto_reshape", "\u9577/\u5bec\u683c\u5f0f\u8f49\u63db"))
          )
        )
      }
      if (n == 1 && has_codes()) {
        # 原文： 資料中有遺漏值代碼，建議先到「遺漏值處理」。
        uses <- shiny::tagList(uses, " \u8cc7\u6599\u4e2d\u6709\u907a\u6f0f\u503c\u4ee3\u78bc\uff0c\u5efa\u8b70\u5148\u5230\u300c\u907a\u6f0f\u503c\u8655\u7406\u300d\u3002")
      }
      target <- next_target()
      kit_next_step(
        text = uses,
        button_id = if (!is.null(target)) ns("go_next"),
        # 原文：下一步：遺漏值處理
        label = switch(target %||% "", missing = "\u4e0b\u4e00\u6b65\uff1a\u907a\u6f0f\u503c\u8655\u7406",
                       # 原文：下一步：橫向資料串接
                       join = "\u4e0b\u4e00\u6b65\uff1a\u6a6b\u5411\u8cc7\u6599\u4e32\u63a5", NULL)
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
      # 原文：還沒有加入檔案。
      if (length(recs) == 0) return(shiny::tags$p(class = "kit-hint", "\u9084\u6c92\u6709\u52a0\u5165\u6a94\u6848\u3002"))
      choices <- stats::setNames(
        purrr::map_chr(recs, "key"),
        purrr::map_chr(recs, "name")
      )
      selected <- shiny::isolate(input$preview_key)
      if (is.null(selected) || !selected %in% choices) selected <- choices[[1]]
      shiny::tagList(
        shiny::selectInput(
          ns("preview_key"),
          # 原文：要預覽的檔案
          "\u8981\u9810\u89bd\u7684\u6a94\u6848",
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
      encodings = shiny::reactive(purrr::map_chr(records(), "encoding")),
      # 讀檔時保留了 SPSS／Stata 的使用者自訂遺漏值（「用 R 怎麼做」的讀檔程式碼跟著寫）
      user_na = shiny::reactive(isTRUE(input$user_na))
    )

    output$code <- shiny::renderUI(
      kit_code_blocks(code_read(code_files_from(files)), note = KIT_CODE_NOTE_JOIN)
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
        # 原文：　編碼：BIG5。這個 SPSS 檔沒有標明正確的編碼，中文原本會變成亂碼，
        "\u3000\u7de8\u78bc\uff1aBIG5\u3002\u9019\u500b SPSS \u6a94\u6c92\u6709\u6a19\u660e\u6b63\u78ba\u7684\u7de8\u78bc\uff0c\u4e2d\u6587\u539f\u672c\u6703\u8b8a\u6210\u4e82\u78bc\uff0c",
        # 原文：已改用 BIG5（CP950）讀取。請在下方預覽確認中文正常。
        "\u5df2\u6539\u7528 BIG5\uff08CP950\uff09\u8b80\u53d6\u3002\u8acb\u5728\u4e0b\u65b9\u9810\u89bd\u78ba\u8a8d\u4e2d\u6587\u6b63\u5e38\u3002"
      )
    } else if (!is.na(enc) && enc == "Latin1") {
      shiny::tags$div(
        class = "text-danger small",
        # 原文：⚠ 中文可能是亂碼：請用 Excel 另存為「CSV UTF-8」後重新上傳。
        "\u26a0 \u4e2d\u6587\u53ef\u80fd\u662f\u4e82\u78bc\uff1a\u8acb\u7528 Excel \u53e6\u5b58\u70ba\u300cCSV UTF-8\u300d\u5f8c\u91cd\u65b0\u4e0a\u50b3\u3002"
      )
    } else if (!is.na(enc)) {
      # 原文：　編碼：
      shiny::tags$small(class = "text-muted", paste0("\u3000\u7de8\u78bc\uff1a", enc))
    },
    if (!is.null(record$delim) && record$delim != ",") {
      shiny::tags$div(
        class = "text-muted small",
        paste0(
          # 原文：　分隔符號： | 分號（;）
          "\u3000\u5206\u9694\u7b26\u865f\uff1a", if (record$delim == ";") "\u5206\u865f\uff08;\uff09" else "Tab",
          # 原文：。這個檔案的直欄不是用逗號分隔，已自動改用 | 分號 | 讀取。
          "\u3002\u9019\u500b\u6a94\u6848\u7684\u76f4\u6b04\u4e0d\u662f\u7528\u9017\u865f\u5206\u9694\uff0c\u5df2\u81ea\u52d5\u6539\u7528", if (record$delim == ";") "\u5206\u865f" else " Tab", "\u8b80\u53d6\u3002"
        )
      )
    },
    if (record$n_problems > 0) {
      shiny::tags$div(
        class = "text-danger small",
        # 原文：⚠ 有  |  處資料格式異常（例如直欄數不對），請用 Excel 檢查原檔。
        paste0("\u26a0 \u6709 ", kit_num(record$n_problems), " \u8655\u8cc7\u6599\u683c\u5f0f\u7570\u5e38\uff08\u4f8b\u5982\u76f4\u6b04\u6578\u4e0d\u5c0d\uff09\uff0c\u8acb\u7528 Excel \u6aa2\u67e5\u539f\u6a94\u3002")
      )
    }
  )
}
