# SAV 等檔案預設上傳上限為 5MB，在此提高（單位：bytes）
options(shiny.maxRequestSize = 500 * 1024^2)

library(shiny)
library(bslib)
library(dplyr)
library(readr)
library(haven)
library(purrr)

# 修正字元欄位為合法 UTF-8（避免預覽時 invalid UTF-8 錯誤）
fix_utf8_chr <- function(x) {
  if (!is.character(x)) return(x)
  x <- as.character(x)
  iconv(x, from = "", to = "UTF-8", sub = "")
}

fix_utf8_df <- function(df) {
  df %>%
    mutate(across(everything(), function(x) {
      if (inherits(x, "labelled") && is.numeric(x)) {
        x
      } else if (inherits(x, "labelled") || is.character(x) || is.factor(x)) {
        fix_utf8_chr(as.character(x))
      } else {
        x
      }
    }))
}

# 預覽用：將 haven 標籤欄轉成一般字元
df_for_display <- function(df, n = NULL) {
  out <- fix_utf8_df(df)
  if (!is.null(n)) out <- head(out, n)
  out
}

# 讀取 CSV（自動嘗試常見編碼）
read_csv_auto <- function(path) {
  try_enc <- function(enc) {
    read_csv(
      path,
      show_col_types = FALSE,
      locale = locale(encoding = enc)
    )
  }
  df <- tryCatch(try_enc("UTF-8"), error = function(e) NULL)
  if (is.null(df)) df <- tryCatch(try_enc("BIG5"), error = function(e) NULL)
  if (is.null(df)) df <- try_enc("Latin1")
  fix_utf8_df(df)
}

# 讀取單一檔案
read_data_file <- function(path, name) {
  ext <- tolower(tools::file_ext(name))
  if (ext == "csv") {
    read_csv_auto(path)
  } else if (ext %in% c("sav", "zsav")) {
    fix_utf8_df(read_sav(path))
  } else {
    stop("不支援的檔案格式：", name)
  }
}

# 對非 by 欄位加後綴
apply_suffix <- function(df, suffix, by_vars) {
  if (is.null(suffix) || suffix == "") return(df)
  rename_with(df, ~ paste0(.x, suffix), .cols = -all_of(by_vars))
}

# by 鍵重複情形
key_dup_stats <- function(df, by_vars) {
  key_df <- df %>% select(all_of(by_vars))
  key_count <- key_df %>% count(across(all_of(by_vars)))
  list(
    n_rows = nrow(df),
    n_unique_keys = nrow(distinct(key_df)),
    n_dup_key_groups = sum(key_count$n > 1, na.rm = TRUE),
    max_rows_per_key = if (nrow(key_count) > 0) max(key_count$n) else 0L
  )
}

has_dup_keys <- function(df, by_vars) {
  key_dup_stats(df, by_vars)$n_dup_key_groups > 0
}

keys_in_all_files <- function(dfs, by_vars) {
  key_sets <- map(dfs, ~ .x %>% select(all_of(by_vars)) %>% distinct())
  reduce(key_sets, inner_join, by = by_vars) %>% nrow()
}

keys_in_any_file <- function(dfs, by_vars) {
  map(dfs, ~ .x %>% select(all_of(by_vars)) %>% distinct()) %>%
    bind_rows() %>%
    distinct() %>%
    nrow()
}

# 串接後核對：各檔統計、逐步合併、最終列欄數是否符合規則
build_join_audit <- function(ordered_dfs, file_names, by_vars, join_method, result) {
  join_fn <- get(join_method, envir = asNamespace("dplyr"))
  n_by <- length(by_vars)
  any_dup <- any(map_lgl(ordered_dfs, ~ has_dup_keys(.x, by_vars)))

  file_stats <- imap_dfr(ordered_dfs, function(df, i) {
    st <- key_dup_stats(df, by_vars)
    tibble(
      順序 = i,
      檔案 = file_names[i],
      列數 = st$n_rows,
      欄位數 = ncol(df),
      新增欄位數 = ncol(df) - n_by,
      不重複鍵數 = st$n_unique_keys,
      重複鍵組數 = st$n_dup_key_groups,
      單鍵最多列數 = st$max_rows_per_key
    )
  })

  step_rows <- list()
  acc <- ordered_dfs[[1]]
  for (i in seq_along(ordered_dfs)[-1]) {
    right <- ordered_dfs[[i]]
    exp_ncol <- ncol(acc) + ncol(right) - n_by
    new_acc <- join_fn(acc, right, by = by_vars)
    step_rows[[length(step_rows) + 1]] <- tibble(
      步驟 = paste0("第 ", i, " 步：", file_names[i - 1], " ", join_method, " ", file_names[i]),
      左表列數 = nrow(acc),
      右表列數 = nrow(right),
      結果列數 = nrow(new_acc),
      預期欄位數 = exp_ncol,
      實際欄位數 = ncol(new_acc),
      欄位數符合 = exp_ncol == ncol(new_acc)
    )
    acc <- new_acc
  }
  join_steps <- bind_rows(step_rows)

  exp_ncol_final <- n_by + sum(file_stats$新增欄位數)
  act_ncol <- ncol(result)
  act_nrow <- nrow(result)
  ncol_ok <- exp_ncol_final == act_ncol

  keys_all <- keys_in_all_files(ordered_dfs, by_vars)
  keys_any <- keys_in_any_file(ordered_dfs, by_vars)

  exp_nrow <- switch(
    join_method,
    left_join = nrow(ordered_dfs[[1]]),
    right_join = nrow(ordered_dfs[[length(ordered_dfs)]]),
    inner_join = keys_all,
    full_join = keys_any
  )

  exp_nrow_note <- switch(
    join_method,
    left_join = "以順序第 1 個檔案為主體；by 鍵在後續檔案皆唯一且無額外配對列時，列數應等於第 1 檔列數",
    right_join = "以順序最後一個檔案為主體；by 鍵在前序檔案皆唯一且無額外配對列時，列數應等於最後 1 檔列數",
    inner_join = "僅保留所有檔案皆有的 by 鍵；by 鍵各檔皆唯一時，列數應等於各檔鍵的交集筆數",
    full_join = "保留任一方出現過的 by 鍵；by 鍵各檔皆唯一時，列數應等於各檔鍵的聯集筆數"
  )

  if (any_dup) {
    exp_nrow_note <- paste0(
      exp_nrow_note,
      "。（偵測到 by 鍵有重複列，列數可能因一對多而膨脹，以下「預期列數」僅供參考）"
    )
  }

  nrow_ok <- if (any_dup) {
    NA
  } else {
    act_nrow == exp_nrow
  }

  list(
    file_stats = file_stats,
    join_steps = join_steps,
    summary = tibble(
      檢查項目 = c(
        "結果欄位數",
        "預期欄位數（by 欄 + 各檔新增欄）",
        "欄位數是否符合",
        "結果列數",
        "預期列數（依 join 規則）",
        "列數是否符合",
        "各檔鍵交集筆數",
        "各檔鍵聯集筆數",
        "by 鍵是否有重複列"
      ),
      數值或狀態 = c(
        as.character(act_ncol),
        as.character(exp_ncol_final),
        if (ncol_ok) "符合" else "不符合",
        as.character(act_nrow),
        if (any_dup) "（有重複鍵，無單一預期值）" else as.character(exp_nrow),
        if (is.na(nrow_ok)) "需人工判讀（有重複鍵）" else if (nrow_ok) "符合" else "不符合",
        as.character(keys_all),
        as.character(keys_any),
        if (any_dup) "是" else "否"
      )
    ),
    rule_note = exp_nrow_note,
    ncol_ok = ncol_ok,
    nrow_ok = nrow_ok,
    any_dup = any_dup,
    join_method = join_method,
    by_vars = paste(by_vars, collapse = ", ")
  )
}

# Bootswatch Brite 尚未納入 bslib（Bootstrap 5），改以 Lumen 為底，
# 配色參考 KIT 資料跨波次串連平臺：https://kitwaves.hdfs.ntnu.edu.tw/
kit_theme <- bs_theme(
  bootswatch = "lumen",
  primary = "#2d7535",
  secondary = "#616161",
  success = "#2d7535",
  info = "#2778c4",
  warning = "#c6d033",
  danger = "#d9534f",
  base_font = font_google("Roboto"),
  heading_font = font_google("Roboto"),
  "navbar-bg" = "#2d7535",
  "body-bg" = "#f5f5f5"
)

kit_header <- div(
  class = "kit-header mb-4",
  h1("KIT 資料串接工具"),
  p(
    tags$span(class = "kit-subtitle", "臺灣幼兒發展調查資料庫"),
    " · 資料跨波次串連"
  )
)

ui <- page_fluid(
  theme = kit_theme,
  title = "KIT 資料串接工具",
  tags$head(
    tags$style(HTML("
      .kit-header {
        background: linear-gradient(135deg, #2d7535 0%, #3a8f45 100%);
        color: #fff;
        padding: 1.25rem 1.5rem;
        border-radius: 0.5rem;
        border-bottom: 4px solid #c6d033;
        box-shadow: 0 2px 8px rgba(45, 117, 53, 0.2);
      }
      .kit-header h1 {
        margin: 0;
        font-size: 1.75rem;
        font-weight: 500;
      }
      .kit-header p {
        margin: 0.35rem 0 0;
        opacity: 0.92;
        font-size: 0.95rem;
      }
      .kit-subtitle {
        color: #e8f0a0;
        font-weight: 400;
      }
      .nav-tabs .nav-link.active {
        border-bottom: 3px solid #2d7535;
        font-weight: 500;
      }
      .nav-tabs .nav-link {
        color: #424242;
      }
      .table {
        background: #fff;
      }
      .well, pre {
        background-color: #fff;
        border: 1px solid #e0e0e0;
      }
    "))
  ),
  kit_header,

  tabsetPanel(
    # Tab 1：上傳與預覽
    tabPanel(
      "上傳與預覽",
      br(),
      fileInput(
        "files",
        "選擇 CSV 或 SAV 檔案（可多選，單檔上限 500MB）",
        multiple = TRUE,
        accept = c(".csv", ".sav", ".zsav")
      ),
      uiOutput("file_config_ui"),
      hr(),
      h4("第一個檔案預覽"),
      tableOutput("preview_table")
    ),

    # Tab 2：串接設定
    tabPanel(
      "串接設定",
      br(),
      selectizeInput(
        "by_vars",
        "選擇 by 變項（ID 欄，可多選；可從清單選或手動輸入欄位名）",
        choices = NULL,
        multiple = TRUE,
        options = list(create = TRUE)
      ),
      tags$small(
        class = "text-muted",
        "灰色為各檔共有欄位；若欄位名相同但未出現在清單，可直接輸入後按 Enter 新增。"
      ),
      br(), br(),
      selectInput(
        "join_method",
        "串接方法",
        choices = c(
          "left_join" = "left_join",
          "right_join" = "right_join",
          "inner_join" = "inner_join",
          "full_join" = "full_join"
        ),
        selected = "left_join"
      ),
      actionButton("run_join", "執行串接", class = "btn-primary"),
      br(), br(),
      uiOutput("join_status_ui"),
      verbatimTextOutput("join_summary"),
      hr(),
      h4("串接核對"),
      uiOutput("audit_verdict_ui"),
      tags$div(class = "text-muted", style = "margin-bottom: 12px;", textOutput("audit_rule_note")),
      h5("各檔案列欄與 by 鍵"),
      tableOutput("audit_files"),
      h5("逐步合併（欄位數）"),
      tableOutput("audit_steps"),
      h5("最終核對摘要"),
      tableOutput("audit_summary"),
      hr(),
      h4("串接結果預覽"),
      tableOutput("join_preview")
    ),

    # Tab 3：匯出
    tabPanel(
      "匯出",
      br(),
      p("請先在「串接設定」分頁執行串接，再下載結果。"),
      downloadButton("download_csv", "下載 CSV", class = "btn-primary me-2"),
      downloadButton("download_sav", "下載 SAV", class = "btn-outline-primary")
    )
  )
)

server <- function(input, output, session) {
  file_order <- reactiveVal(NULL)
  join_status <- reactiveVal(list(state = "idle", message = ""))
  join_audit <- reactiveVal(NULL)
  joined_data <- reactiveVal(NULL)

  # 讀取所有上傳檔案
  loaded_data <- reactive({
    req(input$files)
    files <- input$files
    map(seq_len(nrow(files)), function(i) {
      read_data_file(files$datapath[i], files$name[i])
    })
  })


  # 動態產生：後綴輸入 + 排序按鈕
  output$file_config_ui <- renderUI({
    req(input$files)
    order <- file_order()
    files <- input$files

    tagList(
      h4("檔案設定（由上到下為串接順序）"),
      lapply(seq_along(order), function(pos) {
        idx <- order[pos]
        fluidRow(
          column(
            4,
            strong(files$name[idx]),
            br(),
            tags$small(paste0("順序：", pos))
          ),
          column(
            4,
            textInput(
              paste0("suffix_", idx),
              "後綴名稱",
              value = paste0("_t", idx),
              width = "100%"
            )
          ),
          column(
            4,
            br(),
            actionButton(paste0("up_", pos), "↑ 上移", class = "btn-sm"),
            actionButton(paste0("down_", pos), "↓ 下移", class = "btn-sm")
          )
        )
      })
    )
  })

  # 上傳新檔案：重置排序並註冊上移 / 下移按鈕
  observeEvent(input$files, {
    req(input$files)
    n <- nrow(input$files)
    file_order(seq_len(n))
    join_status(list(state = "idle", message = ""))
    join_audit(NULL)
    joined_data(NULL)

    for (pos in seq_len(n)) {
      local({
        p <- pos
        up_btn <- paste0("up_", p)
        down_btn <- paste0("down_", p)

        observeEvent(input[[up_btn]], {
          ord <- file_order()
          if (p > 1 && length(ord) >= p) {
            ord[c(p - 1, p)] <- ord[c(p, p - 1)]
            file_order(ord)
          }
        }, ignoreInit = TRUE)

        observeEvent(input[[down_btn]], {
          ord <- file_order()
          if (p < length(ord)) {
            ord[c(p, p + 1)] <- ord[c(p + 1, p)]
            file_order(ord)
          }
        }, ignoreInit = TRUE)
      })
    }
  })

  # 依排序取得各檔後綴
  get_suffixes <- reactive({
    req(input$files)
    order <- file_order()
    setNames(
      map_chr(order, function(idx) {
        val <- input[[paste0("suffix_", idx)]]
        if (is.null(val)) "" else val
      }),
      as.character(order)
    )
  })

  # 各檔共有欄位（建議選項）
  common_cols <- reactive({
    dfs <- loaded_data()
    req(length(dfs) >= 1)
    reduce(map(dfs, names), intersect)
  })

  # 所有檔案欄位聯集（供選擇或手動新增）
  all_cols <- reactive({
    dfs <- loaded_data()
    req(length(dfs) >= 1)
    sort(unique(unlist(map(dfs, names))))
  })

  observeEvent(all_cols(), {
    cols <- all_cols()
    common <- common_cols()
    selected <- input$by_vars
    if (is.null(selected)) selected <- character(0)
    updateSelectizeInput(
      session,
      "by_vars",
      choices = setNames(cols, ifelse(cols %in% common, cols, paste0(cols, " （非共有）"))),
      selected = selected
    )
  })

  # 第一個檔案預覽
  output$preview_table <- renderTable({
    req(input$files)
    order <- file_order()
    req(length(order) >= 1)
    df_for_display(loaded_data()[[order[1]]], 5)
  })

  # 執行串接（進度條持續至檢核與預覽渲染完成）
  observeEvent(input$run_join, {
    join_audit(NULL)
    joined_data(NULL)
    join_status(list(state = "running", message = "串接進行中，請稍候…"))

    prog <- Progress$new(session, min = 0, max = 1)
    prog$set(message = "正在串接資料", value = 0)

    files <- input$files
    by_vars <- input$by_vars

    if (is.null(files) || nrow(files) < 2) {
      msg <- "串接失敗：請至少上傳 2 個檔案。"
      join_status(list(state = "error", message = msg))
      showNotification(msg, type = "error", duration = 8)
      prog$close()
      return()
    }
    if (is.null(by_vars) || length(by_vars) == 0) {
      msg <- "串接失敗：請至少選擇或輸入一個 by 變項。"
      join_status(list(state = "error", message = msg))
      showNotification(msg, type = "error", duration = 8)
      prog$close()
      return()
    }

    tryCatch({
      prog$set(value = 0.05, detail = "讀取與套用後綴…")
      dfs <- loaded_data()
      order <- file_order()
      suffixes <- get_suffixes()
      join_fn <- get(input$join_method, envir = asNamespace("dplyr"))
      file_names <- files$name[order]

      ordered_dfs <- map(order, function(idx) {
        df <- dfs[[idx]]
        suffix <- suffixes[[as.character(idx)]]
        apply_suffix(df, suffix, by_vars)
      })

      n_files <- length(ordered_dfs)
      result <- ordered_dfs[[1]]

      if (n_files >= 2) {
        for (i in 2:n_files) {
          prog$set(
            value = 0.05 + 0.65 * (i - 1) / (n_files - 1),
            detail = paste0(
              "合併第 ", i, " / ", n_files, " 個檔案（",
              file_names[i], "）"
            )
          )
          result <- join_fn(result, ordered_dfs[[i]], by = by_vars)
        }
      }

      prog$set(value = 0.75, detail = "核對列欄數…")
      audit <- build_join_audit(
        ordered_dfs,
        file_names,
        by_vars,
        input$join_method,
        result
      )

      prog$set(value = 0.85, detail = "更新檢核結果…")
      join_audit(audit)
      joined_data(result)

      check_bits <- c(
        if (audit$ncol_ok) "欄位數符合" else "欄位數不符",
        if (is.na(audit$nrow_ok)) "列數需人工確認（by 鍵有重複）"
        else if (audit$nrow_ok) "列數符合"
        else "列數不符"
      )
      success_msg <- paste0(
        "串接成功！共 ", n_files, " 個檔案；",
        "列數：", nrow(result), "；欄位數：", ncol(result),
        "（", paste(check_bits, collapse = "；"), "）"
      )

      prog$set(value = 0.92, detail = "產生預覽…")

      # 等檢核表與預覽表渲染完成後才結束進度條
      session$onFlushed(function() {
        join_status(list(state = "success", message = success_msg))
        showNotification("串接成功", type = "message", duration = 5)
        prog$set(value = 1, detail = "完成")
        prog$close()
      }, once = TRUE)
    }, error = function(e) {
      msg <- paste0("串接失敗：", conditionMessage(e))
      join_status(list(state = "error", message = msg))
      showNotification(msg, type = "error", duration = 10)
      prog$close()
    })
  })

  output$join_status_ui <- renderUI({
    st <- join_status()
    if (is.null(st) || st$state == "idle") return(NULL)

    prefix <- switch(
      st$state,
      running = "【進行中】",
      success = "【成功】",
      error = "【失敗】",
      ""
    )
    cls <- switch(
      st$state,
      running = "alert alert-info",
      success = "alert alert-success",
      error = "alert alert-danger",
      "alert alert-secondary"
    )
    tags$div(
      class = cls,
      role = "alert",
      style = "margin-top: 8px;",
      tags$strong(prefix), " ", st$message
    )
  })

  output$join_summary <- renderText({
    req(joined_data())
    df <- joined_data()
    paste0("列數：", nrow(df), "　欄位數：", ncol(df))
  })

  output$audit_verdict_ui <- renderUI({
    audit <- join_audit()
    req(audit)
    all_ok <- isTRUE(audit$ncol_ok) && (isTRUE(audit$nrow_ok) || is.na(audit$nrow_ok))
    partial <- isTRUE(audit$ncol_ok) && isFALSE(audit$nrow_ok)

    if (all_ok && !audit$any_dup) {
      cls <- "alert alert-success"
      msg <- "核對通過：欄位數與列數皆符合目前串接規則（by 鍵各檔皆唯一）。"
    } else if (all_ok && audit$any_dup) {
      cls <- "alert alert-warning"
      msg <- paste0(
        "欄位數符合規則；列數無法自動比對單一預期值（by 鍵：",
        audit$by_vars,
        " 存在重複列，可能為一對多合併）。請參考下方各檔「重複鍵組數」。"
      )
    } else if (partial) {
      cls <- "alert alert-warning"
      msg <- "欄位數符合，但列數與依 join 方法推算的預期值不一致，請檢查 by 變項或 join 方法是否正確。"
    } else {
      cls <- "alert alert-danger"
      msg <- "核對未通過：欄位數或列數與預期不符，請檢查後綴設定、by 變項與串接順序。"
    }
    tags$div(
      class = cls,
      role = "alert",
      tags$strong("【核對】"), " ", msg,
      tags$br(),
      tags$small(
        "串接方法：", audit$join_method,
        "　by 變項：", audit$by_vars
      )
    )
  })

  output$audit_rule_note <- renderText({
    audit <- join_audit()
    req(audit)
    audit$rule_note
  })

  output$audit_files <- renderTable({
    req(join_audit())
    join_audit()$file_stats
  })

  output$audit_steps <- renderTable({
    audit <- join_audit()
    req(audit)
    if (nrow(audit$join_steps) == 0) {
      return(data.frame(說明 = "僅一個檔案，無逐步合併"))
    }
    audit$join_steps
  })

  output$audit_summary <- renderTable({
    req(join_audit())
    join_audit()$summary
  })

  output$join_preview <- renderTable({
    req(joined_data())
    df_for_display(joined_data(), 10)
  })

  # 匯出 CSV
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("joined_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      req(joined_data())
      write_csv(fix_utf8_df(joined_data()), file)
    }
  )

  # 匯出 SAV
  output$download_sav <- downloadHandler(
    filename = function() {
      paste0("joined_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".sav")
    },
    content = function(file) {
      req(joined_data())
      write_sav(fix_utf8_df(joined_data()), file)
    }
  )
}

shinyApp(ui, server)
