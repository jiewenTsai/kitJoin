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

# 預覽用：欄位過多時只取前 max_cols 欄（優先保留指定欄）
preview_df_cols <- function(df, max_cols = 20L, priority_cols = NULL) {
  cols <- names(df)
  if (length(cols) <= max_cols) return(df)
  if (is.null(priority_cols)) priority_cols <- character(0)
  priority_cols <- intersect(priority_cols, cols)
  keep <- unique(c(priority_cols, setdiff(cols, priority_cols)))
  keep <- keep[seq_len(max_cols)]
  df[, keep, drop = FALSE]
}

# 維度描述：列在前、欄在後
format_dims <- function(nrow, ncol, preview_cols = NULL) {
  base <- sprintf("%d 列 × %d 欄", nrow, ncol)
  if (!is.null(preview_cols)) {
    paste0(base, "，以下僅預覽前 ", preview_cols, " 欄")
  } else {
    base
  }
}

# 長格式疊加前：同名欄位若型別不一致，統一轉成字元以便 bind_rows
col_bind_signature <- function(x) {
  if (inherits(x, "labelled")) {
    return(if (is.numeric(x)) "numeric" else "character")
  }
  if (is.factor(x) || is.character(x)) return("character")
  if (is.logical(x)) return("logical")
  if (is.integer(x) || is.double(x) || is.numeric(x)) return("numeric")
  "character"
}

coerce_for_bind <- function(x) {
  fix_utf8_chr(as.character(x))
}

harmonize_for_bind_rows <- function(dfs) {
  harmonized_cols <- character()
  all_cols <- unique(unlist(map(dfs, names)))
  for (col in all_cols) {
    present <- which(map_lgl(dfs, ~ col %in% names(.x)))
    if (length(present) < 2) next
    sigs <- unique(map_chr(dfs[present], ~ col_bind_signature(.x[[col]])))
    if (length(sigs) <= 1) next
    harmonized_cols <- c(harmonized_cols, col)
    dfs <- map(dfs, function(df) {
      if (!col %in% names(df)) return(df)
      df[[col]] <- coerce_for_bind(df[[col]])
      df
    })
  }
  list(dfs = dfs, harmonized_cols = unique(harmonized_cols))
}

# 長格式疊加：同名欄對齊，不同欄新增，並標記 wave
stack_files_long <- function(dfs, wave_labels) {
  h <- harmonize_for_bind_rows(dfs)
  result <- map2(h$dfs, wave_labels, function(df, w) {
    mutate(df, wave = w, .before = 1)
  }) %>% bind_rows()
  list(result = result, harmonized_cols = h$harmonized_cols)
}

# 長格式疊加後核對
build_stack_audit <- function(ordered_dfs, file_names, wave_labels, result,
                              harmonized_cols = character()) {
  file_stats <- tibble(
    順序 = seq_along(ordered_dfs),
    檔案 = file_names,
    wave = wave_labels,
    列數 = map_int(ordered_dfs, nrow),
    欄位數 = map_int(ordered_dfs, ncol)
  )

  exp_nrow <- sum(file_stats$列數)
  act_nrow <- nrow(result)
  nrow_ok <- exp_nrow == act_nrow

  union_cols <- unique(unlist(map(ordered_dfs, names)))
  exp_ncol <- length(union_cols) + 1L
  act_ncol <- ncol(result)
  ncol_ok <- act_ncol == exp_ncol

  wave_ok <- all(map_lgl(seq_along(wave_labels), function(i) {
    sum(result$wave == wave_labels[i], na.rm = TRUE) == nrow(ordered_dfs[[i]])
  }))

  rule_note <- paste0(
    "垂直疊加後列數應等於各檔列數之和（", exp_nrow, "）；",
    "欄位數應等 union(各檔欄位) + wave（", exp_ncol, "）。",
    if (length(harmonized_cols) > 0) {
      paste0(" 已將 ", length(harmonized_cols), " 個同名欄位統一轉為字元。")
    } else {
      ""
    }
  )

  list(
    file_stats = file_stats,
    harmonized_cols = harmonized_cols,
    summary = tibble(
      檢查項目 = c(
        "結果列數",
        "預期列數（各檔列數之和）",
        "列數是否符合",
        "結果欄位數",
        "預期欄位數（欄位聯集 + wave）",
        "欄位數是否符合",
        "各 wave 列數是否符合",
        "型別調整欄位數"
      ),
      數值或狀態 = c(
        as.character(act_nrow),
        as.character(exp_nrow),
        if (nrow_ok) "符合" else "不符合",
        as.character(act_ncol),
        as.character(exp_ncol),
        if (ncol_ok) "符合" else "不符合",
        if (wave_ok) "符合" else "不符合",
        as.character(length(harmonized_cols))
      )
    ),
    rule_note = rule_note,
    nrow_ok = nrow_ok,
    ncol_ok = ncol_ok,
    wave_ok = wave_ok
  )
}

# 狀態訊息 UI（串接 / 疊加共用）
make_status_ui <- function(st) {
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
}

# 匯出用：將 key 欄位移到最前
reorder_key_first <- function(df, key_cols) {
  key_cols <- intersect(key_cols, names(df))
  if (length(key_cols) == 0) return(df)
  df[, c(key_cols, setdiff(names(df), key_cols)), drop = FALSE]
}

prepare_export <- function(df, mode, by_vars) {
  df <- fix_utf8_df(df)
  if (is.null(by_vars)) by_vars <- character(0)
  key_cols <- if (identical(mode, "long")) {
    unique(c(intersect(by_vars, names(df)), intersect("wave", names(df))))
  } else {
    intersect(by_vars, names(df))
  }
  reorder_key_first(df, key_cols)
}

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

# 寬串接後綴：使用者輸入 t1，實際欄位後綴為 _t1
join_suffix <- function(label) {
  label <- trimws(if (is.null(label)) "" else label)
  if (!nzchar(label)) return("")
  if (startsWith(label, "_")) label else paste0("_", label)
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
    inner_join = keys_all,
    full_join = keys_any
  )

  exp_nrow_note <- switch(
    join_method,
    left_join = "以順序第 1 個檔案為主體；by 鍵在後續檔案皆唯一且無額外配對列時，列數應等於第 1 檔列數",
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
      .kit-table-scroll {
        overflow-x: auto;
        max-width: 100%;
        margin-bottom: 0.5rem;
      }
      .kit-table-scroll table {
        font-size: 0.85rem;
        white-space: nowrap;
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
        "選擇 CSV 或 SAV 檔案上傳。可同時選取多個檔案上傳，單檔上限 500MB。",
        multiple = TRUE,
        accept = c(".csv", ".sav", ".zsav")
      ),
      uiOutput("file_config_ui"),
      hr(),
      h4("第一個檔案預覽"),
      textOutput("preview_meta"),
      tags$div(
        class = "kit-table-scroll",
        tableOutput("preview_table")
      )
    ),

    # Tab 2：寬格式串接
    tabPanel(
      "寬格式串接",
      br(),
      selectizeInput(
        "by_vars",
        "選擇串接 key（by 變項，可多選）",
        choices = NULL,
        multiple = TRUE,
        options = list(create = FALSE)
      ),
      tags$small(
        class = "text-muted",
        "僅顯示所有已上傳檔案皆有的欄位。"
      ),
      br(), br(),
      selectInput(
        "join_method",
        "串接方法",
        choices = c(
          "left_join" = "left_join",
          "inner_join" = "inner_join",
          "full_join" = "full_join"
        ),
        selected = "left_join"
      ),
      tags$small(
        class = "text-muted",
        tags$div("inner join：取得每一波次都有記錄的「全勤樣本」"),
        tags$div("full join：取得任一波次曾經有記錄的「全樣本」"),
        tags$div("left join：取得以第一波次為準的「向後追蹤樣本」")
      ),
      br(),
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
      textOutput("join_preview_meta"),
      tags$div(
        class = "kit-table-scroll",
        tableOutput("join_preview")
      )
    ),

    # Tab 3：長格式疊加
    tabPanel(
      "長格式疊加",
      br(),
      p(
        class = "text-muted",
        "依「上傳與預覽」分頁設定的後綴或波次名稱（作為 wave），將各檔垂直疊加；",
        "相同欄名對齊，不同欄名新增（缺值為 NA）。",
        "若同名欄位型別不一致，會自動轉為字元後疊加。"
      ),
      actionButton("run_stack", "執行疊加", class = "btn-primary"),
      br(), br(),
      uiOutput("stack_status_ui"),
      verbatimTextOutput("stack_summary"),
      hr(),
      h4("疊加核對"),
      uiOutput("stack_audit_verdict_ui"),
      tags$div(
        class = "text-muted",
        style = "margin-bottom: 12px;",
        textOutput("stack_audit_rule_note")
      ),
      h5("各檔案列欄"),
      tableOutput("stack_audit_files"),
      h5("最終核對摘要"),
      tableOutput("stack_audit_summary"),
      hr(),
      h4("疊加結果預覽"),
      textOutput("stack_preview_meta"),
      tags$div(
        class = "kit-table-scroll",
        tableOutput("stack_preview")
      )
    ),

    # Tab 4：匯出
    tabPanel(
      "匯出",
      br(),
      p("請先在「寬格式串接」或「長格式疊加」分頁產生結果，再下載。"),
      downloadButton("download_csv", "下載 CSV", class = "btn-primary me-2"),
      downloadButton("download_sav", "下載 SAV", class = "btn-outline-primary")
    )
  )
)

server <- function(input, output, session) {
  file_order <- reactiveVal(NULL)
  join_status <- reactiveVal(list(state = "idle", message = ""))
  stack_status <- reactiveVal(list(state = "idle", message = ""))
  join_audit <- reactiveVal(NULL)
  stack_audit <- reactiveVal(NULL)
  result_data <- reactiveVal(NULL)
  result_mode <- reactiveVal(NULL)

  reset_results <- function() {
    join_status(list(state = "idle", message = ""))
    stack_status(list(state = "idle", message = ""))
    join_audit(NULL)
    stack_audit(NULL)
    result_data(NULL)
    result_mode(NULL)
  }

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
      tags$small(
        class = "text-muted",
        "後綴或波次名稱：寬串接時自動加底線（輸入 t1 → 欄位後綴 _t1）；長格式疊加時直接使用 t1 作為 wave。"
      ),
      br(),
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
              paste0("label_", idx),
              "後綴或波次名稱",
              value = paste0("t", idx),
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
    reset_results()

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

  # 各檔後綴或波次名稱（寬串接當後綴、長格式當 wave）
  get_file_labels <- reactive({
    req(input$files)
    order <- file_order()
    setNames(
      map_chr(order, function(idx) {
        val <- input[[paste0("label_", idx)]]
        trimws(if (is.null(val)) "" else val)
      }),
      as.character(order)
    )
  })

  # 各檔共有欄位
  common_cols <- reactive({
    dfs <- loaded_data()
    req(length(dfs) >= 1)
    reduce(map(dfs, names), intersect)
  })

  observeEvent(common_cols(), {
    common <- common_cols()
    selected <- input$by_vars
    if (is.null(selected)) selected <- character(0)
    selected <- intersect(selected, common)
    if (length(selected) == 0 && "release_id" %in% common) {
      selected <- "release_id"
    }
    updateSelectizeInput(
      session,
      "by_vars",
      choices = common,
      selected = selected
    )
  }, ignoreNULL = FALSE)

  output$preview_meta <- renderText({
    req(input$files)
    order <- file_order()
    req(length(order) >= 1)
    df <- loaded_data()[[order[1]]]
    n_show <- min(ncol(df), 20L)
    format_dims(nrow(df), ncol(df), n_show)
  })

  # 第一個檔案預覽
  output$preview_table <- renderTable({
    req(input$files)
    order <- file_order()
    req(length(order) >= 1)
    df <- loaded_data()[[order[1]]]
    df <- preview_df_cols(df, priority_cols = input$by_vars)
    df_for_display(df, 5)
  }, striped = TRUE, spacing = "s")

  # 執行串接（進度條持續至檢核與預覽渲染完成）
  observeEvent(input$run_join, {
    stack_audit(NULL)
    result_data(NULL)
    result_mode(NULL)
    join_audit(NULL)
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
    if (length(common_cols()) == 0) {
      msg <- "串接失敗：各檔沒有共同欄位，無法串接。"
      join_status(list(state = "error", message = msg))
      showNotification(msg, type = "error", duration = 8)
      prog$close()
      return()
    }
    if (is.null(by_vars) || length(by_vars) == 0) {
      msg <- "串接失敗：請至少選擇一個串接 key。"
      join_status(list(state = "error", message = msg))
      showNotification(msg, type = "error", duration = 8)
      prog$close()
      return()
    }

    tryCatch({
      prog$set(value = 0.05, detail = "讀取與套用後綴…")
      dfs <- loaded_data()
      order <- file_order()
      suffixes <- get_file_labels()
      join_fn <- get(input$join_method, envir = asNamespace("dplyr"))
      file_names <- files$name[order]

      ordered_dfs <- map(order, function(idx) {
        df <- dfs[[idx]]
        suffix <- join_suffix(suffixes[[as.character(idx)]])
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
      result_data(result)
      result_mode("wide")

      check_bits <- c(
        if (audit$ncol_ok) "欄位數符合" else "欄位數不符",
        if (is.na(audit$nrow_ok)) "列數需人工確認（by 鍵有重複）"
        else if (audit$nrow_ok) "列數符合"
        else "列數不符"
      )
      success_msg <- paste0(
        "串接成功！共 ", n_files, " 個檔案；",
        format_dims(nrow(result), ncol(result)),
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

  output$join_status_ui <- renderUI(make_status_ui(join_status()))

  output$join_summary <- renderText({
    req(result_data(), result_mode() == "wide")
    format_dims(nrow(result_data()), ncol(result_data()))
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

  output$join_preview_meta <- renderText({
    req(result_data(), result_mode() == "wide")
    df <- result_data()
    n_show <- min(ncol(df), 20L)
    format_dims(nrow(df), ncol(df), n_show)
  })

  output$join_preview <- renderTable({
    req(result_data(), result_mode() == "wide")
    df <- preview_df_cols(result_data(), priority_cols = input$by_vars)
    df_for_display(df, 10)
  }, striped = TRUE, spacing = "s")

  observeEvent(input$run_stack, {
    join_audit(NULL)
    stack_audit(NULL)
    result_data(NULL)
    result_mode(NULL)
    stack_status(list(state = "running", message = "疊加進行中，請稍候…"))

    prog <- Progress$new(session, min = 0, max = 1)
    prog$set(message = "正在疊加資料", value = 0)

    files <- input$files
    if (is.null(files) || nrow(files) < 1) {
      msg <- "疊加失敗：請至少上傳 1 個檔案。"
      stack_status(list(state = "error", message = msg))
      showNotification(msg, type = "error", duration = 8)
      prog$close()
      return()
    }

    wave_labels <- get_file_labels()
    if (any(!nzchar(wave_labels))) {
      msg <- "疊加失敗：請為每個檔案填寫後綴或波次名稱。"
      stack_status(list(state = "error", message = msg))
      showNotification(msg, type = "error", duration = 8)
      prog$close()
      return()
    }
    if (any(duplicated(wave_labels))) {
      msg <- "疊加失敗：後綴或波次名稱不可重複。"
      stack_status(list(state = "error", message = msg))
      showNotification(msg, type = "error", duration = 8)
      prog$close()
      return()
    }

    tryCatch({
      prog$set(value = 0.1, detail = "讀取檔案…")
      dfs <- loaded_data()
      order <- file_order()
      file_names <- files$name[order]
      ordered_dfs <- dfs[order]
      n_files <- length(ordered_dfs)

      prog$set(value = 0.25, detail = "檢查欄位型別…")
      stacked <- stack_files_long(ordered_dfs, wave_labels)
      result <- stacked$result

      prog$set(value = 0.65, detail = "核對列欄數…")
      audit <- build_stack_audit(
        ordered_dfs,
        file_names,
        wave_labels,
        result,
        stacked$harmonized_cols
      )

      prog$set(value = 0.85, detail = "更新檢核結果…")
      stack_audit(audit)
      result_data(result)
      result_mode("long")

      check_bits <- c(
        if (audit$nrow_ok) "列數符合" else "列數不符",
        if (audit$ncol_ok) "欄位數符合" else "欄位數不符",
        if (audit$wave_ok) "wave 列數符合" else "wave 列數不符"
      )
      success_msg <- paste0(
        "疊加成功！共 ", n_files, " 個檔案；",
        format_dims(nrow(result), ncol(result)),
        "（", paste(check_bits, collapse = "；"), "）"
      )

      prog$set(value = 0.92, detail = "產生預覽…")
      session$onFlushed(function() {
        stack_status(list(state = "success", message = success_msg))
        showNotification("疊加成功", type = "message", duration = 5)
        prog$set(value = 1, detail = "完成")
        prog$close()
      }, once = TRUE)
    }, error = function(e) {
      msg <- paste0("疊加失敗：", conditionMessage(e))
      stack_status(list(state = "error", message = msg))
      showNotification(msg, type = "error", duration = 10)
      prog$close()
    })
  })

  output$stack_status_ui <- renderUI(make_status_ui(stack_status()))

  output$stack_summary <- renderText({
    req(result_data(), result_mode() == "long")
    format_dims(nrow(result_data()), ncol(result_data()))
  })

  output$stack_audit_verdict_ui <- renderUI({
    audit <- stack_audit()
    req(audit)
    all_ok <- isTRUE(audit$nrow_ok) && isTRUE(audit$ncol_ok) && isTRUE(audit$wave_ok)

    if (all_ok) {
      cls <- "alert alert-success"
      msg <- "核對通過：列數、欄位數與各 wave 列數皆符合疊加規則。"
    } else if (isTRUE(audit$nrow_ok) && isTRUE(audit$ncol_ok)) {
      cls <- "alert alert-warning"
      msg <- "列數與欄位數符合，但部分 wave 列數與來源檔案不一致，請檢查。"
    } else {
      cls <- "alert alert-danger"
      msg <- "核對未通過：列數或欄位數與預期不符，請檢查上傳檔案與波次名稱。"
    }

    tags$div(
      class = cls,
      role = "alert",
      tags$strong("【核對】"), " ", msg,
      if (length(audit$harmonized_cols) > 0) {
        tagList(
          tags$br(),
          tags$small(
            "已統一型別欄位：",
            paste(audit$harmonized_cols, collapse = ", ")
          )
        )
      }
    )
  })

  output$stack_audit_rule_note <- renderText({
    audit <- stack_audit()
    req(audit)
    audit$rule_note
  })

  output$stack_audit_files <- renderTable({
    req(stack_audit())
    stack_audit()$file_stats
  })

  output$stack_audit_summary <- renderTable({
    req(stack_audit())
    stack_audit()$summary
  })

  output$stack_preview_meta <- renderText({
    req(result_data(), result_mode() == "long")
    df <- result_data()
    n_show <- min(ncol(df), 20L)
    format_dims(nrow(df), ncol(df), n_show)
  })

  output$stack_preview <- renderTable({
    req(result_data(), result_mode() == "long")
    df <- preview_df_cols(result_data(), priority_cols = "wave")
    df_for_display(df, 10)
  }, striped = TRUE, spacing = "s")

  export_filename <- function(ext) {
    prefix <- if (identical(result_mode(), "long")) "stacked_" else "joined_"
    paste0(prefix, format(Sys.time(), "%Y%m%d_%H%M%S"), ".", ext)
  }

  # 匯出 CSV
  output$download_csv <- downloadHandler(
    filename = function() export_filename("csv"),
    content = function(file) {
      req(result_data())
      df <- prepare_export(result_data(), result_mode(), input$by_vars)
      write_csv(df, file)
    }
  )

  # 匯出 SAV
  output$download_sav <- downloadHandler(
    filename = function() export_filename("sav"),
    content = function(file) {
      req(result_data())
      df <- prepare_export(result_data(), result_mode(), input$by_vars)
      write_sav(df, file)
    }
  )
}

shinyApp(ui, server)
