ui <- bslib::page_fluid(
  theme = kit_theme(),
  title = "KIT 資料串接工具",
  shiny::tags$head(shiny::tags$style(kit_css())),
  kit_header(
    title = "KIT 資料串接工具",
    subtitle = "臺灣幼兒發展調查資料庫",
    tagline = "跨波次資料處理"
  ),

  shiny::tabsetPanel(

    # ── Tab 1：上傳與預覽 ──────────────────────────────────────────────────────
    shiny::tabPanel(
      "上傳與預覽",
      shiny::br(),
      kit_tab_guide(
        goal = "上傳多個波次的 CSV / SAV 檔案，設定波次名稱與串接順序，並預覽第一個檔案。",
        steps = shiny::tags$ol(
          style = "margin: 0.3rem 0 0 1.2rem; padding: 0;",
          shiny::tags$li("按「+ 新增上傳框」依序為每個波次各新增一個上傳框"),
          shiny::tags$li("每框上傳一個 CSV 或 SAV 檔案"),
          shiny::tags$li("在「後綴或波次名稱」欄填入該波次代號（如 t1、t2）"),
          shiny::tags$li("若需調整順序，使用已上傳清單中的 ↑↓ 按鈕")
        ),
        principle = "每框單檔，支援 CSV / SAV 混用上傳；CSV 自動偵測 UTF-8 / BIG5 / Latin1 編碼。",
        output = "尚未合併；各檔原始資料載入記憶體，供「寬格式串接」、「長格式疊加」及「建立流失樣本」分頁使用。"
      ),

      shiny::div(id = "file_slots_container"),

      shiny::actionButton(
        "add_file_slot", "+ 新增上傳框",
        class = "btn-outline-secondary btn-sm",
        style = "margin-bottom: 1rem;"
      ),

      shiny::hr(),
      shiny::uiOutput("file_order_ui"),
      shiny::hr(),
      shiny::h4("第一個檔案預覽"),
      shiny::textOutput("preview_meta"),
      shiny::tags$div(
        class = "kit-table-scroll",
        shiny::tableOutput("preview_table")
      )
    ),

    # ── Tab 2：寬格式串接 ──────────────────────────────────────────────────────
    shiny::tabPanel(
      "寬格式串接",
      shiny::br(),
      kit_tab_guide(
        goal = "橫向合併多波次資料，每人保持一列，各波變項並排（寬格式）。",
        steps = "選擇串接 key（by 變項）→ 選擇 join 方法 → 執行串接 → 檢視核對結果 → 下載",
        principle = "以共同 ID 欄位做 dplyr join；非 key 欄依波次名稱自動加後綴（輸入 t1 → 欄位後綴 _t1），避免同名衝突。",
        output = "寬格式資料框：列數依 join 方法決定（left = 第一波樣本、inner = 全勤、full = 聯集）；欄 = 各波變項並排。"
      ),
      shiny::selectizeInput(
        "by_vars",
        "選擇串接 key（by 變項，可多選）",
        choices = NULL,
        multiple = TRUE,
        options = list(create = FALSE)
      ),
      shiny::tags$small(
        class = "text-muted",
        "僅顯示所有已上傳檔案皆有的欄位。"
      ),
      shiny::br(), shiny::br(),
      shiny::selectInput(
        "join_method",
        "串接方法",
        choices = c(
          "left_join" = "left_join",
          "inner_join" = "inner_join",
          "full_join" = "full_join"
        ),
        selected = "left_join"
      ),
      shiny::tags$small(
        class = "text-muted",
        shiny::tags$div("inner join：取得每一波次都有記錄的「全勤樣本」"),
        shiny::tags$div("full join：取得任一波次曾經有記錄的「全樣本」"),
        shiny::tags$div("left join：取得以第一波次為準的「向後追蹤樣本」")
      ),
      shiny::br(),
      shiny::actionButton("run_join", "執行串接", class = "btn-primary"),
      shiny::br(), shiny::br(),
      shiny::uiOutput("join_status_ui"),
      shiny::verbatimTextOutput("join_summary"),
      shiny::hr(),
      shiny::h4("串接核對"),
      shiny::uiOutput("audit_verdict_ui"),
      shiny::tags$div(
        class = "text-muted",
        style = "margin-bottom: 12px;",
        shiny::textOutput("audit_rule_note")
      ),
      shiny::h5("各檔案列欄與 by 鍵"),
      shiny::tableOutput("audit_files"),
      shiny::h5("逐步合併（欄位數）"),
      shiny::tableOutput("audit_steps"),
      shiny::h5("最終核對摘要"),
      shiny::tableOutput("audit_summary"),
      shiny::hr(),
      shiny::h4("串接結果預覽"),
      shiny::textOutput("join_preview_meta"),
      shiny::tags$div(
        class = "kit-table-scroll",
        shiny::tableOutput("join_preview")
      ),
      shiny::hr(),
      shiny::h4("匯出"),
      shiny::downloadButton("download_join_csv", "下載 CSV", class = "btn-primary me-2"),
      shiny::downloadButton("download_join_sav", "下載 SAV", class = "btn-outline-primary"),
      shiny::tags$p(
        class = "text-muted",
        style = "font-size: 0.85rem; margin-top: 0.5rem;",
        "請先執行串接產生結果。若下載無反應，請改用 Chrome 或檢查瀏覽器是否封鎖下載。"
      )
    ),

    # ── Tab 3：長格式疊加 ──────────────────────────────────────────────────────
    shiny::tabPanel(
      "長格式疊加",
      shiny::br(),
      kit_tab_guide(
        goal = "縱向疊加多波次資料，每波每人各佔一列，並以 wave 欄標記所屬波次（長格式）。",
        steps = "於「上傳與預覽」分頁為各檔設定波次名稱 → 執行疊加 → 檢視核對結果 → 下載",
        principle = paste0(
          "以 dplyr::bind_rows 垂直合併；同名欄直接對齊，",
          "不同欄新增並填 NA；型別不一致的同名欄自動轉為字元。"
        ),
        output = "長格式資料框：列數 = 各檔列數之和；欄 = 各檔欄位聯集 + wave（波次標籤）。"
      ),
      shiny::actionButton("run_stack", "執行疊加", class = "btn-primary"),
      shiny::br(), shiny::br(),
      shiny::uiOutput("stack_status_ui"),
      shiny::verbatimTextOutput("stack_summary"),
      shiny::hr(),
      shiny::h4("疊加核對"),
      shiny::uiOutput("stack_audit_verdict_ui"),
      shiny::tags$div(
        class = "text-muted",
        style = "margin-bottom: 12px;",
        shiny::textOutput("stack_audit_rule_note")
      ),
      shiny::h5("各檔案列欄"),
      shiny::tableOutput("stack_audit_files"),
      shiny::h5("最終核對摘要"),
      shiny::tableOutput("stack_audit_summary"),
      shiny::hr(),
      shiny::h4("疊加結果預覽"),
      shiny::textOutput("stack_preview_meta"),
      shiny::tags$div(
        class = "kit-table-scroll",
        shiny::tableOutput("stack_preview")
      ),
      shiny::hr(),
      shiny::h4("匯出"),
      shiny::downloadButton("download_stack_csv", "下載 CSV", class = "btn-primary me-2"),
      shiny::downloadButton("download_stack_sav", "下載 SAV", class = "btn-outline-primary"),
      shiny::tags$p(
        class = "text-muted",
        style = "font-size: 0.85rem; margin-top: 0.5rem;",
        "請先執行疊加產生結果。若下載無反應，請改用 Chrome 或檢查瀏覽器是否封鎖下載。"
      )
    ),

    # ── Tab 4：建立流失樣本 ────────────────────────────────────────────────────
    shiny::tabPanel(
      "建立流失樣本",
      shiny::br(),
      kit_tab_guide(
        goal = "以前期樣本為基準，比對後期樣本 ID，標記每筆資料是否流失。",
        steps = "選擇前期樣本（基準）→ 選擇後期樣本（比對）→ 選擇 ID 欄位 → 執行 → 下載",
        principle = paste0(
          "left join（前期為主體）：後期僅取 ID 欄位並 distinct，",
          "比對後以 .kit_matched 標記是否命中，再轉為 attrition 欄。"
        ),
        output = "前期全部欄位 + attrition 欄（0 = 留存，1 = 流失）；列數等於前期樣本列數。",
        note = "後期樣本僅用於 ID 比對，後期的其他變項不會出現在輸出中。"
      ),
      shiny::fluidRow(
        shiny::column(
          5,
          shiny::selectInput(
            "attrition_baseline",
            "前期樣本（基準）",
            choices = NULL
          )
        ),
        shiny::column(
          5,
          shiny::selectInput(
            "attrition_followup",
            "後期樣本（比對）",
            choices = NULL
          )
        )
      ),
      shiny::selectizeInput(
        "attrition_id_vars",
        "ID 欄位（用於比對，可多選）",
        choices = NULL,
        multiple = TRUE,
        options = list(create = FALSE),
        width = "60%"
      ),
      shiny::tags$small(
        class = "text-muted",
        "僅顯示前期與後期樣本共有的欄位。"
      ),
      shiny::br(), shiny::br(),
      shiny::actionButton("run_attrition", "執行建立流失樣本", class = "btn-primary"),
      shiny::br(), shiny::br(),
      shiny::uiOutput("attrition_status_ui"),
      shiny::hr(),
      shiny::h4("流失樣本摘要"),
      shiny::tableOutput("attrition_summary_table"),
      shiny::hr(),
      shiny::h4("結果預覽（前 10 列）"),
      shiny::tags$div(
        class = "kit-table-scroll",
        shiny::tableOutput("attrition_preview")
      ),
      shiny::br(),
      shiny::downloadButton(
        "download_attrition_csv", "下載流失樣本 CSV",
        class = "btn-primary"
      ),
      shiny::tags$p(
        class = "text-muted",
        style = "font-size: 0.85rem; margin-top: 0.5rem;",
        "若下載無反應，請改用 Chrome 或檢查瀏覽器是否封鎖下載。"
      )
    )
  )
)
