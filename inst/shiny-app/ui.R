ui <- bslib::page_fluid(
  theme = kit_theme(),
  title = "KIT 資料串接工具",
  shiny::tags$head(shiny::tags$style(kit_css())),
  kit_header(
    title = "KIT 資料串接工具",
    subtitle = "臺灣幼兒發展調查資料庫",
    tagline = "資料跨波次串連"
  ),

  shiny::tabsetPanel(
    # Tab 1：上傳與預覽
    shiny::tabPanel(
      "上傳與預覽",
      shiny::br(),
      shiny::p(
        class = "text-muted",
        "每個上傳框只接受一個 CSV 或 SAV 檔案（單檔上限 500 MB）。",
        "按「+ 新增上傳框」可增加更多檔案。上傳後可在下方調整串接順序與波次名稱。"
      ),

      # 動態插入的上傳框容器
      shiny::div(id = "file_slots_container"),

      shiny::actionButton(
        "add_file_slot", "+ 新增上傳框",
        class = "btn-outline-secondary btn-sm",
        style = "margin-bottom: 1rem;"
      ),

      shiny::hr(),

      # 已上傳檔案的排序區
      shiny::uiOutput("file_order_ui"),

      shiny::hr(),
      shiny::h4("第一個檔案預覽"),
      shiny::textOutput("preview_meta"),
      shiny::tags$div(
        class = "kit-table-scroll",
        shiny::tableOutput("preview_table")
      )
    ),

    # Tab 2：寬格式串接
    shiny::tabPanel(
      "寬格式串接",
      shiny::br(),
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
      )
    ),

    # Tab 3：長格式疊加
    shiny::tabPanel(
      "長格式疊加",
      shiny::br(),
      shiny::p(
        class = "text-muted",
        "依「上傳與預覽」分頁各檔設定的波次名稱（作為 wave），將各檔垂直疊加；",
        "相同欄名對齊，不同欄名新增（缺值為 NA）。",
        "若同名欄位型別不一致，會自動轉為字元後疊加。"
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
      )
    ),

    # Tab 4：Attrition
    shiny::tabPanel(
      "Attrition",
      shiny::br(),
      shiny::div(
        class = "alert alert-success",
        shiny::tags$strong("【本分頁目標】"),
        "以前期樣本為基準，對照後期樣本，標記每筆資料是否流失。",
        shiny::tags$hr(style = "margin: 0.5rem 0;"),
        shiny::tags$strong("做法："),
        " left join（前期為主體）。兩邊都有的 ID 標為 0（留存），只有前期有的標為 1（流失）。",
        shiny::tags$br(),
        shiny::tags$strong("輸出："),
        " 前期全部欄位 + attrition 欄（0/1），列數等於前期樣本列數。",
        shiny::tags$br(),
        shiny::tags$small(
          class = "text-muted",
          "後期樣本僅用於 ID 比對，不將後期變項併入輸出。"
        )
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
      shiny::actionButton("run_attrition", "產生 Attrition 資料", class = "btn-primary"),
      shiny::br(), shiny::br(),
      shiny::uiOutput("attrition_status_ui"),
      shiny::hr(),
      shiny::h4("Attrition 摘要"),
      shiny::tableOutput("attrition_summary_table"),
      shiny::hr(),
      shiny::h4("結果預覽（前 10 列）"),
      shiny::tags$div(
        class = "kit-table-scroll",
        shiny::tableOutput("attrition_preview")
      ),
      shiny::br(),
      shiny::downloadButton(
        "download_attrition_csv", "下載 Attrition CSV",
        class = "btn-primary"
      ),
      shiny::tags$p(
        class = "text-muted",
        style = "font-size: 0.85rem; margin-top: 0.5rem;",
        "若下載無反應，請改用 Chrome 或檢查瀏覽器是否封鎖下載。"
      )
    ),

    # Tab 5：匯出
    shiny::tabPanel(
      "匯出",
      shiny::br(),
      shiny::p("請先在「寬格式串接」或「長格式疊加」分頁產生結果，再下載。"),
      shiny::downloadButton("download_csv", "下載 CSV", class = "btn-primary me-2"),
      shiny::downloadButton("download_sav", "下載 SAV", class = "btn-outline-primary")
    )
  )
)
