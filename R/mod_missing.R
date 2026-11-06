# 遺漏值處理

# 「依數值標籤」轉換前的預覽表最多列幾個欄位
KIT_LABEL_NA_ROWS <- 200L

# 遺漏值處理的頁面：骨架是 kitDesign::kit_run_tab_ui()；下載在重點（output result）裡，一次一個檔案
mod_missing_ui <- function(id) {
  ns <- shiny::NS(id)
  kit_run_tab_ui(
    # 原文：執行轉換
    id, "\u57f7\u884c\u8f49\u63db",
    guide = kit_tab_guide(
      goal = paste0(
        # 原文：把代表遺漏的代碼（例如 7777、9999，或 SPSS 數值標籤寫著「遺漏值」「跳答」的 98、99）
        "\u628a\u4ee3\u8868\u907a\u6f0f\u7684\u4ee3\u78bc\uff08\u4f8b\u5982 7777\u30019999\uff0c\u6216 SPSS \u6578\u503c\u6a19\u7c64\u5beb\u8457\u300c\u907a\u6f0f\u503c\u300d\u300c\u8df3\u7b54\u300d\u7684 98\u300199\uff09",
        # 原文：改成真正的遺漏值（NA），避免它們在分析時被當成數字。這一步是選用的。
        "\u6539\u6210\u771f\u6b63\u7684\u907a\u6f0f\u503c\uff08NA\uff09\uff0c\u907f\u514d\u5b83\u5011\u5728\u5206\u6790\u6642\u88ab\u7576\u6210\u6578\u5b57\u3002\u9019\u4e00\u6b65\u662f\u9078\u7528\u7684\u3002"
      ),
      steps = kit_steps(
        # 原文：選擇判斷方式：用一組代碼套用到所有直欄，或依 SPSS 數值標籤逐直欄判斷
        "\u9078\u64c7\u5224\u65b7\u65b9\u5f0f\uff1a\u7528\u4e00\u7d44\u4ee3\u78bc\u5957\u7528\u5230\u6240\u6709\u76f4\u6b04\uff0c\u6216\u4f9d SPSS \u6578\u503c\u6a19\u7c64\u9010\u76f4\u6b04\u5224\u65b7",
        # 原文：確認代碼（已預填常用的 7777、9999 等）或遺漏值標籤的關鍵字
        "\u78ba\u8a8d\u4ee3\u78bc\uff08\u5df2\u9810\u586b\u5e38\u7528\u7684 7777\u30019999 \u7b49\uff09\u6216\u907a\u6f0f\u503c\u6a19\u7c64\u7684\u95dc\u9375\u5b57",
        # 原文：確認不要處理的直欄（ID 已預先排除）
        "\u78ba\u8a8d\u4e0d\u8981\u8655\u7406\u7684\u76f4\u6b04\uff08ID \u5df2\u9810\u5148\u6392\u9664\uff09",
        # 原文：按「執行轉換」，檢查每個直欄替換了幾格
        "\u6309\u300c\u57f7\u884c\u8f49\u63db\u300d\uff0c\u6aa2\u67e5\u6bcf\u500b\u76f4\u6b04\u66ff\u63db\u4e86\u5e7e\u683c",
        # 原文：只想改遺漏值：在「下載處理後的檔案」下載就完成了（1 個檔案就可以）
        "\u53ea\u60f3\u6539\u907a\u6f0f\u503c\uff1a\u5728\u300c\u4e0b\u8f09\u8655\u7406\u5f8c\u7684\u6a94\u6848\u300d\u4e0b\u8f09\u5c31\u5b8c\u6210\u4e86\uff081 \u500b\u6a94\u6848\u5c31\u53ef\u4ee5\uff09",
        # 原文：要接著做：後面的分頁（串接、疊加、流失樣本、kits 作答檔、長/寬格式轉換）會自動使用處理後的資料
        "\u8981\u63a5\u8457\u505a\uff1a\u5f8c\u9762\u7684\u5206\u9801\uff08\u4e32\u63a5\u3001\u758a\u52a0\u3001\u6d41\u5931\u6a23\u672c\u3001kits \u4f5c\u7b54\u6a94\u3001\u9577/\u5bec\u683c\u5f0f\u8f49\u63db\uff09\u6703\u81ea\u52d5\u4f7f\u7528\u8655\u7406\u5f8c\u7684\u8cc7\u6599"
      ),
      # 原文：每個檔案各自轉換；橫列數與直欄不變，只有遺漏值代碼變成空白（NA）。可以直接下載處理後的檔案。
      output = "\u6bcf\u500b\u6a94\u6848\u5404\u81ea\u8f49\u63db\uff1b\u6a6b\u5217\u6578\u8207\u76f4\u6b04\u4e0d\u8b8a\uff0c\u53ea\u6709\u907a\u6f0f\u503c\u4ee3\u78bc\u8b8a\u6210\u7a7a\u767d\uff08NA\uff09\u3002\u53ef\u4ee5\u76f4\u63a5\u4e0b\u8f09\u8655\u7406\u5f8c\u7684\u6a94\u6848\u3002",
      note = paste0(
        # 原文：轉換後無法再區分遺漏原因（例如 7777 不適用、9999 拒答）；分析需要區分時請保留原檔。
        "\u8f49\u63db\u5f8c\u7121\u6cd5\u518d\u5340\u5206\u907a\u6f0f\u539f\u56e0\uff08\u4f8b\u5982 7777 \u4e0d\u9069\u7528\u30019999 \u62d2\u7b54\uff09\uff1b\u5206\u6790\u9700\u8981\u5340\u5206\u6642\u8acb\u4fdd\u7559\u539f\u6a94\u3002",
        # 原文：SAV 檔中 SPSS 已設定為遺漏的值，讀入時就已經是 NA。
        "SAV \u6a94\u4e2d SPSS \u5df2\u8a2d\u5b9a\u70ba\u907a\u6f0f\u7684\u503c\uff0c\u8b80\u5165\u6642\u5c31\u5df2\u7d93\u662f NA\u3002"
      ),
      technical = paste0(
        # 原文：依代碼：數值直欄（含 SAV 標籤直欄）中等於任一代碼的值改為 NA，SAV 標籤直欄同時移除這些代碼的數值標籤；
        "\u4f9d\u4ee3\u78bc\uff1a\u6578\u503c\u76f4\u6b04\uff08\u542b SAV \u6a19\u7c64\u76f4\u6b04\uff09\u4e2d\u7b49\u65bc\u4efb\u4e00\u4ee3\u78bc\u7684\u503c\u6539\u70ba NA\uff0cSAV \u6a19\u7c64\u76f4\u6b04\u540c\u6642\u79fb\u9664\u9019\u4e9b\u4ee3\u78bc\u7684\u6578\u503c\u6a19\u7c64\uff1b",
        # 原文：勾選「文字直欄也比對」時，文字直欄中完全等於代碼的值也會改為 NA。
        "\u52fe\u9078\u300c\u6587\u5b57\u76f4\u6b04\u4e5f\u6bd4\u5c0d\u300d\u6642\uff0c\u6587\u5b57\u76f4\u6b04\u4e2d\u5b8c\u5168\u7b49\u65bc\u4ee3\u78bc\u7684\u503c\u4e5f\u6703\u6539\u70ba NA\u3002",
        # 原文：依數值標籤：每一個直欄只轉換自己的數值標籤含關鍵字（遺漏、跳答、不適用…）的值，
        "\u4f9d\u6578\u503c\u6a19\u7c64\uff1a\u6bcf\u4e00\u500b\u76f4\u6b04\u53ea\u8f49\u63db\u81ea\u5df1\u7684\u6578\u503c\u6a19\u7c64\u542b\u95dc\u9375\u5b57\uff08\u907a\u6f0f\u3001\u8df3\u7b54\u3001\u4e0d\u9069\u7528\u2026\uff09\u7684\u503c\uff0c",
        # 原文：而且代碼要是負數或大於其他選項的值（例如 1–5 分的 98、99），
        "\u800c\u4e14\u4ee3\u78bc\u8981\u662f\u8ca0\u6578\u6216\u5927\u65bc\u5176\u4ed6\u9078\u9805\u7684\u503c\uff08\u4f8b\u5982 1\u20135 \u5206\u7684 98\u300199\uff09\uff0c",
        # 原文：所以量表中間的「不清楚」或剛好等於 99 的真實數值（例如體重 99 公斤）不會被轉換。
        "\u6240\u4ee5\u91cf\u8868\u4e2d\u9593\u7684\u300c\u4e0d\u6e05\u695a\u300d\u6216\u525b\u597d\u7b49\u65bc 99 \u7684\u771f\u5be6\u6578\u503c\uff08\u4f8b\u5982\u9ad4\u91cd 99 \u516c\u65a4\uff09\u4e0d\u6703\u88ab\u8f49\u63db\u3002"
      ),
      diagram = missing_diagram()
    ),
    shiny::radioButtons(
      # 原文：怎麼判斷遺漏值？
      ns("na_mode"), "\u600e\u9ebc\u5224\u65b7\u907a\u6f0f\u503c\uff1f",
      choices = c(
        # 原文：依代碼清單：同一組代碼套用到所有直欄（例如 7777、9999）
        "\u4f9d\u4ee3\u78bc\u6e05\u55ae\uff1a\u540c\u4e00\u7d44\u4ee3\u78bc\u5957\u7528\u5230\u6240\u6709\u76f4\u6b04\uff08\u4f8b\u5982 7777\u30019999\uff09" = "codes",
        # 原文：依 SPSS 數值標籤：每一個直欄只轉換標籤寫著「遺漏、跳答、不適用…」的值
        "\u4f9d SPSS \u6578\u503c\u6a19\u7c64\uff1a\u6bcf\u4e00\u500b\u76f4\u6b04\u53ea\u8f49\u63db\u6a19\u7c64\u5beb\u8457\u300c\u907a\u6f0f\u3001\u8df3\u7b54\u3001\u4e0d\u9069\u7528\u2026\u300d\u7684\u503c" = "labels"
      ),
      selected = "codes", width = "100%"
    ),
    shiny::uiOutput(ns("mode_note")),
    shiny::conditionalPanel(
      "input.na_mode != 'labels'", ns = ns,
      shiny::textInput(
        # 原文：遺漏值代碼（以逗號分隔）
        ns("codes"), "\u907a\u6f0f\u503c\u4ee3\u78bc\uff08\u4ee5\u9017\u865f\u5206\u9694\uff09",
        value = kit_na_codes_default(), width = "60%"
      ),
      shiny::uiOutput(ns("codes_parsed")),
      shiny::radioButtons(
        # 原文：預設代碼
        ns("preset"), "\u9810\u8a2d\u4ee3\u78bc",
        choices = c(
          # 原文：KIT 資料庫（7777、8888、9995、9996、9999）
          "KIT \u8cc7\u6599\u5eab\uff087777\u30018888\u30019995\u30019996\u30019999\uff09" = "kit",
          # 原文：一般研究資料：自己填（例如 -99、999）
          "\u4e00\u822c\u7814\u7a76\u8cc7\u6599\uff1a\u81ea\u5df1\u586b\uff08\u4f8b\u5982 -99\u3001999\uff09" = "general"
        ),
        selected = "kit", inline = TRUE, width = "100%"
      ),
      shiny::textAreaInput(
        ns("codes_by_col"),
        # 原文：個別直欄自己的代碼（選填）：每行一個「直欄: 代碼」，列在這裡的直欄只用自己的代碼
        "\u500b\u5225\u76f4\u6b04\u81ea\u5df1\u7684\u4ee3\u78bc\uff08\u9078\u586b\uff09\uff1a\u6bcf\u884c\u4e00\u500b\u300c\u76f4\u6b04: \u4ee3\u78bc\u300d\uff0c\u5217\u5728\u9019\u88e1\u7684\u76f4\u6b04\u53ea\u7528\u81ea\u5df1\u7684\u4ee3\u78bc",
        value = "", rows = 3, width = "60%",
        # 原文：age: 999\nq1, q2, q3: 8, 9\nweight:　（冒號後面空白＝這一個直欄不處理）
        placeholder = "age: 999\nq1, q2, q3: 8, 9\nweight:\u3000\uff08\u5192\u865f\u5f8c\u9762\u7a7a\u767d\uff1d\u9019\u4e00\u500b\u76f4\u6b04\u4e0d\u8655\u7406\uff09"
      ),
      shiny::uiOutput(ns("by_col_parsed"))
    ),
    shiny::uiOutput(ns("user_na_ui")),
    shiny::conditionalPanel(
      "input.na_mode == 'labels'", ns = ns,
      shiny::textInput(
        # 原文：數值標籤含這些字的值，視為遺漏值（以逗號分隔）
        ns("keywords"), "\u6578\u503c\u6a19\u7c64\u542b\u9019\u4e9b\u5b57\u7684\u503c\uff0c\u8996\u70ba\u907a\u6f0f\u503c\uff08\u4ee5\u9017\u865f\u5206\u9694\uff09",
        value = kit_na_label_keywords_default(), width = "100%"
      ),
      shiny::tags$small(
        class = "kit-hint",
        # 原文：預設不含「不知道」「不清楚」（有些題是有效的答案）；要轉成 NA 時請自己加上。
        "\u9810\u8a2d\u4e0d\u542b\u300c\u4e0d\u77e5\u9053\u300d\u300c\u4e0d\u6e05\u695a\u300d\uff08\u6709\u4e9b\u984c\u662f\u6709\u6548\u7684\u7b54\u6848\uff09\uff1b\u8981\u8f49\u6210 NA \u6642\u8acb\u81ea\u5df1\u52a0\u4e0a\u3002"
      ),
      shiny::uiOutput(ns("label_found"))
    ),
    shiny::selectizeInput(
      # 原文：不要處理的直欄（例如 ID，可多選）
      ns("exclude_cols"), "\u4e0d\u8981\u8655\u7406\u7684\u76f4\u6b04\uff08\u4f8b\u5982 ID\uff0c\u53ef\u591a\u9078\uff09",
      choices = NULL, multiple = TRUE, options = list(create = FALSE),
      width = "60%"
    ),
    shiny::tags$small(
      class = "kit-hint",
      # 原文：已預先排除名稱像 ID 的直欄（例如 id、w3id、學號），因為 ID 可能剛好等於 7777 或 9999。
      "\u5df2\u9810\u5148\u6392\u9664\u540d\u7a31\u50cf ID \u7684\u76f4\u6b04\uff08\u4f8b\u5982 id\u3001w3id\u3001\u5b78\u865f\uff09\uff0c\u56e0\u70ba ID \u53ef\u80fd\u525b\u597d\u7b49\u65bc 7777 \u6216 9999\u3002"
    ),
    shiny::checkboxInput(
      # 原文：後面的分頁（串接、疊加、流失樣本、kits 作答檔、長/寬格式轉換）使用處理後的資料
      ns("use_downstream"), "\u5f8c\u9762\u7684\u5206\u9801\uff08\u4e32\u63a5\u3001\u758a\u52a0\u3001\u6d41\u5931\u6a23\u672c\u3001kits \u4f5c\u7b54\u6a94\u3001\u9577/\u5bec\u683c\u5f0f\u8f49\u63db\uff09\u4f7f\u7528\u8655\u7406\u5f8c\u7684\u8cc7\u6599",
      value = TRUE, width = "100%"
    ),
    kit_advanced(
      shiny::conditionalPanel(
        "input.na_mode != 'labels'", ns = ns,
        shiny::checkboxInput(
          # 原文：文字直欄也比對（例如 CSV 中被讀成文字的 \"9999\"）
          ns("match_text"), "\u6587\u5b57\u76f4\u6b04\u4e5f\u6bd4\u5c0d\uff08\u4f8b\u5982 CSV \u4e2d\u88ab\u8b80\u6210\u6587\u5b57\u7684 \"9999\"\uff09",
          value = TRUE, width = "100%"
        )
      ),
      shiny::conditionalPanel(
        "input.na_mode == 'labels'", ns = ns,
        # 原文：依 SPSS 數值標籤時沒有進階設定。
        shiny::tags$p(class = "kit-hint", "\u4f9d SPSS \u6578\u503c\u6a19\u7c64\u6642\u6c92\u6709\u9032\u968e\u8a2d\u5b9a\u3002")
      )
    ),
    result = TRUE, downloads = FALSE, next_step = TRUE
  )
}

# 遺漏值處理。files：mod_upload_server()；goto：NULL 或 function(tab)。回傳 list(data, source,
# code_cells, state)：後面的分頁用的資料與這一頁的狀態。按鈕、狀態框、通知在 kit_run_tab_server()；
# 結果過期的規則是這一頁自己的（移除或調整檔案順序時保留結果、記下「要重新執行」給後面的分頁看）
mod_missing_server <- function(id, files, goto = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    if (!is.null(goto)) watch_goto_links(input, goto)

    codes <- shiny::reactive(parse_na_codes(input$codes))
    all_file_cols <- shiny::reactive(unique(unlist(purrr::map(files$data(), names))))
    by_col <- shiny::reactive(parse_na_codes_by_col(input$codes_by_col, all_file_cols()))
    # 資料裡有 SPSS／Stata 使用者自訂遺漏值（上傳時勾了保留）
    user_na_present <- shiny::reactive(has_user_na(files$data()))
    user_na_on <- shiny::reactive(user_na_present() && !identical(input$user_na_to_na, FALSE))

    # 預設代碼：KIT 資料庫或一般研究資料（只換代碼欄的內容，使用者還可以改）
    shiny::observeEvent(input$preset, {
      shiny::updateTextInput(session, "codes", value = kit_na_codes_default(input$preset))
    }, ignoreInit = TRUE)

    output$by_col_parsed <- shiny::renderUI({
      p <- by_col()
      msgs <- c(
        if (length(p$map) > 0) {
          # 原文：個別設定  |  直欄：
          paste0("\u500b\u5225\u8a2d\u5b9a ", kit_num(length(p$map)), " \u76f4\u6b04\uff1a",
                 paste(utils::head(vapply(names(p$map), function(v) {
                   # 原文： →  | 不處理
                   paste0(v, " \u2192 ", if (length(p$map[[v]]) == 0) "\u4e0d\u8655\u7406"
                          # 原文：、
                          else paste(num_to_text(p$map[[v]]), collapse = "\u3001"))
                 # 原文：；
                 }, character(1)), 5), collapse = "\uff1b"),
                 # 原文：…
                 if (length(p$map) > 5) "\u2026" else "")
        }
      )
      warn <- c(
        # 原文：看不懂這幾行（格式是「直欄: 代碼」）： | ；
        if (length(p$invalid) > 0) paste0("\u770b\u4e0d\u61c2\u9019\u5e7e\u884c\uff08\u683c\u5f0f\u662f\u300c\u76f4\u6b04: \u4ee3\u78bc\u300d\uff09\uff1a", paste(p$invalid, collapse = "\uff1b")),
        # 原文：檔案裡沒有這些直欄： | 、
        if (length(p$unknown) > 0) paste0("\u6a94\u6848\u88e1\u6c92\u6709\u9019\u4e9b\u76f4\u6b04\uff1a", paste(p$unknown, collapse = "\u3001"))
      )
      if (length(msgs) == 0 && length(warn) == 0) return(NULL)
      shiny::tagList(
        if (length(msgs) > 0) shiny::tags$small(class = "kit-hint", msgs),
        if (length(warn) > 0) kit_callout(warn = TRUE, paste(warn, collapse = " "))
      )
    })

    output$user_na_ui <- shiny::renderUI({
      if (!user_na_present()) return(NULL)
      shiny::tagList(
        shiny::checkboxInput(
          ns("user_na_to_na"),
          # 原文：SPSS／Stata 檔裡設成「使用者自訂遺漏值」的值（例如 99 = 拒答）也轉成空白（NA）
          "SPSS\uff0fStata \u6a94\u88e1\u8a2d\u6210\u300c\u4f7f\u7528\u8005\u81ea\u8a02\u907a\u6f0f\u503c\u300d\u7684\u503c\uff08\u4f8b\u5982 99 = \u62d2\u7b54\uff09\u4e5f\u8f49\u6210\u7a7a\u767d\uff08NA\uff09",
          value = isTRUE(input$user_na_to_na %||% TRUE), width = "100%"
        ),
        shiny::tags$small(
          class = "kit-hint",
          # 原文：上傳時勾了保留使用者自訂遺漏值，這些值目前還是原來的代碼。不勾的話留著，匯出 SAV 時 SPSS 裡仍是遺漏值。
          "\u4e0a\u50b3\u6642\u52fe\u4e86\u4fdd\u7559\u4f7f\u7528\u8005\u81ea\u8a02\u907a\u6f0f\u503c\uff0c\u9019\u4e9b\u503c\u76ee\u524d\u9084\u662f\u539f\u4f86\u7684\u4ee3\u78bc\u3002\u4e0d\u52fe\u7684\u8a71\u7559\u8457\uff0c\u532f\u51fa SAV \u6642 SPSS \u88e1\u4ecd\u662f\u907a\u6f0f\u503c\u3002"
        )
      )
    })
    keywords <- shiny::reactive(
      parse_na_keywords(input$keywords %||% kit_na_label_keywords_default())
    )
    mode <- shiny::reactive(if (identical(input$na_mode, "labels")) "labels" else "codes")

    # 新的一批檔案：SPSS 數值標籤標的遺漏值遠多於 KIT 代碼時，改用「依數值標籤」（na_mode_suggestion()）
    auto_note <- shiny::reactiveVal(NULL)
    shiny::observeEvent(files$keys(), {
      dfs <- files$data()
      auto_note(NULL)
      if (length(dfs) == 0 || mode() != "codes") return()
      s <- na_mode_suggestion(
        count_code_cells(dfs),
        count_code_cells(dfs, codes = numeric(0), keywords = keywords())
      )
      if (isTRUE(s$switch)) shiny::updateRadioButtons(session, "na_mode", selected = "labels")
      auto_note(s$note)
    })
    output$mode_note <- shiny::renderUI({
      note <- auto_note()
      if (is.null(note)) return(NULL)
      kit_callout(note)
    })

    # 依數值標籤：轉換前先列出每個欄位會轉換哪些代碼
    label_found <- shiny::reactive({
      label_na_table(
        files$data(), files$names(), keywords(),
        exclude = input$exclude_cols %||% character(0)
      )
    })
    output$label_found <- shiny::renderUI({
      if (mode() != "labels" || length(files$keys()) == 0) return(NULL)
      tbl <- label_found()
      if (nrow(tbl) == 0) {
        return(kit_callout(
          warn = TRUE,
          # 原文：沒有找到數值標籤含這些字的值。CSV 檔沒有數值標籤，請改用「依代碼清單」。
          "\u6c92\u6709\u627e\u5230\u6578\u503c\u6a19\u7c64\u542b\u9019\u4e9b\u5b57\u7684\u503c\u3002CSV \u6a94\u6c92\u6709\u6578\u503c\u6a19\u7c64\uff0c\u8acb\u6539\u7528\u300c\u4f9d\u4ee3\u78bc\u6e05\u55ae\u300d\u3002"
        ))
      }
      shown <- utils::head(tbl, KIT_LABEL_NA_ROWS)
      shiny::tags$details(
        class = "kit-example",
        shiny::tags$summary(paste0(
          # 原文：偵測到  |  個直欄有遺漏值標籤，資料中共 
          "\u5075\u6e2c\u5230 ", kit_num(nrow(tbl)), " \u500b\u76f4\u6b04\u6709\u907a\u6f0f\u503c\u6a19\u7c64\uff0c\u8cc7\u6599\u4e2d\u5171 ",
          # 原文：資料中的格數
          kit_num(sum(tbl[["\u8cc7\u6599\u4e2d\u7684\u683c\u6578"]])),
          # 原文： 格會轉成 NA（按這裡看每個直欄）
          " \u683c\u6703\u8f49\u6210 NA\uff08\u6309\u9019\u88e1\u770b\u6bcf\u500b\u76f4\u6b04\uff09"
        )),
        shiny::tags$small(
          class = "kit-hint",
          # 原文：某個直欄不該轉換（例如「不適用」在這題是有效答案）時，把它加到下方「不要處理的直欄」。
          "\u67d0\u500b\u76f4\u6b04\u4e0d\u8a72\u8f49\u63db\uff08\u4f8b\u5982\u300c\u4e0d\u9069\u7528\u300d\u5728\u9019\u984c\u662f\u6709\u6548\u7b54\u6848\uff09\u6642\uff0c\u628a\u5b83\u52a0\u5230\u4e0b\u65b9\u300c\u4e0d\u8981\u8655\u7406\u7684\u76f4\u6b04\u300d\u3002"
        ),
        shiny::tags$div(class = "kit-table-scroll", kit_static_table(shown)),
        if (nrow(tbl) > nrow(shown)) {
          shiny::tags$small(
            class = "kit-hint",
            paste0(
              # 原文：共  |  個直欄，只列出前 
              "\u5171 ", kit_num(nrow(tbl)), " \u500b\u76f4\u6b04\uff0c\u53ea\u5217\u51fa\u524d ", kit_num(nrow(shown)),
              # 原文： 個；轉換後的「各直欄替換格數」會列出全部。
              " \u500b\uff1b\u8f49\u63db\u5f8c\u7684\u300c\u5404\u76f4\u6b04\u66ff\u63db\u683c\u6578\u300d\u6703\u5217\u51fa\u5168\u90e8\u3002"
            )
          )
        }
      )
    })
    # 舊結果失效時記下來：③～⑦ 才能告訴使用者「② 要重新執行」，而不是默默改用原始資料
    stale <- shiny::reactiveVal(FALSE)

    tab <- kit_run_tab_server(
      # 原文：執行轉換
      input, output, session, "\u57f7\u884c\u8f49\u63db",
      # 主要按鈕：還沒有檔案、沒有代碼或關鍵字時停用
      ready = shiny::reactive({
        if (length(files$keys()) == 0) return(kit_need_files(0L, 1L))
        # 原文：輸入至少一個遺漏值標籤的關鍵字
        if (mode() == "labels" && length(keywords()) == 0) return("\u8f38\u5165\u81f3\u5c11\u4e00\u500b\u907a\u6f0f\u503c\u6a19\u7c64\u7684\u95dc\u9375\u5b57")
        # 原文：輸入至少一個遺漏值代碼
        if (mode() == "codes" && length(codes()$codes) == 0 && length(by_col()$map) == 0 &&
              !user_na_on()) return("\u8f38\u5165\u81f3\u5c11\u4e00\u500b\u907a\u6f0f\u503c\u4ee3\u78bc")
        character(0)
      }),
      ready_link = function() {
        if (length(files$keys()) == 0) {
          # 原文：到「檔案上傳」加入檔案 →
          shiny::actionLink(ns("goto_upload"), "\u5230\u300c\u6a94\u6848\u4e0a\u50b3\u300d\u52a0\u5165\u6a94\u6848 \u2192")
        }
      },
      # 設定摘要：主要按鈕上面一行小字
      settings = function() {
        n <- length(files$keys())
        if (n == 0) return(NULL)
        how <- if (mode() == "labels") {
          # 原文：依 SPSS 數值標籤
          "\u4f9d SPSS \u6578\u503c\u6a19\u7c64"
        } else {
          shown <- utils::head(codes()$codes, 4)
          paste0(
            # 原文：依代碼清單（
            "\u4f9d\u4ee3\u78bc\u6e05\u55ae\uff08",
            paste(format(shown, scientific = FALSE, trim = TRUE, drop0trailing = TRUE),
                  # 原文：、
                  collapse = "\u3001"),
            # 原文：… | ）
            if (length(codes()$codes) > 4) "\u2026", "\uff09"
          )
        }
        n_ex <- length(input$exclude_cols %||% character(0))
        c(
          # 原文： 個檔案
          how, paste0(kit_num(n), " \u500b\u6a94\u6848"),
          # 原文：不處理  |  直欄
          if (n_ex > 0) paste0("\u4e0d\u8655\u7406 ", kit_num(n_ex), " \u76f4\u6b04"),
          # 原文：後面的分頁用處理後的資料 | 後面的分頁仍用原始資料
          if (isTRUE(input$use_downstream)) "\u5f8c\u9762\u7684\u5206\u9801\u7528\u8655\u7406\u5f8c\u7684\u8cc7\u6599" else "\u5f8c\u9762\u7684\u5206\u9801\u4ecd\u7528\u539f\u59cb\u8cc7\u6599"
        )
      },
      run = function() {
        stale(FALSE)
        dfs <- files$data()
        # 原文：請先到「檔案上傳」加入檔案。
        if (length(dfs) == 0) kit_abort("\u8acb\u5148\u5230\u300c\u6a94\u6848\u4e0a\u50b3\u300d\u52a0\u5165\u6a94\u6848\u3002")
        exclude <- input$exclude_cols %||% character(0)
        user <- NULL
        if (user_na_on()) {
          user <- purrr::map(dfs, replace_user_na, exclude = exclude)
          dfs <- purrr::map(user, "data")
        }
        converted <- if (mode() == "labels") {
          if (length(keywords()) == 0) {
            # 原文：請至少輸入一個遺漏值標籤的關鍵字，例如 遺漏。
            kit_abort("\u8acb\u81f3\u5c11\u8f38\u5165\u4e00\u500b\u907a\u6f0f\u503c\u6a19\u7c64\u7684\u95dc\u9375\u5b57\uff0c\u4f8b\u5982 \u907a\u6f0f\u3002")
          }
          purrr::map(dfs, replace_na_by_labels, keywords = keywords(), exclude = exclude)
        } else {
          # 原文：請至少輸入一個遺漏值代碼，例如 9999。
          if (length(codes()$codes) == 0 && length(by_col()$map) == 0 && !user_na_on()) kit_abort("\u8acb\u81f3\u5c11\u8f38\u5165\u4e00\u500b\u907a\u6f0f\u503c\u4ee3\u78bc\uff0c\u4f8b\u5982 9999\u3002")
          purrr::map(
            dfs, replace_na_codes,
            codes = codes()$codes,
            exclude = exclude,
            match_text = isTRUE(input$match_text),
            codes_by_col = by_col()$map
          )
        }
        list(
          keys = files$keys(),
          names = files$names(),
          data = purrr::map(converted, "data"),
          detail = purrr::map(converted, "detail"),
          summary = na_summary_table(files$names(), dfs, converted),
          user_na = if (!is.null(user)) purrr::map(user, "detail"),
          settings = current_settings()
        )
      },
      done = function(res) {
        n_cells <- sum(purrr::map_int(res$detail, function(d) {
          # 原文：合計
          if (is.null(d)) 0L else as.integer(sum(d[["\u5408\u8a08"]]))
        }))
        n_user <- sum(purrr::map_int(res$user_na %||% list(), function(d) {
          # 原文：合計
          if (is.null(d)) 0L else as.integer(sum(d[["\u5408\u8a08"]]))
        }))
        kit_done_status(
          # 原文：轉換完成： |  個檔案共替換  |  格為 NA。
          paste0("\u8f49\u63db\u5b8c\u6210\uff1a", kit_num(length(res$data)), " \u500b\u6a94\u6848\u5171\u66ff\u63db ", kit_num(n_cells), " \u683c\u70ba NA\u3002"),
          # 原文：後面的分頁會使用處理後的資料。
          # 原文：另外，SPSS／Stata 使用者自訂遺漏值轉了  |  格（不在上面的格數裡）。
          notes = c(if (n_user > 0) paste0("\u53e6\u5916\uff0cSPSS\uff0fStata \u4f7f\u7528\u8005\u81ea\u8a02\u907a\u6f0f\u503c\u8f49\u4e86 ", kit_num(n_user), " \u683c\uff08\u4e0d\u5728\u4e0a\u9762\u7684\u683c\u6578\u88e1\uff09\u3002"),
                    if (isTRUE(input$use_downstream)) "\u5f8c\u9762\u7684\u5206\u9801\u6703\u4f7f\u7528\u8655\u7406\u5f8c\u7684\u8cc7\u6599\u3002"
          # 原文：後面的分頁仍使用原始資料（上方選項沒有勾選）。
          else "\u5f8c\u9762\u7684\u5206\u9801\u4ecd\u4f7f\u7528\u539f\u59cb\u8cc7\u6599\uff08\u4e0a\u65b9\u9078\u9805\u6c92\u6709\u52fe\u9078\uff09\u3002")
        )
      },
      # 原文：轉換
      error = function(e) kit_error_status(e, "\u8f49\u63db", kit_upload_todo(ns, length(files$keys()))),
      # 重點：各檔案替換摘要，下面是下載（一次一個檔案）
      result_ui = function(res) {
        choices <- stats::setNames(seq_along(res$names), res$names)
        shiny::tagList(
          # 原文：各檔案替換摘要
          shiny::h4("\u5404\u6a94\u6848\u66ff\u63db\u6458\u8981"),
          shiny::tags$div(class = "kit-table-scroll", shiny::tableOutput(ns("summary"))),
          # 原文：下載處理後的檔案
          shiny::h5("\u4e0b\u8f09\u8655\u7406\u5f8c\u7684\u6a94\u6848"),
          shiny::tags$p(
            class = "kit-hint",
            # 原文：一次下載一個檔案。只想改遺漏值的話，下載後就完成了，不用到後面的分頁。
            "\u4e00\u6b21\u4e0b\u8f09\u4e00\u500b\u6a94\u6848\u3002\u53ea\u60f3\u6539\u907a\u6f0f\u503c\u7684\u8a71\uff0c\u4e0b\u8f09\u5f8c\u5c31\u5b8c\u6210\u4e86\uff0c\u4e0d\u7528\u5230\u5f8c\u9762\u7684\u5206\u9801\u3002"
          ),
          # 原文：要下載與檢視的檔案
          shiny::selectInput(ns("view_file"), "\u8981\u4e0b\u8f09\u8207\u6aa2\u8996\u7684\u6a94\u6848", choices = choices),
          shiny::uiOutput(ns("downloads"))
        )
      },
      # 細節：上面選的那個檔案的每欄替換格數與預覽（收起來）
      details_ui = function(res) {
        shiny::tagList(
          kit_detail(
            # 原文：各直欄替換格數（上面選的檔案）
            "\u5404\u76f4\u6b04\u66ff\u63db\u683c\u6578\uff08\u4e0a\u9762\u9078\u7684\u6a94\u6848\uff09",
            shiny::tags$div(class = "kit-table-scroll", shiny::tableOutput(ns("detail")))
          ),
          kit_detail(
            # 原文：轉換後預覽（上面選的檔案）
            "\u8f49\u63db\u5f8c\u9810\u89bd\uff08\u4e0a\u9762\u9078\u7684\u6a94\u6848\uff09",
            shiny::tags$p(class = "kit-hint", shiny::textOutput(ns("preview_meta"), inline = TRUE)),
            shiny::tags$div(class = "kit-table-scroll", shiny::tableOutput(ns("preview")))
          )
        )
      },
      # page_code：同一個函式也交給 app 的流程紀錄（pipeline_log_server()）
      code = page_code <- function() {
        code_missing(
          code_files_from(files),
          codes()$codes,
          input$exclude_cols,
          isTRUE(input$match_text),
          mode = mode(),
          keywords = keywords(),
          codes_by_col = by_col()$map,
          user_na = user_na_on()
        )
      },
      code_note = KIT_CODE_NOTE_JOIN
    )
    result <- tab$result
    status <- tab$status

    output$codes_parsed <- shiny::renderUI({
      p <- codes()
      shiny::tags$div(
        class = "kit-codes-parsed",
        if (length(p$codes) > 0) {
          shiny::tags$small(
            class = "text-muted d-block",
            # 原文：會替換的代碼：
            "\u6703\u66ff\u63db\u7684\u4ee3\u78bc\uff1a",
            paste(
              format(
                p$codes,
                scientific = FALSE,
                trim = TRUE,
                drop0trailing = TRUE
              ),
              # 原文：、
              collapse = "\u3001"
            )
          )
        },
        if (length(p$invalid) > 0) {
          shiny::tags$small(
            class = "text-danger d-block",
            # 原文：這些不是數字，會略過： | 、
            "\u9019\u4e9b\u4e0d\u662f\u6578\u5b57\uff0c\u6703\u7565\u904e\uff1a", paste(p$invalid, collapse = "\u3001")
          )
        }
      )
    })

    # 不處理的欄位：選項為各檔欄位聯集；新出現的欄位若像 ID 則預設選取
    all_cols <- shiny::reactive(unique(unlist(purrr::map(files$data(), names))))
    known_cols <- character(0)
    shiny::observeEvent(all_cols(), {
      cols <- all_cols()
      prev <- input$exclude_cols %||% character(0)
      selected <- union(
        intersect(prev, cols),
        guess_id_cols(setdiff(cols, known_cols))
      )
      known_cols <<- cols
      shiny::updateSelectizeInput(
        session,
        "exclude_cols",
        choices = cols,
        selected = selected
      )
    }, ignoreNULL = FALSE)

    invalidate <- function() {
      if (is.null(result())) return()
      result(NULL)
      stale(TRUE)
      # 原文：執行轉換
      status(kit_stale_status("\u57f7\u884c\u8f49\u63db"))
    }

    # 檔案變動：只是移除或調整順序時，其他檔案的轉換結果仍然有效，直接保留；
    # 加入新檔案時舊結果失效（新檔案還沒轉換）
    shiny::observeEvent(files$data(), {
      res <- result()
      if (is.null(res)) return()
      keys <- files$keys()
      if (length(keys) == 0) {
        result(NULL)
        stale(FALSE)
        status(kit_status())
        return()
      }
      if (!all(keys %in% res$keys)) {
        invalidate()
        return()
      }
      idx <- match(keys, res$keys)
      if (identical(idx, seq_along(res$keys))) return()
      result(list(
        keys = res$keys[idx],
        names = res$names[idx],
        data = res$data[idx],
        detail = res$detail[idx],
        summary = res$summary[idx, , drop = FALSE],
        settings = res$settings
      ))
      status(kit_done_status(paste0(
        # 原文：檔案已移除或調整順序；其他  |  個檔案的轉換結果保留，不用重新執行。
        "\u6a94\u6848\u5df2\u79fb\u9664\u6216\u8abf\u6574\u9806\u5e8f\uff1b\u5176\u4ed6 ", kit_num(length(keys)), " \u500b\u6a94\u6848\u7684\u8f49\u63db\u7d50\u679c\u4fdd\u7559\uff0c\u4e0d\u7528\u91cd\u65b0\u57f7\u884c\u3002"
      )))
    }, ignoreInit = TRUE)

    # 實際影響轉換的設定（另一種判斷方式的輸入框改了不算）
    current_settings <- function() {
      list(
        mode = mode(),
        codes = if (mode() == "codes") codes()$codes,
        match_text = if (mode() == "codes") isTRUE(input$match_text),
        keywords = if (mode() == "labels") keywords(),
        exclude = sort(input$exclude_cols %||% character(0)),
        by_col = if (mode() == "codes") by_col()$map,
        user_na = user_na_on()
      )
    }
    same_settings <- function(a, b) {
      # 排除欄只比對目前檔案裡有的欄：移除檔案時，它的 ID 從清單消失不算改設定
      cols <- all_cols()
      a$exclude <- intersect(a$exclude, cols)
      b$exclude <- intersect(b$exclude, cols)
      identical(a, b)
    }

    # 代碼、判斷方式、關鍵字、排除欄、文字比對改了：舊結果失效
    shiny::observeEvent(
      list(
        input$na_mode, input$codes, input$keywords, input$exclude_cols,
        input$match_text, all_cols(), input$codes_by_col, input$user_na_to_na
      ),
      {
        res <- result()
        if (!is.null(res) && !same_settings(res$settings, current_settings())) invalidate()
      },
      ignoreInit = TRUE, ignoreNULL = FALSE
    )

    # 下一步：有 2 個以上的檔案時到 ③
    output$next_step <- shiny::renderUI({
      if (is.null(result()) || length(files$keys()) < 2) return(NULL)
      kit_next_step(
        # 原文：後面的分頁會用這一頁的結果；改了設定要再按一次「執行轉換」。
        "\u5f8c\u9762\u7684\u5206\u9801\u6703\u7528\u9019\u4e00\u9801\u7684\u7d50\u679c\uff1b\u6539\u4e86\u8a2d\u5b9a\u8981\u518d\u6309\u4e00\u6b21\u300c\u57f7\u884c\u8f49\u63db\u300d\u3002",
        # 原文：下一步：橫向資料串接
        ns("go_next"), "\u4e0b\u4e00\u6b65\uff1a\u6a6b\u5411\u8cc7\u6599\u4e32\u63a5"
      )
    })
    if (!is.null(goto)) shiny::observeEvent(input$go_next, goto("join"))

    output$summary <- shiny::renderTable(
      result()$summary, digits = 0, format.args = list(big.mark = ",")
    )

    view <- shiny::reactive({
      res <- result()
      i <- suppressWarnings(as.integer(input$view_file))
      if (is.null(res) || length(i) == 0 || is.na(i) || i > length(res$data)) {
        return(NULL)
      }
      list(
        data = res$data[[i]],
        detail = res$detail[[i]],
        name = res$names[[i]]
      )
    })

    output$detail <- shiny::renderTable({
      v <- view()
      shiny::req(v)
      # 原文：說明 | 這個檔案沒有符合遺漏值代碼的值
      if (is.null(v$detail)) return(data.frame("\u8aaa\u660e" = "\u9019\u500b\u6a94\u6848\u6c92\u6709\u7b26\u5408\u907a\u6f0f\u503c\u4ee3\u78bc\u7684\u503c"))
      v$detail
    }, striped = TRUE, spacing = "s", digits = 0, format.args = list(big.mark = ","))

    output$preview_meta <- shiny::renderText({
      v <- view()
      shiny::req(v)
      kit_rows_note(nrow(v$data), ncol(v$data), KIT_PREVIEW_ROWS, KIT_PREVIEW_COLS)
    })

    output$preview <- shiny::renderTable({
      v <- view()
      shiny::req(v)
      # 原文：直欄
      replaced <- if (is.null(v$detail)) character(0) else v$detail[["\u76f4\u6b04"]]
      kit_preview_table(v$data, priority_cols = c(input$exclude_cols, replaced))
    }, striped = TRUE, spacing = "s")

    kit_download_server(
      session, output,
      data_fn = function() view()$data,
      filename_fn = function(ext) na_export_name(view()$name, ext),
      # 原文：執行轉換
      run_label = "\u57f7\u884c\u8f49\u63db"
    )

    use_processed <- shiny::reactive({
      res <- result()
      isTRUE(input$use_downstream) && !is.null(res) &&
        identical(res$keys, files$keys())
    })

    list(
      result = tab$result,
      code = page_code,
      data = shiny::reactive(
        if (use_processed()) result()$data else files$data()
      ),
      # "stale"：② 的結果因為設定或檔案改了而清除，還沒重新執行（後面的分頁用原始資料並提醒）
      source = shiny::reactive({
        if (use_processed()) {
          "na"
        } else if (stale() && isTRUE(input$use_downstream)) {
          "stale"
        } else {
          "raw"
        }
      }),
      code_cells = shiny::reactive(count_code_cells(files$data(), keywords = keywords())),
      # 這一頁的狀態（步驟分頁打 ✓ 用）
      state = tab$state
    )
  })
}

# 各檔案替換摘要表
na_summary_table <- function(file_names, dfs, converted) {
  tibble::tibble(
    # 原文：檔案
    "\u6a94\u6848" = file_names,
    # 原文：橫列數
    "\u6a6b\u5217\u6578" = purrr::map_int(dfs, nrow),
    # 原文：直欄數
    "\u76f4\u6b04\u6578" = purrr::map_int(dfs, ncol),
    # 原文：有替換的直欄數
    "\u6709\u66ff\u63db\u7684\u76f4\u6b04\u6578" = purrr::map_int(converted, function(x) {
      if (is.null(x$detail)) 0L else nrow(x$detail)
    }),
    # 原文：替換為NA的格數
    "\u66ff\u63db\u70baNA\u7684\u683c\u6578" = purrr::map_int(converted, function(x) {
      # 原文：合計
      if (is.null(x$detail)) 0L else as.integer(sum(x$detail[["\u5408\u8a08"]]))
    })
  )
}

# 頁首的圖解：遺漏值代碼變成空白
missing_diagram <- function() {
  kit_diagram(
    # 原文：處理前
    before = kit_dg_table(data.frame(id = c(1, 2), q1 = c(4, 9999), q2 = c(99, 3)), "\u8655\u7406\u524d"),
    # 原文：處理後
    after = kit_dg_table(data.frame(id = c(1, 2), q1 = c(4, NA), q2 = c(NA, 3)), "\u8655\u7406\u5f8c"),
    # 原文：遺漏值代碼（例如 99、9999）改成空白（NA）；其他的值不變
    caption = "\u907a\u6f0f\u503c\u4ee3\u78bc\uff08\u4f8b\u5982 99\u30019999\uff09\u6539\u6210\u7a7a\u767d\uff08NA\uff09\uff1b\u5176\u4ed6\u7684\u503c\u4e0d\u8b8a"
  )
}
