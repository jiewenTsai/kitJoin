#' KIT Shiny 主題與版面元件
#'
#' 配色參考 KIT 資料跨波次串連平臺（https://kitwaves.hdfs.ntnu.edu.tw/）。
#' 以 Bootswatch Brite 為底（bslib 0.11.0 起才有），套 KIT 的顏色與 neo-brutalism 樣式
#' （kit_css_brite()，在 R/kit_brite.R；kitJoin 與 kitsUI 共用同一份）。

#' @return A [bslib::bs_theme()] object.
#' @export
kit_theme <- function() {
  # CJK 字型切成上百個 unicode-range 片段，local = FALSE 改由瀏覽器向
  # Google CDN 按需下載，避免伺服器啟動時抓下全部字檔；
  # 未指定 wght 時只提供 400，需列出 CSS 用到的字重
  kit_font <- bslib::font_collection(
    bslib::font_google(
      "Chiron GoRound TC", local = FALSE, wght = c(400, 500, 700, 900)
    ),
    "sans-serif"
  )
  # 程式碼與等寬字：JetBrains Mono（只有拉丁字母；中文註解退回 app 的中文字型）
  code_font <- bslib::font_collection(
    bslib::font_google("JetBrains Mono", local = FALSE, wght = c(400, 700)),
    "Chiron GoRound TC", "Menlo", "Consolas", "monospace"
  )

  bslib::bs_theme(
    version = 5,
    bootswatch = "brite",
    primary = "#006835",
    secondary = "#616161",
    success = "#006835",
    info = "#2778c4",
    warning = "#d9730d",
    danger = "#d9534f",
    base_font = kit_font,
    # 全文統一用圓體；標題靠字重（700）和內文分開
    heading_font = kit_font,
    code_font = code_font,
    "navbar-bg" = "#006835",
    "body-bg" = "#faf7f0",
    "headings-font-weight" = 700,
    # neo-brutalism：2px 近黑線條（Brite 預設 2px 純黑）
    "border-width" = "2px",
    "border-color" = "#141414",
    # 連結用 KIT 綠（萊姆色當文字太淡）
    "link-color" = "#006835",
    "link-hover-color" = "#004d27",
    # 內文 1rem（Brite 預設 0.875rem）；html 的基準字級在 kit_css_brite() 設成 17px
    "font-size-base" = "1rem"
  )
}

#' 讀取 DESCRIPTION 的版本與日期
#'
#' 雲端版、本機 run.bat、套件 run_kitjoin() 都以 DESCRIPTION 的
#' Version / Date 為準，三者顯示一致（不看檔案修改時間，因為解壓、
#' 安裝、部署都會改變 mtime）。每次更新請同步遞增 Version 並改 Date。
#' @noRd
kit_app_description <- function() {
  if ("kitJoin" %in% loadedNamespaces()) {
    desc <- utils::packageDescription("kitJoin")
    return(list(version = desc$Version, date = desc$Date))
  }
  # 雲端版與 run.bat：工作目錄為專案根目錄
  if (!file.exists("DESCRIPTION")) return(list(version = NA, date = NA))
  desc <- read.dcf("DESCRIPTION", fields = c("Version", "Date"))
  list(version = desc[1, "Version"], date = desc[1, "Date"])
}

#' KIT 品牌色常數（供自訂 CSS 或圖表使用）
#' @export
kit_colors <- function() {
  list(
    primary = "#006835",
    primary_light = "#3a8f45",
    accent = "#c6d033",
    subtitle = "#e8f0a0",
    body_bg = "#f5f5f5",
    text_muted = "#424242",
    border = "#e0e0e0",
    info = "#2778c4",
    danger = "#d9534f"
  )
}

#' KIT App 共用 CSS
#' @export
kit_css <- function() {
  cols <- kit_colors()
  shiny::HTML(paste0(sprintf("
    .kit-header {
      background: linear-gradient(135deg, %1$s 0%%, %2$s 100%%);
      color: #fff;
      padding: 1.25rem 1.5rem;
      border-radius: 0.5rem;
      border-bottom: 4px solid %3$s;
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
      color: %4$s;
      font-weight: 400;
    }
    .nav-tabs .nav-link.active {
      border-bottom: 3px solid %1$s;
      font-weight: 500;
    }
    .nav-tabs .nav-link {
      color: %5$s;
    }
    .table {
      background: #fff;
    }
    .well, pre {
      background-color: #fff;
      border: 1px solid %6$s;
    }
    .kit-table-scroll {
      overflow-x: auto;
      max-width: 100%%;
      margin-bottom: 0.5rem;
    }
    .kit-table-scroll table {
      font-size: 0.85rem;
      white-space: nowrap;
    }
    .kit-table-tall {
      max-height: 26rem;
      overflow-y: auto;
    }
    .kit-table-tall thead th {
      position: sticky;
      top: 0;
      background: #fff;
    }
    .kit-code-head {
      display: flex;
      justify-content: space-between;
      align-items: center;
      margin-bottom: 0.3rem;
    }
    .alert.kit-cloud-banner {
      margin-bottom: 1rem;
      padding: 0.5rem 0.9rem;
      font-size: 0.9rem;
      background: #fff8e1;
      border: 1px solid #e6c65c;
      border-left: 4px solid #b38600;
      color: #3d2c00;
    }
    .alert.kit-cloud-banner a {
      color: #3d2c00;
    }
    .kit-pandoc-notice {
      margin-bottom: 1rem;
    }
    .kit-cloud-footer {
      margin: 2.5rem 0 1.5rem;
      padding-top: 1rem;
      border-top: 1px solid %6$s;
      font-size: 13px;
      line-height: 1.6;
      color: #4f4f4f;
    }
    .kit-cloud-footer p {
      margin: 0 0 0.45rem;
    }
    .kit-cloud-footer strong,
    .kit-cloud-footer-title {
      font-weight: 500;
      color: %5$s;
    }
    .kit-cloud-footer-title {
      margin-bottom: 0.4rem;
    }
    .kit-cloud-footer a {
      color: inherit;
      text-decoration: underline;
    }
    .kit-app-meta {
      margin-top: 0.35rem;
      font-size: 0.72rem;
      color: rgba(255, 255, 255, 0.65);
      line-height: 1.2;
      font-weight: 400;
      letter-spacing: 0.02em;
    }
    .kit-guide {
      background: #f1f7ee;
      border-left: 4px solid %1$s;
      border-radius: 0.375rem;
      padding: 0.9rem 1.1rem;
      margin: 1rem 0;
      color: #263238;
    }
    .kit-guide p {
      margin: 0 0 0.4rem;
    }
    .kit-guide-note {
      font-size: 0.85rem;
      color: #455a64;
    }
    .kit-guide details summary,
    .kit-example summary,
    .kit-status details summary {
      cursor: pointer;
      color: %1$s;
      font-size: 0.85rem;
    }
    /* 可收合的一整段（標題和 h5 一樣大，預設收起來） */
    .kit-fold {
      margin: 0.8rem 0;
    }
    .kit-fold > summary {
      cursor: pointer;
      font-size: 1.25rem;
      font-weight: 500;
      margin-bottom: 0.5rem;
    }
    /* 收合的設定框（④ 通過步驟）：白底框，標題和內文一樣大 */
    .kit-fold-box {
      background: #fff;
      border: 1px solid %6$s;
      border-radius: 0.375rem;
      padding: 0.6rem 1.1rem;
      margin-bottom: 1.2rem;
    }
    .kit-fold-box > summary {
      cursor: pointer;
      font-weight: 500;
    }
    .kit-fold-box[open] > summary {
      margin-bottom: 0.6rem;
    }
    .kit-fold-box details > summary {
      cursor: pointer;
      color: %1$s;
      font-size: 0.9rem;
      margin-bottom: 0.5rem;
    }
    .kit-steps-pick {
      display: flex;
      flex-wrap: wrap;
      align-items: flex-end;
      gap: 0 1rem;
    }
    .kit-steps-pick .form-group,
    .kit-steps-pick .btn {
      margin-bottom: 0.9rem;
    }
    .kit-steps-table {
      --bs-table-bg: transparent;
      width: auto;
      font-size: 0.9rem;
      margin-bottom: 0.6rem;
    }
    .kit-steps-table td,
    .kit-steps-table th {
      vertical-align: middle;
      padding-right: 1.2rem;
    }
    .kit-steps {
      margin: 0.2rem 0 0.4rem 1.2rem;
      padding: 0;
    }
    .kit-hint {
      display: block;
      color: #546e7a;
      font-size: 0.85rem;
      margin: -0.4rem 0 0.9rem;
    }
    .kit-downloads .kit-hint {
      margin: 0.4rem 0 0;
    }
    .kit-hint.kit-tab-caption {
      margin: 0.7rem 0 0.4rem;
    }
    .kit-example {
      margin: -0.3rem 0 1rem;
    }
    .kit-example-table {
      max-width: 44rem;
      font-size: 0.85rem;
      margin-top: 0.4rem;
    }
    .kit-status {
      margin-top: 0.9rem;
    }
    .kit-status ul {
      margin: 0.4rem 0 0;
    }
    .kit-status-notes {
      margin-top: 0.4rem;
      font-size: 0.85rem;
    }
    .kit-downloads {
      margin: 0.9rem 0 1.2rem;
    }
    .kit-summary {
      background: #fff;
      border: 1px solid %6$s;
      border-radius: 0.375rem;
      padding: 0.8rem 1.1rem 0.3rem;
      margin-bottom: 1.2rem;
    }
    .kit-summary ul {
      margin: 0.4rem 0 0.5rem;
    }
    .kit-source-note {
      color: #455a64;
      font-size: 0.9rem;
    }
    .kit-callout {
      background: #eef5fc;
      border: 1px solid #c9dcf0;
      border-left: 4px solid #2778c4;
      border-radius: 0.375rem;
      color: #1f2d3a;
      padding: 0.8rem 1.1rem;
      margin: 0.8rem 0 1rem;
    }
    .kit-warn-text {
      color: #7a3a00;
      font-weight: 600;
    }
    .kit-callout-warn,
    .kit-callout-warning {
      background: #fff1e0;
      border-color: #f3c58f;
      border-left-color: #d9730d;
      color: #5c2b00;
    }
    .kit-callout-warn .kit-warn-text {
      color: #5c2b00;
    }
    .kit-columns {
      font-size: 0.8rem;
    }
    .kit-picks .kit-pick-row {
      display: flex;
      align-items: flex-start;
      gap: 0.6rem;
      margin-bottom: 0.35rem;
    }
    .kit-picks .kit-pick-cohort {
      font-weight: 600;
      flex: 0 0 2.8rem;
    }
    .kit-picks .kit-pick-boxes {
      display: flex;
      flex-wrap: wrap;
      gap: 0.15rem 0.9rem;
    }
    .kit-picks label.kit-pick {
      display: inline-flex;
      align-items: center;
      gap: 0.25rem;
      margin: 0;
      font-weight: 400;
      cursor: pointer;
    }
    .kit-callout ul {
      margin: 0.35rem 0 0;
    }
    .kit-callout a,
    .alert a {
      color: inherit;
      font-weight: 600;
      text-decoration: underline;
    }
    .kit-callout a {
      color: %1$s;
    }
    /* 連到 kitJoin 等外部網站的按鈕：維持按鈕的顏色，不加底線、不轉大寫 */
    a.btn.kit-ext-btn,
    a.btn.kit-ext-btn:hover {
      color: var(--bs-btn-color);
      text-decoration: none;
      text-transform: none;
    }
    a.btn.kit-ext-btn:hover {
      color: var(--bs-btn-hover-color);
    }
    .kit-ext-btn .fa {
      margin-right: 0.3rem;
    }
    .kit-file-table {
      background: #fff;
      margin-bottom: 0.4rem;
    }
    .kit-file-table td {
      vertical-align: middle;
    }
    .kit-file-table .form-group {
      margin-bottom: 0;
    }
    .kit-row-buttons .btn {
      margin-right: 0.25rem;
    }
    .kit-audit {
      margin-top: 1.2rem;
    }
    .kit-codes-parsed {
      margin: -0.5rem 0 1rem;
    }
  ", cols$primary, cols$primary_light, cols$accent,
     cols$subtitle, cols$text_muted, cols$border), kit_css_guide(cols$primary), kit_css_brite()))
}

#' 分頁說明框收合的樣式（kit_css() 的 sprintf() 格式字串有 8192 字元的上限，所以另外接上）
#' @noRd
kit_css_guide <- function(primary) {
  sprintf("
    /* 分頁說明框預設收起：只有一句「這一頁做什麼」，旁邊「看步驟 ▸」 */
    .kit-guide-page > summary {
      cursor: pointer;
      list-style: none;
    }
    .kit-guide-page > summary::-webkit-details-marker {
      display: none;
    }
    .kit-guide-more::before {
      content: '看步驟 ▸';
      margin-left: 0.6rem;
      font-size: 0.85rem;
      color: %s;
      text-decoration: underline;
      white-space: nowrap;
    }
    .kit-guide-page[open] .kit-guide-more::before {
      content: '收起 ▴';
    }
    .kit-guide-body {
      margin-top: 0.6rem;
    }
  ", primary)
}

#' 頁首版本與最後修改日期
#' @export
kit_app_meta <- function() {
  meta <- kit_app_description()
  shiny::div(
    class = "kit-app-meta",
    paste(c(
      if (!is.null(meta$version) && !is.na(meta$version)) {
        paste0("v", meta$version)
      },
      if (!is.null(meta$date) && !is.na(meta$date)) meta$date
    ), collapse = " · ")
  )
}

#' KIT 頁首區塊
#'
#' @param title 主標題
#' @param subtitle 副標題（顯示為淺黃綠色）
#' @param tagline 標語（接在副標題後）
#' @param meta 版本資訊（預設為 [kit_app_meta()]；傳 NULL 不顯示）
#' @export
kit_header <- function(
    title = "KIT 資料串接工具",
    subtitle = "臺灣幼兒發展調查資料庫",
    tagline = "跨波次資料處理",
    meta = kit_app_meta()) {
  shiny::div(
    class = "kit-header mb-4",
    # 分頁圖示（favicon）；Shiny 會把 tags$head() 搬到 <head>
    shiny::tags$head(shiny::tags$link(rel = "icon", type = "image/png", href = kit_logo_uri(64))),
    shiny::div(
      style = "display: flex; align-items: center; gap: 1.1rem;",
      shiny::tags$img(src = kit_logo_uri(192, "logo"), alt = "KIT 標誌", class = "kit-header-logo"),
      shiny::div(
        shiny::h1(title),
        shiny::p(
          shiny::tags$span(class = "kit-subtitle", subtitle),
          " · ", tagline
        ),
        meta
      )
    )
  )
}

#' KIT 標誌：data URI，直接放進頁面（不用另外設定靜態檔案的路徑）
#' @param size 像素（透明底 PNG，在 inst/logo/）：豆子 64、96、256；完整標誌 128、192（高度）
#' @param kind "mark" 只有豆子（分頁圖示）；"logo" 豆子＋下面的 KIT 字樣（頁首）
#' @noRd
kit_logo_uri <- function(size = 96, kind = c("mark", "logo")) {
  kind <- match.arg(kind)
  file <- paste0("kit-", kind, "-", size, ".png")
  pkg <- utils::packageName()
  # 安裝成套件時在 system.file()；用 runApp(資料夾) 跑時（本機、雲端版）在 inst/logo/
  candidates <- c(if (length(pkg) == 1) system.file("logo", file, package = pkg),
                  file.path("inst", "logo", file))
  path <- candidates[nzchar(candidates) & file.exists(candidates)][1]
  if (is.na(path)) return("")
  paste0("data:image/png;base64,", base64enc::base64encode(path))
}
