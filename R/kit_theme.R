#' KIT Shiny 主題與版面元件
#'
#' 配色參考 KIT 資料跨波次串連平臺（https://kitwaves.hdfs.ntnu.edu.tw/）。
#' Bootswatch Brite 尚未納入 bslib，改以 Lumen 為底並覆寫 KIT 品牌色。

#' @return A [bslib::bs_theme()] object.
#' @export
kit_theme <- function() {
  bslib::bs_theme(
    bootswatch = "lumen",
    primary = "#2d7535",
    secondary = "#616161",
    success = "#2d7535",
    info = "#2778c4",
    warning = "#c6d033",
    danger = "#d9534f",
    base_font = bslib::font_google("Roboto"),
    heading_font = bslib::font_google("Roboto"),
    "navbar-bg" = "#2d7535",
    "body-bg" = "#f5f5f5"
  )
}

#' KIT 品牌色常數（供自訂 CSS 或圖表使用）
#' @export
kit_colors <- function() {
  list(
    primary = "#2d7535",
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
  shiny::HTML(sprintf("
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
  ", cols$primary, cols$primary_light, cols$accent,
     cols$subtitle, cols$text_muted, cols$border))
}

#' KIT 頁首區塊
#'
#' @param title 主標題
#' @param subtitle 副標題（顯示為淺黃綠色）
#' @param tagline 標語（接在副標題後）
#' @export
kit_header <- function(
    title = "KIT 資料串接工具",
    subtitle = "臺灣幼兒發展調查資料庫",
    tagline = "跨波次資料處理") {
  shiny::div(
    class = "kit-header mb-4",
    shiny::h1(title),
    shiny::p(
      shiny::tags$span(class = "kit-subtitle", subtitle),
      " · ", tagline
    )
  )
}
