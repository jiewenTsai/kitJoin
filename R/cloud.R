# 雲端版（Posit Connect Cloud）專用的判斷與提醒

#' 是否為雲端版（Posit Connect Cloud）
#'
#' 優先看環境變數 KIT_DEPLOY（"cloud" / "local"，於 Connect Cloud 設定）；
#' 未設定時，以 Posit Connect 預設的 R_CONFIG_ACTIVE = "rsconnect" 判斷。
#' @noRd
kit_is_cloud <- function() {
  deploy <- tolower(Sys.getenv("KIT_DEPLOY"))
  if (nzchar(deploy)) return(identical(deploy, "cloud"))
  identical(Sys.getenv("R_CONFIG_ACTIVE"), "rsconnect")
}

#' 雲端版提醒（標題下方一行）的文字：放進 kitDesign::kit_cloud_banner()
#' @noRd
kit_cloud_banner_text <- function() {
  shiny::tagList(
    shiny::tags$span(
      paste0(
        "此為雲端版：上傳的資料會傳送至位於美國的 Posit 伺服器。",
        "含個資或受 IRB 限制的資料請改用本機版。",
        "上傳前請閱讀頁面最下方的"
      ),
      .noWS = "after"
    ),
    shiny::tags$a(href = "#kit-cloud-terms", "〈雲端版使用須知〉", .noWS = "after"),
    "。"
  )
}

#' 雲端版使用須知（頁面最下方，灰色小字）
#' @noRd
kit_cloud_footer <- function() {
  ext_link <- function(href, text) {
    shiny::tags$a(
      href = href,
      target = "_blank",
      rel = "noopener noreferrer",
      text
    )
  }
  # 標題接句號、內文串成一段，避免 HTML 在標籤與句子間插入空白
  section <- function(title, ...) {
    shiny::tags$p(
      shiny::tags$strong(paste0(title, "。"), .noWS = "after"),
      paste0(...)
    )
  }
  shiny::tags$footer(
    id = "kit-cloud-terms",
    class = "kit-cloud-footer",
    shiny::tags$div(class = "kit-cloud-footer-title", "雲端版使用須知"),
    section(
      "一、資料傳輸與儲存",
      "本雲端版部署於 Posit Connect Cloud，主機位於美國 AWS us-east-2 區域。",
      "您上傳的檔案經加密連線傳送至該伺服器，暫存於伺服器的暫存空間以進行處理，",
      "本工具不另行保存、備份或分享您的資料；下載檔亦僅於您按下下載時即時產生。",
      "各使用者的資料於各自連線中處理，其他使用者無法看到。",
      "平台層級的暫存、日誌與備份由 Posit 管理，請參閱其隱私權政策。"
    ),
    section(
      "二、平台安全性",
      "依 Posit 官方說明：傳輸以 TLS 1.2 以上加密，靜態資料以 AES-256 加密；",
      "平台為多租戶架構，以角色權限（RBAC）隔離各客戶資料；",
      "已完成 SOC 2 Type I 稽核（Type II 預計 2027 年第一季）。",
      "以上為 Posit 之聲明，本網站未另行驗證，亦不構成任何安全保證。"
    ),
    section(
      "三、KIT 使用者注意事項",
      "若資料含可識別個人之資訊（如姓名、身分證字號、出生日期、地址、聯絡方式），",
      "或資料申請時之使用規範、IRB 核准計畫書限制資料存放位置或跨境傳輸，請勿上傳至雲端版。",
      "使用前請自行確認符合 KIT 資料使用規範及《個人資料保護法》相關規定。"
    ),
    section(
      "四、建議使用方式",
      "已去識別化之資料可使用雲端版；含個資或受 IRB 限制之資料請使用本機版（資料不會離開您的電腦）。",
      sprintf("單檔上傳上限為 %d MB；雲端運算資源有限，大型檔案處理時間較長。",
              KIT_UPLOAD_MB)
    ),
    section(
      "五、免責聲明",
      "本工具以現況提供，僅供資料處理輔助之用。使用者應自行確認輸出結果之正確性，",
      "並自行負擔上傳資料之合規責任。對於因使用本工具或第三方平台（Posit Connect Cloud）",
      "所生之資料外洩、遺失或其他損害，本工具開發者不負任何責任。"
    ),
    shiny::tags$p(
      "參考連結：",
      ext_link(
        "https://docs.posit.co/connect-cloud/user/platform/security.html",
        "Posit 安全說明"
      ),
      " · ",
      ext_link(
        paste0(
          "https://docs.posit.co/connect-cloud/whats-new/posts/",
          "2026-07-01-soc-2-type-i-attestation.html"
        ),
        "SOC 2 稽核公告"
      ),
      " · ",
      ext_link("https://trust.posit.co", "Posit Trust Center"),
      " · ",
      ext_link("https://posit.co/about/privacy-policy/", "Posit 隱私權政策"),
      " · ",
      ext_link("https://github.com/jiewenTsai/kitJoin", "kitJoin 本機版（GitHub）")
    )
  )
}
