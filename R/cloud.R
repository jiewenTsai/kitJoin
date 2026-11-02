# 雲端版（Posit Connect Cloud）的文字：橫幅與使用須知（判斷與畫法在 kitDesign：
# kit_is_cloud()、kit_cloud_banner()、kit_cloud_footer()）

#' 雲端版提醒（標題下方一行）的文字：放進 kitDesign::kit_cloud_banner()
#' @noRd
kit_cloud_banner_text <- function() {
  shiny::tagList(
    shiny::tags$span(
      paste0(
        # 原文：此為雲端版：上傳的資料會傳送至位於美國的 Posit 伺服器。
        "\u6b64\u70ba\u96f2\u7aef\u7248\uff1a\u4e0a\u50b3\u7684\u8cc7\u6599\u6703\u50b3\u9001\u81f3\u4f4d\u65bc\u7f8e\u570b\u7684 Posit \u4f3a\u670d\u5668\u3002",
        # 原文：含個資或受 IRB 限制的資料請改用本機版。
        "\u542b\u500b\u8cc7\u6216\u53d7 IRB \u9650\u5236\u7684\u8cc7\u6599\u8acb\u6539\u7528\u672c\u6a5f\u7248\u3002",
        # 原文：上傳前請閱讀頁面最下方的
        "\u4e0a\u50b3\u524d\u8acb\u95b1\u8b80\u9801\u9762\u6700\u4e0b\u65b9\u7684"
      ),
      .noWS = "after"
    ),
    # 原文：〈雲端版使用須知〉
    shiny::tags$a(href = "#kit-cloud-terms", "\u3008\u96f2\u7aef\u7248\u4f7f\u7528\u9808\u77e5\u3009", .noWS = "after"),
    # 原文：。
    "\u3002"
  )
}

# 雲端版使用須知（頁面最下方，灰色小字）：kitJoin 的文字，畫法是 kitDesign::kit_cloud_footer()
kitjoin_cloud_footer <- function() {
  kit_cloud_footer(
    sections = list(
      # 原文：一、資料傳輸與儲存
      "\u4e00\u3001\u8cc7\u6599\u50b3\u8f38\u8207\u5132\u5b58" = c(
        # 原文：本雲端版部署於 Posit Connect Cloud，主機位於美國 AWS us-east-2 區域。
        "\u672c\u96f2\u7aef\u7248\u90e8\u7f72\u65bc Posit Connect Cloud\uff0c\u4e3b\u6a5f\u4f4d\u65bc\u7f8e\u570b AWS us-east-2 \u5340\u57df\u3002",
        # 原文：您上傳的檔案經加密連線傳送至該伺服器，暫存於伺服器的暫存空間以進行處理，
        "\u60a8\u4e0a\u50b3\u7684\u6a94\u6848\u7d93\u52a0\u5bc6\u9023\u7dda\u50b3\u9001\u81f3\u8a72\u4f3a\u670d\u5668\uff0c\u66ab\u5b58\u65bc\u4f3a\u670d\u5668\u7684\u66ab\u5b58\u7a7a\u9593\u4ee5\u9032\u884c\u8655\u7406\uff0c",
        # 原文：本工具不另行保存、備份或分享您的資料；下載檔亦僅於您按下下載時即時產生。
        "\u672c\u5de5\u5177\u4e0d\u53e6\u884c\u4fdd\u5b58\u3001\u5099\u4efd\u6216\u5206\u4eab\u60a8\u7684\u8cc7\u6599\uff1b\u4e0b\u8f09\u6a94\u4ea6\u50c5\u65bc\u60a8\u6309\u4e0b\u4e0b\u8f09\u6642\u5373\u6642\u7522\u751f\u3002",
        # 原文：各使用者的資料於各自連線中處理，其他使用者無法看到。
        "\u5404\u4f7f\u7528\u8005\u7684\u8cc7\u6599\u65bc\u5404\u81ea\u9023\u7dda\u4e2d\u8655\u7406\uff0c\u5176\u4ed6\u4f7f\u7528\u8005\u7121\u6cd5\u770b\u5230\u3002",
        # 原文：平台層級的暫存、日誌與備份由 Posit 管理，請參閱其隱私權政策。
        "\u5e73\u53f0\u5c64\u7d1a\u7684\u66ab\u5b58\u3001\u65e5\u8a8c\u8207\u5099\u4efd\u7531 Posit \u7ba1\u7406\uff0c\u8acb\u53c3\u95b1\u5176\u96b1\u79c1\u6b0a\u653f\u7b56\u3002"
      ),
      # 原文：二、平台安全性
      "\u4e8c\u3001\u5e73\u53f0\u5b89\u5168\u6027" = c(
        # 原文：依 Posit 官方說明：傳輸以 TLS 1.2 以上加密，靜態資料以 AES-256 加密；
        "\u4f9d Posit \u5b98\u65b9\u8aaa\u660e\uff1a\u50b3\u8f38\u4ee5 TLS 1.2 \u4ee5\u4e0a\u52a0\u5bc6\uff0c\u975c\u614b\u8cc7\u6599\u4ee5 AES-256 \u52a0\u5bc6\uff1b",
        # 原文：平台為多租戶架構，以角色權限（RBAC）隔離各客戶資料；
        "\u5e73\u53f0\u70ba\u591a\u79df\u6236\u67b6\u69cb\uff0c\u4ee5\u89d2\u8272\u6b0a\u9650\uff08RBAC\uff09\u9694\u96e2\u5404\u5ba2\u6236\u8cc7\u6599\uff1b",
        # 原文：已完成 SOC 2 Type I 稽核（Type II 預計 2027 年第一季）。
        "\u5df2\u5b8c\u6210 SOC 2 Type I \u7a3d\u6838\uff08Type II \u9810\u8a08 2027 \u5e74\u7b2c\u4e00\u5b63\uff09\u3002",
        # 原文：以上為 Posit 之聲明，本網站未另行驗證，亦不構成任何安全保證。
        "\u4ee5\u4e0a\u70ba Posit \u4e4b\u8072\u660e\uff0c\u672c\u7db2\u7ad9\u672a\u53e6\u884c\u9a57\u8b49\uff0c\u4ea6\u4e0d\u69cb\u6210\u4efb\u4f55\u5b89\u5168\u4fdd\u8b49\u3002"
      ),
      # 原文：三、KIT 使用者注意事項
      "\u4e09\u3001KIT \u4f7f\u7528\u8005\u6ce8\u610f\u4e8b\u9805" = c(
        # 原文：若資料含可識別個人之資訊（如姓名、身分證字號、出生日期、地址、聯絡方式），
        "\u82e5\u8cc7\u6599\u542b\u53ef\u8b58\u5225\u500b\u4eba\u4e4b\u8cc7\u8a0a\uff08\u5982\u59d3\u540d\u3001\u8eab\u5206\u8b49\u5b57\u865f\u3001\u51fa\u751f\u65e5\u671f\u3001\u5730\u5740\u3001\u806f\u7d61\u65b9\u5f0f\uff09\uff0c",
        # 原文：或資料申請時之使用規範、IRB 核准計畫書限制資料存放位置或跨境傳輸，請勿上傳至雲端版。
        "\u6216\u8cc7\u6599\u7533\u8acb\u6642\u4e4b\u4f7f\u7528\u898f\u7bc4\u3001IRB \u6838\u51c6\u8a08\u756b\u66f8\u9650\u5236\u8cc7\u6599\u5b58\u653e\u4f4d\u7f6e\u6216\u8de8\u5883\u50b3\u8f38\uff0c\u8acb\u52ff\u4e0a\u50b3\u81f3\u96f2\u7aef\u7248\u3002",
        # 原文：使用前請自行確認符合 KIT 資料使用規範及《個人資料保護法》相關規定。
        "\u4f7f\u7528\u524d\u8acb\u81ea\u884c\u78ba\u8a8d\u7b26\u5408 KIT \u8cc7\u6599\u4f7f\u7528\u898f\u7bc4\u53ca\u300a\u500b\u4eba\u8cc7\u6599\u4fdd\u8b77\u6cd5\u300b\u76f8\u95dc\u898f\u5b9a\u3002"
      ),
      # 原文：四、建議使用方式
      "\u56db\u3001\u5efa\u8b70\u4f7f\u7528\u65b9\u5f0f" = c(
        # 原文：已去識別化之資料可使用雲端版；含個資或受 IRB 限制之資料請使用本機版（資料不會離開您的電腦）。
        "\u5df2\u53bb\u8b58\u5225\u5316\u4e4b\u8cc7\u6599\u53ef\u4f7f\u7528\u96f2\u7aef\u7248\uff1b\u542b\u500b\u8cc7\u6216\u53d7 IRB \u9650\u5236\u4e4b\u8cc7\u6599\u8acb\u4f7f\u7528\u672c\u6a5f\u7248\uff08\u8cc7\u6599\u4e0d\u6703\u96e2\u958b\u60a8\u7684\u96fb\u8166\uff09\u3002",
        # 原文：單檔上傳上限為 %d MB；雲端運算資源有限，大型檔案處理時間較長。
        sprintf("\u55ae\u6a94\u4e0a\u50b3\u4e0a\u9650\u70ba %d MB\uff1b\u96f2\u7aef\u904b\u7b97\u8cc7\u6e90\u6709\u9650\uff0c\u5927\u578b\u6a94\u6848\u8655\u7406\u6642\u9593\u8f03\u9577\u3002",
                KIT_UPLOAD_MB)
      ),
      # 原文：五、免責聲明
      "\u4e94\u3001\u514d\u8cac\u8072\u660e" = c(
        # 原文：本工具以現況提供，僅供資料處理輔助之用。使用者應自行確認輸出結果之正確性，
        "\u672c\u5de5\u5177\u4ee5\u73fe\u6cc1\u63d0\u4f9b\uff0c\u50c5\u4f9b\u8cc7\u6599\u8655\u7406\u8f14\u52a9\u4e4b\u7528\u3002\u4f7f\u7528\u8005\u61c9\u81ea\u884c\u78ba\u8a8d\u8f38\u51fa\u7d50\u679c\u4e4b\u6b63\u78ba\u6027\uff0c",
        # 原文：並自行負擔上傳資料之合規責任。對於因使用本工具或第三方平台（Posit Connect Cloud）
        "\u4e26\u81ea\u884c\u8ca0\u64d4\u4e0a\u50b3\u8cc7\u6599\u4e4b\u5408\u898f\u8cac\u4efb\u3002\u5c0d\u65bc\u56e0\u4f7f\u7528\u672c\u5de5\u5177\u6216\u7b2c\u4e09\u65b9\u5e73\u53f0\uff08Posit Connect Cloud\uff09",
        # 原文：所生之資料外洩、遺失或其他損害，本工具開發者不負任何責任。
        "\u6240\u751f\u4e4b\u8cc7\u6599\u5916\u6d29\u3001\u907a\u5931\u6216\u5176\u4ed6\u640d\u5bb3\uff0c\u672c\u5de5\u5177\u958b\u767c\u8005\u4e0d\u8ca0\u4efb\u4f55\u8cac\u4efb\u3002"
      )
    ),
    links = KIT_CLOUD_LINKS
  )
}

# 使用須知最後一行的參考連結（連結的字 = 網址）
KIT_CLOUD_LINKS <- c(
  # 原文：Posit 安全說明
  "Posit \u5b89\u5168\u8aaa\u660e" = "https://docs.posit.co/connect-cloud/user/platform/security.html",
  # 原文：SOC 2 稽核公告
  "SOC 2 \u7a3d\u6838\u516c\u544a" = paste0(
    "https://docs.posit.co/connect-cloud/whats-new/posts/",
    "2026-07-01-soc-2-type-i-attestation.html"
  ),
  "Posit Trust Center" = "https://trust.posit.co",
  # 原文：Posit 隱私權政策
  "Posit \u96b1\u79c1\u6b0a\u653f\u7b56" = "https://posit.co/about/privacy-policy/",
  # 原文：kitJoin 本機版（GitHub）
  "kitJoin \u672c\u6a5f\u7248\uff08GitHub\uff09" = "https://github.com/jiewenTsai/kitJoin"
)
