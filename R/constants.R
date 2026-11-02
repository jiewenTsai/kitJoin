# 全 app 共用的常數

# 預覽表最多顯示幾欄、幾列
KIT_PREVIEW_COLS <- 20L
KIT_PREVIEW_ROWS <- 10L
KIT_RESHAPE_PREVIEW_ROWS <- 20L   # 長/寬格式轉換

# 單檔上傳上限（MB）
KIT_UPLOAD_MB <- 500

# kitsUI 雲端版（⑥ 的作答檔、CSV.GZ 可以直接上傳）
KITSUI_URL <- "https://jiewentsai-kitsui.share.connect.posit.cloud/"

# 橫向資料串接方法：值為 dplyr 函式名稱，名稱為給使用者看的中文說明
KIT_JOIN_METHODS <- c(
  # 原文：向後追蹤樣本：只保留第一波次有出席的人，後續新加入的不會被納入（left join）
  "\u5411\u5f8c\u8ffd\u8e64\u6a23\u672c\uff1a\u53ea\u4fdd\u7559\u7b2c\u4e00\u6ce2\u6b21\u6709\u51fa\u5e2d\u7684\u4eba\uff0c\u5f8c\u7e8c\u65b0\u52a0\u5165\u7684\u4e0d\u6703\u88ab\u7d0d\u5165\uff08left join\uff09" = "left_join",
  # 原文：全勤樣本：只保留每個波次都有出席的人（inner join）
  "\u5168\u52e4\u6a23\u672c\uff1a\u53ea\u4fdd\u7559\u6bcf\u500b\u6ce2\u6b21\u90fd\u6709\u51fa\u5e2d\u7684\u4eba\uff08inner join\uff09" = "inner_join",
  # 原文：全樣本：任何一波次有出席就會保留（full join）
  "\u5168\u6a23\u672c\uff1a\u4efb\u4f55\u4e00\u6ce2\u6b21\u6709\u51fa\u5e2d\u5c31\u6703\u4fdd\u7559\uff08full join\uff09" = "full_join"
)

# 「用 R 怎麼做」程式碼上面的說明（kitDesign::kit_code_blocks(note = )）
KIT_CODE_NOTE_JOIN <- paste(
  # 原文：以下程式碼依你目前在這一頁的設定產生，是簡化的示範；
  "\u4ee5\u4e0b\u7a0b\u5f0f\u78bc\u4f9d\u4f60\u76ee\u524d\u5728\u9019\u4e00\u9801\u7684\u8a2d\u5b9a\u7522\u751f\uff0c\u662f\u7c21\u5316\u7684\u793a\u7bc4\uff1b",
  # 原文：app 實際處理另外還有編碼修正、型別檢查與核對等步驟。
  "app \u5be6\u969b\u8655\u7406\u53e6\u5916\u9084\u6709\u7de8\u78bc\u4fee\u6b63\u3001\u578b\u5225\u6aa2\u67e5\u8207\u6838\u5c0d\u7b49\u6b65\u9a5f\u3002"
)

# 通知訊息顯示秒數（kitJoin 自己的通知；狀態框的通知是 kitDesign::kit_notify()）
KIT_NOTIFY_SECONDS <- 6
KIT_NOTIFY_ERROR_SECONDS <- 10
