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
  "向後追蹤樣本：只保留第一波次有出席的人，後續新加入的不會被納入（left join）" = "left_join",
  "全勤樣本：只保留每個波次都有出席的人（inner join）" = "inner_join",
  "全樣本：任何一波次有出席就會保留（full join）" = "full_join"
)

# 「用 R 怎麼做」程式碼上面的說明（kitDesign::kit_code_blocks(note = )）
KIT_CODE_NOTE_JOIN <- paste(
  "以下程式碼依你目前在這一頁的設定產生，是簡化的示範；",
  "app 實際處理另外還有編碼修正、型別檢查與核對等步驟。"
)

# 通知訊息顯示秒數（kitJoin 自己的通知；狀態框的通知是 kitDesign::kit_notify()）
KIT_NOTIFY_SECONDS <- 6
KIT_NOTIFY_ERROR_SECONDS <- 10
