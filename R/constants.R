# 全 app 共用的常數

# 預覽表最多顯示幾欄、幾列
KIT_PREVIEW_COLS <- 20L
KIT_PREVIEW_ROWS <- 10L
KIT_RESHAPE_PREVIEW_ROWS <- 20L   # ⑦ 長寬互轉

# 單檔上傳上限（MB）
KIT_UPLOAD_MB <- 500

# 寬格式串接方法：值為 dplyr 函式名稱，名稱為給使用者看的中文說明
KIT_JOIN_METHODS <- c(
  "向後追蹤樣本：只保留第一波次有出席的人，後續新加入的不會被納入（left join）" = "left_join",
  "全勤樣本：只保留每個波次都有出席的人（inner join）" = "inner_join",
  "全樣本：任何一波次有出席就會保留（full join）" = "full_join"
)

# 通知訊息顯示秒數
KIT_NOTIFY_SECONDS <- 6
KIT_NOTIFY_ERROR_SECONDS <- 10
