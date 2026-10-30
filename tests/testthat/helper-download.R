# 下載按鈕（R/download.R）測試共用的小工具

# 按下下載：呼叫 kit_download_button() 註冊的回應函式
# （testServer 的假 session 不會真的註冊，註冊的東西存在 userData）
# id：模組裡用不含 namespace 的 id；kit_server 層用完整 id（如 "stack-download_csv"）
kit_test_download <- function(session, id) {
  store <- session$userData$kit_downloads
  dl <- store[[id]] %||% store[[session$ns(id)]]
  dl$filter(dl$data, list())
}

# 下載回來的檔案路徑
kit_test_download_file <- function(session, id) kit_test_download(session, id)$content$file

# HTML 裡某個 id 的 <a> 的屬性值
kit_link_attr <- function(html, id, attr) {
  tag <- regmatches(html, regexpr(paste0('<a [^>]*id="', id, '"[^>]*>'), html))
  value <- sub(paste0('.*\\s', attr, '="([^"]*)".*'), "\\1", tag)
  gsub("&amp;", "&", value, fixed = TRUE)
}

# SAV 的位元組，表頭裡的建立日期與時間（第 93–109 位元組）清成 0
sav_bytes <- function(path) {
  b <- readBin(path, "raw", file.size(path))
  b[93:109] <- as.raw(0)
  b
}

file_bytes <- function(path) readBin(path, "raw", file.size(path))
