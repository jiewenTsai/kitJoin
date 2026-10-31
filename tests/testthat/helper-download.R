# 下載按鈕（kitDesign::kit_download_button()）測試共用的小工具
# 按下載是 kitDesign::kit_test_download(session, id)：模組裡用不含 namespace 的 id，
# kit_server 層用完整 id（如 "stack-download_csv"）

# 下載回來的檔案路徑
kit_test_download_file <- function(session, id) kit_test_download(session, id)$content$file

# HTML 裡某個 id 的下載按鈕的屬性值。kitDesign 0.5.2 起按鈕是 <button>，網址與檔名在
# data-kit-href、data-kit-download；這裡照舊用 "href"、"download" 來問
kit_link_attr <- function(html, id, attr) {
  attr <- switch(attr, href = "data-kit-href", download = "data-kit-download", attr)
  tag <- regmatches(html, regexpr(paste0('<(a|button) [^>]*id="', id, '"[^>]*>'), html))
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
