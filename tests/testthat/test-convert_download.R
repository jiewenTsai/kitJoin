# ⑧ 下載按鈕的 download 屬性：中文檔名時瀏覽器靠它取檔名

# 轉換後的結果 HTML 裡所有 download 屬性（空的也算）
download_attrs <- function(html) {
  pattern <- '(?<=\\s)download(="[^"]*")?(?=[\\s>])'
  regmatches(html, gregexpr(pattern, html, perl = TRUE))[[1]]
}

convert_result_html <- function(name, target) {
  dir <- withr::local_tempdir()
  gz <- file.path(dir, name)
  write_kit_csv_gz(data.frame(id = c("0012", "0013"), score = c(1, 2)), gz)
  html <- NULL
  shiny::testServer(mod_convert_server, {
    session$setInputs(file = upload_files(gz))
    session$setInputs(target = target, run = 1)
    html <<- output$result$html
  })
  html
}

test_that("⑧ 中文檔名：按鈕帶 download=\"第一波 資料.sav\"，只有一個", {
  html <- convert_result_html("第一波 資料.csv.gz", "sav")
  expect_equal(download_attrs(html), 'download="第一波 資料.sav"')
})

test_that("⑧ 英文檔名：按鈕帶 download=\"in.csv\"，只有一個", {
  html <- convert_result_html("in.csv.gz", "csv")
  expect_equal(download_attrs(html), 'download="in.csv"')
})
