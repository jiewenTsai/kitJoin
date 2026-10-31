# ⑧ 下載按鈕：網址最後一段、download 屬性、Content-Disposition 都是同一個檔名

# 轉換後的結果 HTML 裡所有下載檔名屬性（data-kit-download；空的也算）
download_attrs <- function(html) {
  pattern <- '(?<=\\s)data-kit-download(="[^"]*")?(?=[\\s>])'
  regmatches(html, gregexpr(pattern, html, perl = TRUE))[[1]]
}

convert_download <- function(name, target) {
  dir <- withr::local_tempdir()
  gz <- file.path(dir, name)
  write_kit_csv_gz(data.frame(id = c("0012", "0013"), score = c(1, 2)), gz)
  out <- NULL
  shiny::testServer(mod_convert_server, {
    session$setInputs(file = upload_files(gz))
    session$setInputs(target = target, run = 1)
    html <- output$result$html
    res <- kit_test_download(session, "download_file")
    out <<- list(
      html = html, href = kit_link_attr(html, session$ns("download_file"), "href"),
      res = res, same = identical(file_bytes(res$content$file), file_bytes(result()$out_path))
    )
  })
  out
}

test_that("⑧ 中文檔名：download、網址結尾、標頭都是「第一波 資料.sav」", {
  out <- convert_download("第一波 資料.csv.gz", "sav")
  expect_equal(download_attrs(out$html), 'data-kit-download="第一波 資料.sav"')
  expect_match(out$href, "/%E7%AC%AC%E4%B8%80%E6%B3%A2%20%E8%B3%87%E6%96%99.sav?", fixed = TRUE)
  expect_equal(
    out$res$headers[["Content-Disposition"]],
    paste0(
      'attachment; filename="kitjoin.sav"; ',
      "filename*=UTF-8''%E7%AC%AC%E4%B8%80%E6%B3%A2%20%E8%B3%87%E6%96%99.sav"
    )
  )
  expect_true(out$same)
  expect_equal(as.vector(haven::read_sav(out$res$content$file)$id), c("0012", "0013"))
})

test_that("⑧ 英文檔名：download=\"in.csv\"，網址結尾 /in.csv", {
  out <- convert_download("in.csv.gz", "csv")
  expect_equal(download_attrs(out$html), 'data-kit-download="in.csv"')
  expect_match(out$href, "/in.csv?w=", fixed = TRUE)
  expect_equal(out$res$headers[["Content-Disposition"]], 'attachment; filename="in.csv"')
  expect_true(out$same)
})
