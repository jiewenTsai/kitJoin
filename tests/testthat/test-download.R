# 下載按鈕：kitDesign::kit_download_button() 的機制在 kitDesign 測；這裡測 kitJoin 自己的部分：
# app 名稱（.onLoad）、CSV／CSV.GZ／SAV 的寫檔函式與 MIME

test_that("app 名稱是 kitJoin：中文檔名的 ASCII 備用名是 kitjoin，log 寫 kitJoin", {
  expect_equal(getOption("kitDesign.app"), "kitJoin")
  session <- shiny::MockShinySession$new()
  kit_download_button(session, "dl", "下載", "第一波 資料.sav", function(file) writeLines("x", file))
  expect_equal(
    kit_test_download(session, "dl")$headers[["Content-Disposition"]],
    paste0(
      'attachment; filename="kitjoin.sav"; ',
      "filename*=UTF-8''%E7%AC%AC%E4%B8%80%E6%B3%A2%20%E8%B3%87%E6%96%99.sav"
    )
  )
  kit_download_button(session, "dl", "下載", "x.csv", function(file) stop("壞掉"))
  expect_message(res <- kit_test_download(session, "dl"), "kitJoin 下載失敗（x.csv）：壞掉")
  expect_equal(res$status, 500)
})

test_that("回應：檔案位元組和寫檔函式一樣；.sav 與 .csv.gz 是 application/octet-stream", {
  df <- data.frame(id = c("0012", "0013"), x = c(1, NA), name = c("王", "李"))
  session <- shiny::MockShinySession$new()
  cases <- list(
    list(name = "第一波 資料.csv", writer = write_kit_csv, bytes = file_bytes,
         type = "text/csv; charset=UTF-8"),
    list(name = "第一波 資料.csv.gz", writer = write_kit_csv_gz, bytes = file_bytes,
         type = "application/octet-stream"),
    list(name = "joined_20261030_153000.sav", writer = write_kit_sav, bytes = sav_bytes,
         type = "application/octet-stream")
  )
  for (cs in cases) {
    kit_download_button(session, "dl", "下載", cs$name, function(file) cs$writer(df, file))
    res <- kit_test_download(session, "dl")
    expect_equal(res$status, 200)
    expect_equal(res$content_type, cs$type)
    expect_true(endsWith(res$content$file, sub("^[^.]*", "", cs$name)))
    old <- withr::local_tempfile(fileext = sub("^[^.]*", "", cs$name))
    cs$writer(df, old)
    expect_equal(cs$bytes(res$content$file), cs$bytes(old))
  }
  back <- haven::read_sav(res$content$file)
  expect_equal(as.vector(back$name), c("王", "李"))
})
