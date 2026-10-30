test_that("CSV.GZ 匯出：是 gzip、遺漏值是空白、0 開頭的 ID 讀回不變", {
  path <- withr::local_tempfile(fileext = ".csv.gz")
  df <- data.frame(id = c("0012", "0013"), x = c(1, NA), name = c("王", "李"))
  write_kit_csv_gz(df, path)
  expect_equal(readBin(path, "raw", 2), as.raw(c(0x1f, 0x8b)))
  expect_equal(readLines(gzfile(path), encoding = "UTF-8")[3], "0013,,李")
  back <- readr::read_csv(path, show_col_types = FALSE)
  expect_equal(back$id, df$id)
})

test_that("下載區有 CSV.GZ 按鈕，下載的檔案是壓縮過的 CSV", {
  dir <- withr::local_tempdir()
  w1 <- file.path(dir, "w1.csv")
  w2 <- file.path(dir, "w2.csv")
  readr::write_csv(data.frame(release_id = 1:3, score = 1:3), w1)
  readr::write_csv(data.frame(release_id = 2:4, score = 4:6), w2)

  shiny::testServer(kit_server, {
    session$setInputs(`join-by_vars` = NULL)
    expect_match(output[["join-downloads"]]$html, "下載 CSV.GZ")

    session$setInputs(`upload-files` = upload_files(c(w1, w2)))
    session$setInputs(`stack-run` = 1)
    expect_match(output[["stack-downloads"]]$html, "download_csvgz")
    path <- output[["stack-download_csvgz"]]
    expect_equal(readBin(path, "raw", 2), as.raw(c(0x1f, 0x8b)))
    back <- readr::read_csv(gzfile(path), show_col_types = FALSE)
    expect_equal(nrow(back), 6)
  })
})

test_that("① 不收 .csv.gz：請使用者先到 ⑧ 轉成 CSV", {
  path <- withr::local_tempfile(fileext = ".gz")
  readr::write_csv(data.frame(x = 1), path)
  expect_error(read_data_file(path, "w1.csv.gz"), "⑧", class = "kit_user_error")
})
