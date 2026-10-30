test_that("⑧ 只列出另外兩種格式，轉換後可下載", {
  dir <- withr::local_tempdir()
  gz <- file.path(dir, "w1.csv.gz")
  readr::write_csv(data.frame(id = c("0012", "0013"), name = c("王", "李")), gz)

  shiny::testServer(mod_convert_server, {
    session$setInputs(file = upload_files(gz))
    html <- output$target_ui$html
    expect_no_match(html, 'value="csv.gz"')
    expect_match(html, 'value="csv"')
    expect_match(html, 'value="sav"')
    expect_no_match(html, "數值標籤")

    session$setInputs(target = "csv", run = 1)
    expect_match(output$status$html, "w1.csv.gz → w1.csv")
    expect_match(output$result$html, "下載 w1.csv")
    expect_match(output$summary_table, "2 列 × 2 欄")
    path <- kit_test_download_file(session, "download_file")
    back <- readr::read_csv(path, col_types = "cc")
    expect_equal(back$id, c("0012", "0013"))

    session$setInputs(target = "sav")
    expect_match(output$status$html, "需要重新執行")
    expect_null(result())
  })
})

test_that("⑧ 上傳 SAV 才出現「數值標籤轉成文字」，預設不勾", {
  dir <- withr::local_tempdir()
  sav <- write_labelled_sav(file.path(dir, "w1.sav"))
  shiny::testServer(mod_convert_server, {
    session$setInputs(file = upload_files(sav))
    expect_match(output$target_ui$html, "數值標籤轉成文字")
    expect_no_match(output$target_ui$html, "checkbox\"[^>]*checked")
    session$setInputs(target = "csv", labels_as_text = FALSE, run = 1)
    back <- readr::read_csv(kit_test_download_file(session, "download_file"), col_types = "cdd")
    expect_equal(back$sex, c(1, 2, NA, NA))
  })
})

test_that("⑧ 沒選檔案就按轉換：提示先選檔案", {
  shiny::testServer(mod_convert_server, {
    session$setInputs(target = "csv.gz", run = 1)
    expect_match(output$status$html, "請先按")
  })
})

test_that("⑧ 與其他分頁完全獨立：① 沒有檔案也能轉，轉完 ① 仍是空的", {
  dir <- withr::local_tempdir()
  csv <- file.path(dir, "w1.csv")
  readr::write_csv(data.frame(id = c("0012", "0013")), csv)

  shiny::testServer(kit_server, {
    session$setInputs(`convert-file` = upload_files(csv))
    session$setInputs(`convert-target` = "csv.gz", `convert-run` = 1)
    expect_match(output[["convert-status"]]$html, "轉換完成")
    expect_null(output[["upload-file_list"]])
    expect_match(output[["stack-source_note"]]$html, "需要至少 2 個波次的檔案")
  })
  expect_equal(names(formals(mod_convert_server)), "id")
})

test_that("⑧ 分頁在 app 裡", {
  expect_match(as.character(kit_ui(NULL)), "⑧ 檔案格式轉換")
})
