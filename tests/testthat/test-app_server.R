upload_df <- function(paths) {
  data.frame(
    name = basename(paths),
    size = file.size(paths),
    type = "",
    datapath = paths,
    stringsAsFactors = FALSE
  )
}

write_wave <- function(dir, name, df) {
  path <- file.path(dir, name)
  readr::write_csv(df, path)
  path
}

test_that("上傳讀不了的檔案不會讓 app 斷線，其他檔案照常載入", {
  dir <- withr::local_tempdir()
  good1 <- write_wave(dir, "w1.csv", data.frame(release_id = 1:3, score = 1:3))
  good2 <- write_wave(dir, "w2.csv", data.frame(release_id = 2:4, score = 4:6))
  bad <- file.path(dir, "data.xlsx")
  writeLines("not excel", bad)

  shiny::testServer(kit_server, {
    session$setInputs(`upload-files` = upload_df(c(good1, bad, good2)))
    expect_length(output[["upload-file_list"]]$html, 1)
    expect_match(output[["upload-file_list"]]$html, "w2.csv")
    expect_no_match(output[["upload-file_list"]]$html, "data.xlsx")
  })
})

test_that("疊加不會清掉寬格式串接的結果，兩邊都能下載", {
  dir <- withr::local_tempdir()
  w1 <- write_wave(dir, "w1.csv", data.frame(release_id = 1:3, score = 1:3))
  w2 <- write_wave(dir, "w2.csv", data.frame(release_id = 2:4, score = 4:6))

  shiny::testServer(kit_server, {
    session$setInputs(`upload-files` = upload_df(c(w1, w2)))
    session$setInputs(
      `join-by_vars` = "release_id", `join-method` = "left_join",
      `join-keys_as_text` = TRUE
    )
    session$setInputs(`join-run` = 1)
    expect_match(output[["join-downloads"]]$html, "download_csv")

    session$setInputs(`stack-run` = 1)
    expect_match(output[["stack-downloads"]]$html, "download_csv")
    expect_match(output[["join-downloads"]]$html, "download_csv")
    expect_match(output[["join-status"]]$html, "串接完成")
  })
})

test_that("沒有結果時下載按鈕是停用的", {
  shiny::testServer(kit_server, {
    session$setInputs(`join-by_vars` = NULL)
    expect_match(output[["join-downloads"]]$html, "disabled")
    expect_match(output[["join-downloads"]]$html, "執行串接")
  })
})

test_that("改了波次名稱後，舊結果失效並提示重新執行", {
  dir <- withr::local_tempdir()
  w1 <- write_wave(dir, "w1.csv", data.frame(release_id = 1:3, score = 1:3))
  w2 <- write_wave(dir, "w2.csv", data.frame(release_id = 2:4, score = 4:6))

  shiny::testServer(kit_server, {
    session$setInputs(`upload-files` = upload_df(c(w1, w2)))
    session$setInputs(`stack-run` = 1)
    expect_match(output[["stack-downloads"]]$html, "download_csv")
    session$setInputs(`upload-label_f2` = "w2")
    expect_match(output[["stack-status"]]$html, "需要重新執行")
    expect_match(output[["stack-downloads"]]$html, "disabled")
  })
})

test_that("一次上傳多個檔案時，每一列都能改波次名稱、排序、移除", {
  dir <- withr::local_tempdir()
  w1 <- write_wave(dir, "w1.csv", data.frame(release_id = 1:3, score = 1:3))
  w2 <- write_wave(dir, "w2.csv", data.frame(release_id = 2:4, score = 4:6))
  w3 <- write_wave(dir, "w3.csv", data.frame(release_id = 3:5, score = 7:9))

  shiny::testServer(
    mod_upload_server,
    args = list(goto = function(tab) NULL),
    {
      session$setInputs(files = upload_df(c(w1, w2, w3)))
      expect_equal(current_labels(), c("t1", "t2", "t3"))

      session$setInputs(label_f1 = "pre")
      session$setInputs(label_f2 = "mid")
      expect_equal(current_labels(), c("pre", "mid", "t3"))

      session$setInputs(down_f1 = 1)
      expect_equal(keys(), c("f2", "f1", "f3"))

      session$setInputs(remove_f2 = 1)
      expect_equal(keys(), c("f1", "f3"))
      expect_equal(current_labels(), c("pre", "t3"))
    }
  )
})
