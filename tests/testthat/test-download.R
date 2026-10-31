# 下載按鈕：檔名標頭（RFC 6266）、網址結尾是檔名、檔案內容和寫檔函式一樣

test_that("Content-Disposition：ASCII 檔名只有 filename", {
  expect_equal(
    kit_content_disposition("joined_20261030_153000.sav"),
    'attachment; filename="joined_20261030_153000.sav"'
  )
  expect_equal(kit_content_disposition('a"b\\c.csv'), 'attachment; filename="a\\"b\\\\c.csv"')
})

test_that("Content-Disposition：中文與空白 → ASCII 備用名 + filename*（UTF-8）", {
  expect_equal(
    kit_content_disposition("第一波 資料.sav"),
    paste0(
      'attachment; filename="kitjoin.sav"; ',
      "filename*=UTF-8''%E7%AC%AC%E4%B8%80%E6%B3%A2%20%E8%B3%87%E6%96%99.sav"
    )
  )
  expect_equal(
    kit_content_disposition("第一波_na.csv.gz"),
    paste0(
      'attachment; filename="kitjoin_na.csv.gz"; ',
      "filename*=UTF-8''%E7%AC%AC%E4%B8%80%E6%B3%A2_na.csv.gz"
    )
  )
  expect_match(kit_content_disposition("w1 第一波.csv"), 'filename="w1.csv"', fixed = TRUE)
  # RFC 5987 attr-char 不編碼；其他（括號、引號）要編碼
  expect_match(kit_content_disposition("資料 (1)'.sav"), "%20%281%29%27.sav", fixed = TRUE)
})

test_that("副檔名與 Content-Type：.csv.gz 算一個", {
  expect_equal(kit_download_ext("第一波.csv.gz"), "csv.gz")
  expect_equal(kit_download_ext("a.SAV"), "SAV")
  expect_equal(kit_download_ext("readme"), "")
  expect_equal(kit_ascii_filename("第一波.CSV.GZ"), "kitjoin.CSV.GZ")
  expect_equal(kit_download_type("a.csv"), "text/csv; charset=UTF-8")
  expect_equal(kit_download_type("a.csv.gz"), "application/octet-stream")
  expect_equal(kit_download_type("a.sav"), "application/octet-stream")
})

test_that("網址最後一段是檔名（registerDataObj 的網址插入檔名），按鈕帶 download", {
  # 像模組的 session：registerDataObj() 自己加 namespace
  registered <- list()
  session <- list(
    ns = function(id) paste0("m-", id),
    registerDataObj = function(name, data, filterFunc) {
      registered[[paste0("m-", name)]] <<- list(data = data, filter = filterFunc)
      paste0("session/abc/dataobj/m-", name, "?w=&nonce=n1")
    }
  )
  html <- as.character(kit_download_button(
    session, "dl", "下載", "第一波_na.sav", function(file) writeLines("x", file)
  ))
  expect_equal(
    kit_link_attr(html, "m-dl", "href"),
    "session/abc/dataobj/m-dl/%E7%AC%AC%E4%B8%80%E6%B3%A2_na.sav?w=&nonce=n1"
  )
  expect_equal(kit_link_attr(html, "m-dl", "download"), "第一波_na.sav")
  expect_match(html, "fa-download")
  expect_named(registered, "m-dl")
  expect_equal(registered[["m-dl"]]$data$filename, "第一波_na.sav")
})

test_that("回應：200、標頭、檔案位元組和寫檔函式一樣（CSV、CSV.GZ、SAV）", {
  df <- data.frame(id = c("0012", "0013"), x = c(1, NA), name = c("王", "李"))
  session <- shiny::MockShinySession$new()
  cases <- list(
    list(name = "第一波 資料.csv", writer = write_kit_csv, bytes = file_bytes),
    list(name = "第一波 資料.csv.gz", writer = write_kit_csv_gz, bytes = file_bytes),
    list(name = "joined_20261030_153000.sav", writer = write_kit_sav, bytes = sav_bytes)
  )
  for (cs in cases) {
    kit_download_button(session, "dl", "下載", cs$name, function(file) cs$writer(df, file))
    res <- kit_test_download(session, "dl")
    expect_equal(res$status, 200)
    expect_equal(res$content_type, kit_download_type(cs$name))
    expect_equal(res$headers[["Content-Disposition"]], kit_content_disposition(cs$name))
    expect_equal(res$headers[["Cache-Control"]], "no-cache")
    expect_true(res$content$owned)
    expect_true(endsWith(res$content$file, paste0(".", kit_download_ext(cs$name))))
    old <- withr::local_tempfile(fileext = paste0(".", kit_download_ext(cs$name)))
    cs$writer(df, old)
    expect_equal(cs$bytes(res$content$file), cs$bytes(old))
  }
  back <- haven::read_sav(res$content$file)
  expect_equal(as.vector(back$name), c("王", "李"))
})

test_that("回應：寫檔函式可以讀 reactive；出錯時回 500 並寫 log", {
  session <- shiny::MockShinySession$new()
  value <- shiny::reactiveVal("a")
  kit_download_button(session, "dl", "下載", "x.csv", function(file) writeLines(value(), file))
  value("b")
  expect_equal(readLines(kit_test_download_file(session, "dl")), "b")

  kit_download_button(session, "dl", "下載", "x.csv", function(file) stop("壞掉"))
  expect_message(res <- kit_test_download(session, "dl"), "壞掉")
  expect_equal(res$status, 500)
  expect_match(res$content, "下載失敗")
})

test_that("下載失敗：在這個 session 跳出紅色通知（不會自己消失）；kit_abort() 的訊息照原文", {
  shown <- list()
  testthat::local_mocked_bindings(
    showNotification = function(ui, action = NULL, duration = 5, closeButton = TRUE, id = NULL,
                                type = "default", session = shiny::getDefaultReactiveDomain()) {
      shown[[length(shown) + 1]] <<- list(ui = ui, duration = duration, type = type,
                                          session = session)
      invisible(id)
    },
    .package = "shiny"
  )
  session <- shiny::MockShinySession$new()
  kit_download_button(session, "dl", "下載", "x.csv", function(file) stop("內部的錯誤"))
  expect_message(res <- kit_test_download(session, "dl"), "內部的錯誤")
  expect_equal(res$status, 500)
  expect_length(shown, 1)
  expect_equal(shown[[1]]$ui, "下載失敗，請重新執行後再試一次。")
  expect_equal(shown[[1]]$type, "error")
  expect_null(shown[[1]]$duration)
  expect_identical(shown[[1]]$session, session)
  expect_equal(res$content, "下載失敗，請重新執行後再試一次。")

  kit_download_button(session, "dl", "下載", "x.csv", function(file) kit_abort("先算分數。"))
  expect_message(res <- kit_test_download(session, "dl"), "先算分數")
  expect_equal(shown[[2]]$ui, "下載失敗：先算分數。")
  expect_equal(res$content, "下載失敗：先算分數。")

  # 通知送不出去（例如連線已經關了）時照樣回 500
  testthat::local_mocked_bindings(showNotification = function(...) stop("closed"),
                                  .package = "shiny")
  expect_message(res <- kit_test_download(session, "dl"), "先算分數")
  expect_equal(res$status, 500)
})
