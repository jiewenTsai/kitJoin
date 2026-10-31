# 測試資料：中文、0 開頭的 ID、遺漏值
sample_df <- function() {
  tibble::tibble(
    id = c("0012", "0013", "0100"),
    name = c("王小明", "李", NA),
    score = c(10.5, NA, 30)
  )
}

# 讀 gzip 檔解壓縮後的前 n 個位元組
read_gz_raw <- function(path, n = 1e6) {
  con <- gzfile(path, "rb")
  on.exit(close(con))
  readBin(con, "raw", n)
}

# 依格式寫出測試檔
write_as <- function(df, dir, kind) {
  path <- file.path(dir, paste0("in.", kind))
  switch(kind,
    csv = readr::write_csv(df, path, na = ""),
    csv.gz = readr::write_csv(df, path, na = ""),
    sav = haven::write_sav(df, path)
  )
  path
}

# 讀回轉換結果（id 一律當文字讀，檢查 0 有沒有保留）
read_back <- function(path) {
  if (grepl("[.]sav$", path)) {
    return(haven::zap_formats(haven::read_sav(path)))
  }
  readr::read_csv(path, col_types = readr::cols(id = "c", name = "c", score = "d"))
}

test_that("依檔名判斷格式、可轉成的格式、輸出檔名", {
  expect_equal(
    convert_kind(c("a.csv", "b.CSV.GZ", "c.gz", "d.sav", "e.zsav", "f.xlsx")),
    c("csv", "csv.gz", "csv.gz", "sav", "sav", NA)
  )
  expect_equal(unname(convert_targets("csv")), c("csv.gz", "sav"))
  expect_equal(unname(convert_targets("csv.gz")), c("csv", "sav"))
  expect_equal(unname(convert_targets("sav")), c("csv", "csv.gz"))
  expect_equal(convert_out_name("w1.csv.gz", "sav"), "w1.sav")
  expect_equal(convert_out_name("W1.SAV", "csv.gz"), "W1.csv.gz")
  expect_equal(format_bytes(c(500, 2048, 5 * 1024^2, NA)), c("500 B", "2.0 KB", "5.0 MB", "—"))
})

test_that("六種轉換：列數、欄數、欄名、0 開頭的 ID 都不變", {
  pairs <- expand.grid(from = c("csv", "csv.gz", "sav"), to = c("csv", "csv.gz", "sav"),
                       stringsAsFactors = FALSE)
  pairs <- pairs[pairs$from != pairs$to, ]
  expect_equal(nrow(pairs), 6)
  for (i in seq_len(nrow(pairs))) {
    from <- pairs$from[i]
    to <- pairs$to[i]
    dir <- withr::local_tempdir()
    path <- write_as(sample_df(), dir, from)
    res <- convert_file(path, basename(path), to, withr::local_tempdir())
    label <- paste(from, "→", to)
    expect_equal(c(res$nrow, res$ncol), c(3, 3), label = label)
    back <- read_back(res$out_path)
    expect_equal(names(back), c("id", "name", "score"), label = label)
    expect_equal(back$id, c("0012", "0013", "0100"), label = label)
    expect_equal(back$name[1], "王小明", label = label)
    expect_equal(back$score, c(10.5, NA, 30), label = label)
  }
})

test_that("CSV → CSV.GZ → CSV 逐段處理，幾 MB 的檔案位元組完全相同（只多 BOM）", {
  dir <- withr::local_tempdir()
  n <- 150000
  lines <- c(
    "id,city,score",
    sprintf("%06d,%s,%.3f\r", seq_len(n), rep(c("臺北市", "高雄市", "花蓮縣"), length.out = n),
            seq_len(n) / 7)
  )
  csv <- file.path(dir, "big.csv")
  writeBin(charToRaw(paste0(paste(lines, collapse = "\n"), "\n")), csv)
  expect_gt(file.size(csv), 2 * 1024^2)

  out1 <- withr::local_tempdir()
  gz <- convert_file(csv, "big.csv", "csv.gz", out1)
  expect_lt(gz$size_after, gz$size_before)
  expect_equal(c(gz$nrow, gz$ncol), c(n, 3))
  out2 <- withr::local_tempdir()
  back <- convert_file(gz$out_path, gz$out_name, "csv", out2)
  expect_equal(read_gz_raw(gz$out_path, 3), charToRaw("id,"))
  expect_equal(readBin(back$out_path, "raw", file.size(back$out_path)),
               c(KIT_UTF8_BOM, readBin(csv, "raw", file.size(csv))))
  expect_equal(c(back$nrow, back$ncol), c(n, 3))
  expect_length(back$notes, 0)
})

test_that("UTF-8 的 CSV.GZ 轉 CSV：開頭補 BOM；原本有 BOM 不重複加", {
  dir <- withr::local_tempdir()
  gz <- file.path(dir, "w1.csv.gz")
  write_kit_csv_gz(data.frame(id = "0012", name = "王小明"), gz)
  expect_false(identical(read_gz_raw(gz, 3), KIT_UTF8_BOM))
  res <- convert_file(gz, "w1.csv.gz", "csv", withr::local_tempdir())
  expect_equal(readBin(res$out_path, "raw", 6), c(KIT_UTF8_BOM, charToRaw("id,")))
  expect_equal(c(res$nrow, res$ncol), c(1, 2))
  expect_equal(readr::read_csv(res$out_path, col_types = "cc")$name, "王小明")

  bom <- file.path(dir, "bom.csv.gz")
  con <- gzfile(bom, "wb")
  writeBin(c(KIT_UTF8_BOM, charToRaw("id,name\n0012,王小明\n")), con)
  close(con)
  res2 <- convert_file(bom, "bom.csv.gz", "csv", withr::local_tempdir())
  expect_equal(readBin(res2$out_path, "raw", 6), c(KIT_UTF8_BOM, charToRaw("id,")))
  expect_equal(c(res2$nrow, res2$ncol), c(1, 2))

  # 編碼無法判斷：照原樣複製，不加 BOM
  bad <- file.path(dir, "bad.csv.gz")
  con <- gzfile(bad, "wb")
  writeBin(as.raw(c(0x61, 0x0a, 0xe9, 0x0a)), con)
  close(con)
  res3 <- convert_file(bad, "bad.csv.gz", "csv", withr::local_tempdir())
  expect_equal(readBin(res3$out_path, "raw", 10), as.raw(c(0x61, 0x0a, 0xe9, 0x0a)))
})

test_that("逐段複製：最後一列沒有換行也算一列", {
  dir <- withr::local_tempdir()
  csv <- file.path(dir, "a.csv")
  writeBin(charToRaw("x,y\n1,2\n3,4"), csv)
  res <- convert_file(csv, "a.csv", "csv.gz", dir)
  expect_equal(c(res$nrow, res$ncol), c(2, 2))
  expect_equal(read_gz_raw(res$out_path), charToRaw("x,y\n1,2\n3,4"))
})

test_that("BIG5 的 CSV 逐段轉成 UTF-8：.csv 加 BOM，.csv.gz 不加", {
  dir <- withr::local_tempdir()
  big5 <- write_big5(file.path(dir, "b.csv.gz"), c("id,name", "0012,王小明"), gz = TRUE)
  expect_equal(detect_csv_encoding(big5, "csv.gz"), "BIG5")

  res <- convert_file(big5, "b.csv.gz", "csv", dir)
  expect_match(res$notes, "BIG5")
  expect_equal(readBin(res$out_path, "raw", 3), as.raw(c(0xef, 0xbb, 0xbf)))
  expect_equal(readr::read_csv(res$out_path, col_types = "cc")$name, "王小明")

  plain <- write_big5(file.path(dir, "c.csv"), c("id,name", "0012,王小明"))
  res2 <- convert_file(plain, "c.csv", "csv.gz", dir)
  first <- read_gz_raw(res2$out_path, 3)
  expect_false(identical(first, as.raw(c(0xef, 0xbb, 0xbf))))
  expect_equal(readLines(gzfile(res2$out_path), encoding = "UTF-8")[2], "0012,王小明")
})

test_that("判斷編碼：UTF-8、BIG5、都不是", {
  dir <- withr::local_tempdir()
  u <- file.path(dir, "u.csv")
  writeLines(c("a", "王"), u, useBytes = TRUE)
  expect_equal(detect_csv_encoding(u, "csv"), "UTF-8")
  bad <- file.path(dir, "bad.csv")
  writeBin(as.raw(c(0x61, 0x0a, 0xe9, 0x0a)), bad)
  expect_true(is.na(detect_csv_encoding(bad, "csv")))
  res <- convert_file(bad, "bad.csv", "csv.gz", dir)
  expect_match(res$notes, "無法判斷編碼")
})

test_that("SAV 轉 CSV：預設寫代碼，勾選時寫標籤文字；自訂遺漏值變 NA", {
  dir <- withr::local_tempdir()
  sav <- write_labelled_sav(file.path(dir, "w1.sav"))
  codes <- convert_file(sav, "w1.sav", "csv", withr::local_tempdir())
  expect_match(paste(codes$notes, collapse = ""), "不會保留 SPSS 的變項標籤")
  back <- readr::read_csv(codes$out_path, col_types = "cdd")
  expect_equal(back$sex, c(1, 2, NA, NA))

  text <- convert_file(sav, "w1.sav", "csv.gz", withr::local_tempdir(), labels_as_text = TRUE)
  back2 <- readr::read_csv(text$out_path, col_types = "ccd")
  expect_equal(back2$sex, c("男", "女", NA, NA))
  expect_equal(back2$id[1], "0012")
})

test_that("CSV 轉 SAV：不合法的欄名自動改名並說明", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "w1.csv")
  readr::write_csv(tibble::tibble(`1st` = 1:2, `a b` = c("x", "y")), path)
  res <- convert_file(path, "w1.csv", "sav", dir)
  expect_match(res$notes, "1st → v_1st")
  expect_equal(names(haven::read_sav(res$out_path)), c("v_1st", "a_b"))
})

test_that("不支援的檔案、相同格式給白話錯誤", {
  dir <- withr::local_tempdir()
  bad <- file.path(dir, "data.xlsx")
  writeLines("x", bad)
  expect_error(convert_file(bad, "data.xlsx", "csv", dir), "CSV UTF-8", class = "kit_user_error")
  csv <- file.path(dir, "a.csv")
  writeLines("x", csv)
  expect_error(convert_file(csv, "a.csv", "csv", dir), "不同的格式", class = "kit_user_error")
})

test_that("結果表：檔案、轉成、列 × 欄、大小", {
  res <- list(name = "a.csv", out_name = "a.csv.gz", nrow = 3, ncol = 2,
              size_before = 2048, size_after = 500, notes = character(0))
  tbl <- convert_summary_table(res)
  expect_equal(names(tbl), c("檔案", "轉成", "列 × 欄", "原始大小", "轉換後大小"))
  expect_equal(tbl$`列 × 欄`, "3 列 × 2 欄")
})
