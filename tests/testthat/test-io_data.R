test_that("不支援的格式給白話錯誤（Excel 附另存說明）", {
  path <- withr::local_tempfile(fileext = ".xlsx")
  writeLines("x", path)
  expect_error(
    read_data_file(path, "data.xlsx"),
    "CSV UTF-8",
    class = "kit_user_error"
  )
})

test_that("損壞的 SAV 給白話錯誤", {
  path <- withr::local_tempfile(fileext = ".sav")
  writeLines("not a sav", path)
  expect_error(read_data_file(path, "bad.sav"), "損壞", class = "kit_user_error")
})

test_that("CSV 用全部列判斷型別，後段的文字值不會變成 NA", {
  path <- withr::local_tempfile(fileext = ".csv")
  values <- c(as.character(1:2000), "N/A_refused")
  readr::write_csv(data.frame(score = values), path)
  out <- read_data_file(path, "a.csv")
  expect_type(out$data$score, "character")
  expect_equal(out$data$score[2001], "N/A_refused")
})

test_that("BIG5 CSV 能正確辨識", {
  path <- withr::local_tempfile(fileext = ".csv")
  con <- file(path, open = "w", encoding = "CP950")
  writeLines(c("id,name", "1,王小明"), con)
  close(con)
  out <- read_data_file(path, "big5.csv")
  expect_equal(out$encoding, "BIG5")
  expect_equal(out$data$name, "王小明")
})

test_that("CSV 匯出的遺漏值是空白，不是 NA", {
  path <- withr::local_tempfile(fileext = ".csv")
  write_kit_csv(data.frame(id = 1:2, x = c(1, NA)), path)
  lines <- readLines(path, encoding = "UTF-8")
  expect_equal(lines[3], "2,")
})

test_that("SAV 欄名自動改成 SPSS 合法名稱並可寫出", {
  cols <- c("1st", "a b", "a.", "_a", "by", "ID", "id", strrep("得", 22))
  safe <- sav_safe_names(cols)
  expect_false(anyDuplicated(toupper(safe)) > 0)
  df <- stats::setNames(as.data.frame(as.list(seq_along(cols))), cols)
  path <- withr::local_tempfile(fileext = ".sav")
  expect_no_error(write_kit_sav(df, path))
  expect_equal(names(haven::read_sav(path)), safe)
  expect_equal(unname(sav_renamed_cols(c("ok", "1st"))), "v_1st")
})

test_that("預覽：整數不顯示小數、標籤欄顯示標籤、日期可讀", {
  df <- tibble::tibble(
    id = c(1, 2),
    sex = haven::labelled(c(1, 2), c("男" = 1, "女" = 2)),
    d = as.Date(c("2020-01-01", "2020-01-02"))
  )
  out <- df_for_display(df)
  expect_equal(out$id, c("1", "2"))
  expect_equal(out$sex, c("[1] 男", "[2] 女"))
  expect_equal(out$d[1], "2020-01-01")
})
