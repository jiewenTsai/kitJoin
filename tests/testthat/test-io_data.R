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

test_that("BIG5 被當成 Latin1 讀進來的亂碼能被辨識出來（A7）", {
  # 模擬舊版 SPSS：BIG5 位元組被當成 Latin1 文字
  mojibake <- function(x) iconv(iconv(x, "UTF-8", "CP950"), "latin1", "UTF-8")
  good <- c("樣本流水代碼", "您的性別是：", "id")
  expect_true(looks_like_big5_mojibake(mojibake(good)))
  expect_false(looks_like_big5_mojibake(good))
  expect_true(looks_like_chinese(good))
  expect_false(looks_like_chinese(mojibake(good)))
})

test_that("歐洲語言的 Latin1 文字不會被誤判成 BIG5", {
  g <- tibble::tibble(name = c("Müller", "Bärbel", "Größe"), x = 1:3)
  attr(g$x, "label") <- "Körpergröße in Zentimeter für Schüler"
  path <- withr::local_tempfile(fileext = ".sav")
  haven::write_sav(g, path)
  expect_false(looks_like_big5_mojibake(sav_text_sample(haven::read_sav(path))))
  expect_true(is.na(read_data_file(path, "de.sav")$encoding))
})

test_that("TDCYP w1_pq.sav（BIG5）讀成正確中文", {
  path <- "/Users/jiewentsai/Documents/ClaudePlayMac/lab20261030_kitsui/lab1031/tdcyp_data/w1_pq.sav"
  skip_if_not(file.exists(path))
  out <- read_data_file(path, "w1_pq.sav")
  expect_equal(out$encoding, "BIG5")
  expect_equal(attr(out$data$w1_id, "label"), "W1:樣本流水代碼")
  forced <- read_data_file(path, "w1_pq.sav", sav_encoding = "CP950")
  expect_equal(attr(forced$data$w1_id, "label"), "W1:樣本流水代碼")
  expect_match(code_read(code_files("w1_pq.sav", "t1", encodings = "BIG5"))$tidy,
               'read_sav\\("w1_pq.sav", encoding = "CP950"\\)')
})

test_that("空檔、只有表頭的檔在 ① 給白話錯誤", {
  expect_error(check_not_empty(tibble::tibble(), "empty.csv"), "空的", class = "kit_user_error")
  expect_error(
    check_not_empty(tibble::tibble(id = character()), "h.csv"), "0 列",
    class = "kit_user_error"
  )
})

test_that("SAV 寫完讀不回來時，清理文字欄的數值標籤再重寫並說明（B2）", {
  # 中文值 + 很長、含控制字元的文字值標籤（TDCYP w1_jsq 的 w1a8_k）
  bad <- haven::labelled(
    c("我喜歡的", "外套"),
    stats::setNames(paste0("99", strrep(" ", 78), "\v"), "遺漏值"),
    label = "選項說明"
  )
  df <- tibble::tibble(id = 1:2, w1a8_k = bad, ok = haven::labelled(1:2, c(是 = 1L)))
  path <- withr::local_tempfile(fileext = ".sav")
  written <- write_kit_sav(df, path)
  expect_equal(written$fixed, "w1a8_k")
  expect_equal(written$step, 1L)
  back <- haven::read_sav(path)
  expect_equal(unname(attr(back$w1a8_k, "labels")), "99")
  expect_equal(attr(back$w1a8_k, "label"), "選項說明")
  expect_equal(attr(back$ok, "labels"), c(是 = 1))
  expect_match(sav_fix_message(written), "w1a8_k")

  # 正常的檔不修改
  ok_path <- withr::local_tempfile(fileext = ".sav")
  expect_length(write_kit_sav(df["ok"], ok_path)$fixed, 0)
  expect_null(sav_fix_message(write_kit_sav(df["ok"], ok_path)))
})

test_that("TDCYP w1_jsq 的 SAV 匯出讀得回來", {
  path <- "/Users/jiewentsai/Documents/ClaudePlayMac/lab20261030_kitsui/lab1031/tdcyp_data/w1_jsq.sav"
  skip_if_not(file.exists(path))
  df <- read_data_file(path, "w1_jsq.sav")$data
  out <- withr::local_tempfile(fileext = ".sav")
  written <- write_kit_sav(df, out)
  expect_true("w1a8_k" %in% written$fixed)
  expect_equal(dim(haven::read_sav(out)), dim(df))
})

test_that("只有英文的 SAV 也能讀（編碼判斷不會出錯）", {
  path <- withr::local_tempfile(fileext = ".sav")
  haven::write_sav(tibble::tibble(id = c("1", "2"), a = 1:2), path)
  expect_true(is.na(read_data_file(path, "ascii.sav")$encoding))
  expect_equal(count_matches(c("abc", "a"), "[\\x{3400}-\\x{9FFF}]"), 0)
  expect_equal(count_matches(c("中文a", "字"), "[\\x{3400}-\\x{9FFF}]"), 3)
})
