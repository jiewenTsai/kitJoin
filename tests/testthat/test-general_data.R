# 一般研究資料：Excel、Stata、SPSS 使用者自訂遺漏值、各欄自己的遺漏值代碼、codebook、流程紀錄

sav_with_user_na <- function(dir, file, ids, v) {
  d <- data.frame(id = ids)
  d$q <- haven::labelled_spss(v, c(Yes = 1, No = 2, Refused = 99), na_values = 99, label = "Question")
  path <- file.path(dir, file)
  haven::write_sav(d, path)
  path
}

test_that("SPSS user-missing values are NA by default, kept when asked, and survive join and SAV export", {
  dir <- withr::local_tempdir()
  pa <- sav_with_user_na(dir, "a.sav", 1:3, c(1, 99, 2))
  pb <- sav_with_user_na(dir, "b.sav", 1:3, c(99, 1, 1))
  expect_equal(as.vector(unclass(read_data_file(pa, "a.sav")$data$q)), c(1, NA, 2))

  a <- read_data_file(pa, "a.sav", user_na = TRUE)$data
  b <- read_data_file(pb, "b.sav", user_na = TRUE)$data
  expect_s3_class(a$q, "haven_labelled_spss")
  expect_true(has_user_na(list(a, b)))

  j <- join_waves(list(a, b), c("a.sav", "b.sav"), c("t1", "t2"), "id")$result
  expect_equal(attr(j$q_t1, "na_values"), 99)
  out <- file.path(dir, "j.sav")
  write_kit_sav(j, out)
  back <- haven::read_sav(out, user_na = TRUE)
  expect_equal(attr(back$q_t1, "na_values"), 99)
  expect_equal(as.vector(unclass(back$q_t1)), c(1, 99, 2))

  r <- replace_user_na(a, "id")
  expect_equal(as.vector(unclass(r$data$q)), c(1, NA, 2))
  expect_false(inherits(r$data$q, "haven_labelled_spss"))
  expect_equal(r$detail[["合計"]], 1L)
})

test_that("Excel and Stata files are read; Stata export renames bad names", {
  dir <- withr::local_tempdir()
  d <- data.frame(id = 1:2, "第一題" = c(3, 4), check.names = FALSE)
  dta <- file.path(dir, "x.dta")
  write_kit_dta(d, dta)
  back <- read_data_file(dta, "x.dta")$data
  expect_equal(nrow(back), 2)
  expect_true(all(grepl("^[A-Za-z_][A-Za-z0-9_]*$", names(back))))
  expect_equal(dta_safe_names(c("a b", "1x", "ok")), c("a_b", "v_1x", "ok"))

  skip_if_not_installed("writexl")
  path <- file.path(dir, "x.xlsx")
  writexl::write_xlsx(list(first = data.frame(pid = c("001", "002"), y = 1:2), second = data.frame(z = 1)), path)
  out <- read_data_file(path, "x.xlsx")
  expect_equal(out$data$pid, c("001", "002"))
  expect_equal(out$sheets, c("first", "second"))
})

test_that("each column can have its own missing codes", {
  d <- data.frame(id = 1:3, age = c(99, 999, 30), q1 = c(8, 1, 9999), q2 = c(9999, 1, 2))
  out <- replace_na_codes(d, 9999, "id", codes_by_col = list(age = 999, q1 = c(8, 9)))
  expect_equal(out$data$age, c(99, NA, 30))   # 99 歲不是遺漏值
  expect_equal(out$data$q1, c(NA, 1, 9999))   # q1 只用自己的代碼
  expect_equal(out$data$q2, c(NA, 1, 2))
  expect_equal(out$detail[["合計"]], c(1, 1, 1))

  p <- parse_na_codes_by_col("age: 999\nq1, q2：8,9\nweight:\nbad line: x", c("age", "q1", "q2"))
  expect_equal(p$map$q1, c(8, 9))
  expect_length(p$map$weight, 0)
  expect_equal(p$invalid, "bad line: x")
  expect_equal(p$unknown, "weight")
  skip_out <- replace_na_codes(data.frame(w = 9999), 9999, codes_by_col = list(w = numeric(0)))
  expect_equal(skip_out$data$w, 9999)
})

test_that("presets and ID guessing cover general research data", {
  expect_equal(kit_na_codes_default(), "7777, 8888, 9995, 9996, 9999")
  expect_equal(kit_na_codes_default("general"), "")
  expect_equal(
    guess_id_cols(c("participant", "subject", "PROLIFIC_PID", "record_id", "score", "valid")),
    c("participant", "subject", "PROLIFIC_PID", "record_id")
  )
})

test_that("missing code shows per-column codes and user-missing conversion", {
  f <- code_files(c("a.csv", "b.sav"), cols = list(c("id", "age"), c("id", "age")))
  x <- code_missing(f, 9999, "id", TRUE, codes_by_col = list(age = 999), user_na = TRUE)
  expect_match(x$tidy, "haven::zap_missing")
  expect_match(x$tidy, "age = replace\\(age, age %in% c\\(999\\), NA\\)")
  expect_match(x$base, "setdiff\\(names\\(data_1\\), c\\(\"id\", \"age\"\\)\\)")
  only_col <- code_missing(f, numeric(0), "id", TRUE, codes_by_col = list(age = 999))
  expect_no_match(only_col$tidy, "na_codes <-")
})

test_that("codebook lists labels, user-missing values and counts", {
  d <- data.frame(id = 1:3)
  d$q <- haven::labelled_spss(c(1, 99, NA), c(Yes = 1, Refused = 99), na_values = 99, label = "Question")
  d$name <- c("a", "b", NA)
  cb <- codebook_table(d)
  expect_equal(cb[["欄位"]], c("id", "q", "name"))
  expect_equal(cb[["變項標籤"]][2], "Question")
  expect_equal(cb[["數值標籤"]][2], "1 = Yes；99 = Refused")
  expect_equal(cb[["使用者自訂遺漏值"]][2], "99")
  expect_equal(cb[["遺漏筆數"]], c(0L, 2L, 1L))
  expect_equal(cb[["型別"]], c("數字", "數字", "文字"))
})

test_that("pipeline script keeps the last run of each page in order; JSON has file fingerprints", {
  log <- list(
    list(page = "A", time = "t1", files = "a.csv", rows = 2, cols = 3, code = "a <- 1"),
    list(page = "B", time = "t2", files = "a.csv", rows = 2, cols = 3, code = "b <- 2"),
    list(page = "A", time = "t3", files = "a.csv", rows = 2, cols = 3, code = "a <- 3")
  )
  script <- pipeline_script(log, code_files("a.csv"))
  expect_true(any(script == "b <- 2"))
  expect_false(any(script == "a <- 1"))
  expect_lt(which(script == "b <- 2"), which(script == "a <- 3"))
  j <- pipeline_log_json(log, "a.csv", list(data.frame(x = 1)))
  expect_length(j$steps, 3)
  expect_match(j$files[[1]]$md5, "^[0-9a-f]{32}$")
  expect_equal(data_md5(data.frame(x = 1)), data_md5(data.frame(x = 1)))
  expect_equal(pipeline_log_table(log)[["分頁"]], c("A", "B", "A"))
})
