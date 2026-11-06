# SPSS 的測量水準（R/sav_measure.R）
# fixtures/measure_demo.sav 是用 pyreadstat（ReadStat）寫的，測量水準是 SPSS 的設定：
# release_id、age_m 尺度；lanc01_m48、a_very_long_variable_name 次序；cogb13_m48、sex、city 名義

demo_measures <- c(
  release_id = "scale", lanc01_m48 = "ordinal", cogb13_m48 = "nominal", sex = "nominal",
  city = "nominal", age_m = "scale", a_very_long_variable_name = "ordinal"
)

test_that("讀 SAV 時每一欄掛上測量水準（含長變項名稱）", {
  path <- test_path("fixtures", "measure_demo.sav")
  expect_equal(sav_read_measures(path), demo_measures)
  df <- read_data_file(path, "measure_demo.sav")$data
  expect_equal(purrr::map_chr(df, col_measure), demo_measures)
})

test_that("haven 寫的 SAV 測量水準不對；write_kit_sav() 寫回原本的，資料不變", {
  path <- test_path("fixtures", "measure_demo.sav")
  df <- read_data_file(path, "measure_demo.sav")$data
  plain <- withr::local_tempfile(fileext = ".sav")
  haven::write_sav(df, plain)
  expect_equal(sav_read_measures(plain)[["lanc01_m48"]], "scale")
  out <- withr::local_tempfile(fileext = ".sav")
  write_kit_sav(df, out)
  expect_equal(sav_read_measures(out), demo_measures)
  back <- haven::read_sav(out)
  expect_equal(haven::zap_labels(back), haven::zap_labels(haven::read_sav(plain)))
  expect_equal(attr(back$lanc01_m48, "labels"), attr(df$lanc01_m48, "labels"))
})

test_that("讀不懂的檔不改任何東西", {
  f <- withr::local_tempfile(fileext = ".sav")
  writeBin(charToRaw("not a sav file at all"), f)
  expect_null(sav_read_measures(f))
  expect_length(sav_write_measures(f, c(x = "nominal")), 0)
  expect_equal(readBin(f, "raw", 100), charToRaw("not a sav file at all"))
})

test_that("疊加：各波的測量水準不同時用第一個檔案的，並提醒", {
  path <- test_path("fixtures", "measure_demo.sav")
  w1 <- read_data_file(path, "a.sav")$data
  w2 <- w1
  attr(w2$lanc01_m48, "measure") <- "nominal"
  attr(w2$age_m, "measure") <- "scale"
  s <- stack_waves(list(w1, w2), c("a.sav", "b.sav"), c("t1", "t2"))
  expect_equal(col_measure(s$result$lanc01_m48), "ordinal")
  expect_equal(col_measure(s$result$a_very_long_variable_name), "ordinal")
  expect_true(any(grepl("lanc01_m48", s$warnings) & grepl("測量水準", s$warnings)))
  out <- withr::local_tempfile(fileext = ".sav")
  write_kit_sav(s$result, out)
  expect_equal(sav_read_measures(out)[["lanc01_m48"]], "ordinal")
})

test_that("串接、篩選、寬轉長之後測量水準還在", {
  path <- test_path("fixtures", "measure_demo.sav")
  w1 <- read_data_file(path, "a.sav")$data
  j <- join_waves(list(w1, w1), c("a.sav", "b.sav"), c("t1", "t2"), "release_id")
  expect_equal(col_measure(j$result$lanc01_m48_t2), "ordinal")
  f <- filter_apply(w1, filter_spec_clean(list(
    keep_mode = "include", keep_cols = c("release_id", "cogb13_m48"), conds = list(),
    combine = "and", sort_cols = "age_m", sort_desc = NULL
  ), w1))
  expect_equal(col_measure(f$data$cogb13_m48), "nominal")
  args <- PIVOT_LONGER_DEFAULTS
  args$cols <- c("lanc01_m48", "a_very_long_variable_name")
  long <- pivot_longer_run(w1, args)
  expect_equal(col_measure(long$value), "ordinal")
})

test_that("文字欄在 SPSS 不能是尺度：寫成名義", {
  df <- tibble::tibble(x = c("a", "b"))
  attr(df$x, "measure") <- "scale"
  out <- withr::local_tempfile(fileext = ".sav")
  write_kit_sav(df, out)
  expect_equal(sav_read_measures(out)[["x"]], "nominal")
})

test_that("codebook 與變項檔有測量水準；變項檔貼回 SAV", {
  path <- test_path("fixtures", "measure_demo.sav")
  df <- read_data_file(path, "a.sav", user_na = TRUE)$data
  cb <- codebook_table(df)
  expect_equal(cb[["測量水準"]][cb[[1]] == "lanc01_m48"], "次序")
  dict <- sav_dictionary(df)
  expect_equal(stats::setNames(dict$measure, dict$variable), demo_measures)
  expect_equal(dict_measure(c("Nominal", "次序", "scale", "", "xx")),
               c("nominal", "ordinal", "scale", NA, NA))
  dir <- withr::local_tempdir()
  res <- convert_file(path, "w1.sav", "csv", dir)
  back <- convert_file(res$out_path, "w1.csv", "sav", dir,
                       dict = list(path = res$dict_path, name = "w1_variables.csv"))
  expect_equal(sav_read_measures(back$out_path), demo_measures)
})
