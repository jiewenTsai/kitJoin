# join_waves()：ID 標準化（前導零、大小寫）與不分波次的欄位

test_that("leading zeros and case are flagged, and matched when asked", {
  a <- data.frame(pid = c(1, 2, 3), x = 1:3)
  b <- data.frame(pid = c("001", "002", "3"), x = 4:6)
  out <- join_waves(list(a, b), c("a.csv", "b.csv"), c("t1", "t2"), "pid", "full_join")
  expect_equal(nrow(out$result), 5)
  expect_true(any(grepl("前導零", out$warnings)))

  out2 <- join_waves(list(a, b), c("a.csv", "b.csv"), c("t1", "t2"), "pid", "full_join",
                     id_normalize = "zeros")
  expect_equal(nrow(out2$result), 3)
  expect_equal(out2$result$x_t2, 4:6)
  expect_true(any(grepl("前導零", out2$notes)))
  expect_false(any(grepl("前導零", out2$warnings)))

  c1 <- data.frame(id = c("a01", "b02"), y = 1:2)
  c2 <- data.frame(id = c("A01", "B02"), y = 3:4)
  out3 <- join_waves(list(c1, c2), c("a.csv", "b.csv"), c("t1", "t2"), "id", "inner_join",
                     id_normalize = "case")
  expect_equal(out3$result$id, c("A01", "B02"))
  expect_equal(out3$result$y_t2, 3:4)
})

test_that("normalize_id_text keeps a lone zero and leaves non-numeric prefixes", {
  expect_equal(normalize_id_text(c("000", "0012", "A001", NA), "zeros"), c("0", "12", "A001", NA))
})

test_that("time-invariant columns become one column, first non-missing wave wins", {
  a <- data.frame(id = 1:3, sex = c(1, NA, 2), score = 1:3)
  b <- data.frame(id = 1:3, sex = c(1, 2, 1), score = 4:6)
  out <- join_waves(list(a, b), c("a.csv", "b.csv"), c("t1", "t2"), "id", "left_join",
                    constant_cols = "sex")
  r <- out$result
  expect_equal(names(r)[1:2], c("id", "sex"))
  expect_false(any(c("sex_t1", "sex_t2") %in% names(r)))
  expect_equal(r$sex, c(1, 2, 2))
  expect_true(any(grepl("sex.*1 人", out$warnings)))
  expect_true(all(c("score_t1", "score_t2") %in% names(r)))
})

test_that("time-invariant columns keep SPSS labels and handle mixed types", {
  a <- data.frame(id = 1:2)
  a$sex <- haven::labelled(c(1, 2), c(boy = 1, girl = 2), label = "Sex")
  b <- data.frame(id = 1:2, sex = c("1", "2"))
  out <- join_waves(list(a, b), c("a.sav", "b.csv"), c("t1", "t2"), "id", constant_cols = "sex")
  expect_equal(as.vector(out$result$sex), c("1", "2"))
  expect_equal(attr(out$result$sex, "label"), "Sex")
  a2 <- a; b2 <- data.frame(id = 1:2); b2$sex <- haven::labelled(c(1, 2), c(boy = 1, girl = 2))
  out2 <- join_waves(list(a2, b2), c("a.sav", "b.sav"), c("t1", "t2"), "id", constant_cols = "sex")
  expect_equal(attr(out2$result$sex, "labels"), c(boy = 1, girl = 2))
  expect_length(out2$warnings, 0)
})

test_that("join code shows normalization and coalesce for time-invariant columns", {
  f <- list(obj = c("t1", "t2"), wave = c("t1", "t2"), name = c("a.csv", "b.csv"))
  code <- code_join(f, "id", "left_join", id_normalize = c("zeros", "case"), constant_cols = "sex")
  expect_match(code$tidy, "toupper\\(sub\\(")
  expect_match(code$tidy, "coalesce\\(sex_t1, sex_t2\\)")
  expect_match(code$base, "for \\(w in \"sex_t2\"\\)")
})
