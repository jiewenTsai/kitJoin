test_that("留存 0、流失 1、ID 空白為 NA", {
  base <- tibble::tibble(id = c(1, 2, NA), x = 1:3)
  follow <- tibble::tibble(id = c(1, NA))
  out <- build_attrition(base, follow, "id")
  expect_equal(out$result$attrition, c(0L, 1L, NA))
  expect_match(out$warnings, "空白")
})

test_that("原檔已有 attrition 欄時不覆蓋", {
  base <- tibble::tibble(id = 1:2, attrition = c(9, 9))
  out <- build_attrition(base, tibble::tibble(id = 1), "id")
  expect_equal(out$flag_col, "kit_attrition")
  expect_equal(out$result$attrition, c(9, 9))
  expect_equal(out$result$kit_attrition, c(0L, 1L))
})

test_that("ID 型別不一致時轉文字比對", {
  base <- tibble::tibble(id = 1:2)
  follow <- tibble::tibble(id = "1")
  out <- build_attrition(base, follow, "id")
  expect_equal(out$result$attrition, c(0L, 1L))
})

test_that("前期 ID 大多重複時停下，少量重複時提醒（A3）", {
  base <- tibble::tibble(c_birth = rep(c(97, 98), each = 10), x = 1:20)
  follow <- tibble::tibble(c_birth = c(97, 98))
  expect_error(build_attrition(base, follow, "c_birth"), "不是用來辨識", class = "kit_user_error")

  base2 <- tibble::tibble(id = c(1, 2, 3, 3), x = 1:4)
  out <- build_attrition(base2, tibble::tibble(id = 1:3), "id")
  expect_match(out$warnings, "前期有 1 個 ID 出現不只一次", all = FALSE)
})

test_that("前期和後期沒有相同 ID 時提醒，不是直接完成（A3）", {
  out <- build_attrition(tibble::tibble(id = 1:3), tibble::tibble(id = 4:6), "id")
  expect_equal(out$result$attrition, c(1L, 1L, 1L))
  expect_match(out$warnings, "沒有任何相同的 ID", all = FALSE)
})

test_that("前期與後期的 ID 欄名不同（w1_id → w4id）也能比對（C1）", {
  base <- tibble::tibble(w1_id = c(1, 2, 3), x = 1:3)
  follow <- tibble::tibble(w4id = c("1", "3"), y = 1:2)
  out <- build_attrition(base, follow, "w1_id", follow_id_vars = "w4id")
  expect_equal(out$result$attrition, c(0L, 1L, 0L))
  expect_named(out$result, c("w1_id", "x", "attrition"))
  expect_match(out$notes, "w4id", all = FALSE)
  expect_match(out$notes, "後期的 w4id 是文字", all = FALSE)
  expect_error(build_attrition(base, follow, "w1_id"), "後期樣本沒有 w1_id", class = "kit_user_error")
})
