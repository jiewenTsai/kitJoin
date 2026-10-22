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
