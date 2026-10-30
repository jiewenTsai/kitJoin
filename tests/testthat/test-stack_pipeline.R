test_that("全空欄不會把其他波的數值欄轉成文字", {
  a <- tibble::tibble(id = 1:2, score = c(NA, NA))
  b <- tibble::tibble(id = 3:4, score = c(1.5, 2))
  out <- stack_waves(list(a, b), c("a", "b"), c("t1", "t2"))
  expect_type(out$result$score, "double")
  expect_length(out$warnings, 0)
})

test_that("原檔已有 wave 欄時不覆蓋，改用 kit_wave", {
  a <- tibble::tibble(id = 1, wave = "orig")
  b <- tibble::tibble(id = 2, wave = "orig")
  out <- stack_waves(list(a, b), c("a", "b"), c("t1", "t2"))
  expect_equal(out$wave_col, "kit_wave")
  expect_equal(out$result$wave, c("orig", "orig"))
  expect_equal(out$result$kit_wave, c("t1", "t2"))
  expect_match(out$warnings, "kit_wave")
})

test_that("型別不一致的欄位轉文字並提醒", {
  a <- tibble::tibble(id = 1, x = 1)
  b <- tibble::tibble(id = 2, x = "a")
  out <- stack_waves(list(a, b), c("a", "b"), c("t1", "t2"))
  expect_type(out$result$x, "character")
  expect_match(out$warnings, "x")
})

test_that("數值標籤衝突會被偵測", {
  a <- tibble::tibble(v = haven::labelled(1, c(yes = 1)))
  b <- tibble::tibble(v = haven::labelled(1, c(agree = 1)))
  out <- stack_waves(list(a, b), c("a", "b"), c("t1", "t2"))
  expect_match(out$warnings, "數值標籤")
})

test_that("只有一個檔案時給白話錯誤", {
  expect_error(
    stack_waves(list(tibble::tibble(id = 1)), "a", "t1"),
    "至少 2 個檔案",
    class = "kit_user_error"
  )
})

test_that("核對：列數與各波列數", {
  a <- tibble::tibble(id = 1:2, x = 1:2)
  b <- tibble::tibble(id = 3:5, y = 1:3)
  out <- stack_waves(list(a, b), c("a", "b"), c("t1", "t2"))
  expect_equal(nrow(out$result), 5)
  expect_equal(out$audit$checks$核對, c("符合", "符合", "符合"))
})

test_that("資料裡有名為 w 的欄（例如權數）時，wave 欄仍是波次名稱（A4）", {
  a <- tibble::tibble(id = 1:2, w = c(0.8, 1.2))
  b <- tibble::tibble(id = 3:4, w = c(1.1, 0.9))
  out <- stack_waves(list(a, b), c("a", "b"), c("t1", "t2"))
  expect_equal(out$result$wave, c("t1", "t1", "t2", "t2"))
  expect_equal(out$result$w, c(0.8, 1.2, 1.1, 0.9))
  expect_length(out$warnings, 0)

  # 只有部分檔案有 w、也有名為 label 的欄
  c1 <- tibble::tibble(id = 1, label = "x", w = 2)
  c2 <- tibble::tibble(id = 2)
  out2 <- stack_waves(list(c1, c2), c("a", "b"), c("t1", "t2"))
  expect_equal(out2$result$wave, c("t1", "t2"))
  expect_equal(out2$audit$checks$核對, c("符合", "符合", "符合"))
})

test_that("疊加時數字欄轉文字不會變成科學記號（A5）", {
  a <- tibble::tibble(id = c(100000, 200000))
  b <- tibble::tibble(id = c("A1", "A2"))
  out <- stack_waves(list(a, b), c("a", "b"), c("t1", "t2"))
  expect_equal(out$result$id, c("100000", "200000", "A1", "A2"))
})
