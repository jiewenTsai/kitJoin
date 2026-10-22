w1 <- tibble::tibble(release_id = 1:4, score = c(10, 20, 30, 40))
w2 <- tibble::tibble(release_id = 2:5, score = c(21, 31, 41, 51))

test_that("left join（向後追蹤樣本）以第一個檔案為準，欄名加後綴", {
  out <- join_waves(
    list(w1, w2),
    c("a.csv", "b.csv"),
    c("t1", "t2"),
    "release_id"
  )
  expect_equal(nrow(out$result), 4)
  expect_named(out$result, c("release_id", "score_t1", "score_t2"))
  expect_length(out$warnings, 0)
  expect_match(out$summary[1], "4 人為準")
  expect_match(out$summary[2], "3 人有資料.*1 人沒有.*另有 1 人")
})

test_that("inner / full join 的列數", {
  inner <- join_waves(
    list(w1, w2),
    c("a", "b"),
    c("t1", "t2"),
    "release_id",
    "inner_join"
  )
  full <- join_waves(
    list(w1, w2),
    c("a", "b"),
    c("t1", "t2"),
    "release_id",
    "full_join"
  )
  expect_equal(nrow(inner$result), 3)
  expect_equal(nrow(full$result), 5)
  expect_equal(full$audit$checks$核對, c("符合", "符合"))
})

test_that("不合法的 join 方法會被拒絕", {
  expect_error(
    join_waves(
      list(w1, w2),
      c("a", "b"),
      c("t1", "t2"),
      "release_id",
      "cross_join"
    )
  )
})

test_that("波次名稱空白或重複時給白話錯誤", {
  expect_error(
    join_waves(list(w1, w2), c("a", "b"), c("t1", ""), "release_id"),
    "沒有填波次名稱",
    class = "kit_user_error"
  )
  expect_error(
    join_waves(list(w1, w2), c("a", "b"), c("t1", "t1"), "release_id"),
    "重複",
    class = "kit_user_error"
  )
})

test_that("ID 型別不一致：預設轉文字比對，關閉時給白話錯誤", {
  w2_text <- dplyr::mutate(w2, release_id = as.character(release_id))
  out <- join_waves(list(w1, w2_text), c("a", "b"), c("t1", "t2"), "release_id")
  expect_equal(sum(!is.na(out$result$score_t2)), 3)
  expect_match(out$notes, "轉成文字")
  expect_error(
    join_waves(
      list(w1, w2_text), c("a", "b"), c("t1", "t2"), "release_id",
      keys_as_text = FALSE
    ),
    "數字.*文字",
    class = "kit_user_error"
  )
})

test_that("ID 重複時提出警告並列出例子", {
  w2_dup <- dplyr::bind_rows(w2, tibble::tibble(release_id = 2L, score = 99))
  out <- join_waves(list(w1, w2_dup), c("a", "b"), c("t1", "t2"), "release_id")
  expect_equal(nrow(out$result), 5)
  expect_match(out$warnings, "release_id = 2（2 次）")
  expect_true(is.na(out$audit$checks$預期[1]))
})

test_that("ID 空白的列不會互相配對", {
  a <- tibble::tibble(id = c(1, NA), x = 1:2)
  b <- tibble::tibble(id = c(1, NA), y = 3:4)
  out <- join_waves(list(a, b), c("a", "b"), c("t1", "t2"), "id")
  expect_true(is.na(out$result$y_t2[2]))
})

test_that("逐步合併的步驟名稱從第 1 步開始", {
  w3 <- tibble::tibble(release_id = 1:4, score = 1:4)
  out <- join_waves(
    list(w1, w2, w3), c("a", "b", "c"), c("t1", "t2", "t3"), "release_id"
  )
  expect_match(out$audit$steps$步驟[1], "^第 1 步：t1")
  expect_match(out$audit$steps$步驟[2], "前面的合併結果")
})
