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

test_that("6 位數以上的數字 ID 轉文字不會變成科學記號（A5）", {
  expect_equal(num_to_text(c(100000, 200000, 1234567, NA, 1.5)),
               c("100000", "200000", "1234567", NA, "1.5"))
  a <- tibble::tibble(id = c(100000, 200000, 1234567), x = 1:3)
  b <- tibble::tibble(id = c("100000", "200000", "1234567"), y = 4:6)
  out <- join_waves(list(a, b), c("a", "b"), c("t1", "t2"), "id", "inner_join")
  expect_equal(nrow(out$result), 3)
  expect_equal(out$result$id, c("100000", "200000", "1234567"))
  att <- build_attrition(a, b, "id")
  expect_equal(att$result$attrition, c(0L, 0L, 0L))
  expect_equal(key_strings(a, "id"), c("100000", "200000", "1234567"))
})

test_that("文字 ID 前後空白一律去掉，只有空白的 ID 不配對", {
  a <- tibble::tibble(id = c(" 1", "2", " "), x = 1:3)
  b <- tibble::tibble(id = c("1", "2 ", ""), y = 4:6)
  out <- join_waves(list(a, b), c("a", "b"), c("t1", "t2"), "id", "inner_join")
  expect_equal(out$result$id, c("1", "2"))
  att <- build_attrition(a, b, "id")
  expect_equal(att$result$attrition, c(0L, 0L, NA))
})

test_that("多對多會暴增時先停下，確認後才執行；太大直接拒絕（A3）", {
  a <- tibble::tibble(birth = rep(1:3, each = 20), x = 1:60)
  b <- tibble::tibble(birth = rep(1:3, each = 20), y = 1:60)
  expect_equal(join_size_estimate(list(a, b), "birth", "left_join"), 1200)
  expect_error(
    join_waves(list(a, b), c("a", "b"), c("t1", "t2"), "birth"),
    "1,200 列",
    class = "kit_join_too_large"
  )
  out <- join_waves(list(a, b), c("a", "b"), c("t1", "t2"), "birth", confirm_large = TRUE)
  expect_equal(nrow(out$result), 1200)

  # 預估：left 保留左邊沒配到的與空白 ID；full 兩邊都保留
  l <- tibble::tibble(id = c(1, 1, 2, NA))
  r <- tibble::tibble(id = c(1, 3, NA))
  expect_equal(join_size_estimate(list(l, r), "id", "left_join"), 4)
  expect_equal(join_size_estimate(list(l, r), "id", "inner_join"), 2)
  expect_equal(join_size_estimate(list(l, r), "id", "full_join"), 6)

  # 正常的少量重複不用確認
  w2_dup <- dplyr::bind_rows(w2, tibble::tibble(release_id = 2L, score = 99))
  expect_no_error(join_waves(list(w1, w2_dup), c("a", "b"), c("t1", "t2"), "release_id"))

  big <- tibble::tibble(k = rep(1, 2e4), x = 1)
  expect_error(
    join_waves(list(big, big), c("a", "b"), c("t1", "t2"), "k", confirm_large = TRUE),
    "太大",
    class = "kit_user_error"
  )
})

test_that("ID 選單附不重複比例，像 ID 的欄排前面（A3）", {
  a <- tibble::tibble(c_birth = c(1, 1, 2, 2), w1_id = 1:4, W = c(1, 1, 1, 2))
  ch <- id_choices(list(a), names(a))
  expect_equal(unname(ch), c("w1_id", "c_birth", "W"))
  expect_equal(names(ch)[1], "w1_id（不重複 100%）")
  expect_equal(names(ch)[2], "c_birth（不重複 50%）")
  expect_equal(default_id_choice(ch), "w1_id")
  expect_equal(format_unique_pct(0.99999), "99.9%")
  tbl <- id_check_table(list(a, a), c("t1", "t2"), "c_birth")
  expect_equal(tbl$重複的ID數, c(2L, 2L))
})

test_that("檔案只有 ID 一欄時也能串接", {
  out <- join_waves(
    list(tibble::tibble(id = 1:3, x = 1:3), tibble::tibble(id = 2:3)),
    c("a", "b"), c("t1", "t2"), "id"
  )
  expect_named(out$result, c("id", "x_t1"))
})

test_that("各檔用自己的 ID 欄（w1_id、w2_id、w3id），改名成共同的 ID 再串接（C1）", {
  a <- tibble::tibble(w1_id = 1:4, x = 1:4)
  b <- tibble::tibble(w2_id = 2:5, x = 5:8)
  c3 <- tibble::tibble(w3id = c("3", "4", "9"), x = 9:11)
  out <- join_waves(
    list(a, b, c3), c("w1.sav", "w2.sav", "w3.sav"), c("t1", "t2", "t3"),
    by_vars = "w1_id", method = "full_join",
    id_cols = list("w1_id", "w2_id", "w3id")
  )
  expect_named(out$result, c("w1_id", "x_t1", "x_t2", "x_t3"))
  expect_equal(nrow(out$result), 6)
  expect_equal(out$result$x_t3[out$result$w1_id == "4"], 10)
  expect_match(out$notes, "w2_id", all = FALSE)
  expect_match(out$notes, "t3（w3.sav）的 w3id 是文字", all = FALSE)

  expect_error(
    join_waves(list(a, b), c("a", "b"), c("t1", "t2"), "w1_id",
               id_cols = list("w1_id", c("w2_id", "x"))),
    "一樣多個 ID 欄", class = "kit_user_error"
  )
  expect_error(
    join_waves(list(a, b), c("a", "b"), c("t1", "t2"), "w1_id",
               id_cols = list("w1_id", "w9_id")),
    "沒有 w9_id", class = "kit_user_error"
  )
  # 後面的檔已經有一個同名、但不是 ID 的欄
  b2 <- tibble::tibble(w2_id = 2:5, w1_id = 0)
  expect_error(
    join_waves(list(a, b2), c("a", "b"), c("t1", "t2"), "w1_id",
               id_cols = list("w1_id", "w2_id")),
    "不是這個檔案的 ID", class = "kit_user_error"
  )
  tbl <- id_check_table(list(a, b), c("t1", "t2"), "w1_id", list("w1_id", "w2_id"))
  expect_equal(tbl$ID欄, c("w1_id", "w2_id"))
})

test_that("TDCYP jsq w1–w5：各檔 ID 欄名不同也能串接（C1）", {
  dir <- "/Users/jiewentsai/Documents/ClaudePlayMac/lab20261030_kitsui/lab1031/tdcyp_data"
  skip_if_not(dir.exists(dir))
  files <- sprintf("w%d_jsq.sav", 1:5)
  ids <- list("w1_id", "w2_id", "w3id", "w4id", "w5_id")
  dfs <- purrr::map2(files, ids, function(f, id) {
    df <- read_data_file(file.path(dir, f), f)$data
    df[c(id, setdiff(names(df), id)[1:5])]
  })
  out <- join_waves(dfs, files, paste0("t", 1:5), "w1_id", "inner_join", id_cols = ids)
  expect_equal(nrow(out$result), 688)   # 5 波都有資料的人數（報告的實測值）
  expect_equal(anyDuplicated(out$result$w1_id), 0)
})
