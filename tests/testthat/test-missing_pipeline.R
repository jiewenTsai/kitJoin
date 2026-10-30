test_that("ID 欄位的猜測不會誤抓 idea_score", {
  expect_equal(
    guess_id_cols(c(
      "id", "release_id", "IDNO", "id_2", "idea_score", "child.id", "valid"
    )),
    c("id", "release_id", "IDNO", "id_2", "child.id")
  )
})

test_that("計算遺漏值代碼格數時略過 ID 欄", {
  df <- tibble::tibble(id = c(9999, 1), x = c(9999, 7777), y = c("9999", "a"))
  expect_equal(count_code_cells(list(df)), 2)
})

test_that("值標籤全是遺漏碼時，轉換後不再是沒有標籤的 labelled，預覽不會出錯（B3）", {
  x <- haven::labelled(c(26, 999, 30), c(遺漏值 = 999), label = "居住地區")
  df <- tibble::tibble(id = 1:3, h_area = x)
  out <- replace_na_codes(df, 999)$data
  expect_false(haven::is.labelled(out$h_area))
  expect_equal(attr(out$h_area, "label"), "居住地區")
  expect_equal(as.vector(out$h_area), c(26, NA, 30))
  expect_no_error(shown <- df_for_display(out))
  expect_equal(shown$h_area, c("26", NA, "30"))
  # 舊資料裡已經是 labels = NULL 的 labelled 也能預覽
  bad <- x
  attr(bad, "labels") <- NULL
  expect_no_error(df_for_display(tibble::tibble(v = bad)))
})

test_that("代碼不在資料裡時，代碼的數值標籤也會移除；其他標籤保留", {
  x <- haven::labelled(c(1, 2), c(是 = 1, 否 = 2, 遺漏值 = 99))
  out <- replace_na_codes(tibble::tibble(x = x), 99)
  expect_equal(attr(out$data$x, "labels"), c(是 = 1, 否 = 2))
  expect_null(out$detail)
})
