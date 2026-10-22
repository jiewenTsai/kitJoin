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
