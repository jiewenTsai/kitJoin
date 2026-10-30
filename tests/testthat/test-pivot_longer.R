wide <- tibble::tibble(id = 1:2, score_t1 = c(1, 2), score_t2 = c(NA, 4))

test_that("程式碼只寫和 tidyr 預設不同的參數（cols 一定寫）", {
  code <- pivot_longer_code(list(cols = c("score_t1", "score_t2")), "wide")
  expect_match(code, "cols = c\\(score_t1, score_t2\\)")
  expect_no_match(code, "names_to")
  expect_no_match(code, "values_drop_na")

  args <- list(cols_pattern = "^score_", names_to = "wave", names_prefix = "score_",
               values_drop_na = TRUE)
  code <- pivot_longer_code(args, "wide")
  expect_match(code, 'cols = matches\\("\\^score_"\\)')
  expect_match(code, 'names_to = "wave"')
  expect_match(code, 'names_prefix = "score_"')
  expect_match(code, "values_drop_na = TRUE")
  expect_no_match(code, "values_to")
})

test_that("程式碼跑出來和 pivot_longer_run() 一樣", {
  args <- list(cols = c("score_t1", "score_t2"), names_to = "wave", names_prefix = "score_",
               values_drop_na = TRUE)
  ran <- pivot_longer_run(wide, args)
  code <- paste0("library(tidyr)\n", pivot_longer_code(args, "wide"))
  env <- new.env()
  env$wide <- wide
  got <- eval(parse(text = code), envir = env)
  expect_equal(got, ran)
  expect_equal(ran$wave, c("t1", "t1", "t2"))
})

test_that("轉換前的檢查：沒選欄、撞名、好幾個 names_to 沒有 names_pattern", {
  expect_error(pivot_longer_run(wide, list(cols = character(0))), "請選")
  expect_error(pivot_longer_run(wide, list(cols = "score_t1", names_to = "id")), "撞名")
  expect_error(
    pivot_longer_run(wide, list(cols = "score_t1", names_to = c("a", "b"))),
    "names_pattern"
  )
  mixed <- tibble::tibble(id = 1, a = 1, b = "x")
  expect_error(pivot_longer_run(mixed, list(cols = c("a", "b"))), "寬轉長沒有成功")
})

test_that("模組：控制項對應參數；固定的參數不能改", {
  shiny::testServer(
    mod_pivot_longer_server,
    args = list(data = shiny::reactive(wide), fixed = list(values_to = "score")),
    {
      session$setInputs(cols_mode = "list", cols = c("score_t1", "score_t2"),
                        names_to = "wave", names_prefix = "score_", values_drop_na = TRUE)
      args <- session$returned()
      expect_equal(args$cols, c("score_t1", "score_t2"))
      expect_equal(args$names_to, "wave")
      expect_equal(args$values_to, "score")
      expect_true(args$values_drop_na)
      expect_null(args$names_pattern)
      expect_match(output$locked_values_to, "score")

      session$setInputs(cols_mode = "pattern", cols_pattern = "_t")
      expect_equal(session$returned()$cols_pattern, "_t")
      expect_equal(nrow(pivot_longer_run(wide, session$returned())), 3)
    }
  )
})

test_that("模組：固定的 cols 可以是 reactive", {
  cols <- shiny::reactiveVal("score_t1")
  shiny::testServer(
    mod_pivot_longer_server,
    args = list(data = shiny::reactive(NULL), fixed = list(cols = cols)),
    {
      expect_equal(session$returned()$cols, "score_t1")
      cols(c("score_t1", "score_t2"))
      expect_equal(session$returned()$cols, c("score_t1", "score_t2"))
      expect_match(output$locked_cols, "score_t2")
    }
  )
})
