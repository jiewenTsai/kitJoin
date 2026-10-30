wide <- tibble::tibble(id = 1:2, score_t1 = c(1, 2), score_t2 = c(NA, 4))

test_that("程式碼只寫和 tidyr 預設不同的參數（cols 一定寫）", {
  code <- pivot_longer_code(list(cols = c("score_t1", "score_t2")), "wide")
  expect_match(code, "cols = c\\(score_t1, score_t2\\)")
  expect_no_match(code, "names_to")
  expect_no_match(code, "values_drop_na")

  args <- list(cols = "id", cols_exclude = TRUE, names_to = "wave", names_prefix = "score_",
               values_drop_na = TRUE)
  code <- pivot_longer_code(args, "wide")
  expect_match(code, "cols = !id,")
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
      session$setInputs(cols_mode = "include", cols = c("score_t2", "score_t1"),
                        names_to = "wave", names_prefix = "score_", values_drop_na = TRUE)
      args <- session$returned()
      # 點選的順序不管，照資料的欄位順序
      expect_equal(args$cols, c("score_t1", "score_t2"))
      expect_false(args$cols_exclude)
      expect_equal(args$names_to, "wave")
      expect_equal(args$values_to, "score")
      expect_true(args$values_drop_na)
      expect_null(args$names_pattern)
      expect_match(output$locked_values_to, "score")

      # 挑不轉的欄：其餘全部轉
      session$setInputs(cols_mode = "exclude", cols = "id")
      expect_true(session$returned()$cols_exclude)
      expect_equal(session$returned()$cols, "id")
      expect_equal(nrow(pivot_longer_run(wide, session$returned())), 3)
      expect_match(output$cols_count$html, "會轉 2 欄")
      expect_match(output$cols_quick$html, "score_ 開頭（2 欄）")
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
      expect_false(session$returned()$cols_exclude)
      cols(c("score_t1", "score_t2"))
      expect_equal(session$returned()$cols, c("score_t1", "score_t2"))
      expect_match(output$locked_cols, "score_t2")
    }
  )
})

# ── 點選 cols：分組、快速選欄、程式碼 ─────────────────────────────────────

wide2 <- tibble::tibble(
  id = 1:3, sex = c(1, 2, 1), age_m = c(12, 24, 36),
  score_lan = c(1, 2, 3), score_cog = c(4, 5, 6), score_mot = c(7, NA, 9),
  lan01 = c(0, 1, 1), lan02 = c(1, 1, 0), lan03 = c(1, 0, NA)
)

# 依程式碼在乾淨的環境跑一次
eval_code <- function(code, data) {
  env <- new.env()
  env$wide2 <- data
  eval(parse(text = code), envir = env)
  env$long
}

test_that("欄名分組：開頭相同的欄，2 欄以上，欄多的在前", {
  g <- pivot_name_groups(names(wide2))
  expect_equal(g$prefix, c("score_", "lan"))
  expect_equal(g$n, c(3L, 3L))
  expect_equal(g$cols[[1]], c("score_lan", "score_cog", "score_mot"))
  expect_equal(g$cols[[2]], c("lan01", "lan02", "lan03"))

  # 數字前的字、到第一個 _ 為止，取先到的；不分大小寫；只有一欄的不算
  g <- pivot_name_groups(c("id", "Q1a", "q2b", "x_1", "x_2", "w1_a", "age_m"))
  expect_equal(g$prefix, c("Q", "x_"))
  expect_equal(g$cols[[1]], c("Q1a", "q2b"))

  # 同樣成員的組只留一個；最多 max_groups 組；沒有組時是空的 tibble
  expect_equal(pivot_name_groups(c("ab_1", "ab_2"))$prefix, "ab_")
  many <- paste0(rep(letters[1:10], each = 2), rep(1:2, 10))
  expect_equal(nrow(pivot_name_groups(many)), 8)
  expect_equal(nrow(pivot_name_groups(c("id", "sex"))), 0)
  expect_equal(nrow(pivot_name_groups(character(0))), 0)
})

test_that("快速選欄：全選、清除、加入一組（照資料的欄位順序）", {
  all <- names(wide2)
  g <- pivot_name_groups(all)
  expect_equal(pivot_quick_pick("all", "id", all, g), all)
  expect_equal(pivot_quick_pick("none", "id", all, g), character(0))
  expect_equal(
    pivot_quick_pick("g2", "score_mot", all, g),
    c("score_mot", "lan01", "lan02", "lan03")
  )
  expect_equal(pivot_quick_pick("g9", "id", all, g), "id")
})

test_that("選欄的效果：會轉幾欄、保留幾欄", {
  all <- names(wide2)
  txt <- pivot_cols_count_text
  expect_match(txt(all, c("id", "sex"), TRUE), "會轉 7 欄")
  expect_match(txt(all, c("id", "sex"), TRUE), "保留 2 欄不動（id、sex）")
  expect_match(txt(all, "lan01", FALSE), "會轉 1 欄（lan01）")
  expect_match(txt(all, NULL, FALSE), "還沒選")
  expect_match(txt(all, all, TRUE), "沒有欄可以轉")
})

test_that("挑要轉的欄 / 挑不轉的欄：轉出來一樣", {
  conv <- c("score_lan", "score_cog", "score_mot")
  inc <- pivot_longer_run(wide2, list(cols = conv))
  exc <- pivot_longer_run(
    wide2, list(cols = c("id", "sex", "age_m", "lan01", "lan02", "lan03"), cols_exclude = TRUE)
  )
  expect_equal(inc, exc)
  expect_equal(dim(inc), c(9, 8))
  expect_equal(pivot_longer_cols(wide2, list(cols = "id", cols_exclude = TRUE)), names(wide2)[-1])
  # 不轉的欄裡有資料沒有的欄：不影響
  expect_equal(pivot_longer_cols(wide2, list(cols = c("id", "zz"), cols_exclude = TRUE)),
               names(wide2)[-1])
})

test_that("cols 的程式碼：!c()、starts_with()、逐欄列出", {
  all <- names(wide2)
  code <- function(cols, exclude = FALSE) {
    pivot_cols_code(list(cols = cols, cols_exclude = exclude), all)
  }
  expect_equal(code(c("id", "sex"), TRUE), "!c(id, sex)")
  expect_equal(code("id", TRUE), "!id")
  expect_equal(code(character(0), TRUE), "everything()")
  expect_equal(code(c("score_lan", "score_cog", "score_mot")), 'starts_with("score_")')
  expect_equal(
    code(c("score_lan", "score_cog", "score_mot", "lan01", "lan02", "lan03")),
    'c(starts_with("score_"), starts_with("lan"))'
  )
  # 不是整組：逐欄列出
  expect_equal(code(c("score_lan", "score_cog")), "c(score_lan, score_cog)")
  # starts_with("lan") 會多選到別的欄（language）時不用 starts_with()
  with_lang <- c(all, "language")
  expect_equal(
    pivot_cols_code(list(cols = c("lan01", "lan02", "lan03")), with_lang),
    "c(lan01, lan02, lan03)"
  )
  # 兩組交錯排列：starts_with() 的順序會不同，逐欄列出
  mixed <- c("id", "a1", "b1", "a2", "b2")
  expect_equal(
    pivot_cols_code(list(cols = c("a1", "b1", "a2", "b2")), mixed),
    "c(a1, b1, a2, b2)"
  )
  # 沒有欄名資訊時逐欄列出
  expect_equal(
    pivot_cols_code(list(cols = c("score_lan", "score_cog", "score_mot"))),
    "c(score_lan, score_cog, score_mot)"
  )
})

test_that("產生的程式碼跑出來和 app 一樣（三種 cols 寫法）", {
  all <- names(wide2)
  cases <- list(
    list(cols = c("id", "sex", "age_m"), cols_exclude = TRUE, values_drop_na = TRUE),
    list(cols = c("score_lan", "score_cog", "score_mot"), names_to = "domain",
         names_prefix = "score_"),
    list(cols = c("score_lan", "score_cog", "score_mot", "lan01", "lan02", "lan03")),
    list(cols = c("score_cog", "lan02"))
  )
  for (args in cases) {
    ran <- pivot_longer_run(wide2, args)
    code <- code_longer(args, "wide2", all)$tidy
    expect_equal(eval_code(code, wide2), ran)
  }
  code <- code_longer(cases[[1]], "wide2", all)$tidy
  expect_match(code, "cols = !c\\(id, sex, age_m\\)")
  expect_match(code, "# cols = !c")
  expect_match(code_longer(cases[[2]], "wide2", all)$tidy, 'cols = starts_with\\("score_"\\)')
})

test_that("模組：分組按鈕、選欄效果；挑要轉的和挑不轉的結果一樣", {
  shiny::testServer(
    mod_pivot_longer_server,
    args = list(data = shiny::reactive(wide2)),
    {
      session$setInputs(cols_mode = "include", cols = c("lan01", "lan02", "lan03"))
      expect_match(output$cols_quick$html, "score_ 開頭（3 欄）")
      expect_match(output$cols_quick$html, "lan 開頭（3 欄）")
      expect_match(output$cols_count$html, "保留 6 欄不動")
      args <- session$returned()
      expect_equal(
        nrow(pivot_longer_run(wide2, args)),
        nrow(pivot_longer_run(wide2, list(cols = setdiff(names(wide2), args$cols),
                                          cols_exclude = TRUE)))
      )
    }
  )
})

test_that("⑥ 的用法：cols 固定、names_prefix / names_pattern 固定成 NULL", {
  items <- shiny::reactiveVal(c("lan01", "lan02", "lan03"))
  shiny::testServer(
    mod_pivot_longer_server,
    args = list(
      data = shiny::reactive(NULL),
      fixed = list(
        cols = items, names_to = "item_id", values_to = "item_score",
        names_prefix = NULL, names_pattern = NULL, values_drop_na = TRUE
      )
    ),
    {
      args <- session$returned()
      expect_false(args$cols_exclude)
      out <- pivot_longer_run(wide2, args)
      expect_equal(nrow(out), 8)
      expect_equal(unique(out$item_id), c("lan01", "lan02", "lan03"))
      code <- pivot_longer_code(args, "", cols_code = "any_of(items)")
      expect_match(code, "cols = any_of\\(items\\)")
      expect_match(output$locked_cols, "lan03")
    }
  )
})
