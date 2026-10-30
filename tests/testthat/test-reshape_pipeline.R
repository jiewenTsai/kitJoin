scores <- sim_kitsui_scores()
preset <- kitsui_score_preset(names(scores))

test_that("kitsUI 分數表預設：只取資料中有的 id 欄", {
  expect_true(is_kitsui_score_table(names(scores)))
  expect_equal(
    preset$id_cols,
    c("visit_id", "subj_id", "cohort", "wave_m", "age_m", "sex", "round")
  )
  expect_equal(preset$values_from, c("d_score", "d_score_se", "scale"))
  expect_equal(preset$names_glue, "{.value}_{domain}")
  expect_equal(preset$names_vary, "slowest")

  no_sex <- kitsui_score_preset(setdiff(names(scores), c("sex", "subj_id")))
  expect_false("sex" %in% no_sex$id_cols)
  expect_null(kitsui_score_preset(setdiff(names(scores), "bank")))
})

test_that("套用預設後：每次施測一列，各領域依 slowest 並排", {
  out <- reshape_wider(scores, preset)
  expect_equal(nrow(out$result), 6)
  expect_equal(
    names(out$result)[-(1:7)],
    c("d_score_lan", "d_score_se_lan", "scale_lan",
      "d_score_motor", "d_score_se_motor", "scale_motor")
  )
  expect_length(out$warnings, 0)
  expect_match(out$summary[1], "11 列 × 17 欄.*6 列 × 13 欄")
})

test_that("d_score 空白分得出：沒有這一列 vs 有列但空白", {
  out <- reshape_wider(scores, preset)
  wide <- out$result
  no_row <- wide[wide$visit_id == "M03_002_24", ]
  not_d <- wide[wide$visit_id == "M36_011_48", ]
  expect_true(is.na(no_row$d_score_motor) && is.na(no_row$scale_motor))
  expect_true(is.na(not_d$d_score_motor))
  expect_equal(not_d$scale_motor, "motor M36")

  motor <- out$empty[out$empty$domain == "motor", ]
  expect_equal(motor$轉換後列數, 6)
  expect_equal(motor$沒有這一列, 1)
  expect_equal(motor$`有列但空白：d_score`, 2)
  expect_equal(motor$`有列但空白：scale`, 0)
})

test_that("check_key_unique：id × names_from 重複時列出重複列", {
  doubled <- dplyr::bind_rows(scores, scores[1, ])
  dup <- check_key_unique(doubled, c("visit_id", "domain"))
  expect_false(dup$ok)
  expect_equal(dup$n_keys, 1)
  expect_equal(dup$rows$列次, c(1, 12))
  expect_equal(unique(dup$rows$重複次數), 2)
  expect_true(check_key_unique(scores, c("visit_id", "domain"))$ok)
})

test_that("重複時停下來，不彙總、不產生 list 欄", {
  doubled <- dplyr::bind_rows(scores, scores[1, ])
  expect_error(
    reshape_wider(doubled, preset),
    "不只一次",
    class = "kit_user_error"
  )
})

test_that("check_id_constant：同一個 visit 的 round 不同會被拆成兩列", {
  mixed <- scores
  mixed$round[mixed$visit_id == "M03_001_12" & mixed$domain == "motor"] <- 2L
  chk <- check_id_constant(mixed, "visit_id", preset$id_cols)
  expect_false(chk$ok)
  expect_equal(chk$cols, "round")
  expect_equal(chk$n_ids, 1)
  expect_equal(chk$rows$列次, c(1, 2))

  out <- reshape_wider(mixed, preset)
  expect_equal(nrow(out$result), 7)
  expect_match(out$warnings, "round.*visit_id.*values_from")

  ok <- check_id_constant(scores, "visit_id", preset$id_cols)
  expect_true(ok$ok)
})

test_that("把 round 移到 values_from 後，一次施測回到一列", {
  mixed <- scores
  mixed$round[mixed$visit_id == "M03_001_12" & mixed$domain == "motor"] <- 2L
  args <- preset
  args$id_cols <- setdiff(args$id_cols, "round")
  args$values_from <- c(args$values_from, "round")
  out <- reshape_wider(mixed, args)
  expect_equal(nrow(out$result), 6)
  expect_equal(out$result$round_motor[1], 2L)
})

test_that("新欄名和 id 欄同名時停下來", {
  df <- tibble::tibble(id = 1:2, k = c("a", "a"), x = 1:2, x_a = 3:4)
  args <- wider_args(
    id_cols = c("id", "x_a"), names_from = "k", values_from = "x", names_prefix = "x_"
  )
  expect_equal(check_wider_names(df, args)$collide, "x_a")
  expect_error(reshape_wider(df, args), "同名", class = "kit_user_error")
})

test_that("新欄名不符合 SPSS 規則時提醒", {
  df <- tibble::tibble(id = 1:2, k = c("1st", "2nd"), x = 1:2)
  args <- wider_args(names_from = "k", values_from = "x", names_glue = "{k}")
  nm <- check_wider_names(df, args)
  expect_equal(unname(nm$sav), c("v_1st", "v_2nd"))
  out <- reshape_wider(df, args)
  expect_match(out$warnings, "SPSS")
})

test_that("id_cols 預設同 tidyr：其餘全部欄位", {
  df <- tibble::tibble(id = c(1, 1, 2), k = c("a", "b", "a"), x = 1:3, y = 4:6)
  args <- wider_args(names_from = "k", values_from = c("x", "y"))
  expect_equal(wider_id_cols(df, args), "id")
  out <- reshape_wider(df, args)
  expect_named(out$result, c("id", "x_a", "x_b", "y_a", "y_b"))
})

test_that("values_fill：數字只填數值欄，文字只填文字欄", {
  expect_null(parse_values_fill("", scores, "d_score"))
  expect_equal(parse_values_fill("0", scores, c("d_score", "d_score_se")), 0)
  expect_equal(
    parse_values_fill("0", scores, c("d_score", "scale")),
    list(d_score = 0)
  )
  expect_equal(parse_values_fill("none", scores, c("d_score", "scale")), list(scale = "none"))

  args <- preset
  args$values_fill <- "0"
  out <- reshape_wider(scores, args)
  row <- out$result[out$result$visit_id == "M03_002_24", ]
  expect_equal(row$d_score_motor, 0)
  expect_true(is.na(row$scale_motor))
  not_d <- out$result[out$result$visit_id == "M36_011_48", ]
  expect_true(is.na(not_d$d_score_motor))
})

test_that("names_sep 與 names_prefix", {
  df <- tibble::tibble(id = c(1, 1), k = c("a", "b"), x = 1:2)
  args <- wider_args(names_from = "k", values_from = "x", names_prefix = "w")
  expect_named(reshape_wider(df, args)$result, c("id", "wa", "wb"))
  df$y <- 3:4
  args <- wider_args(names_from = "k", values_from = c("x", "y"), names_sep = ".")
  expect_named(reshape_wider(df, args)$result, c("id", "x.a", "x.b", "y.a", "y.b"))
})

test_that("pivot_longer：基本轉換與檢查", {
  wide <- tibble::tibble(id = 1:2, score_t1 = c(1, NA), score_t2 = 3:4)
  out <- reshape_longer(wide, longer_args(cols = c("score_t1", "score_t2")))
  expect_equal(dim(out$result), c(4, 3))
  expect_named(out$result, c("id", "name", "value"))
  expect_match(out$summary[2], "每一列變成 2 列")
  # 欄名規則（cols_pattern）和 ⑥ 共用
  out <- reshape_longer(wide, list(cols_pattern = "^score_"))
  expect_equal(dim(out$result), c(4, 3))

  args <- longer_args(
    cols = c("score_t1", "score_t2"), names_to = c(".value", "wave"),
    names_pattern = "(.*)_(t[0-9])", values_drop_na = TRUE
  )
  out <- reshape_longer(wide, args)
  expect_named(out$result, c("id", "wave", "score"))
  expect_equal(nrow(out$result), 3)

  args <- longer_args(cols = "score_t1", names_to = "id")
  expect_error(reshape_longer(wide, args), "撞名", class = "kit_user_error")
  mixed <- tibble::tibble(id = 1, a = 1, b = "x")
  expect_error(
    reshape_longer(mixed, longer_args(cols = c("a", "b"))),
    "型別不同",
    class = "kit_user_error"
  )
})

test_that("教學程式碼只寫和預設不同的參數", {
  code <- code_wider(preset, scores, "data_t1")$tidy
  expect_match(code, "id_cols = c\\(visit_id, subj_id")
  expect_match(code, "names_glue = \"\\{.value\\}_\\{domain\\}\"")
  expect_match(code, "names_vary = \"slowest\"")
  expect_no_match(code, "names_sep|names_prefix|values_fill")

  plain <- wider_args(names_from = "domain", values_from = "d_score")
  code <- code_wider(plain, scores, "data_t1")$tidy
  expect_no_match(code, "id_cols =|names_vary|names_glue")
  expect_match(code, "names_from = domain")

  code <- code_longer(longer_args(cols = c("a", "b")), "data_t2")$tidy
  expect_match(code, "pivot_longer\\(\n    cols = c\\(a, b\\)\n  \\)")
  args <- longer_args(cols = "a", values_to = "score", values_drop_na = TRUE)
  code <- code_longer(args, "data_t2")$tidy
  expect_match(code, "values_to = \"score\"")
  expect_match(code, "values_drop_na = TRUE")
})

test_that("教學程式碼執行結果和 app 相同", {
  code <- code_wider(preset, scores, "data_t1")$tidy
  code <- gsub("library\\([a-z]+\\)", "", code)
  env <- list2env(list(
    data_t1 = scores, count = dplyr::count, filter = dplyr::filter,
    pivot_wider = tidyr::pivot_wider
  ))
  eval(parse(text = code), envir = env)
  expect_equal(env$wide, reshape_wider(scores, preset)$result)
})
