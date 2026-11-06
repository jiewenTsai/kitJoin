# 篩選與摘要：處理邏輯（filter_pipeline.R）

filter_df <- function() {
  tibble::tibble(
    id = 1:8,
    sex = haven::labelled(c(1, 2, 1, NA, 2, 1, 2, 1), c(男 = 1, 女 = 2), label = "性別"),
    score = c(10, 20, NA, 40, 50, 60, NA, 80),
    city = c("臺北市", "新北市", "臺北市", NA, "臺中市", "a\"b", "臺北縣", "高雄市"),
    grp = factor(c("A", "B", "A", "B", NA, "A", "B", "A"))
  )
}

# 一個條件的設定 → 篩選結果
run_cond <- function(df, ..., combine = "and", conds = list(list(...))) {
  spec <- filter_spec_clean(list(conds = conds, combine = combine), df)
  expect_length(spec$problems, 0)
  filter_apply(df, spec)
}

test_that("數字的每個運算子：結果和 dplyr::filter() 相同，空白判斷不了的列另外算", {
  df <- filter_df()
  cases <- list(
    list(op = "eq", num = 20, expect = 2L),
    list(op = "ne", num = 20, expect = c(1L, 4L, 5L, 6L, 8L)),
    list(op = "gt", num = 40, expect = c(5L, 6L, 8L)),
    list(op = "ge", num = 40, expect = c(4L, 5L, 6L, 8L)),
    list(op = "lt", num = 20, expect = 1L),
    list(op = "le", num = 20, expect = 1:2),
    list(op = "is_na", expect = c(3L, 7L)),
    list(op = "not_na", expect = c(1L, 2L, 4L, 5L, 6L, 8L))
  )
  for (cs in cases) {
    res <- run_cond(df, col = "score", op = cs$op, num = cs$num)
    expect_equal(res$data$id, cs$expect, info = cs$op)
    na_expected <- if (cs$op %in% c("is_na", "not_na")) 0L else 2L
    expect_equal(res$counts$excluded_na, na_expected, info = cs$op)
    expect_equal(res$counts$excluded, 8L - length(cs$expect), info = cs$op)
  }
  # 介於：含兩端，兩個數字填反了也可以
  res <- run_cond(df, col = "score", op = "between", lo = 50, hi = 20)
  expect_equal(res$data$id, c(2L, 4L, 5L))
  expect_equal(res$counts$excluded_na, 2L)
})

test_that("文字、類別、有數值標籤的欄：是其中之一、不是其中之一、包含文字", {
  df <- filter_df()
  # 有數值標籤：選項寫成「1 = 男」，比對的是存的值
  expect_equal(unname(filter_value_choices(df$sex)), c("1", "2"))
  expect_equal(names(filter_value_choices(df$sex)), c("1 = 男", "2 = 女"))
  res <- run_cond(df, col = "sex", op = "in", values = "2")
  expect_equal(res$data$id, c(2L, 5L, 7L))
  # %in%：空白不屬於任何一個值，所以「不是其中之一」留下空白的列
  res <- run_cond(df, col = "sex", op = "not_in", values = "2")
  expect_equal(res$data$id, c(1L, 3L, 4L, 6L, 8L))
  expect_equal(res$counts$excluded_na, 0L)
  # 有數值標籤的數字也可以比大小
  expect_equal(run_cond(df, col = "sex", op = "gt", num = 1)$data$id, c(2L, 5L, 7L))

  res <- run_cond(df, col = "city", op = "in", values = c("臺北市", "a\"b"))
  expect_equal(res$data$id, c(1L, 3L, 6L))
  res <- run_cond(df, col = "grp", op = "not_in", values = "A")
  expect_equal(res$data$id, c(2L, 4L, 5L, 7L))
  # 包含文字：照字面，不是正規表示式（"." 不是萬用字元）
  expect_equal(run_cond(df, col = "city", op = "contains", text = "臺北")$data$id, c(1L, 3L, 7L))
  expect_equal(nrow(run_cond(df, col = "city", op = "contains", text = ".")$data), 0)
  expect_equal(run_cond(df, col = "city", op = "contains", text = "\"")$data$id, 6L)
})

test_that("值太多（像 ID）時不列出選項，改成自己輸入一個值", {
  df <- tibble::tibble(pid = sprintf("K%04d", 1:300), x = 1:300)
  expect_null(filter_value_choices(df$pid))
  ops <- filter_ops_for(filter_col_kind(df$pid), many = TRUE)
  expect_true("等於（自己輸入一個值）" %in% names(ops))
  res <- run_cond(df, col = "pid", op = "in", text = "K0007")
  expect_equal(res$data$x, 7L)
  # 選了值卻沒用到（選項框不在畫面上）也不會被拿來比
  spec <- filter_spec_clean(list(conds = list(list(col = "pid", op = "in", values = "K0001"))), df)
  expect_match(spec$problems, "還沒輸入值")
})

test_that("日期與 TRUE/FALSE 欄當成文字比對", {
  df <- tibble::tibble(
    d = as.Date(c("2020-01-01", "2021-05-06", NA)), ok = c(TRUE, FALSE, NA), id = 1:3
  )
  expect_equal(filter_col_kind(df$d), "text_other")
  expect_equal(run_cond(df, col = "d", op = "in", values = "2021-05-06")$data$id, 2L)
  expect_equal(run_cond(df, col = "ok", op = "in", values = "TRUE")$data$id, 1L)
  expect_equal(run_cond(df, col = "d", op = "contains", text = "2020")$data$id, 1L)
})

test_that("全部符合（AND）與任一符合（OR）；NA 照 dplyr 的規則", {
  df <- filter_df()
  conds <- list(
    list(col = "score", op = "ge", num = 40),
    list(col = "sex", op = "in", values = "1")
  )
  and <- run_cond(df, conds = conds, combine = "and")
  expect_equal(and$data$id, c(6L, 8L))
  expect_equal(and$data, dplyr::filter(df, score >= 40, sex %in% 1))
  or <- run_cond(df, conds = conds, combine = "or")
  expect_equal(or$data, dplyr::filter(df, score >= 40 | sex %in% 1))
  # OR：score 空白但 sex = 1 的列（3）留下；score 空白、sex 不是 1 的列（7）判斷不了
  expect_equal(or$data$id, c(1L, 3L, 4L, 5L, 6L, 8L))
  expect_equal(or$counts$excluded_na, 1L)
  expect_equal(or$conds[["判斷不了（空白）"]], c(2L, 0L))
  expect_equal(or$conds[["符合"]], c(4L, 4L))
  # 要留下空白的列：加「是空白」再選任一符合
  keep_blank <- run_cond(df, conds = list(
    list(col = "score", op = "gt", num = 45), list(col = "score", op = "is_na")
  ), combine = "or")
  expect_equal(keep_blank$data$id, c(3L, 5L, 6L, 7L, 8L))
  expect_equal(keep_blank$counts$excluded_na, 0L)
})

test_that("保留哪些欄：挑要保留的、挑要拿掉的、沒挑＝全部保留；排序在選欄之前", {
  df <- filter_df()
  keep <- filter_spec_clean(list(keep_mode = "include", keep_cols = c("score", "id")), df)
  expect_equal(keep$keep_cols, c("id", "score"))
  expect_equal(names(filter_apply(df, keep)$data), c("id", "score"))
  drop <- filter_spec_clean(list(keep_mode = "exclude", keep_cols = "city"), df)
  expect_equal(names(filter_apply(df, drop)$data), c("id", "sex", "score", "grp"))
  expect_equal(drop$drop_cols, "city")
  none <- filter_spec_clean(list(keep_mode = "include", keep_cols = character(0)), df)
  expect_null(none$keep_cols)
  expect_false(none$active)
  all_out <- filter_spec_clean(list(keep_mode = "exclude", keep_cols = names(df)), df)
  expect_match(all_out$problems, "至少要保留一欄")
  expect_equal(filter_keep_count_text(names(df), "exclude", "city"), "保留 4 欄，拿掉 1 欄（city）。")

  # 依拿掉的欄排序；desc() 由大到小，空白排最後
  spec <- filter_spec_clean(list(
    keep_mode = "include", keep_cols = "id", sort_cols = c("sex", "score"), sort_desc = "score"
  ), df)
  out <- filter_apply(df, spec)$data
  expect_equal(out$id, c(8L, 6L, 1L, 3L, 5L, 2L, 7L, 4L))
  expect_equal(out, dplyr::select(dplyr::arrange(df, sex, dplyr::desc(score)), id))
})

test_that("SPSS 的數值標籤與變項標籤留著（篩列、排序、選欄之後）", {
  df <- filter_df()
  attr(df$score, "label") <- "語言分數"
  spec <- filter_spec_clean(list(
    conds = list(list(col = "score", op = "gt", num = 15)),
    keep_mode = "exclude", keep_cols = "city", sort_cols = "score"
  ), df)
  out <- filter_apply(df, spec)$data
  expect_s3_class(out$sex, "haven_labelled")
  expect_equal(attr(out$sex, "labels"), c(男 = 1, 女 = 2))
  expect_equal(attr(out$sex, "label"), "性別")
  expect_equal(attr(out$score, "label"), "語言分數")
  # 寫成 SAV 再讀回來，標籤還在
  path <- withr::local_tempfile(fileext = ".sav")
  write_kit_sav(out, path)
  back <- haven::read_sav(path)
  expect_equal(attr(back$sex, "labels"), c(男 = 1, 女 = 2))
})

test_that("每個檔案各自設定；沒有設定的檔案原樣留著；沒填完時停下", {
  dfs <- list(filter_df(), tibble::tibble(id = 1:3, y = c(5, NA, 7)))
  specs <- list(
    NULL,
    filter_spec_clean(list(conds = list(list(col = "y", op = "gt", num = 1))), dfs[[2]])
  )
  out <- filter_files(dfs, specs, c("t1（a.csv）", "t2（b.csv）"))
  expect_identical(out$data[[1]], dfs[[1]])
  expect_equal(out$data[[2]]$id, c(1L, 3L))
  expect_equal(out$report[["有設定"]], c("否（原樣往後傳）", "是"))
  expect_equal(out$report[["保留列數"]], c(8L, 2L))
  expect_equal(out$report[["其中因為空白（判斷不了）而排除"]], c(0L, 1L))
  expect_match(out$warnings, "t2（b.csv）：有 1 列因為條件用到的欄是空白")
  expect_match(out$points[1], "沒有設定，原樣往後傳")
  expect_match(out$points[2], "3 列 → 保留 2 列（排除 1 列）")

  expect_error(filter_files(dfs, list(NULL, NULL), c("a", "b")), "還沒有設定")
  bad <- filter_spec_clean(list(conds = list(list(col = "y", op = "gt"))), dfs[[2]])
  expect_error(filter_files(dfs, list(NULL, bad), c("a", "b")), "b：條件 1（y）：還沒填數字")
  nocol <- filter_spec_clean(list(conds = list(list(col = ""))), dfs[[2]])
  expect_equal(nocol$problems, "條件 1：還沒選欄")
})

test_that("篩選後沒有任何列：不會出錯，狀態框說清楚", {
  df <- filter_df()
  spec <- filter_spec_clean(list(conds = list(list(col = "score", op = "gt", num = 1000))), df)
  out <- filter_files(list(df), list(spec), "t1")
  expect_equal(nrow(out$data[[1]]), 0)
  expect_equal(names(out$data[[1]]), names(df))
  expect_true(any(grepl("篩選後沒有任何列", out$warnings)))
  # 摘要也不會出錯：沒有列
  expect_equal(nrow(summary_table(out$data[[1]], "sex", "score")), 0)
})

test_that("摘要：一列 = 一組 × 一個變項；整組空白是 NA；數值標籤的分組寫成 [1] 男", {
  df <- filter_df()
  df$empty <- NA_real_
  tbl <- summary_table(df, "sex", c("score", "empty"), names(SUMMARY_STATS))
  expect_named(tbl, c("sex", "variable", "n", names(SUMMARY_STATS)))
  expect_equal(as.character(tbl$sex), rep(c("[1] 男", "[2] 女", NA), each = 2))
  expect_equal(tbl$variable, rep(c("score", "empty"), 3))
  expect_equal(tbl$n, c(4L, 4L, 3L, 3L, 1L, 1L))
  male <- tbl[tbl$sex %in% "[1] 男" & tbl$variable == "score", ]
  expect_equal(male$n_valid, 3L)
  expect_equal(male$n_missing, 1L)
  expect_equal(male$mean, mean(c(10, 60, 80)))
  expect_equal(male$sd, stats::sd(c(10, 60, 80)))
  expect_equal(male$median, 60)
  expect_equal(c(male$min, male$max), c(10, 80))
  empty <- tbl[tbl$variable == "empty", ]
  for (s in c("mean", "sd", "median", "min", "max")) expect_true(all(is.na(empty[[s]])), info = s)
  expect_false(any(is.nan(unlist(empty[c("mean", "median")]))))
  # 不分組；只算勾的統計量
  one <- summary_table(df, character(0), "score", c("mean"))
  expect_named(one, c("variable", "n", "mean"))
  expect_equal(one$mean, mean(df$score, na.rm = TRUE))
  # 交叉分組
  two <- summary_table(df, c("sex", "grp"), "score", "n_valid")
  expect_equal(nrow(two), nrow(dplyr::distinct(df, sex, grp)))
})

test_that("摘要：選錯欄時停下並說明", {
  df <- filter_df()
  expect_error(summary_table(df, "sex", character(0)), "請選要摘要的變項")
  expect_error(summary_table(df, "sex", "city"), "不是數字")
  expect_error(summary_table(dplyr::rename(df, n = grp), "n", "score"), "撞名")
  # 分組選到 ID：組數 × 變項數太多時停下
  wide <- tibble::as_tibble(c(list(id = 1:1100), stats::setNames(rep(list(1:1100), 1000),
                                                                 paste0("x", 1:1000))))
  expect_error(summary_table(wide, "id", paste0("x", 1:1000), "mean"), "分組太多")
})


# ── 「用 R 怎麼做」：程式碼的結果和 app 相同 ──────────────────────────────

# 比對用：數值轉文字、因子轉文字（程式碼與 app 的欄型別可能是 int 或 dbl）
flat_df <- function(df) {
  out <- lapply(df, function(x) {
    if (is.factor(x)) return(as.character(x))
    v <- as.vector(unclass(x))
    if (is.numeric(v)) num_to_text(v) else as.character(v)
  })
  tibble::as_tibble(out)
}

run_filter_code <- function(dir, f, lines) {
  code <- paste(c(code_read(f)$tidy, lines), collapse = "\n")
  env <- new.env(parent = globalenv())
  withr::with_dir(dir, {
    suppressMessages(suppressWarnings(eval(parse(text = code, encoding = "UTF-8"), envir = env)))
  })
  env
}

test_that("範例檔（inst/extdata）：篩選與摘要的程式碼結果和 app 完全相同", {
  dir <- system.file("extdata", package = "kitJoin")
  files <- c("example_t1.csv", "example_t2.csv", "example_t3.csv")
  dfs <- lapply(files, function(x) read_data_file(file.path(dir, x), x)$data)
  f <- code_files(files, c("t1", "t2", "t3"), lapply(dfs, names),
                  types = lapply(dfs, code_col_types))
  raws <- list(
    list(
      conds = list(
        list(col = "lang_score", op = "between", lo = 40, hi = 100),
        list(col = "city", op = "in", values = c("臺北市", "新北市", "臺中市"))
      ),
      combine = "or", keep_mode = "exclude", keep_cols = "age_m",
      sort_cols = c("sex", "lang_score"), sort_desc = "lang_score"
    ),
    NULL,
    list(
      conds = list(
        list(col = "lang_score", op = "lt", num = 9999),
        list(col = "city", op = "contains", text = "北"),
        list(col = "sex", op = "ne", num = 2)
      ),
      keep_mode = "include", keep_cols = c("release_id", "lang_score")
    )
  )
  specs <- purrr::map2(raws, dfs, function(raw, df) if (!is.null(raw)) filter_spec_clean(raw, df))
  shown <- paste0(c("t1", "t2", "t3"), "（", files, "）")
  app <- filter_files(dfs, specs, shown)
  expect_true(all(app$report[["保留列數"]] > 0))
  for (in_place in c(TRUE, FALSE)) {
    lines <- c("library(dplyr)", "library(tidyr)", code_filter(f, specs, shown, in_place))
    env <- run_filter_code(dir, f, lines)
    for (i in c(1, 3)) {
      got <- env[[filter_target_obj(f$obj[i], in_place)]]
      expect_equal(flat_df(got), flat_df(app$data[[i]]), info = paste(i, in_place))
      expect_named(got, names(app$data[[i]]))
    }
    # 沒有設定的檔案沒有程式碼，物件不變
    expect_equal(flat_df(env$data_2), flat_df(dfs[[2]]))
    expect_false(exists("filtered_2", envir = env, inherits = FALSE))
  }

  # 摘要：接在篩選後面
  lines <- c(
    "library(dplyr)", "library(tidyr)", code_filter(f, specs, shown, TRUE),
    code_summary("data_1", "summary_1", "sex", c("lang_score", "release_id"),
                 names(SUMMARY_STATS)),
    code_summary("data_2", "summary_2", character(0), "lang_score", c("n_missing", "max"))
  )
  env <- run_filter_code(dir, f, lines)
  app1 <- summary_table(app$data[[1]], "sex", c("lang_score", "release_id"), names(SUMMARY_STATS))
  expect_equal(flat_df(env$summary_1), flat_df(app1))
  app2 <- summary_table(dfs[[2]], character(0), "lang_score", c("n_missing", "max"))
  expect_equal(flat_df(env$summary_2), flat_df(app2))
})

test_that("SAV（數值標籤、整組空白）與特殊欄名的 CSV：程式碼結果和 app 相同", {
  dir <- withr::local_tempdir()
  sav <- tibble::tibble(
    學號 = c("001", "002", "003", "004", "005", "006"),
    sex = haven::labelled(c(1, 2, 1, 2, NA, 1), c(男 = 1, 女 = 2)),
    q1 = haven::labelled(c(1, 5, 3, NA, 2, 4), c(非常不同意 = 1, 非常同意 = 5)),
    blank = c(NA, NA, 3, NA, NA, NA),
    note = c("a.b", "ab", "x", NA, "a.bc", "")
  )
  haven::write_sav(sav, file.path(dir, "w1.sav"))
  readr::write_csv(
    tibble::tibble(`my id` = 1:4, `x"y` = c(0.1, 1 / 3, NA, 100000)), file.path(dir, "w2.csv")
  )
  files <- c("w1.sav", "w2.csv")
  dfs <- lapply(files, function(x) read_data_file(file.path(dir, x), x)$data)
  f <- code_files(files, c("t1", "t2"), lapply(dfs, names), types = lapply(dfs, code_col_types))
  raws <- list(
    list(
      conds = list(
        list(col = "q1", op = "in", values = c("1", "3", "5")),
        list(col = "note", op = "contains", text = "a.b"),
        list(col = "學號", op = "not_in", values = "006")
      ),
      combine = "or", sort_cols = "學號", sort_desc = "學號"
    ),
    list(
      conds = list(list(col = "x\"y", op = "ge", num = 1 / 3)),
      keep_mode = "include", keep_cols = c("x\"y", "my id"), sort_cols = "my id",
      sort_desc = "my id"
    )
  )
  specs <- purrr::map2(raws, dfs, filter_spec_clean)
  app <- filter_files(dfs, specs, c("t1", "t2"))
  expect_equal(app$data[[2]]$`my id`, c(4L, 2L))
  lines <- c(
    "library(dplyr)", "library(tidyr)", code_filter(f, specs, c("t1", "t2"), TRUE),
    code_summary("data_1", "summary_1", "sex", c("q1", "blank"), names(SUMMARY_STATS), "sex")
  )
  env <- run_filter_code(dir, f, lines)
  for (i in 1:2) {
    expect_equal(flat_df(env[[f$obj[i]]]), flat_df(app$data[[i]]), info = i)
  }
  expect_equal(attr(env$data_1$q1, "labels"), attr(app$data[[1]]$q1, "labels"))
  app_sum <- summary_table(app$data[[1]], "sex", c("q1", "blank"), names(SUMMARY_STATS))
  expect_equal(flat_df(env$summary_1), flat_df(app_sum))
  expect_true(any(is.na(app_sum$mean)))
})
