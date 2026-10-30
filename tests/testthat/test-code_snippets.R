# 「用 R 怎麼做？」產生的程式碼要能執行，而且結果和 app 相同（B7）
# 小檔案：數字 ID 有空白與 100000、一波的 ID 是文字、波次名稱是中文、數字與含引號

# 比對用：數值一律轉文字、欄依名稱排序、列依內容排序（merge() 會重排列）
norm_df <- function(df) {
  out <- lapply(df, function(x) {
    v <- as.vector(unclass(x))
    if (is.numeric(v)) num_to_text(v) else as.character(v)
  })
  out <- as.data.frame(out, check.names = FALSE, stringsAsFactors = FALSE)
  out <- out[, sort(names(out)), drop = FALSE]
  out <- out[do.call(order, c(unname(as.list(out)), na.last = TRUE)), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# 在資料夾裡執行讀檔 + 某一步的程式碼，回傳執行後的環境
run_code <- function(dir, f, snip, style) {
  code <- paste(code_read(f)[[style]], snip[[style]], sep = "\n")
  env <- new.env(parent = globalenv())
  withr::with_dir(dir, {
    suppressMessages(suppressWarnings(eval(parse(text = code, encoding = "UTF-8"), envir = env)))
  })
  env
}

make_fixture <- function() {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  readr::write_csv(
    tibble::tibble(
      id = c(1, 2, 3, NA, 100000), score = c(10, 9999, 12, 13, 7777),
      x = c(1.5, 2, NA, 4, 5), txt = c("a", " 9999", "b", "c", "d")
    ),
    file.path(dir, "w1.csv"), na = ""
  )
  readr::write_csv(
    tibble::tibble(id = c(1, 3, NA, 6, 100000), score = c(20, 21, 9999, 22, 5)),
    file.path(dir, "w2.csv"), na = ""
  )
  haven::write_sav(
    tibble::tibble(
      id = c("1", "2", "6", "100000"),
      score = haven::labelled(c(1, 98, 99, 2), c(同意 = 1, 不同意 = 2, 跳答 = 98, 遺漏值 = 99)),
      x = c("a", "b", "c", "d")
    ),
    file.path(dir, "w3.sav")
  )
  files <- c("w1.csv", "w2.csv", "w3.sav")
  waves <- c("第一波", "2019", "a\"b")
  dfs <- lapply(files, function(x) read_data_file(file.path(dir, x), x)$data)
  f <- code_files(
    files, waves, lapply(dfs, names), 2L,
    encodings = c("UTF-8", "UTF-8", NA), types = lapply(dfs, code_col_types)
  )
  list(dir = dir, files = files, waves = waves, dfs = dfs, f = f)
}

test_that("物件名稱是 data_1、data_2，字串會跳脫", {
  f <- code_files(c("a.csv", "b.csv"), c("2019", "第\"一\"波"))
  expect_equal(f$obj, c("data_1", "data_2"))
  expect_equal(r_str("a\"b"), "\"a\\\"b\"")
  expect_equal(r_col("學 號"), "`學 號`")
  code <- code_stack(f)
  expect_no_error(parse(text = code$tidy))
  expect_no_error(parse(text = code$base))
  expect_no_error(parse(text = code_join(f, "id", "left_join")$tidy))
  expect_no_error(parse(text = code_join(f, "id", "left_join")$base))
  expect_match(code_read(code_files("a.csv", "t1"))$base, "check.names = FALSE")
})

test_that("② 依代碼：程式碼的結果和 app 相同", {
  fx <- make_fixture()
  codes <- c(7777, 9999)
  for (match_text in c(TRUE, FALSE)) {
    snip <- code_missing(fx$f, codes, "id", match_text)
    for (style in c("tidy", "base")) {
      env <- run_code(fx$dir, fx$f, snip, style)
      for (i in seq_along(fx$dfs)) {
        app <- replace_na_codes(fx$dfs[[i]], codes, "id", match_text)$data
        expect_equal(
          norm_df(env[[fx$f$obj[i]]]), norm_df(app),
          info = paste(style, match_text, i)
        )
      }
    }
  }
})

test_that("② 依數值標籤：程式碼的結果和 app 相同", {
  fx <- make_fixture()
  kw <- parse_na_keywords(kit_na_label_keywords_default())
  snip <- code_missing(fx$f, numeric(0), "id", mode = "labels", keywords = kw)
  for (style in c("tidy", "base")) {
    env <- run_code(fx$dir, fx$f, snip, style)
    for (i in seq_along(fx$dfs)) {
      app <- replace_na_by_labels(fx$dfs[[i]], kw, "id")$data
      expect_equal(norm_df(env[[fx$f$obj[i]]]), norm_df(app), info = paste(style, i))
    }
  }
})

test_that("③ 三種串接：ID 有空白、數字配文字、波次名稱特殊，程式碼的結果和 app 相同", {
  fx <- make_fixture()
  for (method in unname(KIT_JOIN_METHODS)) {
    app <- join_waves(fx$dfs, fx$files, fx$waves, "id", method)$result
    snip <- code_join(fx$f, "id", method)
    for (style in c("tidy", "base")) {
      env <- run_code(fx$dir, fx$f, snip, style)
      expect_equal(norm_df(env$joined), norm_df(app), info = paste(method, style))
    }
  }
})

test_that("③ 各檔 ID 欄名不同：程式碼先改名，結果和 app 相同（C1）", {
  dir <- withr::local_tempdir()
  readr::write_csv(tibble::tibble(w1_id = c(1, 2, 3), a = 1:3), file.path(dir, "w1.csv"))
  readr::write_csv(tibble::tibble(w2_id = c(2, 3, 4), a = 4:6), file.path(dir, "w2.csv"))
  haven::write_sav(tibble::tibble(w3id = c("3", "4"), a = 7:8), file.path(dir, "w3.sav"))
  files <- c("w1.csv", "w2.csv", "w3.sav")
  dfs <- lapply(files, function(x) read_data_file(file.path(dir, x), x)$data)
  ids <- list("w1_id", "w2_id", "w3id")
  f <- code_files(files, c("t1", "t2", "t3"), lapply(dfs, names), 2L,
                  types = lapply(dfs, code_col_types))
  for (method in unname(KIT_JOIN_METHODS)) {
    app <- join_waves(dfs, files, f$wave, "w1_id", method, id_cols = ids)$result
    for (style in c("tidy", "base")) {
      env <- run_code(dir, f, code_join(f, "w1_id", method, id_cols = ids), style)
      expect_equal(norm_df(env$joined), norm_df(app), info = paste(method, style))
    }
  }
  app <- build_attrition(dfs[[1]], dfs[[3]], "w1_id", "w3id")$result
  for (style in c("tidy", "base")) {
    env <- run_code(dir, f, code_attrition(f, 1L, 3L, "w1_id", "w3id"), style)
    expect_equal(as.numeric(env$attrition_df$attrition), as.numeric(app$attrition), info = style)
  }
})

test_that("④ 疊加：型別不同的欄先轉文字，程式碼的結果和 app 相同", {
  fx <- make_fixture()
  app <- stack_waves(fx$dfs, fx$files, fx$waves)$result
  snip <- code_stack(fx$f)
  for (style in c("tidy", "base")) {
    env <- run_code(fx$dir, fx$f, snip, style)
    expect_equal(norm_df(env$stacked), norm_df(app), info = style)
  }
})

test_that("⑤ 流失：ID 空白是 NA、數字配文字，程式碼的結果和 app 相同", {
  fx <- make_fixture()
  for (follow in 2:3) {
    app <- build_attrition(fx$dfs[[1]], fx$dfs[[follow]], "id")$result
    snip <- code_attrition(fx$f, 1L, follow, "id")
    for (style in c("tidy", "base")) {
      env <- run_code(fx$dir, fx$f, snip, style)
      expect_equal(
        as.numeric(env$attrition_df$attrition), as.numeric(app$attrition),
        info = paste(follow, style)
      )
    }
  }
  # 多個 ID 欄
  app <- build_attrition(fx$dfs[[1]], fx$dfs[[2]], c("id", "score"))$result
  for (style in c("tidy", "base")) {
    env <- run_code(fx$dir, fx$f, code_attrition(fx$f, 1L, 2L, c("id", "score")), style)
    expect_equal(as.numeric(env$attrition_df$attrition), as.numeric(app$attrition), info = style)
  }
})

test_that("欄名含空白時 base R 版也能執行（check.names = FALSE）", {
  dir <- withr::local_tempdir()
  readr::write_csv(tibble::tibble(`學 號` = 1:3, a = 1:3), file.path(dir, "a.csv"))
  readr::write_csv(tibble::tibble(`學 號` = 2:4, a = 4:6), file.path(dir, "b.csv"))
  files <- c("a.csv", "b.csv")
  dfs <- lapply(files, function(x) read_data_file(file.path(dir, x), x)$data)
  f <- code_files(files, c("t1", "t2"), lapply(dfs, names), 2L,
                  types = lapply(dfs, code_col_types))
  app <- join_waves(dfs, files, f$wave, "學 號", "inner_join")$result
  for (style in c("tidy", "base")) {
    env <- run_code(dir, f, code_join(f, "學 號", "inner_join"), style)
    expect_equal(norm_df(env$joined), norm_df(app), info = style)
    env <- run_code(dir, f, code_attrition(f, 1L, 2L, "學 號"), style)
    expect_equal(as.numeric(env$attrition_df$attrition), c(1, 0, 0), info = style)
  }
})
