# 寬轉長：欄名怎麼變成值（點選；app 寫規則，使用者不用寫正規表示式）

raw3 <- tibble::tibble(
  id = 1:2, sex = c(1, 2),
  score_t1_raw = c(1, 2), score_t2_raw = c(3, NA), score_t3_raw = c(5, 6)
)

# 依程式碼在乾淨的環境跑一次
run_code <- function(args, data) {
  env <- new.env()
  env$dat <- data
  eval(parse(text = paste0("library(tidyr)\nlong <- ", pivot_longer_code(args, "dat"))),
       envir = env)
  env$long
}

# 點選的設定 → 參數（和模組一樣的步驟）
click_args <- function(cols, start = "", end = "", split = "", names_to = NULL,
                       numeric = FALSE) {
  m <- pivot_name_middles(cols, start, end)
  info <- if (nzchar(split)) pivot_name_split(m$middles, split)
  names_to <- names_to %||% if (is.null(info)) "name" else pivot_part_default_names(info$digit)
  num <- if (is.null(info)) pivot_middles_numeric(m$middles) else info$numeric
  c(list(cols = cols),
    pivot_names_args(start, end, split, info, names_to, numeric & num),
    list(names_by_app = TRUE))
}

test_that("可以去掉的開頭、結尾：切在分隔符號、數字交界、小寫接大寫；長的在前", {
  cols <- c("score_t1_raw", "score_t2_raw", "score_t3_raw")
  expect_equal(pivot_name_starts(cols), c("score_t", "score_", "score"))
  expect_equal(pivot_name_ends(cols), c("_raw", "raw"))
  expect_equal(pivot_name_starts(c("lan01", "lan02", "lan10")), "lan")
  expect_equal(pivot_name_ends(c("lan01", "lan02", "lan10")), character(0))
  # 小寫接大寫是切點；大小寫不同就不是共同的開頭（tidyr 也分大小寫）
  expect_equal(pivot_name_starts(c("ScoreT1", "ScoreT2")), c("ScoreT", "Score"))
  expect_equal(pivot_name_starts(c("Score_1", "score_2")), character(0))
  # 有特殊符號的欄名
  expect_equal(pivot_name_starts(c("a.b(1)x", "a.b(2)x")), c("a.b(", "a.", "a"))
  expect_equal(pivot_name_ends(c("a.b(1)x", "a.b(2)x")), ")x")
  # 沒有共同的部分；不能把整個欄名去掉
  expect_equal(pivot_name_starts(c("id", "sex")), character(0))
  expect_equal(pivot_name_starts(c("t1", "t1x")), "t")
  expect_equal(pivot_name_starts(character(0)), character(0))
})

test_that("去掉開頭、結尾後沒有剩下字：說明", {
  m <- pivot_name_middles(c("a_1", "a_2"), "a_", "")
  expect_equal(m$middles, c("1", "2"))
  expect_null(m$problem)
  m <- pivot_name_middles(c("ab_1", "ab_"), "ab_", "")
  expect_match(m$problem, "沒有剩下字：ab_")
})

test_that("可以用的切法：每個欄名都有的符號；文字和數字分開", {
  expect_equal(pivot_name_split_choices(c("lan_1", "cog_2")), c("_", "digits"))
  expect_equal(pivot_name_split_choices(c("a.b", "c.d-e")), ".")
  expect_equal(pivot_name_split_choices(c("a b", "c d")), " ")
  expect_equal(pivot_name_split_choices(c("lan12", "cog3")), "digits")
  expect_equal(pivot_name_split_choices(c("12", "13")), character(0))
  expect_equal(pivot_name_split_choices(c("ab", "cd")), character(0))
})

test_that("切成幾段：段數、哪段是數字；段數不一樣時說明哪些", {
  s <- pivot_name_split(c("lan12", "cog03"), "digits")
  expect_equal(s$n, 2L)
  expect_equal(s$digit, c(FALSE, TRUE))
  expect_equal(s$parts[[2]], c("cog", "03"))
  expect_null(s$problem)
  s <- pivot_name_split(c("a_1", "b_2_x"), "_")
  expect_true(is.na(s$n))
  expect_match(s$problem, "a_1 → 2 段")
  expect_match(s$problem, "b_2_x → 3 段")
  # 段數一樣、但數字和文字的順序不一樣
  s <- pivot_name_split(c("lan12", "12lan"), "digits")
  expect_match(s$problem, "段數不一樣")
  # 空的段也算一段（和 tidyr 的 names_sep 一樣）
  expect_equal(pivot_name_split(c("a_", "b_c"), "_")$parts[[1]], c("a", ""))
  expect_equal(pivot_part_default_names(c(FALSE, TRUE)), c("name", "number"))
  expect_equal(pivot_part_default_names(c(TRUE, FALSE, TRUE)), c("number1", "name", "number2"))
})

test_that("點選 → tidyr 參數：特殊字一律跳脫", {
  expect_equal(regex_escape("a.b(1)"), "a\\.b\\(1\\)")
  expect_equal(regex_escape("x_y-z"), "x_y-z")
  cols <- c("score_t1_raw", "score_t2_raw")
  # 只去掉開頭：names_prefix
  a <- click_args(cols, start = "score_")
  expect_equal(a$names_prefix, "score_")
  expect_null(a$names_pattern)
  # 有去結尾：names_pattern
  a <- click_args(cols, start = "score_t", end = "_raw")
  expect_equal(a$names_pattern, "^score_t(.*)_raw$")
  expect_null(a$names_prefix)
  # 用一種符號切、沒去結尾：names_sep（點要跳脫）
  a <- click_args(c("lan.1", "cog.2"), split = ".")
  expect_equal(a$names_sep, "\\.")
  expect_equal(a$names_to, c("name", "number"))
  # 用符號切又去結尾：names_pattern
  a <- click_args(c("x_lan_1_raw", "x_cog_2_raw"), start = "x_", end = "_raw", split = "_")
  expect_equal(a$names_pattern, "^x_([^_]*)_([^_]*)_raw$")
  # 文字和數字分開
  a <- click_args(c("lan12", "cog3"), split = "digits", numeric = TRUE)
  expect_equal(a$names_pattern, "^([^0-9]+)([0-9]+)$")
  expect_equal(names(a$names_transform), "number")
  # 開頭有特殊字：字面跳脫
  a <- click_args(c("a.b(1)", "a.b(2)"), start = "a.b(")
  expect_equal(a$names_prefix, "a\\.b\\(")
})

test_that("跳脫過的規則照字面比對（tidyr 的 names_prefix、names_pattern 都是）", {
  df <- tibble::tibble(id = 1, `a.b(1)` = 1, `a.b(2)` = 2)
  out <- pivot_longer_run(df, click_args(c("a.b(1)", "a.b(2)"), start = "a.b("))
  expect_equal(out$name, c("1)", "2)"))
  out <- pivot_longer_run(df, click_args(c("a.b(1)", "a.b(2)"), start = "a.b(", end = ")"))
  expect_equal(out$name, c("1", "2"))
  # 沒跳脫時「.」什麼字都對得上：aXb 也會被去掉（自己寫規則時照 tidyr 的意思）
  df2 <- tibble::tibble(id = 1, a.b1 = 1, aXb2 = 2)
  expect_equal(pivot_longer_run(df2, list(cols = c("a.b1", "aXb2"), names_prefix = "a.b"))$name,
               c("1", "2"))
  esc <- list(cols = c("a.b1", "aXb2"), names_prefix = regex_escape("a.b"))
  expect_equal(pivot_longer_run(df2, esc)$name, c("1", "aXb2"))
})

test_that("兩個欄變成一樣的值：說明並擋下來", {
  df <- tibble::tibble(id = 1, lan_01 = 1, lan_1 = 2)
  a <- click_args(c("lan_01", "lan_1"), start = "lan_", numeric = TRUE)
  chk <- pivot_names_check(a$cols, a)
  expect_equal(chk$dup, c("lan_01", "lan_1"))
  expect_match(chk$problem, "一樣的值")
  expect_error(pivot_longer_run(df, a), "一樣的值")
  # 不轉成數字就分得出來
  a <- click_args(c("lan_01", "lan_1"), start = "lan_")
  expect_equal(pivot_longer_run(df, a)$name, c("01", "1"))
  # 自己寫的規則也查
  expect_error(
    pivot_longer_run(df, list(cols = c("lan_01", "lan_1"), names_pattern = "^(lan)_.*$")),
    "一樣的值"
  )
  # 段數不一樣（模組放在 names_problem）也擋下來
  expect_error(pivot_longer_run(df, list(cols = "lan_1", names_problem = "段數不一樣")),
               "段數不一樣")
})

test_that("預覽：每個欄名會變成什麼（和 tidyr 算的一樣）", {
  a <- click_args(names(raw3)[3:5], start = "score_t", end = "_raw", numeric = TRUE,
                  names_to = "wave")
  chk <- pivot_names_check(a$cols, a)
  expect_equal(chk$preview[["原本的欄名"]], names(raw3)[3:5])
  expect_equal(chk$preview$wave, c("1", "2", "3"))
  expect_null(chk$problem)
  # 對不上規則的欄名（自己寫的規則）
  chk <- pivot_names_check(c("t1", "x"), list(names_to = "w", names_pattern = "^t(.*)$"))
  expect_equal(chk$unmatched, "x")
})

test_that("產生的程式碼跑出來和 pivot_longer_run() 一樣；app 寫的規則有說明", {
  wide <- tibble::tibble(
    id = 1:2, `lan.1` = c(1, NA), `lan.2` = c(3, 4), `cog.1` = c(5, 6),
    `a.b(1)` = 7:8, `a.b(2)` = 9:10
  )
  cases <- list(
    c(click_args(c("lan.1", "lan.2", "cog.1"), split = ".", numeric = TRUE),
      values_drop_na = TRUE),
    click_args(c("lan.1", "lan.2", "cog.1"), split = "digits"),
    click_args(c("lan.1", "lan.2"), start = "lan.", names_to = "wave", numeric = TRUE),
    click_args(c("a.b(1)", "a.b(2)"), start = "a.b(", end = ")", names_to = "k"),
    click_args(c("a.b(1)", "a.b(2)"), start = "a.b(")
  )
  for (args in cases) {
    expect_equal(run_code(args, wide), pivot_longer_run(wide, args))
  }
  expect_type(pivot_longer_run(wide, cases[[1]])$number, "integer")
  code <- pivot_longer_code(cases[[1]], "dat")
  expect_match(code, 'names_sep = "\\\\\\\\."', fixed = FALSE)
  expect_match(code, "names_transform = list(number = as.integer)", fixed = TRUE)
  expect_match(code, "# names_sep 是 app 依你點的設定寫出來的")
  code <- pivot_longer_code(cases[[4]], "dat")
  expect_match(code, 'names_pattern = "^a\\\\.b\\\\(', fixed = TRUE)
  # 每行不超過 100 字
  expect_true(all(nchar(strsplit(code, "\n")[[1]]) <= 100))
  # 自己寫的規則：沒有這行說明
  expect_no_match(pivot_longer_code(list(cols = "x", names_prefix = "s_"), "dat"), "app 依")
})

test_that("模組：選欄後列出可以去掉的開頭、結尾；點了就有參數和預覽", {
  shiny::testServer(mod_pivot_longer_server, args = list(data = shiny::reactive(raw3)), {
    session$setInputs(cols_mode = "include", cols = names(raw3)[3:5])
    html <- output$names_cut$html
    expect_match(html, "score_t")
    expect_match(html, "_raw")
    expect_match(html, "不去掉")
    # 沒點：和原本一樣
    args <- session$returned()
    expect_null(args$names_prefix)
    expect_null(args$names_pattern)
    expect_null(args$names_problem)

    session$setInputs(name_start = "score_t", name_end = "_raw", names_to = "wave")
    args <- session$returned()
    expect_equal(args$names_pattern, "^score_t(.*)_raw$")
    expect_true(args$names_by_app)
    expect_match(output$names_preview$html, "score_t1_raw")
    expect_match(output$names_parts$html, "數字的部分轉成數字")
    out <- pivot_longer_run(raw3, args)
    expect_equal(unique(out$wave), c("1", "2", "3"))

    session$setInputs(names_num = TRUE)
    expect_type(pivot_longer_run(raw3, session$returned())$wave, "integer")

    # 開頭少去一點、文字和數字分開：兩段，各自的欄名
    session$setInputs(name_start = "score_")
    expect_match(output$names_split$html, "文字和數字分開")
    session$setInputs(name_split = "digits")
    expect_match(output$names_parts$html, "第 2 段放到哪一欄")
    session$setInputs(part_1 = "letter", part_2 = "wave")
    args <- session$returned()
    expect_equal(args$names_to, c("letter", "wave"))
    expect_equal(args$names_pattern, "^score_([^0-9]+)([0-9]+)_raw$")
    out <- pivot_longer_run(raw3, args)
    expect_named(out, c("id", "sex", "letter", "wave", "value"))

    # 兩段同名：擋下來
    session$setInputs(part_2 = "letter")
    expect_match(session$returned()$names_problem, "不同名字")
    expect_match(output$names_preview$html, "不同名字")

    # 換了欄、舊的選擇不在新的選項裡：當作不去掉
    session$setInputs(cols = c("id", "sex"))
    expect_null(session$returned()$names_pattern)

    # 自己寫規則：以那裡為準
    session$setInputs(cols = names(raw3)[3:5], name_split = "", names_to = "wave",
                      name_start = "score_t", names_prefix = "score_")
    args <- session$returned()
    expect_equal(args$names_prefix, "score_")
    expect_null(args$names_pattern)
    expect_null(args$names_by_app)
    expect_match(output$names_preview$html, "自己寫規則")
  })
})

test_that("模組：段數不一樣時列出來並擋下來", {
  df <- tibble::tibble(id = 1, a_1 = 1, b_2_x = 2)
  shiny::testServer(mod_pivot_longer_server, args = list(data = shiny::reactive(df)), {
    session$setInputs(cols_mode = "include", cols = c("a_1", "b_2_x"), name_split = "_")
    expect_match(session$returned()$names_problem, "b_2_x → 3 段")
    expect_match(output$names_preview$html, "段數不一樣")
    expect_error(pivot_longer_run(df, session$returned()), "段數不一樣")
  })
})
