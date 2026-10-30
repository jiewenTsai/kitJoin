# 「kits 作答檔」三個步驟的純函式（R/kitsdata_steps.R）、逐題表、數字文字、依欄名篩選

kt_dfs <- function() {
  list(
    tibble::tibble(id = c("K1", "K2"), wave_m = c(3, 3), sex = c("boy", "girl"),
                   co = c("A", "A"), q1 = c(0, 1)),
    tibble::tibble(kid = c("K1", "K3"), wave_m = c(6, 6), sex2 = c("boy", "boy"),
                   co = c("A", "A"), q1 = c(1, 2))
  )
}
kt_files <- c("f1.csv", "f2.csv")

kt_cells <- function(...) {
  base <- list(cohort = c("M03", "M06"), wave_m = c("wave_m", "wave_m"), subj_id = c("id", "kid"))
  utils::modifyList(base, list(...))
}

test_that("第 1 步：每個檔案都選欄（同名放 map、不同名放 per_file）", {
  s <- kits_step1_spec(kt_dfs(), kt_files, kt_cells(sex = c("sex", "sex2")))
  expect_length(s$problems, 0)
  expect_equal(s$map$wave_m, "wave_m")
  expect_equal(s$map$subj_id, "")
  expect_equal(s$per_file$subj_id, c("id", "kid"))
  expect_equal(s$per_file$sex, c("sex", "sex2"))
  expect_length(s$typed, 0)
  expect_equal(s$cohort, list(list(type = "text", value = "M03"),
                              list(type = "text", value = "M06")))
  # 前後空白不算；NA 當成空白
  s2 <- kits_step1_spec(kt_dfs(), kt_files, kt_cells(wave_m = c(" wave_m ", "wave_m"),
                                                      sex = c(NA, NA)))
  expect_length(s2$problems, 0)
  expect_equal(s2$map$wave_m, "wave_m")
  expect_null(s2$per_file$sex)
})

test_that("第 1 步：每個檔案都填值（typed）；數字欄填的不是數字時列出問題", {
  s <- kits_step1_spec(kt_dfs(), kt_files, kt_cells(wave_m = c("3", "6"), wave = c("m03", "m06")))
  expect_length(s$problems, 0)
  expect_equal(s$typed$wave_m, c("3", "6"))
  expect_equal(s$typed$wave, c("m03", "m06"))
  expect_equal(s$map$wave_m, "")
  s <- kits_step1_spec(kt_dfs(), kt_files, kt_cells(wave_m = c("3", "x")))
  expect_match(s$problems, "f2.csv：時間點（wave_m）要填數字（例如 18），現在是「x」", all = FALSE)
  # 孩子編號不能自己填
  s <- kits_step1_spec(kt_dfs(), kt_files, kt_cells(subj_id = c("abc", "abc")))
  expect_match(s$problems, "孩子編號在哪一欄要選檔案裡的一欄（「abc」不是欄名）", all = FALSE)
})

test_that("第 1 步：有的選欄、有的填值；列次編號各檔不同名", {
  s <- kits_step1_spec(kt_dfs(), kt_files, kt_cells(wave_m = c("wave_m", "6")))
  expect_match(s$problems, "波次（時間點）：有的檔案選了欄、有的填了值", all = FALSE)
  expect_equal(s$map$wave_m, "")
  expect_null(s$typed$wave_m)
  s <- kits_step1_spec(kt_dfs(), kt_files, kt_cells(visit_id = c("id", "kid")))
  expect_match(s$problems, "列次編號：每個檔案要選同一個欄名", all = FALSE)
})

test_that("第 1 步：沒填的格", {
  s <- kits_step1_spec(kt_dfs(), kt_files, kt_cells(cohort = c("M03", ""), wave_m = c("", "")))
  expect_match(s$problems, "世代還沒填：f2.csv", all = FALSE)
  expect_match(s$problems, "波次（時間點）：每個檔案都要填", all = FALSE)
  s <- kits_step1_spec(kt_dfs(), kt_files, kt_cells(wave_m = c("wave_m", ""), subj_id = c("id", "")))
  expect_match(s$problems, "波次（時間點）還沒選：f2.csv", all = FALSE)
  expect_match(s$problems, "孩子編號在哪一欄還沒選：f2.csv", all = FALSE)
  s <- kits_step1_spec(kt_dfs(), kt_files, kt_cells(wave = c("m03", "")))
  expect_match(s$problems, "波次標籤還沒填：f2.csv", all = FALSE)
  # 選填的欄可以都空白；性別只有一個檔案有也可以
  s <- kits_step1_spec(kt_dfs(), kt_files, kt_cells(sex = c("sex", ""), age_m = c("", "")))
  expect_length(s$problems, 0)
  expect_equal(s$per_file$sex, c("sex", ""))
  # 沒有檔案
  expect_type(kits_step1_spec(list(), character(0), list())$problems, "character")
  expect_equal(unique(s$problems), s$problems)
})

test_that("第 1 步：世代可以選檔案裡的欄，每個檔案可以不一樣", {
  s <- kits_step1_spec(kt_dfs(), kt_files, kt_cells(cohort = c("co", "M06")))
  expect_length(s$problems, 0)
  expect_equal(s$cohort[[1]], list(type = "column", value = "co"))
  expect_equal(s$cohort[[2]], list(type = "text", value = "M06"))
  # 不是那個檔案的欄名：當成自己填的值
  s <- kits_step1_spec(kt_dfs(), kt_files, kt_cells(cohort = c("sex2", "sex2")))
  expect_equal(s$cohort[[1]], list(type = "text", value = "sex2"))
  expect_equal(s$cohort[[2]], list(type = "column", value = "sex2"))
})

test_that("第 1 步的猜測：世代不猜，各檔的 ID 欄各自猜", {
  dfs <- kt_dfs()
  names(dfs[[1]])[1] <- "w1_id"
  names(dfs[[2]])[1] <- "w2_id"
  g <- kits_step1_guess(dfs)
  expect_equal(g$cohort, c("", ""))
  expect_equal(g$wave_m, c("wave_m", "wave_m"))
  expect_equal(g$subj_id, c("w1_id", "w2_id"))
  expect_equal(g$sex, c("sex", ""))
  expect_setequal(names(g), c(KITS_STEP1_MAIN, KITS_STEP1_MORE))
})

test_that("第 1 步收起來的一行", {
  cells <- list(cohort = c("TDCYP", "TDCYP", ""), wave_m = c("2009", "2011", "2013"),
                subj_id = c("w1_id", "w2_id", "w3id"))
  expect_equal(kits_step1_summary(cells, 3, "year"),
               "3 個檔案 · 世代 TDCYP · 2009、2011、2013（年） · 孩子編號 w1_id 等 3 欄")
  one <- list(cohort = c("M03", "M06"), wave_m = c("3", "6"), subj_id = c("id", "id"))
  expect_equal(kits_step1_summary(one, 2, "month"), "2 個檔案 · 世代 M03、M06 · 3、6（月） · 孩子編號 id")
  expect_no_match(kits_step1_summary(one, 2, "other"), "（")
  many <- list(cohort = paste0("C", 1:4), wave_m = as.character(1:8), subj_id = character(0))
  out <- kits_step1_summary(many, 8, "index")
  expect_match(out, "世代 C1、C2、C3…", fixed = TRUE)
  expect_match(out, "（波次序號）", fixed = TRUE)
  expect_no_match(out, "孩子編號")
})

test_that("第 2 步：挑出幾欄、合成幾題、各領域幾題", {
  df <- tibble::tibble(q1 = c(0, 1), q2 = c(0, 1), q3 = c(0, 1), q4 = c(0, 1))
  s <- kits_item_settings(list(df), names(df), include = FALSE)
  expect_false(any(s$include))
  cnt <- kits_step2_counts(s)
  expect_equal(cnt$n_cols, 0)
  expect_equal(cnt$text, "還沒有挑題目")
  s <- kits_bulk_include(s, c("q1", "q2", "q3"), TRUE) |>
    kits_bulk_domain(c("q1", "q2"), "lan") |> kits_bulk_domain("q3", "cog")
  cnt <- kits_step2_counts(s)
  expect_equal(cnt$n_cols, 3)
  expect_equal(cnt$n_items, 3)
  expect_equal(cnt$text, "挑出 3 欄（lan 2 題、cog 1 題）")
  # q1、q2 輸出成同一個題號 x
  s$item_id_out[1:2] <- "x"
  cnt <- kits_step2_counts(s)
  expect_equal(cnt$n_items, 2)
  expect_equal(as.integer(cnt$by_domain[["lan"]]), 1)
  expect_equal(cnt$text, "挑出 3 欄 → 合成 2 題（lan 1 題、cog 1 題）")
  # 沒有領域：只寫欄數
  s$domain <- NA_character_
  expect_equal(kits_step2_counts(s)$text, "挑出 3 欄 → 合成 2 題")
})

test_that("用欄名挑：開頭是這些字（不分大小寫，頓號、逗號、空白都可以隔開）", {
  cols <- c("w1d5_1", "W2d5_1", "w1c1", "id", "xw1d5")
  expect_equal(kits_pick_cols(cols, "w1d5_、 w2d5_"), c("w1d5_1", "W2d5_1"))
  expect_equal(kits_pick_cols(cols, "w1,id"), c("w1d5_1", "w1c1", "id"))
  expect_equal(kits_pick_cols(cols, "W1D5；ID"), c("w1d5_1", "id"))
  expect_equal(kits_pick_cols(cols, ""), character(0))
  expect_equal(kits_pick_cols(cols, " 、 "), character(0))
  expect_equal(kits_pick_cols(cols, NULL), character(0))
  expect_equal(kits_pick_cols(cols, "zz"), character(0))
  expect_equal(kits_pick_cols(cols, "d5."), character(0))    # 不是正規表示式
})

# 計分檢查用的題目設定：a 有 9999、從 1 開始；b 從 1 開始
kt_check_settings <- function() {
  df <- tibble::tibble(a = c(1, 2, 9999), b = c(1, 2, 3))
  kits_item_settings(list(df), c("a", "b")) |> kits_bulk_domain(c("a", "b"), "lan")
}
kt_state <- function(checks, id) checks[[id]]$state

test_that("第 3 步：沒有挑題目時沒有檢查", {
  s <- kt_check_settings()
  s$include <- FALSE
  expect_equal(kits_step3_checks(s), list())
})

test_that("第 3 步：遺漏值代碼要決定；「它們是真的分數」之後沒問題；格數變了要重新決定", {
  s <- kt_check_settings()
  ch <- kits_step3_checks(s)
  expect_equal(kt_state(ch, "codes"), "todo")
  expect_equal(ch$codes$title, "1 欄有遺漏值代碼 9999（共 1 格）")
  expect_match(ch$codes$text, "kits 會把 9999 當成分數")
  # 性別、wave_m、age_m 的代碼也算格數
  expect_match(kits_step3_checks(s, side_cells = 2)$codes$title, "（共 3 格）")
  ch <- kits_step3_checks(s, ack = list(codes = 1))
  expect_equal(kt_state(ch, "codes"), "ok")
  expect_equal(ch$codes$title, "1 欄的 9999 照原樣當成分數（你選的）")
  expect_equal(kt_state(kits_step3_checks(s, side_cells = 1, ack = list(codes = 1)), "codes"),
               "todo")
  # 沒有代碼；用過「遺漏值處理」
  s$n_codes <- 0L
  s$codes <- list(kits_no_codes(), kits_no_codes())
  expect_equal(kits_step3_checks(s)$codes$title, "沒有看起來像遺漏值代碼的值")
  expect_equal(kits_step3_checks(s, na_applied = TRUE)$codes$title,
               "遺漏值代碼已經用「遺漏值處理」改成 NA")
})

test_that("第 3 步：都從 1 開始時建議減 1；「不用改」「已經減了」都是沒問題；最低分不同只提醒", {
  s <- kt_check_settings()
  ch <- kits_step3_checks(s)
  expect_equal(kt_state(ch, "shift"), "todo")
  expect_equal(ch$shift$title, "2 欄的分數都從 1 開始")
  expect_equal(ch$shift$text, "kits 要從 0 開始。全部減 1 之後是 0–2。")
  ch <- kits_step3_checks(s, ack = list(shift = 1))
  expect_equal(kt_state(ch, "shift"), "ok")
  expect_equal(ch$shift$title, "分數從 1 開始，不改（你選的）")
  shifted <- kits_bulk_shift(s, s$column, -1)
  ch <- kits_step3_checks(shifted, ack = list(shifted = 1))
  expect_equal(kt_state(ch, "shift"), "ok")
  expect_equal(ch$shift$title, "分數全部減了 1，現在從 0 開始")
  expect_equal(kits_step3_checks(shifted)$shift$title, "分數都從 0 開始")
  mixed <- kits_bulk_shift(s, "a", -1)
  ch <- kits_step3_checks(mixed)
  expect_equal(kt_state(ch, "shift"), "note")
  expect_equal(ch$shift$title, "1 欄沒有 0 分")
  # 負分只提醒
  neg <- kits_bulk_shift(s, "a", -2)
  expect_equal(kt_state(kits_step3_checks(neg), "neg"), "note")
  expect_null(kits_step3_checks(s)$neg)
})

test_that("第 3 步：還沒有領域、計分有缺、小數", {
  df <- tibble::tibble(a = c(0, 1, 1), b = c(0, 1, 0), c = c(0.5, 1, 2))
  s <- kits_item_settings(list(df), c("a", "b", "c"))
  ch <- kits_step3_checks(s)
  expect_equal(kt_state(ch, "domain"), "todo")
  expect_equal(ch$domain$title, "3 欄還沒有領域")
  expect_equal(kt_state(ch, "frac"), "todo")
  expect_equal(ch$frac$title, "1 欄的分數有小數（可能不是題目）")
  expect_equal(ch$frac$text, "c")
  expect_null(ch$unscored)
  s <- kits_bulk_domain(s, c("a", "b", "c"), "lan")
  s$score[[2]] <- c(0, NA)
  s <- kits_bulk_include(s, "c", FALSE)
  ch <- kits_step3_checks(s)
  expect_equal(kt_state(ch, "domain"), "ok")
  expect_equal(kt_state(ch, "frac"), "ok")
  expect_equal(kt_state(ch, "unscored"), "todo")
  expect_equal(ch$unscored$title, "1 欄有原分數沒有計分")
  # 沒有作答的欄不算
  s2 <- kits_item_settings(list(tibble::tibble(e = c(NA_real_, NA_real_))), "e")
  expect_equal(kits_step3_checks(s2), list())
})

test_that("第 3 步：反向題（用了題目表時才列）；收起來的一行", {
  dfs <- list(tibble::tibble(id = 1:2, w1a8 = c(0, 1), w1ra8 = c(1, 0)))
  up <- list(type = "table", data = tibble::tibble(item_id = "w1a8", domain = "self"))
  s <- kits_item_settings(dfs, c("w1a8", "w1ra8"), include = FALSE)
  s <- kits_settings_apply(s, up, reverse = "prefix")
  expect_true(all(s$include))                       # 題目表列到的（含反向題）才納入
  ch <- kits_step3_checks(s, reverse_used = TRUE)
  expect_equal(kt_state(ch, "reverse"), "ok")
  expect_equal(ch$reverse$title, "1 欄是反向題，用原題的領域")
  expect_null(kits_step3_checks(s)$reverse)
  s <- kits_settings_apply(kits_item_settings(dfs, c("w1a8", "w1ra8"), include = FALSE), up,
                           reverse = "none")
  expect_equal(s$include, c(TRUE, FALSE))
  expect_equal(kits_step3_checks(s, reverse_used = TRUE)$reverse$title, "沒有反向題")

  ch <- kits_step3_checks(kt_check_settings(), ack = list(codes = 1, shift = 1))
  expect_equal(kits_step3_summary(ch),
               "1 欄的 9999 照原樣當成分數（你選的） · 分數從 1 開始，不改（你選的） · 每一題都有領域")
  expect_equal(kits_step3_summary(kits_step3_checks(kt_check_settings())), "每一題都有領域")
  expect_equal(kits_step3_summary(list()), "都沒問題")
})

test_that("逐題表的計分：沒改、有改、沒有作答、種類太多、缺計分", {
  raw <- list(c(0, 1, 2), c(1, 2, 3), numeric(0), 1:20, 0:19, c(1, 2), c(0.5, 1))
  score <- list(c(0, 1, 2), c(0, 1, 2), numeric(0), 1:20, c(0:18, 0), c(0, NA), c(0.5, 1))
  expect_equal(
    kits_score_line(raw, score),
    c("0 1 2", "1→0 2→1 3→2", "（沒有作答）", "20 種（1–20）", "20 種（0–19），有改",
      "1→0 2→？", "0.5 1")
  )
  expect_equal(kits_score_line(list(), list()), character(0))
})

test_that("逐題表的提醒", {
  s <- tibble::tibble(
    column = letters[1:7], include = c(TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE),
    domain = c(NA, NA, "a", "a", "a", "a", "a"),
    raw = list(c(0, 1), c(0, 1), c(1, 2), c(0, 1), c(0, 1, 2), c(0, 2), c(0, 1)),
    score = list(c(0, 1), c(0, 1), c(1, 2), c(-1, 0), c(0, 0.5, 1), c(0, 2), c(0, NA)),
    codes = list(kits_no_codes(), kits_no_codes(), tibble::tibble(value = 9999, why = "x"),
                 kits_no_codes(), kits_no_codes(), kits_no_codes(), kits_no_codes())
  )
  expect_equal(kits_item_flags(s), c(
    "沒有領域", "", "代碼 9999、沒有 0 分", "有負分", "有小數", "分數有缺", "有原分數沒有計分"
  ))
  s$codes <- NULL
  expect_equal(kits_item_flags(s)[3], "沒有 0 分")
})

test_that("逐題表一頁的 HTML：勾選框、跳脫、沒挑的列、輸出題號", {
  df <- tibble::tibble(q1 = c(0, 1), q2 = c(1, 2), q3 = c(0, 1))
  s <- kits_item_settings(list(df), names(df))
  s$column[1] <- "a<b&\"c'"
  s$domain[1] <- "<b>lan</b>"
  s$include[3] <- FALSE
  s$item_id_out[2] <- "d<5>"
  html <- as.character(kits_items_page_html("kitsdata-sel", s, selected = s$column[1]))
  expect_s3_class(kits_items_page_html("kitsdata-sel", s), "html")
  expect_match(html, '<div id="kitsdata-sel" class="form-group shiny-input-checkboxgroup',
               fixed = TRUE)
  expect_equal(lengths(regmatches(html, gregexpr('type="checkbox" name="kitsdata-sel"', html))),
               3)
  expect_match(html, 'value="a&lt;b&amp;&quot;c&#39;" checked="checked"/>', fixed = TRUE)
  expect_match(html, 'value="q2"/>', fixed = TRUE)                 # 沒勾
  expect_equal(lengths(regmatches(html, gregexpr("checked=", html))), 1)
  expect_match(html, "<code>a&lt;b&amp;\"c'</code>", fixed = TRUE)
  expect_match(html, "&lt;b&gt;lan&lt;/b&gt;", fixed = TRUE)
  expect_no_match(html, "<b>", fixed = TRUE)
  expect_match(html, "→ 題號 d&lt;5&gt;</span>", fixed = TRUE)
  expect_match(html, '<tr class="kit-row-off">', fixed = TRUE)
  expect_equal(lengths(regmatches(html, gregexpr("（沒挑）", html))), 1)
  expect_match(html, "（沒有）")                                     # q2、q3 沒有領域
  expect_match(html, "<td>1 2</td>", fixed = TRUE)                  # 沒改計分
  # 沒有列
  empty <- as.character(kits_items_page_html("x", s[0, ]))
  expect_match(empty, "沒有符合的欄")
  expect_no_match(empty, "checkbox")
})

test_that("kits_num_text() 和一個一個 format() 相同", {
  x <- c(0, -0, 1, -3, 100000, 123456789, 1e14, 2.5, 1 / 3, 1234567.5, 0.1 + 0.2, -0.75,
         1e20, 1e-8, NA, Inf)
  expect_identical(kits_num_text(x), vapply(x, kits_num_format, character(1)))
  expect_identical(kits_num_text(numeric(0)), character(0))
})

test_that("依欄名篩選：不分大小寫、去掉前後空白、不是正規表示式", {
  cols <- c("lana01", "LANb02", "cog.1", "cog11")
  expect_identical(kitsdata_filter_cols(cols, NULL), cols)
  expect_identical(kitsdata_filter_cols(cols, "  "), cols)
  expect_identical(kitsdata_filter_cols(cols, " Lan "), c("lana01", "LANb02"))
  expect_identical(kitsdata_filter_cols(cols, "g.1"), "cog.1")
})
