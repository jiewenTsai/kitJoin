# 「3. 題目設定」表：一次組好的 HTML 和舊的（htmltools 標籤）標記相同

# 舊的標記去掉標籤之間的換行與縮排；新的去掉 data-col（只給篩選用）
kt_old <- function(...) gsub("\n *", "", as.character(kitsdata_settings_table_old(...)))
kt_new <- function(...) {
  gsub(" data-col=\"[^\"]*\"", "", as.character(kitsdata_settings_table(...)))
}
kt_same <- function(id, s, selected = character(0), shown = s$column) {
  expect_identical(kt_new(id, s, selected, shown), kt_old(id, s, selected, shown))
}

# 9 欄的寬資料（和 ⑥ 的測試資料一樣有文字欄、小數、9999）
kt_settings9 <- function() {
  df <- tibble::tibble(
    release_id = c("K1", "K2", "K1"), wave = c("m03g01", "m03g01", "m06g02"),
    wave_m = c(3, 3, 6), age_m = c(3.1, 3.25, 6.1), sex = c(1, 2, 1),
    city = "臺北市", lana01 = c(1, 2, 9999), bodyaf01 = c(2, 1, 3), extra = c(5, 5, 5)
  )
  kits_item_settings(list(df), kits_candidate_cols(list(df), character(0))$numeric)
}

# 150 欄：合併、平移、反向、排除、缺計分、超過 12 種、小數、大數、沒有作答、各種來源
kt_settings150 <- function() {
  set.seed(150)
  n <- 150
  cols <- sprintf("%s%03d%s", rep(c("lan", "cog", "mot", "soc", "emo"), 30), seq_len(n),
                  ifelse(seq_len(n) %% 7 == 0, "r", ""))
  raws <- lapply(seq_len(n), function(i) {
    switch(i %% 10 + 1,
      sort(sample(0:20, 15)),                       # 超過 12 種
      round(seq(8, 12, length.out = 30), 1),        # 連續（小數）
      numeric(0),                                   # 沒有作答
      c(1.5, 2, 3),                                 # 有小數的題
      c(0, 123456, 1234567.5),                      # 大數
      sort(sample(0:6, 4)),
      1:5, 0:4, 0:2, 0:3
    )
  })
  s <- tibble::tibble(
    column = cols, include = TRUE, domain = NA_character_, source = NA_character_,
    raw = raws, score = raws, has_codes = seq_len(n) %% 11 == 0
  )
  tab <- tibble::tibble(item_id = sub("r$", "", cols[seq(1, n, 3)]), domain = "lan")
  s <- kits_settings_apply(s, list(type = "table", data = tab))
  s <- kits_bulk_domain(s, cols[seq(2, n, 5)], "cog")
  s <- kits_bulk_merge(s, cols[1:60], c(3, 4), 3)
  s <- kits_bulk_shift(s, cols[seq(1, n, 2)], -1)
  s <- kits_bulk_reverse(s, cols[seq(3, n, 9)])
  s <- kits_bulk_include(s, cols[seq(5, n, 6)], FALSE)
  # 缺計分（設定檔對不到的原分數）
  for (i in seq(4, n, 13)) s$score[[i]][seq_along(s$score[[i]]) %% 2 == 0] <- NA
  s$source[seq(6, n, 17)] <- "設定檔"
  s
}

test_that("新的表和舊的標記相同：9 欄", {
  s <- kt_settings9()
  expect_equal(nrow(s), 6)
  kt_same("kitsdata-sel", s)
  s$domain[1:2] <- "lan"
  s$source[1] <- "題目表"
  s$include[3] <- FALSE
  s <- kits_bulk_shift(s, s$column[4:5], -1)
  s$score[[6]] <- NA_real_
  kt_same("kitsdata-sel", s, selected = s$column[c(1, 4)], shown = s$column[2:5])
})

test_that("新的表和舊的標記相同：150 欄（合併、平移、反向、排除、缺計分、超過 12 種）", {
  s <- kt_settings150()
  expect_true(any(lengths(s$raw) > KITS_MAP_SHOW_MAX))
  expect_true(any(purrr::map_lgl(s$score, anyNA)))
  expect_true(any(!s$include))
  expect_true(all(c("題目表", "自己設", "設定檔") %in% s$source))
  expect_true(any(grepl("原題", s$source)))
  kt_same("kitsdata-sel", s)
  kt_same("kitsdata-sel", s, selected = s$column[seq(1, 150, 4)],
          shown = kitsdata_filter_cols(s$column, "LAN"))
  html <- as.character(kitsdata_settings_table("kitsdata-sel", s))
  expect_match(html, "展開計分對照")
  expect_match(html, "<td class=\"changed text-danger\">？</td>", fixed = TRUE)
  expect_match(html, "<td class=\"changed\">", fixed = TRUE)
  expect_s3_class(kitsdata_settings_table("kitsdata-sel", s), "html")
  # 每一列有 data-col
  expect_equal(lengths(regmatches(html, gregexpr("<tr data-col=\"", html))), 150)
  expect_match(html, paste0("<tr data-col=\"", s$column[1], "\""), fixed = TRUE)
})

test_that("欄名、領域、來源、提醒都有跳脫", {
  s <- kt_settings9()[1:2, ]
  s$column[1] <- "a<b&\"c'"
  s$domain[1] <- "<b>lan</b>"
  s$source[1] <- "題目表 & <x>"
  kt_same("kitsdata-sel", s, selected = s$column[1])
  html <- as.character(kitsdata_settings_table("kitsdata-sel", s, s$column[1]))
  expect_match(html, "value=\"a&lt;b&amp;&quot;c&#39;\"", fixed = TRUE)
  expect_match(html, "data-col=\"a&lt;b&amp;&quot;c&#39;\"", fixed = TRUE)
  expect_match(html, "<code>a&lt;b&amp;\"c'</code>", fixed = TRUE)
  expect_match(html, "&lt;b&gt;lan&lt;/b&gt;", fixed = TRUE)
  expect_no_match(html, "<b>", fixed = TRUE)
})

test_that("沒有題目欄時是提示文字", {
  expect_identical(
    as.character(kitsdata_settings_table("x", kits_settings_empty())),
    as.character(kitsdata_settings_table_old("x", kits_settings_empty()))
  )
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

test_that("⑥ 模組：打篩選字不重畫表格；重畫時帶入目前的篩選", {
  dir <- withr::local_tempdir()
  df <- tibble::tibble(id = c("K1", "K2"), lana01 = c(0, 1), lanb02 = c(1, 2), cog01 = c(2, 3))
  path <- file.path(dir, "w.csv")
  readr::write_csv(df, path)
  up <- data.frame(name = "w.csv", size = file.size(path), type = "", datapath = path)
  ui <- as.character(mod_kitsdata_ui("kitsdata"))
  expect_match(ui, "var box = 'kitsdata-sel_filter', out = 'kitsdata-settings_table';",
               fixed = TRUE)
  shiny::testServer(kit_server, {
    session$setInputs(`upload-files` = up, `kitsdata-map_subj_id` = "id")
    before <- output[["kitsdata-settings_table"]]$html
    expect_no_match(before, "display: none")
    session$setInputs(`kitsdata-sel_filter` = "LAN")
    expect_identical(output[["kitsdata-settings_table"]]$html, before)
    session$setInputs(`kitsdata-sel_filtered` = 1)
    expect_equal(output[["kitsdata-sel_count"]], "已選取 2 列")
    session$setInputs(`kitsdata-bulk_shift` = -1, `kitsdata-do_shift` = 1)
    after <- output[["kitsdata-settings_table"]]$html
    expect_match(after, "<tr data-col=\"cog01\" style=\"display: none;\">", fixed = TRUE)
    expect_match(after, "<tr data-col=\"lana01\">", fixed = TRUE)
    expect_match(after, "value=\"lana01\" checked=\"checked\"", fixed = TRUE)
  })
})
