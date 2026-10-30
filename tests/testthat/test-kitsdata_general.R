# ⑥ 通用性：非 KIT 資料（TDCYP 這類）也能在 app 內轉換

# 數字 ID、1–4 計分、題目欄名有波次前綴
kg_wave <- function(ids, prefix, wave_m, seed) {
  set.seed(seed)
  n <- length(ids)
  out <- tibble::tibble(id = ids, wave_m = wave_m)
  for (k in 1:3) out[[paste0(prefix, "q", k)]] <- sample(0:3, n, TRUE)
  out
}

# ── A5：數字 ID 不寫成科學記號 ───────────────────────────────────────────────

test_that("A5：數字的 ID（100000、1e15）、波次標籤轉成文字時不是科學記號", {
  expect_equal(kits_id_text(c(100000, 200000, 1200000, 1e15, NA)),
               c("100000", "200000", "1200000", "1000000000000000", NA))
  expect_equal(kits_id_text(c(0.5, 0.50000001, 3)), c("0.5", "0.50000001", "3"))
  expect_equal(kits_id_text(c("0012", NA)), c("0012", NA))
  expect_equal(kits_id_text(haven::labelled(c(1, 2), c(a = 1))), c("1", "2"))

  dfs <- list(kg_wave(c(100000, 200000, 1e15), "", 0, 1))
  dfs[[1]]$wave <- 2000000
  map <- kits_guess_map(names(dfs[[1]]))
  map$subj_id <- "id"
  s <- kits_item_settings(dfs, c("q1", "q2", "q3")) |>
    kits_bulk_domain(c("q1", "q2", "q3"), "lan")
  spec <- kg_spec(s, map, "B85")
  out <- kits_build(dfs, "w1.csv", spec)
  expect_length(out$problems, 0)
  long <- kits_long_of(out)
  expect_setequal(unique(long$subj_id), c("100000", "200000", "1000000000000000"))
  expect_true("B85_100000_2000000" %in% long$visit_id)
  expect_false(any(grepl("e\\+", long$visit_id)))

  got <- kg_run_code(dfs, spec, s)
  expect_match(got$code, "as_id <- function(x)", fixed = TRUE)
  kg_same_long(got$long, long)
})

# ── A1、C5：遺漏值代碼不會變成分數 ───────────────────────────────────────────

# TDCYP 的樣子：1–4 計分，98 跳答、99 遺漏值寫在值標籤裡
kg_labelled <- function(x) {
  haven::labelled(x, c("非常不同意" = 1, "不同意" = 2, "同意" = 3, "非常同意" = 4,
                       "跳答" = 98, "遺漏值" = 99), label = "W1:老師很關心我")
}

test_that("A1：遺漏值代碼來自值標籤、② 的代碼、7777 以上、和分數分開的值", {
  codes <- kits_value_codes(c(1, 2, 3, 4, 98, 99), c("1" = "非常不同意", "98" = "跳答",
                                                    "99" = "遺漏值"))
  expect_equal(codes$value, c(98, 99))
  expect_match(codes$why, "值標籤「跳答」", all = FALSE)
  expect_equal(kits_value_codes(c(0, 1, 2, 9), na_codes = 9)$why, "② 的代碼")
  expect_equal(kits_value_codes(c(0, 1, 9999))$why, "7777 以上")
  expect_equal(kits_value_codes(c(1:5, 95, 97, 98, 99))$value, c(95, 97, 98, 99))
  expect_equal(kits_value_codes(c(1:5, 95, 99, 999))$value, c(95, 99, 999))
  # 不是代碼：連續的分數、次數、只有一種值
  expect_equal(nrow(kits_value_codes(0:10)), 0)
  expect_equal(nrow(kits_value_codes(c(0, 5, 10, 30, 99))), 0)
  expect_equal(nrow(kits_value_codes(c(1, 99))), 0)
  expect_equal(nrow(kits_value_codes(c(12.5, 13, 14))), 0)
  expect_equal(kits_code_like(c(0, 1, 2, 9)), 9)
  expect_equal(kits_code_like(c(0, 1, 3)), numeric(0))
})

test_that("A1：TDCYP 的 98、99 不會寫進作答檔；平移 −1 後也不會變成 97、98 分", {
  dfs <- list(
    tibble::tibble(id = 1:6, q1 = kg_labelled(c(1, 2, 3, 4, 98, 99)),
                   q2 = c(1, 2, 3, 4, 5, 97), q3 = c(1, 2, 2, 1, 9999, NA)),
    tibble::tibble(id = 1:3, q1 = kg_labelled(c(4, 99, 2)), q2 = c(5, 1, 2), q3 = c(1, 2, 1))
  )
  map <- kits_guess_map(c("id", "q1"))
  map$subj_id <- "id"
  s <- kits_item_settings(dfs, c("q1", "q2", "q3"))
  expect_equal(s$raw[[1]], 1:4)
  expect_equal(s$codes[[1]]$value, c(98, 99))
  expect_equal(s$codes[[2]]$value, 97)            # 1–5 加上 97：和分數分開
  expect_equal(s$codes[[3]]$why, "7777 以上")      # KIT 的代碼照舊
  expect_equal(s$n_codes, c(3L, 1L, 1L))
  expect_match(kits_settings_alerts(s)[1], "遺漏值代碼 98（值標籤「跳答」）、99")
  # 建議平移 −1（代碼不算最低分以外的分數），平移後代碼仍是代碼
  expect_equal(kits_settings_shift_hint(s), 1)
  s <- kits_bulk_shift(s, s$column, -1) |> kits_bulk_domain(s$column, "tsup")
  spec <- kg_spec(s, map, c("B85", "B86"), typed = list(wave_m = c("0", "12")))
  out <- kits_build(dfs, c("w1.sav", "w2.sav"), spec)
  expect_length(out$problems, 0)
  long <- kits_long_of(out)
  expect_true(all(long$item_score %in% 0:4))
  expect_false(any(long$item_score >= 90))
  w <- paste(out$warnings, collapse = "\n")
  expect_match(w, "3 題有遺漏值代碼，共 5 格")
  expect_match(w, "不是遺漏值代碼")
  expect_no_match(w, "合併")
  # 程式碼：遺漏值代碼改成 NA，結果和 app 相同（SAV 讀進來有值標籤）
  got <- kg_run_code(dfs, spec, s, sav = TRUE)
  expect_match(got$code, "replace(x, x %in% c(98, 99), NA)", fixed = TRUE)
  kg_same_long(got$long, long)
})

test_that("A1：自己改代碼（當成遺漏值、不是遺漏值代碼）；資料變了也照用；設定檔來回", {
  dfs <- list(tibble::tibble(id = 1:5, q1 = c(0, 1, 2, 9, 1), q2 = c(1, 2, 3, 99, 1)))
  s <- kits_item_settings(dfs, c("q1", "q2"))
  expect_equal(nrow(s$codes[[1]]), 0)      # 0–2 加上 9：不確定，不自動當代碼
  expect_match(kits_settings_alerts(s)[1], "9 像遺漏值代碼")
  expect_no_match(kits_settings_alerts(s)[1], "請用「合併」")
  s <- kits_bulk_codes(s, "q1", 9)
  expect_equal(s$raw[[1]], c(0, 1, 2))
  expect_equal(s$codes[[1]]$why, "自己設")
  s <- kits_bulk_uncode(s, "q2")
  expect_equal(s$raw[[2]], c(1, 2, 3, 99))
  expect_equal(s$score[[2]], c(1, 2, 3, 99))
  # 資料變了：使用者決定的代碼照用
  dfs2 <- list(tibble::tibble(id = 1:3, q1 = c(9, 1, 3), q2 = c(1, 99, 2)))
  s2 <- kits_settings_sync(s, kits_item_settings(dfs2, c("q1", "q2")))
  expect_equal(s2$codes[[1]]$value, 9)
  expect_equal(s2$raw[[1]], c(1, 3))
  expect_equal(nrow(s2$codes[[2]]), 0)
  # 設定檔：missing_codes 欄
  ex <- kits_settings_export(s)
  expect_equal(ex$missing_codes, c("9", ""))
  up <- kits_settings_import(ex)
  fresh <- kits_item_settings(dfs, c("q1", "q2"))
  back <- kits_settings_apply(fresh, up)
  expect_equal(back$codes[[1]]$value, 9)
  expect_equal(back$raw[[2]], c(1, 2, 3, 99))
  # 舊的設定檔（沒有 missing_codes）：照自動偵測
  old <- kits_settings_apply(fresh, kits_settings_import(ex[c("item_id", "include", "domain",
                                                              "map_raw_to_score")]))
  expect_equal(old$codes[[2]]$value, 99)
})

test_that("A1：分數有缺而且最高的值像代碼時，提醒不要合併", {
  dfs <- list(tibble::tibble(id = 1:4, q1 = c(0, 1, 9, 0), wave_m = 0))
  s <- kits_item_settings(dfs, "q1") |> kits_bulk_domain("q1", "a")
  map <- kits_guess_map(names(dfs[[1]]))
  map$subj_id <- "id"
  out <- kits_build(dfs, "a.csv", kg_spec(s, map, "B1"))
  w <- paste(out$warnings, collapse = "\n")
  expect_match(w, "最高的值像遺漏值代碼（q1：9）")
  expect_match(w, "請不要用「合併」")
  expect_no_match(w, "用「合併」把分數接起來")
})

test_that("A1：wave_m、age_m 的遺漏值代碼當成空白；程式碼也一樣", {
  dfs <- list(tibble::tibble(id = 1:3, wave_m = c(0, 0, 0), age_m = c(150, 9999, 151),
                             q1 = c(0, 1, 1)))
  map <- kits_guess_map(names(dfs[[1]]))
  map$subj_id <- "id"
  s <- kits_item_settings(dfs, "q1") |> kits_bulk_domain("q1", "a")
  spec <- kg_spec(s, map, "B1")
  out <- kits_build(dfs, "a.csv", spec)
  expect_match(out$warnings, "age_m（age_m）有 1 格是遺漏值代碼 9999", all = FALSE)
  long <- kits_long_of(out)
  expect_true(is.na(long$age_m[long$subj_id == "2"]))
  side <- kits_side_codes(dfs, map[c("wave_m", "age_m")])
  expect_named(side, "age_m")
  got <- kg_run_code(dfs, spec, s)
  expect_no_match(got$code, "9999")   # 沒給 side_codes 時不寫
  code <- code_kitsdata(code_files("w1.csv", "t1"), spec, side_codes = side)
  expect_match(code, 'any_of(c("age_m")), \\(x) replace(x, x %in% 9999, NA)', fixed = TRUE)
})

kg_upload <- function(paths) {
  data.frame(name = basename(paths), size = file.size(paths), type = "", datapath = paths,
             stringsAsFactors = FALSE)
}

test_that("A1：app 裡 ② 的代碼清單（沒按執行也算）、按鈕「當成遺漏值」「不是遺漏值代碼」", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "w1.csv")
  readr::write_csv(tibble::tibble(id = 1:5, wave_m = 0, q1 = c(0, 1, 2, 9, 1),
                                  q2 = c(1, 2, 3, 99, 1)), path)
  shiny::testServer(kit_server, {
    session$setInputs(`upload-files` = kg_upload(path))
    session$setInputs(`kitsdata-map_subj_id` = "id", `kitsdata-map_wave_m` = "wave_m",
                      `kitsdata-cohort_src_f1` = "__text__", `kitsdata-cohort_txt_f1` = "B1")
    table <- output[["kitsdata-settings_table"]]$html
    expect_match(table, "9 像遺漏值代碼")
    expect_match(output[["kitsdata-codes_note"]]$html, "1 格是遺漏值代碼")   # q2 的 99
    session$setInputs(`missing-codes` = "9, 99")
    table <- output[["kitsdata-settings_table"]]$html
    expect_match(table, "遺漏值代碼 9（② 的代碼）")
    expect_match(output[["kitsdata-codes_note"]]$html, "2 格是遺漏值代碼")
    session$setInputs(`kitsdata-sel` = "q2", `kitsdata-do_uncode` = 1)
    expect_no_match(output[["kitsdata-settings_table"]]$html, "99（")
    session$setInputs(`missing-codes` = "7777")
    session$setInputs(`kitsdata-sel` = "q1", `kitsdata-merge_from` = "9",
                      `kitsdata-do_code` = 1)
    expect_match(output[["kitsdata-settings_table"]]$html, "遺漏值代碼 9（自己設）")
    expect_match(output[["kitsdata-code"]]$html, "replace(x, x %in% 9, NA)", fixed = TRUE)
  })
})
