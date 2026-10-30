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

# ── C1：各檔各選一欄（ID、波次、時間點、性別欄名各檔不同） ─────────────────

# TDCYP 的樣子：w1_id、w2_id、w3id；時間點欄、性別欄名稱也不同；第三個檔案沒有性別
kg_three <- function() {
  list(
    tibble::tibble(w1_id = c(100000, 2, 3), w1_mon = 0, w1_sex = c("boy", "girl", "boy"),
                   q1 = c(0, 1, 2), q2 = c(1, 1, 0)),
    tibble::tibble(w2_id = c(100000, 3), w2_mon = 12, w2_sex = c("boy", "boy"),
                   q1 = c(1, 2), q2 = c(0, 1)),
    tibble::tibble(w3id = c("2", "3"), w3_mon = c(24, 24), q1 = c(2, 2), q2 = c(1, 0))
  )
}

kg_three_spec <- function(dfs, per_file) {
  map <- kits_guess_map(character(0))
  s <- kits_item_settings(dfs, c("q1", "q2")) |> kits_bulk_domain(c("q1", "q2"), "a")
  kg_spec(s, map, c("B85", "B85", "B85"), per_file = per_file)
}

test_that("C1：猜的來源：各檔 ID 欄名不同時是「各檔各選一欄」；w3id 也猜得到", {
  dfs <- kg_three()
  expect_equal(kits_guess_id(c("w1_id", "w3id", "w4id", "idno", "width", "paid")),
               c("w1_id", "w3id", "w4id", "idno"))
  each <- kits_guess_per_file(dfs)
  expect_equal(each$subj_id, c("w1_id", "w2_id", "w3id"))
  src <- kits_guess_sources(dfs)
  expect_equal(src[["subj_id"]], "per_file")
  expect_equal(src[["wave_m"]], "column")   # 猜不到：必填的欄還是「某一欄」
  # 各檔欄名相同時照舊是「某一欄」
  same <- list(tibble::tibble(id = 1, wave_m = 3), tibble::tibble(id = 2, wave_m = 6))
  expect_equal(kits_guess_sources(same)[c("subj_id", "wave_m")],
               c(subj_id = "column", wave_m = "column"))
})

test_that("C1：每個檔案用自己的 ID、時間點、性別欄；結果和「先改成同一個欄名」相同", {
  dfs <- kg_three()
  per_file <- list(subj_id = c("w1_id", "w2_id", "w3id"),
                   wave_m = c("w1_mon", "w2_mon", "w3_mon"), sex = c("w1_sex", "w2_sex", ""))
  spec <- kg_three_spec(dfs, per_file)
  out <- kits_build(dfs, c("w1.sav", "w2.sav", "w3.sav"), spec)
  expect_length(out$problems, 0)
  long <- kits_long_of(out)
  expect_setequal(unique(long$visit_id),
                  c("B85_100000_0", "B85_2_0", "B85_3_0", "B85_100000_12", "B85_3_12",
                    "B85_2_24", "B85_3_24"))
  expect_true(all(is.na(long$sex[long$wave_m == 24])))
  expect_equal(unique(long$sex[long$visit_id == "B85_2_0"]), "girl")
  expect_false(any(c("w1_mon", "w2_sex") %in% long$item_id))

  # 同樣的資料，先把欄名改成一樣、用「某一欄」：寫出來完全相同
  renamed <- purrr::map(seq_along(dfs), function(i) {
    df <- dfs[[i]]
    names(df)[names(df) == per_file$subj_id[i]] <- "id"
    names(df)[names(df) == per_file$wave_m[i]] <- "wave_m"
    if (nzchar(per_file$sex[i])) names(df)[names(df) == per_file$sex[i]] <- "sex" else
      df$sex <- NA_character_
    df
  })
  map <- kits_guess_map(c("id", "wave_m", "sex"))
  map$subj_id <- "id"
  s <- kits_item_settings(renamed, c("q1", "q2")) |> kits_bulk_domain(c("q1", "q2"), "a")
  long2 <- kits_long_of(kits_build(renamed, c("a", "b", "c"), kg_spec(s, map, rep("B85", 3))))
  kg_same_long(long2, long)

  # 程式碼：每個檔案做出 subj_id、wave_m、sex，再合併；和 app 相同
  got <- kg_run_code(dfs, spec, kits_item_settings(dfs, c("q1", "q2")) |>
                       kits_bulk_domain(c("q1", "q2"), "a"))
  expect_match(got$code, "subj_id = as_id(w3id)", fixed = TRUE)
  expect_match(got$code, "wave_m = as.numeric(w2_mon)", fixed = TRUE)
  kg_same_long(got$long, long)
})

test_that("C1：各檔各選一欄沒選、選錯；一個欄名套到沒有這欄的檔案時提示「各檔各選一欄」", {
  dfs <- kg_three()
  spec <- kg_three_spec(dfs, list(subj_id = c("w1_id", "", "w3id"),
                                  wave_m = c("w1_mon", "w2_mon", "nope")))
  p <- kits_build(dfs, c("w1", "w2", "w3"), spec)$problems
  expect_match(p, "w2：還沒有選孩子編號（subj_id）的欄", all = FALSE)
  expect_match(p, "w3：沒有.*（wave_m）的欄 nope", all = FALSE)

  spec <- kg_three_spec(dfs, list(wave_m = c("w1_mon", "w2_mon", "w3_mon")))
  spec$map$subj_id <- "w1_id"
  p <- kits_build(dfs, c("w1", "w2", "w3"), spec)$problems
  expect_match(p, "w2：沒有對應的欄 w1_id。各檔欄名不同時，請在「2. 對應欄位」選「各檔各選一欄」",
               all = FALSE)
})

test_that("C1：波次標籤也可以各檔各選一欄", {
  dfs <- kg_three()
  dfs[[1]]$lab1 <- "W1"
  dfs[[2]]$lab2 <- "W2"
  dfs[[3]]$lab3 <- "W3"
  per_file <- list(subj_id = c("w1_id", "w2_id", "w3id"), wave = c("lab1", "lab2", "lab3"),
                   wave_m = c("w1_mon", "w2_mon", "w3_mon"))
  spec <- kg_three_spec(dfs, per_file)
  out <- kits_build(dfs, c("a", "b", "c"), spec)
  expect_match(out$notes[1], "各檔選的波次標籤欄")
  long <- kits_long_of(out)
  expect_true(all(c("B85_100000_W1", "B85_3_W3") %in% long$visit_id))
  expect_false(any(c("lab1", "lab2") %in% long$item_id))
  got <- kg_run_code(dfs, spec, kits_item_settings(dfs, c("q1", "q2")) |>
                       kits_bulk_domain(c("q1", "q2"), "a"))
  kg_same_long(got$long, long)
})

test_that("C1：app 裡選「各檔各選一欄」，「1.」的表裡每個檔案各選一欄", {
  dir <- withr::local_tempdir()
  dfs <- kg_three()
  paths <- file.path(dir, c("w1.csv", "w2.csv", "w3.csv"))
  for (i in 1:3) readr::write_csv(dfs[[i]], paths[i])
  shiny::testServer(kit_server, {
    session$setInputs(`upload-files` = kg_upload(paths))
    session$setInputs(`kitsdata-src_subj_id` = "per_file", `kitsdata-src_wave_m` = "per_file",
                      `kitsdata-src_sex` = "none")
    html <- output[["kitsdata-cohort_table"]]$html
    expect_match(html, "kitsdata-pf_subj_id_f3")
    expect_match(html, '<option value="w3id" selected>')
    # 對應到的欄不是題目
    table <- output[["kitsdata-settings_table"]]$html
    expect_no_match(table, 'value="w2_id"')
    session$setInputs(`kitsdata-pf_wave_m_f1` = "w1_mon", `kitsdata-pf_wave_m_f2` = "w2_mon",
                      `kitsdata-pf_wave_m_f3` = "w3_mon")
    expect_no_match(output[["kitsdata-settings_table"]]$html, 'value="w2_mon"')
    expect_match(output[["kitsdata-visit_note"]]$html, "\\{wave_m\\}")
    for (k in 1:3) {
      do.call(session$setInputs, stats::setNames(
        list("__text__", "B85"), paste0("kitsdata-cohort_", c("src", "txt"), "_f", k)
      ))
    }
    session$setInputs(`kitsdata-sel` = c("q1", "q2", "w1_sex", "w2_sex"),
                      `kitsdata-bulk_domain` = "a", `kitsdata-do_domain` = 1)
    session$setInputs(`kitsdata-sel` = c("w1_sex", "w2_sex"), `kitsdata-do_exclude` = 1)
    session$setInputs(`kitsdata-run` = 1)
    expect_match(output[["kitsdata-status"]]$html, "檢查完成")
    expect_match(output[["kitsdata-code"]]$html, "subj_id = as_id(w3id)", fixed = TRUE)
  })
})

test_that("C1：TDCYP W1–W3 jsq 猜得到各檔的 ID 欄（有檔案時才跑）", {
  paths <- kg_tdcyp(sprintf("w%d_jsq.sav", 1:3))
  skip_if_not(all(file.exists(paths)))
  dfs <- purrr::map(paths, function(p) read_data_file(p, basename(p))$data)
  expect_equal(kits_guess_per_file(dfs)$subj_id, c("w1_id", "w2_id", "w3id"))
  expect_equal(kits_guess_sources(dfs)[["subj_id"]], "per_file")
})

# ── C2：輸出題號（跨波次同一題給同一個 item_id） ─────────────────────────────

kg_waves <- function() {
  list(
    tibble::tibble(id = 1:3, w1d5_1 = c(0, 1, 2), w1d5_2 = c(1, 1, 0), w1c21_5 = c(0, 1, 1)),
    tibble::tibble(id = 1:2, w2d5_1 = c(1, 2), w2d5_2 = c(0, 0)),
    tibble::tibble(id = 2:3, w3c5_1 = c(2, 2), w3c5_2 = c(1, 0), w3d32_5 = c(1, 1))
  )
}

kg_waves_spec <- function(dfs, s) {
  map <- kits_guess_map("id")
  kg_spec(s, map, rep("B85", 3), typed = list(wave_m = c("0", "12", "24")))
}

test_that("C2：去掉 ^w[0-9]+；題目表的 item_id_out；作答檔用輸出題號，程式碼也一樣", {
  dfs <- kg_waves()
  cols <- unique(unlist(purrr::map(dfs, function(df) setdiff(names(df), "id"))))
  s <- kits_item_settings(dfs, cols) |> kits_bulk_domain(cols, "bsrs")
  s <- kits_bulk_out_prefix(s, cols, "^w[0-9]+")
  expect_equal(s$item_id_out[s$column == "w2d5_1"], "d5_1")
  expect_error(kits_bulk_out_prefix(s, cols, "("), "看不懂")
  expect_error(kits_bulk_out_prefix(s, cols, "^.*$"), "空白")
  # 題號換了的（W3 的 c5_ 是 W1 的 d5_）：題目表給 item_id_out；w3d32_5 不給（保留欄名）
  tab <- kits_settings_import(data.frame(
    item_id = c("w3c5_1", "w3c5_2", "w3d32_5"), item_id_out = c("d5_1", "d5_2", "")
  ))
  expect_equal(tab$type, "table")
  s <- kits_settings_apply(s, tab)
  s <- kits_bulk_out_reset(s, "w3d32_5")
  expect_equal(s$item_id_out[s$column %in% c("w3c5_1", "w3d32_5")], c("d5_1", NA))
  expect_match(kitsdata_settings_table("x", s), "→ 題號 <code>d5_1</code>", fixed = TRUE)

  spec <- kg_waves_spec(dfs, s)
  out <- kits_build(dfs, c("w1", "w2", "w3"), spec)
  expect_length(out$problems, 0)
  long <- kits_long_of(out)
  expect_setequal(unique(long$item_id), c("d5_1", "d5_2", "c21_5", "w3d32_5"))
  expect_equal(out$n_items, 4)
  w <- paste(out$warnings, collapse = "\n")
  expect_match(w, "2 個輸出題號由不同的欄合成（d5_1 ← w1d5_1、w2d5_1、w3c5_1")
  expect_match(w, "同一個題號在不同波次可能是不同的題")

  got <- kg_run_code(dfs, spec, s)
  expect_match(got$code, "mutate(item_id = coalesce(item_id_out, item_id))", fixed = TRUE)
  kg_same_long(got$long, long)

  # 設定檔來回：item_id_out 也回來
  ex <- kits_settings_export(s)
  back <- kits_settings_apply(kits_item_settings(dfs, cols), kits_settings_import(ex))
  expect_equal(back$item_id_out, s$item_id_out)
})

test_that("C2：同一個檔案裡兩欄變成同一個題號時擋下；同一個題號兩個領域也擋下", {
  dfs <- kg_waves()
  cols <- c("w1d5_1", "w1c21_5")
  s <- kits_item_settings(dfs[1], cols) |> kits_bulk_domain(cols, "a")
  s$item_id_out <- c("x1", "x1")
  out <- kits_build(dfs[1], "w1.sav", kg_waves_spec(dfs[1], s))
  expect_match(out$problems, "w1.sav：w1d5_1、w1c21_5 都輸出成題號 x1", all = FALSE)

  s <- kits_item_settings(dfs[1:2], c("w1d5_1", "w2d5_1")) |>
    kits_bulk_domain("w1d5_1", "a") |> kits_bulk_domain("w2d5_1", "b") |>
    kits_bulk_out_prefix(c("w1d5_1", "w2d5_1"), "^w[0-9]+")
  spec <- kg_waves_spec(dfs[1:2], s)
  spec$typed$wave_m <- c("0", "12")
  out <- kits_build(dfs[1:2], c("a", "b"), spec)
  expect_match(out$problems, "對到不同的領域：d5_1", all = FALSE)
})

test_that("C2：變項標籤不同的欄合成同一個題號時，特別提醒", {
  dfs <- kg_waves()
  attr(dfs[[1]]$w1c21_5, "label") <- "W1:家扶老師很關心我"
  attr(dfs[[3]]$w3d32_5, "label") <- "W3:學校老師很關心我"
  cols <- c("w1c21_5", "w3d32_5")
  s <- kits_item_settings(dfs, cols) |> kits_bulk_domain(cols, "tsup")
  s$item_id_out <- c("tsup5", "tsup5")
  out <- kits_build(dfs, c("a", "b", "c"), kg_waves_spec(dfs, s))
  expect_match(out$warnings, "變項標籤不一樣，請特別確認：tsup5", all = FALSE)
})

test_that("C2：app 裡用「去掉」改輸出題號，摘要與表都看得到", {
  dir <- withr::local_tempdir()
  dfs <- kg_waves()[1:2]
  paths <- file.path(dir, c("w1.csv", "w2.csv"))
  for (i in 1:2) readr::write_csv(dfs[[i]], paths[i])
  shiny::testServer(kit_server, {
    session$setInputs(`upload-files` = kg_upload(paths))
    session$setInputs(`kitsdata-sel` = c("w1d5_1", "w2d5_1"), `kitsdata-out_pattern` = "^w[0-9]+",
                      `kitsdata-do_out_prefix` = 1)
    expect_match(output[["kitsdata-settings_table"]]$html, "→ 題號 <code>d5_1</code>")
    expect_match(output[["kitsdata-settings_summary"]]$html, "2 題改了輸出題號，合成 1 個題號")
    session$setInputs(`kitsdata-sel` = c("w1d5_1", "w1d5_2"), `kitsdata-out_pattern` = "[0-9]$",
                      `kitsdata-do_out_prefix` = 2)
    expect_match(output[["kitsdata-settings_summary"]]$html, "都輸出成題號 w1d5_")
  })
})

# ── C8：反向題的欄名規則可以設定 ────────────────────────────────────────────

test_that("C8：反向題規則：結尾 r（預設，KIT）、開頭 r（TDCYP）、不用", {
  cols <- c("cogd01r", "w1ra8", "ra8", "w1st_hour", "lana01")
  expect_equal(kits_reverse_base(cols), c("cogd01", NA, NA, "w1st_hou", NA))
  expect_equal(kits_reverse_base(cols, "prefix"), c(NA, "w1a8", "a8", NA, NA))
  expect_true(all(is.na(kits_reverse_base(cols, "none"))))
  tab <- tibble::tibble(item_id = c("cogd01", "w1a8", "a8", "w1st_hou"),
                        domain = c("cog", "self", "self", "time"))
  expect_equal(kits_item_domains(cols, tab)$domain, c("cog", NA, NA, "time", NA))
  expect_equal(kits_item_domains(cols, tab, "prefix")$domain, c(NA, "self", "self", NA, NA))
  expect_equal(kits_item_domains(cols, tab, "none")$domain, rep(NA_character_, 5))

  dfs <- list(tibble::tibble(id = 1:2, w1a8 = c(1, 2), w1ra8 = c(2, 1)))
  up <- list(type = "table", data = tibble::tibble(item_id = "w1a8", domain = "self"))
  s <- kits_settings_apply(kits_item_settings(dfs, c("w1a8", "w1ra8")), up)
  expect_equal(s$domain, c("self", NA))
  s <- kits_settings_apply(kits_item_settings(dfs, c("w1a8", "w1ra8")), up, reverse = "prefix")
  expect_equal(s$domain, c("self", "self"))
  expect_match(s$source[2], "原題 w1a8")
})

test_that("C8：app 裡換反向題規則，領域跟著換", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "w1.csv")
  readr::write_csv(tibble::tibble(id = 1:2, w1a8 = c(1, 2), w1ra8 = c(2, 1),
                                  w1st_hour = c(1, 2)), path)
  tab <- file.path(dir, "items.csv")
  readr::write_csv(tibble::tibble(item_id = c("w1a8", "w1st_hou"), domain = c("self", "time")),
                   tab)
  shiny::testServer(kit_server, {
    session$setInputs(`upload-files` = kg_upload(path))
    session$setInputs(`kitsdata-item_file` = kg_upload(tab))
    html <- output[["kitsdata-settings_table"]]$html
    expect_match(html, "原題 w1st_hou")    # 預設：結尾 r
    expect_no_match(html, "原題 w1a8")
    session$setInputs(`kitsdata-reverse_rule` = "prefix")
    html <- output[["kitsdata-settings_table"]]$html
    expect_match(html, "原題 w1a8")
    expect_no_match(html, "原題 w1st_hou")
  })
})
