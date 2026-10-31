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
  expect_equal(kits_value_codes(c(0, 1, 2, 9), na_codes = 9)$why, "「遺漏值處理」的代碼")
  expect_equal(kits_value_codes(c(0, 1, 9999))$why, "7777 以上")
  expect_equal(kits_value_codes(c(1:5, 95, 97, 98, 99))$value, c(95, 97, 98, 99))
  expect_equal(kits_value_codes(c(1:5, 95, 99, 999))$value, c(95, 99, 999))
  # 不是代碼：連續的分數、次數、只有一種值
  expect_equal(nrow(kits_value_codes(0:10)), 0)
  expect_equal(nrow(kits_value_codes(c(0, 5, 10, 30, 99))), 0)
  expect_equal(nrow(kits_value_codes(c(1, 99))), 0)
  expect_equal(nrow(kits_value_codes(c(12.5, 13, 14))), 0)
  # 「不知道」「不清楚」「don't know」不是疑似代碼（和 ② 的預設關鍵字一樣）
  expect_equal(nrow(kits_value_codes(c(0, 1, 2, 3), c("3" = "不知道或不清楚"))), 0)
  expect_equal(nrow(kits_value_codes(c(0, 1, 7), c("7" = "Don't know"))), 0)
  expect_equal(kits_value_codes(c(0, 1, 7), c("7" = "Refused"))$value, 7)
  expect_equal(kits_code_like(c(0, 1, 2, 9)), 9)
  expect_equal(kits_code_like(c(0, 1, 3)), numeric(0))
})

test_that("A1：TDCYP 的 98、99 照原樣寫進作答檔並提醒到 ②；平移 −1 不會動到它們", {
  dfs <- list(
    tibble::tibble(id = 1:6, q1 = kg_labelled(c(1, 2, 3, 4, 98, 99)),
                   q2 = c(1, 2, 3, 4, 5, 97), q3 = c(1, 2, 2, 1, 9999, NA)),
    tibble::tibble(id = 1:3, q1 = kg_labelled(c(4, 99, 2)), q2 = c(5, 1, 2), q3 = c(1, 2, 1))
  )
  map <- kits_guess_map(c("id", "q1"))
  map$subj_id <- "id"
  s <- kits_item_settings(dfs, c("q1", "q2", "q3"))
  expect_equal(s$raw[[1]], 1:4)                   # 代碼不在計分對照裡
  expect_equal(s$codes[[1]]$value, c(98, 99))
  expect_equal(s$codes[[2]]$value, 97)            # 1–5 加上 97：和分數分開
  expect_equal(s$codes[[3]]$why, "7777 以上")      # KIT 的代碼照舊
  expect_equal(s$n_codes, c(3L, 1L, 1L))
  expect_match(kits_settings_alerts(s)[1],
               "疑似遺漏值代碼 98（值標籤「跳答」）、99（值標籤「遺漏值」）：會照原樣輸出")
  # 建議平移 −1（代碼不算）；平移只動計分對照裡的分數
  expect_equal(kits_settings_shift_hint(s), 1)
  s <- kits_bulk_shift(s, s$column, -1) |> kits_bulk_domain(s$column, "tsup")
  spec <- kg_spec(s, map, c("B85", "B86"), typed = list(wave_m = c("0", "12")))
  out <- kits_build(dfs, c("w1.sav", "w2.sav"), spec)
  expect_length(out$problems, 0)
  long <- kits_long_of(out)
  # 預設資料是對的：98、99、97、9999 照原樣寫進作答檔（不會變成 97、98 分，也不會不見）
  expect_setequal(long$item_score[long$item_id == "q1"], c(0:3, 98, 99))
  expect_setequal(long$item_score[long$item_id == "q2"], c(0:4, 97))
  expect_setequal(long$item_score[long$item_id == "q3"], c(0, 1, 9999))
  expect_equal(sum(long$item_score %in% c(97, 98, 99, 9999)), 5)
  w <- paste(out$warnings, collapse = "\n")
  expect_match(w, "3 題有疑似遺漏值代碼，共 5 格")
  expect_match(w, "q1：98（值標籤「跳答」）、99（值標籤「遺漏值」）")
  expect_match(w, "會照原樣寫進作答檔")
  expect_match(w, "請先到「遺漏值處理」把它們改成 NA")
  expect_no_match(w, "合併")
  expect_no_match(w, "中間有缺")                   # 代碼不算分數的空隙
  # 狀態不是綠色的「完成」
  expect_equal(kit_done_status("x", warnings = out$warnings)$state, "warning")
  # 程式碼：不改成 NA；平移時代碼照原樣；結果和 app 相同（SAV 讀進來有值標籤）
  got <- kg_run_code(dfs, spec, s, sav = TRUE)
  expect_no_match(got$code, "replace(x", fixed = TRUE)
  expect_match(got$code, "# 注意：有 3 題有疑似遺漏值代碼（97、98、99、9999）", fixed = TRUE)
  expect_match(got$code, "case_when(x %in% c(98, 99) ~ x, .default = x - 1)", fixed = TRUE)
  expect_match(got$code, "case_when(x %in% 9999 ~ x, .default = x - 1)", fixed = TRUE)
  kg_same_long(got$long, long)

  # 先用 ② 依數值標籤改成 NA：q1 的 98、99 不見了，也不再提醒 q1
  kw <- parse_na_keywords(kit_na_label_keywords_default())
  dfs2 <- purrr::map(dfs, function(df) replace_na_by_labels(df, kw, exclude = "id")$data)
  s2 <- kits_settings_sync(s, kits_item_settings(dfs2, c("q1", "q2", "q3")))
  expect_equal(nrow(s2$codes[[1]]), 0)
  out2 <- kits_build(dfs2, c("w1.sav", "w2.sav"),
                     kg_spec(s2, map, c("B85", "B86"), typed = list(wave_m = c("0", "12"))))
  long2 <- kits_long_of(out2)
  expect_setequal(long2$item_score[long2$item_id == "q1"], 0:3)
  expect_match(paste(out2$warnings, collapse = "\n"), "2 題有疑似遺漏值代碼，共 2 格")
})

test_that("A1：沒有「當成遺漏值」按鈕；舊設定檔的 missing_codes 欄略過；設定檔來回", {
  dfs <- list(tibble::tibble(id = 1:5, q1 = c(0, 1, 2, 9, 1), q2 = c(1, 2, 3, 99, 1)))
  s <- kits_item_settings(dfs, c("q1", "q2"))
  expect_equal(nrow(s$codes[[1]]), 0)      # 0–2 加上 9：不確定，不算疑似代碼
  expect_match(kits_settings_alerts(s)[1], "9 像遺漏值代碼：請先用「遺漏值處理」改成 NA，不要合併")
  expect_no_match(kits_settings_alerts(s)[1], "請用「合併」")
  expect_equal(s$codes[[2]]$value, 99)
  expect_false(exists("kits_bulk_codes", mode = "function"))
  expect_no_match(as.character(mod_kitsdata_ui("kitsdata")), "do_code|do_uncode|當成遺漏值（不輸出）|不是遺漏值代碼")
  # 設定檔：沒有 missing_codes 欄
  s <- kits_bulk_shift(s, "q2", -1)
  ex <- kits_settings_export(s)
  expect_null(ex[["missing_codes"]])
  fresh <- kits_item_settings(dfs, c("q1", "q2"))
  back <- kits_settings_apply(fresh, kits_settings_import(ex))
  expect_equal(back$score[[2]], c(0, 1, 2))
  # 0.5.0 以前的設定檔（有 missing_codes）也讀得進來，這一欄略過：9 不會被拿掉
  old <- ex
  old$missing_codes <- c("9", "99")
  back <- kits_settings_apply(fresh, kits_settings_import(old))
  expect_equal(back$raw[[1]], c(0, 1, 2, 9))
  expect_equal(back$score[[2]], c(0, 1, 2))
  expect_equal(back$codes[[2]]$value, 99)   # 自動偵測照舊（只提醒）
})

test_that("A1：分數有缺而且最高的值像代碼時，提醒不要合併、先到 ②", {
  dfs <- list(tibble::tibble(id = 1:4, q1 = c(0, 1, 9, 0), wave_m = 0))
  s <- kits_item_settings(dfs, "q1") |> kits_bulk_domain("q1", "a")
  map <- kits_guess_map(names(dfs[[1]]))
  map$subj_id <- "id"
  out <- kits_build(dfs, "a.csv", kg_spec(s, map, "B1"))
  w <- paste(out$warnings, collapse = "\n")
  expect_match(w, "最高的值像遺漏值代碼（q1：9）")
  expect_match(w, "請不要用「合併」")
  expect_match(w, "請先到「遺漏值處理」")
  expect_no_match(w, "用「合併」把分數接起來")
  expect_no_match(w, "當成遺漏值")
})

test_that("A1：性別、wave_m、age_m 的疑似遺漏值代碼照原樣輸出並提醒；程式碼也一樣", {
  dfs <- list(tibble::tibble(id = 1:3, wave_m = c(0, 0, 0), age_m = c(150, 9999, 151),
                             q1 = c(0, 1, 1)))
  map <- kits_guess_map(names(dfs[[1]]))
  map$subj_id <- "id"
  s <- kits_item_settings(dfs, "q1") |> kits_bulk_domain("q1", "a")
  spec <- kg_spec(s, map, "B1")
  out <- kits_build(dfs, "a.csv", spec)
  expect_match(out$warnings, "age_m（age_m）有 1 格疑似遺漏值代碼 9999（7777 以上）",
               all = FALSE)
  expect_match(out$warnings, "遺漏值處理", all = FALSE)
  long <- kits_long_of(out)
  expect_equal(unique(long$age_m[long$subj_id == "2"]), 9999)
  expect_named(kits_side_codes(dfs, map[c("wave_m", "age_m")]), "age_m")
  got <- kg_run_code(dfs, spec, s)
  expect_no_match(got$code, "9999")
  kg_same_long(got$long, long)
})

kg_upload <- function(paths) {
  data.frame(name = basename(paths), size = file.size(paths), type = "", datapath = paths,
             stringsAsFactors = FALSE)
}

test_that("A1：app 裡「遺漏值處理」的代碼清單只用來提醒；要決定照原樣當分數或改掉；用了「遺漏值處理」就沒有提醒", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "w1.csv")
  readr::write_csv(tibble::tibble(id = 1:5, wave_m = 0, q1 = c(0, 1, 2, 9, 1),
                                  q2 = c(1, 2, 3, 99, 1)), path)
  shiny::testServer(kit_server, {
    session$setInputs(`upload-files` = kg_upload(path))
    kd_cells(session, "cohort", "B1")
    kd_cells(session, "subj_id", "id")
    kd_click(session, "ok1")
    kd_pick(session, "q", "a")
    kd_click(session, "ok2")
    expect_match(kd_out(output, "items_table"), "q1 a 0 1 2 9 分數有缺")
    expect_match(kd_out(output, "items_table"), "q2 a 1 2 3 代碼 99")   # 99：和分數分開
    expect_equal(kd_check_state(output, "遺漏值代碼"), "todo")
    expect_match(kd_out(output, "checks"), "1 欄有遺漏值代碼 99（共 1 格）")
    expect_match(output[["kitsdata-checks"]]$html, "kitsdata-goto_missing")
    session$setInputs(`missing-codes` = "9, 99")   # 「遺漏值處理」還沒執行：只拿來找代碼
    session$elapse(1000)   # 代碼清單停 0.8 秒才更新（debounce）
    expect_match(kd_out(output, "items_table"), "q1 a 0 1 2 代碼 9")
    expect_match(kd_out(output, "checks"), "2 欄有遺漏值代碼 9、99（共 2 格）")
    kd_click(session, "run")
    expect_match(kd_out(output, "status"), "還不能轉換")   # 要先決定
    kd_click(session, "ack_codes")
    kd_click(session, "run")
    # 已經在第 3 步決定了：結果不再重複提醒代碼；9、99 照原樣寫進作答檔
    st <- kd_out(output, "status")
    expect_match(st, "完成")
    expect_no_match(st, "疑似遺漏值代碼")
    expect_no_match(output[["kitsdata-code"]]$html, "replace(x", fixed = TRUE)
    long <- readr::read_csv(kit_test_download_file(session, "kitsdata-download_csv"),
                            col_types = "ccccddcic")
    expect_true(all(c(9, 99) %in% long$item_score))
    # 用「遺漏值處理」改成 NA、再回來：沒有提醒，9、99 不在作答檔
    session$setInputs(`missing-exclude_cols` = "id", `missing-use_downstream` = TRUE)
    session$setInputs(`missing-run` = 1)
    expect_equal(kd_check_state(output, "遺漏值"), "ok")
    expect_match(kd_out(output, "checks"), "遺漏值代碼已經用「遺漏值處理」改成 NA")
    expect_null(output[["kitsdata-result"]])         # 資料換了：舊結果清除
    kd_click(session, "run")
    st <- kd_out(output, "status")
    expect_match(st, "完成")
    expect_no_match(st, "疑似遺漏值代碼")
    long <- readr::read_csv(kit_test_download_file(session, "kitsdata-download_csv"),
                            col_types = "ccccddcic")
    expect_false(any(c(9, 99) %in% long$item_score))
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
  expect_match(p, "w2：沒有對應的欄 w1_id。各檔欄名不同時，請在第 1 步的表裡替每個檔案各選一欄",
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

test_that("C1：app 的第 1 步每個檔案一列：各檔猜自己的 ID 欄，時間點、性別各檔各選一欄", {
  dir <- withr::local_tempdir()
  dfs <- kg_three()
  paths <- file.path(dir, c("w1.csv", "w2.csv", "w3.csv"))
  for (i in 1:3) readr::write_csv(dfs[[i]], paths[i])
  shiny::testServer(kit_server, {
    session$setInputs(`upload-files` = kg_upload(paths))
    html <- output[["kitsdata-files_table"]]$html
    expect_match(html, "kitsdata-cell_subj_id_f3")
    expect_match(html, '<option value="w3id" selected>')
    expect_match(html, '<option value="w1_id" selected>')
    kd_cells(session, "wave_m", c("w1_mon", "w2_mon", "w3_mon"))
    kd_cells(session, "sex", c("w1_sex", "w2_sex", ""))
    kd_cells(session, "cohort", rep("B85", 3))
    kd_click(session, "ok1")
    expect_equal(kd_out(output, "step1_problems"), "")
    expect_equal(kd_head_state(output, 1), "done")
    expect_match(kd_out(output, "head1"), "孩子編號 w1_id 等 3 欄")
    expect_match(kd_out(output, "visit_note"), "B85_100000_0")
    # 對應到的欄不是題目
    kd_set(session, tbl_all = TRUE)
    table <- output[["kitsdata-items_table"]]$html
    expect_match(table, 'value="q1"')
    expect_no_match(table, 'value="w2_id"')
    expect_no_match(table, 'value="w2_mon"')
    kd_pick(session, "q", "a")
    kd_click(session, "ok2")
    kd_click(session, "run")
    expect_match(kd_out(output, "status"), "完成")
    expect_match(output[["kitsdata-code"]]$html, "subj_id = as_id(w3id)", fixed = TRUE)
    expect_match(output[["kitsdata-code"]]$html, "wave_m = as.numeric(w2_mon)", fixed = TRUE)
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
  expect_match(as.character(kits_items_page_html("x", s)), "→ 題號 d5_1</span>", fixed = TRUE)

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
  s <- kits_bulk_shift(s, cols, 1)   # 計分對照之後欄就沒有變項標籤：要在之前比
  out <- kits_build(dfs, c("a", "b", "c"), kg_waves_spec(dfs, s))
  expect_match(out$warnings, "變項標籤不一樣，請特別確認：tsup5", all = FALSE)
})

test_that("C2：app 裡用「去掉」改輸出題號，表與題目清單都看得到；同檔撞號會提醒", {
  dir <- withr::local_tempdir()
  dfs <- kg_waves()[1:2]
  paths <- file.path(dir, c("w1.csv", "w2.csv"))
  for (i in 1:2) readr::write_csv(dfs[[i]], paths[i])
  shiny::testServer(kit_server, {
    session$setInputs(`upload-files` = kg_upload(paths))
    kd_pick(session, "w", "bsrs")
    expect_match(kd_out(output, "items_status"), "挑出 5 欄")
    kd_set(session, sel = c("w1d5_1", "w2d5_1"), bulk_action = "out_prefix",
           out_pattern = "^w[0-9]+")
    kd_click(session, "bulk_apply")
    expect_match(output[["kitsdata-items_table"]]$html, "→ 題號 d5_1</span>", fixed = TRUE)
    expect_match(kd_out(output, "items_status"), "挑出 5 欄、3 組 → 合成 4 題")
    expect_match(kd_out(output, "items_list"), "d5_1 w1d5_1、w2d5_1")
    expect_match(output[["kitsdata-items_title"]], "（5 欄 · bsrs 4 題）", fixed = TRUE)
    kd_set(session, sel = c("w1d5_1", "w1d5_2"), out_pattern = "[0-9]$")
    kd_click(session, "bulk_apply")
    expect_match(kd_out(output, "items_status"), "都輸出成題號 w1d5_")
    kd_set(session, bulk_action = "out_reset")
    kd_click(session, "bulk_apply")
    expect_no_match(kd_out(output, "items_status"), "都輸出成題號")
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
    # 預設：結尾 r（w1st_hour 是 w1st_hou 的反向題）
    expect_match(kd_out(output, "items_table"), "w1st_hour time")
    expect_no_match(kd_out(output, "items_list"), "w1ra8")
    expect_match(kd_out(output, "checks"), "1 欄是反向題，用原題的領域")
    session$setInputs(`kitsdata-reverse_rule` = "prefix")
    expect_match(kd_out(output, "items_table"), "w1ra8 self")
    expect_no_match(kd_out(output, "items_table"), "w1st_hour time")
    # 拿掉的領域變回預設的組名
    expect_match(kd_out(output, "items_table"), "w1st_hour w1st_hour （組名）")
    expect_match(kd_out(output, "checks"), "1 欄是反向題，用原題的領域")
  })
})

# ── C4（⑥）與發現 10：wave_m 用語、時間點與 visit_id 的檢查 ───────────────────

kg_one <- function(...) {
  df <- tibble::tibble(id = c("1", "2"), wave_m = c(0, 12), q1 = c(0, 1), ...)
  map <- kits_guess_map(names(df))
  map$subj_id <- "id"
  s <- kits_item_settings(list(df), "q1") |> kits_bulk_domain("q1", "a")
  list(df = df, map = map, s = s)
}

test_that("C4：wave_m 不寫死「施測月齡」；單位可以選，預設是月", {
  expect_match(KITS_COLUMNS$meaning[KITS_COLUMNS$column == "wave_m"], "時間點")
  expect_no_match(KITS_COLUMNS$meaning[KITS_COLUMNS$column == "wave_m"], "^波次（施測月齡）")
  expect_equal(KITS_MAP_ROWS$label[KITS_MAP_ROWS$field == "wave_m"], "時間點")
  ui <- as.character(mod_kitsdata_ui("kitsdata"))
  expect_match(ui, "波次的單位")
  expect_match(ui, '<option value="month" selected>')
  shiny::testServer(kit_server, {
    expect_null(output[["kitsdata-wave_unit_note"]])
    session$setInputs(`kitsdata-wave_unit` = "year")
    expect_match(output[["kitsdata-wave_unit_note"]]$html, "kitsUI 目前把 wave_m 當成月")
  })
})

test_that("發現 10：wave_m 是負的、不是數字時擋下（說是哪一欄、哪個值）；age_m 只提醒", {
  x <- kg_one()
  x$df$wave_m <- c(-12, 0)
  out <- kits_build(list(x$df), "a.csv", kg_spec(x$s, x$map, "B1"))
  expect_match(out$problems, "wave_m 是負的（例如 -12）", all = FALSE)

  x <- kg_one(age = c("150", "12m"))
  x$df$wave_m <- c("12m", "0")
  x$map$age_m <- "age"
  out <- expect_no_warning(kits_build(list(x$df), "a.csv", kg_spec(x$s, x$map, "B1")))
  expect_match(out$problems, "a.csv：wave_m 的欄 wave_m 有不是數字的值（例如「12m」）",
               all = FALSE)
  x$df$wave_m <- c("12", "0")
  out <- expect_no_warning(kits_build(list(x$df), "a.csv", kg_spec(x$s, x$map, "B1")))
  expect_length(out$problems, 0)
  expect_match(out$warnings, "age 有不是數字的值（例如「12m」），這些格的 age_m 會是空白",
               all = FALSE)
  expect_equal(sort(kits_long_of(out)$wave_m), c(0, 12))
})

test_that("發現 10：底線造成的 visit_id 撞號擋下；世代只差空白或大小寫時提醒", {
  x <- kg_one()
  x$df$id <- c("B_1", "1")
  x$df$wave_m <- c(3, 3)
  x$df$co <- c("A", "A_B")
  x$map$wave <- ""
  spec <- kg_spec(x$s, x$map, "x")
  spec$cohort <- list(list(type = "column", value = "co"))
  out <- kits_build(list(x$df), "a.csv", spec)
  expect_match(out$problems, "visit_id 撞號.*A_B_1_3.*底線", all = FALSE)
  expect_no_match(paste(out$problems, collapse = ""), "同一列次同一題有好幾列")

  x <- kg_one()
  x$df$co <- c("G7 ", "g7")
  spec <- kg_spec(x$s, x$map, "x")
  spec$cohort <- list(list(type = "column", value = "co"))
  out <- kits_build(list(x$df), "a.csv", spec)
  expect_length(out$problems, 0)
  expect_match(out$warnings, "只差空白或大小寫.*「G7 」「g7」", all = FALSE)

  x <- kg_one(wave = c("W_1", "W_2"))
  out <- kits_build(list(x$df), "a.csv", kg_spec(x$s, x$map, "B1"))
  expect_match(out$warnings, "底線「_」（例如 W_1", all = FALSE)
})

# ── 發現 7：世代欄、性別欄是帶標籤的欄 ──────────────────────────────────────

kg_grade <- function() {
  tibble::tibble(
    id = 1:5, wave_m = 0,
    grade = haven::labelled(c(7, 8, 98, 7, 99), c("國一" = 7, "國二" = 8, "跳答" = 98,
                                                  "遺漏值" = 99)),
    gender = haven::labelled(c(1, 2, 1, 9, 2), c("男" = 1, "女" = 2, "拒答" = 9)),
    q1 = c(0, 1, 1, 0, 1)
  )
}

kg_grade_spec <- function(df, value_labels = "code") {
  map <- kits_guess_map(names(df))
  map$subj_id <- "id"
  map$sex <- "gender"
  s <- kits_item_settings(list(df), "q1") |> kits_bulk_domain("q1", "a")
  spec <- kg_spec(s, map, "x")
  spec$cohort <- list(list(type = "column", value = "grade"))
  spec$value_labels <- value_labels
  list(spec = spec, s = s)
}

test_that("發現 7：世代、性別可以輸出代碼（預設）或標籤；性別、世代的代碼照原樣並提醒", {
  df <- kg_grade()
  x <- kg_grade_spec(df)
  out <- kits_build(list(df), "w1.sav", x$spec)
  expect_length(out$problems, 0)
  long <- kits_long_of(out)
  expect_setequal(unique(long$cohort), c("7", "8", "98", "99"))
  expect_equal(sort(unique(stats::na.omit(long$sex))), c("1", "2", "9"))
  expect_equal(unique(long$sex[long$subj_id == "4"]), "9")  # 9 拒答：照原樣（0.5.0 起）
  w <- paste(out$warnings, collapse = "\n")
  expect_match(w, "w1.sav：世代欄 grade 有遺漏值代碼 98（值標籤「跳答」）、99（值標籤「遺漏值」），共 2 列")
  expect_match(w, "sex（gender）有 1 格疑似遺漏值代碼 9（值標籤「拒答」）")
  got <- kg_run_code(list(df), x$spec, x$s, sav = TRUE)
  kg_same_long(got$long, long)

  x <- kg_grade_spec(df, "label")
  long <- kits_long_of(kits_build(list(df), "w1.sav", x$spec))
  expect_setequal(unique(long$cohort), c("國一", "國二", "跳答", "遺漏值"))
  expect_setequal(unique(stats::na.omit(long$sex)), c("男", "女", "拒答"))
  expect_equal(kits_label_text(df$gender), c("男", "女", "男", "拒答", "女"))
  got <- kg_run_code(list(df), x$spec, x$s, sav = TRUE)
  expect_match(got$code, "cohort = as_label(grade)", fixed = TRUE)
  expect_match(got$code, "sex = as_label(gender)", fixed = TRUE)
  kg_same_long(got$long, long)
})

test_that("發現 7：app 裡世代選檔案裡的欄、值標籤選「標籤」", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "w1.sav")
  df <- kg_grade()
  df$grade <- haven::labelled(c(7, 8, 8, 7, 7), c("國一" = 7, "國二" = 8))
  haven::write_sav(df, path)
  shiny::testServer(kit_server, {
    session$setInputs(`upload-files` = kg_upload(path))
    kd_cells(session, "cohort", "grade")
    kd_cells(session, "subj_id", "id")
    kd_cells(session, "wave_m", "wave_m")
    kd_cells(session, "sex", "gender")
    kd_set(session, value_labels = "label")
    kd_click(session, "ok1")
    expect_equal(kd_head_state(output, 1), "done")
    expect_match(kd_out(output, "head1"), "grade")
    kd_pick(session, "q1", "a")
    kd_click(session, "ok2")
    expect_equal(kd_check_state(output, "遺漏值代碼"), "todo")   # 性別的 9 拒答
    expect_match(kd_out(output, "checks"), "（共 1 格）")
    # R/ 的 bug（已回報）：只有性別、wave_m、age_m 有代碼時，檢查沒寫出是哪個值
    expect_match(kd_out(output, "checks"), "遺漏值代碼 9")
    kd_click(session, "ack_codes")
    kd_click(session, "run")
    expect_match(kd_out(output, "status"), "完成")
    expect_match(paste(output[["kitsdata-summary"]], collapse = ""), "國一")
    expect_match(output[["kitsdata-code"]]$html, "as_label(grade)", fixed = TRUE)
    long <- readr::read_csv(kit_test_download_file(session, "kitsdata-download_csv"),
                            col_types = "ccccddcic")
    expect_setequal(unique(long$cohort), c("國一", "國二"))
    expect_setequal(unique(long$sex), c("男", "女", "拒答"))
  })
})

# ── 發現 8：某一題在一個檔案是文字 ──────────────────────────────────────────

test_that("發現 8：文字的題目欄只影響那個檔案：內容都是數字的轉成數字，有文字的那個檔案不轉", {
  dfs <- list(
    tibble::tibble(id = 1:2, wave_m = 0, q1 = c(0, 1), q2 = c(1, 1), note = c("a", "b")),
    tibble::tibble(id = 1:2, wave_m = 12, q1 = c(" 1", "2"), q2 = c("1", "其他"),
                   note = c("c", "d"))
  )
  cand <- kits_candidate_cols(dfs, c("id", "wave_m"))
  expect_setequal(cand$numeric, c("q1", "q2"))
  expect_equal(cand$text, "note")
  expect_equal(cand$mixed$column, "q2")
  expect_equal(cand$mixed$file, 2L)
  map <- kits_guess_map(c("id", "wave_m"))
  map$subj_id <- "id"
  s <- kits_item_settings(dfs, c("q1", "q2")) |> kits_bulk_domain(c("q1", "q2"), "a")
  expect_equal(s$raw[[1]], c(0, 1, 2))
  spec <- kg_spec(s, map, c("B1", "B1"))
  out <- expect_no_warning(kits_build(dfs, c("w1.sav", "w2.sav"), spec))
  expect_length(out$problems, 0)
  expect_match(out$notes, "w2.sav：1 個題目欄是文字、內容都是數字，轉成數字（q1）", all = FALSE)
  expect_match(out$warnings, "w2.sav：1 個題目欄在這個檔案是文字.*q2（例如「其他」）", all = FALSE)
  long <- kits_long_of(out)
  expect_equal(sum(long$item_id == "q2"), 2)       # 只有 w1 的 q2
  expect_equal(sum(long$item_id == "q1"), 4)
  side <- list()
  code <- code_kitsdata(code_files(c("w1.csv", "w2.csv"), c("t1", "t2")), spec,
                        text_fix = kits_text_fix(dfs, spec$items$column))
  expect_match(code, 'select(-any_of(c("q2")))', fixed = TRUE)
  # 程式碼：CSV 讀回來型別會變，所以用 SAV 比對
  dir <- withr::local_tempdir()
  for (i in 1:2) haven::write_sav(dfs[[i]], file.path(dir, paste0("w", i, ".sav")))
  readr::write_excel_csv(kits_settings_export(s), file.path(dir, KITS_SETTINGS_FILE), na = "")
  code <- code_kitsdata(code_files(c("w1.sav", "w2.sav"), c("t1", "t2")), spec,
                        text_fix = kits_text_fix(dfs, spec$items$column))
  env <- new.env()
  withr::with_dir(dir, suppressMessages(eval(parse(text = code), envir = env)))
  kg_same_long(env$kits_long, long)
})

test_that("發現 8：app 的第 2 步列出在部分檔案是文字的題", {
  dir <- withr::local_tempdir()
  paths <- file.path(dir, c("w1.sav", "w2.sav"))
  haven::write_sav(tibble::tibble(id = 1:2, q2 = c(1, 1)), paths[1])
  haven::write_sav(tibble::tibble(id = 1:2, q2 = c("1", "其他")), paths[2])
  shiny::testServer(kit_server, {
    session$setInputs(`upload-files` = kg_upload(paths))
    expect_match(kd_out(output, "items_status"), "1 個題目欄在部分檔案是文字：q2（w2.sav）")
  })
})

# ── 發現 13：全部是 NA、又沒有領域的題不擋 ──────────────────────────────────

test_that("發現 13：沒有任何作答、也沒有領域的題，不擋轉換（只提醒沒有作答）", {
  df <- tibble::tibble(id = 1:2, wave_m = 0, q1 = c(0, 1), empty = c(NA_real_, NA_real_))
  map <- kits_guess_map(names(df))
  map$subj_id <- "id"
  s <- kits_item_settings(list(df), c("q1", "empty")) |> kits_bulk_domain("q1", "a")
  expect_equal(kits_settings_undecided(s), character(0))
  out <- kits_build(list(df), "a.csv", kg_spec(s, map, "B1"))
  expect_length(out$problems, 0)
  expect_match(out$warnings, "沒有任何作答.*empty", all = FALSE)
})

# ── 發現 11：題目表、題目設定檔的解析 ───────────────────────────────────────

test_that("發現 11：BOM、SAV 的標籤欄、分號 CSV、include 與 map_raw_to_score 寫錯、重複的題", {
  bom <- data.frame(a = "lana01", domain = "lan")
  names(bom)[1] <- "﻿item_id"
  expect_equal(kits_item_table(bom)$item_id, "lana01")
  # SAV：item_id 是標籤欄時用標籤；domain 空白 → NA
  sav <- tibble::tibble(item_id = haven::labelled(c(1, 2), c("lana01" = 1, "lana02" = 2)),
                        domain = c("lan", ""))
  tab <- kits_item_table(sav)
  expect_equal(tab$item_id, c("lana01", "lana02"))
  expect_equal(tab$domain, c("lan", NA))
  expect_error(kits_settings_import(data.frame(`item_id;domain` = "a;lan", check.names = FALSE)),
               "分號")
  set <- function(...) {
    data.frame(item_id = "a", include = "TRUE", domain = "lan", map_raw_to_score = "1=0;2=1",
               ...)
  }
  ok <- kits_settings_import(transform(set(), include = "Y", map_raw_to_score = "1=0,2=1"))
  expect_true(ok$data$include)
  expect_equal(ok$data$score[[1]], c(0, 1))
  expect_false(kits_settings_import(transform(set(), include = "N"))$data$include)
  expect_error(kits_settings_import(transform(set(), include = "maybe")), "include 看不懂")
  expect_error(kits_settings_import(transform(set(), map_raw_to_score = "3=a")), "看不懂")
  expect_error(kits_settings_import(transform(set(), map_raw_to_score = "1=0=2")), "看不懂")
  expect_equal(kits_parse_map("1=;2=1")$score, c(NA, 1))
  two <- rbind(set(), transform(set(), domain = "cog"))
  expect_error(kits_settings_import(two), "同一題有好幾列")
  expect_equal(nrow(kits_settings_import(rbind(set(), set()))$data), 1)  # 完全一樣的列只留一列
})

test_that("「遺漏值處理」的結果過期（source = \"stale\"）時，提醒重新執行", {
  df <- tibble::tibble(id = 1:2, q1 = c(0, 1))
  files <- list(keys = function() "f1", names = function() "w1.csv", labels = function() "t1",
                data = function() list(df), encodings = function() NA_character_)
  src <- shiny::reactiveVal("stale")
  working <- list(data = function() list(df), source = src)
  went <- NULL
  shiny::testServer(mod_kitsdata_server, args = list(files = files, working = working,
                                                    goto = function(tab) went <<- tab), {
    expect_match(output$source_note$html, "「遺漏值處理」的設定改過了")
    expect_match(output$source_note$html, "goto_missing_stale")
    session$setInputs(goto_missing_stale = 1)
    expect_equal(went, "missing")
    src("na")
    session$flushReact()
    expect_no_match(kd_text(output$source_note), "改過了")
    expect_match(kd_text(output$source_note), "用的是「遺漏值處理」處理後的資料")
    src("raw")
    session$flushReact()
    expect_equal(kd_text(output$source_note), "")
  })
})

test_that("帶標籤的文字欄（SAV）也能判斷是不是數字；TDCYP W1–W4 jsq（有檔案時才跑）", {
  x <- haven::labelled(c("1", "2", "x"), c(a = "1"))
  expect_equal(kits_as_num(x), c(1, 2, NA))
  expect_false(kits_numeric_like(x))
  expect_true(kits_numeric_like(haven::labelled(c("1", " 2"), c(a = "1"))))
  paths <- kg_tdcyp(sprintf("w%d_jsq.sav", 1:4))
  skip_if_not(all(file.exists(paths)))
  dfs <- purrr::map(paths, function(p) read_data_file(p, basename(p))$data)
  cand <- expect_no_error(kits_candidate_cols(dfs, character(0)))
  expect_true("w4c4_1" %in% cand$numeric)
})
