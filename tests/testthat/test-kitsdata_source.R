# ⑥ 的資料來源與轉換前的整理：多波寬檔、文字的波次欄、單一波次、④ 疊加的結果（0.8.1）

test_that("偵測多波寬檔：欄名結尾或開頭的波次記號；只有一題跨波次、單波次的 KIT 檔不算", {
  d <- kits_detect_wave_suffix(c("id", "sex", "soc01_t1", "soc02_t1", "soc01_t2", "soc02_t2", "age_t1"))
  expect_equal(d$side, "suffix")
  expect_equal(d$waves, c("t1", "t2"))
  expect_equal(d$n_stems, 2)
  expect_setequal(d$cols, c("soc01_t1", "soc02_t1", "soc01_t2", "soc02_t2", "age_t1"))
  expect_equal(d$label_col, "wave_label")
  expect_equal(d$wave_col, "wave_m")
  p <- kits_detect_wave_suffix(c("id", "w1q1", "w1q2", "w2q1", "w2q2", "w10q1", "w10q2"))
  expect_equal(p$side, "prefix")
  expect_equal(p$waves, c("w1", "w2", "w10"))
  m <- kits_detect_wave_suffix(c("id", "lana01_M06", "lana02_M06", "lana01_M12", "lana02_M12"))
  expect_equal(m$token, "m")
  expect_null(kits_detect_wave_suffix(c("release_id", "lana01", "lana02", "mota01", "w3_id")))
  expect_null(kits_detect_wave_suffix(c("id", "age_t1", "age_t2", "q1", "q2")))
  expect_null(kits_detect_wave_suffix(c("id", "q_1", "q_2", "r_1", "r_2")))   # 題號不是波次
})

test_that("多波寬檔轉成一列一個波次：時間點取數字；值標籤不同的題拿掉值標籤；程式碼結果相同", {
  w <- tibble::tibble(
    id = 1:3, sex = c(1, 2, 1),
    soc01_t1 = haven::labelled(c(1, 2, 3), c(low = 1)), soc02_t1 = c(4, 3, 2),
    soc01_t2 = haven::labelled(c(2, 2, NA), c(Low = 1)), soc02_t2 = c(1, NA, 4)
  )
  d <- kits_detect_wave_suffix(names(w))
  expect_equal(kits_wave_zap_cols(w, d), c("soc01_t1", "soc01_t2"))
  p <- kits_pivot_waves(w, d)
  expect_equal(nrow(p$data), 6)
  expect_named(p$data, c("id", "sex", "wave_label", "soc01", "soc02", "wave_m"))
  expect_equal(p$data$wave_m, rep(c(1, 2), 3))
  expect_equal(p$data$soc02, c(4, 1, 3, NA, 2, 4))
  d$zap_cols <- kits_wave_zap_cols(w, d)
  code <- code_kitsdata_source("w", list(list(kind = "pivot", det = d)))
  expect_match(paste(code, collapse = "\n"), "pivot_longer(matches(", fixed = TRUE)
  expect_match(paste(code, collapse = "\n"), "haven::zap_labels", fixed = TRUE)
  env <- new.env()
  env$w <- w
  suppressPackageStartupMessages({
    for (pkg in c("dplyr", "tidyr", "readr")) eval(call("library", pkg), env)
  })
  eval(parse(text = code), env)
  expect_equal(as.data.frame(env$w), as.data.frame(p$data))
})

test_that("文字的波次欄（W1、t2）可以取出數字當時間點；已有 wave_m 或是數字時不用", {
  L <- tibble::tibble(id = rep(1:2, each = 2), wave = rep(c("W1", "W2"), 2), q1 = 1)
  d <- kits_detect_wave_text(L)
  expect_equal(d$col, "wave")
  expect_equal(d$examples, c("W1 → 1", "W2 → 2"))
  expect_equal(kits_source_apply(L, "wave_num", d)$wave_m, c(1, 2, 1, 2))
  expect_null(kits_detect_wave_text(dplyr::mutate(L, wave = c(1, 2, 1, 2))))
  expect_null(kits_detect_wave_text(dplyr::mutate(L, wave_m = 1)))
  expect_null(kits_detect_wave_text(dplyr::mutate(L, wave = c("pre", "post", "pre", "post"))))
  code <- code_kitsdata_source("L", list(list(kind = "wave_num", det = d)))
  expect_match(paste(code, collapse = "\n"), "parse_number(as.character(wave))", fixed = TRUE)
})

test_that("沒轉成一列一個波次就挑了跨波次的題：轉換時提醒", {
  w <- tibble::tibble(id = 1:2, wave_m = 0, a_t1 = 1, b_t1 = 1, a_t2 = 0, b_t2 = 1)
  s <- kits_item_settings(list(w), c("a_t1", "b_t1", "a_t2", "b_t2")) |>
    kits_bulk_domain(c("a_t1", "b_t1", "a_t2", "b_t2"), "x")
  map <- kits_guess_map(names(w))
  map$subj_id <- "id"
  out <- kits_build(list(w), "wide.csv", kg_spec(s, map, "A"))
  expect_match(paste(out$warnings, collapse = "\n"), "wide.csv：a_t1、b_t1 這類題目只差在波次記號")
})

test_that("第 1 步的猜測：單一檔案給 S1 與 0；數字的 wave 欄、檔名的第二個 M、世代欄、一般的編號欄", {
  one <- kits_step1_guess(list(tibble::tibble(participant = 1:2, q1 = 1)), "survey.csv", "t1")
  expect_true(attr(one, "single"))
  expect_equal(one$cohort, "S1")
  expect_equal(one$wave_m, "0")
  expect_equal(one$subj_id, "participant")
  long <- kits_step1_guess(list(tibble::tibble(child = 1, cohort = "M03", wave = 6, q1 = 1)), "long.csv")
  expect_false(attr(long, "single"))
  expect_equal(long$cohort, "cohort")
  expect_equal(long$wave_m, "wave")
  expect_equal(long$subj_id, "child")
  kit <- kits_step1_guess(list(tibble::tibble(release_id = 1), tibble::tibble(release_id = 1)),
                          c("kit_M03_M12.csv", "kit_M03.csv"))
  expect_equal(kit$cohort, c("M03", "M03"))
  expect_equal(kit$wave_m, c("12", ""))
  w2 <- kits_step1_guess(list(tibble::tibble(id = 1), tibble::tibble(id = 1)), c("panel_W1.csv", "panel_W2.csv"))
  expect_equal(w2$wave_m, c("1", "2"))
})

test_that("小量尺上和分數隔開的 9：疑似代碼（不在計分對照裡）；0–7 加上 9 不算", {
  expect_equal(kits_small_scale_codes(c(1, 2, 3, 4, 9)), 9)
  expect_equal(kits_small_scale_codes(c(0, 1, 2, 98, 99)), c(98, 99))
  expect_length(kits_small_scale_codes(c(0:7, 9)), 0)
  expect_length(kits_small_scale_codes(c(0, 1, 2, 8)), 0)   # 8 不像代碼
  s <- kits_item_settings(list(tibble::tibble(q = c(1, 2, 3, 4, 9))), "q")
  expect_equal(s$raw[[1]], 1:4)
  expect_equal(kits_bulk_shift(s, "q", -1)$score[[1]], 0:3)
})

test_that("wave_m 是文字（W1）時，問題說明指到「取出數字當時間點」", {
  L <- tibble::tibble(id = 1:2, wave = c("W1", "W2"), q1 = 1)
  map <- kits_guess_map(names(L))
  map$wave_m <- "wave"
  p <- kits_number_problems(list(L), "l.csv", map)$problems
  expect_match(p, "取出數字當時間點")
  expect_no_match(p, "改用「自己填」")
})

# 在 app 裡做完：下載的作答檔，和「用 R 怎麼做」在同一批檔案上跑出來的一樣
kd_code_same <- function(session, kitsdata, dir) {
  file.copy(kit_test_download_file(session, "kitsdata-settings_dl"), file.path(dir, KITS_SETTINGS_FILE))
  app <- readr::read_csv(kit_test_download_file(session, "kitsdata-download_csv"),
                         col_types = "ccccddcic")
  code <- kitsdata$code()$tidy
  env <- new.env()
  withr::with_dir(dir, suppressMessages(suppressWarnings(eval(parse(text = code), env))))
  kg_same_long(env$kits_long, app)
  code
}

test_that("app：多波寬檔按「轉成一列一個波次」後轉換；程式碼結果相同；還原", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "wide.csv")
  readr::write_csv(tibble::tibble(id = 1:3, sex = c(1, 2, 1), a01_t1 = c(0, 1, 2), a02_t1 = c(1, 1, 0),
                                  a01_t2 = c(2, 2, 1), a02_t2 = c(0, NA, 1)), path)
  shiny::testServer(kit_server, {
    session$setInputs(`upload-files` = kd_upload(path))
    expect_match(kd_out(output, "shape_ui"), "像是多波寬檔")
    key <- files$keys()
    session$setInputs(`kitsdata-tf` = list(key = key, kind = "pivot"))
    expect_match(kd_out(output, "shape_ui"), "已轉成一個橫列一個孩子的一個波次")
    ids <- function(field) paste0("kitsdata-cell_", field, "_", key, "__pivot")
    do.call(session$setInputs, stats::setNames(list("A", "wave_m", "id"), ids(c("cohort", "wave_m", "subj_id"))))
    kd_click(session, "ok1")
    kd_pick(session, "a0", "x")
    kd_click(session, "ok2")
    kd_click(session, "run")
    expect_match(kd_out(output, "status"), "完成")
    code <- kd_code_same(session, kitsdata, dir)
    expect_match(code, "pivot_longer(matches(", fixed = TRUE)
    long <- readr::read_csv(kit_test_download_file(session, "kitsdata-download_csv"),
                            col_types = "ccccddcic")
    expect_setequal(long$item_id, c("a01", "a02"))
    expect_setequal(long$wave_m, c(1, 2))
    expect_true("A_1_t1" %in% long$visit_id)
    session$setInputs(`kitsdata-tf` = list(key = key, kind = "off"))
    expect_match(kd_out(output, "shape_ui"), "像是多波寬檔")
  })
})

test_that("app：單一檔案不用填世代與時間點；只用勾的檔案；程式碼只讀那些檔案", {
  dir <- withr::local_tempdir()
  readr::write_csv(tibble::tibble(participant = 1:3, q1 = c(0, 1, 2), q2 = c(1, 0, 1)), file.path(dir, "s.csv"))
  readr::write_csv(tibble::tibble(participant = 1:2, q1 = c(1, 1), q2 = c(0, 0)), file.path(dir, "other.csv"))
  shiny::testServer(kit_server, {
    session$setInputs(`upload-files` = kd_upload(file.path(dir, c("s.csv", "other.csv"))))
    keys <- files$keys()
    expect_match(kd_out(output, "source_ui"), "要轉的檔案")
    session$setInputs(`kitsdata-use_files` = keys[1])
    expect_match(kd_out(output, "single_note"), "S1")
    kd_click(session, "ok1")
    kd_pick(session, "q", "x")
    kd_click(session, "ok2")
    kd_click(session, "run")
    expect_match(kd_out(output, "status"), "完成")
    code <- kd_code_same(session, kitsdata, dir)
    expect_match(code, "s.csv", fixed = TRUE)
    expect_no_match(code, "other.csv", fixed = TRUE)
    long <- readr::read_csv(kit_test_download_file(session, "kitsdata-download_csv"),
                            col_types = "ccccddcic")
    expect_equal(unique(long$visit_id), c("S1_1_0", "S1_2_0", "S1_3_0"))
  })
})

test_that("app：用 ④ 疊加的結果；文字的波次欄取出數字當時間點", {
  dir <- withr::local_tempdir()
  readr::write_csv(tibble::tibble(id = 1:2, q1 = c(0, 1)), file.path(dir, "w1.csv"))
  readr::write_csv(tibble::tibble(id = 1:2, q1 = c(1, 2)), file.path(dir, "w2.csv"))
  shiny::testServer(kit_server, {
    session$setInputs(`upload-files` = kd_upload(file.path(dir, c("w1.csv", "w2.csv"))))
    session$setInputs(`stack-run` = 1)
    expect_false(is.null(stack$result()))
    expect_match(kd_out(output, "source_ui"), "疊加的結果")
    session$setInputs(`kitsdata-source` = "stack")
    expect_match(kd_out(output, "shape_ui"), "取出數字當時間點")
    session$setInputs(`kitsdata-tf` = list(key = "stack", kind = "wave_num"))
    ids <- function(field) paste0("kitsdata-cell_", field, "_stack__wave_num")
    do.call(session$setInputs, stats::setNames(list("A", "id"), ids(c("cohort", "subj_id"))))
    kd_click(session, "ok1")
    kd_pick(session, "q", "x")
    kd_click(session, "ok2")
    kd_click(session, "run")
    expect_match(kd_out(output, "status"), "完成")
    long <- readr::read_csv(kit_test_download_file(session, "kitsdata-download_csv"),
                            col_types = "ccccddcic")
    expect_setequal(long$wave_m, c(1, 2))
    expect_match(kitsdata$code()$tidy, "stacked", fixed = TRUE)
  })
})

test_that("一波一檔、欄名帶著各檔的波次（socC20_w1、socC20_w2）：輸出題號去掉波次、放在同一組", {
  cols <- list(c("id", "socC20_w1", "socC21_w1", "socB17_w1", "age_w1"),
               c("id", "socC20_w2", "socC21_w2", "socB17_w2", "age_w1"),
               c("id", "socC20_w3", "socC21_w3", "socB17_w3"))
  t <- kits_detect_file_wave_tags(cols)
  expect_equal(t$tags, c("w1", "w2", "w3"))
  expect_equal(unname(t$out[c("socC20_w1", "socC20_w3", "age_w1")]), c("socC20", "socC20", "age"))
  expect_null(kits_detect_file_wave_tags(cols[1]))
  expect_null(kits_detect_file_wave_tags(list(c("id", "lana01", "lana02"), c("id", "lana01", "lana02"))))
  # 同一個記號出現在兩個檔案（例如兩個世代的 w1）：不是各檔一波
  expect_null(kits_detect_file_wave_tags(list(c("a_w1", "b_w1"), c("a_w1", "b_w1"))))
  # 開頭的記號（TDCYP 的 w1d5_1、w2d5_1）不自動去掉
  expect_null(kits_detect_file_wave_tags(list(c("w1d5_1", "w1d5_2"), c("w2d5_1", "w2d5_2"))))
})

test_that("app：三個檔案各一波、欄名帶 _w1 _w2 _w3：一組三波、作答檔的題號相同；程式碼結果相同", {
  dir <- withr::local_tempdir()
  for (k in 1:3) {
    df <- tibble::tibble(id = 1:3, a = c(0, 1, 2), b = c(1, 2, 0), c = c(2, 0, 1))
    names(df)[-1] <- paste0("socC2", 0:2, "_w", k)
    readr::write_csv(df, file.path(dir, paste0("w", k, ".csv")))
  }
  shiny::testServer(kit_server, {
    session$setInputs(`upload-files` = kd_upload(file.path(dir, paste0("w", 1:3, ".csv"))))
    kd_cells(session, "cohort", c("A", "A", "A"))
    kd_cells(session, "wave_m", c("1", "2", "3"))
    kd_cells(session, "subj_id", c("id", "id", "id"))
    kd_click(session, "ok1")
    expect_match(kd_out(output, "intro2"), "socC20_w1 → socC20")
    g <- kd_out(output, "groups_table")
    expect_match(g, "9 直欄")   # 一組 9 欄（3 題 × 3 波）
    kd_pick(session, "soc", "soc")
    kd_click(session, "ok2")
    kd_click(session, "run")
    expect_match(kd_out(output, "status"), "完成")
    kd_code_same(session, kitsdata, dir)
    long <- readr::read_csv(kit_test_download_file(session, "kitsdata-download_csv"),
                            col_types = "ccccddcic")
    expect_setequal(long$item_id, c("socC20", "socC21", "socC22"))
    expect_equal(nrow(long), 27)
  })
})

test_that("用欄名挑：開頭有 s 的也找得到（以前 s 被當成分隔符號）", {
  expect_equal(kits_pick_cols(c("socC20_w1", "id", "sex"), "soc"), "socC20_w1")
  expect_equal(kits_pick_cols(c("socC20_w1", "sdq1", "id"), "soc、sdq"), c("socC20_w1", "sdq1"))
  expect_equal(kits_pick_cols(c("a1", "b1"), "a b"), c("a1", "b1"))
})
