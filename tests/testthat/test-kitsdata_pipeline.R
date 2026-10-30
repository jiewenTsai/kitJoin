# 模擬的寬格式資料：一列一個孩子的一個波次、一題一欄（1–5 計分）
sim_wide <- function(ids, wave, wave_m, seed = 1) {
  set.seed(seed)
  n <- length(ids)
  score <- function() sample(1:5, n, replace = TRUE)
  tibble::tibble(
    release_id = ids, wave = wave, wave_m = wave_m, age_m = wave_m + 0.2,
    sex = rep(c("boy", "girl"), length.out = n), city = "臺北市",
    lana01 = score(), lana02 = score(), bodyaf01 = score(), cogd01r = score()
  )
}

sim_table <- function() {
  tibble::tibble(
    item_id = c("lana01", "lana02", "bodyaf01", "cogd01"),
    domain = c("lan", "lan", "motor", "cog")
  )
}

# 預設的設定：兩個檔案（M03、M36），題目表給領域；fun 是對題目設定的批次動作
sim_spec <- function(dfs, cohorts = c("M03", "M36"), fun = NULL, table = sim_table()) {
  map <- kits_guess_map(unique(unlist(purrr::map(dfs, names))))
  cand <- kits_candidate_cols(dfs, kits_mapped_cols(map))
  s <- kits_settings_apply(kits_item_settings(dfs, cand$numeric),
                           list(type = "table", data = table))
  if (!is.null(fun)) s <- fun(s)
  kits_spec(
    purrr::map(cohorts, function(x) list(type = "text", value = x)), map,
    kits_settings_items(s), kits_settings_undecided(s)
  )
}

sim_dfs <- function() {
  list(
    dplyr::bind_rows(
      sim_wide(c("K1", "K2", "K3"), "m03g01", 3),
      sim_wide(c("K1", "K2"), "m06g02", 6, seed = 2)
    ),
    sim_wide(c("J1", "J2"), "m36g01", 36, seed = 3)
  )
}

test_that("依欄名猜對應的欄", {
  map <- kits_guess_map(c("release_id", "wave", "wave_m", "age_m", "sex", "lana01"))
  expect_equal(map$subj_id, "release_id")
  expect_equal(map$wave, "wave")
  expect_equal(map$wave_m, "wave_m")
  expect_equal(map$age_m, "age_m")
  expect_equal(map$sex, "sex")
  expect_equal(map$visit_id, "")
})

test_that("領域：先查題目表，r 結尾查不到時查原題", {
  d <- kits_item_domains(c("lana01", "cogd01r", "bodyef01"), sim_table())
  expect_equal(d$domain, c("lan", "cog", NA))
  expect_match(d$source[2], "原題 cogd01")
})

test_that("題目表要有 item_id 與 domain，同一題不能有兩個領域", {
  expect_error(kits_item_table(data.frame(item = "a", domain = "lan")), "item_id")
  bad <- data.frame(item_id = c("a", "a"), domain = c("lan", "cog"))
  expect_error(kits_item_table(bad), "不同的領域")
  ok <- kits_item_table(data.frame(Item_ID = " a ", Domain = "lan"))
  expect_equal(ok$item_id, "a")
})

test_that("轉換後固定 9 欄、固定順序與型別；visit_id 用波次標籤", {
  dfs <- sim_dfs()
  out <- kits_build(dfs, c("a.csv", "b.csv"), sim_spec(dfs))
  expect_length(out$problems, 0)
  expect_named(kits_long_of(out), KITS_COLUMNS$column)
  expect_type(kits_long_of(out)$item_score, "integer")
  expect_type(kits_long_of(out)$subj_id, "character")
  expect_type(kits_long_of(out)$sex, "character")
  expect_true("M03_K1_m03g01" %in% kits_long_of(out)$visit_id)
  expect_true("M36_J1_m36g01" %in% kits_long_of(out)$visit_id)
  expect_equal(nrow(kits_long_of(out)), (5 + 2) * 4)
  # cogd01r 保留 r，領域用原題的
  expect_equal(unique(kits_long_of(out)$domain[kits_long_of(out)$item_id == "cogd01r"]), "cog")
  # city 是文字欄，不是題目
  expect_false("city" %in% kits_long_of(out)$item_id)
})

test_that("沒有波次標籤欄時，visit_id 改用 wave_m，並說明", {
  dfs <- purrr::map(sim_dfs(), function(df) dplyr::select(df, -"wave"))
  out <- kits_build(dfs, c("a.csv", "b.csv"), sim_spec(dfs))
  expect_true("M03_K1_3" %in% kits_long_of(out)$visit_id)
  expect_match(out$notes[1], "wave_m")
})

test_that("世代空白、欄位沒決定、沒對應 wave_m 時不轉換", {
  dfs <- sim_dfs()
  out <- kits_build(dfs, c("a.csv", "b.csv"), sim_spec(dfs, cohorts = c("M03", "")))
  expect_null(out$prep)
  expect_match(out$problems, "b.csv：還沒有填世代", all = FALSE)

  spec <- sim_spec(dfs, table = sim_table()[1:3, ])
  out <- kits_build(dfs, c("a.csv", "b.csv"), spec)
  expect_match(out$problems, "個納入的題目沒有領域", all = FALSE)

  spec <- sim_spec(dfs)
  spec$map$wave_m <- ""
  expect_match(kits_build(dfs, c("a.csv", "b.csv"), spec)$problems, "wave_m", all = FALSE)
})

test_that("重複的孩子：同一列次同一題好幾列，擋下", {
  dfs <- sim_dfs()
  dfs[[1]] <- dplyr::bind_rows(dfs[[1]], dfs[[1]][1, ])
  out <- kits_build(dfs, c("a.csv", "b.csv"), sim_spec(dfs))
  expect_null(out$prep)
  expect_match(out$problems, "同一列次同一題有好幾列", all = FALSE)
})

test_that("visit_id 直接用某一欄時，兩個世代撞號會擋下", {
  dfs <- purrr::map(sim_dfs(), function(df) {
    dplyr::mutate(df, vid = paste0("v", dplyr::row_number()))
  })
  spec <- sim_spec(dfs)
  spec$map$visit_id <- "vid"
  spec$items <- dplyr::filter(spec$items, .data$column != "vid")
  spec$undecided <- setdiff(spec$undecided, "vid")
  out <- kits_build(dfs, c("a.csv", "b.csv"), spec)
  expect_match(out$problems, "兩個世代", all = FALSE)
})

# 遺漏值處理 的結果（⑥ 不處理遺漏值代碼）
after_missing <- function(df, codes = c(7777, 9999)) {
  replace_na_codes(df, codes, exclude = "release_id")$data
}

test_that("計分對照：先平移 −1，再反向（9999 已由 ② 改成 NA）；題號不變", {
  dfs <- sim_dfs()
  dfs[[1]]$lana01[1] <- 9999
  dfs[[1]]$lana02 <- c(1, 2, 3, 4, 5)
  dfs <- purrr::map(dfs, after_missing)
  fun <- function(s) {
    s |>
      kits_bulk_shift(c("lana01", "lana02", "bodyaf01", "cogd01r"), -1) |>
      kits_bulk_reverse("lana02", 4)
  }
  out <- kits_build(dfs, c("a.csv", "b.csv"), sim_spec(dfs, fun = fun))
  expect_length(out$problems, 0)
  long <- kits_long_of(out)
  expect_false(any(long$item_score > 4))
  expect_false(any(long$item_id == "lana01" & long$visit_id == "M03_K1_m03g01"))
  # lana02：1..5 → 平移 → 0..4 → 反向 → 4..0；領域不變
  rev <- long[long$item_id == "lana02" & long$cohort == "M03", ]
  expect_equal(rev$item_score, c(4L, 3L, 2L, 1L, 0L))
  expect_equal(unique(rev$domain), "lan")
  expect_match(out$notes, "計分對照：4 題", all = FALSE)
})

test_that("計分後有負分、或原分數沒有計分時擋下", {
  dfs <- sim_dfs()
  dfs[[2]]$lana01[1] <- 0
  out <- kits_build(dfs, c("a.csv", "b.csv"),
                    sim_spec(dfs, fun = function(s) kits_bulk_shift(s, "lana01", -1)))
  expect_match(out$problems, "有負分：lana01", all = FALSE)
  out <- kits_build(dfs, c("a.csv", "b.csv"),
                    sim_spec(dfs, fun = function(s) kits_bulk_reverse(s, "lana02", 3)))
  expect_match(out$problems, "有負分：lana02", all = FALSE)
  # 計分對照裡沒有 0 這個原分數（例如沿用上一波的設定）
  spec <- sim_spec(dfs)
  spec$items$raw[[1]] <- c(1, 2, 3, 4, 5)
  spec$items$score[[1]] <- c(0, 1, 2, 3, 4)
  out <- kits_build(dfs, c("a.csv", "b.csv"), spec)
  expect_match(out$problems, "lana01：原分數 0 沒有計分", all = FALSE)
})

test_that("負分擋下並建議當成遺漏值代碼；文字的題目欄擋下", {
  dfs <- sim_dfs()
  dfs[[1]]$lana01[1] <- -1
  out <- kits_build(dfs, c("a.csv", "b.csv"), sim_spec(dfs))
  expect_match(out$problems, "負分", all = FALSE)
  expect_match(out$problems, "遺漏值處理", all = FALSE)
  dfs <- purrr::map(dfs, after_missing, codes = -1)
  out <- kits_build(dfs, c("a.csv", "b.csv"), sim_spec(dfs))
  expect_length(out$problems, 0)
})

test_that("提醒：沒有 0 分、分數中間有缺、疑似遺漏值代碼、不是錨題的領域", {
  dfs <- sim_dfs()
  dfs[[1]]$bodyaf01 <- c(1, 3, 1, 3, 1)
  dfs[[2]]$bodyaf01 <- c(1, 3)
  dfs[[1]]$lana01[1] <- 9999
  tab <- sim_table()
  tab$domain[tab$item_id == "cogd01"] <- "socemo"
  out <- kits_build(dfs, c("a.csv", "b.csv"), sim_spec(dfs, table = tab))
  expect_length(out$problems, 0)
  w <- paste(out$warnings, collapse = "\n")
  expect_match(w, "沒有 0 分")
  expect_match(w, "中間有缺")
  expect_match(w, "7777 以上")
  expect_match(w, "socemo")
})

test_that("世代可以來自檔案裡的欄", {
  dfs <- purrr::map(sim_dfs(), function(df) dplyr::mutate(df, cohort_col = "MX"))
  spec <- sim_spec(dfs)
  spec$cohort <- list(list(type = "column", value = "cohort_col"),
                      list(type = "text", value = "M36"))
  spec$undecided <- setdiff(spec$undecided, "cohort_col")
  out <- kits_build(dfs, c("a.csv", "b.csv"), spec)
  expect_equal(sort(unique(kits_long_of(out)$cohort)), c("M36", "MX"))
})

test_that(".csv.gz 寫得出來、讀得回來", {
  dfs <- sim_dfs()
  long <- kits_long_of(kits_build(dfs, c("a.csv", "b.csv"), sim_spec(dfs)))
  path <- withr::local_tempfile(fileext = ".csv.gz")
  write_kits_long_gz(dfs, sim_spec(dfs), path)
  back <- readr::read_csv(path, col_types = "cccc" |> paste0("ddcic"))
  expect_equal(nrow(back), nrow(long))
  expect_equal(back$visit_id, long$visit_id)
  expect_gt(kits_csv_mb(long, nrow(long)), 0)
})

test_that("「用 R 怎麼做？」的程式碼跑出來和 app 的結果一樣", {
  dfs <- sim_dfs()
  dfs[[1]]$lana01[1] <- 9999
  dfs[[1]]$lana02 <- c(1, 2, 3, 4, 5)
  dfs <- purrr::map(dfs, after_missing)
  map <- kits_guess_map(unique(unlist(purrr::map(dfs, names))))
  s <- kits_item_settings(dfs, kits_candidate_cols(dfs, kits_mapped_cols(map))$numeric) |>
    kits_settings_apply(list(type = "table", data = sim_table())) |>
    kits_bulk_shift(c("lana01", "lana02", "bodyaf01", "cogd01r"), -1) |>
    kits_bulk_reverse("lana02", 4) |>
    kits_bulk_merge("bodyaf01", c(3, 4), 2)
  cohorts <- list(list(type = "text", value = "M03"), list(type = "text", value = "M36"))
  spec <- kits_spec(cohorts, map, kits_settings_items(s), kits_settings_undecided(s))
  app <- kits_long_of(kits_build(dfs, c("w1.csv", "w2.csv"), spec))

  dir <- withr::local_tempdir()
  readr::write_csv(dfs[[1]], file.path(dir, "w1.csv"))
  readr::write_csv(dfs[[2]], file.path(dir, "w2.csv"))
  readr::write_excel_csv(kits_settings_export(s), file.path(dir, KITS_SETTINGS_FILE), na = "")
  f <- code_files(c("w1.csv", "w2.csv"), c("t1", "t2"))
  code <- code_kitsdata(f, spec)
  expect_false(any(nchar(strsplit(code, "\n")[[1]]) > 100))
  expect_match(code, "(x) x - 1", fixed = TRUE)
  expect_match(code, "(x) 5 - x", fixed = TRUE)
  expect_match(code, "x %in% c(3, 4) ~ 2", fixed = TRUE)

  withr::with_dir(dir, {
    env <- new.env()
    suppressMessages(eval(parse(text = code), envir = env))
    expect_true(file.exists("kits_long.csv.gz"))
  })
  got <- env$kits_long
  expect_equal(nrow(got), nrow(app))
  key <- function(x) dplyr::arrange(as.data.frame(x), .data$visit_id, .data$item_id)
  expect_equal(key(got)$item_score, key(app)$item_score)
  expect_equal(key(got)$domain, key(app)$domain)
  expect_equal(key(got)$visit_id, key(app)$visit_id)
})

test_that("沒改的題不會出現在計分對照的程式碼裡", {
  dfs <- sim_dfs()
  code <- code_kitsdata(code_files(c("a.csv", "b.csv"), c("t1", "t2")), sim_spec(dfs))
  expect_no_match(code, "計分對照（原分數")
  expect_no_match(code, "遺漏值")
  expect_match(code, "pivot_longer")
  expect_match(code, "kits_item_settings.csv")
  code <- code_kitsdata(code_files(c("a.csv", "b.csv"), c("t1", "t2")), sim_spec(dfs),
                        na_applied = TRUE)
  expect_match(code, "「遺漏值處理」已經把遺漏值代碼改成 NA")
})

test_that("疑似遺漏值代碼：數格數；減 1 的建議不被 7777 以上的值誤導", {
  dfs <- sim_dfs()
  dfs[[1]]$lana01 <- c(9999, 1, 2, 1, 3)
  dfs[[2]]$age_m[1] <- 7777
  expect_equal(kits_code_cells(dfs, c("lana01", "lana02", "age_m")), 2)
  r <- kits_item_ranges(dfs, "lana01", below = KITS_CODE_MIN)
  expect_equal(r$max, 5)
  expect_equal(kits_item_ranges(dfs, "lana01")$max, 9999)
  out <- kits_build(dfs, c("a.csv", "b.csv"), sim_spec(dfs))
  expect_match(out$warnings, "lana01：9999（7777 以上）", all = FALSE)
  expect_match(out$warnings, "請先到「遺漏值處理」把它們改成 NA", all = FALSE)
  # KIT 的 9999 照原樣寫進作答檔（以前也是；0.5.0 起多了提醒）
  expect_true(9999 %in% kits_long_of(out)$item_score)
})

test_that("欄位表：名稱、順序、型別固定；必填的欄", {
  expect_equal(
    KITS_COLUMNS$column,
    c("visit_id", "subj_id", "cohort", "sex", "wave_m", "age_m", "item_id", "item_score", "domain")
  )
  expect_equal(KITS_COLUMNS$type, c("c", "c", "c", "c", "d", "d", "c", "i", "c"))
  expect_equal(
    KITS_COLUMNS$column[KITS_COLUMNS$required],
    c("visit_id", "cohort", "wave_m", "item_id", "item_score", "domain")
  )
})

test_that("沒對應的選填欄仍輸出（空白）；.csv.gz 讀回來欄名與順序完全相同", {
  dfs <- purrr::map(sim_dfs(), function(df) {
    dplyr::mutate(df, vid = paste(.data$release_id, .data$wave))
  })
  spec <- sim_spec(dfs)
  spec$map$visit_id <- "vid"
  spec$map$subj_id <- ""
  spec$map$sex <- ""
  spec$map$age_m <- ""
  cand <- kits_candidate_cols(dfs, c(kits_mapped_cols(spec$map)))
  dom <- kits_item_domains(cand$numeric, sim_table())
  spec$items <- dplyr::filter(dom, !is.na(.data$domain))[c("column", "domain")]
  spec$undecided <- character(0)
  out <- kits_build(dfs, c("a.csv", "b.csv"), spec)
  expect_length(out$problems, 0)
  path <- withr::local_tempfile(fileext = ".csv.gz")
  write_kits_long_gz(prep = out$prep, file = path)
  back <- readr::read_csv(path, col_types = readr::cols(.default = "c"))
  expect_equal(names(back), KITS_COLUMNS$column)
  expect_true(all(is.na(back$subj_id)))
  expect_true(all(is.na(back$sex)))
  expect_true(all(is.na(back$age_m)))
  expect_false(anyNA(back$visit_id))

  code <- code_kitsdata(code_files(c("a.csv", "b.csv"), c("t1", "t2")), spec)
  expect_match(code, "subj_id = NA_character_")
  expect_match(code, "age_m = NA_real_")
})

test_that("世代欄有空白時擋下（必填）", {
  dfs <- purrr::map(sim_dfs(), function(df) dplyr::mutate(df, co = "M03"))
  dfs[[1]]$co[1] <- NA
  spec <- sim_spec(dfs)
  spec$cohort <- list(list(type = "column", value = "co"), list(type = "text", value = "M36"))
  spec$undecided <- setdiff(spec$undecided, "co")
  out <- kits_build(dfs, c("a.csv", "b.csv"), spec)
  expect_match(out$problems, "cohort 是空白", all = FALSE)
})
