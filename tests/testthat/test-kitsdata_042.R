# 0.4.2：自己填的對應欄位、題目設定（領域、納入、計分對照）

# 一個檔案一個波次、檔案裡沒有 wave、wave_m 欄
one_wave <- function(ids, seed) {
  set.seed(seed)
  n <- length(ids)
  tibble::tibble(
    release_id = ids, sex = rep(c("boy", "girl"), length.out = n),
    lana01 = sample(1:5, n, TRUE), lana02 = sample(1:5, n, TRUE),
    bodyaf01 = sample(1:5, n, TRUE)
  )
}

# 題目設定：每一題都是 lan；fun 是計分對照的批次動作
typed_spec <- function(dfs, typed, fun = NULL) {
  map <- kits_guess_map(unique(unlist(purrr::map(dfs, names))))
  map$wave <- ""
  map$wave_m <- ""
  cand <- kits_candidate_cols(dfs, kits_mapped_cols(map))
  st <- kits_item_settings(dfs, cand$numeric)
  st <- kits_bulk_domain(st, st$column, "lan")
  if (!is.null(fun)) st <- fun(st)
  kits_spec(
    list(list(type = "text", value = "M03"), list(type = "text", value = "M03")), map,
    kits_settings_items(st), kits_settings_undecided(st), typed
  )
}

read_long <- function(path) {
  readr::read_csv(path, col_types = "ccccddcic", na = "", progress = FALSE)
}

test_that("沒有波次欄的兩個檔案：每個檔案自己填波次標籤與 wave_m", {
  dfs <- list(one_wave(c("K1", "K2"), 1), one_wave(c("K1", "K3"), 2))
  typed <- list(wave = c("m018", "m024"), wave_m = c("18", "24"))
  spec <- typed_spec(dfs, typed)
  out <- kits_build(dfs, c("w18.csv", "w24.csv"), spec)
  expect_length(out$problems, 0)
  expect_match(out$notes[1], "自己填的波次標籤")
  path <- withr::local_tempfile(fileext = ".csv.gz")
  write_kits_long_gz(prep = out$prep, file = path)
  long <- read_long(path)
  expect_named(long, KITS_COLUMNS$column)
  expect_setequal(unique(long$visit_id), c("M03_K1_m018", "M03_K2_m018", "M03_K1_m024",
                                           "M03_K3_m024"))
  expect_equal(unique(long$wave_m[long$visit_id == "M03_K3_m024"]), 24)
  expect_true(all(long$domain == "lan"))
})

test_that("自己填的值和「檔案裡有一欄、每一列都是這個值」寫出來完全相同", {
  dfs <- list(one_wave(c("K1", "K2", "K4"), 1), one_wave(c("K1", "K3"), 2))
  typed <- list(wave = c("m018", "m024"), wave_m = c("18", "24"), age_m = c("18.5", "24"))
  spec <- typed_spec(dfs, typed)
  a <- withr::local_tempfile(fileext = ".csv.gz")
  write_kits_long_gz(dfs, spec, a)

  with_cols <- purrr::map2(dfs, 1:2, function(df, i) {
    dplyr::mutate(df, wave = typed$wave[i], wave_m = as.numeric(typed$wave_m[i]),
                  age_m = as.numeric(typed$age_m[i]))
  })
  spec2 <- typed_spec(with_cols, list())
  spec2$map$wave <- "wave"
  spec2$map$wave_m <- "wave_m"
  spec2$map$age_m <- "age_m"
  spec2$items <- spec2$items[!spec2$items$column %in% c("wave_m", "age_m"), ]
  b <- withr::local_tempfile(fileext = ".csv.gz")
  write_kits_long_gz(with_cols, spec2, b)
  read_lines <- function(p) {
    con <- gzfile(p, open = "rb")
    on.exit(close(con))
    readLines(con)
  }
  expect_identical(read_lines(a), read_lines(b))
})

test_that("自己填的 wave_m 不是數字、或沒填：擋下並說是哪個檔案", {
  dfs <- list(one_wave("K1", 1), one_wave("K2", 2))
  spec <- typed_spec(dfs, list(wave_m = c("十八", "")))
  out <- kits_build(dfs, c("w18.csv", "w24.csv"), spec)
  expect_null(out$prep)
  expect_match(out$problems, "w18.csv：時間點（wave_m）要填數字（例如 18），現在是「十八」",
               all = FALSE)
  expect_match(out$problems, "w24.csv：還沒有填時間點", all = FALSE)
  spec <- typed_spec(dfs, list())
  expect_match(kits_build(dfs, c("a", "b"), spec)$problems, "請對應 wave_m", all = FALSE)
})

test_that("程式碼：自己填的值寫成 mutate()，平移量寫成 x - 1、x + 2", {
  dfs <- list(one_wave("K1", 1), one_wave("K2", 2))
  spec <- typed_spec(dfs, list(wave = c("m018", "m024"), wave_m = c("18", "24")),
                     fun = function(st) kits_bulk_shift(st, "lana01", 2))
  code <- code_kitsdata(code_files(c("a.csv", "b.csv"), c("t1", "t2")), spec)
  expect_match(code, 'mutate\\(wave = "m018", wave_m = 18\\)')
  expect_match(code, 'mutate\\(wave = "m024", wave_m = 24\\)')
  expect_match(code, "paste\\(cohort, subj_id, as_id\\(wave\\), sep")
  expect_match(code, "\\\\\\(x\\) x \\+ 2")
  spec <- typed_spec(dfs, list(wave = c("m018", "m024"), wave_m = c("18", "24")),
                     fun = function(st) kits_bulk_shift(st, "lana01", -1))
  code <- code_kitsdata(code_files(c("a.csv", "b.csv"), c("t1", "t2")), spec)
  expect_match(code, "\\\\\\(x\\) x - 1")
})

test_that("題目設定由資料建立：原分數不含 NA 與 7777 以上的值；已對應的欄不是候選", {
  df <- tibble::tibble(release_id = c("K1", "K2", "K3"), wave = "m03g01", wave_m = 3,
                       age_m = 3.2, sex = "boy", a1 = c(1, 3, NA), b2 = c(0, 9999, 2),
                       weight = c(12.5, 13, 14))
  map <- kits_guess_map(names(df))
  cand <- kits_candidate_cols(list(df), kits_mapped_cols(map))
  expect_setequal(cand$numeric, c("a1", "b2", "weight"))
  st <- kits_item_settings(list(df), cand$numeric)
  expect_equal(st$raw[[1]], c(1, 3))
  expect_equal(st$score[[2]], c(0, 2))
  expect_true(st$has_codes[2])
  expect_true(all(st$include))
  expect_true(all(is.na(st$domain)))
  expect_match(kits_settings_alerts(st)[2], "疑似遺漏值代碼 9999（7777 以上）：會照原樣輸出")
  expect_equal(kits_settings_undecided(st), c("a1", "b2", "weight"))
  st <- kits_bulk_include(st, "weight", FALSE)
  expect_equal(kits_settings_undecided(st), c("a1", "b2"))
})

test_that("批次動作：平移、反向、合併、還原（都在目前的計分上做）", {
  st <- tibble::tibble(
    column = c("x", "y"), include = TRUE, domain = "lan", source = NA_character_,
    raw = list(1:5 + 0, c(0, 1, 2)), score = list(1:5 + 0, c(0, 1, 2)), has_codes = FALSE
  )
  st <- kits_bulk_shift(st, "x", -1)
  expect_equal(st$score[[1]], 0:4)
  expect_equal(st$score[[2]], c(0, 1, 2))   # 沒選的列不動
  st <- kits_bulk_reverse(st, "x")           # 最高分預設是目前最高計分 4
  expect_equal(st$score[[1]], 4:0)
  st <- kits_bulk_reverse(st, "y", 3)
  expect_equal(st$score[[2]], c(3, 2, 1))
  st <- kits_bulk_merge(st, "x", c(4, 5), 0)  # 原分數 4、5 → 計分 0
  expect_equal(st$score[[1]], c(4, 3, 2, 0, 0))
  expect_match(kits_settings_alerts(st)[1], "分數有缺")
  expect_match(kits_settings_alerts(st)[2], "沒有 0 分")
  st <- kits_bulk_restore(st, c("x", "y"))
  expect_equal(st$score, st$raw)
  expect_equal(kits_parse_values("4、5, 6"), c(4, 5, 6))
  st <- kits_bulk_shift(st, "y", -1)
  expect_match(kits_settings_alerts(st)[2], "有負分")
})

test_that("題目設定下載再上傳：納入、領域、計分對照都回來", {
  dfs <- list(one_wave(c("K1", "K2", "K3"), 1))
  st <- kits_item_settings(dfs, c("lana01", "lana02", "bodyaf01")) |>
    kits_bulk_domain(c("lana01", "lana02"), "lan") |>
    kits_bulk_domain("bodyaf01", "motor") |>
    kits_bulk_shift(c("lana01", "lana02"), -1) |>
    kits_bulk_include("lana02", FALSE)
  path <- withr::local_tempfile(fileext = ".csv")
  readr::write_excel_csv(kits_settings_export(st), path, na = "")
  back <- readr::read_csv(path, show_col_types = FALSE)
  expect_named(back, c("item_id", "include", "domain", "map_raw_to_score", "item_id_out"))
  expect_match(back$map_raw_to_score[1], "^[0-9]+=[0-9]+(;[0-9]+=[0-9]+)*$")

  up <- kits_settings_import(back)
  expect_equal(up$type, "settings")
  fresh <- kits_item_settings(dfs, c("lana01", "lana02", "bodyaf01"))
  got <- kits_settings_apply(fresh, up)
  expect_equal(got$include, st$include)
  expect_equal(got$domain, st$domain)
  expect_equal(got$score, st$score)
  expect_equal(got$source, rep("設定檔", 3))
})

test_that("上傳的題目表只給領域；r 題查不到時用原題；設定檔的對照少了新的原分數時擋下", {
  st <- tibble::tibble(
    column = c("cogd01r", "lana01"), include = TRUE, domain = NA_character_,
    source = NA_character_, raw = list(c(0, 1), c(0, 1, 2)), score = list(c(0, 1), c(0, 1, 2)),
    has_codes = FALSE
  )
  up <- kits_settings_import(data.frame(item_id = c("cogd01", "lana01"), domain = c("cog", "lan")))
  expect_equal(up$type, "table")
  got <- kits_settings_apply(st, up)
  expect_equal(got$domain, c("cog", "lan"))
  expect_match(got$source[1], "原題 cogd01")

  # 上一波的設定：lana01 是 1=0;2=1（平移過），這一波多了原分數 0 → 沒有計分
  prev <- data.frame(item_id = "lana01", include = TRUE, domain = "lan",
                     map_raw_to_score = "1=0;2=1")
  got <- kits_settings_apply(st, kits_settings_import(prev))
  expect_true(is.na(got$score[[2]][1]))
  expect_match(kits_settings_alerts(got)[2], "原分數 0 沒有計分")
  # 沒改過的對照（原分數 = 計分）會自動補上新的原分數
  same <- data.frame(item_id = "lana01", include = TRUE, domain = "lan",
                     map_raw_to_score = "1=1;2=2")
  got <- kits_settings_apply(st, kits_settings_import(same))
  expect_equal(got$score[[2]], c(0, 1, 2))
  expect_error(kits_settings_import(data.frame(item_id = "a", map_raw_to_score = "x")),
               "看不懂")
})

test_that("建議：所有納入的題都從同一個 m 開始才建議平移 −m", {
  st <- tibble::tibble(
    column = c("x", "y"), include = TRUE, domain = "lan", source = NA_character_,
    raw = list(c(1, 2), c(1, 3)), score = list(c(1, 2), c(1, 3)), has_codes = FALSE
  )
  expect_equal(kits_settings_shift_hint(st), 1)
  st$score[[2]] <- c(0, 1)
  expect_null(kits_settings_shift_hint(st))
  st <- kits_bulk_include(st, "y", FALSE)
  expect_equal(kits_settings_shift_hint(st), 1)
})

# ── 模組 ────────────────────────────────────────────────────────────────────

kd42_upload <- function(dir) {
  dfs <- list(one_wave(c("K1", "K2"), 1), one_wave(c("K1", "K3"), 2))
  dfs[[2]]$lana01 <- c(0, 2)
  paths <- file.path(dir, c("w18.csv", "w24.csv"))
  purrr::walk2(dfs, paths, readr::write_csv)
  data.frame(name = basename(paths), size = file.size(paths), type = "", datapath = paths,
             stringsAsFactors = FALSE)
}

# 第 1 步：世代 M03；沒有波次欄，波次標籤與時間點自己填
kd42_step1 <- function(session) {
  kd_cells(session, "cohort", c("M03", "M03"))
  kd_cells(session, "subj_id", c("release_id", "release_id"))
  kd_cells(session, "wave", c("m018", "m024"))
  kd_cells(session, "wave_m", c("18", "24"))
  kd_click(session, "ok1")
}

test_that("模組：每個檔案自己填波次標籤、時間點；填錯時第 1 步列出問題；轉換成功", {
  dir <- withr::local_tempdir()
  shiny::testServer(kit_server, {
    session$setInputs(`upload-files` = kd42_upload(dir))
    kd_cells(session, "cohort", c("M03", "M03"))
    kd_cells(session, "subj_id", c("release_id", "release_id"))
    kd_cells(session, "wave", c("m018", "m024"))
    kd_cells(session, "wave_m", c("18", ""))
    kd_click(session, "ok1")
    expect_match(kd_out(output, "step1_problems"), "還沒填：w24.csv")
    kd_cells(session, "wave_m", c("18", "abc"))
    kd_click(session, "ok1")
    expect_match(kd_out(output, "step1_problems"), "w24.csv：時間點（wave_m）要填數字")
    expect_equal(kd_head_state(output, 1), "current")
    kd_cells(session, "wave_m", c("18", "24"))
    kd_click(session, "ok1")
    expect_equal(kd_out(output, "step1_problems"), "")
    expect_equal(kd_head_state(output, 1), "done")
    expect_match(kd_out(output, "head1"), "18、24")
    expect_match(kd_out(output, "visit_note"), "M03_K1_m018")
    kd_set(session, pick_prefix = "lana, bodyaf", pick_domain = "lan")
    kd_click(session, "pick_add")
    kd_click(session, "ok2")
    kd_click(session, "run")
    expect_match(kd_out(output, "status"), "完成")
    expect_match(output[["kitsdata-code"]]$html, "mutate\\(wave = \"m018\", wave_m = 18\\)")
    long <- read_long(kit_test_download_file(session, "kitsdata-download_csv"))
    expect_setequal(unique(long$visit_id), c("M03_K1_m018", "M03_K2_m018", "M03_K1_m024",
                                             "M03_K3_m024"))
  })
})

test_that("模組：找欄名、勾選全部、批次動作；計分欄顯示原分數→計分", {
  dir <- withr::local_tempdir()
  shiny::testServer(kit_server, {
    session$setInputs(`upload-files` = kd42_upload(dir))
    kd42_step1(session)
    kd_set(session, tbl_all = TRUE)
    table <- output[["kitsdata-items_table"]]$html
    expect_match(table, "<th>計分（原分數→計分）</th>", fixed = TRUE)
    expect_match(table, "（沒挑）")

    # 找 lana → 勾選符合的列 → 要這幾欄、設領域
    kd_set(session, tbl_search = "lana")
    kd_click(session, "tbl_sel_all")
    expect_equal(output[["kitsdata-sel_count"]], "已勾選 2 列")
    kd_set(session, bulk_action = "include")
    kd_click(session, "bulk_apply")
    kd_set(session, bulk_action = "domain", bulk_domain = "lan")
    kd_click(session, "bulk_apply")
    expect_match(kd_out(output, "items_status"), "挑出 2 欄（lan 2 題）")

    # 全部勾選後減 1：lana01 有 0 分 → 有負分
    kd_set(session, tbl_search = "")
    kd_click(session, "tbl_sel_all")
    expect_equal(output[["kitsdata-sel_count"]], "已勾選 3 列")
    kd_set(session, bulk_action = "shift", bulk_shift = -1)
    kd_click(session, "bulk_apply")
    table <- kd_out(output, "items_table")
    expect_match(table, "lana01 lan 0→-1 ")
    expect_match(table, "有負分")

    # 還原、合併：lana01 原分數 0、1 → 計分 0
    kd_set(session, sel = "lana01", bulk_action = "restore")
    kd_click(session, "bulk_apply")
    expect_match(kd_out(output, "items_table"), "lana01 lan 0 1 ")
    kd_set(session, bulk_action = "merge", merge_from = "0、1", merge_to = 0)
    kd_click(session, "bulk_apply")
    expect_match(kd_out(output, "items_table"), "lana01 lan 0→0 1→0 2→2 ")
    expect_match(output[["kitsdata-code"]]$html, "x == 1 ~ 0")
    kd_click(session, "tbl_sel_none")
    expect_equal(output[["kitsdata-sel_count"]], "已勾選 0 列")
  })
})

test_that("模組：沒有勾選時批次動作不會改設定；參數不對時也不改", {
  dir <- withr::local_tempdir()
  shiny::testServer(kit_server, {
    session$setInputs(`upload-files` = kd42_upload(dir))
    kd42_step1(session)
    kd_set(session, pick_prefix = "lana", pick_domain = "lan")
    kd_click(session, "pick_add")
    before <- output[["kitsdata-items_table"]]$html
    kd_set(session, bulk_action = "shift", bulk_shift = -1)
    kd_click(session, "bulk_apply")
    expect_identical(output[["kitsdata-items_table"]]$html, before)
    # 加減 0、合併沒填：不改
    kd_set(session, sel = "lana01", bulk_shift = 0)
    kd_click(session, "bulk_apply")
    kd_set(session, bulk_action = "merge", merge_from = "")
    kd_click(session, "bulk_apply")
    kd_set(session, bulk_action = "domain", bulk_domain = "")
    kd_click(session, "bulk_apply")
    after <- output[["kitsdata-items_table"]]$html
    expect_identical(gsub(" checked=\"checked\"", "", after), before)
  })
})
