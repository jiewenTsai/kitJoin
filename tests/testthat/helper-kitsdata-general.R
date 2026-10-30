# ⑥ 通用性（非 KIT 資料）的測試共用工具

# 「用 R 怎麼做？」的程式碼實際跑一次：資料寫成 CSV（或 SAV）、題目設定寫成 CSV，
# 在暫存資料夾裡執行，回傳 kits_long（和 app 的作答檔比對用）
kg_run_code <- function(dfs, spec, settings, sav = FALSE, labels = NULL) {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  ext <- if (sav) ".sav" else ".csv"
  names <- paste0("w", seq_along(dfs), ext)
  for (i in seq_along(dfs)) {
    if (sav) haven::write_sav(dfs[[i]], file.path(dir, names[i]))
    else readr::write_csv(dfs[[i]], file.path(dir, names[i]), na = "")
  }
  readr::write_excel_csv(kits_settings_export(settings), file.path(dir, KITS_SETTINGS_FILE),
                         na = "")
  # 和 ⑥ 的畫面一樣：性別、wave_m、age_m 欄的遺漏值代碼由資料算出來
  side <- kits_side_codes(dfs, kits_side_cols(spec$map, spec$per_file %||% list()), spec$na_codes)
  code <- code_kitsdata(code_files(names, labels %||% paste0("t", seq_along(dfs))), spec,
                        side_codes = side)
  env <- new.env(parent = globalenv())   # 和使用者在 Console 執行一樣
  withr::with_dir(dir, suppressMessages(eval(parse(text = code), envir = env)))
  list(long = env$kits_long, code = code)
}

# 比對兩個作答檔（依 visit_id、item_id 排序，9 欄都比）
kg_same_long <- function(got, app) {
  key <- function(x) {
    x <- as.data.frame(x)
    x$wave_m <- as.numeric(x$wave_m)
    x$age_m <- as.numeric(x$age_m)
    x$item_score <- as.integer(x$item_score)
    x <- x[order(x$visit_id, x$item_id), KITS_COLUMNS$column]
    rownames(x) <- NULL
    x
  }
  testthat::expect_equal(key(got), key(app))
}

# 題目設定 → spec（世代都是自己填）
kg_spec <- function(settings, map, cohorts, typed = list(), per_file = list()) {
  kits_spec(
    purrr::map(cohorts, function(x) list(type = "text", value = x)), map,
    kits_settings_items(settings), kits_settings_undecided(settings), typed,
    per_file = per_file
  )
}

# TDCYP 的路徑（沒有這些檔案時，用到的測試會略過）
kg_tdcyp <- function(file) {
  file.path("/Users/jiewentsai/Documents/ClaudePlayMac/lab20261030_kitsui/lab1031/tdcyp_data", file)
}
