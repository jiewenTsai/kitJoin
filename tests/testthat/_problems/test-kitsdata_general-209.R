# Extracted from test-kitsdata_general.R:209

# prequel ----------------------------------------------------------------------
kg_wave <- function(ids, prefix, wave_m, seed) {
  set.seed(seed)
  n <- length(ids)
  out <- tibble::tibble(id = ids, wave_m = wave_m)
  for (k in 1:3) out[[paste0(prefix, "q", k)]] <- sample(0:3, n, TRUE)
  out
}
kg_labelled <- function(x) {
  haven::labelled(x, c("非常不同意" = 1, "不同意" = 2, "同意" = 3, "非常同意" = 4,
                       "跳答" = 98, "遺漏值" = 99), label = "W1:老師很關心我")
}
kg_upload <- function(paths) {
  data.frame(name = basename(paths), size = file.size(paths), type = "", datapath = paths,
             stringsAsFactors = FALSE)
}

# test -------------------------------------------------------------------------
dir <- withr::local_tempdir()
path <- file.path(dir, "w1.csv")
readr::write_csv(tibble::tibble(id = 1:5, wave_m = 0, q1 = c(0, 1, 2, 9, 1),
                                  q2 = c(1, 2, 3, 99, 1)), path)
shiny::testServer(kit_server, {
    session$setInputs(`upload-files` = kg_upload(path))
    kd_cells(session, "cohort", "B1")
    kd_cells(session, "subj_id", "id")
    kd_click(session, "ok1")
    kd_set(session, pick_prefix = "q", pick_domain = "a")
    kd_click(session, "pick_add")
    kd_click(session, "ok2")
    expect_match(kd_out(output, "items_table"), "q1 a 0 1 2 9 分數有缺")
    expect_match(kd_out(output, "items_table"), "q2 a 1 2 3 代碼 99")   # 99：和分數分開
    expect_equal(kd_check_state(output, "遺漏值代碼"), "todo")
    expect_match(kd_out(output, "checks"), "1 欄有遺漏值代碼 99（共 1 格）")
    expect_match(output[["kitsdata-checks"]]$html, "kitsdata-goto_missing")
    session$setInputs(`missing-codes` = "9, 99")   # 「遺漏值處理」還沒執行：只拿來找代碼
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
