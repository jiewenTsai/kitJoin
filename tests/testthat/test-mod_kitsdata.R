kd_upload <- function(paths) {
  data.frame(name = basename(paths), size = file.size(paths), type = "", datapath = paths,
             stringsAsFactors = FALSE)
}

kd_files <- function(dir) {
  w1 <- tibble::tibble(
    release_id = c("K1", "K2", "K1"), wave = c("m03g01", "m03g01", "m06g02"),
    wave_m = c(3, 3, 6), age_m = c(3.1, 3.2, 6.1), sex = c("boy", "girl", "boy"),
    city = "臺北市", lana01 = c(1, 2, 9999), bodyaf01 = c(2, 1, 3), extra = c(5, 5, 5)
  )
  w2 <- tibble::tibble(
    release_id = c("J1", "J2"), wave = "m36g01", wave_m = 36, age_m = 36.2,
    sex = c("girl", "boy"), city = "臺中市", lana01 = c(2, 1), bodyaf01 = c(1, 1),
    extra = c(5, 5)
  )
  paths <- file.path(dir, c("m03.csv", "m36.csv"))
  readr::write_csv(w1, paths[1])
  readr::write_csv(w2, paths[2])
  paths
}

# 對應欄位（testServer 裡 updateSelectInput() 不會改 input，要自己設）
kd_map <- function(session) {
  session$setInputs(
    `kitsdata-map_visit_id` = "", `kitsdata-map_subj_id` = "release_id",
    `kitsdata-map_sex` = "sex", `kitsdata-map_wave` = "wave",
    `kitsdata-map_wave_m` = "wave_m", `kitsdata-map_age_m` = "age_m",
    `kitsdata-cohort_src_f1` = "__text__", `kitsdata-cohort_src_f2` = "__text__"
  )
}

test_that("⑥：設定好世代、規則與排除後，轉換成功並有 .csv.gz", {
  dir <- withr::local_tempdir()
  paths <- kd_files(dir)
  shiny::testServer(kit_server, {
    session$setInputs(`upload-files` = kd_upload(paths))
    kd_map(session)
    session$setInputs(
      `kitsdata-cohort_txt_f1` = "M03", `kitsdata-cohort_txt_f2` = "M36",
      `kitsdata-rules` = "^lan = lan\n^body = motor"
    )
    # extra 沒有領域：要決定
    expect_match(output[["kitsdata-undecided"]]$html, "extra")
    extra_id <- paste0("kitsdata-nodom_", paste(as.character(charToRaw("extra")), collapse = ""))
    session$setInputs(`kitsdata-run` = 1)
    expect_match(output[["kitsdata-status"]]$html, "沒有領域、也還沒決定")

    do.call(session$setInputs, stats::setNames(list("__exclude__"), extra_id))
    # 9999 還在、也沒用 ②：提醒到 ②（不擋）；減 1 的建議不管 9999
    expect_match(output[["kitsdata-codes_note"]]$html, "goto_missing")
    expect_no_match(output[["kitsdata-code"]]$html, "已經把遺漏值代碼改成 NA")
    session$setInputs(`kitsdata-goto_missing` = 1)
    expect_match(output[["kitsdata-shift_suggest"]]$html, "建議")
    # 到 ② 把 9999 改成 NA
    session$setInputs(
      `missing-codes` = "9999", `missing-exclude_cols` = "release_id",
      `missing-match_text` = TRUE, `missing-use_downstream` = TRUE
    )
    session$setInputs(`missing-run` = 1)
    expect_null(output[["kitsdata-codes_note"]])
    expect_match(output[["kitsdata-code"]]$html, "已經把遺漏值代碼改成 NA")
    # 每題最低分是 1：建議勾步驟 1，但不會自動勾
    expect_match(output[["kitsdata-shift_suggest"]]$html, "建議")
    expect_null(input[["kitsdata-step_shift"]])
    session$setInputs(`kitsdata-step_shift` = TRUE, `kitsdata-shift_scope` = "all")
    expect_null(output[["kitsdata-shift_suggest"]])

    session$setInputs(`kitsdata-run` = 2)
    expect_match(output[["kitsdata-status"]]$html, "檢查完成")
    expect_match(output[["kitsdata-result"]]$html, "download_gz")
    expect_match(output[["kitsdata-code"]]$html, "步驟 1")
    expect_no_match(output[["kitsdata-code"]]$html, "步驟 2")
    expect_match(output[["kitsdata-visit_note"]]$html, "M03_K1_m03g01")
  })
})

test_that("⑥：世代空白時不轉換；改設定後舊結果失效", {
  dir <- withr::local_tempdir()
  paths <- kd_files(dir)
  shiny::testServer(kit_server, {
    session$setInputs(`upload-files` = kd_upload(paths))
    kd_map(session)
    extra_id <- paste0("kitsdata-nodom_", paste(as.character(charToRaw("extra")), collapse = ""))
    session$setInputs(
      `kitsdata-cohort_txt_f1` = "M03", `kitsdata-cohort_txt_f2` = "",
      `kitsdata-rules` = "^lan = lan\n^body = motor"
    )
    do.call(session$setInputs, stats::setNames(list("__exclude__"), extra_id))
    session$setInputs(`kitsdata-run` = 1)
    expect_match(output[["kitsdata-status"]]$html, "m36.csv：還沒有填世代")

    session$setInputs(`kitsdata-cohort_txt_f2` = "M36")
    session$setInputs(`kitsdata-run` = 2)
    expect_match(output[["kitsdata-status"]]$html, "完成")
    session$setInputs(`kitsdata-map_wave` = "")
    expect_match(output[["kitsdata-status"]]$html, "需要重新執行")
    expect_match(output[["kitsdata-visit_note"]]$html, "wave_m")
  })
})

test_that("⑥：還沒有上傳檔案時不會出錯", {
  shiny::testServer(kit_server, {
    expect_match(output[["kitsdata-source_note"]]$html, "請先到")
    expect_null(output[["kitsdata-cohort_table"]])
    session$setInputs(`kitsdata-run` = 1)
    expect_match(output[["kitsdata-status"]]$html, "請先到「① 上傳檔案」")
  })
})

test_that("候選欄與領域：沒有檔案時是空的表", {
  cand <- kits_candidate_cols(list(), character(0))
  expect_equal(cand$numeric, character(0))
  expect_named(kits_item_domains(cand$numeric), c("column", "domain", "source"))
})
