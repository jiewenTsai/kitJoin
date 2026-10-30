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

# 題目設定：選取幾列、做一個批次動作
kd_act <- function(session, rows, button, ...) {
  session$setInputs(`kitsdata-sel` = rows, ...)
  do.call(session$setInputs, stats::setNames(list(1), paste0("kitsdata-", button)))
}

test_that("⑥：題目設定給領域、排除、② 處理代碼、建議平移，轉換成功", {
  dir <- withr::local_tempdir()
  paths <- kd_files(dir)
  shiny::testServer(kit_server, {
    session$setInputs(`upload-files` = kd_upload(paths))
    kd_map(session)
    session$setInputs(`kitsdata-cohort_txt_f1` = "M03", `kitsdata-cohort_txt_f2` = "M36")
    table <- output[["kitsdata-settings_table"]]$html
    expect_match(table, 'value="lana01"')
    expect_match(table, 'value="extra"')
    expect_no_match(table, 'value="city"')   # 文字欄不列
    session$setInputs(`kitsdata-run` = 1)
    expect_match(output[["kitsdata-status"]]$html, "個納入的題目沒有領域")

    kd_act(session, "lana01", "do_domain", `kitsdata-bulk_domain` = "lan")
    kd_act(session, "bodyaf01", "do_domain", `kitsdata-bulk_domain` = "motor")
    kd_act(session, "extra", "do_exclude")
    expect_match(output[["kitsdata-settings_summary"]]$html, "納入 2、排除 1")

    # 9999 還在、也沒用 ②：提醒到 ②（不擋）
    expect_match(output[["kitsdata-codes_note"]]$html, "goto_missing")
    expect_no_match(output[["kitsdata-code"]]$html, "已經把遺漏值代碼改成 NA")
    session$setInputs(`kitsdata-goto_missing` = 1)
    session$setInputs(
      `missing-codes` = "9999", `missing-exclude_cols` = "release_id",
      `missing-match_text` = TRUE, `missing-use_downstream` = TRUE
    )
    session$setInputs(`missing-run` = 1)
    expect_null(output[["kitsdata-codes_note"]])
    expect_match(output[["kitsdata-code"]]$html, "已經把遺漏值代碼改成 NA")

    # 每題最低分都是 1：建議（不會自動做）；按了才平移
    expect_match(output[["kitsdata-shift_suggest"]]$html, "所有題目都從 1 開始")
    expect_no_match(output[["kitsdata-code"]]$html, "x - 1")
    session$setInputs(`kitsdata-apply_hint` = 1)
    expect_null(output[["kitsdata-shift_suggest"]])
    expect_match(output[["kitsdata-code"]]$html, "x - 1")

    session$setInputs(`kitsdata-run` = 2)
    expect_match(output[["kitsdata-status"]]$html, "檢查完成")
    expect_match(output[["kitsdata-result"]]$html, "download_gz")
    # 下載：網址結尾是檔名；.csv.gz 解壓後和未壓縮的 CSV 一樣
    href <- kit_link_attr(output[["kitsdata-result"]]$html, "kitsdata-download_gz", "href")
    expect_match(href, "/kits_long_[0-9_]+\\.csv\\.gz\\?")
    gz <- kit_test_download(session, "kitsdata-download_gz")
    expect_match(gz$headers[["Content-Disposition"]], 'filename="kits_long_[0-9_]+\\.csv\\.gz"')
    expect_equal(readBin(gz$content$file, "raw", 2), as.raw(c(0x1f, 0x8b)))
    csv <- kit_test_download_file(session, "kitsdata-download_csv")
    expect_equal(readLines(gzfile(gz$content$file)), readLines(csv))
    # 題目設定：按下載當下的設定（extra 已排除）
    expect_equal(
      kit_link_attr(output[["kitsdata-settings_dl_ui"]]$html, "kitsdata-settings_dl", "download"),
      KITS_SETTINGS_FILE
    )
    settings_csv <- readr::read_csv(
      kit_test_download_file(session, "kitsdata-settings_dl"), show_col_types = FALSE
    )
    expect_false(settings_csv$include[settings_csv$item_id == "extra"])
    expect_match(output[["kitsdata-visit_note"]]$html, "M03_K1_m03g01")
  })
})

test_that("⑥：世代空白時不轉換；改設定後舊結果失效", {
  dir <- withr::local_tempdir()
  paths <- kd_files(dir)
  shiny::testServer(kit_server, {
    session$setInputs(`upload-files` = kd_upload(paths))
    kd_map(session)
    session$setInputs(`kitsdata-cohort_txt_f1` = "M03", `kitsdata-cohort_txt_f2` = "")
    kd_act(session, c("lana01", "bodyaf01"), "do_domain", `kitsdata-bulk_domain` = "lan")
    kd_act(session, "extra", "do_exclude")
    session$setInputs(`kitsdata-run` = 1)
    expect_match(output[["kitsdata-status"]]$html, "m36.csv：還沒有填世代")

    session$setInputs(`kitsdata-cohort_txt_f2` = "M36")
    session$setInputs(`kitsdata-run` = 2)
    expect_match(output[["kitsdata-status"]]$html, "完成")
    session$setInputs(`kitsdata-map_wave` = "")
    expect_match(output[["kitsdata-status"]]$html, "要重新執行")
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
  expect_equal(nrow(kits_item_settings(list(), cand$numeric)), 0)
})

test_that("⑥：說明和下載區都有開啟 kitsUI 的按鈕（新分頁）", {
  ui <- as.character(mod_kitsdata_ui("kitsdata"))
  expect_match(ui, 'href="https://jiewentsai-kitsui.share.connect.posit.cloud/"', fixed = TRUE)
  expect_match(ui, 'target="_blank"', fixed = TRUE)
  dir <- withr::local_tempdir()
  paths <- kd_files(dir)
  shiny::testServer(kit_server, {
    session$setInputs(`upload-files` = kd_upload(paths))
    kd_map(session)
    session$setInputs(`kitsdata-cohort_txt_f1` = "M03", `kitsdata-cohort_txt_f2` = "M36")
    kd_act(session, c("lana01", "bodyaf01"), "do_domain", `kitsdata-bulk_domain` = "lan")
    kd_act(session, "extra", "do_exclude")
    session$setInputs(`kitsdata-run` = 1)
    html <- output[["kitsdata-result"]]$html
    expect_match(html, "開啟 kitsUI 上傳作答檔")
    expect_match(html, 'href="https://jiewentsai-kitsui.share.connect.posit.cloud/"', fixed = TRUE)
  })
})
