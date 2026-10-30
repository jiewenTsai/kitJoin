# 「kits 作答檔」分頁：三個步驟卡（世代與波次、挑題目、計分檢查）→ 轉換 → 下載
# 共用工具（kd_upload、kd_set、kd_click、kd_cells、kd_out…）在 helper-kitsdata-module.R

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

# 第 1 步：世代自己填（其他欄照欄名猜），按「對，下一步」
kd_step1 <- function(session, cohorts = c("M03", "M36")) {
  kd_cells(session, "cohort", cohorts)
  kd_click(session, "ok1")
}

# 用欄名挑題目（可以給領域）
kd_pick <- function(session, prefix, domain = "") {
  kd_set(session, pick_prefix = prefix, pick_domain = domain)
  kd_click(session, "pick_add")
}

# 逐題表：勾選幾列、做一件事
kd_bulk <- function(session, rows, action, ...) {
  kd_set(session, sel = rows, bulk_action = action, ...)
  kd_click(session, "bulk_apply")
}

# 三步都做完（lana01、bodyaf01 都是 lan；9999 照原樣當分數；全部減 1）
kd_ready <- function(session) {
  kd_step1(session)
  kd_pick(session, "lana, bodyaf", "lan")
  kd_click(session, "ok2")
  kd_click(session, "ack_codes")
  kd_click(session, "do_shift")
}

test_that("領域、排除、到「遺漏值處理」改掉代碼、減 1，轉換成功、下載", {
  dir <- withr::local_tempdir()
  paths <- kd_files(dir)
  shiny::testServer(kit_server, {
    session$setInputs(`upload-files` = kd_upload(paths))
    kd_step1(session)
    expect_equal(kd_head_state(output, 1), "done")
    expect_equal(kd_head_state(output, 2), "current")

    kd_pick(session, "lana", "lan")
    kd_pick(session, "bodyaf", "motor")
    kd_pick(session, "extra")
    expect_match(kd_out(output, "items_status"), "挑出 3 欄")
    expect_match(kd_out(output, "text_cols"), "city")         # 文字欄不能當題目
    kd_click(session, "ok2")
    expect_equal(kd_head_state(output, 3), "current")
    expect_equal(kd_check_state(output, "還沒有領域"), "todo")
    expect_match(kd_out(output, "checks"), "1 欄還沒有領域")
    kd_click(session, "run")
    expect_match(kd_out(output, "status"), "還不能轉換：第 3 步有 2 件事要決定")   # 代碼、領域

    # 逐題表：extra 不要
    kd_bulk(session, "extra", "exclude")
    expect_match(kd_out(output, "items_status"), "挑出 2 欄（lan 1 題、motor 1 題）")
    expect_equal(kd_check_state(output, "每一題都有領域"), "ok")

    # 9999 還在、也沒用「遺漏值處理」：要決定（不會自己改）
    expect_equal(kd_check_state(output, "遺漏值代碼 9999"), "todo")
    expect_match(output[["kitsdata-checks"]]$html, "kitsdata-goto_missing")
    expect_no_match(output[["kitsdata-code"]]$html, "已經把遺漏值代碼改成 NA")
    kd_click(session, "goto_missing")
    session$setInputs(
      `missing-codes` = "9999", `missing-exclude_cols` = "release_id",
      `missing-match_text` = TRUE, `missing-use_downstream` = TRUE
    )
    session$setInputs(`missing-run` = 1)
    expect_equal(kd_check_state(output, "遺漏值處理"), "ok")
    expect_match(kd_out(output, "checks"), "遺漏值代碼已經用「遺漏值處理」改成 NA")
    expect_match(output[["kitsdata-code"]]$html, "已經把遺漏值代碼改成 NA")
    expect_match(kd_out(output, "items_status"), "挑出 2 欄")   # 設定沒有不見

    # 每題最低分都是 1：建議（不會自己做）；按了才減
    expect_equal(kd_check_state(output, "都從 1 開始"), "todo")
    expect_no_match(output[["kitsdata-code"]]$html, "x - 1")
    kd_click(session, "do_shift")
    expect_equal(kd_check_state(output, "現在從 0 開始"), "ok")
    expect_match(output[["kitsdata-code"]]$html, "x - 1")
    expect_match(kd_out(output, "ready"), "三步都好了")

    kd_click(session, "run")
    expect_match(kd_out(output, "status"), "【完成】")
    expect_match(output[["kitsdata-result"]]$html, "download_gz")
    # 下載：網址結尾是檔名；.csv.gz 解壓後和未壓縮的 CSV 一樣
    href <- kit_link_attr(output[["kitsdata-result"]]$html, "kitsdata-download_gz", "href")
    expect_match(href, "/kits_long_[0-9_]+\\.csv\\.gz\\?")
    gz <- kit_test_download(session, "kitsdata-download_gz")
    expect_match(gz$headers[["Content-Disposition"]], 'filename="kits_long_[0-9_]+\\.csv\\.gz"')
    expect_equal(readBin(gz$content$file, "raw", 2), as.raw(c(0x1f, 0x8b)))
    csv <- kit_test_download_file(session, "kitsdata-download_csv")
    expect_equal(readLines(gzfile(gz$content$file)), readLines(csv))
    long <- readr::read_csv(csv, col_types = "ccccddcic")
    expect_false(9999 %in% long$item_score)
    expect_setequal(unique(long$domain), c("lan", "motor"))
    expect_equal(min(long$item_score), 0)
    # 題目設定：按下載當下的設定（extra 沒挑）
    expect_equal(
      kit_link_attr(output[["kitsdata-settings_dl_ui"]]$html, "kitsdata-settings_dl", "download"),
      KITS_SETTINGS_FILE
    )
    settings_csv <- readr::read_csv(
      kit_test_download_file(session, "kitsdata-settings_dl"), show_col_types = FALSE
    )
    expect_false(settings_csv$include[settings_csv$item_id == "extra"])
    expect_true(all(settings_csv$include[settings_csv$item_id != "extra"]))
    expect_match(kd_out(output, "visit_note"), "M03_K1_m03g01")
  })
})

test_that("世代空白時第 1 步不能確認；改設定後舊結果失效", {
  dir <- withr::local_tempdir()
  paths <- kd_files(dir)
  shiny::testServer(kit_server, {
    session$setInputs(`upload-files` = kd_upload(paths))
    kd_step1(session, c("M03", ""))
    expect_match(kd_out(output, "step1_problems"), "世代還沒填：m36.csv")
    expect_equal(kd_head_state(output, 1), "current")
    expect_equal(kd_head_state(output, 2), "locked")
    kd_click(session, "run")
    expect_match(kd_out(output, "status"), "還不能轉換：第 1 步確認、第 2 步挑題目")
    expect_null(output[["kitsdata-result"]])

    # 填好之後提示就不見；確認、挑題目、決定、轉換
    kd_cells(session, "cohort", c("M03", "M36"))
    expect_equal(kd_out(output, "step1_problems"), "")
    kd_click(session, "ok1")
    kd_pick(session, "lana, bodyaf", "lan")
    kd_click(session, "ok2")
    kd_click(session, "ack_codes")
    kd_click(session, "do_shift")
    kd_click(session, "run")
    expect_match(kd_out(output, "status"), "完成")
    expect_false(is.null(output[["kitsdata-result"]]))

    # 不用波次標籤：舊結果清除，列次編號改用 wave_m
    kd_cells(session, "wave", c("", ""))
    expect_match(kd_out(output, "status"), "舊的結果已清除")
    expect_null(output[["kitsdata-result"]])
    expect_match(kd_out(output, "visit_note"), "M03_K1_3")
  })
})

test_that("還沒有上傳檔案時不會出錯", {
  shiny::testServer(kit_server, {
    expect_match(kd_out(output, "source_note"), "請先到「檔案上傳」")
    expect_null(output[["kitsdata-files_table"]])
    expect_null(output[["kitsdata-more_table"]])
    expect_no_error(output[["kitsdata-head1"]])
    expect_no_error(output[["kitsdata-head2"]])
    expect_no_error(output[["kitsdata-head3"]])
    expect_match(kd_out(output, "checks"), "還沒有挑題目")
    expect_match(kd_out(output, "items_table"), "沒有符合的欄")
    expect_match(kd_out(output, "ready"), "第 1 步確認")
    kd_click(session, "ok1")
    kd_click(session, "ok2")
    kd_click(session, "pick_add")
    kd_click(session, "bulk_apply")
    kd_click(session, "run")
    expect_match(kd_out(output, "status"), "還不能轉換")
    expect_null(output[["kitsdata-result"]])
  })
})

test_that("候選欄與領域：沒有檔案時是空的表", {
  cand <- kits_candidate_cols(list(), character(0))
  expect_equal(cand$numeric, character(0))
  expect_named(kits_item_domains(cand$numeric), c("column", "domain", "source"))
  expect_equal(nrow(kits_item_settings(list(), cand$numeric)), 0)
})

test_that("說明和下載區都有開啟 kitsUI 的按鈕（新分頁）", {
  ui <- as.character(mod_kitsdata_ui("kitsdata"))
  expect_match(ui, 'href="https://jiewentsai-kitsui.share.connect.posit.cloud/"', fixed = TRUE)
  expect_match(ui, 'target="_blank"', fixed = TRUE)
  dir <- withr::local_tempdir()
  paths <- kd_files(dir)
  shiny::testServer(kit_server, {
    session$setInputs(`upload-files` = kd_upload(paths))
    kd_ready(session)
    kd_click(session, "run")
    html <- output[["kitsdata-result"]]$html
    expect_match(html, "開啟 kitsUI，上傳這個檔")
    expect_match(html, 'href="https://jiewentsai-kitsui.share.connect.posit.cloud/"', fixed = TRUE)
    expect_match(html, 'target="_blank"', fixed = TRUE)
  })
})

# ── 新的流程 ─────────────────────────────────────────────────────────────────

test_that("一開始一題都不納入：要上傳題目表或用欄名挑；「全部清掉」回到沒有", {
  dir <- withr::local_tempdir()
  paths <- kd_files(dir)
  tab <- file.path(dir, "items.csv")
  readr::write_csv(tibble::tibble(item_id = c("lana01", "bodyaf01"), domain = c("lan", "motor")),
                   tab)
  shiny::testServer(kit_server, {
    session$setInputs(`upload-files` = kd_upload(paths))
    kd_step1(session)
    expect_equal(kd_out(output, "items_status"), "")
    expect_match(kd_out(output, "items_list"), "還沒有挑題目")
    expect_match(kd_out(output, "items_table"), "沒有符合的欄")
    expect_match(kd_out(output, "checks"), "還沒有挑題目")
    # 也顯示沒挑的欄：三個數字欄都在，都是「沒挑」
    kd_set(session, tbl_all = TRUE)
    table <- output[["kitsdata-items_table"]]$html
    expect_match(table, 'value="lana01"')
    expect_match(table, 'value="extra"')
    expect_no_match(table, 'value="city"')
    expect_length(gregexpr("（沒挑）", table)[[1]], 3)
    # 沒有挑：不能到第 3 步
    kd_click(session, "ok2")
    expect_equal(kd_head_state(output, 2), "current")
    expect_equal(kd_head_state(output, 3), "locked")

    # 題目表：列到的題才納入
    session$setInputs(`kitsdata-item_file` = kd_upload(tab))
    expect_match(kd_out(output, "items_status"), "items.csv：挑出 2 欄（lan 1 題、motor 1 題）")
    expect_no_match(kd_out(output, "items_list"), "extra")
    kd_click(session, "pick_clear")
    expect_match(kd_out(output, "items_list"), "還沒有挑題目")

    # 用欄名挑
    kd_pick(session, "LANA, extra")
    expect_match(kd_out(output, "items_status"), "挑出 2 欄")
    expect_match(kd_out(output, "items_list"), "lana01")
    expect_match(kd_out(output, "items_list"), "extra")
    expect_no_match(kd_out(output, "items_list"), "bodyaf01")
    kd_click(session, "ok2")
    expect_equal(kd_head_state(output, 3), "current")
  })
})

test_that("第 1 步：世代空白時按「對，下一步」列出還差什麼，不會到第 2 步", {
  dir <- withr::local_tempdir()
  paths <- kd_files(dir)
  shiny::testServer(kit_server, {
    session$setInputs(`upload-files` = kd_upload(paths))
    expect_equal(kd_out(output, "step1_problems"), "")
    kd_click(session, "ok1")
    expect_match(kd_out(output, "step1_problems"), "還差：")
    expect_match(kd_out(output, "step1_problems"), "世代還沒填：m03.csv、m36.csv")
    expect_equal(kd_head_state(output, 1), "current")
    expect_equal(kd_head_state(output, 2), "locked")
    expect_match(kd_out(output, "head2"), "先完成第 1 步")
    expect_match(kd_out(output, "ready"), "第 1 步確認")
    # 只填一個
    kd_cells(session, "cohort", c("", "M36"))
    kd_click(session, "ok1")
    expect_match(kd_out(output, "step1_problems"), "世代還沒填：m03.csv")
    expect_no_match(kd_out(output, "step1_problems"), "m36.csv")
    expect_equal(kd_head_state(output, 2), "locked")
  })
})

test_that("第 3 步還有要決定的事時，不能轉換", {
  dir <- withr::local_tempdir()
  paths <- kd_files(dir)
  shiny::testServer(kit_server, {
    session$setInputs(`upload-files` = kd_upload(paths))
    kd_step1(session)
    kd_pick(session, "lana, bodyaf", "lan")
    kd_click(session, "ok2")
    expect_equal(kd_check_state(output, "遺漏值代碼"), "todo")
    expect_equal(kd_check_state(output, "都從 1 開始"), "todo")
    expect_match(kd_out(output, "ready"), "還差：第 3 步有 2 件事要決定")
    expect_match(output[["kitsdata-run_ui"]]$html, "disabled")
    kd_click(session, "run")
    expect_match(kd_out(output, "status"), "還不能轉換：第 3 步有 2 件事要決定")
    expect_null(output[["kitsdata-result"]])
    # 只決定一件：還是不行
    kd_click(session, "ack_shift")
    expect_equal(kd_check_state(output, "不改（你選的）"), "ok")
    kd_click(session, "run")
    expect_match(kd_out(output, "status"), "還不能轉換：第 3 步有 1 件事要決定")
    expect_null(output[["kitsdata-result"]])
    kd_click(session, "ack_codes")
    expect_no_match(output[["kitsdata-run_ui"]]$html, "disabled")
    kd_click(session, "run")
    expect_match(kd_out(output, "status"), "完成")
    # 「不用改」：分數照原樣從 1 開始
    long <- readr::read_csv(kit_test_download_file(session, "kitsdata-download_csv"),
                            col_types = "ccccddcic")
    expect_equal(min(long$item_score), 1)
  })
})

test_that("遺漏值代碼：「它們是真的分數」變成沒問題，「改回來」又要決定", {
  dir <- withr::local_tempdir()
  paths <- kd_files(dir)
  shiny::testServer(kit_server, {
    session$setInputs(`upload-files` = kd_upload(paths))
    kd_step1(session)
    kd_pick(session, "lana, bodyaf", "lan")
    kd_click(session, "ok2")
    expect_equal(kd_check_state(output, "9999"), "todo")
    expect_match(kd_out(output, "checks"), "1 欄有遺漏值代碼 9999（共 1 格）")
    expect_match(output[["kitsdata-checks"]]$html, "kitsdata-ack_codes")
    expect_no_match(output[["kitsdata-checks"]]$html, "kitsdata-unack_codes")
    kd_click(session, "ack_codes")
    expect_equal(kd_check_state(output, "9999"), "ok")
    expect_match(kd_out(output, "checks"), "1 欄的 9999 照原樣當成分數（你選的）")
    expect_match(output[["kitsdata-checks"]]$html, "kitsdata-unack_codes")
    kd_click(session, "unack_codes")
    expect_equal(kd_check_state(output, "9999"), "todo")
    expect_match(output[["kitsdata-checks"]]$html, "kitsdata-ack_codes")
  })
})

test_that("「全部減 1」減的是所有挑出來的題，沒挑的不動", {
  dir <- withr::local_tempdir()
  paths <- kd_files(dir)
  shiny::testServer(kit_server, {
    session$setInputs(`upload-files` = kd_upload(paths))
    kd_step1(session)
    kd_pick(session, "lana, bodyaf", "lan")
    kd_click(session, "ok2")
    kd_set(session, tbl_all = TRUE)
    expect_match(kd_out(output, "items_table"), "lana01 lan 1 2")
    kd_click(session, "do_shift")
    table <- kd_out(output, "items_table")
    expect_match(table, "lana01 lan 1→0 2→1")
    expect_match(table, "bodyaf01 lan 1→0 2→1 3→2")
    expect_match(table, "extra （沒挑） （沒有） 5 ")         # 沒挑：不動
    expect_equal(kd_check_state(output, "分數全部減了 1"), "ok")
    expect_match(output[["kitsdata-code"]]$html, "x - 1")
  })
})

test_that("逐題表換頁：勾選留著；「勾選全部符合的列」「全部不勾」", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "w1.csv")
  df <- tibble::tibble(id = 1:3, wave_m = 0)
  for (k in 1:30) df[[sprintf("q%02d", k)]] <- c(0, 1, 2)
  readr::write_csv(df, path)
  shiny::testServer(kit_server, {
    session$setInputs(`upload-files` = kd_upload(path))
    kd_cells(session, "cohort", "B1")
    kd_cells(session, "subj_id", "id")
    kd_click(session, "ok1")
    kd_pick(session, "q")
    kd_click(session, "ok2")
    expect_equal(output[["kitsdata-tbl_page"]], "第 1 / 2 頁")
    page1 <- output[["kitsdata-items_table"]]$html
    expect_match(page1, 'value="q25"')
    expect_no_match(page1, 'value="q26"')

    kd_set(session, sel = c("q01", "q02"))
    kd_click(session, "tbl_next")
    expect_equal(output[["kitsdata-tbl_page"]], "第 2 / 2 頁")
    page2 <- output[["kitsdata-items_table"]]$html
    expect_match(page2, 'value="q30"')
    expect_no_match(page2, 'value="q01"')
    kd_set(session, sel = "q27")
    expect_equal(output[["kitsdata-sel_count"]], "已勾選 3 列")
    kd_click(session, "tbl_prev")
    page1 <- output[["kitsdata-items_table"]]$html
    expect_match(page1, 'value="q01" checked="checked"', fixed = TRUE)
    expect_match(page1, 'value="q02" checked="checked"', fixed = TRUE)
    expect_no_match(page1, 'value="q03" checked', fixed = TRUE)

    # 對三列設領域（兩頁的都改到）
    kd_set(session, bulk_action = "domain", bulk_domain = "a")
    kd_click(session, "bulk_apply")
    expect_match(kd_out(output, "items_status"), "（a 3 題）")
    expect_match(kd_out(output, "checks"), "27 欄還沒有領域")

    # 找欄名後全選：只勾符合的
    kd_set(session, tbl_search = "q1")
    expect_equal(output[["kitsdata-tbl_page"]], "第 1 / 1 頁")
    kd_click(session, "tbl_sel_all")
    expect_equal(output[["kitsdata-sel_count"]], "已勾選 10 列")
    kd_click(session, "tbl_sel_none")
    expect_equal(output[["kitsdata-sel_count"]], "已勾選 0 列")
  })
})

test_that("轉換完成後三步都打 ✓；按第 2 步的「改」重新打開第 2 步", {
  dir <- withr::local_tempdir()
  paths <- kd_files(dir)
  shiny::testServer(kit_server, {
    session$setInputs(`upload-files` = kd_upload(paths))
    kd_ready(session)
    expect_equal(kd_head_state(output, 3), "current")
    kd_click(session, "run")
    expect_match(kd_out(output, "status"), "完成")
    for (k in 1:3) {
      expect_equal(kd_head_state(output, k), "done")
      expect_match(kd_out(output, paste0("head", k)), "^✓")
      expect_match(output[[paste0("kitsdata-head", k)]]$html, paste0("kitsdata-edit", k))
    }
    expect_match(kd_out(output, "head1"), "2 個檔案 · 世代 M03、M36")
    expect_match(kd_out(output, "head2"), "挑出 2 欄（lan 2 題）")
    expect_match(kd_out(output, "head3"), "分數全部減了 1")
    expect_null(output[["kitsdata-ready"]])
    expect_null(output[["kitsdata-run_ui"]])

    kd_click(session, "edit2")
    expect_equal(kd_head_state(output, 2), "current")
    expect_equal(kd_head_state(output, 1), "done")
    expect_equal(output[["kitsdata-open"]], "2")
    # 只是打開來看：結果還在
    expect_false(is.null(output[["kitsdata-result"]]))
  })
})

test_that("標頭重畫時「改」連結送出 0：不會跳回前一步", {
  dir <- withr::local_tempdir()
  paths <- kd_files(dir)
  shiny::testServer(kit_server, {
    session$setInputs(`upload-files` = kd_upload(paths))
    kd_step1(session)
    kd_pick(session, "lana, bodyaf", "lan")
    kd_click(session, "ok2")
    expect_equal(output[["kitsdata-open"]], "3")
    # 瀏覽器重畫標頭時，新的 actionLink 送出 0
    session$setInputs(`kitsdata-edit1` = 0, `kitsdata-edit2` = 0)
    expect_equal(output[["kitsdata-open"]], "3")
    expect_equal(kd_head_state(output, 3), "current")
    # 真的按了（值 > 0）才打開
    session$setInputs(`kitsdata-edit2` = 1)
    expect_equal(output[["kitsdata-open"]], "2")
  })
})
