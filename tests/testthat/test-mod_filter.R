# 篩選與摘要：模組（用假的 files / working 直接驅動）與整個 app

filter_fake_args <- function(dfs) {
  n <- length(dfs)
  files <- list(
    keys = shiny::reactive(sprintf("f%d", seq_len(n))),
    names = shiny::reactive(sprintf("w%d.csv", seq_len(n))),
    labels = shiny::reactive(sprintf("t%d", seq_len(n))),
    data = shiny::reactive(dfs),
    encodings = shiny::reactive(rep("UTF-8", n))
  )
  working <- list(
    data = files$data, source = shiny::reactive("raw"), code_cells = shiny::reactive(0L)
  )
  list(files = files, working = working, goto = function(tab) NULL)
}

# 步驟分頁裡「篩選與摘要」的狀態
filter_tab_state <- function(states) {
  purrr::keep(states, function(s) identical(s$value, "filter"))[[1]]$state
}

filter_mod_dfs <- function() {
  list(
    tibble::tibble(
      id = 1:6, sex = haven::labelled(c(1, 2, 1, 2, NA, 1), c(男 = 1, 女 = 2)),
      score = c(10, 20, NA, 40, 50, 60)
    ),
    tibble::tibble(id = 1:3, y = c(5, 6, 7))
  )
}

test_that("模組：每個檔案各自設定，執行後下游拿到篩選後的資料；勾掉或改設定就回到原本的資料", {
  orig <- filter_mod_dfs()
  shiny::testServer(mod_filter_server, args = filter_fake_args(orig), {
    expect_false(session$returned$filtered())
    expect_identical(session$returned$data(), orig)
    # 第 1 個檔案：score 大於 15、拿掉 sex、依 score 由大到小
    session$setInputs(file = "f1", add_cond = 1)
    session$setInputs(c1_col = "score", c1_op = "gt", c1_num = 15)
    session$setInputs(keep_mode_f1 = "exclude", keep_cols_f1 = "sex",
                      sort_cols_f1 = "score", sort_desc_f1 = "score")
    expect_match(output$settings_line$html, "t1（w1.csv）")
    session$setInputs(run = 1)
    expect_match(output$status$html, "篩選完成")
    expect_match(output$status$html, "6 橫列 → 保留 4 橫列（排除 2 橫列）")
    # score 空白的那一列判斷不了：狀態框寫出來
    expect_match(output$status$html, "有 1 橫列因為條件用到的直欄是空白")
    expect_match(output$status$html, "t2（w2.csv）：沒有設定，原樣往後傳")
    expect_true(session$returned$filtered())
    expect_equal(session$returned$filter_state(), "on")
    out <- session$returned$data()
    expect_equal(out[[1]]$id, c(6L, 5L, 4L, 2L))
    expect_named(out[[1]], c("id", "score"))
    expect_identical(out[[2]], orig[[2]])
    expect_match(output$report, "其中因為空白")
    expect_match(output$code$html, "filter\\(score &gt; 15\\)")
    expect_match(output$code$html, "arrange\\(desc\\(score\\)\\)")
    expect_match(output$code$html, "select\\(!sex\\)")

    # 下載：選的檔案的篩選結果
    csv <- readr::read_csv(kit_test_download_file(session, "download_csv"), show_col_types = FALSE)
    expect_equal(csv$id, c(6, 5, 4, 2))

    # 勾掉「後面的分頁使用篩選後的資料」
    session$setInputs(use_downstream = FALSE)
    expect_false(session$returned$filtered())
    expect_identical(session$returned$data(), orig)
    session$setInputs(use_downstream = TRUE)
    expect_true(session$returned$filtered())

    # 改設定：舊結果失效，下游回到原本的資料並提醒
    session$setInputs(c1_num = 30)
    expect_match(output$status$html, "要重新執行")
    expect_false(session$returned$filtered())
    expect_equal(session$returned$filter_state(), "stale")
    expect_identical(session$returned$data(), orig)

    # 第 2 個檔案也設定；第 1 個的設定還在
    session$setInputs(file = "f2", add_cond = 2)
    session$setInputs(c2_col = "y", c2_op = "between", c2_lo = 6, c2_hi = 7)
    session$setInputs(run = 2)
    out <- session$returned$data()
    expect_equal(out[[1]]$id, c(6L, 5L, 4L))
    expect_equal(out[[2]]$y, c(6, 7))

    # 拿掉條件
    session$setInputs(c2_rm = 1)
    expect_match(output$status$html, "要重新執行")
    session$setInputs(run = 3)
    expect_identical(session$returned$data()[[2]], orig[[2]])
  })
})

test_that("模組：沒填完的條件不能執行；篩選後沒有任何列時說清楚", {
  shiny::testServer(mod_filter_server, args = filter_fake_args(filter_mod_dfs()), {
    session$setInputs(add_cond = 1)
    session$setInputs(c1_col = "score", c1_op = "gt")
    expect_match(output$ready$html, "還沒填數字")
    session$setInputs(c1_num = 1000)
    session$setInputs(run = 1)
    expect_match(output$status$html, "篩選後沒有任何橫列")
    expect_equal(nrow(session$returned$data()[[1]]), 0)
  })
})

test_that("模組：有數值標籤的欄用「是其中之一」，條件的選項寫成「1 = 男」", {
  shiny::testServer(mod_filter_server, args = filter_fake_args(filter_mod_dfs()), {
    session$setInputs(add_cond = 1)
    session$setInputs(c1_col = "sex")
    expect_match(output$c1_op_ui$html, "是其中之一")
    session$setInputs(c1_op = "in")
    expect_match(output$c1_value_ui$html, "1 = 男")
    session$setInputs(c1_values = "2")
    session$setInputs(run = 1)
    expect_equal(session$returned$data()[[1]]$id, c(2L, 4L))
    expect_s3_class(session$returned$data()[[1]]$sex, "haven_labelled")
    expect_match(output$code$html, "sex %in% c\\(2\\)")
  })
})

test_that("模組：摘要用篩選後的資料，按「計算摘要」才算，可以下載", {
  shiny::testServer(mod_filter_server, args = filter_fake_args(filter_mod_dfs()), {
    session$setInputs(sum_groups = "sex", sum_vars = "score",
                      sum_stats = c("n_valid", "mean"))
    expect_match(output$sum_source$html, "篩選前的資料")
    session$setInputs(sum_run = 1)
    expect_match(output$sum_result$html, "共 3 橫列")
    expect_match(output$sum_table, "\\[1\\] 男")
    csv <- readr::read_csv(kit_test_download_file(session, "sum_csv"), show_col_types = FALSE)
    expect_named(csv, c("sex", "variable", "n", "n_valid", "mean"))
    expect_equal(csv$mean, c(35, 30, 50))
    expect_match(output$code$html, "group_by\\(sex, variable\\)")

    # 篩選後：摘要改用篩選後的資料，舊的摘要清掉
    session$setInputs(add_cond = 1)
    session$setInputs(c1_col = "score", c1_op = "ge", c1_num = 20)
    session$setInputs(run = 1)
    expect_match(output$sum_source$html, "篩選後的資料")
    expect_match(output$sum_result$html, "計算摘要")
    session$setInputs(sum_run = 2)
    csv <- readr::read_csv(kit_test_download_file(session, "sum_csv"), show_col_types = FALSE)
    expect_equal(csv$n, c(1, 2, 1))
    expect_equal(csv$mean, c(60, 30, 50))

    # 不是數字的欄不能摘要
    session$setInputs(sum_vars = character(0))
    session$setInputs(sum_run = 3)
    expect_match(output$sum_result$html, "請選要摘要的變項")
  })
})

test_that("整個 app：疊加拿到篩選後的資料；勾掉或設定改了就用遺漏值處理的資料並提醒", {
  dir <- withr::local_tempdir()
  w1 <- file.path(dir, "w1.csv")
  w2 <- file.path(dir, "w2.csv")
  readr::write_csv(data.frame(release_id = 1:4, sex = c(1, 2, 1, 2), score = c(1, 9999, 3, 4)), w1)
  readr::write_csv(data.frame(release_id = 2:5, sex = c(2, 1, 2, 1), score = 5:8), w2)
  up <- function(paths) {
    data.frame(name = basename(paths), size = file.size(paths), type = "",
               datapath = paths, stringsAsFactors = FALSE)
  }
  shiny::testServer(kit_server, {
    session$setInputs(`upload-files` = up(c(w1, w2)))
    session$setInputs(`missing-codes` = "9999", `missing-match_text` = TRUE,
                      `missing-use_downstream` = TRUE, `missing-exclude_cols` = "release_id")
    session$setInputs(`missing-run` = 1)
    expect_equal(filter_tab_state(tab_states()), "ready")

    session$setInputs(`filter-file` = "f1", `filter-add_cond` = 1)
    session$setInputs(`filter-c1_col` = "sex", `filter-c1_op` = "eq", `filter-c1_num` = 1)
    session$setInputs(`filter-run` = 1)
    expect_match(output[["filter-status"]]$html, "4 橫列 → 保留 2 橫列")
    expect_equal(filter_tab_state(tab_states()), "done")
    expect_match(output[["stack-source_note"]]$html, "再經過「篩選與摘要」的資料")

    session$setInputs(`stack-run` = 1)
    expect_match(output[["stack-status"]]$html, "結果共 6 橫列")
    expect_match(output[["stack-settings_line"]]$html, "遺漏值處理、篩選後的資料")
    csv <- readr::read_csv(kit_test_download_file(session, "stack-download_csv"),
                           show_col_types = FALSE)
    expect_equal(sort(csv$release_id), c(1, 2, 3, 3, 4, 5))
    expect_match(output[["log-log"]]$html, "3 個步驟")

    # 勾掉：疊加的舊結果失效，再執行就是 8 列（遺漏值處理後的資料）
    session$setInputs(`filter-use_downstream` = FALSE)
    expect_match(output[["stack-status"]]$html, "要重新執行")
    expect_match(output[["stack-source_note"]]$html, "「遺漏值處理」處理後的資料")
    session$setInputs(`stack-run` = 2)
    expect_match(output[["stack-status"]]$html, "結果共 8 橫列")

    # 勾回來、改了條件：篩選結果過期，疊加用沒篩選的資料並提醒
    session$setInputs(`filter-use_downstream` = TRUE)
    session$setInputs(`filter-c1_num` = 2)
    expect_equal(filter_tab_state(tab_states()), "stale")
    expect_match(output[["stack-source_note"]]$html, "請回「篩選與摘要」重新按「執行篩選」")
    session$setInputs(`stack-run` = 3)
    expect_match(output[["stack-status"]]$html, "結果共 8 橫列")

    # ② 重新執行：篩選結果也過期
    session$setInputs(`filter-run` = 2)
    expect_equal(filter_tab_state(tab_states()), "done")
    session$setInputs(`missing-codes` = "9999, 8888", `missing-run` = 2)
    expect_equal(filter_tab_state(tab_states()), "stale")
  })
})

test_that("摘要表的整數欄（分組代碼）顯示成整數", {
  tab <- tibble::tibble(sex = c(1, 2), variable = "x", n = c(3L, 4L), mean = c(36, 52.25))
  out <- summary_display_ints(tab)
  expect_type(out$sex, "integer")
  expect_type(out$mean, "double")
})
