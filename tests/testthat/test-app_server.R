upload_df <- function(paths) {
  data.frame(
    name = basename(paths),
    size = file.size(paths),
    type = "",
    datapath = paths,
    stringsAsFactors = FALSE
  )
}

write_wave <- function(dir, name, df) {
  path <- file.path(dir, name)
  readr::write_csv(df, path)
  path
}

test_that("上傳讀不了的檔案不會讓 app 斷線，其他檔案照常載入", {
  dir <- withr::local_tempdir()
  good1 <- write_wave(dir, "w1.csv", data.frame(release_id = 1:3, score = 1:3))
  good2 <- write_wave(dir, "w2.csv", data.frame(release_id = 2:4, score = 4:6))
  bad <- file.path(dir, "data.xlsx")
  writeLines("not excel", bad)

  shiny::testServer(kit_server, {
    session$setInputs(`upload-files` = upload_df(c(good1, bad, good2)))
    expect_length(output[["upload-file_list"]]$html, 1)
    expect_match(output[["upload-file_list"]]$html, "w2.csv")
    expect_no_match(output[["upload-file_list"]]$html, "data.xlsx")
  })
})

test_that("疊加不會清掉寬格式串接的結果，兩邊都能下載", {
  dir <- withr::local_tempdir()
  w1 <- write_wave(dir, "w1.csv", data.frame(release_id = 1:3, score = 1:3))
  w2 <- write_wave(dir, "w2.csv", data.frame(release_id = 2:4, score = 4:6))

  shiny::testServer(kit_server, {
    session$setInputs(`upload-files` = upload_df(c(w1, w2)))
    session$setInputs(
      `join-by_vars` = "release_id", `join-method` = "left_join",
      `join-keys_as_text` = TRUE
    )
    session$setInputs(`join-run` = 1)
    expect_match(output[["join-downloads"]]$html, "download_csv")

    session$setInputs(`stack-run` = 1)
    expect_match(output[["stack-downloads"]]$html, "download_csv")
    expect_match(output[["join-downloads"]]$html, "download_csv")
    expect_match(output[["join-status"]]$html, "串接完成")
  })
})

test_that("沒有結果時下載按鈕是停用的", {
  shiny::testServer(kit_server, {
    session$setInputs(`join-by_vars` = NULL)
    expect_match(output[["join-downloads"]]$html, "disabled")
    expect_match(output[["join-downloads"]]$html, "執行串接")
  })
})

test_that("改了波次名稱後，舊結果失效並提示重新執行", {
  dir <- withr::local_tempdir()
  w1 <- write_wave(dir, "w1.csv", data.frame(release_id = 1:3, score = 1:3))
  w2 <- write_wave(dir, "w2.csv", data.frame(release_id = 2:4, score = 4:6))

  shiny::testServer(kit_server, {
    session$setInputs(`upload-files` = upload_df(c(w1, w2)))
    session$setInputs(`stack-run` = 1)
    expect_match(output[["stack-downloads"]]$html, "download_csv")
    session$setInputs(`upload-label_f2` = "w2")
    expect_match(output[["stack-status"]]$html, "要重新執行")
    expect_match(output[["stack-downloads"]]$html, "disabled")
  })
})

test_that("一次上傳多個檔案時，每一列都能改波次名稱、排序、移除", {
  dir <- withr::local_tempdir()
  w1 <- write_wave(dir, "w1.csv", data.frame(release_id = 1:3, score = 1:3))
  w2 <- write_wave(dir, "w2.csv", data.frame(release_id = 2:4, score = 4:6))
  w3 <- write_wave(dir, "w3.csv", data.frame(release_id = 3:5, score = 7:9))

  shiny::testServer(
    mod_upload_server,
    args = list(goto = function(tab) NULL),
    {
      session$setInputs(files = upload_df(c(w1, w2, w3)))
      expect_equal(current_labels(), c("t1", "t2", "t3"))

      session$setInputs(label_f1 = "pre")
      session$setInputs(label_f2 = "mid")
      expect_equal(current_labels(), c("pre", "mid", "t3"))

      session$setInputs(down_f1 = 1)
      expect_equal(keys(), c("f2", "f1", "f3"))

      session$setInputs(remove_f2 = 1)
      expect_equal(keys(), c("f1", "f3"))
      expect_equal(current_labels(), c("pre", "t3"))
    }
  )
})

test_that("只有 1 個檔案：② 可以轉換並下載，⑦ 使用 ② 處理後的資料", {
  shiny::testServer(kit_server, {
    path <- tempfile(fileext = ".csv")
    readr::write_csv(
      tibble::tibble(release_id = c("001", "002", "003"), score = c(1, 9999, 3)), path
    )
    session$setInputs(`upload-files` = data.frame(
      name = "one.csv", size = file.size(path), type = "text/csv", datapath = path
    ))
    expect_match(output[["upload-next_step"]]$html, "遺漏值轉 NA")
    expect_match(output[["upload-next_step"]]$html, "長寬互轉")
    session$setInputs(`missing-codes` = "9999", `missing-use_downstream` = TRUE,
                      `missing-match_text` = TRUE, `missing-run` = 1)
    expect_match(output[["missing-result"]]$html, "下載處理後的檔案")
    session$setInputs(`missing-view_file` = "1")
    csv <- readr::read_csv(
      kit_test_download_file(session, "missing-download_csv"), show_col_types = FALSE
    )
    expect_equal(csv$score, c(1, NA, 3))
    session$setInputs(`reshape-file` = "1")
    expect_match(output[["reshape-source_note"]]$html, "處理後")
  })
})

test_that("① 空檔與只有表頭的檔不會被加入（低：空檔）", {
  dir <- withr::local_tempdir()
  empty <- file.path(dir, "empty.csv")
  file.create(empty)
  header <- file.path(dir, "header.csv")
  writeLines("id,score", header)
  good <- write_wave(dir, "w1.csv", data.frame(id = 1:3, score = 1:3))
  shiny::testServer(
    mod_upload_server,
    args = list(goto = function(tab) NULL),
    {
      session$setInputs(files = upload_df(c(empty, header, good)))
      expect_equal(keys(), "f1")
      expect_equal(files$names(), "w1.csv")
    }
  )
})

test_that("④ 下載的 SAV 讀得回來，即使原檔有 SPSS 存不了的文字值標籤（B2）", {
  bad <- haven::labelled(
    c("我喜歡的", "外套"),
    stats::setNames(paste0("99", strrep(" ", 78), "\v"), "遺漏值")
  )
  dfs <- list(tibble::tibble(id = 1:2, k = bad), tibble::tibble(id = 3:4, k = bad))
  files <- list(
    keys = shiny::reactive(c("f1", "f2")),
    names = shiny::reactive(c("a.sav", "b.sav")),
    labels = shiny::reactive(c("t1", "t2")),
    data = shiny::reactive(dfs),
    encodings = shiny::reactive(c(NA_character_, NA_character_))
  )
  working <- list(
    data = shiny::reactive(dfs),
    source = shiny::reactive("raw"),
    code_cells = shiny::reactive(0L)
  )
  shiny::testServer(
    mod_stack_server,
    args = list(files = files, working = working, goto = function(tab) NULL),
    {
      session$setInputs(run = 1)
      sav <- kit_test_download_file(session, "download_sav")
      expect_true(sav_readable(sav))
      back <- haven::read_sav(sav)
      expect_equal(unname(attr(back$k, "labels")), "99")
      expect_equal(nrow(back), 4)
    }
  )
})

test_that("③ 結果會暴增時跳出確認，按「仍要執行」才串接（A3）", {
  dir <- withr::local_tempdir()
  w1 <- write_wave(dir, "w1.csv", data.frame(id = 1:60, birth = rep(1:3, 20)))
  w2 <- write_wave(dir, "w2.csv", data.frame(id = 1:60, birth = rep(1:3, 20)))
  shiny::testServer(kit_server, {
    session$setInputs(`upload-files` = upload_df(c(w1, w2)))
    session$setInputs(
      `join-by_vars` = "birth", `join-method` = "left_join", `join-keys_as_text` = TRUE
    )
    expect_match(output[["join-id_check"]]$html, "ID 重複")
    session$setInputs(`join-run` = 1)
    expect_match(output[["join-status"]]$html, "仍要執行")
    expect_match(output[["join-downloads"]]$html, "disabled")
    session$setInputs(`join-run_confirm` = 1)
    expect_match(output[["join-status"]]$html, "串接完成")
  })
})

test_that("⑤ 選到出生年這種重複很多的欄，不會顯示完成（A3）", {
  dir <- withr::local_tempdir()
  w1 <- write_wave(dir, "w1.csv", data.frame(id = 1:60, birth = rep(1:3, 20)))
  w2 <- write_wave(dir, "w2.csv", data.frame(id = 1:30, birth = rep(1:3, 10)))
  shiny::testServer(kit_server, {
    session$setInputs(`upload-files` = upload_df(c(w1, w2)))
    session$setInputs(`attrition-baseline` = "f1", `attrition-followup` = "f2",
                      `attrition-id_vars` = "birth")
    session$setInputs(`attrition-run` = 1)
    expect_match(output[["attrition-status"]]$html, "停下來")
    expect_match(output[["attrition-status"]]$html, "不是用來辨識")
  })
})

test_that("③⑤ 各檔 ID 欄名不同（w1_id、w2_id、w3id）時自動改成分別指定並能執行（C1）", {
  dir <- withr::local_tempdir()
  w1 <- write_wave(dir, "w1.csv", data.frame(w1_id = 1:4, a = 1:4))
  w2 <- write_wave(dir, "w2.csv", data.frame(w2_id = 2:5, a = 5:8))
  w3 <- write_wave(dir, "w3.csv", data.frame(w3id = 3:6, a = 9:12))
  shiny::testServer(kit_server, {
    session$setInputs(`upload-files` = upload_df(c(w1, w2, w3)))
    expect_match(output[["join-per_file_ids"]]$html, "w2_id")
    # 自動勾選是送到瀏覽器的更新；測試裡自己設定
    session$setInputs(
      `join-id_per_file` = TRUE, `join-id_f1` = "w1_id", `join-id_f2` = "w2_id",
      `join-id_f3` = "w3id", `join-method` = "full_join", `join-keys_as_text` = TRUE
    )
    expect_match(output[["join-id_check"]]$html, "w3id")
    session$setInputs(`join-run` = 1)
    expect_match(output[["join-status"]]$html, "串接完成")
    csv <- readr::read_csv(
      kit_test_download_file(session, "join-download_csv"), show_col_types = FALSE
    )
    expect_named(csv, c("w1_id", "a_t1", "a_t2", "a_t3"))
    expect_equal(nrow(csv), 6)
    expect_match(output[["join-code"]]$html, "rename\\(data_2, w1_id = w2_id\\)")

    session$setInputs(
      `attrition-baseline` = "f1", `attrition-followup` = "f3",
      `attrition-id_per_file` = TRUE, `attrition-id_vars` = "w1_id",
      `attrition-follow_id_vars` = "w3id"
    )
    session$setInputs(`attrition-run` = 1)
    expect_match(output[["attrition-status"]]$html, "留存 2 人、流失 2 人")
    expect_match(output[["attrition-code"]]$html, "data_3\\$w3id")
  })
})

test_that("② 依數值標籤轉換；沒有 KIT 代碼時自動建議這個方式（A6、C5）", {
  dir <- withr::local_tempdir()
  df <- tibble::tibble(
    w3id = c(98, 2, 3),
    q1 = haven::labelled(c(1, 98, 99), c(同意 = 1, 跳答 = 98, 遺漏值 = 99)),
    kg = c(99, 50, 60)
  )
  path <- file.path(dir, "w3.sav")
  haven::write_sav(df, path)
  kit <- write_wave(dir, "kit.csv", data.frame(release_id = 1:2, x = c(9999, 1)))
  shiny::testServer(kit_server, {
    session$setInputs(`upload-files` = upload_df(path))
    expect_match(output[["missing-mode_note"]]$html, "依 SPSS 數值標籤")
    expect_match(output[["upload-next_step"]]$html, "遺漏值代碼")
    session$setInputs(`missing-na_mode` = "labels", `missing-keywords` = kit_na_label_keywords_default(),
                      `missing-exclude_cols` = "w3id")
    expect_match(output[["missing-label_found"]]$html, "1 個欄位")
    expect_match(output[["missing-code"]]$html, "na_words")
    session$setInputs(`missing-use_downstream` = TRUE, `missing-run` = 1)
    expect_match(output[["missing-status"]]$html, "共替換 2 格")
    session$setInputs(`missing-view_file` = "1")
    expect_match(output[["missing-detail"]], "98 跳答")
  })
  shiny::testServer(kit_server, {
    session$setInputs(`upload-files` = upload_df(kit))
    expect_null(output[["missing-mode_note"]])
  })
})

test_that("② 標籤標的遺漏值遠多於 KIT 代碼時也改用依數值標籤（TDCYP：91 格等於 9999 這類值）", {
  expect_identical(na_mode_suggestion(0, 0), list(switch = FALSE, note = NULL))
  expect_false(na_mode_suggestion(500, 300)$switch)
  expect_null(na_mode_suggestion(500, 300)$note)
  s <- na_mode_suggestion(0, 120)
  expect_true(s$switch)
  expect_match(s$note, "資料中沒有 7777、9999")
  s <- na_mode_suggestion(91, 687525)
  expect_true(s$switch)
  expect_match(s$note, "只有 91 格等於")
  expect_match(s$note, "687,525 格遺漏值")
  s <- na_mode_suggestion(100, 300)
  expect_false(s$switch)
  expect_match(s$note, "可以改選「依 SPSS 數值標籤」")

  dir <- withr::local_tempdir()
  df <- tibble::tibble(
    id = 1:12,
    q1 = haven::labelled(c(rep(99, 11), 1), c(同意 = 1, 遺漏值 = 99)),
    money = c(9999, rep(10, 11))
  )
  path <- file.path(dir, "w2.sav")
  haven::write_sav(df, path)
  shiny::testServer(kit_server, {
    session$setInputs(`upload-files` = upload_df(path))
    expect_match(output[["missing-mode_note"]]$html, "只有 1 格等於")
  })
})

test_that("② 改了代碼、排除欄或文字比對後，舊結果失效，③ 會提醒（A8）", {
  dir <- withr::local_tempdir()
  w1 <- write_wave(dir, "w1.csv", data.frame(release_id = 1:3, score = c(1, 9999, 3)))
  w2 <- write_wave(dir, "w2.csv", data.frame(release_id = 2:4, score = c(9999, 5, 6)))
  shiny::testServer(kit_server, {
    session$setInputs(`upload-files` = upload_df(c(w1, w2)))
    session$setInputs(`missing-codes` = "9999", `missing-match_text` = TRUE,
                      `missing-use_downstream` = TRUE, `missing-exclude_cols` = "release_id")
    session$setInputs(`missing-run` = 1)
    expect_match(output[["join-source_note"]]$html, "處理後的資料")

    session$setInputs(`missing-codes` = "9999, 8888")
    expect_match(output[["missing-status"]]$html, "要重新執行")
    expect_match(output[["join-source_note"]]$html, "請回「遺漏值轉 NA」重新按")

    session$setInputs(`missing-run` = 2)
    expect_match(output[["join-source_note"]]$html, "處理後的資料")
    session$setInputs(`missing-exclude_cols` = c("release_id", "score"))
    expect_match(output[["join-source_note"]]$html, "請回「遺漏值轉 NA」重新按")

    session$setInputs(`missing-exclude_cols` = "release_id", `missing-run` = 3)
    session$setInputs(`missing-match_text` = FALSE)
    expect_match(output[["missing-status"]]$html, "要重新執行")
  })
})

test_that("在 ① 移除檔案：其他檔案的 ② 結果保留；加入新檔案時提醒重新執行（A8）", {
  dir <- withr::local_tempdir()
  w1 <- write_wave(dir, "w1.csv", data.frame(w1_id = 1:3, score = c(1, 9999, 3)))
  w2 <- write_wave(dir, "w2.csv", data.frame(release_id = 2:4, score = c(9999, 5, 6)))
  w3 <- write_wave(dir, "w3.csv", data.frame(release_id = 2:4, score = c(9999, 5, 6)))
  shiny::testServer(kit_server, {
    session$setInputs(`upload-files` = upload_df(c(w1, w2, w3)))
    session$setInputs(`missing-codes` = "9999", `missing-match_text` = TRUE,
                      `missing-use_downstream` = TRUE,
                      `missing-exclude_cols` = c("w1_id", "release_id"))
    session$setInputs(`missing-run` = 1)
    session$setInputs(`upload-remove_f1` = 1)
    # 瀏覽器會把排除欄更新成目前還有的欄
    session$setInputs(`missing-exclude_cols` = "release_id")
    expect_match(output[["missing-status"]]$html, "結果保留")
    expect_match(output[["join-source_note"]]$html, "處理後的資料")

    session$setInputs(`upload-files` = upload_df(w1))
    expect_match(output[["missing-status"]]$html, "要重新執行")
    expect_match(output[["stack-source_note"]]$html, "請回「遺漏值轉 NA」重新按")
  })
})

test_that("① 同一個檔案加兩次會提醒，但仍然加入（低）", {
  dir <- withr::local_tempdir()
  w1 <- write_wave(dir, "w1.csv", data.frame(id = 1:3, score = 1:3))
  shiny::testServer(
    mod_upload_server,
    args = list(goto = function(tab) NULL),
    {
      session$setInputs(files = upload_df(w1))
      session$setInputs(files = upload_df(w1))
      expect_equal(keys(), c("f1", "f2"))
    }
  )
})
