# A2：⑦ 寬轉長不把各欄不同的數值標籤合併成第一欄的那一套

# TDCYP 的樣子：w1d5_*（1=完全沒有…5=非常厲害）、w1c21_*（1=非常不同意…4=非常同意）
lab_wide <- function() {
  d5 <- c("完全沒有" = 1, "輕微" = 2, "中等程度" = 3, "厲害" = 4, "非常厲害" = 5)
  c21 <- c("非常不同意" = 1, "不同意" = 2, "同意" = 3, "非常同意" = 4)
  tibble::tibble(
    id = 1:3,
    w1d5_1 = haven::labelled(c(1, 2, 5), d5, label = "W1:感覺緊張不安"),
    w1d5_2 = haven::labelled(c(3, 4, 1), d5, label = "W1:覺得容易苦惱"),
    w1c21_1 = haven::labelled(c(1, 4, 3), c21, label = "W1:老師很關心我")
  )
}

test_that("A2：數值標籤不一樣時拿掉（值不變），並提醒；變項標籤也拿掉", {
  df <- lab_wide()
  args <- list(cols = c("w1d5_1", "w1d5_2", "w1c21_1"), names_to = "item", values_to = "score")
  out <- reshape_longer(df, args)
  expect_false(haven::is.labelled(out$result$score))
  expect_null(attr(out$result$score, "labels"))
  expect_null(attr(out$result$score, "label", exact = TRUE))
  expect_equal(out$result$score, c(1, 3, 1, 2, 4, 4, 5, 1, 3))
  expect_match(out$warnings, "2 種不同的數值標籤", all = FALSE)
  expect_match(out$warnings, "w1c21_1（1=非常不同意", all = FALSE)
  expect_match(out$summary, "變項標籤不一樣", all = FALSE)
  expect_true(out$labels$conflict)
  # tidyr 的 conflicting value labels 警告不會漏出來
  expect_no_warning(pivot_longer_run(df, args))
})

test_that("A2：數值標籤都一樣時保留；變項標籤不一樣時只拿掉變項標籤", {
  df <- lab_wide()
  out <- reshape_longer(df, list(cols = c("w1d5_1", "w1d5_2"), values_to = "score"))
  expect_true(haven::is.labelled(out$result$score))
  expect_equal(attr(out$result$score, "labels"), attr(df$w1d5_1, "labels"))
  expect_null(attr(out$result$score, "label", exact = TRUE))
  expect_length(out$warnings, 0)

  df$w1d5_2 <- haven::labelled(c(3, 4, 1), attr(df$w1d5_1, "labels"), label = "W1:感覺緊張不安")
  out <- reshape_longer(df, list(cols = c("w1d5_1", "w1d5_2"), values_to = "score"))
  expect_equal(attr(out$result$score, "label", exact = TRUE), "W1:感覺緊張不安")
  expect_false(out$labels$zap_var)
})

test_that("A2：有的欄有數值標籤、有的沒有：也拿掉", {
  df <- lab_wide()
  df$plain <- c(2, 2, 2)
  out <- reshape_longer(df, list(cols = c("w1d5_1", "plain"), values_to = "score"))
  expect_false(haven::is.labelled(out$result$score))
  expect_match(out$warnings, "有的欄沒有數值標籤", all = FALSE)
})

test_that("A2：程式碼先拿掉標籤，跑出來和 app 一樣；下載的 SAV 沒有錯的標籤", {
  df <- lab_wide()
  args <- list(cols = c("w1d5_1", "w1d5_2", "w1c21_1"), names_to = "item", values_to = "score")
  app <- reshape_longer(df, args)$result
  labels <- pivot_longer_labels(df, args$cols)
  code <- code_longer(utils::modifyList(PIVOT_LONGER_DEFAULTS, args), "data_t1", names(df),
                      labels)$tidy
  expect_match(code, "haven::zap_label(haven::zap_labels(x))", fixed = TRUE)
  env <- new.env()
  env$data_t1 <- df
  suppressMessages(eval(parse(text = code), envir = env))
  expect_equal(env$long, app)

  path <- withr::local_tempfile(fileext = ".sav")
  haven::write_sav(app, path)
  back <- haven::read_sav(path)
  expect_null(attr(back$score, "labels"))

  # 標籤都一樣時，程式碼不拿掉數值標籤
  same <- pivot_longer_labels(df, c("w1d5_1", "w1d5_2"))
  code <- code_longer(list(cols = c("w1d5_1", "w1d5_2")), "data_t1", names(df), same)$tidy
  expect_match(code, "across(c(w1d5_1, w1d5_2), haven::zap_label))", fixed = TRUE)
  expect_null(pivot_longer_labels_code(pivot_longer_labels(tibble::tibble(id = 1, a = 1), "a"), "d"))
})


test_that("A2：⑦ 的畫面顯示提醒，程式碼也有拿掉標籤", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "w1.sav")
  haven::write_sav(lab_wide(), path)
  shiny::testServer(kit_server, {
    session$setInputs(`upload-files` = data.frame(name = "w1.sav", size = file.size(path),
                                                  type = "", datapath = path))
    session$setInputs(`reshape-file` = "f1", `reshape-direction` = "longer",
                      `reshape-longer-cols_mode` = "include",
                      `reshape-longer-cols` = c("w1d5_1", "w1d5_2", "w1c21_1"))
    session$setInputs(`reshape-run` = 1)
    expect_match(output[["reshape-status"]]$html, "不同的數值標籤")
    expect_match(output[["reshape-code"]]$html, "haven::zap_labels")
  })
})

test_that("A2：TDCYP w1_jsq（有檔案時才跑）", {
  path <- file.path("/Users/jiewentsai/Documents/ClaudePlayMac/lab20261030_kitsui/lab1031",
                    "tdcyp_data", "w1_jsq.sav")
  skip_if_not(file.exists(path))
  df <- haven::read_sav(path)
  cols <- c(paste0("w1d5_", 1:5), paste0("w1c21_", 1:6))
  out <- reshape_longer(df, list(cols = cols, names_to = "item", values_to = "score"))
  expect_false(haven::is.labelled(out$result$score))
  expect_match(out$warnings, "不同的數值標籤", all = FALSE)
  expect_equal(nrow(out$result), nrow(df) * length(cols))
})
