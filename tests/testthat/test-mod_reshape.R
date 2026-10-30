# ⑦ 長寬互轉模組：用假的 files / working 直接驅動

fake_files <- function(dfs) {
  n <- length(dfs)
  list(
    keys = shiny::reactive(sprintf("f%d", seq_len(n))),
    names = shiny::reactive(sprintf("file%d.csv", seq_len(n))),
    labels = shiny::reactive(sprintf("t%d", seq_len(n))),
    data = shiny::reactive(dfs),
    encodings = shiny::reactive(rep("UTF-8", n))
  )
}

reshape_args <- function(dfs) {
  files <- fake_files(dfs)
  working <- list(
    data = files$data,
    source = shiny::reactive("raw"),
    code_cells = shiny::reactive(0L)
  )
  list(files = files, working = working, goto = function(tab) NULL)
}

# 套用 kitsUI 分數表預設後的控制項值（按鈕只填控制項；testServer 不會回填 input）
set_preset_inputs <- function(session) {
  session$setInputs(
    file = "f1", direction = "wider", id_auto = FALSE,
    id_cols = c("visit_id", "subj_id", "cohort", "wave_m", "age_m", "sex", "round"),
    names_from = "domain", values_from = c("d_score", "d_score_se", "scale"),
    name_mode = "glue", names_glue = "{.value}_{domain}", names_vary = "slowest",
    names_sep = "_", names_prefix = "", values_fill = ""
  )
}

test_that("kitsUI 分數表：顯示預設按鈕，轉換後可下載並看到空白格來源", {
  shiny::testServer(
    mod_reshape_server,
    args = reshape_args(list(sim_kitsui_scores())),
    {
      session$setInputs(file = "f1")
      expect_match(output$preset$html, "套用 kitsUI 分數表預設")
      expect_match(output$downloads$html, "disabled")

      set_preset_inputs(session)
      session$setInputs(run = 1)
      expect_match(output$status$html, "轉換完成")
      expect_equal(dim(result()$result), c(6, 13))
      expect_match(output$downloads$html, "download_csv")
      expect_match(output$preview_meta, "11 列 × 17 欄.*6 列 × 13 欄")
      expect_match(output$result$html, "空白格從哪裡來")
      expect_match(output$empty_table, "motor")
      expect_match(output$code$html, 'names_vary = "slowest"')
      expect_no_match(output$code$html, "base R 版")
    }
  )
})

test_that("一般資料不顯示 kitsUI 預設按鈕；id_cols 預設為其餘全部欄位", {
  df <- tibble::tibble(id = c(1, 1, 2), k = c("a", "b", "a"), x = 1:3, y = 4:6)
  shiny::testServer(mod_reshape_server, args = reshape_args(list(df)), {
    session$setInputs(
      file = "f1", direction = "wider", id_auto = TRUE,
      names_from = "k", values_from = c("x", "y"), name_mode = "sep",
      names_sep = "_", names_prefix = "", names_vary = "fastest", values_fill = ""
    )
    expect_null(output$preset)
    expect_match(output$id_note$html, "id 欄（1 欄）：id")
    session$setInputs(run = 1)
    expect_named(result()$result, c("id", "x_a", "x_b", "y_a", "y_b"))
    expect_no_match(output$code$html, "id_cols =")
  })
})

test_that("id × names_from 重複時停下並列出重複列", {
  scores <- sim_kitsui_scores()
  doubled <- dplyr::bind_rows(scores, scores[1, ])
  shiny::testServer(mod_reshape_server, args = reshape_args(list(doubled)), {
    set_preset_inputs(session)
    session$setInputs(run = 1)
    expect_match(output$status$html, "沒有完成")
    expect_null(result())
    expect_match(output$blocked$html, "重複的列（共 2 列")
    expect_match(output$blocked_rows, "M03_001_12")
    expect_match(output$downloads$html, "disabled")

    # 改設定後，重複列的表會清掉
    session$setInputs(values_from = "d_score")
    expect_null(output$blocked)
  })
})

test_that("round 在同一個 visit 內不同：提醒並列出不一致的列", {
  scores <- sim_kitsui_scores()
  scores$round[scores$visit_id == "M03_001_12" & scores$domain == "motor"] <- 2L
  shiny::testServer(mod_reshape_server, args = reshape_args(list(scores)), {
    set_preset_inputs(session)
    session$setInputs(run = 1)
    expect_match(output$status$html, "完成，但請注意")
    expect_match(output$status$html, "round")
    expect_match(output$result$html, "id 欄不一致的列")
    expect_match(output$id_rows, "M03_001_12")
  })
})

test_that("寬轉長（暫時控制項）與換方向後結果失效", {
  wide <- tibble::tibble(id = 1:2, score_t1 = c(1, 2), score_t2 = c(3, 4))
  shiny::testServer(mod_reshape_server, args = reshape_args(list(wide)), {
    session$setInputs(file = "f1", direction = "longer")
    session$setInputs(
      `longer-cols` = c("score_t1", "score_t2"),
      `longer-names_to` = "wave", `longer-values_to` = "score",
      `longer-names_prefix` = "score_", `longer-names_pattern` = "",
      `longer-values_drop_na` = FALSE
    )
    session$setInputs(run = 1)
    expect_named(result()$result, c("id", "wave", "score"))
    expect_equal(result()$result$wave, c("t1", "t2", "t1", "t2"))
    expect_match(output$code$html, 'names_prefix = "score_"')
    expect_match(output$code$html, "pivot_longer")

    session$setInputs(direction = "wider")
    expect_match(output$status$html, "需要重新執行")
    expect_null(result())
  })
})

test_that("選第二個檔案、② 處理後的資料也會被使用", {
  a <- tibble::tibble(id = 1, k = "a", x = 1)
  b <- tibble::tibble(id = c(1, 1), k = c("a", "b"), x = c(9999, 2))
  args <- reshape_args(list(a, b))
  processed <- list(a, dplyr::mutate(b, x = dplyr::na_if(x, 9999)))
  args$working$data <- shiny::reactive(processed)
  args$working$source <- shiny::reactive("na")
  shiny::testServer(mod_reshape_server, args = args, {
    session$setInputs(
      file = "f2", direction = "wider", id_auto = TRUE,
      names_from = "k", values_from = "x", name_mode = "sep",
      names_sep = "_", names_prefix = "", names_vary = "fastest", values_fill = ""
    )
    expect_match(output$source_note$html, "遺漏值轉 NA")
    session$setInputs(run = 1)
    expect_equal(result()$result$a, NA_real_)
    expect_equal(result()$result$b, 2)
    expect_match(output$code$html, "data_t2")
  })
})

test_that("沒有檔案時提示先上傳", {
  shiny::testServer(mod_reshape_server, args = reshape_args(list()), {
    expect_match(output$source_note$html, "還沒有檔案")
    session$setInputs(direction = "wider", run = 1)
    expect_match(output$status$html, "請先到")
  })
})

test_that("整個 app：⑦ 分頁存在，只上傳 1 個檔案也能轉換", {
  expect_match(as.character(kit_ui(NULL)), "⑦ 長寬互轉")
  dir <- withr::local_tempdir()
  path <- file.path(dir, "scores.csv")
  readr::write_csv(sim_kitsui_scores(), path, na = "")
  upload <- data.frame(
    name = "scores.csv", size = file.size(path), type = "", datapath = path
  )
  shiny::testServer(kit_server, {
    session$setInputs(`upload-files` = upload)
    session$setInputs(`reshape-file` = "f1")
    expect_match(output[["reshape-preset"]]$html, "kitsUI")
    session$setInputs(
      `reshape-direction` = "wider", `reshape-id_auto` = FALSE,
      `reshape-id_cols` = c("visit_id", "cohort", "wave_m", "age_m", "sex"),
      `reshape-names_from` = "domain", `reshape-values_from` = c("d_score", "scale"),
      `reshape-name_mode` = "sep", `reshape-names_sep` = "_",
      `reshape-names_prefix` = "", `reshape-names_vary` = "fastest",
      `reshape-values_fill` = ""
    )
    session$setInputs(`reshape-run` = 1)
    expect_match(output[["reshape-status"]]$html, "轉換完成")
    expect_match(output[["reshape-downloads"]]$html, "download_sav")
  })
})

test_that("轉換前提示格式；方向相反時提醒；寬轉長的程式碼用 starts_with()", {
  wide <- tibble::tibble(
    id = 1:2, sex = c(1, 2), score_t1 = c(1, 2), score_t2 = c(3, 4)
  )
  shiny::testServer(mod_reshape_server, args = reshape_args(list(wide)), {
    session$setInputs(file = "f1", direction = "wider")
    expect_match(output$guess$html, "用哪一欄當 ID")
    expect_match(output$guess_text$html, "寬格式")
    expect_match(output$guess_flip$html, "改選「寬轉長」")
    expect_match(output$direction_note$html, "已經是寬格式")

    session$setInputs(direction = "longer")
    expect_null(output$direction_note)
    expect_null(output$guess_flip)
    session$setInputs(
      `longer-cols_mode` = "include", `longer-cols` = c("score_t1", "score_t2")
    )
    expect_match(output$code$html, "starts_with")
    session$setInputs(run = 1)
    expect_equal(dim(result()$result), c(4, 4))

    # 自己選 ID：sex 每個值一列 → 還是寬格式
    session$setInputs(guess_id = "sex")
    expect_match(output$guess_text$html, "每個 sex 一列")
  })

  long <- sim_kitsui_scores()
  shiny::testServer(mod_reshape_server, args = reshape_args(list(long)), {
    session$setInputs(file = "f1", direction = "longer")
    expect_match(output$guess_text$html, "長格式")
    expect_match(output$direction_note$html, "已經是長格式")
    session$setInputs(direction = "wider")
    expect_null(output$direction_note)
  })
})
