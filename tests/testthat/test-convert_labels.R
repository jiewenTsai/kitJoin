test_that("SAV → CSV 一起輸出變項檔，CSV ＋ 變項檔 → SAV 標籤與自訂遺漏值都回來", {
  dir <- withr::local_tempdir()
  sav <- write_labelled_sav(file.path(dir, "w1.sav"))
  out1 <- file.path(dir, "o1"); dir.create(out1)
  res <- convert_file(sav, "w1.sav", "csv", out1)
  expect_equal(res$dict_name, "w1_variables.csv")
  expect_true(file.exists(res$dict_path))
  expect_match(paste(res$notes, collapse = ""), "w1_variables.csv")

  dict <- readr::read_csv(res$dict_path, col_types = readr::cols(.default = "c"))
  expect_true(all(c("variable", "label", "value_labels", "na_values", "na_range") %in% names(dict)))
  sex <- dict[dict$variable == "sex", ]
  expect_equal(sex$label, "性別")
  expect_equal(sex$value_labels, "[1] 男; [2] 女; [9] 拒答")
  expect_equal(sex$na_values, "9")

  # 和 R 裡用 labelled 做的一模一樣
  ref <- labelled::convert_list_columns_to_character(
    labelled::generate_dictionary(haven::read_sav(sav, user_na = TRUE), details = "full")
  )
  expect_equal(dict$value_labels, unname(ifelse(ref$value_labels == "", NA, ref$value_labels)))

  data <- readr::read_csv(res$out_path, col_types = "cdd")
  expect_equal(data$sex, c(1, 2, NA, NA))

  # 轉回 SAV：自訂遺漏值已變成空白，但標籤與 na_values 設定都回來
  out2 <- file.path(dir, "o2"); dir.create(out2)
  back <- convert_file(res$out_path, "w1.csv", "sav", out2,
                       dict = list(path = res$dict_path, name = "w1_variables.csv"))
  expect_match(back$notes[1], "3 個欄位")
  s <- haven::read_sav(back$out_path, user_na = TRUE)
  expect_equal(attr(s$sex, "label"), "性別")
  expect_equal(unclass(attr(s$sex, "labels")), c("男" = 1, "女" = 2, "拒答" = 9))
  expect_equal(attr(s$sex, "na_values"), 9)
  expect_equal(as.vector(s$id), c("0012", "0013", "0014", "0015"))
})

test_that("沒附變項檔：CSV → SAV 照舊、沒有標籤", {
  dir <- withr::local_tempdir()
  csv <- file.path(dir, "w1.csv")
  readr::write_csv(data.frame(id = 1:2, sex = c(1, 2)), csv)
  res <- convert_file(csv, "w1.csv", "sav", dir)
  expect_null(res$dict_path)
  s <- haven::read_sav(res$out_path)
  expect_null(attr(s$sex, "labels"))
})

test_that("變項檔也接受 kitJoin 的 codebook 格式；對不上的欄位寫進說明", {
  dir <- withr::local_tempdir()
  csv <- file.path(dir, "w1.csv")
  readr::write_csv(data.frame(id = c("a", "b"), sex = c(1, 99), grp = c("x", "y"), extra = 1:2), csv)
  cb <- file.path(dir, "w1_codebook.csv")
  readr::write_excel_csv(tibble::tibble(
    "欄位" = c("sex", "grp", "gone"),
    "變項標籤" = c("性別", "組別", "不見了"),
    "數值標籤" = c("1 = 男；2 = 女；99 = 拒答", "x = 甲；y = 乙", ""),
    "使用者自訂遺漏值" = c("99、90～98", "", "")
  ), cb)
  res <- convert_file(csv, "w1.csv", "sav", dir, dict = list(path = cb, name = "w1_codebook.csv"))
  notes <- paste(res$notes, collapse = "\n")
  expect_match(notes, "2 個欄位")
  expect_match(notes, "gone")
  expect_match(notes, "id、extra")
  s <- haven::read_sav(res$out_path, user_na = TRUE)
  expect_equal(unclass(attr(s$sex, "labels")), c("男" = 1, "女" = 2, "拒答" = 99))
  expect_equal(attr(s$sex, "na_values"), 99)
  expect_equal(attr(s$sex, "na_range"), c(90, 98))
  expect_equal(unclass(attr(s$grp, "labels")), c("甲" = "x", "乙" = "y"))
  expect_equal(attr(s$grp, "label"), "組別")
})

test_that("文字的數值標籤配數字欄：不貼、寫進說明", {
  vars <- list(sex = list(label = "性別", values = c("m", "f"), labels = c("男", "女"),
                          na_values = character(0), na_range = character(0)))
  out <- apply_variables(tibble::tibble(sex = c(1, 2)), vars, "v.csv")
  expect_null(attr(out$data$sex, "labels"))
  expect_equal(attr(out$data$sex, "label"), "性別")
  expect_match(paste(out$notes, collapse = ""), "對不上")
})

test_that("變項檔格式不對：白話錯誤", {
  dir <- withr::local_tempdir()
  f <- file.path(dir, "x.csv")
  readr::write_csv(data.frame(a = 1), f)
  expect_error(read_variables_file(f, "x.csv"), "variable")
})

test_that("「用 R 怎麼做」跑出來和 app 一樣", {
  dir <- withr::local_tempdir()
  sav <- write_labelled_sav(file.path(dir, "w1.sav"))
  app <- file.path(dir, "app"); dir.create(app)
  res <- convert_file(sav, "w1.sav", "csv", app)
  back <- convert_file(res$out_path, "w1.csv", "sav", app,
                       dict = list(path = res$dict_path, name = "w1_variables.csv"))

  run <- file.path(dir, "run"); dir.create(run)
  file.copy(sav, run)
  withr::with_dir(run, {
    eval(parse(text = code_convert("w1.sav", "csv")), envir = new.env())
    expect_equal(readLines("w1_variables.csv"), readLines(res$dict_path))
    expect_equal(readLines("w1.csv"), readLines(res$out_path))
    eval(parse(text = code_convert("w1.csv", "sav", dict_name = "w1_variables.csv")), envir = new.env())
    a <- haven::read_sav("w1.sav", user_na = TRUE)
  })
  b <- haven::read_sav(back$out_path, user_na = TRUE)
  expect_equal(attributes(a$sex)[c("label", "labels", "na_values")], attributes(b$sex)[c("label", "labels", "na_values")])
  expect_null(code_convert("w1.sav", "sav"))
  expect_match(code_convert("w1.csv", "sav"), "沒有變項檔")
})

test_that("⑧ 畫面：SAV 有兩個下載，CSV → SAV 才出現變項檔上傳", {
  dir <- withr::local_tempdir()
  sav <- write_labelled_sav(file.path(dir, "w1.sav"))
  shiny::testServer(mod_convert_server, {
    session$setInputs(file = upload_files(sav), target = "csv", run = 1)
    expect_null(output$dict_ui$html)
    expect_match(output$result$html, "下載 w1_variables.csv")
    dict_path <- kit_test_download_file(session, "download_dict")
    expect_match(paste(readLines(dict_path), collapse = ""), "性別")
    expect_match(output$code$html, "generate_dictionary")
  })

  csv <- file.path(dir, "w1.csv")
  readr::write_csv(data.frame(id = c("a", "b"), sex = c(1, 2)), csv)
  vars <- file.path(dir, "w1_variables.csv")
  readr::write_excel_csv(tibble::tibble(variable = "sex", label = "性別", value_labels = "[1] 男; [2] 女"), vars)
  shiny::testServer(mod_convert_server, {
    session$setInputs(file = upload_files(csv), target = "csv.gz")
    expect_null(output$dict_ui$html)
    session$setInputs(target = "sav")
    expect_match(output$dict_ui$html, "變項檔")
    session$setInputs(dict = upload_files(vars), run = 1)
    expect_match(output$status$html, "1 個欄位")
    s <- haven::read_sav(kit_test_download_file(session, "download_file"))
    expect_equal(attr(s$sex, "label"), "性別")
    expect_match(output$code$html, "labelled_spss")
  })
})
