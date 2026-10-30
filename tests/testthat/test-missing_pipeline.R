test_that("ID 欄位的猜測不會誤抓 idea_score", {
  expect_equal(
    guess_id_cols(c(
      "id", "release_id", "IDNO", "id_2", "idea_score", "child.id", "valid"
    )),
    c("id", "release_id", "IDNO", "id_2", "child.id")
  )
})

test_that("計算遺漏值代碼格數時略過 ID 欄", {
  df <- tibble::tibble(id = c(9999, 1), x = c(9999, 7777), y = c("9999", "a"))
  expect_equal(count_code_cells(list(df)), 2)
})

test_that("值標籤全是遺漏碼時，轉換後不再是沒有標籤的 labelled，預覽不會出錯（B3）", {
  x <- haven::labelled(c(26, 999, 30), c(遺漏值 = 999), label = "居住地區")
  df <- tibble::tibble(id = 1:3, h_area = x)
  out <- replace_na_codes(df, 999)$data
  expect_false(haven::is.labelled(out$h_area))
  expect_equal(attr(out$h_area, "label"), "居住地區")
  expect_equal(as.vector(out$h_area), c(26, NA, 30))
  expect_no_error(shown <- df_for_display(out))
  expect_equal(shown$h_area, c("26", NA, "30"))
  # 舊資料裡已經是 labels = NULL 的 labelled 也能預覽
  bad <- x
  attr(bad, "labels") <- NULL
  expect_no_error(df_for_display(tibble::tibble(v = bad)))
})

test_that("代碼不在資料裡時，代碼的數值標籤也會移除；其他標籤保留", {
  x <- haven::labelled(c(1, 2), c(是 = 1, 否 = 2, 遺漏值 = 99))
  out <- replace_na_codes(tibble::tibble(x = x), 99)
  expect_equal(attr(out$data$x, "labels"), c(是 = 1, 否 = 2))
  expect_null(out$detail)
})

test_that("ID 辨識：w3id、w4id、w5id、學號、編號、studentID 也算（A6）", {
  cols <- c("w3id", "w4id", "w5id", "學號", "學生編號", "sid", "w1_id", "w5e1_id",
            "studentID", "idea_score", "valid", "paid", "covid19", "lipid_2")
  expect_equal(
    guess_id_cols(cols),
    c("w3id", "w4id", "w5id", "學號", "學生編號", "sid", "w1_id", "w5e1_id", "studentID")
  )
})

kw <- parse_na_keywords(kit_na_label_keywords_default())

test_that("依數值標籤：每一欄只轉換自己標成遺漏的值（A6）", {
  weight <- haven::labelled(
    c(45, 95, 99, 997, 999), c(不知道或不清楚 = 997, 遺漏值 = 999), label = "體重"
  )
  scale <- haven::labelled(c(1, 2, 98, 99, 3), c(很不清楚 = 1, 不清楚 = 2, 清楚 = 3, 跳答 = 98, 遺漏值 = 99))
  plain <- c(95, 99, 1, 2, 3)
  df <- tibble::tibble(w3id = c(95, 96, 97, 98, 99), weight = weight, scale = scale, plain = plain)
  out <- replace_na_by_labels(df, kw, exclude = guess_id_cols(names(df)))
  # 真實的 95、99 公斤保留；997、999 轉換
  expect_equal(as.vector(out$data$weight), c(45, 95, 99, NA, NA))
  # 量表中間的「不清楚」(2) 不是遺漏
  expect_equal(as.vector(out$data$scale), c(1, 2, NA, NA, 3))
  expect_equal(attr(out$data$scale, "labels"), c(很不清楚 = 1, 不清楚 = 2, 清楚 = 3))
  expect_equal(out$data$plain, plain)
  expect_equal(out$data$w3id, df$w3id)
  expect_equal(out$detail$欄位, c("weight", "scale"))
  expect_equal(out$detail$轉換的代碼[1], "997 不知道或不清楚（1 格）、999 遺漏值（1 格）")
  expect_equal(out$detail$合計, c(2L, 2L))

  tbl <- label_na_table(list(df), "a.sav", kw)
  expect_equal(tbl$欄位, c("weight", "scale"))
  expect_equal(tbl$資料中的格數, c(2L, 2L))
  expect_equal(label_na_table(list(tibble::tibble(x = 1)), "a.csv", kw)$欄位, character())
})

test_that("依代碼清單仍可用：KIT 預設代碼不變（A6）", {
  expect_equal(kit_na_codes_default(), "7777, 8888, 9995, 9996, 9999")
  df <- tibble::tibble(id = c(1, 2), x = c(9999, 3))
  expect_equal(replace_na_codes(df, c(9999), "id")$data$x, c(NA, 3))
})

test_that("遺漏值格數提醒也算數值標籤標成遺漏的值（C5）", {
  x <- haven::labelled(c(1, 99, 98), c(遺漏值 = 99, 跳答 = 98))
  df <- tibble::tibble(id = 1:3, x = x)
  expect_equal(count_code_cells(list(df)), 0)
  expect_equal(count_code_cells(list(df), keywords = kw), 2)
})

test_that("TDCYP w1_jsq：依數值標籤不會改到真實體重，997/999 會轉換", {
  path <- "/Users/jiewentsai/Documents/ClaudePlayMac/lab20261030_kitsui/lab1031/tdcyp_data/w1_jsq.sav"
  skip_if_not(file.exists(path))
  df <- read_data_file(path, "w1_jsq.sav")$data
  out <- replace_na_by_labels(df, kw, exclude = guess_id_cols(names(df)))
  before <- as.vector(unclass(df$w1d28_2))
  after <- as.vector(unclass(out$data$w1d28_2))
  real <- before %in% 95:99
  expect_gt(sum(real), 0)
  expect_equal(after[real], before[real])
  expect_false(any(after %in% c(997, 999)))
})
