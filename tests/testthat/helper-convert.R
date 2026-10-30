# ⑧ 與 .csv.gz 測試共用的小工具

# 寫一個 BIG5（CP950）編碼的文字檔；gz = TRUE 時順便壓縮
write_big5 <- function(path, lines, gz = FALSE) {
  con <- if (gz) gzfile(path, "w", encoding = "CP950") else file(path, "w", encoding = "CP950")
  writeLines(lines, con)
  close(con)
  path
}

# 有變項標籤、數值標籤與 SPSS 自訂遺漏值（9 = 拒答）的 SAV
write_labelled_sav <- function(path, compress = "byte") {
  df <- tibble::tibble(
    id = c("0012", "0013", "0014", "0015"),
    sex = haven::labelled_spss(
      c(1, 2, 9, NA),
      c("男" = 1, "女" = 2, "拒答" = 9),
      na_values = 9,
      label = "性別"
    ),
    score = c(10, 20, 30, 40)
  )
  haven::write_sav(df, path, compress = compress)
  path
}

# shiny::fileInput() 的值
upload_files <- function(paths) {
  data.frame(
    name = basename(paths),
    size = file.size(paths),
    type = "",
    datapath = paths,
    stringsAsFactors = FALSE
  )
}
