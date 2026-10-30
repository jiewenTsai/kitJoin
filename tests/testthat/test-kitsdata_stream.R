# 分段寫出的 .csv.gz 和舊的做法（整張長表放在記憶體，helper-kitsdata-old.R）一列一列相同

stream_wide <- function(ids, wave, wave_m, seed) {
  set.seed(seed)
  n <- length(ids)
  score <- function(p_na = 0.3) {
    x <- sample(1:5, n, replace = TRUE)
    x[stats::runif(n) < p_na] <- NA
    x
  }
  tibble::tibble(
    release_id = ids, wave = wave, wave_m = wave_m, age_m = wave_m + stats::runif(n),
    sex = sample(c("boy", "girl"), n, replace = TRUE), city = "臺北市",
    lana01 = score(), lana02 = score(), bodyaf01 = score(), bodyef01 = score(0.8),
    cogd01r = score(), coga01 = score(1)
  )
}

stream_dfs <- function() {
  a <- dplyr::bind_rows(
    stream_wide(sprintf("K%03d", 1:23), "m03g01", 3, 1),
    stream_wide(sprintf("K%03d", 1:19), "m06g02", 6, 2)
  )
  a$lana01[c(2, 9)] <- 9999
  b <- stream_wide(sprintf("J%03d", 1:17), "m36g01", 36, 3)
  b$bodyaf01[4] <- 7777
  c <- stream_wide(sprintf("L%03d", 1:11), "m60g02", 60, 4)
  c$co <- "M60"
  c$lana02 <- NULL   # 這個檔案沒有 lana02
  list(a, b, c)
}

stream_spec <- function(dfs, steps = list(), map_drop = character(0)) {
  table <- tibble::tibble(
    item_id = c("lana01", "lana02", "bodyaf01", "cogd01", "coga01"),
    domain = c("lan", "lan", "motor", "cog", "cog")
  )
  map <- kits_guess_map(unique(unlist(purrr::map(dfs, names))))
  map[map_drop] <- ""
  cand <- kits_candidate_cols(dfs, c(kits_mapped_cols(map), "co"))
  rules <- kits_parse_rules("^body = motor")$rules
  dom <- kits_item_domains(cand$numeric, table, rules)
  cohorts <- list(list(type = "text", value = "M03"), list(type = "text", value = "M36"),
                  list(type = "column", value = "co"))
  # 沒對應的欄（例如 age_m）不是題目：排除
  kits_spec(cohorts, map, dom[!is.na(dom$domain), c("column", "domain")],
            setdiff(dom$column[is.na(dom$domain)], map_drop), steps)
}

# 舊做法的結果寫成 .csv.gz（和舊的 write_kits_long_gz() 相同：write_csv 到 gzfile）
read_gz_lines <- function(path) {
  con <- gzfile(path, open = "rb")
  on.exit(close(con))
  readLines(con, encoding = "UTF-8")
}

old_lines <- function(long) {
  path <- withr::local_tempfile(fileext = ".csv.gz")
  con <- gzfile(path, open = "wb")
  readr::write_csv(long, con, na = "")
  close(con)
  read_gz_lines(path)
}

new_lines <- function(dfs, spec, chunk_rows) {
  path <- withr::local_tempfile(fileext = ".csv.gz")
  write_kits_long_gz(dfs, spec, path, chunk_rows = chunk_rows)
  read_gz_lines(path)
}

all_steps <- list(
  na = c(7777, 9999),
  shift = c("lana01", "lana02", "bodyaf01", "bodyef01", "cogd01r", "coga01"),
  reverse = list(items = c("lana02", "coga01"), max = 4, add_r = TRUE)
)

test_that("分段寫出和整張長表一列一列相同：多個檔案、r 題、步驟 1–3、各種分段大小", {
  dfs <- stream_dfs()
  spec <- stream_spec(dfs, all_steps)
  old <- kits_build_old(dfs, c("a", "b", "c"), spec)
  expect_length(old$problems, 0)
  want <- old_lines(old$long)
  expect_gt(length(want), 100)
  for (chunk in c(1, 2, 7, 5000)) {
    expect_identical(new_lines(dfs, spec, chunk), want)
  }
  # 加了 r 的題、原本就是 r 的題都在
  expect_true(any(grepl(",lana02r,", want)))
  expect_true(any(grepl(",cogd01r,", want)))
})

test_that("分段寫出相同：沒對應的選填欄（sex、age_m）、沒有波次標籤（用 wave_m）", {
  dfs <- stream_dfs()
  spec <- stream_spec(dfs, list(na = c(7777, 9999)), map_drop = c("sex", "age_m", "wave"))
  old <- kits_build_old(dfs, c("a", "b", "c"), spec)
  expect_length(old$problems, 0)
  want <- old_lines(old$long)
  expect_identical(new_lines(dfs, spec, 4), want)
  expect_match(want[2], "^M03_K001_3,K001,M03,,3,,")
})

test_that("摘要、列數、預覽、檔案大小估計和舊做法相同", {
  dfs <- stream_dfs()
  spec <- stream_spec(dfs, all_steps)
  old <- kits_build_old(dfs, c("a", "b", "c"), spec)
  new <- kits_build(dfs, c("a", "b", "c"), spec)
  expect_equal(new$summary, old$summary)
  expect_equal(new$n_rows, nrow(old$long))
  expect_equal(new$n_visits, dplyr::n_distinct(old$long$visit_id))
  expect_equal(new$n_items, dplyr::n_distinct(old$long$item_id))
  expect_equal(new$preview, utils::head(old$long, nrow(new$preview)))
  expect_equal(new$csv_mb, kits_csv_mb_old(old$long))
  expect_equal(new$notes, old$notes)
})

test_that("在寬資料上檢查：擋下的情況和舊做法相同", {
  dfs <- stream_dfs()
  dfs[[1]] <- dplyr::bind_rows(dfs[[1]], dfs[[1]][3, ])   # 重複的孩子
  spec <- stream_spec(dfs, list(na = c(7777, 9999)))
  old <- kits_build_old(dfs, c("a", "b", "c"), spec)
  new <- kits_build(dfs, c("a", "b", "c"), spec)
  expect_null(new$prep)
  expect_match(old$problems, "同一列次同一題有好幾列", all = FALSE)
  expect_match(new$problems, "同一列次同一題有好幾列", all = FALSE)
  expect_match(new$problems, "K003", all = FALSE)
  path <- withr::local_tempfile(fileext = ".csv.gz")
  expect_no_error(write_kits_long_gz(dfs, spec, path))   # 寫檔本身不重做重複的檢查
})

test_that("在寬資料上的提醒：沒有 0 分、疑似遺漏值代碼、沒有作答的題", {
  dfs <- stream_dfs()
  new <- kits_build(dfs, c("a", "b", "c"), stream_spec(dfs))
  w <- paste(new$warnings, collapse = "\n")
  expect_match(w, "沒有 0 分")
  expect_match(w, "7777 以上")
  expect_match(w, "沒有任何作答")
  expect_match(w, "coga01")
})

test_that("沒有任何作答時，仍寫出欄名", {
  dfs <- stream_dfs()
  dfs <- purrr::map(dfs, function(df) {
    df[c("lana01", "bodyaf01", "bodyef01", "cogd01r", "coga01")] <- NA
    if ("lana02" %in% names(df)) df$lana02 <- NA
    df
  })
  spec <- stream_spec(dfs)
  path <- withr::local_tempfile(fileext = ".csv.gz")
  write_kits_long_gz(dfs, spec, path)
  expect_equal(read_gz_lines(path), paste(KITS_COLUMNS$column, collapse = ","))
})
