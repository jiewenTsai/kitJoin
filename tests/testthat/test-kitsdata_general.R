# ⑥ 通用性：非 KIT 資料（TDCYP 這類）也能在 app 內轉換

# 數字 ID、1–4 計分、題目欄名有波次前綴
kg_wave <- function(ids, prefix, wave_m, seed) {
  set.seed(seed)
  n <- length(ids)
  out <- tibble::tibble(id = ids, wave_m = wave_m)
  for (k in 1:3) out[[paste0(prefix, "q", k)]] <- sample(0:3, n, TRUE)
  out
}

# ── A5：數字 ID 不寫成科學記號 ───────────────────────────────────────────────

test_that("A5：數字的 ID（100000、1e15）、波次標籤轉成文字時不是科學記號", {
  expect_equal(kits_id_text(c(100000, 200000, 1200000, 1e15, NA)),
               c("100000", "200000", "1200000", "1000000000000000", NA))
  expect_equal(kits_id_text(c(0.5, 0.50000001, 3)), c("0.5", "0.50000001", "3"))
  expect_equal(kits_id_text(c("0012", NA)), c("0012", NA))
  expect_equal(kits_id_text(haven::labelled(c(1, 2), c(a = 1))), c("1", "2"))

  dfs <- list(kg_wave(c(100000, 200000, 1e15), "", 0, 1))
  dfs[[1]]$wave <- 2000000
  map <- kits_guess_map(names(dfs[[1]]))
  map$subj_id <- "id"
  s <- kits_item_settings(dfs, c("q1", "q2", "q3")) |>
    kits_bulk_domain(c("q1", "q2", "q3"), "lan")
  spec <- kg_spec(s, map, "B85")
  out <- kits_build(dfs, "w1.csv", spec)
  expect_length(out$problems, 0)
  long <- kits_long_of(out)
  expect_setequal(unique(long$subj_id), c("100000", "200000", "1000000000000000"))
  expect_true("B85_100000_2000000" %in% long$visit_id)
  expect_false(any(grepl("e\\+", long$visit_id)))

  got <- kg_run_code(dfs, spec, s)
  expect_match(got$code, "as_id <- function(x)", fixed = TRUE)
  kg_same_long(got$long, long)
})
