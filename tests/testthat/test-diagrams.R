# 分頁說明框下面的小圖解（kit_diagram()、kit_dg_table()）

test_that("小表格：<caption>、表頭 scope、NA 與空字串顯示成灰色的「（空）」", {
  t <- as.character(kit_dg_table(
    data.frame(id = c(1, 2), score = c(5, NA), note = c("", "a")), "第 1 波（t1）"
  ))
  expect_match(t, '^<table class="kit-dg-table">')
  expect_match(t, "<caption>第 1 波（t1）</caption>")
  expect_equal(lengths(regmatches(t, gregexpr('<th scope="col"', t))), 3L)
  expect_equal(lengths(regmatches(t, gregexpr('<span class="kit-dg-empty">（空）</span>', t))), 2L)
  expect_match(t, "<td>5</td>")
  expect_false(grepl("NA", t))
})

test_that("小表格：新加的欄淡底，而且欄名寫「（新）」（不只靠顏色）；第 1 欄可以是列標題", {
  t <- as.character(kit_dg_table(data.frame(wave = "t1", id = 1), "結果", added = "wave"))
  expect_match(t, '<th scope="col" class="kit-dg-added">\\s*wave\\s*<span class="kit-dg-new">（新）</span>')
  expect_match(t, '<td class="kit-dg-added">t1</td>')
  expect_false(grepl('class="kit-dg-added">1<', t))
  r <- as.character(kit_dg_table(data.frame(a = "列", b = "值"), "比較", row_header = TRUE))
  expect_match(r, '<th scope="row">列</th>')
})

test_that("圖解：前面幾張小表 → 結果，一個箭頭（螢幕閱讀器略過），下面一行說明", {
  t1 <- kit_dg_table(data.frame(id = 1, score = 5), "t1")
  t2 <- kit_dg_table(data.frame(id = 1, score = 6), "t2")
  res <- kit_dg_table(data.frame(id = 1, score_t1 = 5), "結果")
  d <- as.character(kit_diagram(list(t1, t2), res, caption = "依 id 左右接起來"))
  expect_match(d, '^<figure class="kit-diagram">')
  expect_match(d, '<div class="kit-dg-arrow" aria-hidden="true">→</div>')
  expect_equal(lengths(regmatches(d, gregexpr("→", d))), 1L)
  expect_match(d, '<figcaption class="kit-dg-caption">依 id 左右接起來</figcaption>')
  before <- sub('<div class="kit-dg-arrow".*$', "", d)
  after <- sub('^.*<div class="kit-dg-after">', "", d)
  expect_equal(lengths(regmatches(before, gregexpr("<table", before))), 2L)
  expect_equal(lengths(regmatches(after, gregexpr("<table", after))), 1L)
  # 只給一張表也可以
  expect_match(as.character(kit_diagram(t1, res, "x")), '<div class="kit-dg-before">\\s*<table')
})

test_that("分頁說明框：有圖解時圖解在 <details> 外面（收起時也看得到），沒有時和原本一樣", {
  d <- kit_diagram(kit_dg_table(data.frame(id = 1, q = 99), "前"),
                   kit_dg_table(data.frame(id = 1, q = NA), "後"), "99 → 空白")
  g <- as.character(kit_tab_guide("目的", "步驟一", "結果", diagram = d))
  expect_match(g, '^<div class="kit-guide kit-guide-wrap">\\s*<details class="kit-guide-page">')
  expect_match(g, '</details>\\s*<figure class="kit-diagram">')
  expect_match(g, "<summary>\\s*<strong>這一頁做什麼：</strong>\\s*目的")
  expect_match(as.character(kit_tab_guide("目的", "一", "結果")), '^<details class="kit-guide kit-guide-page">')
})

test_that("五個分頁的說明框都有圖解，而且各自畫出該做的事", {
  pages <- list(
    missing = mod_missing_ui("missing"),
    join = mod_join_ui("join"),
    stack = mod_stack_ui("stack"),
    attrition = mod_attrition_ui("attrition"),
    reshape = mod_reshape_ui("reshape")
  )
  html <- lapply(pages, function(p) as.character(p))
  guide_of <- function(h) {
    sub("</figure>.*$", "", sub('^.*?<div class="kit-guide kit-guide-wrap">', "", h, perl = TRUE))
  }
  for (nm in names(html)) {
    h <- html[[nm]]
    expect_match(h, 'class="kit-guide kit-guide-wrap"', info = nm)
    g <- guide_of(h)
    expect_match(g, '<details class="kit-guide-page">', info = nm)
    expect_match(g, '<figcaption class="kit-dg-caption">', info = nm)
    expect_match(g, "kit-dg-empty|kit-dg-added", info = nm)
    expect_true(all(grepl("<caption>", strsplit(g, "<table")[[1]][-1])), info = nm)
  }
  # 串接：預設（向後追蹤）的結果，2 號的 t2 是空的；下面另有三種方式的小比較
  expect_match(guide_of(html$join), "score_t1.*score_t2")
  expect_match(guide_of(html$join), "kit-dg-empty")
  expect_match(html$join, 'class="kit-dg-compare"')
  expect_match(html$join, "全勤樣本.*全樣本")
  # 疊加：新的 wave 欄
  expect_match(guide_of(html$stack), 'kit-dg-added">\\s*wave')
  # 流失樣本：attrition 欄寫留存／流失
  expect_match(guide_of(html$attrition), 'kit-dg-added">\\s*attrition')
  # 長寬轉換：寬格式在前（教育、心理的資料多半是寬的），雙箭頭表示兩個方向都能轉
  expect_match(guide_of(html$reshape), "寬格式.*⇄.*長格式")
  expect_match(guide_of(html$attrition), "0 留存.*1 流失")
  # 長／寬：長格式 → 寬格式
  expect_match(guide_of(html$reshape), "長格式.*寬格式")
  # 遺漏值：99、9999 → （空）
  expect_match(guide_of(html$missing), "9999.*（空）")
})
