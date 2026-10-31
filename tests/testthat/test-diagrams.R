# 各分頁說明框下面的小圖解（kitDesign 的 kit_diagram()、kit_dg_table() 本身的測試在 kitDesign）

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
