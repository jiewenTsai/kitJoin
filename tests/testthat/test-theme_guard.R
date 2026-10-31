# bslib 太舊（沒有 Brite 主題）時，kit_theme() 用中文說清楚怎麼辦（kitJoin 與 kitsUI 共用）
test_that("bslib 比 0.11.0 舊時，錯誤訊息寫出版本和怎麼更新", {
  testthat::local_mocked_bindings(packageVersion = function(...) package_version("0.9.0"),
                                  .package = "utils")
  expect_error(kit_theme(), "bslib 版本太舊（0.9.0）.*install.packages\\(\"bslib\"\\)")
})
