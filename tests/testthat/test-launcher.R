# run.bat 的啟動腳本（scripts/run_app.R）：照 DESCRIPTION 決定要裝或更新哪些套件
# （kitJoin 與 kitsUI 共用同一份測試）

launcher <- function() {
  path <- testthat::test_path("..", "..", "scripts", "run_app.R")
  testthat::skip_if_not(file.exists(path), "scripts/ 不在打包好的套件裡")
  env <- new.env()
  withr::with_envvar(c(KIT_LAUNCHER_TEST = "1"), sys.source(path, envir = env))
  env
}

fake_desc <- function() {
  f <- withr::local_tempfile(fileext = ".dcf", .local_envir = parent.frame())
  writeLines(c(
    "Package: kitTest",
    "Depends: R (>= 4.1)",
    "Imports: bslib (>= 0.11.0), dplyr (>= 1.1.0), haven, stats, kits (>= 0.25.2)",
    "Remotes: jiewenTsai/kits"
  ), f)
  f
}

test_that("讀 DESCRIPTION：套件、最低版本、GitHub 的位置；R 和內建套件不算", {
  env <- launcher()
  req <- env$read_requirements(fake_desc())
  expect_identical(req$pkg, c("bslib", "dplyr", "haven", "kits"))
  expect_identical(req$min, c("0.11.0", "1.1.0", NA, "0.25.2"))
  expect_identical(req$github, c(NA, NA, NA, "jiewenTsai/kits"))
})

test_that("沒裝的、版本太舊的要裝；夠新的不動（舊的 bslib 沒有 Brite 主題）", {
  env <- launcher()
  req <- env$read_requirements(fake_desc())
  versions <- c(bslib = "0.9.0", dplyr = "1.1.4", haven = NA, kits = "0.25.2")
  todo <- env$needs_install(req, version_of = function(p) unname(versions[[p]]))
  expect_identical(todo$pkg, c("bslib", "haven"))
  expect_match(todo$why[1], "版本太舊（0.9.0，需要 0.11.0 以上）")
  expect_identical(todo$why[2], "沒有安裝")
})

test_that("這個 app 自己的 DESCRIPTION 讀得出 bslib (>= 0.11.0)", {
  env <- launcher()
  desc <- testthat::test_path("..", "..", "DESCRIPTION")
  req <- env$read_requirements(desc)
  expect_identical(req$min[req$pkg == "bslib"], "0.11.0")
  expect_false(any(req$pkg %in% c("R", "stats", "utils", "tools")))
})
