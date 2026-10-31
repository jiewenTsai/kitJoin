test_that("停下來時「怎麼辦」連到「檔案上傳」（檔案不夠時）", {
  user <- tryCatch(kit_abort("需要至少 2 個檔案。"), error = function(e) e)
  todo <- kit_upload_todo(identity, 1, 2L)
  html <- as.character(kit_status_ui(kit_error_status(user, "串接", todo)))
  expect_match(html, "需要至少 2 個檔案。")
  expect_no_match(html, "原始訊息")
  expect_match(html, "kit-status-action")
  expect_match(html, "goto_upload_status")
  expect_match(as.character(todo), "「檔案上傳」")
  expect_null(kit_upload_todo(identity, 2, 2L))
})

test_that("本機版不連到雲端的 kitsUI：改成一行「在 R 執行 kitsUI::run_kitsui()」（研究者 4）", {
  local <- as.character(kit_kitsui_link("開啟 kitsUI", cloud = FALSE))
  expect_no_match(local, "href|posit\\.cloud")
  expect_match(local, "kitsUI::run_kitsui()", fixed = TRUE)
  expect_match(local, "作答檔不會離開這台電腦")
  cloud <- as.character(kit_kitsui_link("開啟 kitsUI，上傳這個檔 →", cloud = TRUE, class = "kit-next-link"))
  expect_match(cloud, paste0('href="', KITSUI_URL, '"'), fixed = TRUE)
  expect_match(cloud, 'class="kit-next-link"', fixed = TRUE)
  expect_match(cloud, "雲端版")
  btn <- as.character(kit_kitsui_link("開啟 kitsUI", kind = "secondary", cloud = TRUE))
  expect_match(btn, "btn kit-ext-btn btn-sm btn-outline-secondary", fixed = TRUE)
  expect_match(cloud, 'class="kit-app-link"', fixed = TRUE)

  withr::local_envvar(KIT_DEPLOY = "local")
  expect_no_match(as.character(kit_kitsui_button()), "posit\\.cloud")
  expect_no_match(as.character(kit_ui(NULL)), "kitsui\\.share\\.connect\\.posit\\.cloud")
  withr::local_envvar(KIT_DEPLOY = "cloud")
  expect_match(as.character(kit_kitsui_button()), "kitsui.share.connect.posit.cloud", fixed = TRUE)
})

test_that("主要按鈕還不能按時停用，上面一行 .kit-ready.todo 寫還差什麼（設計審查 22）", {
  # 各頁的主要按鈕一開始是停用的（伺服器算好以後才打開）
  for (page in list(
    list(mod_stack_ui, "stack"), list(mod_join_ui, "join"),
    list(mod_attrition_ui, "attrition"), list(mod_missing_ui, "missing"),
    list(mod_reshape_ui, "reshape")
  )) {
    html <- as.character(page[[1]](page[[2]]))
    expect_match(html, paste0('id="', page[[2]], '-ready"'), info = page[[2]])
    run <- regmatches(html, regexpr(paste0('<button[^>]*id="', page[[2]], '-run"[^>]*>'), html))
    expect_match(run, "disabled", info = page[[2]])
  }

  # 直向波次疊加只有 1 個檔案：「還差」+ 到檔案上傳的連結；不另開黃框
  one <- list(tibble::tibble(id = 1:2, x = 1:2))
  files <- function(dfs) list(
    keys = shiny::reactive(sprintf("f%d", seq_along(dfs))),
    names = shiny::reactive(sprintf("w%d.csv", seq_along(dfs))),
    labels = shiny::reactive(sprintf("t%d", seq_along(dfs))),
    data = shiny::reactive(dfs)
  )
  args <- function(dfs) {
    f <- files(dfs)
    list(files = f, goto = function(tab) NULL, working = list(
      data = f$data, source = shiny::reactive("raw"), code_cells = shiny::reactive(0L)
    ))
  }
  shiny::testServer(mod_stack_server, args = args(one), {
    expect_match(output$ready$html, "至少要 2 個波次的檔案，目前只有 1 個")
    expect_match(output$ready$html, "goto_upload")
    expect_null(output$source_note)
    session$setInputs(run = 1)   # 停用只是畫面上的：伺服器端仍然擋
    expect_match(output$status$html, "停下來")
  })
  two <- list(tibble::tibble(id = 1:2, x = 1:2), tibble::tibble(id = 2:3, x = 3:4))
  shiny::testServer(mod_stack_server, args = args(two), {
    expect_null(output$ready)
  })
  shiny::testServer(mod_join_server, args = args(two), {
    session$setInputs(id_per_file = FALSE, by_vars = character(0))
    expect_match(output$ready$html, "選 ID 欄")
    session$setInputs(by_vars = "id")
    expect_null(output$ready)
  })
  shiny::testServer(mod_attrition_server, args = args(two), {
    session$setInputs(baseline = "f1", followup = "f1", id_vars = "id")
    expect_match(output$ready$html, "不同的檔案")
    session$setInputs(followup = "f2")
    expect_null(output$ready)
  })
})

test_that("結果出來以後把狀態框捲到畫面上方（kitDesign::kit_run_tab_server()；設計審查 17）", {
  # 各頁的主要按鈕都交給 kit_run_tab_server()：執行完通知、kit_scroll_to("status")（kitDesign 測）
  for (f in c("mod_join.R", "mod_stack.R", "mod_attrition.R", "mod_missing.R",
              "mod_reshape.R", "mod_convert.R", "mod_kitsdata.R")) {
    src <- paste(deparse(get(sub("\\.R$", "_server", f))), collapse = "\n")
    expect_match(src, "kit_run_tab_server(", fixed = TRUE, info = f)
  }
})

