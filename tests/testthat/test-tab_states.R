tab_state_of <- function(states) {
  stats::setNames(purrr::map_chr(states, "state"), purrr::map_chr(states, "value"))
}
tab_tip_of <- function(states) {
  stats::setNames(purrr::map(states, "tip"), purrr::map_chr(states, "value"))
}

test_that("步驟分頁：還不能用的變灰並說明要先做什麼", {
  s <- kit_tab_states(0)
  expect_equal(purrr::map_chr(s, "value"), names(KIT_TAB_NEED))
  st <- tab_state_of(s)
  expect_equal(
    unname(st[c("upload", "missing", "join", "stack", "attrition", "kitsdata", "reshape")]),
    c("ready", rep("locked", 6))
  )
  # ⑧ 有自己的上傳，永遠可以用
  expect_equal(st[["convert"]], "ready")
  tips <- tab_tip_of(s)
  expect_equal(tips$missing, "還不能用：先在「上傳檔案」加入檔案")
  expect_equal(tips$join, "還不能用：先在「上傳檔案」加入至少 2 個檔案")
  expect_null(tips$convert)

  one <- kit_tab_states(1, list(upload = "success"))
  st <- tab_state_of(one)
  expect_equal(st[["upload"]], "done")
  expect_equal(unname(st[c("missing", "kitsdata", "reshape")]), rep("ready", 3))
  expect_equal(unname(st[c("join", "stack", "attrition")]), rep("locked", 3))
  expect_match(tab_tip_of(one)$join, "要 2 個以上的檔案")
})

test_that("步驟分頁：完成（含要注意）打 ✓；要重新執行時 ✓ 拿掉；停下來不打 ✓", {
  s <- kit_tab_states(2, list(
    upload = "success", missing = "stale", join = "success", stack = "warning",
    attrition = "error", convert = "success"
  ))
  st <- tab_state_of(s)
  expect_equal(st[["join"]], "done")
  expect_equal(st[["stack"]], "done")
  expect_equal(st[["missing"]], "stale")
  expect_equal(st[["attrition"]], "ready")
  expect_equal(st[["convert"]], "done")
  tips <- tab_tip_of(s)
  expect_equal(tips$join, "已完成")
  expect_match(tips$stack, "要注意")
  expect_match(tips$missing, "要重新執行")
  # 檔案少於需要的數量時，舊的狀態不算：變灰
  expect_equal(tab_state_of(kit_tab_states(1, list(join = "success")))[["join"]], "locked")
})

test_that("步驟分頁的狀態跟著 app 走：上傳、執行、改設定、移除檔案", {
  dir <- withr::local_tempdir()
  w1 <- file.path(dir, "w1.csv")
  w2 <- file.path(dir, "w2.csv")
  readr::write_csv(data.frame(release_id = 1:3, score = c(1, 9999, 3)), w1)
  readr::write_csv(data.frame(release_id = 2:4, score = 4:6), w2)
  up <- function(paths) {
    data.frame(name = basename(paths), size = file.size(paths), type = "",
               datapath = paths, stringsAsFactors = FALSE)
  }
  shiny::testServer(kit_server, {
    st <- tab_state_of(tab_states())
    expect_equal(st[["upload"]], "ready")
    expect_equal(st[["join"]], "locked")

    session$setInputs(`upload-files` = up(w1))
    st <- tab_state_of(tab_states())
    expect_equal(st[["upload"]], "done")
    expect_equal(st[["missing"]], "ready")
    expect_equal(st[["join"]], "locked")

    session$setInputs(`upload-files` = up(w2))
    session$setInputs(
      `join-by_vars` = "release_id", `join-method` = "inner_join", `join-keys_as_text` = TRUE
    )
    expect_equal(tab_state_of(tab_states())[["join"]], "ready")
    session$setInputs(`join-run` = 1)
    expect_equal(tab_state_of(tab_states())[["join"]], "done")
    session$setInputs(`join-method` = "full_join")
    expect_equal(tab_state_of(tab_states())[["join"]], "stale")
    expect_match(output[["join-status"]]$html, "要重新執行")

    session$setInputs(`missing-codes` = "9999", `missing-match_text` = TRUE,
                      `missing-use_downstream` = TRUE, `missing-exclude_cols` = "release_id",
                      `missing-run` = 1)
    expect_equal(tab_state_of(tab_states())[["missing"]], "done")
    session$setInputs(`missing-codes` = "9999, 8888")
    expect_equal(tab_state_of(tab_states())[["missing"]], "stale")

    session$setInputs(`upload-remove_f2` = 1)
    expect_equal(tab_state_of(tab_states())[["join"]], "locked")
  })
})

test_that("頁面帶著分頁狀態的 JavaScript；每一頁只有一個主要按鈕", {
  ui <- as.character(kit_ui(NULL))
  expect_match(ui, "kit-tab-states", fixed = TRUE)
  expect_match(ui, "kit-open-details", fixed = TRUE)
  pages <- list(
    upload = mod_upload_ui("upload"), missing = mod_missing_ui("missing"),
    join = mod_join_ui("join"), stack = mod_stack_ui("stack"),
    attrition = mod_attrition_ui("attrition"), reshape = mod_reshape_ui("reshape"),
    convert = mod_convert_ui("convert")
  )
  for (tab in names(pages)) {
    html <- as.character(pages[[tab]])
    n_primary <- lengths(regmatches(html, gregexpr("btn-primary", html, fixed = TRUE)))
    # ① 的主要動作是「選擇檔案…」（kit-file-primary），其他頁是一個 btn-primary
    expect_equal(n_primary, if (tab == "upload") 0L else 1L, info = tab)
    if (tab != "convert") expect_match(html, "<summary>用 R 怎麼做</summary>", info = tab)
    expect_no_match(html, "kit-callout-warning|accordion", info = tab)
  }
})

test_that("分頁列：三組、不編號，分頁的 value 不變；Shiny 自己的分頁列藏起來", {
  bar <- as.character(kit_navgroups("tabs", selected = "upload"))
  expect_match(bar, 'data-tabset="tabs"', fixed = TRUE)
  for (label in c("準備", "做出資料（選一個）", "其他工具")) expect_match(bar, label, fixed = TRUE)
  values <- regmatches(bar, gregexpr('data-kit-tab="[a-z]+"', bar))[[1]]
  expect_equal(
    gsub('data-kit-tab="|"', "", values),
    c("upload", "missing", "join", "stack", "attrition", "kitsdata", "reshape", "convert")
  )
  expect_no_match(bar, "[①②③④⑤⑥⑦⑧]")
  expect_match(bar, 'class="kit-navgroup tools"', fixed = TRUE)
  expect_match(bar, '<a class="nav-link active" href="#" data-kit-tab="upload" aria-current="page">',
               fixed = TRUE)
  ui <- as.character(kit_ui(NULL))
  expect_match(ui, 'class="nav nav-hidden shiny-tab-input" id="tabs"', fixed = TRUE)
  expect_match(ui, 'class="kit-navgroups" data-tabset="tabs"', fixed = TRUE)
})

test_that("改了分頁列以後，頁首仍有版本與日期，雲端版頁尾仍有使用須知", {
  withr::local_envvar(KIT_DEPLOY = "cloud")
  ui <- as.character(kit_ui(NULL))
  expect_match(ui, "kit-app-meta\">v[0-9.]+ · [0-9]{4}-[0-9]{2}-[0-9]{2}<")
  expect_match(ui, "雲端版使用須知", fixed = TRUE)
  # 頁首在分頁列前面、頁尾在分頁內容後面
  expect_lt(regexpr("kit-header", ui), regexpr("kit-navgroups", ui))
  expect_gt(regexpr('id="kit-cloud-terms"', ui, fixed = TRUE), regexpr("nav-hidden", ui))
})
