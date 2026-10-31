test_that("分頁說明框預設收起：只看到「這一頁做什麼」一句，步驟與技術說明在裡面", {
  g <- as.character(kit_tab_guide("目的", "步驟一", "結果", technical = "原理", note = "補充"))
  expect_match(g, '^<details class="kit-guide kit-guide-page">')
  expect_false(grepl("<details[^>]*open", g))
  expect_match(g, "<summary>\\s*<strong>這一頁做什麼：</strong>\\s*目的")
  body <- sub("^.*kit-guide-body", "", g)
  expect_match(body, "步驟一")
  expect_match(body, "kit-guide-note")
  expect_match(body, "技術說明")
  expect_false(grepl("kit-guide-note", as.character(kit_tab_guide("目的", "一", "結果"))))
})

test_that("狀態框只有四種標籤與顏色（KIT 設計語言 3.6）", {
  html <- function(st) as.character(make_status_ui(st))
  expect_null(make_status_ui(kit_status()))
  ok <- html(kit_done_status("串接完成：結果共 688 列 × 2,058 欄。"))
  expect_match(ok, "alert-success")
  expect_match(ok, "【完成】</strong>\\s*串接完成：結果共 688 列 × 2,058 欄。")
  warn <- html(kit_done_status("疊加完成。", warnings = "有 3 個欄位型別不一致"))
  expect_match(warn, "alert-warning")
  expect_match(warn, "【完成，但請注意】")
  stale <- html(kit_stale_status("執行串接"))
  expect_match(stale, "alert-warning")
  expect_match(stale, "【要重新執行】")
  expect_match(stale, "請再按一次「執行串接」")
  stop_html <- html(kit_status("error", "請選擇 ID 欄位。"))
  expect_match(stop_html, "alert-danger")
  expect_match(stop_html, "【停下來】")
  all <- paste(ok, warn, stale, stop_html)
  expect_no_match(all, "沒有完成|需要重新執行|alert-info")
})

test_that("狀態框最多直接列 3 點，其他收在「還有 N 點」", {
  st <- kit_done_status("完成。", warnings = paste0("第 ", 1:5, " 點"))
  html <- as.character(make_status_ui(st))
  head <- sub("<details.*$", "", html)
  expect_match(head, "第 3 點")
  expect_no_match(head, "第 4 點")
  expect_match(html, "<summary>還有 2 點</summary>")
  expect_match(html, "第 5 點")
})

test_that("停下來：未預期的錯誤寫怎麼辦，原始訊息收起來；白話錯誤直接顯示", {
  st <- kit_error_status(simpleError("subscript out of bounds"), "串接")
  html <- as.character(make_status_ui(st))
  expect_match(html, "【停下來】</strong>\\s*串接時發生沒有預期到的問題")
  expect_match(html, "怎麼辦")
  expect_match(html, "<summary>原始訊息</summary>")
  expect_match(html, "subscript out of bounds")
  user <- tryCatch(kit_abort("需要至少 2 個檔案。"), error = function(e) e)
  todo <- shiny::actionLink("goto_upload_status", "到「檔案上傳」加入檔案 →")
  html <- as.character(make_status_ui(kit_error_status(user, "串接", todo)))
  expect_match(html, "需要至少 2 個檔案。")
  expect_no_match(html, "原始訊息")
  expect_match(html, "kit-status-action")
  expect_match(html, "goto_upload_status")
  expect_null(kit_upload_todo(identity, 2, 2L))
  expect_match(as.character(kit_upload_todo(identity, 1, 2L)), "「檔案上傳」")
})

test_that("數字加千分位", {
  expect_equal(kit_num(c(5, 3418, 1234567)), c("5", "3,418", "1,234,567"))
  expect_equal(format_dims(688, 2058), "688 列 × 2,058 欄")
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

  withr::local_envvar(KIT_DEPLOY = "local")
  expect_no_match(as.character(kit_kitsui_button()), "posit\\.cloud")
  expect_no_match(as.character(kit_ui(NULL)), "kitsui\\.share\\.connect\\.posit\\.cloud")
  withr::local_envvar(KIT_DEPLOY = "cloud")
  expect_match(as.character(kit_kitsui_button()), "kitsui.share.connect.posit.cloud", fixed = TRUE)
})

test_that("主要按鈕還不能按時停用，上面一行 .kit-ready.todo 寫還差什麼（設計審查 22）", {
  expect_null(kit_ready_line(character(0)))
  line <- as.character(kit_ready_line(c("選 ID 欄", "選檔案")))
  expect_match(line, 'class="kit-ready todo"', fixed = TRUE)
  expect_match(line, "還差：選 ID 欄；選檔案。")
  expect_null(kit_need_files(2, 2L))
  expect_match(kit_need_files(1, 2L), "只有 1 個")

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

test_that("結果出來以後把狀態框捲到畫面上方；減少動態效果時不平滑捲動（設計審查 17）", {
  js <- as.character(kit_app_js())
  expect_match(js, "addCustomMessageHandler('kit-scroll'", fixed = TRUE)
  expect_match(js, "prefers-reduced-motion: reduce", fixed = TRUE)
  expect_match(js, "scrollIntoView")

  sent <- list()
  flushed <- list()
  fake <- list(
    ns = shiny::NS("join"),
    onFlushed = function(f, once = TRUE) flushed[[length(flushed) + 1]] <<- f,
    sendCustomMessage = function(type, message) sent[[length(sent) + 1]] <<- list(type, message)
  )
  expect_equal(kit_scroll_to("status", session = fake), "join-status")
  expect_length(sent, 0)          # 等 output 都送出去以後才捲
  flushed[[1]]()
  expect_equal(sent[[1]], list("kit-scroll", "join-status"))
  expect_null(kit_scroll_to("status", session = NULL))

  # 各頁按下主要按鈕以後都會捲
  for (f in c("mod_join.R", "mod_stack.R", "mod_attrition.R", "mod_missing.R",
              "mod_reshape.R", "mod_convert.R")) {
    src <- paste(deparse(get(sub("\\.R$", "_server", f))), collapse = "\n")
    expect_match(src, 'kit_scroll_to("status")', fixed = TRUE, info = f)
  }
})

test_that("狀態框的重點（points）跟在要注意的事後面，不會讓狀態變成「請注意」", {
  st <- kit_done_status("串接完成。", points = c("6,864 人", "688 人每個檔案都有"))
  html <- as.character(make_status_ui(st))
  expect_match(html, "alert-success")
  expect_match(html, "<li>6,864 人</li>")
  warn <- as.character(make_status_ui(kit_done_status("完成。", warnings = "ID 重複", points = "重點")))
  expect_match(warn, "ID 重複</li>\\s*<li>重點")
})
