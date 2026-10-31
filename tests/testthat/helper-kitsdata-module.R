# 「kits 作答檔」分頁（模組 kitsdata）的 testServer 共用工具
#
# 三個步驟卡：第 1 步每個檔案一列（cell_<欄>_<檔案 key>），第 2 步挑題目（不設領域），
# 第 3 步題目表：領域與計分（檢查清單 + 題目表）。
# 這些工具都在 shiny::testServer(kit_server, ...) 裡用（input id 要加 "kitsdata-"）。

kd_upload <- function(paths) {
  data.frame(name = basename(paths), size = file.size(paths), type = "", datapath = paths,
             stringsAsFactors = FALSE)
}

# 設定 kitsdata 的 input（名稱不用寫 "kitsdata-"）
kd_set <- function(session, ...) {
  args <- list(...)
  do.call(session$setInputs, stats::setNames(args, paste0("kitsdata-", names(args))))
}

# 按一下按鈕（值加 1；testServer 裡同樣的值不會觸發 observeEvent）
kd_click <- function(session, id) {
  full <- paste0("kitsdata-", id)
  now <- shiny::isolate(session$input[[full]]) %||% 0
  do.call(session$setInputs, stats::setNames(list(now + 1), full))
}

# 第 1 步：某一欄每個檔案填什麼（欄名或自己填的值），檔案 key 是 f1、f2…
kd_cells <- function(session, field, values) {
  ids <- paste0("cell_", field, "_f", seq_along(values))
  do.call(kd_set, c(list(session), stats::setNames(as.list(values), ids)))
}

# 輸出的文字（去掉標籤、連續空白變一個）
kd_text <- function(x) {
  if (is.null(x)) return("")
  if (is.list(x)) x <- x$html
  x <- as.vector(unclass(x))
  trimws(gsub("\\s+", " ", gsub("<[^>]+>", " ", x)))
}

# 輸出（renderUI）的文字
kd_out <- function(output, id) kd_text(output[[paste0("kitsdata-", id)]])

# 第 3 步的一件檢查是什麼狀態（"ok"、"todo"、"note"；沒有這一列時是 NA）
kd_check_state <- function(output, title_pattern) {
  html <- as.vector(unclass(output[["kitsdata-checks"]]$html))
  items <- regmatches(html, gregexpr("(?s)<li class=\"kit-check [a-z]+\">.*?</li>", html,
                                     perl = TRUE))[[1]]
  hit <- items[grepl(title_pattern, items)]
  if (length(hit) == 0) return(NA_character_)
  sub("(?s)^<li class=\"kit-check ([a-z]+)\">.*", "\\1", hit[1], perl = TRUE)
}

# 步驟標頭的狀態（"current"、"done"、"locked"）
kd_head_state <- function(output, k) {
  html <- as.vector(unclass(output[[paste0("kitsdata-head", k)]]$html))
  sub(".*class=\"kit-stephead is-([a-z]+)\".*", "\\1", gsub("\n", " ", html))
}

# 第 2 步用欄名挑題目；給了 domain 時，再到第 3 步的題目表把這些欄設成這個領域
# （第 2 步不設領域）。題目表用「找欄名」（包含就算）找到每一段開頭，勾選全部、設領域。
kd_pick <- function(session, prefix, domain = "") {
  kd_set(session, pick_prefix = prefix)
  kd_click(session, "pick_add")
  if (nzchar(domain)) kd_domain(session, prefix, domain)
}

# 第 3 步的題目表：已經挑的、欄名包含這幾段的欄設成 domain（之後清掉找欄名與勾選）
kd_domain <- function(session, prefix, domain) {
  parts <- trimws(strsplit(prefix, "[、,，;；[:space:]]+")[[1]])
  for (p in parts[nzchar(parts)]) {
    kd_set(session, tbl_search = p)
    kd_click(session, "tbl_sel_all")
    kd_set(session, bulk_action = "domain", bulk_domain = domain)
    kd_click(session, "bulk_apply")
  }
  kd_set(session, tbl_search = "")
  kd_click(session, "tbl_sel_none")
}
