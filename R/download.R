# 下載按鈕：自己送檔案，每個瀏覽器都拿得到正確檔名與副檔名
#
# shiny::downloadHandler() 遇到中文檔名會先轉址、只送 `attachment`，有些瀏覽器就存成
# `missing-download_sav`；有些瀏覽器（或下載外掛）甚至不看檔名標頭，只看網址最後一段。
# 所以這裡用 session$registerDataObj() 自己回應，三道保險都放上同一個檔名：
#   1. 網址最後一段就是檔名：session/<token>/dataobj/<key>/<檔名>?w=&nonce=
#   2. Content-Disposition 同時有 filename（ASCII 備用名）與 filename*（UTF-8，RFC 6266）
#   3. <a download="檔名">
# 整個檔案自成一體（只用 shiny、htmltools 的公開函式），kitsUI 可以直接拿去用。

#' 副檔名（.csv.gz 算一個；只認 ASCII 英數字）
#' @noRd
kit_download_ext <- function(filename) {
  m <- regmatches(filename, regexpr("(?i)\\.(csv\\.gz|[a-z0-9]+)$", filename, perl = TRUE))
  if (length(m) == 0) "" else substring(m, 2)
}

#' 依副檔名決定 Content-Type
#'
#' .csv.gz 與 .sav 都用 application/octet-stream：照 MIME 補副檔名的瀏覽器才不會多加 .gz。
#' @noRd
kit_download_type <- function(filename) {
  csv <- tolower(kit_download_ext(filename)) == "csv"
  if (csv) "text/csv; charset=UTF-8" else "application/octet-stream"
}

#' 百分比編碼（UTF-8 位元組，%XX 大寫）
#' @param keep 不用編碼的字元
#' @noRd
kit_percent_encode <- function(x, keep) {
  bytes <- charToRaw(enc2utf8(x))
  ok <- bytes %in% charToRaw(keep)
  out <- sprintf("%%%02X", as.integer(bytes))
  out[ok] <- vapply(bytes[ok], rawToChar, "")
  paste(out, collapse = "")
}

# RFC 5987 attr-char（filename* 用）與網址路徑的安全字元（unreserved）
KIT_ATTR_CHAR <- paste0(
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789", "!#$&+-.^_`|~"
)
KIT_URL_CHAR <- "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-._~"

#' 只有 ASCII 的備用檔名（給不認得 filename* 的舊瀏覽器）
#'
#' 去掉非 ASCII 字元（副檔名保留，.csv.gz 整個保留）；剩下空白就叫 kitjoin，
#' 開頭是 _ - . 就在前面補 kitjoin。例如「第一波_na.sav」→「kitjoin_na.sav」。
#' @noRd
kit_ascii_filename <- function(filename) {
  ext <- kit_download_ext(filename)
  base <- if (nzchar(ext)) substr(filename, 1, nchar(filename) - nchar(ext) - 1) else filename
  base <- gsub("[^ -~]+", "", base) |> gsub(pattern = " +", replacement = " ") |> trimws()
  if (!nzchar(base)) base <- "kitjoin"
  if (grepl("^[-_.]", base)) base <- paste0("kitjoin", base)
  paste0(base, if (nzchar(ext)) ".", ext)
}

#' Content-Disposition 標頭（RFC 6266）
#'
#' ASCII 檔名：`attachment; filename="name.sav"`（" 與 \ 前面加 \）。
#' 有非 ASCII 字元：再加 `filename*=UTF-8''<百分比編碼>`。
#' @noRd
kit_content_disposition <- function(filename) {
  quote <- function(x) paste0('"', gsub('(["\\\\])', "\\\\\\1", x), '"')
  if (!grepl("[^ -~]", filename)) return(paste0("attachment; filename=", quote(filename)))
  paste0(
    "attachment; filename=", quote(kit_ascii_filename(filename)),
    "; filename*=UTF-8''", kit_percent_encode(filename, KIT_ATTR_CHAR)
  )
}

#' 收到下載請求時：寫暫存檔、回傳 httpResponse
#'
#' content(file) 在 session 的 reactive domain、isolate() 裡執行，可以讀 reactive。
#' 出錯時回 500 與一句純文字，錯誤寫進 log。
#' @noRd
kit_download_filter <- function(session) {
  function(data, req) {
    tryCatch(
      shiny::withReactiveDomain(session, shiny::isolate(kit_download_response(data))),
      error = function(e) {
        message("kitJoin 下載失敗（", data$filename, "）：", conditionMessage(e))
        shiny::httpResponse(500, "text/plain; charset=UTF-8", "下載失敗，請重新執行後再試。")
      }
    )
  }
}

# 寫暫存檔（副檔名照下載檔名），回傳檔案本身；httpuv 送完會刪掉（owned = TRUE）
kit_download_response <- function(data) {
  ext <- kit_download_ext(data$filename)
  tmp <- tempfile(fileext = if (nzchar(ext)) paste0(".", ext) else "")
  done <- FALSE
  on.exit(if (!done) unlink(tmp))
  data$content(tmp)
  if (!file.exists(tmp)) stop("沒有寫出檔案")
  done <- TRUE
  shiny::httpResponse(
    200, kit_download_type(data$filename), list(file = tmp, owned = TRUE),
    headers = list(
      "Content-Disposition" = kit_content_disposition(data$filename),
      "Cache-Control" = "no-cache"
    )
  )
}

#' 註冊下載，回傳網址（相對於 app，Posit Connect 的 /_w_.../ 底下也能用）
#'
#' 網址最後一段是編碼過的檔名；shiny 比對 dataobj/<key>/<多一段> 時只看 key。
#' 每次註冊 nonce 都不同，所以結果更新後網址也會變。
#' 模組的 session 會自己幫 id 加上 namespace（key 是「模組-id」）。
#' testServer() 的假 session 的 registerDataObj 什麼都不做（回傳 NULL）：
#' 改存在 userData$kit_downloads[[key]]（測試從這裡呼叫），網址照同樣格式。
#' @param id 不含 namespace 的 id
#' @noRd
kit_register_download <- function(session, id, filename, content) {
  data <- list(filename = filename, content = content)
  filter <- kit_download_filter(session)
  url <- session$registerDataObj(id, data, filter)
  if (is.null(url)) {
    key <- session$ns(id)
    store <- session$userData
    store$kit_downloads[[key]] <- list(data = data, filter = filter)
    url <- paste0("session/test/dataobj/", utils::URLencode(key, TRUE), "?w=&nonce=test")
  }
  sub("?", paste0("/", kit_percent_encode(filename, KIT_URL_CHAR), "?"), url, fixed = TRUE)
}

#' 下載按鈕（放在 renderUI() 裡）
#'
#' 外觀和 shiny::downloadButton() 一樣，但網址由我們決定、帶 download="檔名"。
#' renderUI() 重算時會重新註冊，網址跟著換，不會拿到舊檔。
#'
#' @param session 模組的 session
#' @param id 按鈕 id（不含 namespace）
#' @param filename 下載檔名（字串；網址、標頭、download 屬性都用它）
#' @param content function(file)，把檔案寫到 file
#' @noRd
kit_download_button <- function(session, id, label, filename, content, class = NULL) {
  shiny::tags$a(
    id = session$ns(id),
    class = "btn btn-default kit-download",
    class = class,
    href = kit_register_download(session, id, filename, content),
    target = "_blank",
    download = filename,
    shiny::icon("download"),
    label
  )
}
