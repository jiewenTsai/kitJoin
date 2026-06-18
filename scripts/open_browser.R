# 在非互動 Rscript 下開啟預設瀏覽器（Windows 用 start / shell.exec）
open_kitjoin_browser <- function(url) {
  message("開啟瀏覽器：", url)
  if (.Platform$OS.type == "windows") {
  # shell.exec 較 browseURL 在 Rscript 下更可靠
    ok <- tryCatch({
      utils::shell.exec(url)
      TRUE
    }, error = function(e) FALSE)
    if (!ok) {
      shell(paste0("start \"\" ", shQuote(url, type = "cmd")), wait = FALSE)
    }
  } else {
    utils::browseURL(url)
  }
  invisible(NULL)
}
