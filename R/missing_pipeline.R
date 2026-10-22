#' 預設遺漏值代碼（以函式提供：套件模式下 load_backend 只匯入函式）
#' @noRd
kit_na_codes_default <- function() "7777, 8888, 9995, 9996, 9999"

#' 解析遺漏值代碼字串（半形／全形逗號、頓號、分號或空白分隔）
#' @return list(codes = 數值向量, invalid = 無法解析的片段)
#' @noRd
parse_na_codes <- function(text) {
  text <- if (is.null(text)) "" else text
  parts <- strsplit(text, "[,，、;；[:space:]]+")[[1]]
  parts <- parts[nzchar(parts)]
  nums <- suppressWarnings(as.numeric(parts))
  list(
    codes = unique(nums[!is.na(nums)]),
    invalid = parts[is.na(nums)]
  )
}

#' 猜測 ID 欄位（預設不做遺漏值轉換）：如 id、ID、release_id、idno、id_2
#' @noRd
guess_id_cols <- function(cols) {
  cols[grepl("(^|[_.])id(no)?($|[_.0-9])", cols, ignore.case = TRUE)]
}

#' 資料中等於遺漏值代碼的數值格數（不含 ID 欄），用來提醒使用者先處理
#' @noRd
count_code_cells <- function(
    dfs,
    codes = parse_na_codes(kit_na_codes_default())$codes) {
  sum(purrr::map_int(dfs, function(df) {
    cols <- setdiff(names(df), guess_id_cols(names(df)))
    sum(purrr::map_int(df[cols], function(x) {
      if (is.numeric(x)) sum(as.numeric(unclass(x)) %in% codes) else 0L
    }))
  }))
}

#' 將遺漏值代碼改為 NA
#'
#' 數值欄（含 SAV 標籤欄）比對代碼；文字欄僅在 match_text = TRUE 時比對。
#' SAV 標籤欄會一併移除對應代碼的數值標籤。
#'
#' @param df 資料框
#' @param codes 遺漏值代碼（數值向量）
#' @param exclude 不處理的欄位
#' @param match_text 文字欄是否也比對（去除前後空白後完全相同）
#' @return list(data, detail)；detail 每列一個有替換的欄位，各代碼筆數與合計
#' @noRd
replace_na_codes <- function(
    df,
    codes,
    exclude = character(),
    match_text = FALSE) {
  code_text <- format(
    codes,
    scientific = FALSE,
    trim = TRUE,
    drop0trailing = TRUE
  )
  target_cols <- setdiff(names(df), exclude)
  rows <- list()

  for (col in target_cols) {
    x <- df[[col]]
    if (is.numeric(x)) {
      val <- as.numeric(unclass(x))
      hit <- !is.na(val) & val %in% codes
      matched <- val[hit]
      counts <- vapply(codes, function(cd) sum(matched == cd), integer(1))
    } else if (match_text && is.character(x)) {
      val <- trimws(x)
      hit <- !is.na(val) & val %in% code_text
      matched <- val[hit]
      counts <- vapply(code_text, function(cd) sum(matched == cd), integer(1))
    } else {
      next
    }
    if (!any(hit)) next

    x[hit] <- NA
    labels <- attr(x, "labels", exact = TRUE)
    if (!is.null(labels) && is.numeric(labels)) {
      keep <- labels[!(as.numeric(labels) %in% codes)]
      attr(x, "labels") <- if (length(keep) > 0) keep else NULL
    }
    df[[col]] <- x
    rows[[col]] <- c(stats::setNames(counts, code_text), 合計 = sum(hit))
  }

  detail <- if (length(rows) == 0) {
    NULL
  } else {
    tibble::as_tibble(as.data.frame(do.call(rbind, rows), check.names = FALSE),
                      rownames = "欄位")
  }
  list(data = df, detail = detail)
}

#' 匯出檔名：原檔名加 _na
#' @noRd
na_export_name <- function(name, ext) {
  paste0(tools::file_path_sans_ext(name), "_na.", ext)
}
