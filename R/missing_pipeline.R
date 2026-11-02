#' 預設遺漏值代碼（以函式提供：套件模式下 load_backend 只匯入函式）
#' @noRd
kit_na_codes_default <- function() "7777, 8888, 9995, 9996, 9999"

#' 解析遺漏值代碼字串（半形／全形逗號、頓號、分號或空白分隔）
#' @return list(codes = 數值向量, invalid = 無法解析的片段)
#' @noRd
parse_na_codes <- function(text) {
  text <- if (is.null(text)) "" else text
  # 原文：[,，、;；[:space:]]+
  parts <- strsplit(text, "[,\uff0c\u3001;\uff1b[:space:]]+")[[1]]
  parts <- parts[nzchar(parts)]
  nums <- suppressWarnings(as.numeric(parts))
  list(
    codes = unique(nums[!is.na(nums)]),
    invalid = parts[is.na(nums)]
  )
}

#' 猜測 ID 欄位（預設不做遺漏值轉換）
#'
#' - id 前後不是英文字母：id、ID、release_id、idno、id_2、child.id、w3id、學生ID
#' - 小寫字母後接大寫 ID / Id：studentID、childId
#' - 常見縮寫：sid、pid、uid、hhid、stuid、caseid…
#' - 中文：編號、學號、序號、流水號、識別碼
#' 不會抓到 idea_score、valid、paid。
#' @noRd
guess_id_cols <- function(cols) {
  hit <- grepl("(^|[^A-Za-z])id(no)?($|[^A-Za-z])", cols, ignore.case = TRUE) |
    grepl("[a-z](ID|Id)$", cols) |
    grepl("^(s|p|u|c|hh|stu|subj|child|case|resp)id$", cols, ignore.case = TRUE) |
    # 原文：編號|學號|序號|流水號|識別碼
    grepl("\u7de8\u865f|\u5b78\u865f|\u5e8f\u865f|\u6d41\u6c34\u865f|\u8b58\u5225\u78bc", cols)
  cols[hit]
}

#' 預設的「遺漏值標籤」關鍵字（數值標籤含這些字的值，視為遺漏值代碼）
#'
#' 不含「不知道」「不清楚」「don't know」：在有些題是有效的答案（例如「97 不知道或不清楚」），
#' 要轉成 NA 時請使用者自己加上。
#' @noRd
kit_na_label_keywords_default <- function() {
  paste(
    # 原文：遺漏, 跳答, 跳題, 不適用, 拒答, 拒絕回答, 未回答, 未作答, 漏答, 無反應,
    "\u907a\u6f0f, \u8df3\u7b54, \u8df3\u984c, \u4e0d\u9069\u7528, \u62d2\u7b54, \u62d2\u7d55\u56de\u7b54, \u672a\u56de\u7b54, \u672a\u4f5c\u7b54, \u6f0f\u7b54, \u7121\u53cd\u61c9,",
    "missing, refused, not applicable, skip, no answer"
  )
}

#' 解析關鍵字（逗號、頓號、分號分隔；英文片語中的空白保留）
#' @noRd
parse_na_keywords <- function(text) {
  # 原文：[,，、;；]+
  parts <- trimws(strsplit(text %||% "", "[,\uff0c\u3001;\uff1b]+")[[1]])
  unique(parts[nzchar(parts)])
}

#' 依數值標籤找出某一欄的遺漏值代碼
#'
#' 標籤含任一關鍵字的值才算；數值欄另外要求代碼是負數，或大於其他所有標籤的值
#' （遺漏值代碼通常在選項範圍外，例如 1–5 分的 98、99）。
#' 量表中間的「不清楚」這類一般選項因此不會被當成遺漏。
#' @return 具名向量（值 = 代碼，名稱 = 標籤）；沒有時長度 0
#' @noRd
label_na_codes_of <- function(x, keywords) {
  labels <- attr(x, "labels", exact = TRUE)
  if (length(labels) == 0 || length(keywords) == 0) return(NULL)
  pattern <- paste(gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", keywords), collapse = "|")
  is_na <- grepl(pattern, names(labels), ignore.case = TRUE, perl = TRUE)
  values <- as.vector(unclass(labels))
  if (is.numeric(values) && any(is_na)) {
    top <- max(c(-Inf, values[!is_na]))
    is_na <- is_na & (values < 0 | values > top)
  }
  if (!any(is_na)) return(NULL)
  stats::setNames(values[is_na], names(labels)[is_na])
}

#' 各欄依數值標籤的遺漏值代碼
#' @return 具名 list：名稱為欄名，值為 label_na_codes_of() 的結果
#' @noRd
label_na_codes <- function(df, keywords, exclude = character(0)) {
  cols <- setdiff(names(df), exclude)
  out <- lapply(df[cols], label_na_codes_of, keywords = keywords)
  out[!vapply(out, is.null, logical(1))]
}

#' 代碼與標籤的文字：「98 跳答、99 遺漏值」
#' @noRd
format_label_codes <- function(codes, counts = NULL) {
  value <- if (is.numeric(codes)) num_to_text(codes) else trimws(codes)
  text <- paste(value, trimws(names(codes)))
  # 原文：（ |  格）
  if (!is.null(counts)) text <- paste0(text, "\uff08", kit_num(counts), " \u683c\uff09")
  # 原文：、
  paste(text, collapse = "\u3001")
}

#' 依數值標籤偵測到的遺漏值代碼（給 ② 在轉換前預覽）
#' @return tibble(檔案, 欄位, 遺漏值代碼, 資料中的格數)；沒有時 0 列
#' @noRd
label_na_table <- function(dfs, file_names, keywords, exclude = character(0)) {
  rows <- purrr::map2(dfs, file_names, function(df, name) {
    found <- label_na_codes(df, keywords, exclude)
    if (length(found) == 0) return(NULL)
    tibble::tibble(
      # 原文：檔案
      "\u6a94\u6848" = name,
      # 原文：欄位
      "\u6b04\u4f4d" = names(found),
      # 原文：遺漏值代碼
      "\u907a\u6f0f\u503c\u4ee3\u78bc" = vapply(found, format_label_codes, character(1), USE.NAMES = FALSE),
      # 原文：資料中的格數
      "\u8cc7\u6599\u4e2d\u7684\u683c\u6578" = vapply(names(found), function(col) {
        sum(na_code_hits(df[[col]], found[[col]]))
      }, integer(1), USE.NAMES = FALSE)
    )
  })
  rows <- purrr::compact(rows)
  if (length(rows) == 0) {
    return(tibble::tibble(
      # 原文：檔案 | 欄位 | 遺漏值代碼 | 資料中的格數
      "\u6a94\u6848" = character(), "\u6b04\u4f4d" = character(), "\u907a\u6f0f\u503c\u4ee3\u78bc" = character(), "\u8cc7\u6599\u4e2d\u7684\u683c\u6578" = integer()
    ))
  }
  dplyr::bind_rows(rows)
}

#' x 中等於代碼的位置（文字欄去掉前後空白後比對）
#' @noRd
na_code_hits <- function(x, codes) {
  if (is.numeric(x)) {
    val <- as.numeric(unclass(x))
    return(!is.na(val) & val %in% as.numeric(codes))
  }
  val <- trimws(as.vector(unclass(x)))
  !is.na(val) & val %in% trimws(gsub("[[:cntrl:]]", "", codes))
}

#' ② 的判斷方式要不要改成「依數值標籤」
#'
#' 資料沒有 KIT 代碼，或數值標籤標的遺漏值是 KIT 代碼格數的 10 倍以上時改用依標籤
#' （TDCYP 有幾十格真實值剛好等於 9999，但標籤標了幾十萬格遺漏值）；
#' 兩者都有、標籤較多但沒有多到 10 倍時只建議，不改；標籤沒有比較多時什麼都不說。
#' @param kit_cells KIT 代碼的格數（count_code_cells()）
#' @param label_cells 數值標籤標為遺漏值的格數
#' @return list(switch = TRUE/FALSE, note = 一句話或 NULL)
#' @noRd
na_mode_suggestion <- function(kit_cells, label_cells) {
  fmt <- \(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
  if (label_cells == 0 || label_cells <= kit_cells) return(list(switch = FALSE, note = NULL))
  # 原文：SPSS 數值標籤標示了  |  格遺漏值（例如「遺漏值」「跳答」）
  what <- paste0("SPSS \u6578\u503c\u6a19\u7c64\u6a19\u793a\u4e86 ", fmt(label_cells), " \u683c\u907a\u6f0f\u503c\uff08\u4f8b\u5982\u300c\u907a\u6f0f\u503c\u300d\u300c\u8df3\u7b54\u300d\uff09")
  if (kit_cells == 0 || label_cells >= 10 * kit_cells) {
    kit <- if (kit_cells == 0) {
      # 原文：資料中沒有 7777、9999 這類遺漏值代碼，
      "\u8cc7\u6599\u4e2d\u6c92\u6709 7777\u30019999 \u9019\u985e\u907a\u6f0f\u503c\u4ee3\u78bc\uff0c"
    } else {
      # 原文：資料中只有  |  格等於 7777、9999 這類遺漏值代碼（可能是真實的值），
      paste0("\u8cc7\u6599\u4e2d\u53ea\u6709 ", fmt(kit_cells), " \u683c\u7b49\u65bc 7777\u30019999 \u9019\u985e\u907a\u6f0f\u503c\u4ee3\u78bc\uff08\u53ef\u80fd\u662f\u771f\u5be6\u7684\u503c\uff09\uff0c")
    }
    return(list(switch = TRUE, note = paste0(
      # 原文：但 | ，所以已改用「依 SPSS 數值標籤」。
      kit, "\u4f46", what, "\uff0c\u6240\u4ee5\u5df2\u6539\u7528\u300c\u4f9d SPSS \u6578\u503c\u6a19\u7c64\u300d\u3002",
      # 原文：要用自己的代碼清單，請改選「依代碼清單」。
      "\u8981\u7528\u81ea\u5df1\u7684\u4ee3\u78bc\u6e05\u55ae\uff0c\u8acb\u6539\u9078\u300c\u4f9d\u4ee3\u78bc\u6e05\u55ae\u300d\u3002"
    )))
  }
  list(switch = FALSE, note = paste0(
    # 原文：資料中有  |  格等於 7777、9999 這類遺漏值代碼，
    "\u8cc7\u6599\u4e2d\u6709 ", fmt(kit_cells), " \u683c\u7b49\u65bc 7777\u30019999 \u9019\u985e\u907a\u6f0f\u503c\u4ee3\u78bc\uff0c", what,
    # 原文：。如果資料的遺漏值是寫在數值標籤裡，可以改選「依 SPSS 數值標籤」。
    "\u3002\u5982\u679c\u8cc7\u6599\u7684\u907a\u6f0f\u503c\u662f\u5beb\u5728\u6578\u503c\u6a19\u7c64\u88e1\uff0c\u53ef\u4ee5\u6539\u9078\u300c\u4f9d SPSS \u6578\u503c\u6a19\u7c64\u300d\u3002"
  ))
}

#' 資料中像遺漏值代碼的格數（不含 ID 欄），用來提醒使用者先處理
#'
#' 數值欄等於 codes 的格；給 keywords 時，另加各欄數值標籤標成遺漏的值。
#' @noRd
count_code_cells <- function(
    dfs,
    codes = parse_na_codes(kit_na_codes_default())$codes,
    keywords = NULL) {
  sum(purrr::map_int(dfs, function(df) {
    cols <- setdiff(names(df), guess_id_cols(names(df)))
    sum(purrr::map_int(df[cols], function(x) {
      col_codes <- codes
      by_label <- if (length(keywords) > 0) label_na_codes_of(x, keywords)
      if (length(by_label) > 0 && is.numeric(by_label)) {
        col_codes <- union(col_codes, by_label)
      }
      n <- if (is.numeric(x)) sum(as.numeric(unclass(x)) %in% col_codes) else 0L
      if (length(by_label) > 0 && !is.numeric(by_label)) {
        n <- n + sum(na_code_hits(x, by_label))
      }
      as.integer(n)
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
    # 代碼的數值標籤一律移除（資料裡沒有這個代碼也一樣）
    old_labels <- attr(x, "labels", exact = TRUE)
    x <- drop_value_labels(x, if (is.numeric(x)) codes else code_text)
    if (!any(hit)) {
      if (!identical(attr(x, "labels", exact = TRUE), old_labels)) df[[col]] <- x
      next
    }

    x[hit] <- NA
    df[[col]] <- x
    # 原文：合計
    rows[[col]] <- c(stats::setNames(counts, code_text), "\u5408\u8a08" = sum(hit))
  }

  detail <- if (length(rows) == 0) {
    NULL
  } else {
    tibble::as_tibble(as.data.frame(do.call(rbind, rows), check.names = FALSE),
                      # 原文：欄位
                      rownames = "\u6b04\u4f4d")
  }
  list(data = df, detail = detail)
}

#' 依數值標籤把各欄的遺漏值代碼改為 NA（每欄只轉換自己標成遺漏的值）
#'
#' 沒有遺漏標籤的值（例如體重剛好 99 公斤）不會轉換。
#'
#' @param df 資料框
#' @param keywords 遺漏值標籤關鍵字
#' @param exclude 不處理的欄位
#' @return list(data, detail)；detail 每列一個有替換的欄位：欄位、轉換的代碼、合計
#' @noRd
replace_na_by_labels <- function(df, keywords, exclude = character(0)) {
  found <- label_na_codes(df, keywords, exclude)
  rows <- list()
  for (col in names(found)) {
    x <- df[[col]]
    codes <- found[[col]]
    hit <- na_code_hits(x, codes)
    counts <- vapply(seq_along(codes), function(i) {
      sum(na_code_hits(x, codes[i]))
    }, integer(1))
    x <- drop_value_labels(x, codes)
    x[hit] <- NA
    df[[col]] <- x
    if (any(hit)) {
      shown <- counts > 0
      rows[[col]] <- tibble::tibble(
        # 原文：欄位
        "\u6b04\u4f4d" = col,
        # 原文：轉換的代碼
        "\u8f49\u63db\u7684\u4ee3\u78bc" = format_label_codes(codes[shown], counts[shown]),
        # 原文：合計
        "\u5408\u8a08" = sum(hit)
      )
    }
  }
  list(data = df, detail = if (length(rows) == 0) NULL else dplyr::bind_rows(rows))
}

#' 移除指定值的數值標籤；標籤全部移除時不再是 labelled（保留變項標籤）
#'
#' labelled 卻沒有 labels 的欄，預覽（haven::as_factor()）會出錯。
#' @noRd
drop_value_labels <- function(x, values) {
  labels <- attr(x, "labels", exact = TRUE)
  if (is.null(labels) || length(values) == 0) return(x)
  keep <- labels[!(unclass(labels) %in% values)]
  if (length(keep) == length(labels)) return(x)
  if (length(keep) > 0) {
    attr(x, "labels") <- keep
    return(x)
  }
  if (haven::is.labelled(x)) return(haven::zap_labels(x))
  attr(x, "labels") <- NULL
  x
}

#' 匯出檔名：原檔名加 _na
#' @noRd
na_export_name <- function(name, ext) {
  paste0(tools::file_path_sans_ext(name), "_na.", ext)
}
