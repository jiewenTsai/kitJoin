#' 預設遺漏值代碼（以函式提供：套件模式下 load_backend 只匯入函式）
#'
#' @param preset "kit"（KIT 資料庫的代碼，預設）或 "general"（一般研究資料：沒有預設，自己填）；
#'   代碼放在 constants.R 的 KIT_PRESETS
#' @noRd
kit_na_codes_default <- function(preset = "kit") {
  KIT_PRESETS[[preset]]$na_codes %||% KIT_PRESETS$kit$na_codes
}

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
#' - 一般研究資料常見的：participant、subject、PROLIFIC_PID、record_id…（KIT_PRESETS$general$id_names）
#' 不會抓到 idea_score、valid、paid。
#' @noRd
guess_id_cols <- function(cols) {
  hit <- grepl("(^|[^A-Za-z])id(no)?($|[^A-Za-z])", cols, ignore.case = TRUE) |
    grepl("[a-z](ID|Id)$", cols) |
    grepl("^(s|p|u|c|hh|stu|subj|child|case|resp)id$", cols, ignore.case = TRUE) |
    grepl(KIT_PRESETS$general$id_names, cols, ignore.case = TRUE) |
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
#' @param codes 遺漏值代碼（數值向量）：沒有列在 codes_by_col 的欄都用這一組
#' @param exclude 不處理的欄位
#' @param match_text 文字欄是否也比對（去除前後空白後完全相同）
#' @param codes_by_col 各欄自己的代碼（具名 list：欄名 → 數值向量）；列在這裡的欄只用自己的代碼，
#'   不套用 codes（給空的向量就是這一欄不處理）
#' @return list(data, detail)；detail 每列一個有替換的欄位，各代碼筆數與合計
#' @noRd
replace_na_codes <- function(
    df,
    codes,
    exclude = character(),
    match_text = FALSE,
    codes_by_col = list()) {
  as_code_text <- function(x) format(x, scientific = FALSE, trim = TRUE, drop0trailing = TRUE)
  target_cols <- setdiff(names(df), exclude)
  rows <- list()
  all_codes <- character(0)

  for (col in target_cols) {
    x <- df[[col]]
    col_codes <- if (col %in% names(codes_by_col)) codes_by_col[[col]] else codes
    if (length(col_codes) == 0) next
    code_text <- as_code_text(col_codes)
    if (is.numeric(x)) {
      val <- as.numeric(unclass(x))
      hit <- !is.na(val) & val %in% col_codes
      matched <- val[hit]
      counts <- vapply(col_codes, function(cd) sum(matched == cd), integer(1))
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
    x <- drop_value_labels(x, if (is.numeric(x)) col_codes else code_text)
    if (!any(hit)) {
      if (!identical(attr(x, "labels", exact = TRUE), old_labels)) df[[col]] <- x
      next
    }

    x[hit] <- NA
    df[[col]] <- x
    rows[[col]] <- stats::setNames(as.integer(counts), code_text)
    all_codes <- union(all_codes, code_text)
  }

  detail <- if (length(rows) == 0) {
    NULL
  } else {
    # 各欄的代碼可能不同：沒有的代碼填 0；全域代碼排前面
    shown <- c(intersect(as_code_text(codes), all_codes), setdiff(all_codes, as_code_text(codes)))
    m <- t(vapply(rows, function(r) {
      out <- stats::setNames(integer(length(shown)), shown)
      out[names(r)] <- r
      out
    }, integer(length(shown))))
    if (length(shown) == 1) m <- matrix(m, ncol = 1, dimnames = list(names(rows), shown))
    tibble::as_tibble(
      # 原文：合計
      as.data.frame(cbind(m, "\u5408\u8a08" = rowSums(m)), check.names = FALSE),
      # 原文：欄位
      rownames = "\u6b04\u4f4d"
    )
  }
  list(data = df, detail = detail)
}

#' 各欄自己的遺漏值代碼：每行「欄位: 代碼」，冒號、等號或 Tab 分隔
#'
#' 「q1, q2: 8, 9」可以一次設好幾欄；「age:」（沒有代碼）表示這一欄不處理。
#' 從 Excel 複製兩欄（欄位、代碼）貼上也可以。
#' @param cols 資料裡有的欄（NULL 不檢查）
#' @return list(map = 具名 list（欄名 → 數值向量）, invalid = 看不懂的行, unknown = 資料裡沒有的欄)
#' @noRd
parse_na_codes_by_col <- function(text, cols = NULL) {
  lines <- trimws(strsplit(text %||% "", "\r?\n")[[1]])
  lines <- lines[nzchar(lines)]
  map <- list()
  invalid <- character(0)
  for (line in lines) {
    # 原文：[:：=\t]
    parts <- strsplit(line, "[:\uff1a=\t]", perl = TRUE)[[1]]
    if (length(parts) < 1 || length(parts) > 2) {
      invalid <- c(invalid, line)
      next
    }
    # 原文：[,，、;；]+
    vars <- trimws(strsplit(parts[1], "[,\uff0c\u3001;\uff1b]+")[[1]])
    vars <- vars[nzchar(vars)]
    parsed <- parse_na_codes(if (length(parts) == 2) parts[2] else "")
    if (length(vars) == 0 || length(parsed$invalid) > 0) {
      invalid <- c(invalid, line)
      next
    }
    for (v in vars) map[[v]] <- parsed$codes
  }
  unknown <- if (is.null(cols)) character(0) else setdiff(names(map), cols)
  list(map = map, invalid = invalid, unknown = unknown)
}

#' SPSS 使用者自訂遺漏值（user-missing）改成 NA
#'
#' 只有讀檔時勾了「保留 SPSS 使用者自訂遺漏值」才會有（haven 的 labelled_spss）；
#' 改完拿掉 na_values／na_range，數值標籤保留。
#' @return list(data, detail)；detail：欄位、轉換的代碼、合計
#' @noRd
replace_user_na <- function(df, exclude = character(0)) {
  rows <- list()
  for (col in setdiff(names(df), exclude)) {
    x <- df[[col]]
    if (!inherits(x, "haven_labelled_spss")) next
    user <- is.na(x) & !is.na(unclass(x))
    vals <- unique(as.vector(unclass(x))[user])
    df[[col]] <- haven::zap_missing(x)
    if (any(user)) {
      rows[[col]] <- tibble::tibble(
        # 原文：欄位
        "\u6b04\u4f4d" = col,
        # 原文：轉換的代碼 | 、
        "\u8f49\u63db\u7684\u4ee3\u78bc" = paste(num_to_text(sort(vals)), collapse = "\u3001"),
        # 原文：合計
        "\u5408\u8a08" = sum(user)
      )
    }
  }
  list(data = df, detail = if (length(rows) == 0) NULL else dplyr::bind_rows(rows))
}

#' 資料裡有沒有 SPSS 使用者自訂遺漏值（labelled_spss 欄）
#' @noRd
has_user_na <- function(dfs) {
  any(purrr::map_lgl(dfs, function(df) {
    any(vapply(df, inherits, logical(1), what = "haven_labelled_spss"))
  }))
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
