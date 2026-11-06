# 篩選與摘要：篩選（filter、select、arrange）與分組摘要的處理邏輯
#
# 每個條件先寫成一段 R 程式碼（filter_cond_code()），app 和「用 R 怎麼做」用的是同一段：
# app 用 eval() 算出每一列符不符合，所以結果和程式碼一定相同。
# 空白（NA）照 dplyr::filter()：條件算出來是 NA 的列不留下，但另外算出有幾列，告訴使用者。

# 條件的運算子：值 → 畫面上的文字
FILTER_OP_LABELS <- c(
  # 原文：等於 | 不等於 | 大於 | 大於或等於 | 小於 | 小於或等於
  eq = "\u7b49\u65bc", ne = "\u4e0d\u7b49\u65bc", gt = "\u5927\u65bc", ge = "\u5927\u65bc\u6216\u7b49\u65bc", lt = "\u5c0f\u65bc", le = "\u5c0f\u65bc\u6216\u7b49\u65bc",
  # 原文：介於（含兩端） | 是其中之一 | 不是其中之一
  between = "\u4ecb\u65bc\uff08\u542b\u5169\u7aef\uff09", `in` = "\u662f\u5176\u4e2d\u4e4b\u4e00", not_in = "\u4e0d\u662f\u5176\u4e2d\u4e4b\u4e00",
  # 原文：包含文字 | 是空白 | 不是空白
  contains = "\u5305\u542b\u6587\u5b57", is_na = "\u662f\u7a7a\u767d", not_na = "\u4e0d\u662f\u7a7a\u767d"
)

# 不重複的值超過這個數（例如 ID）時，不列出所有的值，改成自己輸入
FILTER_MAX_CHOICES <- 200L

# 摘要的統計量：值 → 畫面上的文字（人數 n 一定會算，不在這裡）
SUMMARY_STATS <- c(
  # 原文：有效值個數 | 遺漏數 | 平均數 | 標準差
  n_valid = "\u6709\u6548\u503c\u500b\u6578", n_missing = "\u907a\u6f0f\u6578", mean = "\u5e73\u5747\u6578", sd = "\u6a19\u6e96\u5dee",
  # 原文：中位數 | 最小值 | 最大值
  median = "\u4e2d\u4f4d\u6578", min = "\u6700\u5c0f\u503c", max = "\u6700\u5927\u503c"
)
SUMMARY_STATS_DEFAULT <- c("n_valid", "n_missing", "mean", "sd")

# 分組超過這個數（組數 × 變項數）時停下：例如誤選 ID 當分組欄
SUMMARY_MAX_CELLS <- 1e6


# ── 欄的種類與選項 ───────────────────────────────────────────────────────────

#' 欄的種類：決定可以用哪些運算子
#'
#' "num"（數字）、"labelled_num"（有 SPSS 數值標籤的數字）、
#' "text"（文字、類別）、"text_other"（日期、TRUE/FALSE 等：比對時先轉成文字）
#' @noRd
filter_col_kind <- function(x) {
  if (is.numeric(x)) return(if (haven::is.labelled(x)) "labelled_num" else "num")
  if (is.character(x) || is.factor(x)) return("text")
  "text_other"
}

#' 這一欄可以用的運算子（具名向量：畫面文字 = 值）
#' @param many 不重複的值太多（自己輸入一個值，不列出選項）
#' @noRd
filter_ops_for <- function(kind, many = FALSE) {
  num <- c("eq", "ne", "gt", "ge", "lt", "le", "between")
  text <- c("in", "not_in")
  ops <- switch(kind,
    num = c(num, "is_na", "not_na"),
    labelled_num = c(text, num, "is_na", "not_na"),
    c(text, "contains", "is_na", "not_na")
  )
  labels <- FILTER_OP_LABELS[ops]
  if (many) {
    # 原文：等於（自己輸入一個值）
    labels[ops == "in"] <- "\u7b49\u65bc\uff08\u81ea\u5df1\u8f38\u5165\u4e00\u500b\u503c\uff09"
    # 原文：不等於（自己輸入一個值）
    labels[ops == "not_in"] <- "\u4e0d\u7b49\u65bc\uff08\u81ea\u5df1\u8f38\u5165\u4e00\u500b\u503c\uff09"
  }
  stats::setNames(ops, labels)
}

#' 「是其中之一」的選項：欄裡不重複的值（有數值標籤的寫成「1 = 男」）
#'
#' @return 具名文字向量（畫面文字 = 值的文字）；不重複的值超過 max 個時 NULL
#' @noRd
filter_value_choices <- function(x, max = FILTER_MAX_CHOICES) {
  if (is.factor(x)) {
    vals <- levels(x)
    if (length(vals) > max) return(NULL)
    return(stats::setNames(vals, vals))
  }
  u <- unique(vctrs::vec_data(x))
  u <- u[!is.na(u)]
  if (length(u) > max) return(NULL)
  u <- sort(u)
  if (!is.numeric(u)) {
    u <- as.character(u)
    return(stats::setNames(u, u))
  }
  text <- num_to_text(u)
  shown <- text
  labs <- attr(x, "labels", exact = TRUE)
  if (!is.null(labs)) {
    hit <- match(u, unname(labs))
    shown <- ifelse(is.na(hit), text, paste0(text, " = ", names(labs)[hit]))
  }
  stats::setNames(text, shown)
}


# ── 條件 ─────────────────────────────────────────────────────────────────────

#' 畫面上的一個條件整理成固定的樣子，並檢查有沒有填完
#'
#' @param cond list(col, op, num, lo, hi, values, text)：畫面上的值（可以是 NULL）
#' @param df 這個檔案的資料（判斷欄的種類）
#' @param i 第幾個條件（訊息用）
#' @return list(col, op, kind, value, problem)；problem 是沒填完時的訊息（填完是 NULL）
#' @noRd
filter_cond_clean <- function(cond, df, i = 1L) {
  col <- cond$col %||% ""
  out <- list(col = col, op = NULL, kind = NULL, value = NULL, problem = NULL)
  # 原文：條件 
  label <- paste0("\u689d\u4ef6 ", i)
  if (!nzchar(col)) {
    # 原文：：還沒選直欄
    out$problem <- paste0(label, "\uff1a\u9084\u6c92\u9078\u76f4\u6b04")
    return(out)
  }
  if (!col %in% names(df)) {
    # 原文：：檔案裡沒有  |  這一個直欄
    out$problem <- paste0(label, "\uff1a\u6a94\u6848\u88e1\u6c92\u6709 ", col, " \u9019\u4e00\u500b\u76f4\u6b04")
    return(out)
  }
  x <- df[[col]]
  kind <- filter_col_kind(x)
  ops <- filter_ops_for(kind)
  op <- cond$op %||% ""
  if (!op %in% ops) op <- unname(ops[1])
  out$op <- op
  out$kind <- kind
  is_num <- kind %in% c("num", "labelled_num")
  one_num <- function(v) {
    v <- suppressWarnings(as.numeric(v))
    if (length(v) != 1 || is.na(v)) NULL else v
  }
  # 原文：（ | ）：
  missing_value <- function(what) paste0(label, "\uff08", col, "\uff09\uff1a", what)
  if (op %in% c("eq", "ne", "gt", "ge", "lt", "le")) {
    out$value <- one_num(cond$num)
    # 原文：還沒填數字
    if (is.null(out$value)) out$problem <- missing_value("\u9084\u6c92\u586b\u6578\u5b57")
  } else if (op == "between") {
    lo <- one_num(cond$lo)
    hi <- one_num(cond$hi)
    if (is.null(lo) || is.null(hi)) {
      # 原文：「介於」的兩個數字都要填
      out$problem <- missing_value("\u300c\u4ecb\u65bc\u300d\u7684\u5169\u500b\u6578\u5b57\u90fd\u8981\u586b")
    } else {
      out$value <- sort(c(lo, hi))
    }
  } else if (op %in% c("in", "not_in")) {
    many <- is.null(filter_value_choices(x))
    vals <- if (many) cond$text else cond$values
    vals <- vals[!is.na(vals) & nzchar(vals %||% character(0))]
    if (length(vals) == 0) {
      # 原文：還沒輸入值 | 還沒選值
      out$problem <- missing_value(if (many) "\u9084\u6c92\u8f38\u5165\u503c" else "\u9084\u6c92\u9078\u503c")
    } else if (is_num) {
      nums <- suppressWarnings(as.numeric(vals))
      if (anyNA(nums)) {
        # 原文：這一個直欄是數字，請輸入數字
        out$problem <- missing_value("\u9019\u4e00\u500b\u76f4\u6b04\u662f\u6578\u5b57\uff0c\u8acb\u8f38\u5165\u6578\u5b57")
      } else {
        out$value <- unique(nums)
      }
    } else {
      out$value <- unique(as.character(vals))
    }
  } else if (op == "contains") {
    text <- cond$text %||% ""
    if (!nzchar(text)) {
      # 原文：還沒輸入要找的文字
      out$problem <- missing_value("\u9084\u6c92\u8f38\u5165\u8981\u627e\u7684\u6587\u5b57")
    } else {
      out$value <- text
    }
  }
  out
}

#' 一個數字寫成 R 程式碼（讀回來是同一個數字）
#' @noRd
r_num1 <- function(v) {
  for (d in 15:17) {
    s <- format(v, digits = d, scientific = FALSE, trim = TRUE)
    if (identical(as.numeric(s), v)) return(s)
  }
  s
}

#' 一個條件的 R 程式碼（dplyr::filter() 裡的一段；app 也用它算）
#' @param cond filter_cond_clean() 的結果（沒有 problem）
#' @noRd
filter_cond_code <- function(cond) {
  col <- r_col(cond$col)
  x <- if (identical(cond$kind, "text_other")) paste0("as.character(", col, ")") else col
  v <- cond$value
  vec <- function(v) {
    items <- if (is.numeric(v)) vapply(v, r_num1, "") else r_str_each(v)
    if (length(items) == 1) paste0("c(", items, ")") else r_wrap_c(items, "      ")
  }
  switch(cond$op,
    eq = paste(col, "==", r_num1(v)),
    ne = paste(col, "!=", r_num1(v)),
    gt = paste(col, ">", r_num1(v)),
    ge = paste(col, ">=", r_num1(v)),
    lt = paste(col, "<", r_num1(v)),
    le = paste(col, "<=", r_num1(v)),
    between = paste0("(", col, " >= ", r_num1(v[1]), " & ", col, " <= ", r_num1(v[2]), ")"),
    `in` = paste0(x, " %in% ", vec(v)),
    not_in = paste0("!", x, " %in% ", vec(v)),
    contains = paste0("grepl(", r_str(v), ", ", x, ", fixed = TRUE)"),
    is_na = paste0("is.na(", col, ")"),
    not_na = paste0("!is.na(", col, ")")
  )
}

#' 一個條件的白話說明：「lang_score 大於 50」「sex 是其中之一：1 = 男、2 = 女」
#' @param x 這一欄（有數值標籤時寫出標籤）
#' @noRd
filter_cond_text <- function(cond, x = NULL) {
  op <- FILTER_OP_LABELS[[cond$op]]
  v <- cond$value
  shown <- function(v) {
    # 原文：「 | 」
    if (!is.numeric(v)) return(paste0("\u300c", v, "\u300d"))
    text <- num_to_text(v)
    labs <- attr(x, "labels", exact = TRUE)
    if (is.null(labs)) return(text)
    hit <- match(v, unname(labs))
    ifelse(is.na(hit), text, paste0(text, " = ", names(labs)[hit]))
  }
  rest <- switch(cond$op,
    # 原文： 到 
    between = paste0(" ", shown(v[1]), " \u5230 ", shown(v[2])),
    # 原文：： | 、
    `in` = , not_in = paste0("\uff1a", paste(utils::head(shown(v), 5), collapse = "\u3001"),
                            # 原文：…（共  |  個）
                            if (length(v) > 5) paste0("\u2026\uff08\u5171 ", length(v), " \u500b\uff09")),
    is_na = , not_na = "",
    paste0(" ", shown(v))
  )
  paste0(cond$col, " ", op, rest)
}


# ── 一個檔案的設定 ───────────────────────────────────────────────────────────

#' 一個檔案的設定整理成固定的樣子
#'
#' @param raw list(keep_mode, keep_cols, conds = 畫面上的條件 list, combine,
#'   sort_cols, sort_desc)
#' @param df 這個檔案的資料
#' @return list(keep_mode, keep_cols = 實際保留的欄（全部保留時 NULL）, conds, combine,
#'   sort_cols, sort_desc, active = 有沒有任何設定, problems)
#' @noRd
filter_spec_clean <- function(raw, df) {
  all <- names(df)
  mode <- if (identical(raw$keep_mode, "exclude")) "exclude" else "include"
  picked <- intersect(raw$keep_cols %||% character(0), all)
  kept <- filter_keep_cols(all, mode, picked)
  conds <- purrr::imap(raw$conds %||% list(), function(cond, i) filter_cond_clean(cond, df, i))
  sort_cols <- intersect(raw$sort_cols %||% character(0), all)
  sort_cols <- sort_cols[!duplicated(sort_cols)]
  spec <- list(
    keep_mode = mode,
    keep_cols = if (identical(kept, all)) NULL else kept,
    drop_cols = if (mode == "exclude" && !identical(kept, all)) setdiff(all, kept),
    conds = conds,
    combine = if (identical(raw$combine, "or")) "or" else "and",
    sort_cols = sort_cols,
    sort_desc = intersect(raw$sort_desc %||% character(0), sort_cols)
  )
  spec$active <- !is.null(spec$keep_cols) || length(conds) > 0 || length(sort_cols) > 0
  spec$problems <- unlist(c(
    purrr::map(conds, "problem"),
    # 原文：每一個直欄都拿掉了，至少要保留一個直欄
    if (length(kept) == 0) "\u6bcf\u4e00\u500b\u76f4\u6b04\u90fd\u62ff\u6389\u4e86\uff0c\u81f3\u5c11\u8981\u4fdd\u7559\u4e00\u500b\u76f4\u6b04"
  ))
  spec
}

#' 實際保留的欄（依資料的欄位順序）：挑要保留的欄，或挑要拿掉的欄；沒挑 = 全部保留
#' @noRd
filter_keep_cols <- function(all, mode, picked) {
  picked <- picked %||% character(0)
  if (length(picked) == 0) return(all)
  if (identical(mode, "exclude")) setdiff(all, picked) else all[all %in% picked]
}

#' 設定摘要：「2 個條件（全部符合）· 保留 5 欄 · 依 age_m 排序」
#' @noRd
filter_spec_short <- function(spec, n_cols) {
  n <- length(spec$conds)
  c(
    if (n > 0) {
      # 原文： 個條件
      paste0(kit_num(n), " \u500b\u689d\u4ef6",
             # 原文：（任一符合） | （全部符合）
             if (n > 1) if (spec$combine == "or") "\uff08\u4efb\u4e00\u7b26\u5408\uff09" else "\uff08\u5168\u90e8\u7b26\u5408\uff09")
    },
    if (!is.null(spec$keep_cols)) {
      # 原文：保留  | ／ |  直欄
      paste0("\u4fdd\u7559 ", kit_num(length(spec$keep_cols)), "\uff0f", kit_num(n_cols), " \u76f4\u6b04")
    },
    if (length(spec$sort_cols) > 0) {
      # 原文：依  | 、
      paste0("\u4f9d ", paste(utils::head(spec$sort_cols, 3), collapse = "\u3001"),
             # 原文：… |  排序
             if (length(spec$sort_cols) > 3) "\u2026", " \u6392\u5e8f")
    }
  )
}


# ── 執行 ─────────────────────────────────────────────────────────────────────

#' 篩選一個檔案：先篩列（filter），再排序（arrange），最後選欄（select）
#'
#' 排序在選欄前面，所以可以依拿掉的欄排序；條件也可以用拿掉的欄。
#' 列與欄都用 dplyr 取，SPSS 的數值標籤、變項標籤會留著。
#' @param spec filter_spec_clean() 的結果（沒有 problems）
#' @return list(data, counts = list(rows_in, rows_out, excluded, excluded_na, cols_in,
#'   cols_out), conds = 每個條件各有幾列符合（tibble）)
#' @noRd
filter_apply <- function(df, spec) {
  n <- nrow(df)
  counts <- list(rows_in = n, rows_out = n, excluded = 0L, excluded_na = 0L,
                 cols_in = ncol(df), cols_out = ncol(df))
  cond_tbl <- NULL
  if (length(spec$conds) > 0) {
    masks <- purrr::map(spec$conds, function(cond) {
      m <- eval(str2lang(filter_cond_code(cond)), df, baseenv())
      if (length(m) == 1) m <- rep(m, n)
      as.logical(m)
    })
    combined <- Reduce(if (spec$combine == "or") `|` else `&`, masks)
    keep <- !is.na(combined) & combined
    df <- dplyr::filter(df, !!keep)
    counts$rows_out <- nrow(df)
    counts$excluded <- n - nrow(df)
    counts$excluded_na <- sum(is.na(combined))
    cond_tbl <- tibble::tibble(
      # 原文：條件
      "\u689d\u4ef6" = purrr::map2_chr(spec$conds, seq_along(spec$conds), function(cond, i) {
        paste0(i, ". ", filter_cond_text(cond, df[[cond$col]]))
      }),
      # 原文：符合
      "\u7b26\u5408" = purrr::map_int(masks, function(m) sum(m, na.rm = TRUE)),
      # 原文：不符合
      "\u4e0d\u7b26\u5408" = purrr::map_int(masks, function(m) sum(!m, na.rm = TRUE)),
      # 原文：判斷不了（空白）
      "\u5224\u65b7\u4e0d\u4e86\uff08\u7a7a\u767d\uff09" = purrr::map_int(masks, function(m) sum(is.na(m)))
    )
  }
  if (length(spec$sort_cols) > 0) {
    df <- dplyr::arrange(df, !!!filter_sort_exprs(spec, app = TRUE))
  }
  if (!is.null(spec$keep_cols)) {
    df <- dplyr::select(df, dplyr::all_of(spec$keep_cols))
    counts$cols_out <- ncol(df)
  }
  list(data = df, counts = counts, conds = cond_tbl)
}

#' 排序的運算式：desc(score)、id（app = TRUE 時寫 dplyr::desc，套件裡才找得到）
#' @noRd
filter_sort_exprs <- function(spec, app = FALSE) {
  desc <- if (app) "dplyr::desc(" else "desc("
  purrr::map(spec$sort_cols, function(col) {
    code <- if (col %in% spec$sort_desc) paste0(desc, r_col(col), ")") else r_col(col)
    str2lang(code)
  })
}

#' 篩選所有檔案：沒有設定的檔案原樣留著
#'
#' @param dfs 各檔資料（list）
#' @param specs 各檔設定（list，和 dfs 同順序；NULL 或 active = FALSE 的檔案不處理）
#' @param shown 各檔顯示名稱（「t1（w1.csv）」）
#' @return list(data, report = 各檔的列數、欄數（tibble）, counts, conds, active,
#'   warnings, points)
#' @noRd
filter_files <- function(dfs, specs, shown) {
  problems <- unlist(purrr::map2(specs, shown, function(spec, name) {
    if (is.null(spec) || !isTRUE(spec$active) || length(spec$problems) == 0) return(NULL)
    # 原文：：
    paste0(name, "\uff1a", spec$problems)
  }))
  if (length(problems) > 0) {
    # 原文：設定還沒填完： | ； | 。
    kit_abort("\u8a2d\u5b9a\u9084\u6c92\u586b\u5b8c\uff1a", paste(problems, collapse = "\uff1b"), "\u3002")
  }
  active <- purrr::map_lgl(specs, function(spec) isTRUE(spec$active))
  if (!any(active)) {
    # 原文：還沒有設定：至少替一個檔案選要保留的直欄、加一個條件，或選排序的直欄。
    kit_abort("\u9084\u6c92\u6709\u8a2d\u5b9a\uff1a\u81f3\u5c11\u66ff\u4e00\u500b\u6a94\u6848\u9078\u8981\u4fdd\u7559\u7684\u76f4\u6b04\u3001\u52a0\u4e00\u500b\u689d\u4ef6\uff0c\u6216\u9078\u6392\u5e8f\u7684\u76f4\u6b04\u3002")
  }
  out <- purrr::map2(dfs, specs, function(df, spec) {
    if (is.null(spec) || !isTRUE(spec$active)) return(NULL)
    filter_apply(df, spec)
  })
  data <- purrr::map2(dfs, out, function(df, res) if (is.null(res)) df else res$data)
  counts <- purrr::map2(dfs, out, function(df, res) {
    res$counts %||% list(rows_in = nrow(df), rows_out = nrow(df), excluded = 0L,
                         excluded_na = 0L, cols_in = ncol(df), cols_out = ncol(df))
  })
  num <- function(name) purrr::map_int(counts, function(x) as.integer(x[[name]]))
  report <- tibble::tibble(
    # 原文：檔案
    "\u6a94\u6848" = shown,
    # 原文：有設定 | 是 | 否（原樣往後傳）
    "\u6709\u8a2d\u5b9a" = ifelse(active, "\u662f", "\u5426\uff08\u539f\u6a23\u5f80\u5f8c\u50b3\uff09"),
    # 原文：原橫列數
    "\u539f\u6a6b\u5217\u6578" = num("rows_in"),
    # 原文：保留橫列數
    "\u4fdd\u7559\u6a6b\u5217\u6578" = num("rows_out"),
    # 原文：被條件排除的橫列數
    "\u88ab\u689d\u4ef6\u6392\u9664\u7684\u6a6b\u5217\u6578" = num("excluded"),
    # 原文：其中因為空白（判斷不了）而排除
    "\u5176\u4e2d\u56e0\u70ba\u7a7a\u767d\uff08\u5224\u65b7\u4e0d\u4e86\uff09\u800c\u6392\u9664" = num("excluded_na"),
    # 原文：原直欄數
    "\u539f\u76f4\u6b04\u6578" = num("cols_in"),
    # 原文：保留直欄數
    "\u4fdd\u7559\u76f4\u6b04\u6578" = num("cols_out")
  )
  points <- purrr::pmap_chr(list(shown, counts, active), function(name, x, on) {
    # 原文：：沒有設定，原樣往後傳（ | ）。
    if (!on) return(paste0(name, "\uff1a\u6c92\u6709\u8a2d\u5b9a\uff0c\u539f\u6a23\u5f80\u5f8c\u50b3\uff08", kit_dims(x$rows_in, x$cols_in), "\uff09\u3002"))
    paste0(
      # 原文：： |  橫列 → 保留  |  橫列
      name, "\uff1a", kit_num(x$rows_in), " \u6a6b\u5217 \u2192 \u4fdd\u7559 ", kit_num(x$rows_out), " \u6a6b\u5217",
      # 原文：（排除  |  橫列）
      if (x$excluded > 0) paste0("\uff08\u6392\u9664 ", kit_num(x$excluded), " \u6a6b\u5217\uff09"),
      # 原文：； |  直欄 → 保留  |  直欄。
      "\uff1b", kit_num(x$cols_in), " \u76f4\u6b04 \u2192 \u4fdd\u7559 ", kit_num(x$cols_out), " \u76f4\u6b04\u3002"
    )
  })
  warnings <- c(
    unlist(purrr::pmap(list(shown, counts, active), function(name, x, on) {
      if (!on || x$excluded_na == 0) return(NULL)
      paste0(
        # 原文：：有  |  橫列因為條件用到的直欄是空白、判斷不了，
        name, "\uff1a\u6709 ", kit_num(x$excluded_na), " \u6a6b\u5217\u56e0\u70ba\u689d\u4ef6\u7528\u5230\u7684\u76f4\u6b04\u662f\u7a7a\u767d\u3001\u5224\u65b7\u4e0d\u4e86\uff0c",
        # 原文：所以排除（已算在排除的橫列數裡）。要留下這些橫列，請加「是空白」的條件並選「任一條件符合」。
        "\u6240\u4ee5\u6392\u9664\uff08\u5df2\u7b97\u5728\u6392\u9664\u7684\u6a6b\u5217\u6578\u88e1\uff09\u3002\u8981\u7559\u4e0b\u9019\u4e9b\u6a6b\u5217\uff0c\u8acb\u52a0\u300c\u662f\u7a7a\u767d\u300d\u7684\u689d\u4ef6\u4e26\u9078\u300c\u4efb\u4e00\u689d\u4ef6\u7b26\u5408\u300d\u3002"
      )
    })),
    unlist(purrr::pmap(list(shown, counts, active), function(name, x, on) {
      if (!on || x$rows_out > 0 || x$rows_in == 0) return(NULL)
      # 原文：：篩選後沒有任何橫列。請檢查條件（例如數字是不是填錯、是不是該選「任一條件符合」）。
      paste0(name, "\uff1a\u7be9\u9078\u5f8c\u6c92\u6709\u4efb\u4f55\u6a6b\u5217\u3002\u8acb\u6aa2\u67e5\u689d\u4ef6\uff08\u4f8b\u5982\u6578\u5b57\u662f\u4e0d\u662f\u586b\u932f\u3001\u662f\u4e0d\u662f\u8a72\u9078\u300c\u4efb\u4e00\u689d\u4ef6\u7b26\u5408\u300d\uff09\u3002")
    }))
  )
  list(
    data = data, report = report, counts = counts,
    conds = purrr::map(out, "conds"), active = active,
    warnings = warnings, points = points
  )
}


# ── 程式碼 ───────────────────────────────────────────────────────────────────

#' 一個檔案的篩選程式碼：filter() → arrange() → select()
#'
#' @param spec filter_spec_clean() 的結果
#' @param obj 讀進來的物件名稱（data_1）
#' @param target 結果的物件名稱（往後傳時就是 obj，不然是 filtered_1）
#' @param name 檔案的顯示名稱（註解用）
#' @return 文字向量（每行一個元素）
#' @noRd
code_filter_one <- function(spec, obj, target, name) {
  conds <- purrr::keep(spec$conds, function(cond) is.null(cond$problem))
  codes <- purrr::map_chr(conds, filter_cond_code)
  steps <- list()
  if (length(codes) == 1) {
    steps <- c(steps, list(paste0("  filter(", codes, ")")))
  } else if (length(codes) > 1) {
    sep <- if (spec$combine == "or") " |" else ","
    body <- paste0("    ", codes, c(rep(sep, length(codes) - 1), ""))
    steps <- c(steps, list(c("  filter(", body, "  )")))
  }
  if (length(spec$sort_cols) > 0) {
    sorts <- vapply(filter_sort_exprs(spec), deparse1, "")
    steps <- c(steps, list(paste0("  arrange(", paste(sorts, collapse = ", "), ")")))
  }
  if (!is.null(spec$keep_cols)) {
    sel <- if (length(spec$drop_cols) > 0) {
      one <- length(spec$drop_cols) == 1
      paste0("!", if (one) r_col(spec$drop_cols) else r_cols_vector(spec$drop_cols))
    } else {
      r_cols_vector(spec$keep_cols)
    }
    steps <- c(steps, list(paste0("  select(", sel, ")")))
  }
  if (length(steps) == 0) return(NULL)
  # 每一步之間接上 |>
  for (i in seq_along(steps)[-length(steps)]) {
    last <- length(steps[[i]])
    steps[[i]][last] <- paste0(steps[[i]][last], " |>")
  }
  how <- if (length(codes) > 1) {
    # 原文：# 任一條件符合就留下（|） | # 全部條件都符合才留下（逗號＝而且）
    if (spec$combine == "or") "# \u4efb\u4e00\u689d\u4ef6\u7b26\u5408\u5c31\u7559\u4e0b\uff08|\uff09" else "# \u5168\u90e8\u689d\u4ef6\u90fd\u7b26\u5408\u624d\u7559\u4e0b\uff08\u9017\u865f\uff1d\u800c\u4e14\uff09"
  }
  c(
    paste0("# ", name),
    how,
    paste0(target, " <- ", obj, " |>"),
    unlist(steps),
    # 原文：)   # 篩選後的橫列數、直欄數
    paste0("dim(", target, ")   # \u7be9\u9078\u5f8c\u7684\u6a6b\u5217\u6578\u3001\u76f4\u6b04\u6578"),
    ""
  )
}

#' 篩選的程式碼（所有有設定的檔案）
#'
#' @param f code_files() 的結果
#' @param specs 各檔設定（和 f$obj 同順序）
#' @param shown 各檔顯示名稱
#' @param in_place TRUE：結果放回 data_1（後面的分頁用篩選後的資料）；FALSE：放到 filtered_1
#' @return 文字向量；沒有任何設定時 NULL
#' @noRd
code_filter <- function(f, specs, shown, in_place = TRUE) {
  idx <- which(purrr::map_lgl(specs, function(spec) isTRUE(spec$active)))
  if (length(idx) == 0) return(NULL)
  blocks <- purrr::map(idx, function(i) {
    target <- if (in_place) f$obj[i] else sub("^data_", "filtered_", f$obj[i])
    code_filter_one(specs[[i]], f$obj[i], target, shown[i])
  })
  c(
    # 原文：# 空白（NA）的處理和 filter() 一樣：條件算出來是 NA 的橫列不會留下
    "# \u7a7a\u767d\uff08NA\uff09\u7684\u8655\u7406\u548c filter() \u4e00\u6a23\uff1a\u689d\u4ef6\u7b97\u51fa\u4f86\u662f NA \u7684\u6a6b\u5217\u4e0d\u6703\u7559\u4e0b",
    unlist(blocks)
  )
}

#' 篩選完的物件名稱（摘要的程式碼接在後面用）
#' @noRd
filter_target_obj <- function(obj, in_place) {
  if (in_place) obj else sub("^data_", "filtered_", obj)
}


# ── 摘要 ─────────────────────────────────────────────────────────────────────

#' 分組欄：有數值標籤的換成「[1] 男」這樣的類別（haven::as_factor(levels = "both")）
#' @noRd
summary_group_col <- function(x) {
  if (haven::is.labelled(x)) haven::as_factor(x, levels = "both") else x
}

#' 可以摘要的欄：數字（含有數值標籤的數字）
#' @noRd
summary_numeric_cols <- function(df) {
  names(df)[purrr::map_lgl(df, is.numeric)]
}

#' 摘要前的檢查；沒問題時 NULL
#' @noRd
summary_problem <- function(df, groups, vars) {
  # 原文：請選要摘要的變項（數值直欄）。
  if (length(vars) == 0) return("\u8acb\u9078\u8981\u6458\u8981\u7684\u8b8a\u9805\uff08\u6578\u503c\u76f4\u6b04\uff09\u3002")
  miss <- setdiff(c(groups, vars), names(df))
  # 原文：資料裡沒有這些直欄： | 、 | 。
  if (length(miss) > 0) return(paste0("\u8cc7\u6599\u88e1\u6c92\u6709\u9019\u4e9b\u76f4\u6b04\uff1a", paste(miss, collapse = "\u3001"), "\u3002"))
  not_num <- vars[!purrr::map_lgl(df[vars], is.numeric)]
  if (length(not_num) > 0) {
    # 原文：這些直欄不是數字，不能算平均數： | 、 | 。
    return(paste0("\u9019\u4e9b\u76f4\u6b04\u4e0d\u662f\u6578\u5b57\uff0c\u4e0d\u80fd\u7b97\u5e73\u5747\u6578\uff1a", paste(not_num, collapse = "\u3001"), "\u3002"))
  }
  both <- intersect(groups, vars)
  if (length(both) > 0) {
    # 原文：同一個直欄不能同時是分組直欄和要摘要的變項： | 、 | 。
    return(paste0("\u540c\u4e00\u500b\u76f4\u6b04\u4e0d\u80fd\u540c\u6642\u662f\u5206\u7d44\u76f4\u6b04\u548c\u8981\u6458\u8981\u7684\u8b8a\u9805\uff1a", paste(both, collapse = "\u3001"), "\u3002"))
  }
  reserved <- intersect(groups, c("variable", "value", "n", names(SUMMARY_STATS)))
  if (length(reserved) > 0) {
    return(paste0(
      # 原文：分組直欄的名稱和摘要表的直欄名稱撞名了： | 、
      "\u5206\u7d44\u76f4\u6b04\u7684\u540d\u7a31\u548c\u6458\u8981\u8868\u7684\u76f4\u6b04\u540d\u7a31\u649e\u540d\u4e86\uff1a", paste(reserved, collapse = "\u3001"),
      # 原文：。請先改直欄名稱再摘要。
      "\u3002\u8acb\u5148\u6539\u76f4\u6b04\u540d\u7a31\u518d\u6458\u8981\u3002"
    ))
  }
  NULL
}

#' 分組摘要：一列 = 一組 × 一個變項
#'
#' 統計量都去掉空白再算（na.rm = TRUE）；整組都是空白時是 NA（不是 NaN 或 Inf）。
#' 不把資料轉成長格式（大資料轉長格式太慢）：先用 summarise(across()) 算出每組一列，
#' 再把這張小表排成每組 × 每個變項一列。列的順序是分組的順序，同一組內照變項的順序。
#' @param stats 要算的統計量（names(SUMMARY_STATS) 的一部分；n 一定會算）
#' @return tibble：分組欄…、variable、n、選的統計量
#' @noRd
summary_table <- function(df, groups = character(0), vars, stats = SUMMARY_STATS_DEFAULT) {
  problem <- summary_problem(df, groups, vars)
  if (!is.null(problem)) kit_abort(problem)
  stats <- intersect(names(SUMMARY_STATS), stats)
  keys <- paste0(".kit_v", seq_along(vars))
  work <- tibble::as_tibble(c(
    purrr::map(df[groups], summary_group_col),
    stats::setNames(purrr::map(df[vars], as.numeric), keys)
  ))
  grouped <- dplyr::group_by(work, dplyr::across(dplyr::all_of(groups)))
  n_groups <- dplyr::n_groups(grouped)
  if (n_groups * length(vars) > SUMMARY_MAX_CELLS) {
    kit_abort(
      # 原文：分組太多： |  組 ×  |  個變項。
      "\u5206\u7d44\u592a\u591a\uff1a", kit_num(n_groups), " \u7d44 \u00d7 ", kit_num(length(vars)), " \u500b\u8b8a\u9805\u3002",
      # 原文：分組直欄是不是選到 ID 這類每個人都不一樣的直欄？
      "\u5206\u7d44\u76f4\u6b04\u662f\u4e0d\u662f\u9078\u5230 ID \u9019\u985e\u6bcf\u500b\u4eba\u90fd\u4e0d\u4e00\u6a23\u7684\u76f4\u6b04\uff1f"
    )
  }
  guard <- function(f) function(x) if (any(!is.na(x))) f(x, na.rm = TRUE) else NA_real_
  fns <- list(
    n_valid = function(x) sum(!is.na(x)),
    n_missing = function(x) sum(is.na(x)),
    mean = guard(mean), sd = guard(stats::sd), median = guard(stats::median),
    min = guard(min), max = guard(max)
  )[stats]
  wide <- dplyr::summarise(
    grouped,
    n = dplyr::n(),
    dplyr::across(dplyr::all_of(keys), fns, .names = "{.col}.{.fn}"),
    .groups = "drop"
  )
  if (nrow(df) == 0) wide <- wide[0, ]
  long <- purrr::map(seq_along(vars), function(i) {
    cols <- wide[paste0(keys[i], ".", stats)]
    names(cols) <- stats
    tibble::tibble(wide[groups], variable = vars[i], n = wide$n, cols, .row = seq_len(nrow(wide)),
                   .var = i)
  }) |>
    purrr::list_rbind()
  long <- long[order(long$.row, long$.var), , drop = FALSE]
  long$.row <- NULL
  long$.var <- NULL
  long
}

#' 摘要的程式碼（tidyverse）：轉成長格式後 group_by() + summarise()
#'
#' @param obj 資料的物件名稱
#' @param target 結果的物件名稱（summary_1）
#' @param labelled_groups 有數值標籤的分組欄（先轉成「[1] 男」）
#' @return 文字向量
#' @noRd
code_summary <- function(obj, target, groups, vars, stats, labelled_groups = character(0),
                         name = NULL) {
  stats <- intersect(names(SUMMARY_STATS), stats)
  has_value <- if ("n_valid" %in% stats) "n_valid > 0" else "any(!is.na(value))"
  guarded <- function(fn) {
    paste0(fn, " = if (", has_value, ") ", fn, "(value, na.rm = TRUE) else NA_real_")
  }
  stat_lines <- c(
    "n = n()",
    if ("n_valid" %in% stats) "n_valid = sum(!is.na(value))",
    if ("n_missing" %in% stats) "n_missing = sum(is.na(value))",
    purrr::map_chr(intersect(stats, c("mean", "sd", "median", "min", "max")), guarded)
  )
  group_by <- paste(c(r_col(groups), "variable"), collapse = ", ")
  arrange_by <- paste(c(r_col(groups), "match(variable, summary_vars)"), collapse = ", ")
  select_cols <- paste(c(r_col(groups), "all_of(summary_vars)"), collapse = ", ")
  c(
    # 原文：# 摘要 | ：
    paste0("# \u6458\u8981", if (!is.null(name)) paste0("\uff1a", name),
           # 原文：，依  | 、 |  分組
           if (length(groups) > 0) paste0("\uff0c\u4f9d ", paste(groups, collapse = "\u3001"), " \u5206\u7d44")),
    # 原文：# 一個橫列 = 一組 × 一個變項；整組都是空白時是 NA
    "# \u4e00\u500b\u6a6b\u5217 = \u4e00\u7d44 \u00d7 \u4e00\u500b\u8b8a\u9805\uff1b\u6574\u7d44\u90fd\u662f\u7a7a\u767d\u6642\u662f NA",
    paste0("summary_vars <- ", r_wrap_c(r_str_each(vars), "  ")),
    paste0(target, " <- ", obj, " |>"),
    if (length(labelled_groups) > 0) {
      paste0("  mutate(across(", r_cols_vector(labelled_groups),
             # 原文：, \\(x) as_factor(x, levels = \"both\"))) |>   # 數值標籤：[1] 男
             ", \\(x) as_factor(x, levels = \"both\"))) |>   # \u6578\u503c\u6a19\u7c64\uff1a[1] \u7537")
    },
    paste0("  select(", select_cols, ") |>"),
    "  mutate(across(all_of(summary_vars), as.numeric)) |>",
    "  pivot_longer(all_of(summary_vars), names_to = \"variable\", values_to = \"value\") |>",
    paste0("  group_by(", group_by, ") |>"),
    "  summarise(",
    paste0("    ", stat_lines, ","),
    "    .groups = \"drop\"",
    "  ) |>",
    paste0("  arrange(", arrange_by, ")"),
    target
  )
}
