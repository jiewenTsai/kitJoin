# 寬轉長：欄名怎麼變成值（點選，不用寫正規表示式）
#
# 從選到的欄名找出大家都有的開頭、結尾，讓使用者點選要不要去掉；也可以把欄名切成幾段。
# 點好的設定由 app 寫成 tidyr 的參數：
#   只去掉開頭                → names_prefix（字面跳脫）
#   只用一種符號切（沒去結尾） → names_prefix + names_sep（字面跳脫）
#   其他（去結尾、文字和數字分開） → names_pattern（app 寫的、已跳脫）
# 使用者從頭到尾不用打任何規則。
#
# 介面（都是純函式，模組在 mod_pivot_longer.R）：
#   pivot_name_starts(names) / pivot_name_ends(names) → 可以去掉的開頭 / 結尾（長的在前）
#   pivot_name_middles(names, start, end)             → 去掉開頭、結尾後剩下的字
#   pivot_name_split_choices(middles)                 → 可以用的切法
#   pivot_name_split(middles, split)                  → 每個欄名切成的段
#   pivot_names_args(start, end, split, parts, part_names, as_number) → tidyr 參數
#   pivot_names_check(cols, args)                     → 預覽表、重複的值

# 可以用來切欄名的符號
PIVOT_NAME_SEPS <- c("_", ".", "-", " ")

# 切法的代號：分隔符號本身，或 "digits"（文字和數字分開）
PIVOT_NAME_DIGITS <- "digits"

#' 正規表示式的特殊字前面加反斜線（字面比對用）
#'
#' 結果在 tidyr 的 names_prefix（base R 的 gsub()）和 names_pattern、names_sep
#' （perl = TRUE）裡都是字面意思。
#' @noRd
regex_escape <- function(x) {
  gsub("([.\\\\|()\\[\\]{}^$*+?])", "\\\\\\1", x, perl = TRUE)
}

# 一個字的種類：d 數字、s 分隔符號、u 大寫英文、l 小寫英文、o 其他
pivot_char_kind <- function(ch) {
  dplyr::case_when(
    grepl("^[0-9]$", ch) ~ "d",
    ch %in% PIVOT_NAME_SEPS ~ "s",
    grepl("^[A-Z]$", ch) ~ "u",
    grepl("^[a-z]$", ch) ~ "l",
    TRUE ~ "o"
  )
}

#' 欄名第 pos 個字和第 pos + 1 個字之間，是不是好的切點
#'
#' 好的切點：旁邊是分隔符號（_ . - 空白）、數字和非數字的交界、小寫接大寫（scoreT1）。
#' 兩邊都要有字（不能把整個欄名切掉）。
#' @noRd
pivot_name_cut_ok <- function(name, pos) {
  if (pos < 1 || pos >= nchar(name)) return(FALSE)
  a <- pivot_char_kind(substr(name, pos, pos))
  b <- pivot_char_kind(substr(name, pos + 1, pos + 1))
  a == "s" || b == "s" || ((a == "d") != (b == "d")) || (a == "l" && b == "u")
}

# 每個欄名都有的開頭（逐字比對，分大小寫）
pivot_common_start <- function(names) {
  if (length(names) == 0) return("")
  first <- names[1]
  n <- min(nchar(names))
  k <- 0
  while (k < n && all(substr(names, k + 1, k + 1) == substr(first, k + 1, k + 1))) k <- k + 1
  substr(first, 1, k)
}

# 每個欄名都有的結尾
pivot_common_end <- function(names) {
  rev_str <- function(x) vapply(strsplit(x, ""), function(ch) paste(rev(ch), collapse = ""), "")
  if (length(names) == 0) return("")
  rev_str(pivot_common_start(rev_str(names)))
}

#' 可以去掉的開頭：每個欄名都有、切在好的切點上，長的在前
#'
#' 例如 score_t1_raw、score_t2_raw → score_t、score_、score。分大小寫（tidyr 也分）。
#' @param names 要轉的欄名
#' @param max 最多幾個
#' @return 字串向量（沒有時 character(0)）
#' @noRd
pivot_name_starts <- function(names, max = 6L) {
  common <- pivot_common_start(names)
  ks <- rev(seq_len(nchar(common)))
  ok <- purrr::map_lgl(ks, function(k) all(purrr::map_lgl(names, pivot_name_cut_ok, pos = k)))
  utils::head(substr(rep(common, sum(ok)), 1, ks[ok]), max)
}

#' 可以去掉的結尾：每個欄名都有、切在好的切點上，長的在前（例如 _raw、raw）
#' @noRd
pivot_name_ends <- function(names, max = 6L) {
  common <- pivot_common_end(names)
  n <- nchar(common)
  ks <- rev(seq_len(n))
  ok <- purrr::map_lgl(ks, function(k) {
    all(purrr::map_lgl(names, function(x) pivot_name_cut_ok(x, nchar(x) - k)))
  })
  utils::head(substr(rep(common, sum(ok)), n - ks[ok] + 1, n), max)
}

#' 去掉開頭、結尾後剩下的字
#' @return list(middles, problem)；problem 是給使用者看的文字（沒問題時 NULL）
#' @noRd
pivot_name_middles <- function(names, start = "", end = "") {
  start <- start %||% ""
  end <- end %||% ""
  ok <- startsWith(names, start) & endsWith(names, end) &
    nchar(names) > nchar(start) + nchar(end)
  middles <- substr(names, nchar(start) + 1, nchar(names) - nchar(end))
  problem <- if (!all(ok)) {
    paste0(
      # 原文：去掉開頭和結尾以後，這些欄名沒有剩下字： | 。
      "\u53bb\u6389\u958b\u982d\u548c\u7d50\u5c3e\u4ee5\u5f8c\uff0c\u9019\u4e9b\u6b04\u540d\u6c92\u6709\u5269\u4e0b\u5b57\uff1a", kit_name_list(names[!ok]), "\u3002",
      # 原文：請少去掉一點。
      "\u8acb\u5c11\u53bb\u6389\u4e00\u9ede\u3002"
    )
  }
  list(middles = middles, problem = problem)
}

#' 可以用的切法：每個欄名（去掉開頭、結尾後）都有的分隔符號；
#' 每個都有數字也有別的字時，加上「文字和數字分開」
#' @return 切法代號的向量（分隔符號或 "digits"）
#' @noRd
pivot_name_split_choices <- function(middles) {
  if (length(middles) == 0) return(character(0))
  seps <- PIVOT_NAME_SEPS[purrr::map_lgl(PIVOT_NAME_SEPS, function(s) {
    all(grepl(s, middles, fixed = TRUE))
  })]
  digits <- all(grepl("[0-9]", middles) & grepl("[^0-9]", middles))
  c(seps, if (digits) PIVOT_NAME_DIGITS)
}

# 一個欄名切成幾段（保留空的段，和 tidyr 的 names_sep 一樣）
pivot_split_one <- function(x, split) {
  if (identical(split, PIVOT_NAME_DIGITS)) {
    return(regmatches(x, gregexpr("[0-9]+|[^0-9]+", x))[[1]])
  }
  strsplit(paste0(x, split), split, fixed = TRUE)[[1]]
}

#' 每個欄名切成的段
#'
#' 每個欄名要切成一樣多段；文字和數字分開時，每段是數字還是文字也要一樣
#' （lan12 和 12lan 不一樣）。不一樣時 problem 列出各種切法的例子。
#' @param middles 去掉開頭、結尾後的欄名
#' @param split 切法代號
#' @return list(parts（每個欄名一個字串向量）, n（段數；不一樣時 NA）,
#'   digit（每段是不是數字）, numeric（每段是不是都是不太長的整數）, problem)
#' @noRd
pivot_name_split <- function(middles, split) {
  parts <- purrr::map(middles, pivot_split_one, split = split)
  shape <- purrr::map_chr(parts, function(p) {
    if (identical(split, PIVOT_NAME_DIGITS)) {
      # 原文：數字 | 文字 | ＋
      paste(ifelse(grepl("^[0-9]", p), "\u6578\u5b57", "\u6587\u5b57"), collapse = "\uff0b")
    } else {
      # 原文：段
      paste(length(p), "\u6bb5")
    }
  })
  out <- list(parts = parts, n = NA_integer_, digit = logical(0), numeric = logical(0),
              problem = NULL)
  if (length(unique(shape)) > 1) {
    first <- middles[match(unique(shape), shape)]
    # 原文： → 
    examples <- paste0(utils::head(first, 4), " \u2192 ", utils::head(unique(shape), 4))
    out$problem <- paste0(
      # 原文：這些欄名切出來的段數不一樣： | ； | 。
      "\u9019\u4e9b\u6b04\u540d\u5207\u51fa\u4f86\u7684\u6bb5\u6578\u4e0d\u4e00\u6a23\uff1a", paste(examples, collapse = "\uff1b"), "\u3002",
      # 原文：每個欄名要切成一樣多段。請換一種切法，或只選同一種樣子的欄。
      "\u6bcf\u500b\u6b04\u540d\u8981\u5207\u6210\u4e00\u6a23\u591a\u6bb5\u3002\u8acb\u63db\u4e00\u7a2e\u5207\u6cd5\uff0c\u6216\u53ea\u9078\u540c\u4e00\u7a2e\u6a23\u5b50\u7684\u6b04\u3002"
    )
    return(out)
  }
  n <- length(parts[[1]])
  by_part <- purrr::map(seq_len(n), function(i) purrr::map_chr(parts, i))
  out$n <- n
  out$digit <- purrr::map_lgl(by_part, function(v) all(grepl("^[0-9]+$", v)))
  out$numeric <- purrr::map_lgl(by_part, function(v) all(grepl("^[0-9]{1,9}$", v)))
  out
}

#' 每一段預設的欄名：文字的段叫 name、數字的段叫 number（同一種有好幾段時加編號）
#' @noRd
pivot_part_default_names <- function(digit) {
  base <- ifelse(digit, "number", "name")
  purrr::map_chr(seq_along(base), function(i) {
    same <- which(base == base[i])
    if (length(same) > 1) paste0(base[i], match(i, same)) else base[i]
  })
}

#' 點好的設定 → tidyr 的參數（app 寫的規則，特殊字都跳脫）
#'
#' @param start,end 要去掉的開頭、結尾（"" = 不去掉）
#' @param split 切法代號（"" = 不切）
#' @param split_info pivot_name_split() 的結果（不切時 NULL）
#' @param names_to 欄名放到哪一欄（不切時一個；切的時候每段一個）
#' @param to_integer 和 names_to 一樣長的 TRUE/FALSE：哪幾欄轉成整數（names_transform）
#' @return list(names_to, names_prefix, names_sep, names_pattern, names_transform)
#' @noRd
pivot_names_args <- function(start = "", end = "", split = "", split_info = NULL,
                             names_to = "name", to_integer = FALSE) {
  start <- start %||% ""
  end <- end %||% ""
  split <- split %||% ""
  out <- list(names_to = names_to, names_prefix = NULL, names_sep = NULL,
              names_pattern = NULL, names_transform = NULL)
  is_sep <- split %in% PIVOT_NAME_SEPS
  if (!nzchar(end) && (!nzchar(split) || is_sep)) {
    if (nzchar(start)) out$names_prefix <- regex_escape(start)
    if (is_sep) out$names_sep <- regex_escape(split)
  } else {
    groups <- if (!nzchar(split)) {
      "(.*)"
    } else if (is_sep) {
      # [] 裡面這四個符號都是字面意思，不用跳脫
      paste(rep(paste0("([^", split, "]*)"), split_info$n), collapse = regex_escape(split))
    } else {
      paste(ifelse(split_info$digit, "([0-9]+)", "([^0-9]+)"), collapse = "")
    }
    out$names_pattern <- paste0("^", regex_escape(start), groups, regex_escape(end), "$")
  }
  to_integer <- rep_len(to_integer %in% TRUE, length(names_to))
  if (any(to_integer)) {
    keep <- names_to[to_integer]
    out$names_transform <- stats::setNames(rep(list(as.integer), length(keep)), keep)
  }
  out
}

#' 不切的時候，去掉開頭、結尾後是不是都是數字（可以勾「轉成數字」）
#' @noRd
pivot_middles_numeric <- function(middles) {
  length(middles) > 0 && all(grepl("^[0-9]{1,9}$", middles))
}

#' 依參數算出每個欄名會變成什麼（用 tidyr 自己算，和實際轉換完全一樣）
#'
#' @param cols 要轉的欄名
#' @param args 含 names_to、names_prefix、names_sep、names_pattern、names_transform
#' @return list(preview = tibble（原本的欄名、names_to 各欄）, dup = 重複的欄名,
#'   unmatched = 規則對不上的欄名, problem)
#' @noRd
pivot_names_check <- function(cols, args) {
  empty <- tibble::as_tibble(
    stats::setNames(rep(list(logical(0)), length(cols)), cols), .name_repair = "minimal"
  )
  spec <- tryCatch(
    suppressWarnings(tidyr::build_longer_spec(
      empty, cols = dplyr::all_of(cols), names_to = args$names_to,
      names_prefix = args$names_prefix, names_sep = args$names_sep,
      names_pattern = args$names_pattern, names_transform = args$names_transform
    )),
    error = function(e) e
  )
  if (inherits(spec, "error")) {
    return(list(preview = NULL, dup = character(0), unmatched = character(0),
                # 原文：欄名的規則有問題：
                problem = paste0("\u6b04\u540d\u7684\u898f\u5247\u6709\u554f\u984c\uff1a", conditionMessage(spec))))
  }
  values <- spec[args$names_to]
  # 原本的欄名
  # 原文：原本的欄名
  preview <- dplyr::bind_cols(tibble::tibble("\u539f\u672c\u7684\u6b04\u540d" = spec$.name),
                              dplyr::mutate(values, dplyr::across(dplyr::everything(),
                                                                  as.character)))
  unmatched <- spec$.name[!stats::complete.cases(values)]
  dup_rows <- duplicated(values) | duplicated(values, fromLast = TRUE)
  dup <- spec$.name[dup_rows]
  problem <- if (length(dup) > 0) {
    paste0(
      # 原文：這些欄會變成一樣的值： | 。
      "\u9019\u4e9b\u6b04\u6703\u8b8a\u6210\u4e00\u6a23\u7684\u503c\uff1a", kit_name_list(dup), "\u3002",
      # 原文：一樣的值分不出原本是哪一欄，所以先停下來。
      "\u4e00\u6a23\u7684\u503c\u5206\u4e0d\u51fa\u539f\u672c\u662f\u54ea\u4e00\u6b04\uff0c\u6240\u4ee5\u5148\u505c\u4e0b\u4f86\u3002",
      # 原文：請少去掉一點，或多切一段。
      "\u8acb\u5c11\u53bb\u6389\u4e00\u9ede\uff0c\u6216\u591a\u5207\u4e00\u6bb5\u3002"
    )
  }
  list(preview = preview, dup = dup, unmatched = unmatched, problem = problem)
}
