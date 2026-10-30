# ⑥ 轉成 kits 作答檔：寬格式（一列一個孩子的一個波次、一題一欄）→ kits 的長格式
#
# 輸出固定 9 欄、固定順序（kits 與 kitsUI 的資料約定）：
#   visit_id、subj_id、cohort、sex、wave_m、age_m、item_id、item_score、domain
# 編號與 sex 一律寫成文字；wave_m、age_m 是數字；item_score 是整數。
# 進階調整依固定順序執行：1 分數減 1 → 2 反向計分（max − x）。遺漏值代碼由 ② 處理。
# 這個檔案只有純函式（不碰 Shiny），由 mod_kitsdata.R 呼叫。

# kits 作答檔的欄位（和 kits::kitm03$responses 相同的名稱、順序與型別；不依賴 kits）
# type：c 文字、d 數字、i 整數；required：空白時不能轉換；
# from：這一頁哪裡給值（map = 「2. 對應欄位」的一列）
KITS_COLUMNS <- tibble::tribble(
  ~column, ~type, ~required, ~from, ~meaning,
  "visit_id", "c", TRUE, "map", "列次編號：一個孩子的一個波次一個編號。兩個世代不能重複。",
  "subj_id", "c", FALSE, "map", "孩子編號：同一個孩子的各波次相同。",
  "cohort", "c", TRUE, "cohort", "世代，例如 M03、M36（「1. 世代」）。",
  "sex", "c", FALSE, "map", "性別，例如 boy、girl。",
  "wave_m", "d", TRUE, "map", "波次（施測月齡），例如 3、6、12。",
  "age_m", "d", FALSE, "map", "實際月齡。題目表的通過年齡要用它。",
  "item_id", "c", TRUE, "items", "題號，例如 lana01（題目欄的欄名）。",
  "item_score", "i", TRUE, "items", "得分：0、1、2…（題目欄的值；沒有作答的題不放）。",
  "domain", "c", TRUE, "items", "領域，例如 lan、motor、cog（「3. 題目與領域」）。"
)

# 「2. 對應欄位」的列：kits 的欄（from = map）加上波次標籤（只用來組 visit_id）
KITS_MAP_FIELDS <- c(
  stats::setNames(
    KITS_COLUMNS$meaning[KITS_COLUMNS$from == "map"],
    KITS_COLUMNS$column[KITS_COLUMNS$from == "map"]
  ),
  wave = "波次標籤，例如 m108g03（不會輸出，只用來組 visit_id）。"
)[c("visit_id", "subj_id", "sex", "wave", "wave_m", "age_m")]

#' @importFrom dplyr .data
NULL

# 外部連結的錨題只認這三個領域名稱
KITS_ANCHOR_DOMAINS <- c("lan", "motor", "cog")

# 這個值以上的分數當成疑似遺漏值代碼（例如 7777、9999），提醒先到 ② 處理
KITS_CODE_MIN <- 7777

# kitsUI 單檔上傳上限（MB，未壓縮的估計超過時提醒）
KITSUI_UPLOAD_MB <- 200


# ── 對應欄位 ─────────────────────────────────────────────────────────────────

#' 依欄名猜對應的欄
#'
#' @param cols 各檔欄位的聯集
#' @return 具名 list（names(KITS_MAP_FIELDS)）：欄名，猜不到時為 ""；
#'   visit_id 只在有同名欄時才猜（否則自動組成）
#' @noRd
kits_guess_map <- function(cols) {
  first <- function(pattern) {
    hit <- cols[grepl(pattern, cols, ignore.case = TRUE)]
    if (length(hit) == 0) "" else hit[[1]]
  }
  ids <- setdiff(guess_id_cols(cols), cols[tolower(cols) == "visit_id"])
  subj <- intersect(c("subj_id", "release_id"), cols)
  list(
    visit_id = first("^visit_id$"),
    subj_id = if (length(subj) > 0) subj[[1]] else if (length(ids) > 0) ids[[1]] else "",
    sex = first("^(sex|gender|性別)$"),
    wave = first("^wave(_label)?$"),
    wave_m = first("^wave_m$"),
    age_m = first("^(age_m|age)$")
  )
}

#' 已對應的欄（不會被當成題目）
#' @noRd
kits_mapped_cols <- function(map) {
  vals <- unlist(map, use.names = FALSE)
  unique(vals[!is.na(vals) & nzchar(vals)])
}


# ── 領域：題目表與規則 ───────────────────────────────────────────────────────

#' 題目表（item_id、domain）整理與檢查
#'
#' 欄名不分大小寫；同一題有兩個不同領域時停下來。
#' @return tibble：item_id、domain（文字，去掉前後空白）
#' @noRd
kits_item_table <- function(df) {
  names(df) <- tolower(trimws(names(df)))
  miss <- setdiff(c("item_id", "domain"), names(df))
  if (length(miss) > 0) {
    kit_abort(
      "題目表需要 item_id 與 domain 兩欄，缺少：", paste(miss, collapse = "、"), "。",
      "請用一列一題的表，例如 item_id = lana01、domain = lan。"
    )
  }
  tab <- tibble::tibble(
    item_id = trimws(as.character(unclass(df$item_id))),
    domain = trimws(as.character(unclass(df$domain)))
  ) |>
    dplyr::filter(!is.na(.data$item_id), nzchar(.data$item_id)) |>
    dplyr::distinct()
  dup <- unique(tab$item_id[duplicated(tab$item_id)])
  if (length(dup) > 0) {
    kit_abort(
      "題目表裡同一題有不同的領域：", paste(utils::head(dup, 5), collapse = "、"),
      "。每一題只能有一個領域。"
    )
  }
  tab
}

#' 解析規則文字：一行一條「規則 = 領域」，規則是正規表示式（例如 ^lan = lan）
#'
#' @return list(rules = tibble(pattern, domain), invalid = 看不懂的行)
#' @noRd
kits_parse_rules <- function(text) {
  lines <- trimws(strsplit(text %||% "", "\n", fixed = TRUE)[[1]])
  lines <- lines[nzchar(lines) & !startsWith(lines, "#")]
  parts <- strsplit(lines, "\\s*(=|→)\\s*")
  ok <- lengths(parts) == 2 & vapply(parts, function(p) all(nzchar(p)), logical(1))
  valid <- vapply(parts, function(p) {
    length(p) == 2 && !inherits(tryCatch(grepl(p[1], "x"), error = identity), "error")
  }, logical(1))
  keep <- ok & valid
  list(
    rules = tibble::tibble(
      pattern = vapply(parts[keep], `[`, character(1), 1),
      domain = vapply(parts[keep], `[`, character(1), 2)
    ),
    invalid = lines[!keep]
  )
}

#' 每一欄的領域：先查題目表（結尾 r 的題查不到時改查原題），再用規則
#'
#' @param cols 候選欄
#' @param table NULL 或 kits_item_table()
#' @param rules NULL 或 kits_parse_rules()$rules
#' @return tibble：column、domain（找不到時 NA）、source（給使用者看的來源）
#' @noRd
kits_item_domains <- function(cols, table = NULL, rules = NULL) {
  cols <- as.character(cols)
  domain <- rep(NA_character_, length(cols))
  source <- rep(NA_character_, length(cols))
  if (!is.null(table) && nrow(table) > 0) {
    hit <- match(cols, table$item_id)
    domain <- table$domain[hit]
    source[!is.na(hit)] <- "題目表"
    base <- sub("r$", "", cols)
    use_base <- is.na(domain) & grepl("r$", cols) & base %in% table$item_id
    domain[use_base] <- table$domain[match(base[use_base], table$item_id)]
    source[use_base] <- paste0("題目表（原題 ", base[use_base], "）")
  }
  for (i in seq_len(if (is.null(rules)) 0 else nrow(rules))) {
    todo <- is.na(domain) & grepl(rules$pattern[i], cols)
    domain[todo] <- rules$domain[i]
    source[todo] <- paste0("規則 ", rules$pattern[i])
  }
  tibble::tibble(column = cols, domain = domain, source = source)
}

#' 候選題目欄：各檔欄位聯集，去掉已對應的欄與世代欄
#' @return list(numeric = 數值欄, text = 文字欄（不能當題目）)
#' @noRd
kits_candidate_cols <- function(dfs, mapped) {
  cols <- setdiff(as.character(unique(unlist(purrr::map(dfs, names)))), mapped)
  is_num <- vapply(cols, function(col) {
    all(purrr::map_lgl(dfs, function(df) {
      !col %in% names(df) || is.numeric(df[[col]]) || all(is.na(df[[col]]))
    }))
  }, logical(1))
  list(numeric = cols[is_num], text = cols[!is_num])
}


# ── 作答分數 ─────────────────────────────────────────────────────────────────

#' 每一題觀察到的最小、最大分數（所有檔案合起來）
#' @param below 只看小於這個值的分數（例如 7777：先不管疑似遺漏值代碼）
#' @return tibble：item_id、min、max、n（有作答的格數）
#' @noRd
kits_item_ranges <- function(dfs, items, below = Inf) {
  purrr::map(dfs, function(df) {
    have <- intersect(items, names(df))
    purrr::map(have, function(col) {
      x <- as.numeric(unclass(df[[col]]))
      x <- x[!is.na(x) & x < below]
      tibble::tibble(item_id = col, min = suppressWarnings(min(x)),
                     max = suppressWarnings(max(x)), n = length(x))
    }) |>
      purrr::list_rbind()
  }) |>
    purrr::list_rbind() |>
    dplyr::summarise(
      min = suppressWarnings(min(.data$min)), max = suppressWarnings(max(.data$max)),
      n = sum(.data$n), .by = "item_id"
    )
}

#' 疑似遺漏值代碼（KITS_CODE_MIN 以上）的格數
#' @param cols 要看的欄（題目、wave_m、age_m）
#' @noRd
kits_code_cells <- function(dfs, cols) {
  sum(purrr::map_dbl(dfs, function(df) {
    have <- intersect(cols, names(df))
    sum(purrr::map_dbl(df[have], function(x) {
      if (!is.numeric(x)) return(0)
      sum(as.numeric(unclass(x)) >= KITS_CODE_MIN, na.rm = TRUE)
    }))
  }))
}

#' 步驟 1 與 2：分數減 1、反向計分（max − x）
#' @noRd
kits_step_scores <- function(df, cols, fun) {
  for (col in intersect(cols, names(df))) {
    df[[col]] <- fun(as.numeric(unclass(df[[col]])))
  }
  df
}


# ── 設定的檢查（執行前） ─────────────────────────────────────────────────────

#' 執行前就能看出來的問題（有問題就不轉換）
#'
#' @param spec kits_spec()
#' @return 中文訊息；沒問題時長度 0
#' @noRd
kits_spec_problems <- function(dfs, file_names, spec) {
  out <- character(0)
  if (length(dfs) == 0) return("請先到「① 上傳檔案」加入檔案。")
  for (i in seq_along(dfs)) {
    co <- spec$cohort[[i]]
    if (is.null(co) || !nzchar(co$value %||% "")) {
      out <- c(out, paste0(file_names[i], "：還沒有填世代（例如 M03）。"))
    } else if (identical(co$type, "column") && !co$value %in% names(dfs[[i]])) {
      out <- c(out, paste0(file_names[i], "：沒有世代欄 ", co$value, "。"))
    }
  }
  map <- spec$map
  if (!nzchar(map$wave_m %||% "")) out <- c(out, "請對應 wave_m（波次，月）。kitsUI 一定要這一欄。")
  if (!nzchar(map$visit_id %||% "") && !nzchar(map$subj_id %||% "")) {
    out <- c(out, "請對應 subj_id（孩子編號）：visit_id 要用它組成。")
  }
  for (i in seq_along(dfs)) {
    miss <- setdiff(kits_mapped_cols(map), names(dfs[[i]]))
    if (length(miss) > 0) {
      out <- c(out, paste0(file_names[i], "：沒有對應的欄 ", paste(miss, collapse = "、"), "。"))
    }
  }
  if (length(spec$undecided) > 0) {
    out <- c(out, paste0(
      "還有 ", length(spec$undecided), " 欄沒有領域、也還沒決定要不要排除：",
      paste(utils::head(spec$undecided, 8), collapse = "、"),
      if (length(spec$undecided) > 8) "…" else "", "。"
    ))
  }
  if (nrow(spec$items) == 0) out <- c(out, "沒有題目欄。請上傳題目表、輸入規則，或替欄位指定領域。")
  out <- c(out, kits_step_problems(dfs, spec))
  out
}

# 進階調整的設定問題
kits_step_problems <- function(dfs, spec) {
  out <- character(0)
  st <- spec$steps
  if (!is.null(st$shift) && length(st$shift) == 0) {
    out <- c(out, "步驟 1：請選要減 1 的題目。")
  }
  if (!is.null(st$reverse)) {
    if (length(st$reverse$items) == 0) out <- c(out, "步驟 2：請選要反向計分的題目。")
    if (is.null(st$reverse$max) || is.na(st$reverse$max)) {
      out <- c(out, "步驟 2：請填最高分（0 起算，例如 0–4 計分填 4）。")
    }
  }
  out
}


# ── 轉換 ─────────────────────────────────────────────────────────────────────

#' 轉換的設定
#'
#' @param cohort list（每個檔案一個）：list(type = "text" 或 "column", value)
#' @param map 對應欄位（kits_guess_map() 的形狀）
#' @param items tibble：column、domain（要轉的題目；領域已決定）
#' @param undecided 沒有領域也還沒決定的欄
#' @param steps list(na = 代碼或 NULL, shift = 題目或 NULL,
#'   reverse = NULL 或 list(items, max, add_r))；NULL 表示沒有勾
#' @noRd
kits_spec <- function(cohort, map, items, undecided = character(0), steps = list()) {
  list(cohort = cohort, map = map, items = items, undecided = undecided, steps = steps)
}

#' 寬格式 → kits 長格式：準備與檢查（不做出整張長表）
#'
#' 進階調整與所有檢查都在寬資料上做（寬資料本來就在記憶體裡，比長格式小很多）；
#' 長格式只在下載時一段一段做出來、寫進 .csv.gz（kits_write_long()），所以幾百萬列也不會
#' 一次佔用大量記憶體。
#'
#' @param dfs 各檔資料
#' @param file_names 檔名（給訊息用）
#' @param spec kits_spec()
#' @param pivot_args NULL 或 mod_pivot_longer_server() 的參數（names_to 等已固定）
#' @return list(prep, problems, warnings, notes, summary, preview, n_rows, n_visits, n_items,
#'   csv_mb, visit_rule)；problems 不是空的時候 prep 是 NULL
#' @noRd
kits_build <- function(dfs, file_names, spec, pivot_args = NULL) {
  prep <- kits_prepare(dfs, file_names, spec, pivot_args)
  if (length(prep$problems) > 0) return(list(prep = NULL, problems = prep$problems))
  checks <- kits_wide_checks(prep, spec)
  ok <- length(checks$problems) == 0
  head_long <- kits_long_head(prep, 2000)
  n_rows <- sum(purrr::map_dbl(prep$files, function(f) sum(f$n_resp)))
  csv_mb <- kits_csv_mb(head_long, n_rows)
  warnings <- checks$warnings
  if (csv_mb > KITSUI_UPLOAD_MB) {
    warnings <- c(warnings, sprintf(
      "未壓縮的 CSV 約 %.0f MB，超過 kitsUI 單檔上限 %d MB；請下載 .csv.gz（壓縮檔）。",
      csv_mb, KITSUI_UPLOAD_MB
    ))
  }
  list(
    prep = if (ok) prep, problems = checks$problems, warnings = warnings, notes = prep$notes,
    summary = if (ok) kits_summary(prep), preview = utils::head(head_long, KIT_PREVIEW_ROWS * 2),
    n_rows = n_rows, n_visits = checks$n_visits, n_items = checks$n_items,
    csv_mb = csv_mb, visit_rule = prep$visit_rule
  )
}

#' 進階調整（固定順序 1 → 2 → 3）與每個檔案的孩子欄、題目欄
#'
#' @return list(files, rename, domain_of, pivot_args, notes, visit_rule, problems)；
#'   files 每個檔案一個 list(persons, scores, have, n_resp)：persons 是每一列的孩子欄
#'   （含 visit_id），scores 是題目欄（數字），n_resp 是每一列有作答的題數
#' @noRd
kits_prepare <- function(dfs, file_names, spec, pivot_args = NULL) {
  problems <- kits_spec_problems(dfs, file_names, spec)
  if (length(problems) > 0) return(list(problems = problems))
  items <- spec$items$column
  map <- spec$map
  notes <- character(0)

  problems <- kits_score_problems(dfs, items)
  if (length(problems) > 0) return(list(problems = problems))

  # 步驟 1：分數減 1（已經有 0 分的題不能減）
  shift <- spec$steps$shift
  if (!is.null(shift)) {
    zero <- dplyr::filter(kits_item_ranges(dfs, shift), .data$min <= 0)
    if (nrow(zero) > 0) {
      return(list(problems = paste0(
        "步驟 1：這些題已經有 0 分，減 1 會變成負分：",
        paste(utils::head(zero$item_id, 10), collapse = "、"),
        if (nrow(zero) > 10) "…" else "", "。請把它們從要減 1 的題目拿掉。"
      )))
    }
    dfs <- purrr::map(dfs, kits_step_scores, cols = shift, fun = function(x) x - 1)
    notes <- c(notes, paste0("步驟 1：", length(shift), " 題的分數減 1。"))
  }

  # 步驟 2：反向計分（0 起算的量尺上 max − x）
  rev <- spec$steps$reverse
  rename <- character(0)
  if (!is.null(rev)) {
    over <- dplyr::filter(kits_item_ranges(dfs, rev$items), .data$max > rev$max)
    if (nrow(over) > 0) {
      return(list(problems = paste0(
        "步驟 2：這些題的分數比最高分 ", rev$max, " 還大：",
        paste(utils::head(over$item_id, 10), collapse = "、"),
        "。請確認最高分（0 起算）。"
      )))
    }
    dfs <- purrr::map(dfs, kits_step_scores, cols = rev$items, fun = function(x) rev$max - x)
    notes <- c(notes, paste0("步驟 2：", length(rev$items), " 題反向計分（", rev$max, " − 分數）。"))
    if (isTRUE(rev$add_r)) {
      todo <- rev$items[!grepl("r$", rev$items)]
      rename <- stats::setNames(paste0(todo, "r"), todo)
    }
  }

  visit_rule <- kits_visit_rule(map)
  files <- purrr::map(seq_along(dfs), function(i) {
    kits_prepare_file(dfs[[i]], spec$cohort[[i]], map, items)
  })
  out_ids <- kits_out_ids(items, rename)
  list(
    files = files, rename = rename,
    domain_of = stats::setNames(spec$items$domain, out_ids),
    pivot_args = pivot_args, notes = c(visit_rule$note, notes), visit_rule = visit_rule,
    problems = character(0)
  )
}

# 輸出的題號（反向計分加 r 之後）
kits_out_ids <- function(ids, rename) {
  if (length(rename) == 0) return(ids)
  dplyr::coalesce(unname(rename[ids]), ids)
}

# 一個檔案：每一列的孩子欄（含 visit_id）、題目欄、每一列有作答的題數
kits_prepare_file <- function(df, cohort, map, items) {
  n <- nrow(df)
  pick <- function(field) {
    col <- map[[field]] %||% ""
    if (nzchar(col)) df[[col]] else rep(NA, n)
  }
  as_text <- function(x) as.character(unclass(x))
  co <- if (identical(cohort$type, "column")) as_text(df[[cohort$value]]) else
    rep(cohort$value, n)
  persons <- tibble::tibble(
    subj_id = as_text(pick("subj_id")), cohort = co, sex = as_text(pick("sex")),
    wave_m = as.numeric(unclass(pick("wave_m"))), age_m = as.numeric(unclass(pick("age_m")))
  )
  persons$visit_id <- kits_visit_ids(persons, df, map)
  have <- intersect(items, names(df))
  scores <- purrr::map(df[have], function(x) as.numeric(unclass(x)))
  n_resp <- Reduce(`+`, lapply(scores, function(x) !is.na(x)), integer(n))
  list(persons = persons, scores = scores, have = have, n_resp = as.integer(n_resp))
}

#' 一個檔案的幾列寬資料 → 長格式（固定 9 欄）
#'
#' 和整張表一次轉換的結果相同：一樣用 pivot_longer_run()，列的順序是寬資料的列、
#' 每一列裡照題目欄的順序。
#' @param rows 寬資料的列（沒有作答的列會略過）
#' @noRd
kits_long_chunk <- function(prep, i, rows) {
  f <- prep$files[[i]]
  rows <- rows[f$n_resp[rows] > 0]
  if (length(rows) == 0 || length(f$have) == 0) return(NULL)
  wide <- dplyr::bind_cols(
    f$persons[rows, ],
    tibble::as_tibble(lapply(f$scores, function(x) x[rows]))
  )
  args <- utils::modifyList(
    prep$pivot_args %||% list(),
    list(cols = f$have, names_to = "item_id", values_to = "item_score", values_drop_na = TRUE)
  )
  long <- pivot_longer_run(wide, args)
  long$item_id <- kits_out_ids(long$item_id, prep$rename)
  long$domain <- unname(prep$domain_of[long$item_id])
  kits_finish_columns(long)
}

# 每一段的列：1:n 切成每段 chunk_rows 列
kits_chunks <- function(n, chunk_rows) {
  if (n == 0) return(list())
  split(seq_len(n), ceiling(seq_len(n) / chunk_rows))
}

#' 長格式的前 n 列（預覽、估計檔案大小）
#' @noRd
kits_long_head <- function(prep, n) {
  out <- list()
  got <- 0
  for (i in seq_along(prep$files)) {
    for (rows in kits_chunks(length(prep$files[[i]]$n_resp), 200)) {
      part <- kits_long_chunk(prep, i, rows)
      if (is.null(part)) next
      out[[length(out) + 1]] <- part
      got <- got + nrow(part)
      if (got >= n) return(utils::head(dplyr::bind_rows(out), n))
    }
  }
  if (length(out) == 0) return(kits_finish_columns(tibble::tibble()))
  dplyr::bind_rows(out)
}

#' 長格式分段寫進一個連線（第一段寫欄名）
#'
#' @param con 已開啟的連線（gzfile() 或 file()）
#' @param chunk_rows 每段幾列寬資料
#' @param progress NULL 或 function(fraction)
#' @noRd
kits_write_long <- function(prep, con, chunk_rows = 5000, progress = NULL) {
  first <- TRUE
  total <- sum(purrr::map_int(prep$files, function(f) length(f$n_resp)))
  done <- 0
  for (i in seq_along(prep$files)) {
    for (rows in kits_chunks(length(prep$files[[i]]$n_resp), chunk_rows)) {
      part <- kits_long_chunk(prep, i, rows)
      done <- done + length(rows)
      if (!is.null(progress)) progress(done / total)
      if (is.null(part)) next
      readr::write_csv(part, con, na = "", col_names = first)
      first <- FALSE
      rm(part)
      if (isTRUE(getOption("kitjoin.gc_chunks", TRUE))) gc(verbose = FALSE)   # 每段寫完就釋放
    }
  }
  if (first) readr::write_csv(kits_finish_columns(tibble::tibble()), con, na = "")
  invisible(con)
}

#' 作答檔寫成 .csv.gz（UTF-8；遺漏值空白），一段一段寫，不做出整張長表
#'
#' 用 gzfile() 寫，不靠檔名判斷要不要壓縮（下載的暫存檔不一定以 .gz 結尾）。
#' @param dfs,spec 同 kits_build()；也可以直接給 prep（kits_build()$prep）
#' @param chunk_rows 每段幾列寬資料
#' @noRd
write_kits_long_gz <- function(dfs, spec, file, chunk_rows = 5000, prep = NULL,
                               pivot_args = NULL, progress = NULL) {
  prep <- prep %||% kits_prep_or_abort(dfs, spec, pivot_args)
  con <- gzfile(file, open = "wb")
  on.exit(close(con))
  kits_write_long(prep, con, chunk_rows, progress)
  invisible(file)
}

#' 作答檔寫成未壓縮的 CSV（同 write_kits_long_gz()）
#' @noRd
write_kits_long_csv <- function(dfs, spec, file, chunk_rows = 5000, prep = NULL,
                                pivot_args = NULL, progress = NULL) {
  prep <- prep %||% kits_prep_or_abort(dfs, spec, pivot_args)
  con <- file(file, open = "wb")
  on.exit(close(con))
  kits_write_long(prep, con, chunk_rows, progress)
  invisible(file)
}

kits_prep_or_abort <- function(dfs, spec, pivot_args) {
  prep <- kits_prepare(dfs, paste0("檔案 ", seq_along(dfs)), spec, pivot_args)
  if (length(prep$problems) > 0) kit_abort(paste(prep$problems, collapse = "\n"))
  prep
}

#' visit_id 怎麼組：{cohort}_{subj_id}_{波次標籤}；沒有波次標籤時用 wave_m
#' @return list(parts, note)
#' @noRd
kits_visit_rule <- function(map) {
  if (nzchar(map$visit_id %||% "")) {
    return(list(parts = NULL, note = paste0("visit_id 直接用 ", map$visit_id, " 欄。")))
  }
  if (nzchar(map$wave %||% "")) {
    return(list(
      parts = c("cohort", "subj_id", map$wave),
      note = paste0("visit_id 由 {cohort}_{subj_id}_{", map$wave, "} 組成。")
    ))
  }
  list(
    parts = c("cohort", "subj_id", "wave_m"),
    note = paste0(
      "沒有對應波次標籤欄，visit_id 改由 {cohort}_{subj_id}_{wave_m} 組成。",
      "如果之前的檔案是用波次標籤（例如 m108g03）組的，請對應波次標籤欄，兩邊才接得起來。"
    )
  )
}

# visit_id：有對應欄時直接用，否則照 kits_visit_rule() 組成（任一部分缺漏時 NA）
kits_visit_ids <- function(persons, df, map) {
  if (nzchar(map$visit_id %||% "")) return(as.character(unclass(df[[map$visit_id]])))
  wave <- if (nzchar(map$wave %||% "")) {
    as.character(unclass(df[[map$wave]]))
  } else {
    format(persons$wave_m, trim = TRUE, drop0trailing = TRUE, scientific = FALSE)
  }
  wave[is.na(persons$wave_m) & !nzchar(map$wave %||% "")] <- NA
  parts <- list(persons$cohort, persons$subj_id, wave)
  out <- do.call(paste, c(parts, sep = "_"))
  bad <- Reduce(`|`, lapply(parts, function(p) is.na(p) | !nzchar(trimws(p))))
  out[bad] <- NA_character_
  out
}

# 固定 9 欄、固定順序與型別（KITS_COLUMNS）；沒有的欄補成空白（NA）
kits_finish_columns <- function(long) {
  cast <- list(c = as.character, d = as.numeric, i = function(x) as.integer(round(x)))
  cols <- purrr::map2(KITS_COLUMNS$column, KITS_COLUMNS$type, function(col, type) {
    x <- if (col %in% names(long)) long[[col]] else rep(NA, nrow(long))
    cast[[type]](unclass(x))
  })
  tibble::as_tibble(stats::setNames(cols, KITS_COLUMNS$column))
}

# 分數是數字嗎（文字欄、非整數、負分在轉換前就擋下）
kits_score_problems <- function(dfs, items) {
  out <- character(0)
  bad_type <- unique(unlist(purrr::map(dfs, function(df) {
    have <- intersect(items, names(df))
    have[!vapply(df[have], function(x) is.numeric(x) || all(is.na(x)), logical(1))]
  })))
  if (length(bad_type) > 0) {
    out <- c(out, paste0(
      "這些題目欄不是數字：", paste(utils::head(bad_type, 8), collapse = "、"),
      "。請排除它們，或回原檔確認分數。"
    ))
  }
  ranges <- kits_item_ranges(dfs, setdiff(items, bad_type))
  frac <- unique(unlist(purrr::map(dfs, function(df) {
    have <- intersect(setdiff(items, bad_type), names(df))
    have[vapply(df[have], function(x) {
      x <- as.numeric(unclass(x))
      any(!is.na(x) & x != round(x))
    }, logical(1))]
  })))
  if (length(frac) > 0) {
    out <- c(out, paste0("這些題有不是整數的分數：", paste(utils::head(frac, 8), collapse = "、"), "。"))
  }
  neg <- ranges$item_id[is.finite(ranges$min) & ranges$min < 0]
  if (length(neg) > 0) {
    out <- c(out, paste0(
      "這些題有負分：", paste(utils::head(neg, 8), collapse = "、"),
      "。如果負數（例如 -1）代表遺漏，請先到「② 遺漏值轉 NA」把它加進遺漏值代碼。"
    ))
  }
  out
}


# ── 檢查（在寬資料上做） ─────────────────────────────────────────────────────

#' 有作答的列的孩子欄（所有檔案接起來）；.n 是這一列有作答的題數
#' @noRd
kits_answered_persons <- function(prep) {
  purrr::map(prep$files, function(f) {
    keep <- f$n_resp > 0
    p <- f$persons[keep, ]
    p$.n <- f$n_resp[keep]
    p
  }) |>
    purrr::list_rbind()
}

#' 每一題（輸出的題號）的作答數、最小、最大、有沒有缺的分數（所有檔案合起來）
#' @noRd
kits_item_stats <- function(prep) {
  cols <- unique(unlist(purrr::map(prep$files, "have")))
  stats <- purrr::map(cols, function(col) {
    vals <- unlist(purrr::map(prep$files, function(f) {
      x <- f$scores[[col]]
      if (is.null(x)) NULL else unique(x[!is.na(x)])
    }))
    n <- sum(purrr::map_int(prep$files, function(f) {
      x <- f$scores[[col]]
      if (is.null(x)) 0L else sum(!is.na(x))
    }))
    vals <- unique(vals)
    if (length(vals) == 0) {
      return(tibble::tibble(column = col, n = 0L, min = NA_real_, max = NA_real_, gap = FALSE))
    }
    tibble::tibble(
      column = col, n = n, min = min(vals), max = max(vals),
      gap = !all(seq(min(vals), max(vals)) %in% vals)
    )
  }) |>
    purrr::list_rbind()
  if (nrow(stats) == 0) {
    stats <- tibble::tibble(column = character(0), n = integer(0), min = numeric(0),
                            max = numeric(0), gap = logical(0))
  }
  stats$item_id <- kits_out_ids(stats$column, prep$rename)
  stats$domain <- unname(prep$domain_of[stats$item_id])
  stats
}

#' 轉換前的檢查（都在寬資料上做，結果和長格式上檢查相同）
#'
#' problems（紅色，有就不輸出）：必填欄空白、同一個 visit_id 在好幾列（同一列次同一題
#' 會有好幾列）、同一個 visit_id 在兩個世代、同一列次的孩子欄不一致、題目沒有領域。
#' warnings（橘色）：沒有 0 分的題、分數中間有缺、疑似遺漏值代碼、不是錨題的領域、
#' 沒有作答的題、age_m 空白、反向題已以 r 結尾。
#' @return list(problems, warnings, n_visits, n_items)
#' @noRd
kits_wide_checks <- function(prep, spec) {
  problems <- character(0)
  persons <- kits_answered_persons(prep)
  stats <- kits_item_stats(prep)
  answered <- stats[stats$n > 0, ]

  # 必填的欄不能空白：visit_id、cohort、wave_m 看每一列；item_id、item_score 來自題目欄
  blank <- function(x) is.na(x) | (is.character(x) & !nzchar(trimws(x)))
  why <- c(visit_id = "（組成它的世代、孩子編號或波次有缺）", cohort = "（世代欄有空白）")
  for (col in c("visit_id", "cohort", "wave_m")) {
    hit <- blank(persons[[col]])
    if (any(hit)) {
      problems <- c(problems, paste0(
        sum(hit), " 個列次（", format(sum(persons$.n[hit]), big.mark = ","), " 列作答）的 ", col,
        " 是空白", if (col %in% names(why)) why[[col]], "；這一欄是必填。"
      ))
    }
  }
  ok <- persons[!blank(persons$visit_id), ]

  # 同一個 visit_id 在好幾列寬資料 → 同一列次同一題會有好幾列
  twice <- vctrs::vec_duplicate_detect(ok$visit_id)
  if (any(twice)) {
    problems <- c(problems, paste0(
      "同一列次同一題有好幾列：", sum(twice), " 列寬資料的 visit_id 重複（例如 ",
      ok$visit_id[which(twice)[1]], "）。原檔可能有重複的孩子，或 visit_id 沒有分開不同波次。"
    ))
    cross <- dplyr::distinct(ok, .data$visit_id, .data$cohort)
    dup_c <- unique(cross$visit_id[duplicated(cross$visit_id)])
    people <- dplyr::distinct(ok[c("visit_id", "subj_id", "cohort", "sex", "wave_m", "age_m")])
    if (length(dup_c) > 0) {
      problems <- c(problems, paste0(
        "同一個 visit_id 出現在兩個世代（例如 ", dup_c[1], "）。請讓 visit_id 包含世代。"
      ))
    } else if (anyDuplicated(people$visit_id) > 0) {
      v <- people$visit_id[duplicated(people$visit_id)][1]
      problems <- c(problems, paste0(
        "同一個列次（例如 ", v, "）的孩子欄（subj_id、sex、wave_m、age_m）不一致。",
        "請確認原檔沒有重複的孩子。"
      ))
    }
  }
  no_dom <- answered$item_id[blank(answered$domain)]
  if (length(no_dom) > 0) {
    problems <- c(problems, paste0(
      "這些題沒有領域：", paste(utils::head(no_dom, 8), collapse = "、"), "。"
    ))
  }

  list(
    problems = problems,
    warnings = kits_score_warnings(stats, ok, spec),
    n_visits = length(unique(ok$visit_id)),
    n_items = nrow(answered)
  )
}

# 分數與其他提醒（橘色）
kits_score_warnings <- function(stats, persons, spec) {
  out <- character(0)
  per_item <- stats[stats$n > 0, ]
  some <- function(x) paste(utils::head(x, 5), collapse = "、")
  no_zero <- per_item$item_id[per_item$min > 0]
  if (length(no_zero) > 0) {
    out <- c(out, paste0(
      length(no_zero), " 題沒有 0 分（例如 ", some(no_zero),
      "）。kits 要有 0 分才能自由校準；分數如果是從 1 開始，請勾「步驟 1：分數減 1」。"
    ))
  }
  gap <- per_item$item_id[per_item$gap]
  if (length(gap) > 0) {
    out <- c(out, paste0(
      length(gap), " 題的分數中間有缺（例如 ", some(gap), "）。kits 校準時會停下來，要先合併分數。"
    ))
  }
  big <- per_item$item_id[per_item$max >= KITS_CODE_MIN]
  if (length(big) > 0) {
    out <- c(out, paste0(
      length(big), " 題有 7777 以上的分數（例如 ", some(big), "），可能是遺漏值代碼。",
      "請先到「② 遺漏值轉 NA」處理。"
    ))
  }
  doms <- setdiff(unique(stats::na.omit(per_item$domain)), KITS_ANCHOR_DOMAINS)
  if (length(doms) > 0) {
    out <- c(out, paste0(
      "領域 ", paste(doms, collapse = "、"), " 不是 lan、motor、cog；",
      "外部連結的錨題只認這三個名稱（其他領域只做內部連結時沒關係）。"
    ))
  }
  empty <- c(setdiff(spec$items$column, stats$column), stats$column[stats$n == 0])
  if (length(empty) > 0) {
    out <- c(out, paste0(
      length(empty), " 題沒有任何作答，不會出現在作答檔：",
      paste(utils::head(empty, 8), collapse = "、"), if (length(empty) > 8) "…" else "", "。"
    ))
  }
  n_age <- length(unique(persons$visit_id[is.na(persons$age_m)]))
  if (n_age > 0) out <- c(out, paste0(n_age, " 個列次沒有 age_m（題目表的通過年齡要用它）。"))
  rev <- spec$steps$reverse
  if (!is.null(rev)) {
    again <- rev$items[grepl("r$", rev$items)]
    if (length(again) > 0) {
      out <- c(out, paste0(
        "反向計分的題已經以 r 結尾（", some(again), "），可能已經反向過；請確認沒有反向兩次。"
      ))
    }
  }
  out
}

#' 未壓縮 CSV 大約幾 MB（用前幾列長格式的平均長度估計）
#' @param head_long 長格式的前幾列（kits_long_head()）
#' @param n_rows 長格式的總列數
#' @noRd
kits_csv_mb <- function(head_long, n_rows) {
  if (nrow(head_long) == 0 || n_rows == 0) return(0)
  text <- readr::format_csv(head_long, na = "")
  per_row <- nchar(text, type = "bytes") / (nrow(head_long) + 1)
  per_row * (n_rows + 1) / 1024^2
}

#' 摘要：每個世代 × 領域幾個列次、幾題、幾格作答、哪些波次（由寬資料的題目欄計數）
#' @noRd
kits_summary <- function(prep) {
  parts <- purrr::map(prep$files, function(f) {
    if (length(f$have) == 0) return(NULL)
    dom <- unname(prep$domain_of[kits_out_ids(f$have, prep$rename)])
    purrr::map(unique(dom), function(d) {
      cols <- f$have[dom == d]
      n <- Reduce(`+`, lapply(f$scores[cols], function(x) !is.na(x)), integer(nrow(f$persons)))
      keep <- n > 0
      rows <- tibble::tibble(
        cohort = f$persons$cohort[keep], domain = d, visit_id = f$persons$visit_id[keep],
        wave_m = f$persons$wave_m[keep], n = as.integer(n[keep])
      )
      items <- purrr::map(cols, function(col) {
        co <- unique(f$persons$cohort[!is.na(f$scores[[col]])])
        tibble::tibble(cohort = co, domain = rep(d, length(co)),
                       item_id = rep(kits_out_ids(col, prep$rename), length(co)))
      }) |>
        purrr::list_rbind()
      list(rows = rows, items = items)
    })
  }) |>
    purrr::list_flatten()
  rows <- purrr::list_rbind(purrr::map(parts, "rows"))
  items <- purrr::list_rbind(purrr::map(parts, "items"))
  if (is.null(rows) || nrow(rows) == 0) return(NULL)
  n_items <- dplyr::summarise(items, 題數 = dplyr::n_distinct(.data$item_id),
                              .by = c("cohort", "domain"))
  rows |>
    dplyr::summarise(
      列次 = dplyr::n_distinct(.data$visit_id), 作答格數 = sum(.data$n),
      波次 = paste(sort(unique(.data$wave_m)), collapse = ", "),
      .by = c("cohort", "domain")
    ) |>
    dplyr::left_join(n_items, by = c("cohort", "domain")) |>
    dplyr::select("cohort", "domain", "列次", "題數", "作答格數", "波次") |>
    dplyr::arrange(.data$cohort, .data$domain) |>
    dplyr::rename(世代 = "cohort", 領域 = "domain")
}
