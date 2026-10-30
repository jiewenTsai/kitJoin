# 0.4.0 合併版的 kits_build()（整張長表放在記憶體；0.4.1 拿掉遺漏值步驟）：只給測試比對用，
# 確認分段寫出的 .csv.gz 和舊的結果一列一列相同。

kits_build_old <- function(dfs, file_names, spec, pivot_args = NULL) {
  problems <- kits_spec_problems(dfs, file_names, spec)
  if (length(problems) > 0) return(list(long = NULL, problems = problems))
  items <- spec$items$column
  map <- spec$map
  notes <- character(0)

  problems <- kits_score_problems(dfs, items)
  if (length(problems) > 0) return(list(long = NULL, problems = problems))

  # 步驟 1：分數減 1（已經有 0 分的題不能減）
  shift <- spec$steps$shift
  if (!is.null(shift)) {
    zero <- dplyr::filter(kits_item_ranges(dfs, shift), .data$min <= 0)
    if (nrow(zero) > 0) {
      return(list(long = NULL, problems = paste0(
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
      return(list(long = NULL, problems = paste0(
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
  longs <- purrr::map(seq_along(dfs), function(i) {
    kits_long_one_old(dfs[[i]], spec$cohort[[i]], map, items, pivot_args)
  })
  domains <- spec$items |>
    dplyr::mutate(item_id = dplyr::coalesce(unname(rename[.data$column]), .data$column))
  long <- purrr::list_rbind(longs) |>
    dplyr::mutate(item_id = dplyr::coalesce(unname(rename[.data$item_id]), .data$item_id)) |>
    dplyr::left_join(dplyr::select(domains, "item_id", "domain"), by = "item_id") |>
    kits_finish_columns()

  checks <- kits_long_checks_old(long, dfs, spec, items)
  list(
    long = if (length(checks$problems) > 0) NULL else long,
    problems = checks$problems,
    warnings = checks$warnings,
    notes = c(visit_rule$note, notes),
    summary = if (length(checks$problems) > 0) NULL else kits_summary_old(long),
    visit_rule = visit_rule
  )
}

kits_long_one_old <- function(df, cohort, map, items, pivot_args) {
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
  wide <- dplyr::bind_cols(persons, tibble::as_tibble(scores))
  args <- utils::modifyList(
    pivot_args %||% list(),
    list(cols = have, names_to = "item_id", values_to = "item_score", values_drop_na = TRUE)
  )
  pivot_longer_run(wide, args)
}

kits_required_empty_old <- function(long) {
  req <- KITS_COLUMNS$column[KITS_COLUMNS$required]
  n <- vapply(req, function(col) {
    x <- long[[col]]
    if (!is.character(x)) return(sum(is.na(x)))
    # 只對不重複的值判斷空白（幾百萬列時快很多）
    u <- unique(x)
    blank <- u[is.na(u) | !nzchar(trimws(u))]
    if (length(blank) == 0) 0L else sum(x %in% blank)
  }, integer(1))
  n[n > 0]
}

kits_long_checks_old <- function(long, dfs, spec, items) {
  problems <- character(0)
  warnings <- character(0)
  add <- function(x, msg) c(x, msg)

  # 必填的欄不能空白（domain 下面逐題列出）
  empty <- kits_required_empty_old(long)
  why <- c(visit_id = "（組成它的世代、孩子編號或波次有缺）", cohort = "（世代欄有空白）")
  for (col in setdiff(names(empty), "domain")) {
    problems <- add(problems, paste0(
      empty[[col]], " 列的 ", col, " 是空白", if (col %in% names(why)) why[[col]], "；這一欄是必填。"
    ))
  }
  ok <- long[!is.na(long$visit_id), ]

  # 同一列次同一題：用雜湊找重複（不分組計數，幾百萬列時快很多）
  twice <- vctrs::vec_duplicate_detect(ok[c("visit_id", "item_id")])
  if (any(twice)) {
    first <- which(twice)[1]
    problems <- add(problems, paste0(
      "同一列次同一題有好幾列（共 ", sum(twice), " 列），例如 ", ok$visit_id[first], " 的 ",
      ok$item_id[first], "。原檔可能有重複的孩子，或 visit_id 沒有分開不同波次。"
    ))
  }
  person_cols <- c("visit_id", "subj_id", "cohort", "sex", "wave_m", "age_m")
  persons <- dplyr::distinct(ok[person_cols])
  cross <- dplyr::distinct(persons, .data$visit_id, .data$cohort)
  dup_c <- unique(cross$visit_id[duplicated(cross$visit_id)])
  if (length(dup_c) > 0) {
    problems <- add(problems, paste0(
      "同一個 visit_id 出現在兩個世代（例如 ", dup_c[1], "）。請讓 visit_id 包含世代。"
    ))
  } else if (anyDuplicated(persons$visit_id) > 0) {
    v <- persons$visit_id[duplicated(persons$visit_id)][1]
    problems <- add(problems, paste0(
      "同一個列次（例如 ", v, "）的孩子欄（subj_id、sex、wave_m、age_m）不一致。",
      "請確認原檔沒有重複的孩子。"
    ))
  }
  no_dom <- unique(ok$item_id[is.na(ok$domain) | !nzchar(ok$domain)])
  if (length(no_dom) > 0) {
    problems <- add(problems, paste0(
      "這些題沒有領域：", paste(utils::head(no_dom, 8), collapse = "、"), "。"
    ))
  }

  warnings <- c(warnings, kits_score_warnings_old(ok, dfs, spec, items))
  list(problems = problems, warnings = warnings)
}

kits_score_warnings_old <- function(long, dfs, spec, items) {
  out <- character(0)
  per_item <- long |>
    dplyr::summarise(
      min = min(.data$item_score), max = max(.data$item_score),
      gap = !all(seq(min(.data$item_score), max(.data$item_score)) %in% .data$item_score),
      .by = "item_id"
    )
  no_zero <- per_item$item_id[per_item$min > 0]
  if (length(no_zero) > 0) {
    out <- c(out, paste0(
      length(no_zero), " 題沒有 0 分（例如 ", paste(utils::head(no_zero, 5), collapse = "、"),
      "）。kits 要有 0 分才能自由校準；分數如果是從 1 開始，請勾「步驟 2：分數減 1」。"
    ))
  }
  gap <- per_item$item_id[per_item$gap]
  if (length(gap) > 0) {
    out <- c(out, paste0(
      length(gap), " 題的分數中間有缺（例如 ", paste(utils::head(gap, 5), collapse = "、"),
      "）。kits 校準時會停下來，要先合併分數。"
    ))
  }
  big <- per_item$item_id[per_item$max >= 7777]
  if (length(big) > 0) {
    out <- c(out, paste0(
      length(big), " 題有 7777 以上的分數（例如 ", paste(utils::head(big, 5), collapse = "、"),
      "），可能是遺漏值代碼。請勾「步驟 1」。"
    ))
  }
  doms <- setdiff(unique(stats::na.omit(long$domain)), KITS_ANCHOR_DOMAINS)
  if (length(doms) > 0) {
    out <- c(out, paste0(
      "領域 ", paste(doms, collapse = "、"), " 不是 lan、motor、cog；",
      "外部連結的錨題只認這三個名稱（其他領域只做內部連結時沒關係）。"
    ))
  }
  empty <- setdiff(items, unique(long$item_id))
  rev <- spec$steps$reverse
  if (!is.null(rev) && isTRUE(rev$add_r)) empty <- setdiff(empty, rev$items)
  if (length(empty) > 0) {
    out <- c(out, paste0(
      length(empty), " 題沒有任何作答，不會出現在作答檔：",
      paste(utils::head(empty, 8), collapse = "、"), if (length(empty) > 8) "…" else "", "。"
    ))
  }
  n_age <- sum(is.na(dplyr::distinct(long, .data$visit_id, .data$age_m)$age_m))
  if (n_age > 0) out <- c(out, paste0(n_age, " 個列次沒有 age_m（題目表的通過年齡要用它）。"))
  if (!is.null(rev)) {
    again <- rev$items[grepl("r$", rev$items)]
    if (length(again) > 0) {
      out <- c(out, paste0(
        "反向計分的題已經以 r 結尾（", paste(utils::head(again, 5), collapse = "、"),
        "），可能已經反向過；請確認沒有反向兩次。"
      ))
    }
  }
  mb <- kits_csv_mb_old(long)
  if (mb > KITSUI_UPLOAD_MB) {
    out <- c(out, sprintf(
      "未壓縮的 CSV 約 %.0f MB，超過 kitsUI 單檔上限 %d MB；請下載 .csv.gz（壓縮檔）。",
      mb, KITSUI_UPLOAD_MB
    ))
  }
  out
}

kits_csv_mb_old <- function(long) {
  if (nrow(long) == 0) return(0)
  sample <- utils::head(long, 2000)
  text <- readr::format_csv(sample, na = "")
  per_row <- nchar(text, type = "bytes") / (nrow(sample) + 1)
  per_row * (nrow(long) + 1) / 1024^2
}

kits_summary_old <- function(long) {
  long |>
    dplyr::summarise(
      列次 = dplyr::n_distinct(.data$visit_id), 題數 = dplyr::n_distinct(.data$item_id),
      作答格數 = dplyr::n(),
      波次 = paste(sort(unique(.data$wave_m)), collapse = ", "),
      .by = c("cohort", "domain")
    ) |>
    dplyr::arrange(.data$cohort, .data$domain) |>
    dplyr::rename(世代 = "cohort", 領域 = "domain")
}

# 新的結果做成整張長表（測試用）：分段寫成 .csv.gz 再讀回來
kits_long_of <- function(out, chunk_rows = 3) {
  if (is.null(out$prep)) return(NULL)
  path <- tempfile(fileext = ".csv.gz")
  on.exit(unlink(path))
  write_kits_long_gz(prep = out$prep, file = path, chunk_rows = chunk_rows)
  readr::read_csv(path, col_types = "ccccddcic", na = "", progress = FALSE)
}
