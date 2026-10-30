# 寬格式串接：每人一列，各波變項加上後綴並排

# 串接結果（列數 × 欄數）超過這個格數就直接拒絕：多對多暴增時 R 會用光記憶體
KIT_JOIN_MAX_CELLS <- 2e8

#' 寬格式串接（含檢查、核對與白話摘要）
#'
#' @param dfs 各波資料框（依串接順序）
#' @param file_names 各檔檔名
#' @param wave_labels 各檔波次名稱（作為欄名後綴，t1 → _t1）
#' @param by_vars 辨識同一個人的 ID 欄位（結果中的 ID 欄名）
#' @param method "left_join"、"inner_join" 或 "full_join"
#' @param keys_as_text ID 型別不一致時是否自動轉成文字再比對
#' @param confirm_large 預估列數遠多於原檔（多對多暴增）時是否照樣執行；
#'   FALSE 時以 class "kit_join_too_large" 停下，讓 UI 請使用者確認
#' @param id_cols 各檔自己的 ID 欄名（list，每個檔案一個字元向量，依序對應 by_vars）；
#'   NULL 表示每個檔案都用 by_vars。各檔欄名不同時（w1_id、w2_id、w3id）
#'   先改名成 by_vars 再比對。
#' @return list(result, audit, summary, warnings, notes)
#' @noRd
join_waves <- function(dfs, file_names, wave_labels, by_vars,
                       method = "left_join", keys_as_text = TRUE,
                       confirm_large = FALSE, id_cols = NULL) {
  method <- match.arg(method, unname(KIT_JOIN_METHODS))
  id_cols <- id_cols %||% rep(list(by_vars), length(dfs))
  check_join_inputs(dfs, file_names, wave_labels, by_vars, id_cols)
  names_shown <- file_display_name(wave_labels, file_names)

  notes <- id_rename_note(id_cols, by_vars, names_shown)
  dfs <- unify_id_names(dfs, id_cols, by_vars, names_shown)
  dfs <- clean_keys(dfs, by_vars)
  conflict <- key_type_conflict(dfs, by_vars, names_shown, id_cols)
  if (!is.null(conflict)) {
    if (!keys_as_text) {
      kit_abort(
        conflict, "無法直接比對。請勾選「ID 型別不一致時，自動轉成文字再比對」，",
        "或回原檔把 ID 改成同一種格式。"
      )
    }
    dfs <- keys_to_text(dfs, by_vars)
    notes <- c(notes, paste0(conflict, "已自動把 ID 轉成文字再比對。"))
  }

  check_join_size(dfs, by_vars, method, names_shown, confirm_large)

  suffixed <- purrr::map2(dfs, wave_labels, function(df, w) {
    apply_suffix(df, join_suffix(w), by_vars)
  })
  join_fn <- getExportedValue("dplyr", method)

  # 逐步合併並記錄每一步的列欄數（供核對）
  result <- suffixed[[1]]
  steps <- vector("list", length(suffixed) - 1)
  for (i in seq_along(suffixed)[-1]) {
    right <- suffixed[[i]]
    joined <- join_fn(
      result, right,
      by = by_vars, na_matches = "never", relationship = "many-to-many"
    )
    steps[[i - 1]] <- list(
      n_left = nrow(result), n_right = nrow(right), n_out = nrow(joined),
      ncol_expected = ncol(result) + ncol(right) - length(by_vars),
      ncol_out = ncol(joined)
    )
    result <- joined
  }

  stats <- join_stats(dfs, by_vars, method, result)
  list(
    result = result,
    audit = join_audit_tables(stats, steps, names_shown, method),
    summary = join_plain_summary(stats, names_shown, method, result),
    warnings = dup_key_warnings(stats, names_shown),
    notes = notes
  )
}

#' 串接前檢查：檔案數、ID 欄位、波次名稱
#' @noRd
check_join_inputs <- function(dfs, file_names, wave_labels, by_vars,
                              id_cols = rep(list(by_vars), length(dfs))) {
  if (length(dfs) < 2) {
    kit_abort("需要至少 2 個檔案才能串接。請到「① 上傳檔案」再加入一個波次的檔案。")
  }
  if (length(by_vars) == 0) {
    kit_abort("請先選擇用來辨識同一個人的 ID 欄位。")
  }
  per_file <- !all(purrr::map_lgl(id_cols, identical, by_vars))
  for (i in seq_along(dfs)) {
    if (length(id_cols[[i]]) != length(by_vars)) {
      kit_abort(
        "每個檔案要選一樣多個 ID 欄：第 1 個檔案選了 ", length(by_vars), " 個，",
        file_names[i], " 選了 ", length(id_cols[[i]]), " 個。"
      )
    }
    missing_by <- setdiff(id_cols[[i]], names(dfs[[i]]))
    if (length(missing_by) > 0) {
      kit_abort(
        file_names[i], " 沒有 ", paste(missing_by, collapse = "、"), " 欄位。",
        if (per_file) {
          "請重新選這個檔案的 ID 欄。"
        } else {
          "請改選每個檔案都有的 ID 欄位；各檔 ID 欄名不同時，請勾選「各檔的 ID 欄名不同」分別指定。"
        }
      )
    }
  }
  problem <- check_wave_labels(wave_labels, file_names, purpose = "join")
  if (!is.null(problem)) kit_abort(problem)
  invisible(TRUE)
}

#' 各檔的 ID 欄改名成共同的 ID 欄名（by_vars）
#'
#' 檔案裡已經有一個同名、但不是 ID 的欄時無法改名，給白話錯誤。
#' @noRd
unify_id_names <- function(dfs, id_cols, by_vars, names_shown) {
  purrr::map(seq_along(dfs), function(i) {
    df <- dfs[[i]]
    own <- id_cols[[i]]
    if (identical(own, by_vars)) return(df)
    taken <- intersect(setdiff(names(df), own), by_vars)
    if (length(taken) > 0) {
      kit_abort(
        names_shown[i], "已經有名為 ", paste(taken, collapse = "、"),
        " 的欄，但它不是這個檔案的 ID，所以不能把 ", paste(own, collapse = "、"),
        " 改成這個名稱。請把第 1 個檔案的 ID 欄改選成其他欄，或先在原檔改欄名。"
      )
    }
    names(df)[match(own, names(df))] <- by_vars
    df
  })
}

#' 各檔 ID 欄名不同時的說明：「各檔的 ID 已對應到 w1_id：t2 的 w2_id、t3 的 w3id。」
#' @noRd
id_rename_note <- function(id_cols, by_vars, names_shown) {
  differs <- which(!purrr::map_lgl(id_cols, identical, by_vars))
  if (length(differs) == 0) return(character(0))
  paste0(
    "各檔的 ID 已對應到 ", paste(by_vars, collapse = "＋"), "：",
    paste0(
      names_shown[differs], "的 ",
      purrr::map_chr(id_cols[differs], paste, collapse = "＋"),
      collapse = "、"
    ),
    "。結果中的 ID 欄叫 ", paste(by_vars, collapse = "、"), "。"
  )
}

#' 預估串接結果的列數（不實際合併）
#'
#' 每個 ID 的列數相乘：兩檔各有 3 列 ID = 1 時，結果有 9 列。
#' ID 空白的列不配對：left join 保留左邊的、full join 兩邊都保留。
#' @noRd
join_size_estimate <- function(dfs, by_vars, method) {
  count_keys <- function(df) {
    k <- key_strings(df, by_vars)
    list(counts = table(k[!is.na(k)]), n_na = sum(is.na(k)))
  }
  left <- count_keys(dfs[[1]])
  counts <- stats::setNames(as.numeric(left$counts), names(left$counts))
  n_na <- left$n_na
  for (df in dfs[-1]) {
    right <- count_keys(df)
    r <- stats::setNames(as.numeric(right$counts), names(right$counts))
    common <- intersect(names(counts), names(r))
    both <- counts[common] * r[common]
    left_only <- counts[setdiff(names(counts), common)]
    right_only <- r[setdiff(names(r), common)]
    counts <- switch(method,
      inner_join = both,
      left_join = c(both, left_only),
      full_join = c(both, left_only, right_only)
    )
    n_na <- switch(method,
      inner_join = 0,
      left_join = n_na,
      full_join = n_na + right$n_na
    )
  }
  sum(counts) + n_na
}

#' 串接結果會暴增時停下來：太大直接拒絕，偏大則請使用者確認
#'
#' 正常情況下結果列數不會超過各檔列數相加；超過「各檔列數相加」與
#' 「最大檔案列數的 2 倍」時，通常是 ID 選錯（例如出生年）或大量重複。
#' @noRd
check_join_size <- function(dfs, by_vars, method, names_shown, confirm_large) {
  n_rows <- purrr::map_int(dfs, nrow)
  estimate <- join_size_estimate(dfs, by_vars, method)
  limit <- max(sum(n_rows), 2 * max(n_rows))
  if (estimate <= limit) return(invisible(estimate))
  ncol_out <- length(by_vars) + sum(purrr::map_int(dfs, ncol) - length(by_vars))
  biggest <- which.max(n_rows)
  times <- floor(estimate / n_rows[biggest])
  why <- paste0(
    "預估串接結果有 ", format(estimate, scientific = FALSE, big.mark = ","), " 列，",
    "是最大檔案（", names_shown[biggest], "，", n_rows[biggest], " 列）的 ", times, " 倍。",
    "這通常表示 ID 欄選錯（例如選到出生年、權數），或 ID 大量重複：",
    "同一個 ID 在兩個檔案各有多列時，會兩兩配對成很多列。"
  )
  if (estimate * ncol_out > KIT_JOIN_MAX_CELLS) {
    kit_abort(why, "結果太大，無法產生。請改選每個人都不同的 ID 欄。")
  }
  if (!confirm_large) {
    kit_abort_class(
      "kit_join_too_large",
      why, "請先確認 ID 欄；確定沒問題再按「仍要執行」。",
      data = list(estimate = estimate)
    )
  }
  invisible(estimate)
}

#' 各欄的不重複比例（不算空白；各檔取最小值），選 ID 時參考
#' @noRd
id_uniqueness <- function(dfs, cols) {
  vapply(cols, function(col) {
    min(vapply(dfs, function(df) {
      if (!col %in% names(df)) return(0)
      x <- df[[col]]
      x <- x[!is.na(x)]
      if (length(x) == 0) 0 else vctrs::vec_unique_count(x) / length(x)
    }, numeric(1)))
  }, numeric(1), USE.NAMES = FALSE)
}

#' 不重複比例的文字：1 → "100%"；不到 1 的不會顯示成 100%
#' @noRd
format_unique_pct <- function(ratio) {
  ifelse(ratio >= 1, "100%", paste0(floor(ratio * 1000) / 10, "%"))
}

#' ID 下拉選單：名稱像 ID、不重複比例高的欄排前面，選項附上不重複比例
#' @return 具名字元向量（名稱是給使用者看的文字）
#' @noRd
id_choices <- function(dfs, cols) {
  if (length(cols) == 0) return(character(0))
  ratio <- id_uniqueness(dfs, cols)
  guessed <- cols %in% guess_id_cols(cols)
  ord <- order(!guessed, -floor(ratio * 1000), seq_along(cols))
  shown <- paste0(cols, "（不重複 ", format_unique_pct(ratio), "）")
  stats::setNames(cols[ord], shown[ord])
}

#' ID 下拉選單的預設：名稱像 ID 的欄中，不重複比例最高的第一個
#' @noRd
default_id_choice <- function(choices) {
  cols <- unname(choices)
  guessed <- cols[cols %in% guess_id_cols(cols)]
  utils::head(guessed, 1)
}

#' 選好的 ID 在各檔的情況（串接前給使用者看）
#' @noRd
id_check_table <- function(dfs, names_shown, by_vars,
                           id_cols = rep(list(by_vars), length(dfs))) {
  stats <- purrr::map(seq_along(dfs), function(i) {
    df <- dfs[[i]]
    keys <- key_strings(df, id_cols[[i]])
    ok <- keys[!is.na(keys)]
    c(nrow(df), length(unique(ok)), sum(table(ok) > 1), sum(is.na(keys)))
  })
  m <- do.call(rbind, stats)
  tibble::tibble(
    檔案 = names_shown,
    ID欄 = purrr::map_chr(id_cols, paste, collapse = "＋"),
    列數 = m[, 1],
    不重複ID數 = m[, 2],
    重複的ID數 = m[, 3],
    ID空白列數 = m[, 4]
  )
}

#' 檢查波次名稱：不可空白、不可重複
#'
#' @param purpose "join"（作為欄名後綴）或 "stack"（作為 wave 欄的值）
#' @return 問題說明；沒問題時為 NULL
#' @noRd
check_wave_labels <- function(
    wave_labels,
    file_names,
    purpose = c("join", "stack")) {
  purpose <- match.arg(purpose)
  why <- if (purpose == "join") {
    "因為它會加在欄名後面（例如 score_t1），用來分辨各波的變項。"
  } else {
    "因為它會填進 wave 欄，用來分辨每一列屬於哪一波。"
  }
  empty <- which(!nzchar(trimws(wave_labels)))
  if (length(empty) > 0) {
    return(paste0(
      "第 ", empty[1], " 個檔案（", file_names[empty[1]], "）沒有填波次名稱。",
      "請到「① 上傳檔案」填寫，例如 t", empty[1], "；每個檔案都需要波次名稱，", why
    ))
  }
  normalized <- join_suffix(wave_labels)
  dup <- unique(wave_labels[duplicated(normalized)])
  if (length(dup) > 0) {
    return(paste0(
      "波次名稱「", dup[1], "」重複了。請到「① 上傳檔案」把每個檔案的波次名稱改成不同的，",
      why
    ))
  }
  NULL
}

#' 寬串接後綴：使用者輸入 t1，實際欄位後綴為 _t1
#' @noRd
join_suffix <- function(label) {
  label <- trimws(label)
  ifelse(!nzchar(label) | startsWith(label, "_"), label, paste0("_", label))
}

#' 對非 ID 欄位加後綴
#' @noRd
apply_suffix <- function(df, suffix, by_vars) {
  if (!nzchar(suffix)) return(df)
  # 只有 ID 欄的檔（例如後期名冊）：paste0(character(0), suffix) 會多出一個元素
  dplyr::rename_with(
    df,
    function(x) if (length(x) == 0) x else paste0(x, suffix),
    .cols = -dplyr::all_of(by_vars)
  )
}

#' ID 欄位型別是「數字」還是「文字」
#' @noRd
key_kind <- function(x) {
  if (is.numeric(x)) {
    "數字"
  } else if (is.character(x) || is.factor(x)) {
    "文字"
  } else {
    class(x)[1]
  }
}

#' ID 欄位在各檔的型別不一致時，回傳白話說明；一致時回傳 NULL
#' @noRd
key_type_conflict <- function(dfs, by_vars, names_shown,
                              id_cols = rep(list(by_vars), length(dfs))) {
  for (k in seq_along(by_vars)) {
    col <- by_vars[k]
    kinds <- vapply(dfs, function(df) key_kind(df[[col]]), character(1))
    if (length(unique(kinds)) > 1) {
      first <- which(kinds != kinds[1])[1]
      return(paste0(
        names_shown[1], "的 ", id_cols[[1]][k], " 是", kinds[1], "，",
        names_shown[first], "的 ", id_cols[[first]][k], " 是", kinds[first], "，"
      ))
    }
  }
  NULL
}

#' 把各檔的 ID 欄位轉成文字（去掉前後空白；數字不用科學記號）
#' @noRd
keys_to_text <- function(dfs, by_vars) {
  purrr::map(dfs, function(df) {
    for (col in by_vars) {
      label <- attr(df[[col]], "label", exact = TRUE)
      df[[col]] <- clean_key(num_to_text(unclass(df[[col]])))
      attr(df[[col]], "label") <- label
    }
    df
  })
}

#' 文字 ID 去掉前後空白，只有空白的 ID 當成缺漏（NA）
#'
#' 兩檔都是文字時也一樣處理，" 1" 和 "1" 才配得到；數字 ID 不變。
#' @noRd
clean_key <- function(x) {
  if (!is.character(x)) return(x)
  v <- trimws(as.vector(unclass(x)))
  v[!is.na(v) & !nzchar(v)] <- NA_character_
  attributes(v) <- attributes(x)
  v
}

#' 各檔的 ID 欄位都套用 clean_key()
#' @noRd
clean_keys <- function(dfs, by_vars) {
  purrr::map(dfs, function(df) {
    for (col in intersect(by_vars, names(df))) df[[col]] <- clean_key(df[[col]])
    df
  })
}

#' 重複 ID 的例子：「release_id = 1（2 次）」
#' @noRd
dup_key_examples <- function(df, by_vars, n = 5) {
  keys <- key_strings(df, by_vars)
  counts <- table(keys[!is.na(keys)])
  dups <- counts[counts > 1]
  if (length(dups) == 0) return(character(0))
  shown <- utils::head(dups, n)
  values <- strsplit(names(shown), "\r", fixed = TRUE)
  vapply(seq_along(shown), function(i) {
    pairs <- paste(by_vars, "=", values[[i]], collapse = "、")
    paste0(pairs, "（", shown[[i]], " 次）")
  }, character(1))
}


# ── 核對：先算數字，再組成給使用者看的表格與文字 ────────────────────────────

#' 串接核對所需的數字
#' @noRd
join_stats <- function(dfs, by_vars, method, result) {
  keys <- purrr::map(dfs, key_strings, by_vars = by_vars)
  key_sets <- purrr::map(keys, function(k) unique(k[!is.na(k)]))
  n_dup <- purrr::map_int(keys, function(k) {
    counts <- table(k[!is.na(k)])
    sum(counts > 1)
  })
  n_na_key <- purrr::map_int(keys, function(k) sum(is.na(k)))
  in_all <- Reduce(intersect, key_sets)
  in_any <- Reduce(union, key_sets)

  # 預期列數只在「後面的檔案 ID 都唯一」時有單一答案
  later_dup <- any(n_dup[-1] > 0)
  any_dup <- any(n_dup > 0)
  nrow_expected <- switch(method,
    left_join = if (later_dup) NA_integer_ else nrow(dfs[[1]]),
    inner_join = if (any_dup) NA_integer_ else length(in_all),
    full_join = if (any_dup) NA_integer_ else length(in_any) + sum(n_na_key)
  )
  ncol_expected <- length(by_vars) +
    sum(purrr::map_int(dfs, ncol) - length(by_vars))

  list(
    n_rows = purrr::map_int(dfs, nrow),
    n_cols = purrr::map_int(dfs, ncol),
    key_sets = key_sets,
    n_unique = lengths(key_sets),
    n_dup = n_dup,
    dup_examples = purrr::map(dfs, dup_key_examples, by_vars = by_vars),
    nrow_out = nrow(result),
    ncol_out = ncol(result),
    n_na_key = n_na_key,
    n_in_all = length(in_all),
    n_in_any = length(in_any),
    any_dup = any_dup,
    nrow_expected = nrow_expected,
    nrow_ok = if (is.na(nrow_expected)) NA else nrow(result) == nrow_expected,
    ncol_expected = ncol_expected,
    ncol_ok = ncol(result) == ncol_expected
  )
}

#' 白話摘要：誰被留下、誰被排除
#' @return 字元向量，每個元素一句
#' @noRd
join_plain_summary <- function(stats, names_shown, method, result) {
  sets <- stats$key_sets
  first <- names_shown[1]
  out <- switch(method,
    left_join = c(
      paste0("以 ", first, "的 ", length(sets[[1]]), " 人為準。"),
      vapply(seq_along(sets)[-1], function(i) {
        found <- sum(sets[[1]] %in% sets[[i]])
        missing <- length(sets[[1]]) - found
        extra <- sum(!sets[[i]] %in% sets[[1]])
        paste0(
          names_shown[i], "：其中 ", found, " 人有資料",
          if (missing > 0) paste0("，", missing, " 人沒有（他們在這一波的欄位會是空白）"),
          if (extra > 0) {
            paste0("；另有 ", extra, " 人不在 ", first, "，依「向後追蹤樣本」沒有納入")
          },
          "。"
        )
      }, character(1))
    ),
    inner_join = c(
      paste0("共有 ", stats$n_in_all, " 人在每個檔案都有資料，結果只保留這些人。"),
      vapply(seq_along(sets), function(i) {
        dropped <- length(sets[[i]]) - stats$n_in_all
        if (dropped == 0) return(NA_character_)
        paste0(names_shown[i], "有 ", dropped, " 人因為不是每個檔案都有而被排除。")
      }, character(1))
    ),
    full_join = paste0(
      "所有檔案合計出現過 ", stats$n_in_any, " 人，全部保留；其中 ", stats$n_in_all,
      " 人每個檔案都有資料，其他人在缺席波次的欄位會是空白。"
    )
  )
  n_na <- sum(stats$n_na_key)
  c(
    out[!is.na(out)],
    if (n_na > 0) paste0("有 ", n_na, " 列的 ID 是空白，無法比對，這些列不會和其他檔案配對。"),
    paste0("結果共 ", format_dims(nrow(result), ncol(result)), "。"),
    if (stats$any_dup) "因為有 ID 重複（見上方提醒），列數會比人數多。"
  )
}

#' ID 重複時的提醒（每個有重複的檔案一句）
#' @noRd
dup_key_warnings <- function(stats, names_shown) {
  idx <- which(stats$n_dup > 0)
  vapply(idx, function(i) {
    paste0(
      names_shown[i], "有 ", stats$n_dup[i], " 個 ID 出現不只一次",
      "（例：", paste(stats$dup_examples[[i]], collapse = "；"), "），",
      "所以這些人在結果中會佔多列，不再是「每人一列」。",
      "請確認檔案是否重複匯出，或改選能唯一辨識每個人的欄位（例如再加上其他 ID 欄）。"
    )
  }, character(1))
}

#' 詳細核對表（進階）
#' @noRd
join_audit_tables <- function(stats, steps, names_shown, method) {
  yes_no <- function(ok) {
    ifelse(is.na(ok), "無法判斷（ID 有重複）", ifelse(ok, "符合", "不符"))
  }
  files <- tibble::tibble(
    順序 = seq_along(names_shown),
    檔案 = names_shown,
    列數 = stats$n_rows,
    欄位數 = stats$n_cols,
    不重複ID數 = stats$n_unique,
    重複ID數 = stats$n_dup,
    ID空白列數 = stats$n_na_key
  )
  step_tbl <- tibble::tibble(
    步驟 = paste0(
      "第 ", seq_along(steps), " 步：",
      c(names_shown[1], rep("（前面的合併結果）", length(steps) - 1)),
      " ＋ ", names_shown[-1]
    ),
    合併前列數 = purrr::map_int(steps, "n_left"),
    加入檔案列數 = purrr::map_int(steps, "n_right"),
    合併後列數 = purrr::map_int(steps, "n_out"),
    預期欄位數 = purrr::map_int(steps, "ncol_expected"),
    實際欄位數 = purrr::map_int(steps, "ncol_out"),
    欄位數核對 = yes_no(purrr::map_int(steps, "ncol_expected") ==
      purrr::map_int(steps, "ncol_out"))
  )
  rule <- switch(method,
    left_join = "向後追蹤樣本：後面檔案的 ID 都不重複時，列數應等於第一個檔案的列數。",
    inner_join = "全勤樣本：ID 都不重複時，列數應等於每個檔案都有的人數。",
    full_join = "全樣本：ID 都不重複時，列數應等於出現過的總人數。"
  )
  checks <- tibble::tibble(
    檢查項目 = c("列數", "欄位數"),
    結果 = c(stats$nrow_out, stats$ncol_out),
    預期 = c(stats$nrow_expected, stats$ncol_expected),
    核對 = yes_no(c(stats$nrow_ok, stats$ncol_ok))
  )
  list(files = files, steps = step_tbl, checks = checks, rule = rule)
}
