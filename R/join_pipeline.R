# 橫向資料串接：每人一列，各波變項加上後綴並排

# 串接結果（列數 × 欄數）超過這個格數就直接拒絕：多對多暴增時 R 會用光記憶體
KIT_JOIN_MAX_CELLS <- 2e8

#' 橫向資料串接（含檢查、核對與白話摘要）
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
#' @param id_normalize ID 比對前的標準化（可複選）："zeros" 去掉前導零（001 和 1 是同一人）、
#'   "case" 不分大小寫；有選時 ID 一律先轉成文字。不選時，標準化後能多配到人就提醒。
#' @param constant_cols 不分波次的欄位（性別、生日…）：不加後綴，只留一欄，
#'   取最早有值的那一波；各波的值不一樣時提醒
#' @return list(result, audit, summary, people, warnings, notes)：summary 是狀態框裡的重點
#'   （幾句話），people 是「各檔案的人數」小表
#' @noRd
join_waves <- function(dfs, file_names, wave_labels, by_vars,
                       method = "left_join", keys_as_text = TRUE,
                       confirm_large = FALSE, id_cols = NULL,
                       id_normalize = character(0), constant_cols = character(0)) {
  method <- match.arg(method, unname(KIT_JOIN_METHODS))
  id_cols <- id_cols %||% rep(list(by_vars), length(dfs))
  check_join_inputs(dfs, file_names, wave_labels, by_vars, id_cols)
  names_shown <- file_display_name(wave_labels, file_names)
  id_normalize <- intersect(id_normalize, c("zeros", "case"))
  constant_cols <- setdiff(constant_cols, by_vars)

  notes <- id_rename_note(id_cols, by_vars, names_shown)
  warnings <- character(0)
  dfs <- unify_id_names(dfs, id_cols, by_vars, names_shown)
  dfs <- clean_keys(dfs, by_vars)
  if (length(id_normalize) > 0) {
    dfs <- normalize_keys(dfs, by_vars, id_normalize)
    notes <- c(notes, id_normalize_note(id_normalize))
  } else {
    gain <- id_normalize_gain(dfs, by_vars)
    if (gain > 0) {
      warnings <- c(warnings, paste0(
        # 原文：有  |  個 ID 只差在前導零或大小寫（例如 1 和 001、a01 和 A01），
        "\u6709 ", kit_num(gain), " \u500b ID \u53ea\u5dee\u5728\u524d\u5c0e\u96f6\u6216\u5927\u5c0f\u5beb\uff08\u4f8b\u5982 1 \u548c 001\u3001a01 \u548c A01\uff09\uff0c",
        # 原文：照現在的設定會當成不同的人。如果是同一個人，到「進階設定」勾「ID 比對時去掉前導零」
        "\u7167\u73fe\u5728\u7684\u8a2d\u5b9a\u6703\u7576\u6210\u4e0d\u540c\u7684\u4eba\u3002\u5982\u679c\u662f\u540c\u4e00\u500b\u4eba\uff0c\u5230\u300c\u9032\u968e\u8a2d\u5b9a\u300d\u52fe\u300cID \u6bd4\u5c0d\u6642\u53bb\u6389\u524d\u5c0e\u96f6\u300d",
        # 原文：或「ID 比對時不分大小寫」再執行。
        "\u6216\u300cID \u6bd4\u5c0d\u6642\u4e0d\u5206\u5927\u5c0f\u5beb\u300d\u518d\u57f7\u884c\u3002"
      ))
    }
  }
  conflict <- key_type_conflict(dfs, by_vars, names_shown, id_cols)
  if (!is.null(conflict)) {
    if (!keys_as_text) {
      kit_abort(
        # 原文：無法直接比對。請勾選「ID 型別不一致時，自動轉成文字再比對」，
        conflict, "\u7121\u6cd5\u76f4\u63a5\u6bd4\u5c0d\u3002\u8acb\u52fe\u9078\u300cID \u578b\u5225\u4e0d\u4e00\u81f4\u6642\uff0c\u81ea\u52d5\u8f49\u6210\u6587\u5b57\u518d\u6bd4\u5c0d\u300d\uff0c",
        # 原文：或回原檔把 ID 改成同一種格式。
        "\u6216\u56de\u539f\u6a94\u628a ID \u6539\u6210\u540c\u4e00\u7a2e\u683c\u5f0f\u3002"
      )
    }
    dfs <- keys_to_text(dfs, by_vars)
    # 原文：已自動把 ID 轉成文字再比對。
    notes <- c(notes, paste0(conflict, "\u5df2\u81ea\u52d5\u628a ID \u8f49\u6210\u6587\u5b57\u518d\u6bd4\u5c0d\u3002"))
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
  summary <- join_plain_summary(stats, names_shown, method, result)
  # 核對（逐步合併的欄數）照加後綴的結果算；不分波次的欄位最後才合成一欄
  if (length(constant_cols) > 0) {
    merged <- merge_constant_cols(result, constant_cols, join_suffix(wave_labels), by_vars)
    result <- merged$result
    notes <- c(notes, merged$note)
    warnings <- c(warnings, merged$warnings)
  }
  list(
    result = result,
    audit = join_audit_tables(stats, steps, names_shown, method),
    summary = summary,
    people = join_people_table(stats, names_shown, method),
    warnings = c(dup_key_warnings(stats, names_shown), warnings),
    notes = notes
  )
}

#' ID 標準化：去掉前導零（"001" → "1"，"000" → "0"）、不分大小寫（一律大寫）
#' @noRd
normalize_id_text <- function(x, how) {
  v <- as.vector(unclass(x))
  if ("zeros" %in% how) v <- sub("^0+(?=[0-9A-Za-z])", "", v, perl = TRUE)
  if ("case" %in% how) v <- toupper(v)
  attributes(v) <- attributes(x)
  v
}

#' 各檔的 ID 欄先轉成文字，再標準化
#' @noRd
normalize_keys <- function(dfs, by_vars, how) {
  if (length(how) == 0) return(dfs)
  purrr::map(keys_to_text(dfs, by_vars), function(df) {
    for (col in by_vars) df[[col]] <- normalize_id_text(df[[col]], how)
    df
  })
}

#' 標準化說明（狀態框的附註）
#' @noRd
id_normalize_note <- function(how) {
  # 原文：去掉前導零（001 和 1 當成同一人） | 不分大小寫（a01 和 A01 當成同一人）
  what <- c(zeros = "\u53bb\u6389\u524d\u5c0e\u96f6\uff08001 \u548c 1 \u7576\u6210\u540c\u4e00\u4eba\uff09", case = "\u4e0d\u5206\u5927\u5c0f\u5beb\uff08a01 \u548c A01 \u7576\u6210\u540c\u4e00\u4eba\uff09")
  paste0(
    # 原文：ID 比對前已 | 、
    "ID \u6bd4\u5c0d\u524d\u5df2", paste(what[how], collapse = "\u3001"),
    # 原文：；結果裡的 ID 是標準化後的寫法
    "\uff1b\u7d50\u679c\u88e1\u7684 ID \u662f\u6a19\u6e96\u5316\u5f8c\u7684\u5beb\u6cd5",
    # 原文：（英文字母都是大寫） | 。
    if ("case" %in% how) "\uff08\u82f1\u6587\u5b57\u6bcd\u90fd\u662f\u5927\u5beb\uff09", "\u3002"
  )
}

#' 去掉前導零、不分大小寫以後，第 2 個以後的檔案能多配到第 1 個檔案幾個 ID
#' @noRd
id_normalize_gain <- function(dfs, by_vars) {
  matched <- function(d) {
    keys <- lapply(d, key_strings, by_vars = by_vars)
    first <- unique(keys[[1]][!is.na(keys[[1]])])
    sum(vapply(keys[-1], function(k) sum(unique(k[!is.na(k)]) %in% first), integer(1)))
  }
  tryCatch(
    matched(normalize_keys(dfs, by_vars, c("zeros", "case"))) - matched(keys_to_text(dfs, by_vars)),
    error = function(e) 0L
  )
}

#' 不分波次的欄位合成一欄：取最早有值的那一波，放在 ID 欄後面；各波值不同時提醒
#'
#' @param suffixes 各波的欄名後綴（"_t1"…）
#' @return list(result, note, warnings)
#' @noRd
merge_constant_cols <- function(df, cols, suffixes, by_vars) {
  done <- character(0)
  warnings <- character(0)
  for (col in cols) {
    wide <- paste0(col, suffixes)
    wide <- wide[wide %in% names(df)]
    if (length(wide) == 0 || col %in% setdiff(names(df), wide)) next
    vals <- df[wide]
    as_text <- !all(vapply(vals, is.numeric, logical(1)))
    out <- vals[[1]]
    if (as_text) out <- num_to_text(unclass(out)) else out <- as.vector(unclass(out))
    for (w in wide[-1]) {
      v <- if (as_text) num_to_text(unclass(vals[[w]])) else as.vector(unclass(vals[[w]]))
      fill <- is.na(out) & !is.na(v)
      out[fill] <- v[fill]
    }
    # 各波的值不一樣的人數（只算有值的波）
    txt <- vapply(vals, function(v) num_to_text(unclass(v)), character(nrow(df)))
    if (is.null(dim(txt))) txt <- matrix(txt, nrow = nrow(df))
    differ <- apply(txt, 1, function(r) length(unique(r[!is.na(r)])) > 1)
    if (any(differ)) {
      warnings <- c(warnings, paste0(
        # 原文：：有  |  人各波的值不一樣，結果取最早有值的那一波。
        col, "\uff1a\u6709 ", kit_num(sum(differ)), " \u4eba\u5404\u6ce2\u7684\u503c\u4e0d\u4e00\u6a23\uff0c\u7d50\u679c\u53d6\u6700\u65e9\u6709\u503c\u7684\u90a3\u4e00\u6ce2\u3002"
      ))
    }
    first <- vals[[1]]
    if (!as_text && haven::is.labelled(first)) {
      out <- haven::labelled(out, labels = attr(first, "labels", exact = TRUE))
    }
    attr(out, "label") <- attr(first, "label", exact = TRUE)
    df[[col]] <- out
    df <- df[, setdiff(names(df), wide), drop = FALSE]
    done <- c(done, col)
  }
  if (length(done) > 0) {
    keep <- intersect(by_vars, names(df))
    df <- df[, c(keep, done, setdiff(names(df), c(keep, done))), drop = FALSE]
  }
  note <- if (length(done) > 0) {
    paste0(
      # 原文：不分波次的直欄只留一個直欄、不加後綴，取最早有值的那一波：
      "\u4e0d\u5206\u6ce2\u6b21\u7684\u76f4\u6b04\u53ea\u7559\u4e00\u500b\u76f4\u6b04\u3001\u4e0d\u52a0\u5f8c\u7db4\uff0c\u53d6\u6700\u65e9\u6709\u503c\u7684\u90a3\u4e00\u6ce2\uff1a",
      # 原文：、 | 。
      paste(done, collapse = "\u3001"), "\u3002"
    )
  }
  list(result = df, note = note, warnings = warnings)
}

#' 串接前檢查：檔案數、ID 欄位、波次名稱
#' @noRd
check_join_inputs <- function(dfs, file_names, wave_labels, by_vars,
                              id_cols = rep(list(by_vars), length(dfs))) {
  if (length(dfs) < 2) {
    # 原文：需要至少 2 個檔案才能串接。請到「檔案上傳」再加入一個波次的檔案。
    kit_abort("\u9700\u8981\u81f3\u5c11 2 \u500b\u6a94\u6848\u624d\u80fd\u4e32\u63a5\u3002\u8acb\u5230\u300c\u6a94\u6848\u4e0a\u50b3\u300d\u518d\u52a0\u5165\u4e00\u500b\u6ce2\u6b21\u7684\u6a94\u6848\u3002")
  }
  if (length(by_vars) == 0) {
    # 原文：請先選擇用來辨識同一個人的 ID 直欄。
    kit_abort("\u8acb\u5148\u9078\u64c7\u7528\u4f86\u8fa8\u8b58\u540c\u4e00\u500b\u4eba\u7684 ID \u76f4\u6b04\u3002")
  }
  per_file <- !all(purrr::map_lgl(id_cols, identical, by_vars))
  for (i in seq_along(dfs)) {
    if (length(id_cols[[i]]) != length(by_vars)) {
      kit_abort(
        # 原文：每個檔案要選一樣多個 ID 直欄：第 1 個檔案選了  |  個，
        "\u6bcf\u500b\u6a94\u6848\u8981\u9078\u4e00\u6a23\u591a\u500b ID \u76f4\u6b04\uff1a\u7b2c 1 \u500b\u6a94\u6848\u9078\u4e86 ", length(by_vars), " \u500b\uff0c",
        # 原文： 選了  |  個。
        file_names[i], " \u9078\u4e86 ", length(id_cols[[i]]), " \u500b\u3002"
      )
    }
    missing_by <- setdiff(id_cols[[i]], names(dfs[[i]]))
    if (length(missing_by) > 0) {
      kit_abort(
        # 原文： 沒有  | 、 |  直欄。
        file_names[i], " \u6c92\u6709 ", paste(missing_by, collapse = "\u3001"), " \u76f4\u6b04\u3002",
        if (per_file) {
          # 原文：請重新選這個檔案的 ID 直欄。
          "\u8acb\u91cd\u65b0\u9078\u9019\u500b\u6a94\u6848\u7684 ID \u76f4\u6b04\u3002"
        } else {
          # 原文：請改選每個檔案都有的 ID 直欄；各檔 ID 直欄名稱不同時，請勾選「各檔的 ID 直欄名稱不同」分別指定。
          "\u8acb\u6539\u9078\u6bcf\u500b\u6a94\u6848\u90fd\u6709\u7684 ID \u76f4\u6b04\uff1b\u5404\u6a94 ID \u76f4\u6b04\u540d\u7a31\u4e0d\u540c\u6642\uff0c\u8acb\u52fe\u9078\u300c\u5404\u6a94\u7684 ID \u76f4\u6b04\u540d\u7a31\u4e0d\u540c\u300d\u5206\u5225\u6307\u5b9a\u3002"
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
        # 原文：已經有名為  | 、
        names_shown[i], "\u5df2\u7d93\u6709\u540d\u70ba ", paste(taken, collapse = "\u3001"),
        # 原文： 的直欄，但它不是這個檔案的 ID，所以不能把  | 、
        " \u7684\u76f4\u6b04\uff0c\u4f46\u5b83\u4e0d\u662f\u9019\u500b\u6a94\u6848\u7684 ID\uff0c\u6240\u4ee5\u4e0d\u80fd\u628a ", paste(own, collapse = "\u3001"),
        # 原文： 改成這個名稱。請把第 1 個檔案的 ID 直欄改選成其他直欄，或先在原檔改直欄名稱。
        " \u6539\u6210\u9019\u500b\u540d\u7a31\u3002\u8acb\u628a\u7b2c 1 \u500b\u6a94\u6848\u7684 ID \u76f4\u6b04\u6539\u9078\u6210\u5176\u4ed6\u76f4\u6b04\uff0c\u6216\u5148\u5728\u539f\u6a94\u6539\u76f4\u6b04\u540d\u7a31\u3002"
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
    # 原文：各檔的 ID 已對應到  | ＋ | ：
    "\u5404\u6a94\u7684 ID \u5df2\u5c0d\u61c9\u5230 ", paste(by_vars, collapse = "\uff0b"), "\uff1a",
    paste0(
      # 原文：的 
      names_shown[differs], "\u7684 ",
      # 原文：＋
      purrr::map_chr(id_cols[differs], paste, collapse = "\uff0b"),
      # 原文：、
      collapse = "\u3001"
    ),
    # 原文：。結果中的 ID 直欄叫  | 、 | 。
    "\u3002\u7d50\u679c\u4e2d\u7684 ID \u76f4\u6b04\u53eb ", paste(by_vars, collapse = "\u3001"), "\u3002"
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
    # 原文：預估串接結果有  |  橫列，
    "\u9810\u4f30\u4e32\u63a5\u7d50\u679c\u6709 ", format(estimate, scientific = FALSE, big.mark = ","), " \u6a6b\u5217\uff0c",
    # 原文：是最大檔案（ | ， |  橫列）的 
    "\u662f\u6700\u5927\u6a94\u6848\uff08", names_shown[biggest], "\uff0c", kit_num(n_rows[biggest]), " \u6a6b\u5217\uff09\u7684 ",
    # 原文： 倍。
    kit_num(times), " \u500d\u3002",
    # 原文：這通常表示 ID 直欄選錯（例如選到出生年、權數），或 ID 大量重複：
    "\u9019\u901a\u5e38\u8868\u793a ID \u76f4\u6b04\u9078\u932f\uff08\u4f8b\u5982\u9078\u5230\u51fa\u751f\u5e74\u3001\u6b0a\u6578\uff09\uff0c\u6216 ID \u5927\u91cf\u91cd\u8907\uff1a",
    # 原文：同一個 ID 在兩個檔案各有多橫列時，會兩兩配對成很多橫列。
    "\u540c\u4e00\u500b ID \u5728\u5169\u500b\u6a94\u6848\u5404\u6709\u591a\u6a6b\u5217\u6642\uff0c\u6703\u5169\u5169\u914d\u5c0d\u6210\u5f88\u591a\u6a6b\u5217\u3002"
  )
  if (estimate * ncol_out > KIT_JOIN_MAX_CELLS) {
    # 原文：結果太大，無法產生。請改選每個人都不同的 ID 直欄。
    kit_abort(why, "\u7d50\u679c\u592a\u5927\uff0c\u7121\u6cd5\u7522\u751f\u3002\u8acb\u6539\u9078\u6bcf\u500b\u4eba\u90fd\u4e0d\u540c\u7684 ID \u76f4\u6b04\u3002")
  }
  if (!confirm_large) {
    kit_abort(
      # 原文：請先確認 ID 直欄；確定沒問題再按「仍要執行」。
      why, "\u8acb\u5148\u78ba\u8a8d ID \u76f4\u6b04\uff1b\u78ba\u5b9a\u6c92\u554f\u984c\u518d\u6309\u300c\u4ecd\u8981\u57f7\u884c\u300d\u3002",
      class = "kit_join_too_large", data = list(estimate = estimate)
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
  # 原文：（不重複  | ）
  shown <- paste0(cols, "\uff08\u4e0d\u91cd\u8907 ", format_unique_pct(ratio), "\uff09")
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
    # 原文：檔案
    "\u6a94\u6848" = names_shown,
    # 原文：ID直欄 | ＋
    "ID\u76f4\u6b04" = purrr::map_chr(id_cols, paste, collapse = "\uff0b"),
    # 原文：橫列數
    "\u6a6b\u5217\u6578" = m[, 1],
    # 原文：不重複ID數
    "\u4e0d\u91cd\u8907ID\u6578" = m[, 2],
    # 原文：重複的ID數
    "\u91cd\u8907\u7684ID\u6578" = m[, 3],
    # 原文：ID空白橫列數
    "ID\u7a7a\u767d\u6a6b\u5217\u6578" = m[, 4]
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
    # 原文：因為它會加在直欄名稱後面（例如 score_t1），用來分辨各波的變項。
    "\u56e0\u70ba\u5b83\u6703\u52a0\u5728\u76f4\u6b04\u540d\u7a31\u5f8c\u9762\uff08\u4f8b\u5982 score_t1\uff09\uff0c\u7528\u4f86\u5206\u8fa8\u5404\u6ce2\u7684\u8b8a\u9805\u3002"
  } else {
    # 原文：因為它會填進 wave 直欄，用來分辨每一個橫列屬於哪一波。
    "\u56e0\u70ba\u5b83\u6703\u586b\u9032 wave \u76f4\u6b04\uff0c\u7528\u4f86\u5206\u8fa8\u6bcf\u4e00\u500b\u6a6b\u5217\u5c6c\u65bc\u54ea\u4e00\u6ce2\u3002"
  }
  empty <- which(!nzchar(trimws(wave_labels)))
  if (length(empty) > 0) {
    return(paste0(
      # 原文：第  |  個檔案（ | ）沒有填波次名稱。
      "\u7b2c ", empty[1], " \u500b\u6a94\u6848\uff08", file_names[empty[1]], "\uff09\u6c92\u6709\u586b\u6ce2\u6b21\u540d\u7a31\u3002",
      # 原文：請到「檔案上傳」填寫，例如 t | ；每個檔案都需要波次名稱，
      "\u8acb\u5230\u300c\u6a94\u6848\u4e0a\u50b3\u300d\u586b\u5beb\uff0c\u4f8b\u5982 t", empty[1], "\uff1b\u6bcf\u500b\u6a94\u6848\u90fd\u9700\u8981\u6ce2\u6b21\u540d\u7a31\uff0c", why
    ))
  }
  normalized <- join_suffix(wave_labels)
  dup <- unique(wave_labels[duplicated(normalized)])
  if (length(dup) > 0) {
    return(paste0(
      # 原文：波次名稱「 | 」重複了。請到「檔案上傳」把每個檔案的波次名稱改成不同的，
      "\u6ce2\u6b21\u540d\u7a31\u300c", dup[1], "\u300d\u91cd\u8907\u4e86\u3002\u8acb\u5230\u300c\u6a94\u6848\u4e0a\u50b3\u300d\u628a\u6bcf\u500b\u6a94\u6848\u7684\u6ce2\u6b21\u540d\u7a31\u6539\u6210\u4e0d\u540c\u7684\uff0c",
      why
    ))
  }
  # 只差在大小寫（T1、t1）：SPSS 欄名不分大小寫，匯出 SAV 時會被改名；看起來也容易混淆
  case_dup <- which(duplicated(tolower(normalized)))
  if (length(case_dup) > 0) {
    other <- wave_labels[match(tolower(normalized[case_dup[1]]), tolower(normalized))]
    return(paste0(
      # 原文：波次名稱「 | 」和「 | 」只差在大小寫，
      "\u6ce2\u6b21\u540d\u7a31\u300c", other, "\u300d\u548c\u300c", wave_labels[case_dup[1]], "\u300d\u53ea\u5dee\u5728\u5927\u5c0f\u5beb\uff0c",
      # 原文：容易混淆，而且 SPSS 不分大小寫，匯出 SAV 時直欄名稱會衝突。
      "\u5bb9\u6613\u6df7\u6dc6\uff0c\u800c\u4e14 SPSS \u4e0d\u5206\u5927\u5c0f\u5beb\uff0c\u532f\u51fa SAV \u6642\u76f4\u6b04\u540d\u7a31\u6703\u885d\u7a81\u3002",
      # 原文：請到「檔案上傳」改成明顯不同的名稱。
      "\u8acb\u5230\u300c\u6a94\u6848\u4e0a\u50b3\u300d\u6539\u6210\u660e\u986f\u4e0d\u540c\u7684\u540d\u7a31\u3002"
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
    # 原文：數字
    "\u6578\u5b57"
  } else if (is.character(x) || is.factor(x)) {
    # 原文：文字
    "\u6587\u5b57"
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
        # 原文：的  |  是 | ，
        names_shown[1], "\u7684 ", id_cols[[1]][k], " \u662f", kinds[1], "\uff0c",
        # 原文：的  |  是 | ，
        names_shown[first], "\u7684 ", id_cols[[first]][k], " \u662f", kinds[first], "\uff0c"
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
    # 原文：、
    pairs <- paste(by_vars, "=", values[[i]], collapse = "\u3001")
    # 原文：（ |  次）
    paste0(pairs, "\uff08", kit_num(shown[[i]]), " \u6b21\uff09")
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

#' 狀態框裡的重點：誰被留下（整體一兩句；各檔案的人數在 join_people_table()）
#' @return 字元向量，每個元素一句
#' @noRd
join_plain_summary <- function(stats, names_shown, method, result) {
  sets <- stats$key_sets
  first <- names_shown[1]
  n <- kit_num
  out <- switch(method,
    left_join = {
      later <- unique(unlist(sets[-1]))
      extra <- sum(!later %in% sets[[1]])
      paste0(
        # 原文：以  | 的  |  人為準，其中 
        "\u4ee5 ", first, "\u7684 ", n(length(sets[[1]])), " \u4eba\u70ba\u6e96\uff0c\u5176\u4e2d ", n(stats$n_in_all),
        # 原文： 人每個檔案都有資料；在某一波沒有資料的人，那一波的直欄是空白
        " \u4eba\u6bcf\u500b\u6a94\u6848\u90fd\u6709\u8cc7\u6599\uff1b\u5728\u67d0\u4e00\u6ce2\u6c92\u6709\u8cc7\u6599\u7684\u4eba\uff0c\u90a3\u4e00\u6ce2\u7684\u76f4\u6b04\u662f\u7a7a\u767d",
        # 原文：。另有  |  人不在  | ，依「向後追蹤樣本」沒有納入
        if (extra > 0) paste0("\u3002\u53e6\u6709 ", n(extra), " \u4eba\u4e0d\u5728 ", first, "\uff0c\u4f9d\u300c\u5411\u5f8c\u8ffd\u8e64\u6a23\u672c\u300d\u6c92\u6709\u7d0d\u5165"),
        # 原文：。
        "\u3002"
      )
    },
    inner_join = paste0(
      # 原文：共有  |  人在每個檔案都有資料，結果只保留這些人；
      "\u5171\u6709 ", n(stats$n_in_all), " \u4eba\u5728\u6bcf\u500b\u6a94\u6848\u90fd\u6709\u8cc7\u6599\uff0c\u7d50\u679c\u53ea\u4fdd\u7559\u9019\u4e9b\u4eba\uff1b",
      # 原文：其他  |  人因為不是每個檔案都有而沒有納入。
      "\u5176\u4ed6 ", n(stats$n_in_any - stats$n_in_all), " \u4eba\u56e0\u70ba\u4e0d\u662f\u6bcf\u500b\u6a94\u6848\u90fd\u6709\u800c\u6c92\u6709\u7d0d\u5165\u3002"
    ),
    full_join = paste0(
      # 原文：所有檔案合計出現過  |  人，全部保留；其中 
      "\u6240\u6709\u6a94\u6848\u5408\u8a08\u51fa\u73fe\u904e ", n(stats$n_in_any), " \u4eba\uff0c\u5168\u90e8\u4fdd\u7559\uff1b\u5176\u4e2d ", n(stats$n_in_all),
      # 原文： 人每個檔案都有資料，其他人在缺席波次的直欄會是空白。
      " \u4eba\u6bcf\u500b\u6a94\u6848\u90fd\u6709\u8cc7\u6599\uff0c\u5176\u4ed6\u4eba\u5728\u7f3a\u5e2d\u6ce2\u6b21\u7684\u76f4\u6b04\u6703\u662f\u7a7a\u767d\u3002"
    )
  )
  n_na <- sum(stats$n_na_key)
  c(
    out,
    # 原文：有  |  橫列的 ID 是空白，無法比對，這些橫列不會和其他檔案配對。
    if (n_na > 0) paste0("\u6709 ", n(n_na), " \u6a6b\u5217\u7684 ID \u662f\u7a7a\u767d\uff0c\u7121\u6cd5\u6bd4\u5c0d\uff0c\u9019\u4e9b\u6a6b\u5217\u4e0d\u6703\u548c\u5176\u4ed6\u6a94\u6848\u914d\u5c0d\u3002"),
    # 原文：因為有 ID 重複（見上方提醒），橫列數會比人數多。
    if (stats$any_dup) "\u56e0\u70ba\u6709 ID \u91cd\u8907\uff08\u898b\u4e0a\u65b9\u63d0\u9192\uff09\uff0c\u6a6b\u5217\u6578\u6703\u6bd4\u4eba\u6578\u591a\u3002"
  )
}

#' 各檔案的人數：每個檔案有幾人、在結果中有資料的、這一波空白的、沒有納入的
#'
#' 「結果中這一波空白」= 結果裡有這個人、但這個檔案沒有他（那一波的欄位是空白）。
#' 「沒有納入」= 這個檔案有他、但結果沒有（向後追蹤樣本：不在第一個檔案；全勤樣本：不是每個檔案都有）。
#' @noRd
join_people_table <- function(stats, names_shown, method) {
  sets <- stats$key_sets
  kept <- switch(method,
    left_join = sets[[1]],
    inner_join = Reduce(intersect, sets),
    full_join = Reduce(union, sets)
  )
  tibble::tibble(
    # 原文：檔案
    "\u6a94\u6848" = names_shown,
    # 原文：人數
    "\u4eba\u6578" = lengths(sets),
    # 原文：結果中有資料
    "\u7d50\u679c\u4e2d\u6709\u8cc7\u6599" = purrr::map_int(sets, function(s) sum(kept %in% s)),
    # 原文：結果中這一波空白
    "\u7d50\u679c\u4e2d\u9019\u4e00\u6ce2\u7a7a\u767d" = purrr::map_int(sets, function(s) sum(!kept %in% s)),
    # 原文：沒有納入
    "\u6c92\u6709\u7d0d\u5165" = purrr::map_int(sets, function(s) sum(!s %in% kept))
  )
}

#' ID 重複時的提醒（每個有重複的檔案一句）
#' @noRd
dup_key_warnings <- function(stats, names_shown) {
  idx <- which(stats$n_dup > 0)
  vapply(idx, function(i) {
    paste0(
      # 原文：有  |  個 ID 出現不只一次
      names_shown[i], "\u6709 ", kit_num(stats$n_dup[i]), " \u500b ID \u51fa\u73fe\u4e0d\u53ea\u4e00\u6b21",
      # 原文：（例： | ； | ），
      "\uff08\u4f8b\uff1a", paste(stats$dup_examples[[i]], collapse = "\uff1b"), "\uff09\uff0c",
      # 原文：所以這些人在結果中會佔多橫列，不再是「每人一個橫列」。
      "\u6240\u4ee5\u9019\u4e9b\u4eba\u5728\u7d50\u679c\u4e2d\u6703\u4f54\u591a\u6a6b\u5217\uff0c\u4e0d\u518d\u662f\u300c\u6bcf\u4eba\u4e00\u500b\u6a6b\u5217\u300d\u3002",
      # 原文：請確認檔案是否重複匯出，或改選能唯一辨識每個人的直欄（例如再加上其他 ID 直欄）。
      "\u8acb\u78ba\u8a8d\u6a94\u6848\u662f\u5426\u91cd\u8907\u532f\u51fa\uff0c\u6216\u6539\u9078\u80fd\u552f\u4e00\u8fa8\u8b58\u6bcf\u500b\u4eba\u7684\u76f4\u6b04\uff08\u4f8b\u5982\u518d\u52a0\u4e0a\u5176\u4ed6 ID \u76f4\u6b04\uff09\u3002"
    )
  }, character(1))
}

#' 詳細核對表（進階）
#' @noRd
join_audit_tables <- function(stats, steps, names_shown, method) {
  yes_no <- function(ok) {
    # 原文：無法判斷（ID 有重複） | 符合 | 不符
    ifelse(is.na(ok), "\u7121\u6cd5\u5224\u65b7\uff08ID \u6709\u91cd\u8907\uff09", ifelse(ok, "\u7b26\u5408", "\u4e0d\u7b26"))
  }
  files <- tibble::tibble(
    # 原文：順序
    "\u9806\u5e8f" = seq_along(names_shown),
    # 原文：檔案
    "\u6a94\u6848" = names_shown,
    # 原文：橫列數
    "\u6a6b\u5217\u6578" = stats$n_rows,
    # 原文：直欄數
    "\u76f4\u6b04\u6578" = stats$n_cols,
    # 原文：不重複ID數
    "\u4e0d\u91cd\u8907ID\u6578" = stats$n_unique,
    # 原文：重複ID數
    "\u91cd\u8907ID\u6578" = stats$n_dup,
    # 原文：ID空白橫列數
    "ID\u7a7a\u767d\u6a6b\u5217\u6578" = stats$n_na_key
  )
  step_tbl <- tibble::tibble(
    # 原文：步驟
    "\u6b65\u9a5f" = paste0(
      # 原文：第  |  步：
      "\u7b2c ", seq_along(steps), " \u6b65\uff1a",
      # 原文：（前面的合併結果）
      c(names_shown[1], rep("\uff08\u524d\u9762\u7684\u5408\u4f75\u7d50\u679c\uff09", length(steps) - 1)),
      # 原文： ＋ 
      " \uff0b ", names_shown[-1]
    ),
    # 原文：合併前橫列數
    "\u5408\u4f75\u524d\u6a6b\u5217\u6578" = purrr::map_int(steps, "n_left"),
    # 原文：加入檔案橫列數
    "\u52a0\u5165\u6a94\u6848\u6a6b\u5217\u6578" = purrr::map_int(steps, "n_right"),
    # 原文：合併後橫列數
    "\u5408\u4f75\u5f8c\u6a6b\u5217\u6578" = purrr::map_int(steps, "n_out"),
    # 原文：預期直欄數
    "\u9810\u671f\u76f4\u6b04\u6578" = purrr::map_int(steps, "ncol_expected"),
    # 原文：實際直欄數
    "\u5be6\u969b\u76f4\u6b04\u6578" = purrr::map_int(steps, "ncol_out"),
    # 原文：直欄數核對
    "\u76f4\u6b04\u6578\u6838\u5c0d" = yes_no(purrr::map_int(steps, "ncol_expected") ==
      purrr::map_int(steps, "ncol_out"))
  )
  rule <- switch(method,
    # 原文：向後追蹤樣本：後面檔案的 ID 都不重複時，橫列數應等於第一個檔案的橫列數。
    left_join = "\u5411\u5f8c\u8ffd\u8e64\u6a23\u672c\uff1a\u5f8c\u9762\u6a94\u6848\u7684 ID \u90fd\u4e0d\u91cd\u8907\u6642\uff0c\u6a6b\u5217\u6578\u61c9\u7b49\u65bc\u7b2c\u4e00\u500b\u6a94\u6848\u7684\u6a6b\u5217\u6578\u3002",
    # 原文：全勤樣本：ID 都不重複時，橫列數應等於每個檔案都有的人數。
    inner_join = "\u5168\u52e4\u6a23\u672c\uff1aID \u90fd\u4e0d\u91cd\u8907\u6642\uff0c\u6a6b\u5217\u6578\u61c9\u7b49\u65bc\u6bcf\u500b\u6a94\u6848\u90fd\u6709\u7684\u4eba\u6578\u3002",
    # 原文：全樣本：ID 都不重複時，橫列數應等於出現過的總人數。
    full_join = "\u5168\u6a23\u672c\uff1aID \u90fd\u4e0d\u91cd\u8907\u6642\uff0c\u6a6b\u5217\u6578\u61c9\u7b49\u65bc\u51fa\u73fe\u904e\u7684\u7e3d\u4eba\u6578\u3002"
  )
  checks <- tibble::tibble(
    # 原文：檢查項目 | 橫列數 | 直欄數
    "\u6aa2\u67e5\u9805\u76ee" = c("\u6a6b\u5217\u6578", "\u76f4\u6b04\u6578"),
    # 原文：結果
    "\u7d50\u679c" = c(stats$nrow_out, stats$ncol_out),
    # 原文：預期
    "\u9810\u671f" = c(stats$nrow_expected, stats$ncol_expected),
    # 原文：核對
    "\u6838\u5c0d" = yes_no(c(stats$nrow_ok, stats$ncol_ok))
  )
  list(files = files, steps = step_tbl, checks = checks, rule = rule)
}
