# ⑦ 長寬互轉：tidyr::pivot_wider() / pivot_longer() 的檢查與執行
#
# 設定一律用 tidyr 的參數名稱存成 list，介面、檢查、教學程式碼共用同一份。

# kitsUI 分數表（一列 = 一次施測 × 一個領域）一定有的欄位
KITSUI_SCORE_REQUIRED <- c("visit_id", "domain", "d_score", "scale", "bank")

# kitsUI 分數表預設的 id_cols（依序；只取資料中有的）
KITSUI_SCORE_ID_COLS <- c(
  "visit_id", "subj_id", "cohort", "wave_m", "age_m", "sex", "round"
)

#' pivot_wider 的設定（預設值同 tidyr）
#'
#' @param id_cols id 欄；NULL = tidyr 預設（其餘全部欄位）
#' @param names_from 新欄名來自哪一欄（一欄）
#' @param values_from 要攤開的值（可多欄）
#' @param names_prefix,names_sep,names_glue,names_vary 同 tidyr
#' @param values_fill 缺的格子填什麼：使用者輸入的文字；"" 或 NULL = 不填（NA）
#' @noRd
wider_args <- function(id_cols = NULL, names_from = character(0),
                       values_from = character(0), names_prefix = "",
                       names_sep = "_", names_glue = NULL,
                       names_vary = "fastest", values_fill = NULL) {
  blank <- function(x) is.null(x) || length(x) == 0 || !nzchar(x[1])
  list(
    id_cols = if (length(id_cols) == 0) NULL else id_cols,
    names_from = names_from %||% character(0),
    values_from = values_from %||% character(0),
    names_prefix = if (blank(names_prefix)) "" else names_prefix,
    names_sep = if (is.null(names_sep)) "_" else names_sep,
    names_glue = if (blank(names_glue)) NULL else names_glue,
    names_vary = if (blank(names_vary)) "fastest" else names_vary,
    values_fill = if (blank(values_fill)) NULL else values_fill
  )
}

#' pivot_longer 的設定（預設值同 tidyr）
#'
#' @param cols 要收起來的欄（cols_exclude = TRUE 時是不轉的欄）
#' @param cols_exclude TRUE = cols 以外的全部欄都轉（程式碼寫成 !c(...)）
#' @param names_to 欄名放到哪一欄（可多個，搭配 names_pattern）
#' @param values_to 值放到哪一欄
#' @param names_prefix,names_pattern 同 tidyr；"" = 不用
#' @param values_drop_na 是否丟掉值是 NA 的列
#' @noRd
longer_args <- function(cols = character(0), names_to = "name",
                        values_to = "value", names_prefix = NULL,
                        names_pattern = NULL, values_drop_na = FALSE,
                        cols_exclude = FALSE) {
  blank <- function(x) is.null(x) || length(x) == 0 || !nzchar(x[1])
  names_to <- if (blank(names_to)) "name" else names_to
  list(
    cols = cols %||% character(0),
    cols_exclude = isTRUE(cols_exclude),
    names_to = names_to[nzchar(names_to)],
    values_to = if (blank(values_to)) "value" else values_to,
    names_prefix = if (blank(names_prefix)) NULL else names_prefix,
    names_pattern = if (blank(names_pattern)) NULL else names_pattern,
    values_drop_na = isTRUE(values_drop_na)
  )
}

#' 實際用到的 id 欄：id_cols 為 NULL 時同 tidyr，取其餘全部欄位
#' @noRd
wider_id_cols <- function(data, args) {
  args$id_cols %||% setdiff(names(data), c(args$names_from, args$values_from))
}

#' 檢查 id 欄時用哪一欄代表「同一筆」：visit_id → 像 ID 的欄 → 第一欄
#' @noRd
pick_check_id <- function(id_cols) {
  if ("visit_id" %in% id_cols) return("visit_id")
  guessed <- guess_id_cols(id_cols)
  if (length(guessed) > 0) guessed[1] else utils::head(id_cols, 1)
}


# ── 檢查 ─────────────────────────────────────────────────────────────────────

#' id × names_from 是否唯一（pivot_wider 不彙總，重複就不能轉）
#'
#' @param data 資料框
#' @param keys id_cols 加上 names_from
#' @return list(ok, n_keys = 重複的組合數, rows = 重複的列；第一欄「列次」為原檔列次)
#' @noRd
check_key_unique <- function(data, keys) {
  counted <- data |>
    dplyr::mutate(列次 = dplyr::row_number(), .before = 1) |>
    dplyr::add_count(dplyr::across(dplyr::all_of(keys)), name = ".kit_n")
  rows <- counted[counted$.kit_n > 1, , drop = FALSE] |>
    dplyr::arrange(dplyr::across(dplyr::all_of(c(keys, "列次")))) |>
    dplyr::select("列次", dplyr::all_of(keys), 重複次數 = ".kit_n")
  n_keys <- nrow(dplyr::distinct(rows, dplyr::across(dplyr::all_of(keys))))
  list(ok = nrow(rows) == 0, n_keys = n_keys, rows = rows)
}

#' id_cols 中的其他欄，在同一個 id 內是否只有一個值
#'
#' 不一致時 pivot_wider 會把同一個 id 拆成好幾列（例如疊了兩個 round 的分數表，
#' round 放在 id_cols 會讓同一次施測變成兩列）。
#'
#' @param data 資料框
#' @param id 代表「同一筆」的欄（例如 visit_id）
#' @param cols id_cols 中要檢查的其他欄
#' @return list(ok, cols = 不一致的欄, n_ids = 受影響的 id 數, rows = 受影響的列)
#' @noRd
check_id_constant <- function(data, id, cols) {
  cols <- setdiff(intersect(cols, names(data)), id)
  empty <- list(ok = TRUE, cols = character(0), n_ids = 0L, rows = data[0, ])
  if (length(id) == 0 || length(cols) == 0) return(empty)
  n_values <- data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(id))) |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(cols), dplyr::n_distinct),
      .groups = "drop"
    )
  varies <- n_values[cols] > 1
  bad_cols <- cols[colSums(varies) > 0]
  if (length(bad_cols) == 0) return(empty)
  bad_ids <- n_values[rowSums(varies) > 0, id, drop = FALSE]
  rows <- data |>
    dplyr::mutate(列次 = dplyr::row_number(), .before = 1) |>
    dplyr::semi_join(bad_ids, by = id) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(c(id, "列次")))) |>
    dplyr::select("列次", dplyr::all_of(c(id, bad_cols)), dplyr::everything())
  list(ok = FALSE, cols = bad_cols, n_ids = nrow(bad_ids), rows = rows)
}

#' 轉換後的新欄名：與 id 欄撞名、彼此重複、不符合 SPSS 規則
#'
#' @return list(names = 新欄名, collide = 與 id 欄同名者, dup = 重複者,
#'   sav = SAV 會改名的新欄名，具名向量：原名 → 新名)
#' @noRd
check_wider_names <- function(data, args) {
  spec <- tidyr::build_wider_spec(
    data,
    names_from = dplyr::all_of(args$names_from),
    values_from = dplyr::all_of(args$values_from),
    names_prefix = args$names_prefix,
    names_sep = args$names_sep,
    names_glue = args$names_glue,
    names_vary = args$names_vary
  )
  new <- spec$.name
  id_cols <- wider_id_cols(data, args)
  renamed <- sav_renamed_cols(c(id_cols, new))
  list(
    names = new,
    collide = intersect(new, id_cols),
    dup = unique(new[duplicated(new)]),
    sav = renamed[names(renamed) %in% new]
  )
}

#' 使用者輸入的 values_fill 轉成 tidyr 的值
#'
#' 數字只填進數值欄、文字只填進文字欄（型別不合 tidyr 會報錯）；
#' 全部欄位都適用時回傳單一值，否則回傳具名 list。
#'
#' @return NULL（不填）、單一值或具名 list
#' @noRd
parse_values_fill <- function(text, data, values_from) {
  if (is.null(text) || !nzchar(trimws(text))) return(NULL)
  text <- trimws(text)
  num <- suppressWarnings(as.numeric(text))
  is_num <- !is.na(num)
  fits <- vapply(values_from, function(col) {
    x <- data[[col]]
    if (is_num) is.numeric(x) else is.character(x)
  }, logical(1))
  value <- if (is_num) num else text
  if (!any(fits)) return(NULL)
  if (all(fits)) return(value)
  stats::setNames(rep(list(value), sum(fits)), values_from[fits])
}

#' 每個 names_from 的值：空白格是「沒有這一列」還是「有列但值空白」
#'
#' 轉成寬格式後，兩種情況都是 NA；這張表讓使用者分得出來
#' （例如 kitsUI 分數表：沒有測這個領域 vs 有測但還沒放上 D 量尺）。
#'
#' @return tibble：names_from 的值、轉換後列數、沒有這一列、
#'   各 values_from 欄「有列但空白」的列數
#' @noRd
empty_cells_by_name <- function(data, id_cols, names_from, values_from) {
  n_out <- nrow(dplyr::distinct(data, dplyr::across(dplyr::all_of(id_cols))))
  levels <- unique(data[[names_from]])
  rows <- purrr::map(levels, function(lvl) {
    part <- data[data[[names_from]] %in% lvl, , drop = FALSE]
    present <- nrow(dplyr::distinct(part, dplyr::across(dplyr::all_of(id_cols))))
    empty <- purrr::map_int(values_from, function(v) sum(is.na(part[[v]])))
    names(empty) <- paste0("有列但空白：", values_from)
    tibble::as_tibble(c(
      stats::setNames(list(as.character(lvl)), names_from),
      list(轉換後列數 = n_out, 沒有這一列 = n_out - present),
      as.list(empty)
    ))
  })
  purrr::list_rbind(rows)
}


# ── 執行 ─────────────────────────────────────────────────────────────────────

#' 長轉寬（含檢查與白話摘要）
#'
#' 不做 values_fn：id × names_from 重複時直接停下，不默默彙總、不產生 list 欄。
#'
#' @param data 資料框
#' @param args wider_args() 的結果
#' @param check_id 檢查 id 欄時代表「同一筆」的欄；NULL = pick_check_id()
#' @return list(result, id_cols, summary, warnings, id_check, empty, names)
#' @noRd
reshape_wider <- function(data, args, check_id = NULL) {
  if (length(args$names_from) != 1) {
    kit_abort("請選一個「新欄名來自哪一欄（names_from）」，例如 domain。")
  }
  if (length(args$values_from) == 0) {
    kit_abort("請至少選一個「要攤開的值（values_from）」，例如 d_score。")
  }
  if (args$names_from %in% args$values_from) {
    kit_abort(args$names_from, " 不能同時是 names_from 和 values_from。")
  }
  id_cols <- wider_id_cols(data, args)
  overlap <- intersect(id_cols, c(args$names_from, args$values_from))
  if (length(overlap) > 0) {
    kit_abort(
      paste(overlap, collapse = "、"), " 同時在 id_cols 和 names_from／values_from，",
      "請從 id_cols 拿掉。"
    )
  }
  dup <- check_key_unique(data, c(id_cols, args$names_from))
  if (!dup$ok) {
    kit_abort(
      "有 ", dup$n_keys, " 組 id_cols × ", args$names_from, " 出現不只一次，",
      "同一格會有好幾個值，所以停下來沒有轉換（本工具不自動彙總）。",
      "請看下方列出的重複列：把能區分它們的欄位加進 id_cols，或先回原檔處理重複。"
    )
  }
  nm <- check_wider_names(data, args)
  if (length(nm$collide) > 0) {
    kit_abort(
      "轉換後的新欄名和 id 欄同名：", paste(nm$collide, collapse = "、"), "。",
      "請改「欄名格式（names_glue）」或 names_prefix，讓新欄名不一樣。"
    )
  }
  if (length(nm$dup) > 0) {
    kit_abort(
      "轉換後有重複的新欄名：", paste(utils::head(nm$dup, 5), collapse = "、"), "。",
      "請在「欄名格式（names_glue）」放進 {.value}，讓每個值欄的名字不同。"
    )
  }

  fill <- parse_values_fill(args$values_fill, data, args$values_from)
  result <- tidyr::pivot_wider(
    data,
    id_cols = dplyr::all_of(id_cols),
    names_from = dplyr::all_of(args$names_from),
    values_from = dplyr::all_of(args$values_from),
    names_prefix = args$names_prefix,
    names_sep = args$names_sep,
    names_glue = args$names_glue,
    names_vary = args$names_vary,
    values_fill = fill
  )

  check_id <- check_id %||% pick_check_id(id_cols)
  id_check <- check_id_constant(data, check_id, id_cols)
  empty <- empty_cells_by_name(data, id_cols, args$names_from, args$values_from)
  list(
    result = result,
    id_cols = id_cols,
    summary = wider_summary(data, result, args, nm$names),
    warnings = wider_warnings(data, args, fill, id_check, check_id, nm),
    id_check = id_check,
    empty = empty,
    names = nm$names
  )
}

#' 長轉寬的白話摘要
#' @noRd
wider_summary <- function(data, result, args, new_names) {
  levels <- unique(data[[args$names_from]])
  c(
    paste0(
      "轉換前 ", format_dims(nrow(data), ncol(data)), "；",
      "轉換後 ", format_dims(nrow(result), ncol(result)), "。"
    ),
    paste0(
      args$names_from, " 有 ", length(levels), " 個值（",
      paste(utils::head(levels, 6), collapse = "、"),
      if (length(levels) > 6) "…" else "",
      "），新增 ", length(new_names), " 欄，例如 ",
      paste(utils::head(new_names, 3), collapse = "、"), "。"
    )
  )
}

#' 長轉寬的提醒：id 欄不一致、names_from 有空白、values_fill 沒用上、SAV 改名
#' @noRd
wider_warnings <- function(data, args, fill, id_check, check_id, nm) {
  n_na_name <- sum(is.na(data[[args$names_from]]))
  c(
    if (!id_check$ok) {
      paste0(
        "id_cols 裡的 ", paste(id_check$cols, collapse = "、"), " 在同一個 ",
        check_id, " 內不只一個值，有 ", id_check$n_ids, " 個 ", check_id,
        " 被拆成好幾列（見下方「id 欄不一致的列」）。",
        "建議把 ", paste(id_check$cols, collapse = "、"),
        " 移到「要攤開的值（values_from）」，或從 id_cols 拿掉。"
      )
    },
    if (n_na_name > 0) {
      paste0(
        args$names_from, " 有 ", n_na_name, " 列是空白，",
        "轉換後會變成名稱含 NA 的欄位。"
      )
    },
    if (!is.null(args$values_fill) && is.null(fill)) {
      paste0(
        "「缺的格子填什麼（values_fill）」填的是「", args$values_fill, "」，",
        "但要攤開的值沒有同型別的欄（數字只填數值欄、文字只填文字欄），所以沒有填。"
      )
    },
    if (length(nm$sav) > 0) {
      paste0(
        "有 ", length(nm$sav), " 個新欄名不符合 SPSS 規則，下載 SAV 時會自動改名，例如 ",
        paste(utils::head(paste(names(nm$sav), "→", nm$sav), 3), collapse = "、"), "。"
      )
    }
  )
}

#' 寬轉長（含檢查與白話摘要）
#'
#' 轉換本身和 ⑥ 共用 pivot_longer_run()（同一套檢查與中文錯誤訊息）。
#' @param data 資料框
#' @param args mod_pivot_longer_server() 或 longer_args() 的參數 list
#' @return list(result, summary, warnings)
#' @noRd
reshape_longer <- function(data, args) {
  args <- utils::modifyList(PIVOT_LONGER_DEFAULTS, args)
  cols <- pivot_longer_cols(data, args)
  kinds <- unique(stats::na.omit(purrr::map_chr(data[cols], col_bind_signature)))
  if (length(kinds) > 1) {
    kit_abort(
      "要轉成長格式的欄型別不同（有的是數字、有的是文字），不能放進同一欄。",
      "請只選同一種型別的欄。"
    )
  }
  labels <- pivot_longer_labels(data, cols)
  result <- pivot_longer_run(data, args)
  tidyr_warn <- attr(result, "kit_warnings")
  attr(result, "kit_warnings") <- NULL
  list(
    result = result,
    summary = c(
      paste0(
        "轉換前 ", format_dims(nrow(data), ncol(data)), "；",
        "轉換後 ", format_dims(nrow(result), ncol(result)), "。"
      ),
      paste0(
        "轉換 ", length(cols), " 欄",
        if (isTRUE(args$values_drop_na)) {
          "，並去掉空白的值。"
        } else {
          paste0("；原本每一列變成 ", nrow(result) %/% max(nrow(data), 1L), " 列。")
        }
      ),
      if (labels$zap_var) "要轉的欄變項標籤不一樣，值欄不放變項標籤。"
    ),
    warnings = c(
      labels$warning,
      if (length(tidyr_warn) > 0) {
        paste0("tidyr 的提醒：", paste(utils::head(tidyr_warn, 3), collapse = "；"))
      }
    ),
    labels = labels[c("conflict", "zap_var", "cols")]
  )
}


# ── kitsUI 分數表預設 ────────────────────────────────────────────────────────

#' 是否像 kitsUI 的分數表（一列 = 一次施測 × 一個領域）
#' @noRd
is_kitsui_score_table <- function(cols) {
  all(KITSUI_SCORE_REQUIRED %in% cols)
}

#' kitsUI 分數表的 pivot_wider 預設（只用來填控制項，不自動執行）
#'
#' values_from 多放 scale：d_score 空白時分得出「沒有這個領域的列」
#' 和「有列但還沒放上 D 量尺」（例如 scale = "motor M36"）。
#'
#' @return wider_args()；不像 kitsUI 分數表時回傳 NULL
#' @noRd
kitsui_score_preset <- function(cols) {
  if (!is_kitsui_score_table(cols)) return(NULL)
  wider_args(
    id_cols = intersect(KITSUI_SCORE_ID_COLS, cols),
    names_from = "domain",
    values_from = intersect(c("d_score", "d_score_se", "scale"), cols),
    names_glue = "{.value}_{domain}",
    names_vary = "slowest"
  )
}


# ── 轉換前：看起來是長格式還是寬格式 ─────────────────────────────────────────

# 長格式裡常見、同一個 ID 內會變的欄（可能就是 names_from）
RESHAPE_NAMES_FROM_HINTS <- c(
  "item_id", "item", "name", "domain", "wave", "wave_m", "time", "visit",
  "visit_id", "occasion", "variable", "key", "scale"
)

#' 猜哪一欄是 ID：subj_id、id、child_id → 其他 *_id → 欄名有 id 的欄；item_id 不算
#' @return 欄名；找不到時 NULL
#' @noRd
reshape_guess_id <- function(cols) {
  low <- tolower(cols)
  exact <- match(c("subj_id", "id", "child_id"), low)
  if (any(!is.na(exact))) return(cols[exact[!is.na(exact)][1]])
  other <- cols[grepl("_id$", low) & !low %in% c("item_id", "visit_id")]
  if (length(other) > 0) return(other[1])
  hit <- setdiff(guess_id_cols(cols), cols[low == "item_id"])
  if (length(hit) > 0) hit[1] else NULL
}

#' 轉換前掃一下：資料看起來是長格式還是寬格式（只是提示，不會自動換方向）
#'
#' 同一個 ID 有好幾列 → 長格式；每個 ID 一列、又有成組的欄名（lan01…、score_…）→ 寬格式；
#' 其他 → 看不出來。只用 vctrs 數不重複的值，幾千萬列也很快。
#' @param df 資料框
#' @param id_col NULL（自動猜）或使用者選的 ID 欄
#' @return list(format = "long"/"wide"/"unknown", suggest = "wider"/"longer"/NULL,
#'   id_col, n_rows, n_ids, reps, groups, varying, reason)
#' @noRd
reshape_guess_format <- function(df, id_col = NULL) {
  cols <- names(df)
  if (is.null(id_col) || !id_col %in% cols) id_col <- reshape_guess_id(cols)
  out <- list(
    format = "unknown", suggest = NULL, id_col = id_col, n_rows = nrow(df),
    n_ids = NA_integer_, reps = NA_real_, groups = pivot_name_groups(character(0)),
    varying = character(0), reason = NULL
  )
  if (is.null(id_col)) {
    out$reason <- "找不到像 ID 的欄（例如 subj_id、id）"
    return(out)
  }
  ids <- df[[id_col]]
  if (nrow(df) == 0 || all(is.na(ids))) {
    out$reason <- paste0(id_col, " 沒有值")
    return(out)
  }
  out$n_ids <- vctrs::vec_unique_count(ids)
  out$reps <- nrow(df) / out$n_ids
  if (out$n_ids < nrow(df)) {
    out$format <- "long"
    out$suggest <- "wider"
    out$varying <- reshape_varying_cols(df, id_col, out$n_ids)
    return(out)
  }
  out$groups <- pivot_name_groups(setdiff(cols, id_col))
  if (nrow(out$groups) > 0) {
    out$format <- "wide"
    out$suggest <- "longer"
  } else {
    out$reason <- "每個 ID 只有一列，但沒有成組的欄名（例如 lan01、lan02…）"
  }
  out
}

# 同一個 ID 內會變的欄（只看常見的欄名，最多 3 欄）
reshape_varying_cols <- function(df, id_col, n_ids) {
  low <- tolower(names(df))
  cand <- names(df)[low %in% RESHAPE_NAMES_FROM_HINTS & names(df) != id_col]
  cand <- cand[order(match(tolower(cand), RESHAPE_NAMES_FROM_HINTS))]
  varies <- purrr::map_lgl(utils::head(cand, 6), function(col) {
    vctrs::vec_unique_count(df[c(id_col, col)]) > n_ids
  })
  utils::head(utils::head(cand, 6)[varies], 3)
}

#' 猜測結果的白話說明
#' @noRd
reshape_guess_text <- function(g) {
  big <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
  if (identical(g$format, "long")) {
    reps <- format(round(g$reps, 1), nsmall = 0, trim = TRUE)
    return(paste0(
      "目前看起來是長格式：同一個 ", g$id_col, " 有好幾列（", big(g$n_rows), " 列、",
      big(g$n_ids), " 個 ", g$id_col, "，平均每個 ", reps, " 列）",
      if (length(g$varying) > 0) {
        paste0(
          "；同一個 ", g$id_col, " 內會變的欄：", paste(g$varying, collapse = "、"),
          "（可能就是「新欄名來自哪一欄」）"
        )
      },
      "。通常要選「長轉寬」。"
    ))
  }
  if (identical(g$format, "wide")) {
    shown <- paste0(utils::head(g$groups$prefix, 3), " 開頭 ", utils::head(g$groups$n, 3), " 欄")
    return(paste0(
      "目前看起來是寬格式：每個 ", g$id_col, " 一列（", big(g$n_ids), " 個），",
      "有 ", nrow(g$groups), " 組題目欄（", paste(shown, collapse = "、"),
      if (nrow(g$groups) > 3) "…", "）。通常要選「寬轉長」。"
    ))
  }
  paste0("看不出是長格式還是寬格式：", g$reason, "。可以換一欄當 ID 再看看。")
}
