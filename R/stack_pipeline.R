# 長格式疊加：每人每波一列，以 wave 欄標記波次

#' 長格式疊加（含檢查、核對與白話摘要）
#'
#' 同名欄對齊，不同欄新增並填 NA；同名欄型別不一致時轉成文字。
#' 原檔已有 wave 欄時，波次標記欄改名為 kit_wave（不覆蓋原資料）。
#'
#' @param dfs 各波資料框
#' @param file_names 各檔檔名
#' @param wave_labels 各檔波次名稱（填入波次標記欄）
#' @return list(result, wave_col, audit, summary, warnings)
#' @noRd
stack_waves <- function(dfs, file_names, wave_labels) {
  if (length(dfs) < 2) {
    kit_abort("需要至少 2 個檔案才能疊加。請到「① 上傳檔案」再加入一個波次的檔案。")
  }
  problem <- check_wave_labels(wave_labels, file_names, purpose = "stack")
  if (!is.null(problem)) kit_abort(problem)

  wave_col <- unused_name("wave", unlist(purrr::map(dfs, names)))
  harmonized <- harmonize_for_bind_rows(dfs)
  label_conflicts <- value_label_conflicts(harmonized$dfs)

  # 波次名稱用 .env 取函式內的值：資料裡剛好有名為 label、w 的欄（例如權數 w）時，
  # data masking 會改拿那一欄的值
  tagged <- purrr::map2(harmonized$dfs, wave_labels, function(df, label) {
    df <- dplyr::mutate(df, .kit_wave = .env$label, .before = 1)
    names(df)[1] <- wave_col
    df
  })
  # 數值標籤衝突已自行偵測並提醒，不再讓 haven 的英文警告跑到 console
  result <- withCallingHandlers(
    dplyr::bind_rows(tagged),
    warning = function(w) {
      if (grepl("conflicting value labels", conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    }
  )
  result <- restore_var_labels(result, dfs)

  names_shown <- file_display_name(wave_labels, file_names)
  stats <- stack_stats(dfs, wave_labels, result, wave_col)
  audit_ok <- stats$nrow_out == stats$nrow_expected &&
    stats$ncol_out == stats$ncol_expected && stats$wave_ok
  list(
    result = result,
    wave_col = wave_col,
    audit = stack_audit_tables(stats, names_shown),
    summary = stack_plain_summary(stats, names_shown, wave_col),
    warnings = c(
      if (!audit_ok) {
        paste0(
          "核對結果不符（見下方「詳細核對」）：列數、欄位數或各波列數和預期不同。",
          "請先確認結果再使用，並把這個情況回報給開發者。"
        )
      },
      if (wave_col != "wave") {
        paste0(
          "原始檔案已經有 wave 欄，為了不覆蓋原資料，",
          "波次標記放在新的 ", wave_col, " 欄。"
        )
      },
      if (length(harmonized$cols) > 0) {
        paste0(
          "這些欄位在各波的型別不同（有的是數字、有的是文字），已統一轉成文字：",
          paste(harmonized$cols, collapse = "、"), "。若應該是數字，請回原檔檢查是否有非數字的值。"
        )
      },
      if (length(label_conflicts) > 0) {
        paste0(
          "這些欄位在各波的數值標籤不同（例如 1 在一波是「是」、在另一波是「同意」），",
          "結果只保留第一個檔案的標籤：", paste(label_conflicts, collapse = "、"), "。"
        )
      }
    )
  )
}

#' 不和既有欄名衝突的新欄名：wave → kit_wave → kit_wave_2 …
#' @noRd
unused_name <- function(name, existing) {
  if (!name %in% existing) return(name)
  candidate <- paste0("kit_", name)
  k <- 2L
  while (candidate %in% existing) {
    candidate <- paste0("kit_", name, "_", k)
    k <- k + 1L
  }
  candidate
}

#' 疊加時的型別類別；全空的欄（readr 讀成 logical）不參與比較
#' @noRd
col_bind_signature <- function(x) {
  if (is.logical(x) && all(is.na(x))) return(NA_character_)
  if (is.numeric(x)) return("numeric")
  if (is.logical(x)) return("logical")
  if (inherits(x, "Date")) return("Date")
  "character"
}

#' 同名欄型別不一致時，各檔該欄都轉成文字
#' @return list(dfs, cols = 被轉換的欄名)
#' @noRd
harmonize_for_bind_rows <- function(dfs) {
  all_cols <- unique(unlist(purrr::map(dfs, names)))
  converted <- character(0)
  for (col in all_cols) {
    sigs <- purrr::map_chr(dfs, function(df) {
      if (col %in% names(df)) col_bind_signature(df[[col]]) else NA_character_
    })
    if (length(unique(stats::na.omit(sigs))) <= 1) next
    converted <- c(converted, col)
    dfs <- purrr::map(dfs, function(df) {
      if (col %in% names(df)) df[[col]] <- to_text_keep_label(df[[col]])
      df
    })
  }
  list(dfs = dfs, cols = converted)
}

#' 轉成文字，保留變項標籤；標籤欄用「標籤文字」而非代碼
#' @noRd
to_text_keep_label <- function(x) {
  label <- attr(x, "label", exact = TRUE)
  out <- if (haven::is.labelled(x)) {
    as.character(haven::as_factor(x))
  } else {
    as.character(x)
  }
  out <- fix_utf8_chr(out)
  attr(out, "label") <- label
  out
}

#' 各波數值標籤不一致的欄位（同一個值對到不同標籤文字）
#' @noRd
value_label_conflicts <- function(dfs) {
  all_cols <- unique(unlist(purrr::map(dfs, names)))
  conflict <- vapply(all_cols, function(col) {
    labs <- purrr::compact(purrr::map(dfs, function(df) {
      if (col %in% names(df)) attr(df[[col]], "labels", exact = TRUE)
    }))
    if (length(labs) < 2) return(FALSE)
    pairs <- unlist(purrr::map(labs, function(l) {
      paste(unclass(l), names(l), sep = "\r")
    }))
    values <- sub("\r.*$", "", unique(pairs))
    anyDuplicated(values) > 0
  }, logical(1))
  all_cols[conflict]
}

#' bind_rows 合併一般文字欄時會丟掉屬性：依檔案順序補回第一個找到的變項標籤
#' @noRd
restore_var_labels <- function(result, dfs) {
  for (col in names(result)) {
    if (!is.null(attr(result[[col]], "label", exact = TRUE))) next
    for (df in dfs) {
      label <- if (col %in% names(df)) attr(df[[col]], "label", exact = TRUE)
      if (!is.null(label)) {
        attr(result[[col]], "label") <- label
        break
      }
    }
  }
  result
}


# ── 核對 ─────────────────────────────────────────────────────────────────────

#' @noRd
stack_stats <- function(dfs, wave_labels, result, wave_col) {
  union_cols <- unique(unlist(purrr::map(dfs, names)))
  partial_cols <- union_cols[!purrr::map_lgl(union_cols, function(col) {
    all(purrr::map_lgl(dfs, function(df) col %in% names(df)))
  })]
  n_rows <- purrr::map_int(dfs, nrow)
  n_by_wave <- purrr::map_int(wave_labels, function(w) {
    sum(result[[wave_col]] == w, na.rm = TRUE)
  })
  list(
    n_rows = n_rows,
    n_cols = purrr::map_int(dfs, ncol),
    n_by_wave = n_by_wave,
    partial_cols = partial_cols,
    nrow_out = nrow(result),
    nrow_expected = sum(n_rows),
    ncol_out = ncol(result),
    ncol_expected = length(union_cols) + 1L,
    wave_ok = all(n_by_wave == n_rows)
  )
}

#' @noRd
stack_plain_summary <- function(stats, names_shown, wave_col) {
  c(
    paste0(
      "共疊加 ", length(names_shown), " 個檔案：",
      paste0(names_shown, " ", stats$n_rows, " 列", collapse = "、"), "。"
    ),
    paste0("結果共 ", format_dims(stats$nrow_out, stats$ncol_out), "，",
           "每一列的 ", wave_col, " 欄標示它來自哪一波。"),
    if (length(stats$partial_cols) > 0) {
      paste0(
        "有 ", length(stats$partial_cols), " 個欄位只出現在部分波次（例如 ",
        paste(utils::head(stats$partial_cols, 3), collapse = "、"),
        "），沒有這些欄位的波次會是空白。"
      )
    }
  )
}

#' @noRd
stack_audit_tables <- function(stats, names_shown) {
  yes_no <- function(ok) ifelse(ok, "符合", "不符")
  files <- tibble::tibble(
    順序 = seq_along(names_shown),
    檔案 = names_shown,
    列數 = stats$n_rows,
    欄位數 = stats$n_cols,
    結果中該波列數 = stats$n_by_wave
  )
  checks <- tibble::tibble(
    檢查項目 = c("列數（各檔列數相加）", "欄位數（各檔欄位聯集 + 波次欄）", "各波列數"),
    結果 = c(stats$nrow_out, stats$ncol_out, NA),
    預期 = c(stats$nrow_expected, stats$ncol_expected, NA),
    核對 = yes_no(c(
      stats$nrow_out == stats$nrow_expected,
      stats$ncol_out == stats$ncol_expected,
      stats$wave_ok
    ))
  )
  list(files = files, checks = checks)
}
