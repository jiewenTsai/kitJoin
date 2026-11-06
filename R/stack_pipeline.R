# 直向波次疊加：每人每波一列，以 wave 欄標記波次

#' 直向波次疊加（含檢查、核對與白話摘要）
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
    # 原文：需要至少 2 個檔案才能疊加。請到「檔案上傳」再加入一個波次的檔案。
    kit_abort("\u9700\u8981\u81f3\u5c11 2 \u500b\u6a94\u6848\u624d\u80fd\u758a\u52a0\u3002\u8acb\u5230\u300c\u6a94\u6848\u4e0a\u50b3\u300d\u518d\u52a0\u5165\u4e00\u500b\u6ce2\u6b21\u7684\u6a94\u6848\u3002")
  }
  problem <- check_wave_labels(wave_labels, file_names, purpose = "stack")
  if (!is.null(problem)) kit_abort(problem)

  wave_col <- unused_name("wave", unlist(purrr::map(dfs, names)))
  harmonized <- harmonize_for_bind_rows(dfs)
  label_conflicts <- value_label_conflicts(harmonized$dfs)

  # 波次名稱用 .env（檔案最下面宣告給 R CMD check）取函式內的值：資料裡剛好有名為 label、w 的欄（例如權數 w）時，
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
  # 測量水準（SPSS 的名義、次序、尺度）：bind_rows 會丟掉，依欄名補回；各波不同時用第一個檔案的
  measures <- restore_measures(result, dfs, setdiff(names(result), wave_col))
  result <- measures$df

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
          # 原文：核對結果不符（見下方「詳細核對」）：列數、欄位數或各波列數和預期不同。
          "\u6838\u5c0d\u7d50\u679c\u4e0d\u7b26\uff08\u898b\u4e0b\u65b9\u300c\u8a73\u7d30\u6838\u5c0d\u300d\uff09\uff1a\u5217\u6578\u3001\u6b04\u4f4d\u6578\u6216\u5404\u6ce2\u5217\u6578\u548c\u9810\u671f\u4e0d\u540c\u3002",
          # 原文：請先確認結果再使用，並把這個情況回報給開發者。
          "\u8acb\u5148\u78ba\u8a8d\u7d50\u679c\u518d\u4f7f\u7528\uff0c\u4e26\u628a\u9019\u500b\u60c5\u6cc1\u56de\u5831\u7d66\u958b\u767c\u8005\u3002"
        )
      },
      if (wave_col != "wave") {
        paste0(
          # 原文：原始檔案已經有 wave 欄，為了不覆蓋原資料，
          "\u539f\u59cb\u6a94\u6848\u5df2\u7d93\u6709 wave \u6b04\uff0c\u70ba\u4e86\u4e0d\u8986\u84cb\u539f\u8cc7\u6599\uff0c",
          # 原文：波次標記放在新的  |  欄。
          "\u6ce2\u6b21\u6a19\u8a18\u653e\u5728\u65b0\u7684 ", wave_col, " \u6b04\u3002"
        )
      },
      if (length(harmonized$cols) > 0) {
        paste0(
          # 原文：這些欄位在各波的型別不同（有的是數字、有的是文字），已統一轉成文字：
          "\u9019\u4e9b\u6b04\u4f4d\u5728\u5404\u6ce2\u7684\u578b\u5225\u4e0d\u540c\uff08\u6709\u7684\u662f\u6578\u5b57\u3001\u6709\u7684\u662f\u6587\u5b57\uff09\uff0c\u5df2\u7d71\u4e00\u8f49\u6210\u6587\u5b57\uff1a",
          # 原文：、 | 。若應該是數字，請回原檔檢查是否有非數字的值。
          paste(harmonized$cols, collapse = "\u3001"), "\u3002\u82e5\u61c9\u8a72\u662f\u6578\u5b57\uff0c\u8acb\u56de\u539f\u6a94\u6aa2\u67e5\u662f\u5426\u6709\u975e\u6578\u5b57\u7684\u503c\u3002"
        )
      },
      if (nrow(measures$conflicts) > 0) {
        paste0(
          # 原文：這些欄位在各波的測量水準不同（SPSS 的名義、次序、尺度），結果用第一個檔案的：
          "\u9019\u4e9b\u6b04\u4f4d\u5728\u5404\u6ce2\u7684\u6e2c\u91cf\u6c34\u6e96\u4e0d\u540c\uff08SPSS \u7684\u540d\u7fa9\u3001\u6b21\u5e8f\u3001\u5c3a\u5ea6\uff09\uff0c\u7d50\u679c\u7528\u7b2c\u4e00\u500b\u6a94\u6848\u7684\uff1a",
          # 原文：（ | ） | 、
          paste0(measures$conflicts$column, "\uff08", measures$conflicts$measures, "\uff09", collapse = "\u3001"),
          # 原文：。
          "\u3002"
        )
      },
      if (length(label_conflicts) > 0) {
        paste0(
          # 原文：這些欄位在各波的數值標籤不同（例如 1 在一波是「是」、在另一波是「同意」），
          "\u9019\u4e9b\u6b04\u4f4d\u5728\u5404\u6ce2\u7684\u6578\u503c\u6a19\u7c64\u4e0d\u540c\uff08\u4f8b\u5982 1 \u5728\u4e00\u6ce2\u662f\u300c\u662f\u300d\u3001\u5728\u53e6\u4e00\u6ce2\u662f\u300c\u540c\u610f\u300d\uff09\uff0c",
          # 原文：結果只保留第一個檔案的標籤： | 、 | 。
          "\u7d50\u679c\u53ea\u4fdd\u7559\u7b2c\u4e00\u500b\u6a94\u6848\u7684\u6a19\u7c64\uff1a", paste(label_conflicts, collapse = "\u3001"), "\u3002"
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
    num_to_text(x)
  }
  out <- kit_fix_utf8_chr(out)
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

#' 狀態框裡的重點（最多 3 句）
#' @noRd
stack_plain_summary <- function(stats, names_shown, wave_col) {
  c(
    paste0(
      # 原文：共疊加  |  個檔案：
      "\u5171\u758a\u52a0 ", length(names_shown), " \u500b\u6a94\u6848\uff1a",
      # 原文： 列 | 、 | 。
      paste0(names_shown, " ", kit_num(stats$n_rows), " \u5217", collapse = "\u3001"), "\u3002"
    ),
    # 原文：每一列的  |  欄標示它來自哪一波。
    paste0("\u6bcf\u4e00\u5217\u7684 ", wave_col, " \u6b04\u6a19\u793a\u5b83\u4f86\u81ea\u54ea\u4e00\u6ce2\u3002"),
    if (length(stats$partial_cols) > 0) {
      paste0(
        # 原文：有  |  個欄位只出現在部分波次（例如 
        "\u6709 ", kit_num(length(stats$partial_cols)), " \u500b\u6b04\u4f4d\u53ea\u51fa\u73fe\u5728\u90e8\u5206\u6ce2\u6b21\uff08\u4f8b\u5982 ",
        # 原文：、
        paste(utils::head(stats$partial_cols, 3), collapse = "\u3001"),
        # 原文：），沒有這些欄位的波次會是空白。
        "\uff09\uff0c\u6c92\u6709\u9019\u4e9b\u6b04\u4f4d\u7684\u6ce2\u6b21\u6703\u662f\u7a7a\u767d\u3002"
      )
    }
  )
}

#' @noRd
stack_audit_tables <- function(stats, names_shown) {
  # 原文：符合 | 不符
  yes_no <- function(ok) ifelse(ok, "\u7b26\u5408", "\u4e0d\u7b26")
  files <- tibble::tibble(
    # 原文：順序
    "\u9806\u5e8f" = seq_along(names_shown),
    # 原文：檔案
    "\u6a94\u6848" = names_shown,
    # 原文：列數
    "\u5217\u6578" = stats$n_rows,
    # 原文：欄位數
    "\u6b04\u4f4d\u6578" = stats$n_cols,
    # 原文：結果中該波列數
    "\u7d50\u679c\u4e2d\u8a72\u6ce2\u5217\u6578" = stats$n_by_wave
  )
  checks <- tibble::tibble(
    # 原文：檢查項目 | 列數（各檔列數相加） | 欄位數（各檔欄位聯集 + 波次欄） | 各波列數
    "\u6aa2\u67e5\u9805\u76ee" = c("\u5217\u6578\uff08\u5404\u6a94\u5217\u6578\u76f8\u52a0\uff09", "\u6b04\u4f4d\u6578\uff08\u5404\u6a94\u6b04\u4f4d\u806f\u96c6 + \u6ce2\u6b21\u6b04\uff09", "\u5404\u6ce2\u5217\u6578"),
    # 原文：結果
    "\u7d50\u679c" = c(stats$nrow_out, stats$ncol_out, NA),
    # 原文：預期
    "\u9810\u671f" = c(stats$nrow_expected, stats$ncol_expected, NA),
    # 原文：核對
    "\u6838\u5c0d" = yes_no(c(
      stats$nrow_out == stats$nrow_expected,
      stats$ncol_out == stats$ncol_expected,
      stats$wave_ok
    ))
  )
  list(files = files, checks = checks)
}

# dplyr 的 .env 代名詞（stack_waves()）：R CMD check 不認得，宣告一下
utils::globalVariables(".env")
