# 教學用程式碼產生器（tidyverse 版與 base R 版）
#
# 給初學者看：每個檔案一個物件（data_t1、data_t2…），逐行寫出，
# 盡量少用進階觀念（不用 map()、自訂函式、where() 等）。
# 程式碼依使用者在各分頁的設定產生；尚未上傳檔案時以範例值代入。
# 每個 code_*() 回傳 list(tidy = 字串, base = 字串)；
# 範本以「一行一個元素」的字元向量撰寫，最後用 code_lines() 接起來。

#' 把多行（可含 NULL 或多元素向量）接成一段程式碼
#' @noRd
code_lines <- function(...) paste(unlist(list(...)), collapse = "\n")

#' 字串向量轉成 R 程式碼：c("a", "b")；單一值不加 c()
#' @noRd
r_str <- function(x) {
  q <- paste0('"', x, '"')
  if (length(q) == 1) q else paste0("c(", paste(q, collapse = ", "), ")")
}

#' 數值向量轉成 R 程式碼：c(7777, 8888)
#' @noRd
r_num <- function(x) {
  nums <- format(x, scientific = FALSE, trim = TRUE, drop0trailing = TRUE)
  paste0("c(", paste(nums, collapse = ", "), ")")
}

#' 欄位名稱在 tidyselect 中的寫法（非合法名稱加反引號）
#' @noRd
r_col <- function(x) {
  ifelse(make.names(x) == x, x, paste0("`", x, "`"))
}

#' 排除欄位的 tidyselect 寫法：-id 或 -c(id, idno)
#' @noRd
r_minus_cols <- function(x) {
  if (length(x) == 1) {
    paste0("-", r_col(x))
  } else {
    paste0("-c(", paste(r_col(x), collapse = ", "), ")")
  }
}

#' 範例程式中的檔案資訊：檔名、波次、物件名稱、欄位、編碼
#'
#' @param file_names 上傳檔名（未上傳時為空）
#' @param waves 各檔波次名稱
#' @param cols 各檔欄位名稱（list；未知時為 NULL）
#' @param min_files 檔案數少於此數時改用範例檔（串接、疊加、流失需 2 個）
#' @param encodings 各檔編碼（CSV 為 "UTF-8"、"BIG5"…；SAV 為 NA）
#' @noRd
code_files <- function(
    file_names = character(0),
    waves = character(0),
    cols = NULL,
    min_files = 1L,
    encodings = NULL) {
  if (length(file_names) < min_files) {
    file_names <- c("wave1.csv", "wave2.sav")
    waves <- c("t1", "t2")
    cols <- NULL
    encodings <- NULL
  }
  if (is.null(encodings)) encodings <- rep(NA_character_, length(file_names))
  waves <- ifelse(
    is.na(waves) | !nzchar(waves),
    paste0("t", seq_along(file_names)),
    waves
  )
  obj <- make.unique(
    paste0("data_", gsub("[^A-Za-z0-9_]", "_", waves)),
    sep = "_"
  )
  list(
    file = file_names,
    wave = waves,
    obj = obj,
    cols = cols,
    encoding = encodings
  )
}

#' 由 mod_upload_server() 的回傳值產生 code_files()
#' @noRd
code_files_from <- function(files, min_files = 1L) {
  code_files(
    files$names(), files$labels(), purrr::map(files$data(), names),
    min_files = min_files, encodings = files$encodings()
  )
}

code_read <- function(f) {
  is_csv <- tolower(tools::file_ext(f$file)) == "csv"
  is_big5 <- !is.na(f$encoding) & f$encoding == "BIG5"
  tidy_lines <- ifelse(
    is_csv,
    paste0(
      f$obj, ' <- read_csv("', f$file, '"',
      ifelse(is_big5, ', locale = locale(encoding = "CP950"))   # BIG5 編碼', ")")
    ),
    paste0(
      f$obj, ' <- read_sav("', f$file, '"',
      ifelse(is_big5, ', encoding = "CP950")   # 檔案沒有標明編碼，用 BIG5 讀', ")")
    )
  )
  base_lines <- ifelse(
    is_csv,
    paste0(
      f$obj, ' <- read.csv("', f$file, '"',
      ifelse(is_big5, ', fileEncoding = "CP950")   # BIG5 編碼', ")")
    ),
    paste0(
      f$obj, ' <- haven::read_sav("', f$file, '"',
      ifelse(is_big5, ', encoding = "CP950"', ""),
      ')   # base R 不能讀 SAV，借用 haven'
    )
  )
  setwd_note <- c(
    "# 先把資料檔放在與這個 R 檔相同的資料夾，",
    "# 或在 RStudio 選 Session → Set Working Directory → To Source File Location"
  )
  list(
    tidy = code_lines(
      "library(readr)   # 讀 CSV",
      "library(haven)   # 讀 SAV",
      "",
      setwd_note,
      tidy_lines,
      "",
      paste0("head(", f$obj[1], ")   # 看前 6 列")
    ),
    base = code_lines(
      setwd_note,
      base_lines,
      "",
      paste0("head(", f$obj[1], ")   # 看前 6 列")
    )
  )
}

code_missing <- function(f, codes, exclude = character(0), match_text = TRUE) {
  if (length(codes) == 0) codes <- parse_na_codes(kit_na_codes_default())$codes
  if (is.null(exclude)) exclude <- character(0)
  # 各檔實際存在的排除欄位（欄位未知時直接使用）
  excl_of <- function(i) {
    if (is.null(f$cols)) exclude else intersect(exclude, f$cols[[i]])
  }

  tidy_one <- function(i) {
    ex <- excl_of(i)
    target <- if (length(ex) == 0) "everything()" else r_minus_cols(ex)
    if (!match_text) {
      target <- if (length(ex) == 0) {
        "where(is.numeric)"
      } else {
        paste0("where(is.numeric) & !", sub("^-", "", r_minus_cols(ex)))
      }
    }
    code_lines(
      paste0(f$obj[i], " <- ", f$obj[i], " |>"),
      paste0(
        "  mutate(across(", target,
        ", ~ replace(.x, .x %in% na_codes, NA)))"
      )
    )
  }
  base_one <- function(i) {
    ex <- excl_of(i)
    obj <- f$obj[i]
    code_lines(
      if (length(ex) == 0) {
        paste0("cols <- names(", obj, ")")
      } else {
        paste0("cols <- setdiff(names(", obj, "), ", r_str(ex), ")")
      },
      if (!match_text) {
        paste0("cols <- cols[sapply(", obj, "[cols], is.numeric)]   # 只留數值欄")
      },
      "for (col in cols) {",
      paste0("  ", obj, "[[col]][", obj, "[[col]] %in% na_codes] <- NA"),
      "}"
    )
  }

  idx <- seq_along(f$obj)
  header <- c(
    paste0("na_codes <- ", r_num(codes)),
    "",
    '# %in% 會把數字和文字的 "9999" 都當成相同，所以不用另外寫文字版',
    if (length(exclude) > 0) {
      paste0("# 不處理：", paste(exclude, collapse = "、"), "（如 ID 欄）")
    }
  )
  check <- paste0("colSums(is.na(", f$obj[1], "))   # 檢查：各欄 NA 數")

  list(
    tidy = code_lines(
      "library(dplyr)", "",
      header, "",
      paste(vapply(idx, tidy_one, character(1)), collapse = "\n\n"), "",
      check
    ),
    base = code_lines(
      header, "",
      paste(vapply(idx, base_one, character(1)), collapse = "\n\n"), "",
      check
    )
  )
}

code_join <- function(f, by_vars, join_method) {
  if (length(by_vars) == 0) by_vars <- "release_id"
  if (length(join_method) == 0 || !join_method %in% KIT_JOIN_METHODS) {
    join_method <- "left_join"
  }
  wide <- paste0(sub("^data_", "", f$obj), "_wide")
  suffix <- join_suffix(f$wave)

  tidy_rename <- paste0(
    wide, " <- rename_with(", f$obj, ", ~ paste0(.x, \"", suffix, "\"), ",
    r_minus_cols(by_vars), ")"
  )
  tidy_join <- c(
    paste0("joined <- ", wide[1], " |>"),
    paste0(
      "  ", join_method, "(", wide[-1], ", by = ", r_str(by_vars), ")",
      c(rep(" |>", length(wide) - 2), "")
    )
  )

  keep_test <- if (length(by_vars) == 1) {
    function(w) paste0("names(", w, ") != ", r_str(by_vars))
  } else {
    function(w) paste0("!names(", w, ") %in% ", r_str(by_vars))
  }
  base_rename <- vapply(seq_along(wide), function(i) {
    code_lines(
      paste0(wide[i], " <- ", f$obj[i]),
      paste0("to_rename <- ", keep_test(wide[i])),
      paste0(
        "names(", wide[i], ")[to_rename] <- paste0(names(", wide[i],
        ")[to_rename], \"", suffix[i], "\")"
      )
    )
  }, character(1))
  merge_arg <- switch(join_method,
    left_join = ", all.x = TRUE",
    full_join = ", all = TRUE",
    ""
  )
  base_join <- c(
    paste0(
      "joined <- merge(", wide[1], ", ", wide[2],
      ", by = ", r_str(by_vars), merge_arg, ")"
    ),
    if (length(wide) > 2) {
      paste0(
        "joined <- merge(joined, ", wide[-(1:2)],
        ", by = ", r_str(by_vars), merge_arg, ")"
      )
    }
  )
  merge_note <- switch(join_method,
    left_join = "# all.x = TRUE 相當於 left_join；merge() 會依 key 重新排序",
    full_join = "# all = TRUE 相當於 full_join；merge() 會依 key 重新排序",
    "# 不加 all 參數相當於 inner_join；merge() 會依 key 重新排序"
  )

  list(
    tidy = code_lines(
      "library(dplyr)", "",
      "# 1. 非 ID 欄位加上波次後綴，避免同名欄衝突",
      tidy_rename, "",
      "# 2. 依序合併",
      tidy_join, "",
      "dim(joined)   # 列數、欄數"
    ),
    base = code_lines(
      "# 1. 非 ID 欄位加上波次後綴，避免同名欄衝突",
      paste(base_rename, collapse = "\n\n"), "",
      "# 2. 依序合併",
      merge_note,
      base_join, "",
      "dim(joined)   # 列數、欄數"
    )
  )
}

code_stack <- function(f) {
  long <- paste0(sub("^data_", "", f$obj), "_long")
  list(
    tidy = code_lines(
      "library(dplyr)", "",
      '# .id = "wave"：用前面的名稱（波次）建立 wave 欄',
      "stacked <- bind_rows(",
      paste0('  "', f$wave, '" = ', f$obj, ","),
      '  .id = "wave"',
      ")",
      "# 若同名欄型別不同（一檔數值、一檔文字）會報錯，需先統一型別",
      "",
      "count(stacked, wave)   # 各波次列數"
    ),
    base = code_lines(
      "# 1. 每個檔案加上 wave 欄",
      paste0(long, " <- ", f$obj, "\n", long, '$wave <- "', f$wave, '"'),
      "",
      "# 2. rbind() 需要欄位相同：先補上缺少的欄位（填 NA）",
      paste0(
        "all_cols <- unique(c(",
        paste0("names(", long, ")", collapse = ", "),
        "))"
      ),
      paste0(long, "[setdiff(all_cols, names(", long, "))] <- NA"),
      "",
      "# 3. 上下疊起來",
      paste0("stacked <- rbind(", paste(long, collapse = ", "), ")"),
      "",
      "table(stacked$wave)   # 各波次列數"
    )
  )
}

code_attrition <- function(f, base_i, follow_i, id_vars) {
  if (length(id_vars) == 0) id_vars <- "release_id"
  if (length(base_i) == 0 || is.na(base_i)) base_i <- 1L
  if (length(follow_i) == 0 || is.na(follow_i)) {
    follow_i <- min(2L, length(f$obj))
  }
  base_obj <- f$obj[base_i]
  follow_obj <- f$obj[follow_i]
  rule <- paste0(
    "# 前期（", base_obj, "）的 ID 有出現在後期（", follow_obj, "）→ 0 留存，否則 1 流失"
  )

  if (length(id_vars) == 1) {
    id <- r_col(id_vars)
    lead <- NULL
    tidy_body <- c(
      paste0("attrition_df <- ", base_obj, " |>"),
      paste0(
        "  mutate(attrition = if_else(", id, " %in% ",
        follow_obj, "$", id, ", 0, 1))"
      )
    )
    base_body <- c(
      paste0("attrition_df <- ", base_obj),
      paste0(
        "attrition_df$attrition <- ifelse(attrition_df$", id, " %in% ",
        follow_obj, "$", id, ", 0, 1)"
      )
    )
  } else {
    key_of <- function(obj) {
      paste0("paste(", paste0(obj, "$", r_col(id_vars), collapse = ", "), ")")
    }
    lead <- c(
      "# 多個 ID 欄位：先用 paste() 黏成一個比對用的鍵",
      paste0("followup_key <- ", key_of(follow_obj)),
      ""
    )
    tidy_body <- c(
      paste0("attrition_df <- ", base_obj, " |>"),
      paste0(
        "  mutate(attrition = if_else(paste(",
        paste(r_col(id_vars), collapse = ", "),
        ") %in% followup_key, 0, 1))"
      )
    )
    base_body <- c(
      paste0("attrition_df <- ", base_obj),
      paste0(
        "attrition_df$attrition <- ifelse(",
        key_of("attrition_df"),
        " %in% followup_key, 0, 1)"
      )
    )
  }

  list(
    tidy = code_lines(
      "library(dplyr)", "",
      rule, lead, tidy_body, "",
      "count(attrition_df, attrition)"
    ),
    base = code_lines(
      rule, lead, base_body, "",
      "table(attrition_df$attrition)"
    )
  )
}
