# 教學用程式碼產生器（tidyverse 版與 base R 版）
#
# 給初學者看：每個檔案一個物件（data_1、data_2…），逐行寫出，
# 盡量少用進階觀念（不用 map()、自訂函式等）。
# 物件名稱不從波次名稱推導：波次名稱可能是 2019、第一波或含引號，當不了物件名稱。
# 程式碼依使用者在各分頁的設定產生；尚未上傳檔案時以範例值代入。
# 每個 code_*() 回傳 list(tidy = 字串, base = 字串)；
# 範本以「一行一個元素」的字元向量撰寫，最後用 code_lines() 接起來。
# 產生的程式碼要能直接執行，結果和 app 相同（tests/testthat/test-code_snippets.R 實際執行比對）。

#' 把多行（可含 NULL 或多元素向量）接成一段程式碼
#' @noRd
code_lines <- function(...) paste(unlist(list(...)), collapse = "\n")

#' 字串向量轉成 R 程式碼：c("a", "b")；單一值不加 c()
#'
#' 引號、反斜線、換行等會正確跳脫（encodeString）。
#' @noRd
r_str <- function(x) {
  q <- encodeString(enc2utf8(as.character(x)), quote = '"')
  if (length(q) == 1) q else paste0("c(", paste(q, collapse = ", "), ")")
}

#' 數值向量轉成 R 程式碼：c(7777, 8888)
#' @noRd
r_num <- function(x) {
  nums <- format(x, scientific = FALSE, trim = TRUE, drop0trailing = TRUE)
  paste0("c(", paste(nums, collapse = ", "), ")")
}

#' 欄位名稱在程式碼中的寫法（不是合法名稱就加反引號，反引號與反斜線先跳脫）
#' @noRd
r_col <- function(x) {
  ifelse(
    make.names(x) == x,
    x,
    paste0("`", gsub("([`\\\\])", "\\\\\\1", x), "`")
  )
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

#' 取某一欄：data_1$id 或 data_1$`學 號`
#' @noRd
r_dollar <- function(obj, col) paste0(obj, "$", r_col(col))

#' 數字轉文字的程式碼（和 app 相同：100000 不會變成 1e+05；NA 保持 NA）
#' @noRd
r_num_to_text <- function(expr) {
  paste0(
    "ifelse(is.na(", expr, "), NA, trimws(formatC(as.numeric(", expr,
    "), format = \"fg\", digits = 15)))"
  )
}

#' 範例程式中的檔案資訊：檔名、波次、物件名稱、欄位、編碼、欄位型別
#'
#' @param file_names 上傳檔名（未上傳時為空）
#' @param waves 各檔波次名稱
#' @param cols 各檔欄位名稱（list；未知時為 NULL）
#' @param min_files 檔案數少於此數時改用範例檔（串接、疊加、流失需 2 個）
#' @param encodings 各檔編碼（CSV 為 "UTF-8"、"BIG5"…；SAV 為 NA 或 "BIG5"）
#' @param types 各檔欄位型別（list，code_col_types() 的結果；未知時為 NULL）
#' @noRd
code_files <- function(
    file_names = character(0),
    waves = character(0),
    cols = NULL,
    min_files = 1L,
    encodings = NULL,
    types = NULL) {
  if (length(file_names) < min_files) {
    file_names <- c("wave1.csv", "wave2.sav")
    waves <- c("t1", "t2")
    cols <- NULL
    encodings <- NULL
    types <- NULL
  }
  if (is.null(encodings)) encodings <- rep(NA_character_, length(file_names))
  waves <- ifelse(
    is.na(waves) | !nzchar(waves),
    paste0("t", seq_along(file_names)),
    waves
  )
  list(
    file = file_names,
    wave = waves,
    obj = paste0("data_", seq_along(file_names)),
    cols = cols,
    encoding = encodings,
    types = types
  )
}

#' 由 mod_upload_server() 的回傳值產生 code_files()
#' @noRd
code_files_from <- function(files, min_files = 1L) {
  code_files(
    files$names(), files$labels(), purrr::map(files$data(), names),
    min_files = min_files, encodings = files$encodings(),
    types = purrr::map(files$data(), code_col_types)
  )
}

#' 各欄的型別（給程式碼決定要不要先統一型別）
#'
#' "numeric"、"character"、"logical"、"Date"；有數值標籤的加上 "+labels"；全空的欄是 "empty"。
#' @noRd
code_col_types <- function(df) {
  vapply(df, function(x) {
    sig <- col_bind_signature(x)
    if (is.na(sig)) return("empty")
    if (haven::is.labelled(x)) paste0(sig, "+labels") else sig
  }, character(1))
}

#' 某檔某欄的型別；不知道時為 NA
#' @noRd
code_type_of <- function(f, i, col) {
  if (is.null(f$types)) return(NA_character_)
  unname(f$types[[i]][col])
}

#' 「data_1 = t1（w1.csv）」對照說明
#' @noRd
code_obj_notes <- function(f) {
  paste0("#   ", f$obj, " = ", f$wave, "（", f$file, "）")
}

code_read <- function(f) {
  is_csv <- tolower(tools::file_ext(f$file)) == "csv"
  is_big5 <- !is.na(f$encoding) & f$encoding == "BIG5"
  files <- vapply(f$file, r_str, character(1), USE.NAMES = FALSE)
  tidy_lines <- ifelse(
    is_csv,
    paste0(
      f$obj, " <- read_csv(", files,
      ifelse(is_big5, ', locale = locale(encoding = "CP950"))   # BIG5 編碼', ")")
    ),
    paste0(
      f$obj, " <- read_sav(", files,
      ifelse(is_big5, ', encoding = "CP950")   # 檔案沒有標明編碼，用 BIG5 讀', ")")
    )
  )
  base_lines <- ifelse(
    is_csv,
    paste0(
      f$obj, " <- read.csv(", files,
      ifelse(is_big5, ', fileEncoding = "CP950"', ', fileEncoding = "UTF-8-BOM"'),
      ", check.names = FALSE, na.strings = c(\"\", \"NA\"), strip.white = TRUE)"
    ),
    paste0(
      f$obj, " <- haven::read_sav(", files,
      ifelse(is_big5, ', encoding = "CP950"', ""),
      ")   # base R 不能讀 SAV，借用 haven"
    )
  )
  setwd_note <- c(
    "# 先把資料檔放在與這個 R 檔相同的資料夾，",
    "# 或在 RStudio 選 Session → Set Working Directory → To Source File Location"
  )
  obj_note <- c("# 每個檔案一個物件：", code_obj_notes(f))
  csv_note <- if (any(is_csv)) {
    c(
      "# read.csv() 的設定和 read_csv() 一致：check.names = FALSE 欄名保持原樣（空白不改成點）；",
      "# 空白格當成 NA；去掉前後空白"
    )
  }
  list(
    tidy = code_lines(
      "library(readr)   # 讀 CSV",
      "library(haven)   # 讀 SAV",
      "",
      setwd_note,
      obj_note,
      tidy_lines,
      "",
      paste0("head(", f$obj[1], ")   # 看前 6 列")
    ),
    base = code_lines(
      setwd_note,
      obj_note,
      csv_note,
      base_lines,
      "",
      paste0("head(", f$obj[1], ")   # 看前 6 列")
    )
  )
}

code_missing <- function(f, codes, exclude = character(0), match_text = TRUE,
                         mode = "codes", keywords = character(0)) {
  if (is.null(exclude)) exclude <- character(0)
  if (identical(mode, "labels")) return(code_missing_labels(f, keywords, exclude))
  if (length(codes) == 0) codes <- parse_na_codes(kit_na_codes_default())$codes
  # 各檔實際存在的排除欄位（欄位未知時直接使用）
  excl_of <- function(i) {
    if (is.null(f$cols)) exclude else intersect(exclude, f$cols[[i]])
  }

  tidy_one <- function(i) {
    ex <- excl_of(i)
    target <- function(type) {
      if (length(ex) == 0) {
        paste0("where(", type, ")")
      } else {
        paste0("where(", type, ") & !", sub("^-", "", r_minus_cols(ex)))
      }
    }
    code_lines(
      paste0(f$obj[i], " <- ", f$obj[i], " |>"),
      paste0(
        "  mutate(across(", target("is.numeric"),
        ", ~ replace(.x, .x %in% na_codes, NA)))",
        if (match_text) " |>"
      ),
      if (match_text) {
        paste0(
          "  mutate(across(", target("is.character"),
          ", ~ replace(.x, trimws(.x) %in% na_codes, NA)))"
        )
      }
    )
  }
  base_one <- function(i) {
    ex <- excl_of(i)
    obj <- f$obj[i]
    code_lines(
      if (length(ex) == 0) {
        paste0("for (col in names(", obj, ")) {")
      } else {
        paste0("for (col in setdiff(names(", obj, "), ", r_str(ex), ")) {")
      },
      paste0("  x <- ", obj, "[[col]]"),
      paste0("  if (is.numeric(x)) ", obj, "[[col]][x %in% na_codes] <- NA"),
      if (match_text) {
        paste0("  if (is.character(x)) ", obj, "[[col]][trimws(x) %in% na_codes] <- NA")
      },
      "}"
    )
  }

  idx <- seq_along(f$obj)
  header <- c(
    paste0("na_codes <- ", r_num(codes)),
    "",
    if (match_text) {
      '# 文字欄（例如被讀成文字的 "9999"）去掉前後空白後也比對'
    } else {
      "# 只處理數值欄"
    },
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

#' ② 依數值標籤：每一欄找出標籤含關鍵字、而且在選項範圍外的代碼，改成 NA
#' @noRd
code_missing_labels <- function(f, keywords, exclude) {
  if (length(keywords) == 0) keywords <- parse_na_keywords(kit_na_label_keywords_default())
  excl_of <- function(i) {
    if (is.null(f$cols)) exclude else intersect(exclude, f$cols[[i]])
  }
  one <- function(i) {
    ex <- excl_of(i)
    obj <- f$obj[i]
    cols <- if (length(ex) == 0) {
      paste0("names(", obj, ")")
    } else {
      paste0("setdiff(names(", obj, "), ", r_str(ex), ")")
    }
    code_lines(
      paste0("for (col in ", cols, ") {"),
      paste0("  labs <- attr(", obj, "[[col]], \"labels\")"),
      "  if (is.null(labs)) next",
      "  is_na <- grepl(na_words, names(labs), ignore.case = TRUE)",
      "  if (is.numeric(labs)) {",
      "    # 代碼要在選項範圍外：負數，或大於其他選項的值",
      "    is_na <- is_na & (labs < 0 | labs > max(c(-Inf, labs[!is_na])))",
      "  }",
      paste0("  x <- ", obj, "[[col]]"),
      "  hit <- if (is.numeric(x)) x %in% labs[is_na] else trimws(x) %in% trimws(labs[is_na])",
      paste0("  ", obj, "[[col]][hit] <- NA"),
      "}"
    )
  }
  # 關鍵字裡的特殊字元（括號、點）要先跳脫才能放進正規表示式
  words <- gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", keywords)
  header <- c(
    "# 數值標籤含這些字的值，視為遺漏值代碼（| 是「或」）",
    paste0("na_words <- ", r_str(paste(words, collapse = "|"))),
    if (length(exclude) > 0) {
      paste0("# 不處理：", paste(exclude, collapse = "、"), "（如 ID 欄）")
    }
  )
  body <- paste(vapply(seq_along(f$obj), one, character(1)), collapse = "\n\n")
  check <- paste0("colSums(is.na(", f$obj[1], "))   # 檢查：各欄 NA 數")
  list(
    tidy = code_lines("library(haven)", "", header, "", body, "", check),
    base = code_lines(header, "", body, "", check)
  )
}

#' 各檔 ID 欄名不同時，先把 ID 改成共同名稱的程式碼（同名時回傳 NULL）
#' @noRd
code_rename_ids <- function(f, by_vars, id_cols) {
  if (is.null(id_cols) || length(id_cols) != length(f$obj)) return(NULL)
  differs <- which(!purrr::map_lgl(id_cols, identical, by_vars))
  if (length(differs) == 0) return(NULL)
  tidy <- vapply(differs, function(i) {
    pairs <- paste(r_col(by_vars), "=", r_col(id_cols[[i]]), collapse = ", ")
    paste0(f$obj[i], " <- rename(", f$obj[i], ", ", pairs, ")")
  }, character(1))
  base <- vapply(differs, function(i) {
    obj <- f$obj[i]
    paste0(
      "names(", obj, ")[match(", r_str(id_cols[[i]]), ", names(", obj, "))] <- ",
      r_str(by_vars)
    )
  }, character(1))
  note <- paste0("# 0. 各檔的 ID 欄名不同：先改成相同的名稱（", paste(by_vars, collapse = "、"), "）")
  list(tidy = c(note, tidy, ""), base = c(note, base, ""))
}

#' ID 型別不一致（一波是數字、一波是文字）時，先都轉成文字的程式碼
#'
#' 和 app 相同：只有型別不一致時才轉；數字不用科學記號、文字去掉前後空白。
#' 型別未知（沒有上傳檔案）時回傳 NULL。
#' @param idx 檔案在 f 中的位置
#' @param cols_by_file 各檔原本的 ID 欄名（list，查型別用）
#' @param out_cols 改名後的 ID 欄名（程式碼中用這個名稱）
#' @noRd
code_ids_to_text <- function(f, idx, cols_by_file, out_cols) {
  if (is.null(f$types)) return(NULL)
  lines <- character(0)
  for (k in seq_along(out_cols)) {
    kinds <- vapply(seq_along(idx), function(j) {
      type <- code_type_of(f, idx[j], cols_by_file[[j]][k])
      if (is.na(type)) NA_character_ else sub("\\+labels$", "", type)
    }, character(1))
    if (anyNA(kinds) || length(unique(kinds)) <= 1) next
    for (j in seq_along(idx)) {
      target <- r_dollar(f$obj[idx[j]], out_cols[k])
      lines <- c(lines, if (kinds[j] == "numeric") {
        paste0(target, " <- ", r_num_to_text(target))
      } else {
        paste0(target, " <- trimws(", target, ")")
      })
    }
  }
  if (length(lines) == 0) return(NULL)
  c(
    "# ID 一波是數字、一波是文字：先都轉成文字再比對",
    "# （formatC() 讓 100000 不會變成 1e+05）",
    lines,
    ""
  )
}

code_join <- function(f, by_vars, join_method, id_cols = NULL) {
  if (length(by_vars) == 0) by_vars <- "id"
  if (length(join_method) == 0 || !join_method %in% KIT_JOIN_METHODS) {
    join_method <- "left_join"
  }
  if (is.null(id_cols) || length(id_cols) != length(f$obj)) {
    id_cols <- rep(list(by_vars), length(f$obj))
  }
  renames <- code_rename_ids(f, by_vars, id_cols)
  to_text <- code_ids_to_text(f, seq_along(f$obj), id_cols, by_vars)
  wide <- paste0("wide_", seq_along(f$obj))
  suffix <- join_suffix(f$wave)

  tidy_rename <- paste0(
    wide, " <- rename_with(", f$obj, ", ~ paste0(.x, ", vapply(suffix, r_str, ""), "), ",
    r_minus_cols(by_vars), ")"
  )
  tidy_join <- c(
    paste0("joined <- ", wide[1], " |>"),
    paste0(
      "  ", join_method, "(", wide[-1], ", by = ", r_str(by_vars),
      ", na_matches = \"never\")",
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
        ")[to_rename], ", r_str(suffix[i]), ")"
      )
    )
  }, character(1))
  merge_arg <- paste0(
    switch(join_method,
      left_join = ", all.x = TRUE",
      full_join = ", all = TRUE",
      ""
    ),
    # incomparables 只能用在單一 ID 欄
    if (length(by_vars) == 1) ", incomparables = NA"
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
  merge_note <- c(
    switch(join_method,
      left_join = "# all.x = TRUE 相當於 left_join；merge() 會依 key 重新排序",
      full_join = "# all = TRUE 相當於 full_join；merge() 會依 key 重新排序",
      "# 不加 all 參數相當於 inner_join；merge() 會依 key 重新排序"
    ),
    if (length(by_vars) == 1) {
      "# incomparables = NA：ID 空白的列不互相配對（和 app 一樣）"
    } else {
      "# 注意：多個 ID 欄時 merge() 會把 ID 空白的列互相配對；請先刪掉 ID 空白的列"
    }
  )

  list(
    tidy = code_lines(
      "library(dplyr)", "",
      renames$tidy,
      to_text,
      "# 1. 非 ID 欄位加上波次後綴，避免同名欄衝突",
      tidy_rename, "",
      "# 2. 依序合併；na_matches = \"never\"：ID 空白的列不互相配對（和 app 一樣）",
      tidy_join, "",
      "dim(joined)   # 列數、欄數"
    ),
    base = code_lines(
      renames$base,
      to_text,
      "# 1. 非 ID 欄位加上波次後綴，避免同名欄衝突",
      paste(base_rename, collapse = "\n\n"), "",
      "# 2. 依序合併",
      merge_note,
      base_join, "",
      "dim(joined)   # 列數、欄數"
    )
  )
}

#' ④ 各波型別不同的同名欄：先都轉成文字（和 app 相同；有數值標籤的用標籤文字）
#' @noRd
code_stack_to_text <- function(f) {
  if (is.null(f$types) || is.null(f$cols)) return(NULL)
  all_cols <- unique(unlist(f$cols))
  lines <- character(0)
  for (col in all_cols) {
    types <- vapply(seq_along(f$obj), function(i) {
      if (col %in% f$cols[[i]]) code_type_of(f, i, col) else NA_character_
    }, character(1))
    sigs <- sub("\\+labels$", "", types)
    if (length(unique(stats::na.omit(sigs[sigs != "empty"]))) <= 1) next
    for (i in which(!is.na(types))) {
      target <- r_dollar(f$obj[i], col)
      lines <- c(lines, if (grepl("\\+labels$", types[i])) {
        paste0(target, " <- as.character(haven::as_factor(", target, "))")
      } else if (sigs[i] == "numeric") {
        paste0(target, " <- ", r_num_to_text(target))
      } else {
        paste0(target, " <- as.character(", target, ")")
      })
    }
  }
  if (length(lines) == 0) return(NULL)
  c("# 這些欄在各波的型別不同（一波數字、一波文字）：先都轉成文字", lines, "")
}

code_stack <- function(f) {
  wave_col <- if (is.null(f$cols)) "wave" else unused_name("wave", unlist(f$cols))
  wave_str <- r_str(wave_col)
  long <- paste0("long_", seq_along(f$obj))
  to_text <- code_stack_to_text(f)
  waves <- vapply(f$wave, r_str, character(1), USE.NAMES = FALSE)
  list(
    tidy = code_lines(
      "library(dplyr)", "",
      to_text,
      paste0("# .id = ", wave_str, "：用 list() 裡的名稱（波次）建立 ", wave_col, " 欄"),
      "stacked <- bind_rows(",
      "  list(",
      paste0("    ", waves, " = ", f$obj, c(rep(",", length(f$obj) - 1), "")),
      "  ),",
      paste0("  .id = ", wave_str),
      ")",
      "",
      paste0("count(stacked, ", r_col(wave_col), ")   # 各波次列數")
    ),
    base = code_lines(
      to_text,
      paste0("# 1. 每個檔案加上 ", wave_col, " 欄"),
      paste0(long, " <- ", f$obj, "\n", long, "[[", wave_str, "]] <- ", waves),
      "",
      "# 2. rbind() 需要欄位相同：先補上缺少的欄位（填 NA）",
      paste0(
        "all_cols <- unique(c(",
        paste0("names(", long, ")", collapse = ", "),
        "))"
      ),
      paste0(long, "[setdiff(all_cols, names(", long, "))] <- NA"),
      "",
      "# 3. 上下疊起來，波次欄放第一欄",
      paste0("stacked <- rbind(", paste(long, collapse = ", "), ")"),
      paste0("stacked <- stacked[c(", wave_str, ", setdiff(names(stacked), ", wave_str, "))]"),
      "",
      paste0("table(stacked[[", wave_str, "]])   # 各波次列數")
    )
  )
}

code_attrition <- function(f, base_i, follow_i, id_vars, follow_id_vars = id_vars) {
  if (length(id_vars) == 0) id_vars <- "id"
  if (length(follow_id_vars) != length(id_vars)) follow_id_vars <- id_vars
  if (length(base_i) == 0 || is.na(base_i)) base_i <- 1L
  if (length(follow_i) == 0 || is.na(follow_i)) {
    follow_i <- min(2L, length(f$obj))
  }
  base_obj <- f$obj[base_i]
  follow_obj <- f$obj[follow_i]
  flag <- if (is.null(f$cols)) {
    "attrition"
  } else {
    unused_name("attrition", f$cols[[base_i]])
  }
  flag_col <- r_col(flag)
  rule <- paste0(
    "# 前期（", base_obj, "）的 ID 有出現在後期（", follow_obj, "）→ 0 留存，否則 1 流失；",
    "ID 空白 → NA"
  )
  # 前後期的 ID 欄名可能不同，所以用 code_ids_to_text_pair()
  to_text <- code_ids_to_text_pair(f, base_i, follow_i, id_vars, follow_id_vars)

  if (length(id_vars) == 1) {
    id <- r_col(id_vars)
    lead <- c(
      paste0(
        "followup_ids <- ", r_dollar(follow_obj, follow_id_vars),
        "[!is.na(", r_dollar(follow_obj, follow_id_vars), ")]"
      ),
      ""
    )
    tidy_body <- c(
      paste0("attrition_df <- ", base_obj, " |>"),
      paste0(
        "  mutate(", flag_col, " = if_else(is.na(", id, "), NA, if_else(",
        id, " %in% followup_ids, 0, 1)))"
      )
    )
    base_body <- c(
      paste0("attrition_df <- ", base_obj),
      paste0(
        "attrition_df[[", r_str(flag), "]] <- ifelse(is.na(", r_dollar("attrition_df", id_vars),
        "), NA, ifelse(", r_dollar("attrition_df", id_vars), " %in% followup_ids, 0, 1))"
      )
    )
  } else {
    key_of <- function(obj, cols) {
      paste0("paste(", paste(r_dollar(obj, cols), collapse = ", "), ")")
    }
    any_na <- function(obj, cols) {
      paste(paste0("is.na(", r_dollar(obj, cols), ")"), collapse = " | ")
    }
    lead <- c(
      "# 多個 ID 欄位：先用 paste() 黏成一個比對用的鍵；ID 有空白的列不算",
      paste0("followup_key <- ", key_of(follow_obj, follow_id_vars)),
      paste0("followup_key <- followup_key[!(", any_na(follow_obj, follow_id_vars), ")]"),
      ""
    )
    tidy_body <- c(
      paste0("attrition_df <- ", base_obj, " |>"),
      paste0(
        "  mutate(", flag_col, " = if_else(",
        paste(paste0("is.na(", r_col(id_vars), ")"), collapse = " | "),
        ", NA, if_else(paste(", paste(r_col(id_vars), collapse = ", "),
        ") %in% followup_key, 0, 1)))"
      )
    )
    base_body <- c(
      paste0("attrition_df <- ", base_obj),
      paste0(
        "attrition_df[[", r_str(flag), "]] <- ifelse(", any_na("attrition_df", id_vars),
        ", NA, ifelse(", key_of("attrition_df", id_vars), " %in% followup_key, 0, 1))"
      )
    )
  }

  list(
    tidy = code_lines(
      "library(dplyr)", "",
      to_text, rule, lead, tidy_body, "",
      paste0("count(attrition_df, ", flag_col, ")")
    ),
    base = code_lines(
      to_text, rule, lead, base_body, "",
      paste0("table(attrition_df[[", r_str(flag), "]], useNA = \"ifany\")")
    )
  )
}

#' ⑤ 前期與後期的 ID 型別不一致時，先都轉成文字（欄名可以不同）
#' @noRd
code_ids_to_text_pair <- function(f, base_i, follow_i, id_vars, follow_id_vars) {
  if (is.null(f$types)) return(NULL)
  lines <- character(0)
  for (k in seq_along(id_vars)) {
    sides <- list(c(base_i, id_vars[k]), c(follow_i, follow_id_vars[k]))
    kinds <- vapply(sides, function(s) {
      type <- code_type_of(f, as.integer(s[1]), s[2])
      if (is.na(type)) NA_character_ else sub("\\+labels$", "", type)
    }, character(1))
    if (anyNA(kinds) || length(unique(kinds)) <= 1) next
    for (j in seq_along(sides)) {
      target <- r_dollar(f$obj[as.integer(sides[[j]][1])], sides[[j]][2])
      lines <- c(lines, if (kinds[j] == "numeric") {
        paste0(target, " <- ", r_num_to_text(target))
      } else {
        paste0(target, " <- trimws(", target, ")")
      })
    }
  }
  if (length(lines) == 0) return(NULL)
  c(
    "# ID 一波是數字、一波是文字：先都轉成文字再比對",
    "# （formatC() 讓 100000 不會變成 1e+05）",
    lines,
    ""
  )
}
