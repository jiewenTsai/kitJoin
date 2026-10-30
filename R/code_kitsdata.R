# ⑥ 轉成 kits 作答檔的教學程式碼（tidyverse 版）
#
# 和其他分頁一樣：每個檔案一個物件（data_t1、data_t2…），逐行寫出；
# 只列出計分對照有改的題（一樣的對照寫在一起），pivot_longer() 由 pivot_longer_code() 產生。

#' ⑥ 的程式碼
#'
#' @param f code_files_from(files)
#' @param spec kits_spec()
#' @param na_applied TRUE 時資料已經過「② 遺漏值轉 NA」（加一行註解）
#' @param side_codes kits_side_codes()：wave_m、age_m 欄裡的遺漏值代碼
#' @return 字串
#' @noRd
code_kitsdata <- function(f, spec, na_applied = FALSE, side_codes = list()) {
  # 自己填的值：每個檔案先 mutate() 成一欄（欄名就是 kits 的欄名），後面當成那一欄
  typed <- spec$typed %||% list()
  per_file <- spec$per_file %||% list()
  map <- spec$map
  for (field in names(typed)) map[[field]] <- field
  # 每個檔案先做出 kits 的欄（各檔各選一欄時欄名不同），合併後就用 kits 的欄名
  map_out <- map
  for (field in KITS_MAP_ROWS$field) {
    if (nzchar(map[[field]] %||% "") || field %in% names(per_file)) map_out[[field]] <- field
  }
  map_out$visit_id <- "visit_id"
  idx <- seq_along(f$obj)
  long <- paste0("long_", sub("^data_", "", f$obj))
  per_file_code <- function(fun) paste(vapply(idx, fun, character(1)), collapse = "\n")

  code_lines(
    "library(dplyr)", "library(readr)", "library(tidyr)",
    if (any(tolower(tools::file_ext(f$file)) == "sav")) "library(haven)   # 讀 SAV", "",
    code_kitsdata_helpers(), "",
    grep("^data_.* <- read_", strsplit(code_read(f)$tidy, "\n")[[1]], value = TRUE),
    if (na_applied) {
      c("# 「② 遺漏值轉 NA」已經把遺漏值代碼改成 NA；",
        "# 請先執行 ② 的「用 R 怎麼做？」裡的程式碼，再接著跑下面的程式")
    },
    "",
    "# 題目設定：⑥ 下載的 kits_item_settings.csv（item_id、include、domain、",
    "# map_raw_to_score = 計分對照，寫成「原分數=計分」）",
    paste0("item_settings <- read_csv(", r_str(KITS_SETTINGS_FILE), ")"),
    "items <- item_settings$item_id[item_settings$include]",
    code_kitsdata_codes(f, spec$items, side_codes),
    code_kitsdata_maps(f, spec$items),
    "",
    "# 轉成長格式：一列一個孩子的一個波次的一題",
    per_file_code(function(i) {
      code_kitsdata_long(f$obj[i], long[i], spec$cohort[[i]], kits_file_map(map, per_file, i),
                         purrr::map(typed, i), per_file)
    }),
    "",
    "# 合併各檔，接上領域",
    paste0("kits_long <- bind_rows(", paste(long, collapse = ", "), ") |>"),
    "  left_join(select(item_settings, item_id, domain), by = \"item_id\")",
    "",
    code_kitsdata_finish(map_out),
    "",
    "# 檢查：同一列次同一題只能有一列（應該是 0 列）",
    "count(kits_long, visit_id, item_id) |> filter(n > 1)",
    "",
    "# app 為了省記憶體，是一段一段（每 5,000 列寬資料）轉換、寫進 .csv.gz；結果和這裡相同",
    'write_csv(kits_long, "kits_long.csv.gz")   # 副檔名 .gz 會自動壓縮'
  )
}

# 遺漏值代碼改成 NA：一樣的代碼寫在一起（題目設定的 missing_codes；wave_m、age_m 另外寫）
code_kitsdata_codes <- function(f, items, side_codes = list()) {
  codes <- items[["codes"]] %||% list()
  key <- purrr::map_chr(codes, function(x) {
    v <- (x %||% kits_no_codes())$value
    if (length(v) == 0) NA_character_ else paste(kits_id_text(sort(v)), collapse = ", ")
  })
  cols <- c(items$column[!is.na(key)], names(side_codes))
  key <- c(key[!is.na(key)], purrr::map_chr(side_codes, function(x) {
    paste(kits_id_text(sort(x$value)), collapse = ", ")
  }))
  if (length(cols) == 0) return(NULL)
  groups <- split(cols, factor(key, levels = unique(key)))
  lines <- purrr::imap_chr(groups, function(cols, vals) {
    vec <- if (grepl(",", vals, fixed = TRUE)) paste0("c(", vals, ")") else vals
    paste0("  mutate(across(any_of(", r_cols_vector(cols, indent = "      ", quote = TRUE),
           "), \\(x) replace(x, x %in% ", vec, ", NA)))")
  })
  per_file <- vapply(f$obj, function(obj) {
    code_lines(paste0(obj, " <- ", obj, " |>"),
               paste0(lines, c(rep(" |>", length(lines) - 1), "")))
  }, character(1))
  c("", "# 遺漏值代碼改成 NA（不寫進作答檔；題目設定的 missing_codes）", per_file)
}

# 計分對照：只寫有改的題；一樣的對照寫在一起
# 平移寫成 \(x) x - 1，反向寫成 \(x) 4 - x，其他用 case_when()
code_kitsdata_maps <- function(f, items) {
  if (!all(c("raw", "score") %in% names(items)) || nrow(items) == 0) return(NULL)
  exprs <- purrr::map2_chr(items$raw, items$score, code_map_expr)
  keep <- !is.na(exprs)
  if (!any(keep)) return(NULL)
  groups <- split(items$column[keep], factor(exprs[keep], levels = unique(exprs[keep])))
  lines <- purrr::imap_chr(groups, function(cols, expr) {
    paste0("  mutate(across(any_of(", r_cols_vector(cols, indent = "      ", quote = TRUE),
           "), \\(x) ", expr, "))")
  })
  per_file <- vapply(f$obj, function(obj) {
    code_lines(paste0(obj, " <- ", obj, " |>"),
               paste0(lines, c(rep(" |>", length(lines) - 1), "")))
  }, character(1))
  c("", "# 計分對照（原分數 → 計分；只列出有改的題）", per_file)
}

#' 一題計分對照的寫法：沒改時 NA；平移 x - 1、x + 2；反向 4 - x；其他 case_when()
#' @noRd
code_map_expr <- function(raw, score) {
  ok <- !is.na(score)
  raw <- raw[ok]
  score <- score[ok]
  if (length(raw) == 0 || all(score == raw)) return(NA_character_)
  num <- function(x) format(x, trim = TRUE, drop0trailing = TRUE, scientific = FALSE)
  d <- unique(score - raw)
  if (length(d) == 1) return(if (d < 0) paste0("x - ", num(-d)) else paste0("x + ", num(d)))
  k <- unique(score + raw)
  if (length(k) == 1) return(paste0(num(k), " - x"))
  changed <- score != raw
  pairs <- split(raw[changed], score[changed])
  cases <- purrr::imap_chr(pairs, function(r, sc) {
    lhs <- if (length(r) == 1) paste0("x == ", num(r)) else
      paste0("x %in% c(", paste(num(r), collapse = ", "), ")")
    paste0(lhs, " ~ ", num(as.numeric(sc)))
  })
  # 一般的對照用 case_when()（寫成好幾行，每行不超過 100 字）
  paste0("case_when(\n      ", paste(cases, collapse = ", "), ", .default = x\n    )")
}

# 一個檔案：做出 kits 的欄（世代、孩子編號、性別、wave_m、age_m、visit_id），
# 只留這些欄和題目欄，再 pivot_longer()
# map 是這個檔案的對應欄位（各檔各選一欄時已換成這個檔案選的欄）
code_kitsdata_long <- function(obj, long, cohort, map, typed = list(), per_file = list()) {
  co <- if (identical(cohort$type, "column")) {
    paste0("cohort = as_id(", r_col(cohort$value), ")")
  } else if (nzchar(cohort$value %||% "")) {
    paste0("cohort = ", r_str(cohort$value))
  } else {
    "cohort = NA_character_"   # 還沒有填世代（app 會擋下）
  }
  made <- character(0)   # 做出來的 kits 欄
  sets <- co
  for (field in c("subj_id", "sex", "wave_m", "age_m")) {
    col <- map[[field]] %||% ""
    if (is.na(col) || !nzchar(col)) next
    made <- c(made, field)
    num <- KITS_MAP_ROWS$type[KITS_MAP_ROWS$field == field] == "d"
    if (num && col == field) next
    sets <- c(sets, paste0(field, " = ", if (num) "as.numeric(" else "as_id(", r_col(col), ")"))
  }
  rule <- kits_visit_rule(map)
  visit <- if (is.null(rule$parts)) {
    paste0("visit_id = as_id(", r_col(map$visit_id), ")")
  } else {
    subj <- if ("subj_id" %in% made) "subj_id" else "NA"
    wave <- if (rule$parts[3] == "wave_m") "wave_m" else r_col(rule$parts[3])
    paste0("visit_id = paste(cohort, ", subj, ", as_id(", wave, "), sep = \"_\")")
  }
  keep <- c("visit_id", "cohort", made)
  args <- list(names_to = "item_id", values_to = "item_score", values_drop_na = TRUE)
  piv <- pivot_longer_code(args, "", cols_code = "any_of(items)")
  code_lines(
    paste0(long, " <- ", obj, " |>"),
    code_kitsdata_typed(typed),
    code_kitsdata_call("mutate", c(sets, visit)),
    code_kitsdata_call("select", c(keep, "any_of(items)")),
    sub("^ \\|>\\n", "", piv)
  )
}

# 一個 dplyr 呼叫（後面接 |>）：短的寫一行，長的每行不超過 90 字
code_kitsdata_call <- function(fun, parts) {
  one <- paste0("  ", fun, "(", paste(parts, collapse = ", "), ") |>")
  if (nchar(one) <= 90) return(one)
  lines <- character(0)
  line <- ""
  for (x in parts) {
    piece <- if (nzchar(line)) paste0(line, ", ", x) else x
    if (nchar(piece) > 80 && nzchar(line)) {
      lines <- c(lines, paste0(line, ","))
      line <- x
    } else {
      line <- piece
    }
  }
  c(paste0("  ", fun, "("), paste0("    ", c(lines, line)), "  ) |>")
}

# 小工具：編號轉成文字（和 app 的 kits_id_text() 相同）
code_kitsdata_helpers <- function() {
  c(
    "# 編號轉成文字：數字不寫成科學記號（100000 不會變成 1e+05），空白保持 NA",
    "as_id <- function(x) {",
    "  x <- as.vector(unclass(x))",
    "  if (!is.numeric(x)) return(as.character(x))",
    "  out <- trimws(formatC(x, format = \"fg\", digits = 15))",
    "  out[is.na(x)] <- NA",
    "  out",
    "}"
  )
}

# 固定 9 欄、固定順序與型別（依 KITS_COLUMNS）；沒有對應的欄寫成空白
code_kitsdata_finish <- function(map) {
  cast <- c(c = "as_id", d = "as.numeric", i = "as.integer")
  empty <- c(c = "NA_character_", d = "NA_real_", i = "NA_integer_")
  exprs <- purrr::pmap_chr(KITS_COLUMNS, function(column, type, required, from, meaning) {
    if (from != "map") {
      return(if (type == "i") paste0(column, " = as.integer(", column, ")") else column)
    }
    col <- map[[column]] %||% ""
    if (column == "visit_id" && !nzchar(col)) return("visit_id")
    if (!nzchar(col)) return(paste0(column, " = ", empty[[type]]))
    if (col == column) return(column)   # 每個檔案已經做好這一欄
    value <- paste0(cast[[type]], "(", r_col(col), ")")
    if (type == "d" && col == column) column else paste0(column, " = ", value)
  })
  lines <- character(0)
  line <- ""
  for (x in exprs) {
    piece <- if (nzchar(line)) paste0(line, ", ", x) else x
    if (nchar(piece) > 80) {
      lines <- c(lines, paste0(line, ","))
      line <- x
    } else {
      line <- piece
    }
  }
  code_lines(
    "# 固定 9 欄（名稱、順序、型別和 kits 相同）；沒有對應的欄寫成空白",
    "kits_long <- kits_long |>",
    "  transmute(",
    paste0("    ", c(lines, line)),
    "  )"
  )
}

# 自己填的值：mutate(wave = "m018", wave_m = 18)（數字的欄不加引號）
code_kitsdata_typed <- function(typed) {
  if (length(typed) == 0) return(NULL)
  parts <- purrr::imap_chr(typed, function(v, field) {
    v <- trimws(v %||% "")
    num <- KITS_MAP_ROWS$type[KITS_MAP_ROWS$field == field] == "d"
    value <- if (num && !is.na(suppressWarnings(as.numeric(v)))) {
      format(as.numeric(v), trim = TRUE, drop0trailing = TRUE, scientific = FALSE)
    } else {
      r_str(v)
    }
    paste0(field, " = ", value)
  })
  paste0("  mutate(", paste(parts, collapse = ", "), ") |>   # 自己填的值")
}
