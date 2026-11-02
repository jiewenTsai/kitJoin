# 轉成 kits 作答檔的教學程式碼（tidyverse 版）
#
# 和其他分頁一樣：每個檔案一個物件（data_t1、data_t2…），逐行寫出；
# 只列出計分對照有改的題（一樣的對照寫在一起），pivot_longer() 由 pivot_longer_code() 產生。

#' ⑥ 的程式碼
#'
#' @param f code_files_from(files)
#' @param spec kits_spec()
#' @param na_applied TRUE 時資料已經過「遺漏值處理」（加一行註解）
#' @param text_fix NULL 或 kits_text_fix()：文字的題目欄（轉成數字、或這個檔案不轉）
#' @return 字串
#' @noRd
code_kitsdata <- function(f, spec, na_applied = FALSE, text_fix = NULL) {
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
  renamed <- length(kits_item_rename(spec$items)) > 0
  long <- paste0("long_", sub("^data_", "", f$obj))
  per_file_code <- function(fun) paste(vapply(idx, fun, character(1)), collapse = "\n")

  code_lines(
    "library(dplyr)", "library(readr)", "library(tidyr)",
    # 原文：library(haven)   # 讀 SAV
    if (any(tolower(tools::file_ext(f$file)) == "sav")) "library(haven)   # \u8b80 SAV", "",
    code_kitsdata_helpers(labels = identical(spec$value_labels, "label")), "",
    grep("^data_.* <- read_", strsplit(code_read(f)$tidy, "\n")[[1]], value = TRUE),
    if (na_applied) {
      # 原文：# 「遺漏值處理」已經把遺漏值代碼改成 NA；
      c("# \u300c\u907a\u6f0f\u503c\u8655\u7406\u300d\u5df2\u7d93\u628a\u907a\u6f0f\u503c\u4ee3\u78bc\u6539\u6210 NA\uff1b",
        # 原文：# 請先執行「遺漏值處理」的「用 R 怎麼做」裡的程式碼，再接著跑下面的程式
        "# \u8acb\u5148\u57f7\u884c\u300c\u907a\u6f0f\u503c\u8655\u7406\u300d\u7684\u300c\u7528 R \u600e\u9ebc\u505a\u300d\u88e1\u7684\u7a0b\u5f0f\u78bc\uff0c\u518d\u63a5\u8457\u8dd1\u4e0b\u9762\u7684\u7a0b\u5f0f")
    },
    "",
    # 原文：# 題目表：「kits 作答檔」第 3 步下載的 kits_item_settings.csv（item_id、include、domain、
    "# \u984c\u76ee\u8868\uff1a\u300ckits \u4f5c\u7b54\u6a94\u300d\u7b2c 3 \u6b65\u4e0b\u8f09\u7684 kits_item_settings.csv\uff08item_id\u3001include\u3001domain\u3001",
    # 原文：# map_raw_to_score = 計分對照，寫成「原分數=計分」；item_id_out = 輸出的題號）。
    "# map_raw_to_score = \u8a08\u5206\u5c0d\u7167\uff0c\u5beb\u6210\u300c\u539f\u5206\u6578=\u8a08\u5206\u300d\uff1bitem_id_out = \u8f38\u51fa\u7684\u984c\u865f\uff09\u3002",
    # 原文：# include 是 TRUE/FALSE，其他欄都當文字讀
    "# include \u662f TRUE/FALSE\uff0c\u5176\u4ed6\u6b04\u90fd\u7576\u6587\u5b57\u8b80",
    paste0("item_settings <- read_csv(", r_str(KITS_SETTINGS_FILE),
           ", col_types = cols(include = \"l\", .default = \"c\"))"),
    "items <- item_settings$item_id[item_settings$include]",
    code_kitsdata_text_fix(f, text_fix),
    code_kitsdata_codes(spec$items),
    code_kitsdata_maps(f, spec$items),
    "",
    # 原文：# 轉成長格式：一列一個孩子的一個波次的一題
    "# \u8f49\u6210\u9577\u683c\u5f0f\uff1a\u4e00\u5217\u4e00\u500b\u5b69\u5b50\u7684\u4e00\u500b\u6ce2\u6b21\u7684\u4e00\u984c",
    per_file_code(function(i) {
      code_kitsdata_long(f$obj[i], long[i], spec$cohort[[i]], kits_file_map(map, per_file, i),
                         purrr::map(typed, i), identical(spec$value_labels, "label"))
    }),
    "",
    # 原文：# 合併各檔，接上領域；有 item_id_out 的題改成輸出的題號 | # 合併各檔，接上領域
    if (renamed) "# \u5408\u4f75\u5404\u6a94\uff0c\u63a5\u4e0a\u9818\u57df\uff1b\u6709 item_id_out \u7684\u984c\u6539\u6210\u8f38\u51fa\u7684\u984c\u865f" else "# \u5408\u4f75\u5404\u6a94\uff0c\u63a5\u4e0a\u9818\u57df",
    paste0("kits_long <- bind_rows(", paste(long, collapse = ", "), ") |>"),
    if (renamed) {
      c("  left_join(select(item_settings, item_id, item_id_out, domain), by = \"item_id\") |>",
        "  mutate(item_id = coalesce(item_id_out, item_id))")
    } else {
      "  left_join(select(item_settings, item_id, domain), by = \"item_id\")"
    },
    "",
    code_kitsdata_finish(map_out),
    "",
    # 原文：# 檢查：同一列次同一題只能有一列（應該是 0 列）
    "# \u6aa2\u67e5\uff1a\u540c\u4e00\u5217\u6b21\u540c\u4e00\u984c\u53ea\u80fd\u6709\u4e00\u5217\uff08\u61c9\u8a72\u662f 0 \u5217\uff09",
    "count(kits_long, visit_id, item_id) |> filter(n > 1)",
    "",
    # 原文：# app 為了省記憶體，是一段一段（每 5,000 列寬資料）轉換、寫進 .csv.gz；結果和這裡相同
    "# app \u70ba\u4e86\u7701\u8a18\u61b6\u9ad4\uff0c\u662f\u4e00\u6bb5\u4e00\u6bb5\uff08\u6bcf 5,000 \u5217\u5bec\u8cc7\u6599\uff09\u8f49\u63db\u3001\u5beb\u9032 .csv.gz\uff1b\u7d50\u679c\u548c\u9019\u88e1\u76f8\u540c",
    # 原文：write_csv(kits_long, "kits_long.csv.gz")   # 副檔名 .gz 會自動壓縮
    'write_csv(kits_long, "kits_long.csv.gz")   # \u526f\u6a94\u540d .gz \u6703\u81ea\u52d5\u58d3\u7e2e'
  )
}

# 文字的題目欄：內容都是數字的轉成數字；有一般文字的，這個檔案不轉這一題
code_kitsdata_text_fix <- function(f, text_fix) {
  if (is.null(text_fix)) return(NULL)
  per_file <- purrr::map_chr(seq_along(f$obj), function(i) {
    fx <- text_fix[[i]]
    if (is.null(fx) || (length(fx$convert) == 0 && length(fx$drop) == 0)) return(NA_character_)
    lines <- c(
      if (length(fx$convert) > 0) {
        paste0("  mutate(across(any_of(", r_cols_vector(fx$convert, quote = TRUE),
               "), \\(x) as.numeric(as.vector(x))))")
      },
      if (length(fx$drop) > 0) {
        paste0("  select(-any_of(", r_cols_vector(fx$drop, quote = TRUE), "))")
      }
    )
    obj <- f$obj[i]
    code_lines(paste0(obj, " <- ", obj, " |>"),
               paste0(lines, c(rep(" |>", length(lines) - 1), "")))
  })
  per_file <- per_file[!is.na(per_file)]
  if (length(per_file) == 0) return(NULL)
  # 原文：# 文字的題目欄：內容都是數字的轉成數字；有一般文字的，這個檔案不轉這一題
  c("", "# \u6587\u5b57\u7684\u984c\u76ee\u6b04\uff1a\u5167\u5bb9\u90fd\u662f\u6578\u5b57\u7684\u8f49\u6210\u6578\u5b57\uff1b\u6709\u4e00\u822c\u6587\u5b57\u7684\uff0c\u9019\u500b\u6a94\u6848\u4e0d\u8f49\u9019\u4e00\u984c", per_file)
}

# 疑似遺漏值代碼：⑥ 不改，照原樣寫進作答檔；有的話加一段註解，說要先用「遺漏值處理」改成 NA
code_kitsdata_codes <- function(items) {
  codes <- purrr::map(items[["codes"]] %||% list(), function(x) (x %||% kits_no_codes())$value)
  vals <- sort(unique(unlist(codes)))
  if (length(vals) == 0) return(NULL)
  # 原文：、
  shown <- paste(kits_id_text(utils::head(vals, 6)), collapse = "\u3001")
  c(
    "",
    # 原文：# 注意：有  |  題有疑似遺漏值代碼（
    paste0("# \u6ce8\u610f\uff1a\u6709 ", sum(lengths(codes) > 0), " \u984c\u6709\u7591\u4f3c\u907a\u6f0f\u503c\u4ee3\u78bc\uff08", shown,
           # 原文：… | ），
           if (length(vals) > 6) "\u2026", "\uff09\uff0c"),
    # 原文：# 這裡照原樣當成分數（計分對照不會動到它們）。如果它們是遺漏值，
    "# \u9019\u88e1\u7167\u539f\u6a23\u7576\u6210\u5206\u6578\uff08\u8a08\u5206\u5c0d\u7167\u4e0d\u6703\u52d5\u5230\u5b83\u5011\uff09\u3002\u5982\u679c\u5b83\u5011\u662f\u907a\u6f0f\u503c\uff0c",
    # 原文：# 請先執行「遺漏值處理」的程式碼把它們改成 NA，再跑下面的程式
    "# \u8acb\u5148\u57f7\u884c\u300c\u907a\u6f0f\u503c\u8655\u7406\u300d\u7684\u7a0b\u5f0f\u78bc\u628a\u5b83\u5011\u6539\u6210 NA\uff0c\u518d\u8dd1\u4e0b\u9762\u7684\u7a0b\u5f0f"
  )
}

# 計分對照：只寫有改的題；一樣的對照寫在一起
# 平移寫成 \(x) x - 1，反向寫成 \(x) 4 - x，其他用 case_when()；疑似遺漏值代碼照原樣留下
code_kitsdata_maps <- function(f, items) {
  if (!all(c("raw", "score") %in% names(items)) || nrow(items) == 0) return(NULL)
  codes <- purrr::map(items[["codes"]] %||% rep(list(NULL), nrow(items)),
                      function(x) (x %||% kits_no_codes())$value)
  exprs <- purrr::pmap_chr(list(items$raw, items$score, codes), code_map_expr)
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
  # 原文：# 計分對照（原分數 → 計分；只列出有改的題）
  c("", "# \u8a08\u5206\u5c0d\u7167\uff08\u539f\u5206\u6578 \u2192 \u8a08\u5206\uff1b\u53ea\u5217\u51fa\u6709\u6539\u7684\u984c\uff09", per_file)
}

#' 一題計分對照的寫法：沒改時 NA；平移 x - 1、x + 2；反向 4 - x；其他 case_when()
#'
#' @param keep 疑似遺漏值代碼：照原樣留下（平移、反向時寫成
#'   case_when(x %in% c(98, 99) ~ x, .default = x - 1)；其他 case_when() 本來就是 .default = x）
#' @noRd
code_map_expr <- function(raw, score, keep = numeric(0)) {
  ok <- !is.na(score)
  raw <- raw[ok]
  score <- score[ok]
  if (length(raw) == 0 || all(score == raw)) return(NA_character_)
  num <- function(x) format(x, trim = TRUE, drop0trailing = TRUE, scientific = FALSE)
  as_is <- function(expr) {
    if (length(keep) == 0) return(expr)
    vals <- paste(num(keep), collapse = ", ")
    if (length(keep) > 1) vals <- paste0("c(", vals, ")")
    paste0("case_when(x %in% ", vals, " ~ x, .default = ", expr, ")")
  }
  d <- unique(score - raw)
  if (length(d) == 1) return(as_is(if (d < 0) paste0("x - ", num(-d)) else paste0("x + ", num(d))))
  k <- unique(score + raw)
  if (length(k) == 1) return(as_is(paste0(num(k), " - x")))
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
code_kitsdata_long <- function(obj, long, cohort, map, typed = list(), labels = FALSE) {
  as_text <- if (labels) "as_label(" else "as_id("
  co <- if (identical(cohort$type, "column")) {
    paste0("cohort = ", as_text, r_col(cohort$value), ")")
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
    fun <- if (num) "as.numeric(" else if (field == "sex") as_text else "as_id("
    sets <- c(sets, paste0(field, " = ", fun, r_col(col), ")"))
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

# 小工具：編號轉成文字（和 app 的 kits_id_text() 相同）；labels = TRUE 時再加 as_label()
# （和 kits_label_text() 相同）
code_kitsdata_helpers <- function(labels = FALSE) {
  c(
    # 原文：# 編號轉成文字：數字不寫成科學記號（100000 不會變成 1e+05），空白保持 NA
    "# \u7de8\u865f\u8f49\u6210\u6587\u5b57\uff1a\u6578\u5b57\u4e0d\u5beb\u6210\u79d1\u5b78\u8a18\u865f\uff08100000 \u4e0d\u6703\u8b8a\u6210 1e+05\uff09\uff0c\u7a7a\u767d\u4fdd\u6301 NA",
    "as_id <- function(x) {",
    "  x <- as.vector(unclass(x))",
    "  if (!is.numeric(x)) return(as.character(x))",
    "  out <- trimws(formatC(x, format = \"fg\", digits = 15))",
    "  out[is.na(x)] <- NA",
    "  out",
    "}",
    if (labels) {
      c(
        # 原文：# 有值標籤的欄（世代、性別）用標籤；沒有標籤的值用代碼
        "# \u6709\u503c\u6a19\u7c64\u7684\u6b04\uff08\u4e16\u4ee3\u3001\u6027\u5225\uff09\u7528\u6a19\u7c64\uff1b\u6c92\u6709\u6a19\u7c64\u7684\u503c\u7528\u4ee3\u78bc",
        "as_label <- function(x) {",
        "  out <- as_id(x)",
        "  labels <- attr(x, \"labels\", exact = TRUE)",
        "  if (is.null(labels)) return(out)",
        "  m <- match(as.vector(unclass(x)), as.vector(unclass(labels)))",
        "  out[!is.na(m)] <- names(labels)[m[!is.na(m)]]",
        "  out",
        "}"
      )
    }
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
    # 原文：# 固定 9 欄（名稱、順序、型別和 kits 相同）；沒有對應的欄寫成空白
    "# \u56fa\u5b9a 9 \u6b04\uff08\u540d\u7a31\u3001\u9806\u5e8f\u3001\u578b\u5225\u548c kits \u76f8\u540c\uff09\uff1b\u6c92\u6709\u5c0d\u61c9\u7684\u6b04\u5beb\u6210\u7a7a\u767d",
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
  # 原文：) |>   # 自己填的值
  paste0("  mutate(", paste(parts, collapse = ", "), ") |>   # \u81ea\u5df1\u586b\u7684\u503c")
}
