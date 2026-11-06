# 變項檔：SAV 轉 CSV 時一起輸出，CSV 轉 SAV 時把標籤貼回去（不碰 Shiny）
#
# 變項檔就是 labelled::generate_dictionary(details = "full") 的結果：一個變項一列，有變項標籤、
# 數值標籤（「[1] 男; [2] 女」）與 SPSS 自訂遺漏值（na_values、na_range）。在 R 裡用同樣兩行
# 就能做出一模一樣的檔案。讀回來時也接受 ②～⑦「下載 codebook」的格式（「1 = 男；2 = 女」）。

#' 變項檔的檔名：w1.csv → w1_variables.csv（.csv.gz 的變項檔也是 .csv，Excel 才開得了）
#' @noRd
convert_dict_name <- function(out_name) {
  paste0(sub("[.](csv[.]gz|csv|gz|zsav|sav)$", "", out_name, ignore.case = TRUE), "_variables.csv")
}

#' 變項檔的內容：labelled::generate_dictionary(details = "full")，清單欄轉成文字
#'
#' @param df read_sav(user_na = TRUE) 讀進來的資料（保留自訂遺漏值，na_values 才寫得出來）
#' @noRd
sav_dictionary <- function(df) {
  dict <- labelled::convert_list_columns_to_character(labelled::generate_dictionary(df, details = "full"))
  dict <- tibble::as_tibble(lapply(dict, unname))
  # 加一欄 SPSS 的測量水準（nominal、ordinal、scale；haven 與 labelled 都不讀，見 R/sav_measure.R）
  dict$measure <- unname(purrr::map_chr(df[dict$variable], col_measure))
  dict
}

#' 一格「;」或「；」分隔的值：「97; 98」→ c("97", "98")；空白 → character(0)
#' @noRd
dict_split <- function(s, sep) {
  s <- as.character(s)
  if (length(s) == 0 || is.na(s) || !nzchar(trimws(s))) return(character(0))
  x <- trimws(strsplit(s, sep, perl = TRUE)[[1]])
  x[nzchar(x)]
}

#' 變項檔的欄名：英文（labelled::generate_dictionary()）或中文（kitJoin 的 codebook）都可以，
#' 各欄分開認，也可以混用
#' @noRd
KIT_DICT_COLS <- list(
  # 原文：欄位
  variable = c("variable", "\u6b04\u4f4d"),
  # 原文：變項標籤
  label = c("label", "\u8b8a\u9805\u6a19\u7c64"),
  # 原文：數值標籤
  value_labels = c("value_labels", "\u6578\u503c\u6a19\u7c64"),
  # 原文：使用者自訂遺漏值 | 遺漏值
  na_values = c("na_values", "\u4f7f\u7528\u8005\u81ea\u8a02\u907a\u6f0f\u503c", "\u907a\u6f0f\u503c"),
  na_range = c("na_range"),
  # 原文：測量水準
  measure = c("measure", "\u6e2c\u91cf\u6c34\u6e96")
)

#' 一格數值標籤 → list(values, labels)
#'
#' 兩種寫法都可以：「[1] 男; [2] 女」（labelled）或「1 = 男；2 = 女」（kitJoin codebook）。
#' 開頭是「[」就當 labelled 的寫法。
#' @noRd
parse_value_labels <- function(s) {
  s <- trimws(as.character(s))
  if (length(s) == 0 || is.na(s) || !nzchar(s)) return(list(values = character(0), labels = character(0)))
  if (startsWith(s, "[")) {
    # 原文：[;；] ?(?=\\[)
    parts <- dict_split(s, "[;\uff1b] ?(?=\\[)")
    list(
      values = sub("^\\[(.*?)\\].*$", "\\1", parts, perl = TRUE),
      labels = trimws(sub("^\\[.*?\\]", "", parts, perl = TRUE))
    )
  } else {
    # 原文：[;；]
    parts <- dict_split(s, "[;\uff1b]")
    list(
      values = trimws(sub("=.*$", "", parts)),
      labels = trimws(sub("^[^=]*=", "", parts))
    )
  }
}

#' 自訂遺漏值 → list(na_values, na_range)
#'
#' na_values 一格可以寫「97; 98」或「97、98」，也可以混進範圍「90～99」；
#' na_range 一格寫「90; 99」（labelled）或「90～99」。
#' @noRd
parse_missing <- function(values, range) {
  # 原文：[;；、,，]
  v <- dict_split(values, "[;\uff1b\u3001,\uff0c]")
  # 原文：[～~]
  is_range <- grepl("[\uff5e~]", v)
  # 原文：[～~] | [;；～~]
  r <- if (any(is_range)) dict_split(v[is_range][1], "[\uff5e~]") else dict_split(range, "[;\uff1b\uff5e~]")
  list(na_values = v[!is_range], na_range = r)
}

#' 讀變項檔：每個變項的變項標籤、數值標籤、自訂遺漏值
#'
#' 欄名英文、中文都可以（見 KIT_DICT_COLS）；數值標籤兩種寫法都可以（見 parse_value_labels()）。
#' @return 具名 list：名稱是變項，值是 list(label, values, labels, na_values, na_range)；
#'   values 是數值標籤的值（文字），labels 是對應的標籤
#' @noRd
read_variables_file <- function(path, name) {
  dict <- read_csv_auto(path)$data
  cols <- names(dict)
  low <- tolower(trimws(cols))
  text <- function(field) {
    hit <- match(tolower(KIT_DICT_COLS[[field]]), low)
    hit <- hit[!is.na(hit)]
    if (length(hit) == 0) rep(NA_character_, nrow(dict)) else as.character(dict[[hit[1]]])
  }
  if (!any(tolower(KIT_DICT_COLS$variable) %in% low)) {
    kit_abort(
      # 原文：無法讀取變項檔  | ：找不到 variable 直欄（或中文的「欄位」直欄），
      "\u7121\u6cd5\u8b80\u53d6\u8b8a\u9805\u6a94 ", name, "\uff1a\u627e\u4e0d\u5230 variable \u76f4\u6b04\uff08\u6216\u4e2d\u6587\u7684\u300c\u6b04\u4f4d\u300d\u76f4\u6b04\uff09\uff0c",
      # 原文：不知道每一個橫列是哪個直欄。可以在「變項檔（選填）」旁下載空白變項檔照著填。
      "\u4e0d\u77e5\u9053\u6bcf\u4e00\u500b\u6a6b\u5217\u662f\u54ea\u500b\u76f4\u6b04\u3002\u53ef\u4ee5\u5728\u300c\u8b8a\u9805\u6a94\uff08\u9078\u586b\uff09\u300d\u65c1\u4e0b\u8f09\u7a7a\u767d\u8b8a\u9805\u6a94\u7167\u8457\u586b\u3002"
    )
  }
  vars <- trimws(text("variable"))
  label <- text("label")
  value_labels <- text("value_labels")
  na_values <- text("na_values")
  na_range <- text("na_range")
  measure <- dict_measure(text("measure"))
  keep <- which(!is.na(vars) & nzchar(vars))
  out <- lapply(keep, function(i) {
    vl <- parse_value_labels(value_labels[i])
    na <- parse_missing(na_values[i], na_range[i])
    list(
      label = if (is.na(label[i]) || !nzchar(trimws(label[i]))) NULL else label[i],
      values = vl$values, labels = vl$labels,
      na_values = na$na_values, na_range = na$na_range, measure = measure[i]
    )
  })
  stats::setNames(out, vars[keep])
}

#' 空白變項檔：只有真正用得到的六欄（多一欄測量水準 measure），variable 先填好資料的欄名
#' @noRd
variables_template <- function(cols) {
  tibble::tibble(
    variable = cols,
    label = NA_character_,
    value_labels = NA_character_,
    na_values = NA_character_,
    na_range = NA_character_,
    measure = NA_character_
  )
}

#' 變項檔的測量水準：英文（nominal、ordinal、scale）或中文（名義、次序、尺度）都可以；
#' 其他寫法或空白是 NA（維持原本的）
#' @noRd
dict_measure <- function(x) {
  x <- tolower(trimws(as.character(x)))
  zh <- stats::setNames(names(SAV_MEASURE_ZH), SAV_MEASURE_ZH)
  out <- dplyr::coalesce(unname(zh[x]), x)
  ifelse(out %in% names(SAV_MEASURES), out, NA_character_)
}

#' 把變項檔的標籤貼到資料上（CSV 轉 SAV 前）
#'
#' 數字欄的數值標籤與遺漏值轉成數字；文字欄保留文字（例如 "a"）。有數值標籤或遺漏值的欄
#' 變成 haven::labelled_spss()，只有變項標籤的欄設定 label 屬性。日期欄只貼變項標籤。
#' @return list(data, notes)
#' @noRd
apply_variables <- function(df, vars, dict_name) {
  hit <- intersect(names(vars), names(df))
  skipped <- character(0)
  for (v in hit) {
    spec <- vars[[v]]
    x <- df[[v]]
    if (is.logical(x) && all(is.na(x))) x <- as.double(x)
    if (is.integer(x)) x <- as.double(x)
    is_date <- inherits(x, c("Date", "POSIXt"))
    labels <- NULL
    na_values <- NULL
    na_range <- NULL
    if (!is_date && length(spec$values) > 0) {
      vals <- if (is.numeric(x)) suppressWarnings(as.numeric(spec$values)) else spec$values
      if (anyNA(vals) || anyDuplicated(vals)) {
        skipped <- c(skipped, v)
      } else {
        labels <- stats::setNames(vals, spec$labels)
      }
    }
    if (!is_date && is.numeric(x)) {
      if (length(spec$na_values) > 0) na_values <- suppressWarnings(as.numeric(spec$na_values))
      if (length(spec$na_range) == 2) na_range <- suppressWarnings(as.numeric(spec$na_range))
      if (anyNA(na_values) || anyNA(na_range)) {
        skipped <- union(skipped, v)
        na_values <- NULL
        na_range <- NULL
      }
    }
    if (!is.null(labels) || !is.null(na_values) || !is.null(na_range)) {
      x <- haven::labelled_spss(x, labels, na_values = na_values, na_range = na_range, label = spec$label)
    } else {
      attr(x, "label") <- spec$label
    }
    if (!is.na(spec$measure %||% NA_character_)) attr(x, "measure") <- spec$measure
    df[[v]] <- x
  }
  only_dict <- setdiff(names(vars), names(df))
  no_dict <- setdiff(names(df), names(vars))
  # 原文：已用  |  貼上  |  個直欄的標籤。
  notes <- paste0("\u5df2\u7528 ", dict_name, " \u8cbc\u4e0a ", kit_num(length(hit)), " \u500b\u76f4\u6b04\u7684\u6a19\u7c64\u3002")
  if (length(skipped) > 0) {
    notes <- c(notes, paste0(
      # 原文：這些直欄的數值標籤或遺漏值和資料的型別對不上（例如文字值配數字直欄），沒有貼上：
      "\u9019\u4e9b\u76f4\u6b04\u7684\u6578\u503c\u6a19\u7c64\u6216\u907a\u6f0f\u503c\u548c\u8cc7\u6599\u7684\u578b\u5225\u5c0d\u4e0d\u4e0a\uff08\u4f8b\u5982\u6587\u5b57\u503c\u914d\u6578\u5b57\u76f4\u6b04\uff09\uff0c\u6c92\u6709\u8cbc\u4e0a\uff1a",
      # 原文：。
      kit_name_list(skipped), "\u3002"
    ))
  }
  if (length(only_dict) > 0) {
    # 原文：變項檔裡有、資料裡沒有的直欄（略過）： | 。
    notes <- c(notes, paste0("\u8b8a\u9805\u6a94\u88e1\u6709\u3001\u8cc7\u6599\u88e1\u6c92\u6709\u7684\u76f4\u6b04\uff08\u7565\u904e\uff09\uff1a", kit_name_list(only_dict), "\u3002"))
  }
  if (length(no_dict) > 0) {
    # 原文：資料裡有、變項檔裡沒有的直欄（沒有標籤）： | 。
    notes <- c(notes, paste0("\u8cc7\u6599\u88e1\u6709\u3001\u8b8a\u9805\u6a94\u88e1\u6c92\u6709\u7684\u76f4\u6b04\uff08\u6c92\u6709\u6a19\u7c64\uff09\uff1a", kit_name_list(no_dict), "\u3002"))
  }
  list(data = df, notes = notes)
}

#' 名稱清單：最多列 5 個，其餘寫「等 N 個」
#' @noRd
kit_name_list <- function(x, max = 5L) {
  # 原文：、
  shown <- paste(utils::head(x, max), collapse = "\u3001")
  # 原文： 等  |  個
  if (length(x) > max) paste0(shown, " \u7b49 ", kit_num(length(x)), " \u500b") else shown
}

#' ⑧「用 R 怎麼做」：這次轉換用 R 的寫法
#'
#' @param name 原始檔名
#' @param target 輸出格式
#' @param labels_as_text SAV 轉 CSV 時數值標籤是否轉成文字
#' @param dict_name CSV 轉 SAV 時上傳的變項檔檔名（沒有為 NULL）
#' @noRd
code_convert <- function(name, target, labels_as_text = FALSE, dict_name = NULL) {
  kind <- convert_kind(name)
  if (is.na(kind) || !target %in% convert_targets(kind)) return(NULL)
  out <- convert_out_name(name, target)
  write_fun <- if (target == "csv") "write_excel_csv" else "write_csv"
  if (kind == "sav") {
    return(code_lines(
      "library(haven)", "library(readr)", "library(labelled)", "",
      # 原文：# user_na = TRUE：保留 SPSS 自訂遺漏值，變項檔才寫得出 na_values
      "# user_na = TRUE\uff1a\u4fdd\u7559 SPSS \u81ea\u8a02\u907a\u6f0f\u503c\uff0c\u8b8a\u9805\u6a94\u624d\u5beb\u5f97\u51fa na_values",
      paste0("data <- read_sav(", r_str(name), ", user_na = TRUE)"), "",
      # 原文：# 變項檔：一個變項一個橫列（變項標籤、數值標籤、自訂遺漏值）
      "# \u8b8a\u9805\u6a94\uff1a\u4e00\u500b\u8b8a\u9805\u4e00\u500b\u6a6b\u5217\uff08\u8b8a\u9805\u6a19\u7c64\u3001\u6578\u503c\u6a19\u7c64\u3001\u81ea\u8a02\u907a\u6f0f\u503c\uff09",
      "vars <- generate_dictionary(data, details = \"full\")",
      # 原文：# 測量水準（名義、次序、尺度）：R 的套件讀不到，app 自己從 SAV 檔讀；這裡留空白
      # 原文：# 測量水準（名義、次序、尺度）：R 的套件讀不到，app 自己從 SAV 檔讀；這裡留空白
      "# \u6e2c\u91cf\u6c34\u6e96\uff08\u540d\u7fa9\u3001\u6b21\u5e8f\u3001\u5c3a\u5ea6\uff09\uff1aR \u7684\u5957\u4ef6\u8b80\u4e0d\u5230\uff0capp \u81ea\u5df1\u5f9e SAV \u6a94\u8b80\uff1b\u9019\u88e1\u7559\u7a7a\u767d",
      "vars$measure <- NA",
      paste0("write_excel_csv(convert_list_columns_to_character(vars), ",
             r_str(convert_dict_name(out)), ", na = \"\")"), "",
      if (labels_as_text) {
        # 原文：# 資料檔：自訂遺漏值變成空白，數值標籤轉成文字
        c("# \u8cc7\u6599\u6a94\uff1a\u81ea\u8a02\u907a\u6f0f\u503c\u8b8a\u6210\u7a7a\u767d\uff0c\u6578\u503c\u6a19\u7c64\u8f49\u6210\u6587\u5b57",
          "data_out <- as_factor(zap_missing(data))")
      } else {
        # 原文：# 資料檔：自訂遺漏值變成空白，只留代碼
        c("# \u8cc7\u6599\u6a94\uff1a\u81ea\u8a02\u907a\u6f0f\u503c\u8b8a\u6210\u7a7a\u767d\uff0c\u53ea\u7559\u4ee3\u78bc",
          "data_out <- zap_labels(zap_missing(data))")
      },
      paste0(write_fun, "(data_out, ", r_str(out), ", na = \"\")")
    ))
  }
  read <- paste0("data <- read_csv(", r_str(name), ")")
  if (target != "sav") {
    return(code_lines("library(readr)", "", read, paste0(write_fun, "(data, ", r_str(out), ", na = \"\")")))
  }
  if (is.null(dict_name)) {
    return(code_lines(
      "library(readr)", "library(haven)", "",
      read,
      # 原文：# 沒有變項檔：寫出的 SAV 沒有變項標籤與數值標籤
      "# \u6c92\u6709\u8b8a\u9805\u6a94\uff1a\u5beb\u51fa\u7684 SAV \u6c92\u6709\u8b8a\u9805\u6a19\u7c64\u8207\u6578\u503c\u6a19\u7c64",
      paste0("write_sav(data, ", r_str(out), ")")
    ))
  }
  code_lines(
    "library(readr)", "library(haven)", "library(labelled)", "",
    read,
    # 原文：# 變項檔：英文直欄名稱 variable、label、value_labels、na_values、na_range
    "# \u8b8a\u9805\u6a94\uff1a\u82f1\u6587\u76f4\u6b04\u540d\u7a31 variable\u3001label\u3001value_labels\u3001na_values\u3001na_range",
    paste0("vars <- read_csv(", r_str(dict_name), ", col_types = cols(.default = \"c\"))"),
    "for (col in c(\"label\", \"value_labels\", \"na_values\", \"na_range\")) {",
    "  if (!col %in% names(vars)) vars[[col]] <- NA_character_",
    "}", "",
    # 原文：# 「[1] 男; [2] 女」→ c(男 = 1, 女 = 2)
    "# \u300c[1] \u7537; [2] \u5973\u300d\u2192 c(\u7537 = 1, \u5973 = 2)",
    # 原文：# 也接受「1 = 男；2 = 女」的寫法
    "# \u4e5f\u63a5\u53d7\u300c1 = \u7537\uff1b2 = \u5973\u300d\u7684\u5beb\u6cd5",
    "parse_labels <- function(s, numeric) {",
    "  if (is.na(s) || s == \"\") return(NULL)",
    "  if (startsWith(s, \"[\")) {",
    # 原文：    parts <- strsplit(s, \"[;；] ?(?=\\\\[)\", perl = TRUE)[[1]]
    "    parts <- strsplit(s, \"[;\uff1b] ?(?=\\\\[)\", perl = TRUE)[[1]]",
    "    values <- sub(\"^\\\\[(.*?)\\\\].*$\", \"\\\\1\", parts, perl = TRUE)",
    "    labels <- trimws(sub(\"^\\\\[.*?\\\\]\", \"\", parts, perl = TRUE))",
    "  } else {",
    # 原文：    parts <- trimws(strsplit(s, \"[;；]\")[[1]])
    "    parts <- trimws(strsplit(s, \"[;\uff1b]\")[[1]])",
    "    values <- trimws(sub(\"=.*$\", \"\", parts))",
    "    labels <- trimws(sub(\"^[^=]*=\", \"\", parts))",
    "  }",
    "  setNames(if (numeric) as.numeric(values) else values, labels)",
    "}",
    # 原文：# 「97; 98」或「97、98」；範圍「90; 99」或「90～99」
    "# \u300c97; 98\u300d\u6216\u300c97\u300198\u300d\uff1b\u7bc4\u570d\u300c90; 99\u300d\u6216\u300c90\uff5e99\u300d",
    "split_num <- function(s) {",
    # 原文：  if (is.na(s) || s == \"\") NULL else as.numeric(strsplit(s, \"[;；、,，～~]\")[[1]])
    "  if (is.na(s) || s == \"\") NULL else as.numeric(strsplit(s, \"[;\uff1b\u3001,\uff0c\uff5e~]\")[[1]])",
    "}", "",
    "for (i in seq_len(nrow(vars))) {",
    "  v <- vars$variable[i]",
    "  if (!v %in% names(data)) next",
    "  x <- data[[v]]",
    "  labels <- parse_labels(vars$value_labels[i], is.numeric(x))",
    "  na_values <- if (is.numeric(x)) split_num(vars$na_values[i])",
    "  na_range <- if (is.numeric(x)) split_num(vars$na_range[i])",
    "  if (!is.null(labels) || !is.null(na_values) || !is.null(na_range)) {",
    "    x <- labelled_spss(x, labels, na_values = na_values, na_range = na_range)",
    "  }",
    "  if (!is.na(vars$label[i])) var_label(x) <- vars$label[i]",
    "  data[[v]] <- x",
    "}", "",
    paste0("write_sav(data, ", r_str(out), ")")
  )
}
