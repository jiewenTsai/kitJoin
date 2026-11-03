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
  dict
}

#' 一格「;」或「；」分隔的值：「97; 98」→ c("97", "98")；空白 → character(0)
#' @noRd
dict_split <- function(s, sep) {
  s <- as.character(s)
  if (length(s) == 0 || is.na(s) || !nzchar(trimws(s))) return(character(0))
  trimws(strsplit(s, sep, perl = TRUE)[[1]])
}

#' 讀變項檔：每個變項的變項標籤、數值標籤、自訂遺漏值
#'
#' 接受兩種格式：labelled::generate_dictionary() 的（variable、label、value_labels、
#' na_values、na_range）與 kitJoin codebook 的（欄位、變項標籤、數值標籤、使用者自訂遺漏值）。
#' @return 具名 list：名稱是變項，值是 list(label, values, labels, na_values, na_range)；
#'   values 是數值標籤的值（文字），labels 是對應的標籤
#' @noRd
read_variables_file <- function(path, name) {
  dict <- read_csv_auto(path)$data
  cols <- names(dict)
  text <- function(col) if (col %in% cols) as.character(dict[[col]]) else rep(NA_character_, nrow(dict))
  if ("variable" %in% cols) {
    vars <- text("variable")
    label <- text("label")
    # 「[1] 男; [2] 女」：每一段是「[值] 標籤」
    vl <- lapply(text("value_labels"), function(s) {
      parts <- dict_split(s, "; (?=\\[)")
      list(
        values = sub("^\\[(.*?)\\] ?.*$", "\\1", parts, perl = TRUE),
        labels = sub("^\\[.*?\\] ?", "", parts, perl = TRUE)
      )
    })
    na_values <- lapply(text("na_values"), dict_split, sep = ";")
    na_range <- lapply(text("na_range"), dict_split, sep = ";")
  # 原文：欄位
  } else if ("\u6b04\u4f4d" %in% cols) {
    # 原文：欄位
    vars <- text("\u6b04\u4f4d")
    # 原文：變項標籤
    label <- text("\u8b8a\u9805\u6a19\u7c64")
    # 「1 = 男；2 = 女」
    # 原文：數值標籤
    vl <- lapply(text("\u6578\u503c\u6a19\u7c64"), function(s) {
      # 原文：[；;]
      parts <- dict_split(s, "[\uff1b;]")
      list(
        values = trimws(sub("^(.*?) = .*$", "\\1", parts, perl = TRUE)),
        labels = sub("^.*? = ", "", parts, perl = TRUE)
      )
    })
    # 「97、98、90～99」：有「～」的是範圍
    # 原文：使用者自訂遺漏值 | [、,，]
    na <- lapply(text("\u4f7f\u7528\u8005\u81ea\u8a02\u907a\u6f0f\u503c"), dict_split, sep = "[\u3001,\uff0c]")
    # 原文：～
    na_values <- lapply(na, function(x) x[!grepl("\uff5e", x)])
    na_range <- lapply(na, function(x) {
      # 原文：～
      r <- x[grepl("\uff5e", x)]
      # 原文：～
      if (length(r) == 0) character(0) else trimws(strsplit(r[1], "\uff5e")[[1]])
    })
  } else {
    kit_abort(
      # 原文：無法讀取變項檔  | ：找不到 variable 欄（labelled::generate_dictionary() 的格式）
      "\u7121\u6cd5\u8b80\u53d6\u8b8a\u9805\u6a94 ", name, "\uff1a\u627e\u4e0d\u5230 variable \u6b04\uff08labelled::generate_dictionary() \u7684\u683c\u5f0f\uff09",
      # 原文：或「欄位」欄（kitJoin 的 codebook）。
      "\u6216\u300c\u6b04\u4f4d\u300d\u6b04\uff08kitJoin \u7684 codebook\uff09\u3002"
    )
  }
  keep <- !is.na(vars) & nzchar(vars)
  out <- lapply(which(keep), function(i) list(
    label = if (is.na(label[i]) || !nzchar(label[i])) NULL else label[i],
    values = vl[[i]]$values, labels = vl[[i]]$labels,
    na_values = na_values[[i]], na_range = na_range[[i]]
  ))
  stats::setNames(out, vars[keep])
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
    df[[v]] <- x
  }
  only_dict <- setdiff(names(vars), names(df))
  no_dict <- setdiff(names(df), names(vars))
  # 原文：已用  |  貼上  |  個欄位的標籤。
  notes <- paste0("\u5df2\u7528 ", dict_name, " \u8cbc\u4e0a ", kit_num(length(hit)), " \u500b\u6b04\u4f4d\u7684\u6a19\u7c64\u3002")
  if (length(skipped) > 0) {
    notes <- c(notes, paste0(
      # 原文：這些欄位的數值標籤或遺漏值和資料的型別對不上（例如文字值配數字欄），沒有貼上：
      "\u9019\u4e9b\u6b04\u4f4d\u7684\u6578\u503c\u6a19\u7c64\u6216\u907a\u6f0f\u503c\u548c\u8cc7\u6599\u7684\u578b\u5225\u5c0d\u4e0d\u4e0a\uff08\u4f8b\u5982\u6587\u5b57\u503c\u914d\u6578\u5b57\u6b04\uff09\uff0c\u6c92\u6709\u8cbc\u4e0a\uff1a",
      # 原文：。
      kit_name_list(skipped), "\u3002"
    ))
  }
  if (length(only_dict) > 0) {
    # 原文：變項檔裡有、資料裡沒有的欄位（略過）： | 。
    notes <- c(notes, paste0("\u8b8a\u9805\u6a94\u88e1\u6709\u3001\u8cc7\u6599\u88e1\u6c92\u6709\u7684\u6b04\u4f4d\uff08\u7565\u904e\uff09\uff1a", kit_name_list(only_dict), "\u3002"))
  }
  if (length(no_dict) > 0) {
    # 原文：資料裡有、變項檔裡沒有的欄位（沒有標籤）： | 。
    notes <- c(notes, paste0("\u8cc7\u6599\u88e1\u6709\u3001\u8b8a\u9805\u6a94\u88e1\u6c92\u6709\u7684\u6b04\u4f4d\uff08\u6c92\u6709\u6a19\u7c64\uff09\uff1a", kit_name_list(no_dict), "\u3002"))
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
      # 原文：# 變項檔：一個變項一列（變項標籤、數值標籤、自訂遺漏值）
      "# \u8b8a\u9805\u6a94\uff1a\u4e00\u500b\u8b8a\u9805\u4e00\u5217\uff08\u8b8a\u9805\u6a19\u7c64\u3001\u6578\u503c\u6a19\u7c64\u3001\u81ea\u8a02\u907a\u6f0f\u503c\uff09",
      "vars <- generate_dictionary(data, details = \"full\")",
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
    paste0("vars <- read_csv(", r_str(dict_name), ", col_types = cols(.default = \"c\"))"),
    "for (col in c(\"label\", \"value_labels\", \"na_values\", \"na_range\")) {",
    "  if (!col %in% names(vars)) vars[[col]] <- NA_character_",
    "}", "",
    # 原文：# 「[1] 男; [2] 女」→ c(男 = 1, 女 = 2)
    "# \u300c[1] \u7537; [2] \u5973\u300d\u2192 c(\u7537 = 1, \u5973 = 2)",
    "parse_labels <- function(s, numeric) {",
    "  if (is.na(s) || s == \"\") return(NULL)",
    "  parts <- strsplit(s, \"; (?=\\\\[)\", perl = TRUE)[[1]]",
    "  values <- sub(\"^\\\\[(.*?)\\\\] ?.*$\", \"\\\\1\", parts, perl = TRUE)",
    "  labels <- sub(\"^\\\\[.*?\\\\] ?\", \"\", parts, perl = TRUE)",
    "  setNames(if (numeric) as.numeric(values) else values, labels)",
    "}",
    "split_num <- function(s) {",
    "  if (is.na(s) || s == \"\") NULL else as.numeric(strsplit(s, \";\")[[1]])",
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
