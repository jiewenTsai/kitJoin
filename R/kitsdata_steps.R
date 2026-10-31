# 轉成 kits 作答檔：三個步驟用到的純函式（畫面在 mod_kitsdata.R）
#
# 第 1 步：一列一個檔案，每一格是「檔案裡的一欄」或「自己填的值」→ kits_spec() 要的
#          cohort、map、per_file、typed。
# 第 2 步：哪些欄是題目（題目表、用欄名挑）。
# 第 3 步：計分檢查：只列出要決定的事。

# 第 1 步的欄：主表三欄，其餘收在「還有…（不一定要）」
KITS_STEP1_MAIN <- c("cohort", "wave_m", "subj_id")
KITS_STEP1_MORE <- c("sex", "age_m", "wave", "visit_id")
KITS_STEP1_LABELS <- c(
  cohort = "世代", wave_m = "波次（時間點）", subj_id = "孩子編號在哪一欄",
  sex = "性別", age_m = "實際月齡", wave = "波次標籤", visit_id = "列次編號"
)

# 每頁題目表的列數
KITS_ITEMS_PAGE <- 25L

#' 第 1 步每一格的預設值：猜得到的欄名（世代不猜）
#' @return 具名 list：欄 → 每個檔案一個值（猜不到是 ""）
#' @noRd
kits_step1_guess <- function(dfs) {
  n <- length(dfs)
  each <- kits_guess_per_file(dfs)
  common <- kits_guess_map(unique(unlist(purrr::map(dfs, names))))
  out <- list(cohort = rep("", n))
  for (field in c(KITS_STEP1_MAIN[-1], KITS_STEP1_MORE)) {
    out[[field]] <- if (field %in% names(each)) each[[field]] else {
      g <- common[[field]] %||% ""
      vapply(dfs, function(df) if (nzchar(g) && g %in% names(df)) g else "", character(1))
    }
  }
  out
}

#' 第 1 步的表 → kits_spec() 的 cohort、map、per_file、typed
#'
#' 每一欄（例如時間點）要嘛每個檔案都選檔案裡的欄，要嘛每個檔案都填一個值；
#' 選填的欄可以都空白。世代每個檔案可以不一樣（有的選欄、有的填值）。
#' @param cells 具名 list：欄 → 每個檔案一個字串（欄名或自己填的值）
#' @return list(cohort, map, per_file, typed, problems)
#' @noRd
kits_step1_spec <- function(dfs, file_names, cells) {
  n <- length(dfs)
  get <- function(field) {
    v <- trimws(as.character(cells[[field]] %||% rep("", n)))
    v[is.na(v)] <- ""
    v
  }
  is_col <- function(v) purrr::map2_lgl(dfs, v, function(df, x) nzchar(x) && x %in% names(df))
  problems <- character(0)

  coh <- get("cohort")
  cohort <- purrr::map2(dfs, coh, function(df, x) {
    if (nzchar(x) && x %in% names(df)) list(type = "column", value = x) else
      list(type = "text", value = x)
  })
  if (any(!nzchar(coh))) {
    problems <- c(problems, paste0("世代還沒填：", paste(file_names[!nzchar(coh)], collapse = "、"),
                                   "（例如 M03；也可以選檔案裡的世代欄）。"))
  }

  map <- stats::setNames(as.list(rep("", nrow(KITS_MAP_ROWS))), KITS_MAP_ROWS$field)
  per_file <- list()
  typed <- list()
  for (field in KITS_MAP_ROWS$field) {
    row <- KITS_MAP_ROWS[KITS_MAP_ROWS$field == field, ]
    label <- KITS_STEP1_LABELS[[field]]
    v <- get(field)
    col <- is_col(v)
    empty <- !nzchar(v)
    if (all(empty)) {
      if (row$required) problems <- c(problems, paste0(label, "：每個檔案都要填。"))
      next
    }
    if (all(col | empty) && any(col)) {
      if (all(col) && length(unique(v)) == 1) {
        map[[field]] <- v[1]
      } else if (row$per_file) {
        per_file[[field]] <- v
      } else {
        problems <- c(problems, paste0(label, "：每個檔案要選同一個欄名。"))
      }
      if (any(empty) && (row$required || field == "subj_id")) {
        problems <- c(problems, paste0(label, "還沒選：", paste(file_names[empty], collapse = "、"), "。"))
      }
    } else if (!any(col)) {
      if (!row$typed) {
        problems <- c(problems, paste0(label, "要選檔案裡的一欄（「", v[!empty][1],
                                       "」不是欄名）。"))
      } else if (any(empty)) {
        problems <- c(problems, paste0(label, "還沒填：", paste(file_names[empty], collapse = "、"), "。"))
      } else {
        typed[[field]] <- v
      }
    } else {
      problems <- c(problems, paste0(
        label, "：有的檔案選了欄、有的填了值。請每個檔案都選欄，或每個檔案都填值。"
      ))
    }
  }
  problems <- c(problems, kits_typed_problems(file_names, typed))
  list(cohort = cohort, map = map, per_file = per_file, typed = typed, problems = unique(problems))
}

# 單位的短名稱（摘要用）
kits_unit_short <- function(unit) {
  c(month = "月", year = "年", index = "波次序號", other = "")[[unit %||% "month"]]
}

#' 第 1 步收起來時的一行：「5 個檔案 · 世代 TDCYP · 2009、2011（年）· 孩子編號 w1_id 等 5 欄」
#'
#' 世代、時間點是檔案裡的欄時，寫「世代在 cohort 欄」「時間點在 wave_m 欄」。
#' @param spec kits_step1_spec()（NULL 時都當成自己填的值）
#' @noRd
kits_step1_summary <- function(cells, n_files, unit, spec = NULL) {
  uniq <- function(v) unique(v[nzchar(v)])
  some <- function(v, k) paste0(paste(utils::head(v, k), collapse = "、"), if (length(v) > k) "…")
  coh <- uniq(trimws(cells$cohort))
  wave <- uniq(trimws(cells$wave_m))
  subj <- uniq(trimws(cells$subj_id))
  short <- kits_unit_short(unit)
  coh_col <- !is.null(spec) && any(vapply(spec$cohort, function(x) x$type == "column", logical(1)))
  wave_col <- !is.null(spec) && (nzchar(spec$map$wave_m %||% "") || !is.null(spec$per_file$wave_m))
  paste0(
    n_files, " 個檔案",
    if (coh_col) paste0(" · 世代在 ", some(coh, 3), " 欄") else paste0(" · 世代 ", some(coh, 3)),
    if (wave_col) paste0(" · 時間點在 ", some(wave, 3), " 欄") else paste0(" · ", some(wave, 6)),
    if (nzchar(short)) paste0("（", short, "）"),
    if (length(subj) == 1) paste0(" · 孩子編號 ", subj) else if (length(subj) > 1)
      paste0(" · 孩子編號 ", subj[1], " 等 ", length(subj), " 欄")
  )
}

#' 第 2 步：挑出幾欄、合成幾題、各領域幾題
#' @return list(n_cols, n_items, by_domain（具名整數）, text)
#' @noRd
kits_step2_counts <- function(settings) {
  inc <- settings[settings$include, ]
  out_id <- ifelse(is.na(inc[["item_id_out"]] %||% NA), inc$column, inc[["item_id_out"]])
  per_item <- inc[!duplicated(out_id), ]
  dom <- per_item$domain[!is.na(per_item$domain)]
  by_domain <- if (length(dom) > 0) table(factor(dom, levels = unique(dom))) else integer(0)
  n_items <- length(unique(out_id))
  text <- if (nrow(inc) == 0) "還沒有挑題目" else paste0(
    "挑出 ", kit_num(nrow(inc)), " 欄",
    if (n_items != nrow(inc)) paste0(" → 合成 ", kit_num(n_items), " 題"),
    if (length(by_domain) > 0) paste0(
      "（", paste0(names(by_domain), " ", as.integer(by_domain), " 題", collapse = "、"), "）"
    )
  )
  list(n_cols = nrow(inc), n_items = n_items, by_domain = by_domain, text = text)
}

#' 用欄名挑：開頭是這些字的欄（可以寫好幾個，用頓號、逗號或空白隔開）
#' @noRd
kits_pick_cols <- function(cols, text) {
  pre <- trimws(strsplit(text %||% "", "[、,，;；\\s]+")[[1]])
  pre <- pre[nzchar(pre)]
  if (length(pre) == 0) return(character(0))
  cols[Reduce(`|`, lapply(pre, function(p) startsWith(tolower(cols), tolower(p))))]
}

#' 第 3 步：計分檢查。只列出要決定的事（todo）、已經沒問題的（ok）、提醒（note）
#'
#' @param settings 題目設定
#' @param side_cells 性別、wave_m、age_m 裡疑似遺漏值代碼的格數
#' @param na_applied TRUE 時資料已經過「遺漏值處理」
#' @param ack list(codes = 使用者說「它們是真的分數」時的格數, shift = 「不用改」時的最低分,
#'   shifted = 已經減掉的分數)
#' @param reverse_used TRUE 時用了題目表（反向題才有意義）
#' @param side_codes kits_side_codes()：性別、wave_m、age_m 裡的代碼（欄名 → tibble(value, why)）
#' @return list：每一列 list(id, state, title, text)
#' @noRd
kits_step3_checks <- function(settings, side_cells = 0, na_applied = FALSE, ack = list(),
                              reverse_used = FALSE, side_codes = list()) {
  inc <- settings[settings$include & lengths(settings$raw) > 0, ]
  if (nrow(inc) == 0) return(list())
  row <- function(id, state, title, text = "") list(id = id, state = state, title = title, text = text)
  out <- list()

  # 遺漏值代碼：看所有納入的欄（只有代碼、沒有分數的欄也算），和「它們是真的分數」用同一個算法
  on <- settings[settings$include, ]
  codes <- on[["codes"]] %||% rep(list(kits_no_codes()), nrow(on))
  n_cells <- kits_codes_total(settings, side_cells)
  n_cols <- sum((on$n_codes %||% 0) > 0) + length(side_codes)
  vals <- sort(unique(c(unlist(purrr::map(codes, function(x) x$value)),
                        unlist(purrr::map(side_codes, function(x) x$value)))))
  val_txt <- paste(kits_id_text(utils::head(vals, 6)), collapse = "、")
  if (length(vals) > 6) val_txt <- paste0(val_txt, "…")
  if (n_cells == 0) {
    out$codes <- row("codes", "ok", if (na_applied) "遺漏值代碼已經用「遺漏值處理」改成 NA" else
      "沒有看起來像遺漏值代碼的值")
  } else if (identical(ack$codes, n_cells)) {
    out$codes <- row("codes", "ok", paste0(n_cols, " 欄的 ", val_txt, " 照原樣當成分數（你選的）"))
  } else {
    out$codes <- row(
      "codes", "todo",
      paste0(if (n_cols > 0) paste0(n_cols, " 欄有") else "有", "遺漏值代碼 ", val_txt,
             "（共 ", kit_num(n_cells), " 格）"),
      paste0("不處理的話，kits 會把 ", val_txt, " 當成分數。")
    )
  }

  # 最低分
  m <- kits_settings_shift_hint(settings)
  mins <- purrr::map_dbl(inc$score, function(s) if (all(is.na(s))) NA_real_ else min(s, na.rm = TRUE))
  tops <- purrr::map_dbl(inc$score, function(s) if (all(is.na(s))) NA_real_ else max(s, na.rm = TRUE))
  if (!is.null(m) && !identical(ack$shift, m)) {
    out$shift <- row(
      "shift", "todo", paste0(nrow(inc), " 欄的分數都從 ", kits_id_text(m), " 開始"),
      paste0("kits 要從 0 開始。全部減 ", kits_id_text(m), " 之後是 0–",
             kits_id_text(max(tops, na.rm = TRUE) - m), "。")
    )
  } else if (!is.null(m)) {
    out$shift <- row("shift", "ok", paste0("分數從 ", kits_id_text(m), " 開始，不改（你選的）"))
  } else if ((ack$shifted %||% 0) != 0) {
    out$shift <- row("shift", "ok", paste0("分數全部減了 ", kits_id_text(ack$shifted),
                                           "，現在從 0 開始"))
  } else if (any(mins > 0, na.rm = TRUE)) {
    k <- sum(mins > 0, na.rm = TRUE)
    out$shift <- row("shift", "note", paste0(k, " 欄沒有 0 分"),
                     "最低分不一樣，不能一次全部減；需要的話在下面的逐題表裡一題一題改。")
  } else {
    out$shift <- row("shift", "ok", "分數都從 0 開始")
  }

  # 領域
  und <- kits_settings_undecided(settings)
  out$domain <- if (length(und) > 0) {
    row("domain", "todo", paste0(length(und), " 欄還沒有領域"),
        "每一題都要有領域（例如 distress）。可以一次設好，或在逐題表裡一題一題設。")
  } else {
    row("domain", "ok", "每一題都有領域")
  }

  # 計分有缺、小數、負分
  no_score <- purrr::map_lgl(inc$score, anyNA)
  if (any(no_score)) {
    out$unscored <- row("unscored", "todo", paste0(sum(no_score), " 欄有原分數沒有計分"),
                        "在下面的逐題表裡補上計分（合併或還原）。")
  }
  frac <- purrr::map_lgl(inc$raw, function(r) any(r != round(r)))
  out$frac <- if (any(frac)) {
    row("frac", "todo", paste0(sum(frac), " 欄的分數有小數（可能不是題目）"),
        paste(utils::head(inc$column[frac], 5), collapse = "、"))
  } else {
    row("frac", "ok", "分數都是整數")
  }
  neg <- purrr::map_lgl(inc$score, function(s) any(s < 0, na.rm = TRUE))
  if (any(neg)) {
    out$neg <- row("neg", "note", paste0(sum(neg), " 欄有負分"),
                   "如果負數是遺漏值代碼，請先用「遺漏值處理」改成 NA。")
  }

  # 反向題（用了題目表時才看）
  if (isTRUE(reverse_used)) {
    rev <- sum(!is.na(inc$source) & startsWith(inc$source, "題目表（原題"))
    out$reverse <- row("reverse", "ok", if (rev > 0) paste0(rev, " 欄是反向題，用原題的領域") else
      "沒有反向題")
  }
  out
}

#' 納入的欄裡疑似遺漏值代碼的格數（加上性別、wave_m、age_m 的）：計分檢查與「它們是真的分數」共用
#' @noRd
kits_codes_total <- function(settings, side_cells = 0) {
  sum(settings$n_codes[settings$include] %||% 0) + side_cells
}

#' 第 3 步收起來時的一行
#' @noRd
kits_step3_summary <- function(checks) {
  ok <- purrr::keep(checks, function(x) x$state != "todo")
  pick <- intersect(c("codes", "shift", "domain"), names(ok))
  if (length(pick) == 0) return("都沒問題")
  paste(vapply(ok[pick], function(x) x$title, character(1)), collapse = " · ")
}

#' 逐題表的一格計分：「1→0 2→1 3→2」（沒改的寫「0 1 2」；種類太多寫「29 種（1–31）」）
#' @noRd
kits_score_line <- function(raw, score) {
  purrr::map2_chr(raw, score, function(r, s) {
    if (length(r) == 0) return("（沒有作答）")
    s <- s[seq_along(r)]
    changed <- is.na(s) | s != r
    if (length(r) > KITS_MAP_SHOW_MAX) {
      return(paste0(length(r), " 種（", kits_num_text(min(r)), "–", kits_num_text(max(r)), "）",
                    if (any(changed)) "，有改"))
    }
    if (!any(changed)) return(paste(kits_num_text(r), collapse = " "))
    paste0(kits_num_text(r), "→", ifelse(is.na(s), "？", kits_num_text(s)), collapse = " ")
  })
}

#' 逐題表的提醒：短短幾個字
#' @noRd
kits_item_flags <- function(settings) {
  codes <- settings[["codes"]] %||% rep(list(NULL), nrow(settings))
  purrr::pmap_chr(
    list(settings$include, settings$domain, settings$raw, settings$score, codes),
    function(include, domain, raw, score, codes) {
      out <- character(0)
      if (include && (is.na(domain) || !nzchar(domain))) out <- c(out, "沒有領域")
      if (!is.null(codes) && nrow(codes) > 0) {
        out <- c(out, paste0("代碼 ", paste(kits_id_text(codes$value), collapse = "、")))
      }
      sc <- score[!is.na(score)]
      if (anyNA(score)) out <- c(out, "有原分數沒有計分")
      if (length(sc) > 0) {
        if (min(sc) > 0) out <- c(out, "沒有 0 分")
        if (any(sc < 0)) out <- c(out, "有負分")
        if (any(sc != round(sc))) out <- c(out, "有小數")
        else if (!all(seq(min(sc), max(sc)) %in% sc)) out <- c(out, "分數有缺")
      }
      paste(out, collapse = "、")
    }
  )
}

#' 逐題表（一頁）：勾選框是 Shiny 的 checkboxGroupInput 格式（input[[id]] 是勾的欄名）
#' @param id 已經加上 namespace 的 input id
#' @param rows 這一頁的題目設定（settings 的幾列）
#' @noRd
kits_items_page_html <- function(id, rows, selected = character(0)) {
  if (nrow(rows) == 0) {
    return(shiny::tags$p(class = "kit-hint", "沒有符合的欄。"))
  }
  esc <- function(x) htmltools::htmlEscape(x)
  att <- function(x) htmltools::htmlEscape(x, attribute = TRUE)
  out <- rows[["item_id_out"]] %||% rep(NA_character_, nrow(rows))
  name <- paste0(
    "<code>", esc(rows$column), "</code>",
    ifelse(is.na(out), "", paste0(" <span class=\"kit-hint\">→ 題號 ", esc(out), "</span>"))
  )
  dom <- ifelse(is.na(rows$domain), "<span class=\"kit-muted\">（沒有）</span>", esc(rows$domain))
  tr <- paste0(
    "<tr", ifelse(rows$include, "", " class=\"kit-row-off\""), ">",
    "<td><input type=\"checkbox\" name=\"", att(id), "\" value=\"", att(rows$column), "\"",
    ifelse(rows$column %in% selected, " checked=\"checked\"", ""), "/></td>",
    "<td>", name, ifelse(rows$include, "", " <span class=\"kit-muted\">（沒挑）</span>"), "</td>",
    "<td>", dom, "</td>",
    "<td>", esc(kits_score_line(rows$raw, rows$score)), "</td>",
    "<td class=\"kit-flag\">", esc(kits_item_flags(rows)), "</td></tr>"
  )
  htmltools::HTML(paste0(
    "<div id=\"", att(id), "\" class=\"form-group shiny-input-checkboxgroup shiny-input-container\"",
    " style=\"width: auto; max-width: 100%;\">",
    "<div class=\"shiny-options-group kit-table-scroll\"><table class=\"table kit-items\">",
    "<thead><tr><th></th><th>欄位</th><th>領域</th><th>計分（原分數→計分）</th><th>提醒</th></tr></thead>",
    "<tbody>", paste(tr, collapse = ""), "</tbody></table></div></div>"
  ))
}

# ── 第 2 步：看起來像題目的欄，一組一列（給人勾選） ───────────────────────

# 每頁幾組；分數最大到多少、最多幾種才算「像題目」
KITS_GROUPS_PAGE <- 15L
KITS_GROUP_MAX_SCORE <- 20
KITS_GROUP_MAX_VALUES <- 11L

#' 每一欄的變項標籤（SPSS 的 label），去掉開頭的編號，例如「W1:D5-1   最近一星期_感覺緊張不安」
#' → 「最近一星期_感覺緊張不安」；沒有標籤是 ""
#' @noRd
kits_col_labels <- function(dfs, cols) {
  vapply(cols, function(col) {
    for (df in dfs) {
      lab <- attr(df[[col]], "label", exact = TRUE)
      if (!is.null(lab) && length(lab) == 1 && !is.na(lab) && nzchar(lab)) {
        out <- trimws(sub("^[A-Za-z]*[0-9]*:[A-Za-z0-9_.-]*[[:space:]]*", "", lab))
        return(if (nzchar(out)) out else lab)
      }
    }
    ""
  }, character(1), USE.NAMES = FALSE)
}

#' 欄名的「組名」：同一組的題目欄名只差最後的編號
#'
#' - 結尾是「_數字」：去掉數字（w1d5_1、w1d5_2 → w1d5_）
#' - 結尾是補零的數字，而且同一個開頭的欄位數都一樣（KIT 的 lana01、lana02 → lana）
#' - 開頭沒有數字的（q1、q2 → q）
#' - 其他（TDCYP 的 w1a2、w1a3 是不同的題）：一欄一組
#' @noRd
kits_col_stems <- function(cols) {
  stem <- cols
  under <- grepl("_[0-9]+[A-Za-z]?$", cols)
  stem[under] <- sub("[0-9]+[A-Za-z]?$", "", cols[under])
  rest <- which(!under & grepl("[0-9]+$", cols))
  if (length(rest) > 0) {
    base <- sub("[0-9]+$", "", cols[rest])
    num <- regmatches(cols[rest], regexpr("[0-9]+$", cols[rest]))
    for (b in unique(base)) {
      i <- which(base == b)
      padded <- length(i) > 1 && length(unique(nchar(num[i]))) == 1 && any(startsWith(num[i], "0"))
      plain <- length(i) > 1 && !grepl("[0-9]", b)   # q1、q2、item10（開頭沒有數字）
      if ((padded || plain) && nzchar(b)) stem[rest[i]] <- b
    }
  }
  stem
}

#' 看起來像題目的欄，欄名開頭一樣的放一組（w1d5_1…w1d5_5 → 「w1d5_」）
#'
#' 像題目：原分數是 0–20 的整數，至少 2 種、最多 11 種（不含疑似遺漏值代碼）。
#' 順序照資料裡出現的順序（通常就是問卷的順序）。
#' @return tibble：stem、cols（list）、n、range（「1–5」）、label（第一題的標籤）、
#'   n_in（已經納入幾欄）、domain（組裡已經有的領域，沒有是 NA）
#' @noRd
kits_item_groups <- function(settings, dfs) {
  empty <- tibble::tibble(stem = character(0), cols = list(), n = integer(0), range = character(0),
                          label = character(0), n_in = integer(0), domain = character(0))
  if (nrow(settings) == 0) return(empty)
  like <- purrr::map_lgl(settings$raw, function(r) {
    length(r) >= 2 && length(r) <= KITS_GROUP_MAX_VALUES && all(r == round(r)) &&
      min(r) >= 0 && max(r) <= KITS_GROUP_MAX_SCORE
  })
  s <- settings[like, ]
  if (nrow(s) == 0) return(empty)
  stem <- kits_col_stems(s$column)
  labels <- kits_col_labels(dfs, s$column)
  idx <- split(seq_len(nrow(s)), factor(stem, levels = unique(stem)))
  tibble::tibble(
    stem = names(idx),
    cols = lapply(idx, function(i) s$column[i]),
    n = lengths(idx),
    range = vapply(idx, function(i) {
      r <- unlist(s$raw[i])
      paste0(kits_num_text(min(r)), "–", kits_num_text(max(r)))
    }, character(1)),
    label = vapply(idx, function(i) {
      lab <- labels[i][nzchar(labels[i])]
      if (length(lab) == 0) "" else lab[1]
    }, character(1)),
    n_in = vapply(idx, function(i) sum(s$include[i]), integer(1)),
    domain = vapply(idx, function(i) {
      d <- unique(stats::na.omit(s$domain[i]))
      if (length(d) == 1) d else NA_character_
    }, character(1))
  )
}

#' 找組：欄名或題目內容（標籤）包含這幾個字（不分大小寫）
#' @noRd
kits_groups_filter <- function(groups, text) {
  f <- tolower(trimws(text %||% ""))
  if (!nzchar(f) || nrow(groups) == 0) return(groups)
  hit <- grepl(f, tolower(groups$stem), fixed = TRUE) | grepl(f, tolower(groups$label), fixed = TRUE) |
    vapply(groups$cols, function(c) any(grepl(f, tolower(c), fixed = TRUE)), logical(1))
  groups[hit, ]
}

#' 一組的欄名：「w1d5_1 … w1d5_5（5 欄）」
#' @noRd
kits_group_cols_text <- function(cols) {
  if (length(cols) == 1) return(cols)
  if (length(cols) == 2) return(paste0(cols[1], "、", cols[2], "（2 欄）"))
  paste0(cols[1], " … ", cols[length(cols)], "（", length(cols), " 欄）")
}
