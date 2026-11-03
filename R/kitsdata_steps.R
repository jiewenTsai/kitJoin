# 轉成 kits 作答檔：三個步驟用到的純函式（畫面在 mod_kitsdata.R）
#
# 第 1 步：一列一個檔案，每一格是「檔案裡的一欄」或「自己填的值」→ kits_spec() 要的
#          cohort、map、per_file、typed。
# 第 2 步：哪些欄是題目（勾組、上傳題目表、用欄名挑）。只挑題目，不設領域。
# 第 3 步：題目表：領域與計分。先列檢查清單（只列出要決定的事），下面就是題目表。
#          領域一開始是組名（kits_domain_defaults()），不改也能轉換。

# 第 1 步的欄：主表三欄，其餘收在「還有…（不一定要）」
KITS_STEP1_MAIN <- c("cohort", "wave_m", "subj_id")
KITS_STEP1_MORE <- c("sex", "age_m", "wave", "visit_id")
KITS_STEP1_LABELS <- c(
  # 原文：世代 | 波次（時間點） | 孩子編號在哪一欄
  cohort = "\u4e16\u4ee3", wave_m = "\u6ce2\u6b21\uff08\u6642\u9593\u9ede\uff09", subj_id = "\u5b69\u5b50\u7de8\u865f\u5728\u54ea\u4e00\u6b04",
  # 原文：性別 | 實際月齡 | 波次標籤 | 列次編號
  sex = "\u6027\u5225", age_m = "\u5be6\u969b\u6708\u9f61", wave = "\u6ce2\u6b21\u6a19\u7c64", visit_id = "\u5217\u6b21\u7de8\u865f"
)

# 每頁題目表的列數
KITS_ITEMS_PAGE <- 25L

#' 第 1 步每一格的預設值：猜得到的欄名，或檔名裡的世代、時間點
#'
#' 世代：檔名裡的 M03、B85，或檔案裡的世代欄（cohort）；時間點：wave_m 欄、數字的波次欄
#' （wave、time）、檔名裡的第二個 M12 或 W2、T3，或「檔案上傳」頁是數字的波次名稱。
#' 只有一個檔案、又猜不到世代與時間點時（單一世代、單一波次的研究），世代填 S1、時間點填 0
#' （只用來組列次編號），attr(, "single") 是 TRUE。
#' @return 具名 list：欄 → 每個檔案一個值（猜不到是 ""）
#' @noRd
kits_step1_guess <- function(dfs, file_names = NULL, wave_labels = NULL) {
  n <- length(dfs)
  each <- kits_guess_per_file(dfs)
  common <- kits_guess_map(unique(unlist(purrr::map(dfs, names))))
  coh_re <- "(?<![A-Za-z0-9])[MB][0-9]{2}(?![0-9])"
  # 世代：檔名裡像 M03、B85 的代碼（KIT 的 kit_M03_M12.csv → M03）；沒有時找世代欄
  coh <- rep("", n)
  if (!is.null(file_names)) {
    has <- grepl(coh_re, file_names, perl = TRUE)
    coh[has] <- regmatches(file_names, regexpr(coh_re, file_names, perl = TRUE))
  }
  for (i in seq_len(n)) {
    if (nzchar(coh[i])) next
    # 原文：^(cohort|世代)$
    hit <- names(dfs[[i]])[grepl("^(cohort|\u4e16\u4ee3)$", names(dfs[[i]]), ignore.case = TRUE)]
    if (length(hit) > 0) coh[i] <- hit[[1]]
  }
  out <- list(cohort = coh)
  for (field in c(KITS_STEP1_MAIN[-1], KITS_STEP1_MORE)) {
    out[[field]] <- if (field %in% names(each)) each[[field]] else {
      g <- common[[field]] %||% ""
      vapply(dfs, function(df) if (nzchar(g) && g %in% names(df)) g else "", character(1))
    }
  }
  # 時間點：數字的波次欄（wave、time…）
  for (i in seq_len(n)) {
    if (nzchar(out$wave_m[i])) next
    cols <- names(dfs[[i]])
    cand <- c(out$wave[i], cols[grepl("^(time|timepoint|occasion|wave_?no)$", cols, ignore.case = TRUE)])
    cand <- cand[nzchar(cand) & cand %in% cols]
    num <- cand[vapply(cand, function(x) is.numeric(dfs[[i]][[x]]), logical(1))]
    if (length(num) > 0) out$wave_m[i] <- num[[1]]
  }
  # 時間點：沒有 wave_m 欄時，用「檔案上傳」頁的波次名稱（是數字的話，例如 2009）
  if (!is.null(wave_labels) && length(wave_labels) == n) {
    num <- !is.na(suppressWarnings(as.numeric(wave_labels)))
    fill <- !nzchar(out$wave_m) & num
    out$wave_m[fill] <- wave_labels[fill]
  }
  # 時間點：檔名裡世代以外的 M12、W2、T3（kit_M03_M12.csv → 12）
  if (!is.null(file_names)) {
    for (i in seq_len(n)) {
      if (nzchar(out$wave_m[i])) next
      name <- sub(coh_re, "", file_names[i], perl = TRUE)
      tok <- regmatches(name, gregexpr("(?<![A-Za-z0-9])[MWTmwt][0-9]{1,3}(?![0-9])", name, perl = TRUE))[[1]]
      if (length(tok) > 0) out$wave_m[i] <- kits_id_text(as.numeric(substring(utils::tail(tok, 1), 2)))
    }
  }
  # 單一世代、單一波次：世代與時間點只用來組列次編號，先填好
  single <- n == 1 && !nzchar(out$cohort) && !nzchar(out$wave_m) && !nzchar(out$wave)
  if (single) {
    out$cohort <- "S1"
    out$wave_m <- "0"
  }
  attr(out, "single") <- single
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
    # 原文：世代還沒填： | 、
    problems <- c(problems, paste0("\u4e16\u4ee3\u9084\u6c92\u586b\uff1a", paste(file_names[!nzchar(coh)], collapse = "\u3001"),
                                   # 原文：（例如 M03；也可以選檔案裡的世代欄）。
                                   "\uff08\u4f8b\u5982 M03\uff1b\u4e5f\u53ef\u4ee5\u9078\u6a94\u6848\u88e1\u7684\u4e16\u4ee3\u6b04\uff09\u3002"))
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
      # 原文：：每個檔案都要填。
      if (row$required) problems <- c(problems, paste0(label, "\uff1a\u6bcf\u500b\u6a94\u6848\u90fd\u8981\u586b\u3002"))
      next
    }
    if (all(col | empty) && any(col)) {
      if (all(col) && length(unique(v)) == 1) {
        map[[field]] <- v[1]
      } else if (row$per_file) {
        per_file[[field]] <- v
      } else {
        # 原文：：每個檔案要選同一個欄名。
        problems <- c(problems, paste0(label, "\uff1a\u6bcf\u500b\u6a94\u6848\u8981\u9078\u540c\u4e00\u500b\u6b04\u540d\u3002"))
      }
      if (any(empty) && (row$required || field == "subj_id")) {
        # 原文：還沒選： | 、 | 。
        problems <- c(problems, paste0(label, "\u9084\u6c92\u9078\uff1a", paste(file_names[empty], collapse = "\u3001"), "\u3002"))
      }
    } else if (!any(col)) {
      if (!row$typed) {
        # 原文：要選檔案裡的一欄（「
        problems <- c(problems, paste0(label, "\u8981\u9078\u6a94\u6848\u88e1\u7684\u4e00\u6b04\uff08\u300c", v[!empty][1],
                                       # 原文：」不是欄名）。
                                       "\u300d\u4e0d\u662f\u6b04\u540d\uff09\u3002"))
      } else if (any(empty)) {
        # 原文：還沒填： | 、 | 。
        problems <- c(problems, paste0(label, "\u9084\u6c92\u586b\uff1a", paste(file_names[empty], collapse = "\u3001"), "\u3002"))
      } else {
        typed[[field]] <- v
      }
    } else {
      problems <- c(problems, paste0(
        # 原文：：有的檔案選了欄、有的填了值。請每個檔案都選欄，或每個檔案都填值。
        label, "\uff1a\u6709\u7684\u6a94\u6848\u9078\u4e86\u6b04\u3001\u6709\u7684\u586b\u4e86\u503c\u3002\u8acb\u6bcf\u500b\u6a94\u6848\u90fd\u9078\u6b04\uff0c\u6216\u6bcf\u500b\u6a94\u6848\u90fd\u586b\u503c\u3002"
      ))
    }
  }
  problems <- c(problems, kits_typed_problems(file_names, typed))
  list(cohort = cohort, map = map, per_file = per_file, typed = typed, problems = unique(problems))
}

# 單位的短名稱（摘要用）
kits_unit_short <- function(unit) {
  # 原文：月 | 年 | 波次序號
  c(month = "\u6708", year = "\u5e74", index = "\u6ce2\u6b21\u5e8f\u865f", other = "")[[unit %||% "month"]]
}

#' 第 1 步收起來時的一行：「5 個檔案 · 世代 TDCYP · 2009、2011（年）· 孩子編號 w1_id 等 5 欄」
#'
#' 世代、時間點是檔案裡的欄時，寫「世代在 cohort 欄」「時間點在 wave_m 欄」。
#' @param spec kits_step1_spec()（NULL 時都當成自己填的值）
#' @noRd
kits_step1_summary <- function(cells, n_files, unit, spec = NULL) {
  uniq <- function(v) unique(v[nzchar(v)])
  # 原文：、 | …
  some <- function(v, k) paste0(paste(utils::head(v, k), collapse = "\u3001"), if (length(v) > k) "\u2026")
  coh <- uniq(trimws(cells$cohort))
  wave <- uniq(trimws(cells$wave_m))
  subj <- uniq(trimws(cells$subj_id))
  short <- kits_unit_short(unit)
  coh_col <- !is.null(spec) && any(vapply(spec$cohort, function(x) x$type == "column", logical(1)))
  wave_col <- !is.null(spec) && (nzchar(spec$map$wave_m %||% "") || !is.null(spec$per_file$wave_m))
  paste0(
    # 原文： 個檔案
    n_files, " \u500b\u6a94\u6848",
    # 原文： · 世代在  |  欄 |  · 世代 
    if (coh_col) paste0(" \u00b7 \u4e16\u4ee3\u5728 ", some(coh, 3), " \u6b04") else paste0(" \u00b7 \u4e16\u4ee3 ", some(coh, 3)),
    # 原文： · 時間點在  |  欄 |  · 
    if (wave_col) paste0(" \u00b7 \u6642\u9593\u9ede\u5728 ", some(wave, 3), " \u6b04") else paste0(" \u00b7 ", some(wave, 6)),
    # 原文：（ | ）
    if (nzchar(short)) paste0("\uff08", short, "\uff09"),
    # 原文： · 孩子編號 
    if (length(subj) == 1) paste0(" \u00b7 \u5b69\u5b50\u7de8\u865f ", subj) else if (length(subj) > 1)
      # 原文： · 孩子編號  |  等  |  欄
      paste0(" \u00b7 \u5b69\u5b50\u7de8\u865f ", subj[1], " \u7b49 ", length(subj), " \u6b04")
  )
}

#' 第 2 步：挑出幾欄、幾組、合成幾題、各領域幾題
#' @return list(n_cols, n_groups, n_items, by_domain（具名整數）, text（含領域）,
#'   pick_text（第 2 步用：只有欄數、組數、合成幾題，不提領域）)
#' @noRd
kits_step2_counts <- function(settings) {
  on <- settings$include
  inc <- settings[on, ]
  out_id <- ifelse(is.na(inc[["item_id_out"]] %||% NA), inc$column, inc[["item_id_out"]])
  per_item <- inc[!duplicated(out_id), ]
  dom <- per_item$domain[!is.na(per_item$domain)]
  by_domain <- if (length(dom) > 0) table(factor(dom, levels = unique(dom))) else integer(0)
  n_items <- length(unique(out_id))
  n_groups <- if (nrow(inc) == 0) 0L else length(unique(kits_settings_stems(settings)[on]))
  # 原文： → 合成  |  題
  merged <- if (n_items != nrow(inc)) paste0(" \u2192 \u5408\u6210 ", kit_num(n_items), " \u984c")
  # 原文：還沒有挑題目
  text <- if (nrow(inc) == 0) "\u9084\u6c92\u6709\u6311\u984c\u76ee" else paste0(
    # 原文：挑出  |  欄
    "\u6311\u51fa ", kit_num(nrow(inc)), " \u6b04", merged,
    if (length(by_domain) > 0) paste0(
      # 原文：（ |  題 | 、 | ）
      "\uff08", paste0(names(by_domain), " ", as.integer(by_domain), " \u984c", collapse = "\u3001"), "\uff09"
    )
  )
  # 原文：還沒有挑題目
  pick_text <- if (nrow(inc) == 0) "\u9084\u6c92\u6709\u6311\u984c\u76ee" else paste0(
    # 原文：挑出  |  欄、 |  組
    "\u6311\u51fa ", kit_num(nrow(inc)), " \u6b04\u3001", kit_num(n_groups), " \u7d44", merged
  )
  list(n_cols = nrow(inc), n_groups = n_groups, n_items = n_items, by_domain = by_domain,
       text = text, pick_text = pick_text)
}

#' 用欄名挑：開頭是這些字的欄（可以寫好幾個，用頓號、逗號或空白隔開）
#' @noRd
kits_pick_cols <- function(cols, text) {
  # 原文：[、,，;；\\s]+
  pre <- trimws(strsplit(text %||% "", "[\u3001,\uff0c;\uff1b\\s]+")[[1]])
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
#' @param file_cols 每個檔案的欄名（list）：看各檔案之間有沒有共同的題號
#' @return list：每一列 list(id, state, title, text)
#' @noRd
kits_step3_checks <- function(settings, side_cells = 0, na_applied = FALSE, ack = list(),
                              reverse_used = FALSE, side_codes = list(), file_cols = NULL) {
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
  # 原文：、
  val_txt <- paste(kits_id_text(utils::head(vals, 6)), collapse = "\u3001")
  # 原文：…
  if (length(vals) > 6) val_txt <- paste0(val_txt, "\u2026")
  if (n_cells == 0) {
    # 原文：遺漏值代碼已經用「遺漏值處理」改成 NA
    out$codes <- row("codes", "ok", if (na_applied) "\u907a\u6f0f\u503c\u4ee3\u78bc\u5df2\u7d93\u7528\u300c\u907a\u6f0f\u503c\u8655\u7406\u300d\u6539\u6210 NA" else
      # 原文：沒有看起來像遺漏值代碼的值
      "\u6c92\u6709\u770b\u8d77\u4f86\u50cf\u907a\u6f0f\u503c\u4ee3\u78bc\u7684\u503c")
  } else if (identical(ack$codes, n_cells)) {
    # 原文： 欄的  |  照原樣當成分數（你選的）
    out$codes <- row("codes", "ok", paste0(n_cols, " \u6b04\u7684 ", val_txt, " \u7167\u539f\u6a23\u7576\u6210\u5206\u6578\uff08\u4f60\u9078\u7684\uff09"))
  } else {
    out$codes <- row(
      "codes", "todo",
      # 原文： 欄有 | 有 | 遺漏值代碼 
      paste0(if (n_cols > 0) paste0(n_cols, " \u6b04\u6709") else "\u6709", "\u907a\u6f0f\u503c\u4ee3\u78bc ", val_txt,
             # 原文：（共  |  格）
             "\uff08\u5171 ", kit_num(n_cells), " \u683c\uff09"),
      # 原文：不處理的話，kits 會把  |  當成分數。
      paste0("\u4e0d\u8655\u7406\u7684\u8a71\uff0ckits \u6703\u628a ", val_txt, " \u7576\u6210\u5206\u6578\u3002")
    )
  }

  # 最低分
  m <- kits_settings_shift_hint(settings)
  mins <- purrr::map_dbl(inc$score, function(s) if (all(is.na(s))) NA_real_ else min(s, na.rm = TRUE))
  tops <- purrr::map_dbl(inc$score, function(s) if (all(is.na(s))) NA_real_ else max(s, na.rm = TRUE))
  if (!is.null(m) && !identical(ack$shift, m)) {
    out$shift <- row(
      # 原文： 欄的分數都從  |  開始
      "shift", "todo", paste0(nrow(inc), " \u6b04\u7684\u5206\u6578\u90fd\u5f9e ", kits_id_text(m), " \u958b\u59cb"),
      # 原文：kits 要從 0 開始。全部減  |  之後是 0–
      paste0("kits \u8981\u5f9e 0 \u958b\u59cb\u3002\u5168\u90e8\u6e1b ", kits_id_text(m), " \u4e4b\u5f8c\u662f 0\u2013",
             # 原文：。
             kits_id_text(max(tops, na.rm = TRUE) - m), "\u3002")
    )
  } else if (!is.null(m)) {
    # 原文：分數從  |  開始，不改（你選的）
    out$shift <- row("shift", "ok", paste0("\u5206\u6578\u5f9e ", kits_id_text(m), " \u958b\u59cb\uff0c\u4e0d\u6539\uff08\u4f60\u9078\u7684\uff09"))
  } else if ((ack$shifted %||% 0) != 0) {
    # 原文：分數全部減了 
    out$shift <- row("shift", "ok", paste0("\u5206\u6578\u5168\u90e8\u6e1b\u4e86 ", kits_id_text(ack$shifted),
                                           # 原文：，現在從 0 開始
                                           "\uff0c\u73fe\u5728\u5f9e 0 \u958b\u59cb"))
  } else if (any(mins > 0, na.rm = TRUE)) {
    k <- sum(mins > 0, na.rm = TRUE)
    # 原文： 欄沒有 0 分
    out$shift <- row("shift", "note", paste0(k, " \u6b04\u6c92\u6709 0 \u5206"),
                     # 原文：最低分不一樣，不能一次全部減；需要的話在下面的題目表裡一題一題改。
                     "\u6700\u4f4e\u5206\u4e0d\u4e00\u6a23\uff0c\u4e0d\u80fd\u4e00\u6b21\u5168\u90e8\u6e1b\uff1b\u9700\u8981\u7684\u8a71\u5728\u4e0b\u9762\u7684\u984c\u76ee\u8868\u88e1\u4e00\u984c\u4e00\u984c\u6539\u3002")
  } else {
    # 原文：分數都從 0 開始
    out$shift <- row("shift", "ok", "\u5206\u6578\u90fd\u5f9e 0 \u958b\u59cb")
  }

  # 領域：還沒有領域的要設；還是預設組名的，要按一下才算決定（ack$dom_default 是當時的樣子）
  und <- kits_settings_undecided(settings)
  if (length(und) > 0) {
    # 原文： 欄還沒有領域
    out$domain <- row("domain", "todo", paste0(length(und), " \u6b04\u9084\u6c92\u6709\u9818\u57df"),
                      # 原文：每一題都要有領域（例如 distress）。可以一次設好，或在題目表裡一題一題設。
                      "\u6bcf\u4e00\u984c\u90fd\u8981\u6709\u9818\u57df\uff08\u4f8b\u5982 distress\uff09\u3002\u53ef\u4ee5\u4e00\u6b21\u8a2d\u597d\uff0c\u6216\u5728\u984c\u76ee\u8868\u88e1\u4e00\u984c\u4e00\u984c\u8a2d\u3002")
  }
  dd <- kits_domain_default_rows(inc)
  if (length(dd) > 0) {
    if (identical(ack$dom_default, kits_domain_default_key(inc))) {
      # 原文：領域用組名（你選的）
      out$dom_default <- row("dom_default", "ok", "\u9818\u57df\u7528\u7d44\u540d\uff08\u4f60\u9078\u7684\uff09")
    } else {
      out$dom_default <- row(
        "dom_default", "todo",
        # 原文：有  |  欄的領域還是預設的組名（例如 
        paste0("\u6709 ", kit_num(length(dd)), " \u6b04\u7684\u9818\u57df\u9084\u662f\u9810\u8a2d\u7684\u7d44\u540d\uff08\u4f8b\u5982 ",
               # 原文：）
               inc$domain[match(dd[1], inc$column)], "\uff09"),
        # 原文：kitsUI 會照領域分量尺。不改也能轉換。
        "kitsUI \u6703\u7167\u9818\u57df\u5206\u91cf\u5c3a\u3002\u4e0d\u6539\u4e5f\u80fd\u8f49\u63db\u3002"
      )
    }
  }
  # 原文：每一題都有領域
  if (length(und) == 0 && length(dd) == 0) out$domain <- row("domain", "ok", "\u6bcf\u4e00\u984c\u90fd\u6709\u9818\u57df")

  # 計分有缺、小數、負分
  no_score <- purrr::map_lgl(inc$score, anyNA)
  if (any(no_score)) {
    # 原文： 欄有原分數沒有計分
    out$unscored <- row("unscored", "todo", paste0(sum(no_score), " \u6b04\u6709\u539f\u5206\u6578\u6c92\u6709\u8a08\u5206"),
                        # 原文：在下面的題目表裡補上計分（合併或還原）。
                        "\u5728\u4e0b\u9762\u7684\u984c\u76ee\u8868\u88e1\u88dc\u4e0a\u8a08\u5206\uff08\u5408\u4f75\u6216\u9084\u539f\uff09\u3002")
  }
  frac <- purrr::map_lgl(inc$raw, function(r) any(r != round(r)))
  out$frac <- if (any(frac)) {
    # 原文： 欄的分數有小數（可能不是題目）
    row("frac", "todo", paste0(sum(frac), " \u6b04\u7684\u5206\u6578\u6709\u5c0f\u6578\uff08\u53ef\u80fd\u4e0d\u662f\u984c\u76ee\uff09"),
        # 原文：、
        paste(utils::head(inc$column[frac], 5), collapse = "\u3001"))
  } else {
    # 原文：分數都是整數
    row("frac", "ok", "\u5206\u6578\u90fd\u662f\u6574\u6578")
  }
  neg <- purrr::map_lgl(inc$score, function(s) any(s < 0, na.rm = TRUE))
  if (any(neg)) {
    # 原文： 欄有負分
    out$neg <- row("neg", "note", paste0(sum(neg), " \u6b04\u6709\u8ca0\u5206"),
                   # 原文：如果負數是遺漏值代碼，請先用「遺漏值處理」改成 NA。
                   "\u5982\u679c\u8ca0\u6578\u662f\u907a\u6f0f\u503c\u4ee3\u78bc\uff0c\u8acb\u5148\u7528\u300c\u907a\u6f0f\u503c\u8655\u7406\u300d\u6539\u6210 NA\u3002")
  }

  # 各檔案之間有沒有共同的題號：kits 靠共同題把各波放到同一把尺上
  common <- kits_common_items(settings, file_cols)
  if (!is.null(common)) {
    if (common$n_shared == 0 && !isTRUE(ack$common)) {
      out$common <- row(
        # 原文： 個檔案之間沒有共同的題號
        "common", "todo", paste0(common$n_files, " \u500b\u6a94\u6848\u4e4b\u9593\u6c92\u6709\u5171\u540c\u7684\u984c\u865f"),
        # 原文：kits 要靠各波都有的題（同一個題號）把各波放到同一把尺上。現在每個檔案的題號都不一樣
        paste0("kits \u8981\u9760\u5404\u6ce2\u90fd\u6709\u7684\u984c\uff08\u540c\u4e00\u500b\u984c\u865f\uff09\u628a\u5404\u6ce2\u653e\u5230\u540c\u4e00\u628a\u5c3a\u4e0a\u3002\u73fe\u5728\u6bcf\u500b\u6a94\u6848\u7684\u984c\u865f\u90fd\u4e0d\u4e00\u6a23",
               # 原文：（例如  | 、 | ）。
               "\uff08\u4f8b\u5982 ", paste(utils::head(common$examples, 3), collapse = "\u3001"), "\uff09\u3002",
               # 原文：如果它們是同一份量表，請回第 2 步「合成同一組題號」。
               "\u5982\u679c\u5b83\u5011\u662f\u540c\u4e00\u4efd\u91cf\u8868\uff0c\u8acb\u56de\u7b2c 2 \u6b65\u300c\u5408\u6210\u540c\u4e00\u7d44\u984c\u865f\u300d\u3002")
      )
    } else if (common$n_shared == 0) {
      # 原文：各檔案的題目不是同一份量表，不用共同題號（你選的）
      out$common <- row("common", "ok", "\u5404\u6a94\u6848\u7684\u984c\u76ee\u4e0d\u662f\u540c\u4e00\u4efd\u91cf\u8868\uff0c\u4e0d\u7528\u5171\u540c\u984c\u865f\uff08\u4f60\u9078\u7684\uff09")
    } else {
      # 原文： 個題號在 2 個以上的檔案都有（可以連結）
      out$common <- row("common", "ok", paste0(common$n_shared, " \u500b\u984c\u865f\u5728 2 \u500b\u4ee5\u4e0a\u7684\u6a94\u6848\u90fd\u6709\uff08\u53ef\u4ee5\u9023\u7d50\uff09"))
    }
  }

  # 反向題（用了題目表時才看）
  if (isTRUE(reverse_used)) {
    # 原文：題目表（原題
    rev <- sum(!is.na(inc$source) & startsWith(inc$source, "\u984c\u76ee\u8868\uff08\u539f\u984c"))
    # 原文： 欄是反向題，用原題的領域
    out$reverse <- row("reverse", "ok", if (rev > 0) paste0(rev, " \u6b04\u662f\u53cd\u5411\u984c\uff0c\u7528\u539f\u984c\u7684\u9818\u57df") else
      # 原文：沒有反向題
      "\u6c92\u6709\u53cd\u5411\u984c")
  }
  out
}

#' 納入的欄裡疑似遺漏值代碼的格數（加上性別、wave_m、age_m 的）：計分檢查與「它們是真的分數」共用
#' @noRd
kits_codes_total <- function(settings, side_cells = 0) {
  sum(settings$n_codes[settings$include] %||% 0) + side_cells
}

#' 各檔案之間共同的題號：每個納入的欄換成輸出題號，看出現在幾個檔案
#' @param file_cols 每個檔案的欄名（list）；只有一個檔案時回傳 NULL（不用看）
#' @return NULL，或 list(n_files = 有題目的檔案數, n_shared = 出現在 2 個以上檔案的題號數, examples)
#' @noRd
kits_common_items <- function(settings, file_cols) {
  if (is.null(file_cols) || length(file_cols) < 2) return(NULL)
  on <- settings[settings$include, ]
  if (nrow(on) == 0) return(NULL)
  out_id <- ifelse(is.na(on[["item_id_out"]] %||% NA), on$column, on[["item_id_out"]])
  per_file <- lapply(file_cols, function(cc) unique(out_id[on$column %in% cc]))
  used <- lengths(per_file) > 0
  if (sum(used) < 2) return(NULL)
  n_in <- table(unlist(per_file[used]))
  list(n_files = sum(used), n_shared = sum(n_in >= 2),
       examples = vapply(per_file[used], `[`, character(1), 1))
}

#' 看起來是同一份量表的幾組（各波欄名不同）：題數一樣、第一題的內容（最後一段）一樣
#' @param groups kits_item_groups() 裡已經納入的組
#' @return 組名（最多的那一群）；找不到是 character(0)
#' @noRd
kits_merge_suggest <- function(groups) {
  if (nrow(groups) < 2) return(character(0))
  # 原文：.*[_：:]
  tail <- sub(".*[_\uff1a:]", "", groups$label)
  key <- paste(groups$n, tail, sep = "|")
  key[!nzchar(tail)] <- NA
  tab <- table(key[!is.na(key)])
  if (length(tab) == 0 || max(tab) < 2) return(character(0))
  groups$stem[!is.na(key) & key == names(tab)[which.max(tab)]]
}

#' 把幾組合成同一組題號：每一組的第 1 欄 → 題號 name1、第 2 欄 → name2…（組的題數要一樣）
#' @noRd
kits_merge_groups <- function(settings, groups, stems, name) {
  name <- trimws(name %||% "")
  # 原文：請至少選 2 組（例如每一波各一組）。
  if (length(stems) < 2) kit_abort("\u8acb\u81f3\u5c11\u9078 2 \u7d44\uff08\u4f8b\u5982\u6bcf\u4e00\u6ce2\u5404\u4e00\u7d44\uff09\u3002")
  # 原文：請填題號的開頭，例如 bsrs（不要有空白）。
  if (!nzchar(name) || grepl("[[:space:]]", name)) kit_abort("\u8acb\u586b\u984c\u865f\u7684\u958b\u982d\uff0c\u4f8b\u5982 bsrs\uff08\u4e0d\u8981\u6709\u7a7a\u767d\uff09\u3002")
  g <- groups[match(stems, groups$stem), ]
  if (length(unique(g$n)) != 1) {
    # 原文：這幾組的題數不一樣（ |  題 | 、
    kit_abort(paste0("\u9019\u5e7e\u7d44\u7684\u984c\u6578\u4e0d\u4e00\u6a23\uff08", paste0(g$stem, " ", g$n, " \u984c", collapse = "\u3001"),
                     # 原文：），不能一題對一題合成。
                     "\uff09\uff0c\u4e0d\u80fd\u4e00\u984c\u5c0d\u4e00\u984c\u5408\u6210\u3002"))
  }
  if (is.null(settings[["item_id_out"]])) settings$item_id_out <- NA_character_
  for (cols in g$cols) {
    settings$item_id_out[match(cols, settings$column)] <- paste0(name, seq_along(cols))
  }
  settings
}

#' 第 3 步收起來時的一行
#' @noRd
kits_step3_summary <- function(checks) {
  ok <- purrr::keep(checks, function(x) x$state != "todo")
  pick <- intersect(c("codes", "shift", "domain", "dom_default"), names(ok))
  # 原文：都沒問題
  if (length(pick) == 0) return("\u90fd\u6c92\u554f\u984c")
  # 原文： · 
  paste(vapply(ok[pick], function(x) x$title, character(1)), collapse = " \u00b7 ")
}

#' 逐題表的一格計分：「1→0 2→1 3→2」（沒改的寫「0 1 2」；種類太多寫「29 種（1–31）」）
#' @noRd
kits_score_line <- function(raw, score) {
  purrr::map2_chr(raw, score, function(r, s) {
    # 原文：（沒有作答）
    if (length(r) == 0) return("\uff08\u6c92\u6709\u4f5c\u7b54\uff09")
    s <- s[seq_along(r)]
    changed <- is.na(s) | s != r
    if (length(r) > KITS_MAP_SHOW_MAX) {
      # 原文： 種（ | – | ）
      return(paste0(length(r), " \u7a2e\uff08", kits_num_text(min(r)), "\u2013", kits_num_text(max(r)), "\uff09",
                    # 原文：，有改
                    if (any(changed)) "\uff0c\u6709\u6539"))
    }
    if (!any(changed)) return(paste(kits_num_text(r), collapse = " "))
    # 原文：→ | ？
    paste0(kits_num_text(r), "\u2192", ifelse(is.na(s), "\uff1f", kits_num_text(s)), collapse = " ")
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
      # 原文：沒有領域
      if (include && (is.na(domain) || !nzchar(domain))) out <- c(out, "\u6c92\u6709\u9818\u57df")
      if (!is.null(codes) && nrow(codes) > 0) {
        # 原文：代碼  | 、
        out <- c(out, paste0("\u4ee3\u78bc ", paste(kits_id_text(codes$value), collapse = "\u3001")))
      }
      sc <- score[!is.na(score)]
      # 原文：有原分數沒有計分
      if (anyNA(score)) out <- c(out, "\u6709\u539f\u5206\u6578\u6c92\u6709\u8a08\u5206")
      if (length(sc) > 0) {
        # 原文：沒有 0 分
        if (min(sc) > 0) out <- c(out, "\u6c92\u6709 0 \u5206")
        # 原文：有負分
        if (any(sc < 0)) out <- c(out, "\u6709\u8ca0\u5206")
        # 原文：有小數
        if (any(sc != round(sc))) out <- c(out, "\u6709\u5c0f\u6578")
        # 原文：分數有缺
        else if (!all(seq(min(sc), max(sc)) %in% sc)) out <- c(out, "\u5206\u6578\u6709\u7f3a")
      }
      # 原文：、
      paste(out, collapse = "\u3001")
    }
  )
}

#' 逐題表（一頁）：勾選框是 Shiny 的 checkboxGroupInput 格式（input[[id]] 是勾的欄名）
#' @param id 已經加上 namespace 的 input id
#' @param rows 這一頁的題目設定（settings 的幾列）
#' @noRd
kits_items_page_html <- function(id, rows, selected = character(0)) {
  if (nrow(rows) == 0) {
    # 原文：沒有符合的欄。
    return(shiny::tags$p(class = "kit-hint", "\u6c92\u6709\u7b26\u5408\u7684\u6b04\u3002"))
  }
  esc <- function(x) htmltools::htmlEscape(x)
  att <- function(x) htmltools::htmlEscape(x, attribute = TRUE)
  out <- rows[["item_id_out"]] %||% rep(NA_character_, nrow(rows))
  name <- paste0(
    "<code>", esc(rows$column), "</code>",
    # 原文： <span class=\"kit-hint\">→ 題號 
    ifelse(is.na(out), "", paste0(" <span class=\"kit-hint\">\u2192 \u984c\u865f ", esc(out), "</span>"))
  )
  is_default <- kits_domain_is_default(rows)
  # 原文：<span class=\"kit-muted\">（沒有）</span>
  dom <- ifelse(is.na(rows$domain), "<span class=\"kit-muted\">\uff08\u6c92\u6709\uff09</span>", paste0(
    # 原文： <span class=\"kit-muted\">（組名）</span>
    esc(rows$domain), ifelse(is_default, " <span class=\"kit-muted\">\uff08\u7d44\u540d\uff09</span>", "")
  ))
  tr <- paste0(
    "<tr", ifelse(rows$include, "", " class=\"kit-row-off\""), ">",
    "<td><input type=\"checkbox\" name=\"", att(id), "\" value=\"", att(rows$column), "\"",
    ifelse(rows$column %in% selected, " checked=\"checked\"", ""), "/></td>",
    # 原文： <span class=\"kit-muted\">（沒挑）</span>
    "<td>", name, ifelse(rows$include, "", " <span class=\"kit-muted\">\uff08\u6c92\u6311\uff09</span>"), "</td>",
    "<td>", dom, "</td>",
    "<td>", esc(kits_score_line(rows$raw, rows$score)), "</td>",
    "<td class=\"kit-flag\">", esc(kits_item_flags(rows)), "</td></tr>"
  )
  htmltools::HTML(paste0(
    "<div id=\"", att(id), "\" class=\"form-group shiny-input-checkboxgroup shiny-input-container\"",
    " style=\"width: auto; max-width: 100%;\">",
    "<div class=\"shiny-options-group kit-table-scroll\"><table class=\"table kit-items\">",
    # 原文：<thead><tr><th></th><th>欄位</th><th>領域</th><th>計分（原分數→計分）</th><th>提醒</th></tr></thead>
    "<thead><tr><th></th><th>\u6b04\u4f4d</th><th>\u9818\u57df</th><th>\u8a08\u5206\uff08\u539f\u5206\u6578\u2192\u8a08\u5206\uff09</th><th>\u63d0\u9192</th></tr></thead>",
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
                          label = character(0), labels = list(), n_in = integer(0),
                          domain = character(0))
  if (nrow(settings) == 0) return(empty)
  like <- kits_group_like(settings$raw)
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
      # 原文：–
      paste0(kits_num_text(min(r)), "\u2013", kits_num_text(max(r)))
    }, character(1)),
    label = vapply(idx, function(i) {
      lab <- labels[i][nzchar(labels[i])]
      if (length(lab) == 0) "" else lab[1]
    }, character(1)),
    labels = lapply(idx, function(i) labels[i]),
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
  # 原文：、 | （2 欄）
  if (length(cols) == 2) return(paste0(cols[1], "\u3001", cols[2], "\uff082 \u6b04\uff09"))
  # 原文： …  | （ |  欄）
  paste0(cols[1], " \u2026 ", cols[length(cols)], "\uff08", length(cols), " \u6b04\uff09")
}

#' 像題目的欄：原分數是 0–20 的整數，至少 2 種、最多 11 種
#' @noRd
kits_group_like <- function(raw) {
  purrr::map_lgl(raw, function(r) {
    length(r) >= 2 && length(r) <= KITS_GROUP_MAX_VALUES && all(r == round(r)) &&
      min(r) >= 0 && max(r) <= KITS_GROUP_MAX_SCORE
  })
}

#' 每一欄的組名，和第 2 步的組一樣（像題目的欄、其他的欄分開用 kits_col_stems()）
#' @param ids 用來算組名的名稱（預設是欄名；預設領域用輸出題號，合成的組就是合成的名字）
#' @noRd
kits_settings_stems <- function(settings, ids = settings$column) {
  if (nrow(settings) == 0) return(character(0))
  like <- kits_group_like(settings$raw)
  stem <- ids
  if (any(like)) stem[like] <- kits_col_stems(ids[like])
  if (any(!like)) stem[!like] <- kits_col_stems(ids[!like])
  stem
}

# ── 第 3 步：預設領域＝組名 ────────────────────────────────────────────────

# 預設領域的 source（「領域從哪裡來」）
# 原文：組名
KITS_DOMAIN_DEFAULT <- "\u7d44\u540d"

#' 組名當領域名稱：去掉結尾的 _ . -（w1d5_ → w1d5）
#' @noRd
kits_domain_name <- function(stem) {
  out <- sub("[_.-]+$", "", stem)
  ifelse(nzchar(out), out, stem)
}

#' 沒有領域的欄（和還是預設組名的欄）領域設成組名，不設也能轉換
#'
#' 組名用輸出題號算（合成同一組題號 bsrs1、bsrs2… 的組名是 bsrs）。
#' 自己設的、題目表或設定檔給的領域不動。每次設定改了都重算，組名變了預設也跟著變。
#' @noRd
kits_domain_defaults <- function(settings) {
  if (nrow(settings) == 0) return(settings)
  redo <- is.na(settings$domain) | !nzchar(settings$domain) | kits_domain_is_default(settings)
  if (!any(redo)) return(settings)
  out <- settings[["item_id_out"]] %||% rep(NA_character_, nrow(settings))
  ids <- ifelse(is.na(out), settings$column, out)
  name <- kits_domain_name(kits_settings_stems(settings, ids))
  settings$domain[redo] <- name[redo]
  settings$source[redo] <- KITS_DOMAIN_DEFAULT
  settings
}

#' 領域還是預設組名的欄
#' @param settings 題目設定（通常只放納入的列）
#' @noRd
kits_domain_default_rows <- function(settings) {
  settings$column[kits_domain_is_default(settings)]
}

#' 每一列的領域是不是預設的組名
#' @noRd
kits_domain_is_default <- function(settings) {
  src <- settings[["source"]] %||% rep(NA_character_, nrow(settings))
  !is.na(src) & src == KITS_DOMAIN_DEFAULT
}

#' 「這樣就好」當時的樣子：哪些欄、領域是什麼。之後領域或欄變了，就要再決定一次
#' @noRd
kits_domain_default_key <- function(settings) {
  hit <- kits_domain_is_default(settings)
  key <- paste0(settings$column[hit], "=", settings$domain[hit])
  paste(sort(key), collapse = ";")
}
