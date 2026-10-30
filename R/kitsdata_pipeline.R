# ⑥ 轉成 kits 作答檔：寬格式（一列一個孩子的一個波次、一題一欄）→ kits 的長格式
#
# 輸出固定 9 欄、固定順序（kits 與 kitsUI 的資料約定）：
#   visit_id、subj_id、cohort、sex、wave_m、age_m、item_id、item_score、domain
# 編號與 sex 一律寫成文字；wave_m、age_m 是數字；item_score 是整數。
# 每一題的計分對照（原分數 → 計分）在寬資料上套用；遺漏值代碼（值標籤、② 的代碼、7777 以上、
# 和分數明顯分開的值）改成 NA，不會寫成分數。
# 這個檔案只有純函式（不碰 Shiny），由 mod_kitsdata.R 呼叫。

# kits 作答檔的欄位（和 kits::kitm03$responses 相同的名稱、順序與型別；不依賴 kits）
# type：c 文字、d 數字、i 整數；required：空白時不能轉換；
# from：這一頁哪裡給值（map = 「2. 對應欄位」的一列）
KITS_COLUMNS <- tibble::tribble(
  ~column, ~type, ~required, ~from, ~meaning,
  "visit_id", "c", TRUE, "map", "列次編號：一個孩子的一個波次一個編號。兩個世代不能重複。",
  "subj_id", "c", FALSE, "map", "孩子編號：同一個孩子的各波次相同。",
  "cohort", "c", TRUE, "cohort", "世代，例如 M03、M36（「1. 世代」）。",
  "sex", "c", FALSE, "map", "性別，例如 boy、girl。",
  "wave_m", "d", TRUE, "map", "波次（施測月齡），例如 3、6、12。",
  "age_m", "d", FALSE, "map", "實際月齡。題目表的通過年齡要用它。",
  "item_id", "c", TRUE, "items", "題號，例如 lana01（題目欄的欄名）。",
  "item_score", "i", TRUE, "items", "得分：0、1、2…（題目欄的值；沒有作答的題不放）。",
  "domain", "c", TRUE, "items", "領域，例如 lan、motor、cog（「3. 題目與領域」）。"
)

# 「2. 對應欄位」的列：kits 的欄（from = map）加上波次標籤（wave，不會輸出，只用來組 visit_id）
# typed：可以「自己填」（每個檔案填一個值，例如一個檔案一個波次、檔案裡沒有波次欄時）；
# per_file：可以「各檔各選一欄」（各檔欄名不同時，例如 w1_id、w2_id、w3id）；
# label：「1.」的表裡「自己填」「各檔各選一欄」那一欄的標題
KITS_MAP_ROWS <- dplyr::bind_rows(
  KITS_COLUMNS[KITS_COLUMNS$from == "map", c("column", "type", "required", "meaning")],
  tibble::tibble(
    column = "wave", type = "c", required = FALSE,
    meaning = "波次標籤，例如 m108g03（不會輸出，只用來組 visit_id）。"
  )
) |>
  dplyr::rename(field = "column") |>
  dplyr::mutate(
    required = .data$required & .data$field != "visit_id",   # visit_id 預設自動組成
    typed = .data$field %in% c("sex", "wave", "wave_m", "age_m"),
    per_file = .data$field %in% c("subj_id", "sex", "wave", "wave_m", "age_m"),
    label = unname(c(visit_id = "列次編號", subj_id = "孩子編號", sex = "性別",
                     wave_m = "波次（月齡）", age_m = "實際月齡", wave = "波次標籤")[.data$field])
  ) |>
  dplyr::slice(match(c("visit_id", "subj_id", "sex", "wave", "wave_m", "age_m"), .data$field))

KITS_MAP_FIELDS <- stats::setNames(KITS_MAP_ROWS$meaning, KITS_MAP_ROWS$field)

# 下載的題目設定檔名（「用 R 怎麼做？」也讀這個檔）
KITS_SETTINGS_FILE <- "kits_item_settings.csv"

# 「自己填」的值放進資料時用的欄名開頭（不會和使用者的欄撞名）
KITS_TYPED_PREFIX <- ".kits_typed_"

#' @importFrom dplyr .data
NULL

# 外部連結的錨題只認這三個領域名稱
KITS_ANCHOR_DOMAINS <- c("lan", "motor", "cog")

# 這個值以上的分數當成遺漏值代碼（KIT 的 7777、8888、9999）
KITS_CODE_MIN <- 7777

# 值標籤含這些字的值當成遺漏值代碼（SPSS 檔常見的寫法，例如「[98] 跳答」「[99] 遺漏值」）
KITS_CODE_LABEL_PATTERN <- paste0(
  "遺漏|遺失|跳答|跳題|不適用|不知道|拒答|拒絕|無法回答|未作答|未填答|沒有回答|",
  "missing|refus|don.?t know|not applicable|skip|^n/?a$"
)

# kitsUI 單檔上傳上限（MB，未壓縮的估計超過時提醒）
KITSUI_UPLOAD_MB <- 200


# ── 對應欄位 ─────────────────────────────────────────────────────────────────

#' 依欄名猜對應的欄
#'
#' @param cols 各檔欄位的聯集
#' @return 具名 list（names(KITS_MAP_FIELDS)）：欄名，猜不到時為 ""；
#'   visit_id 只在有同名欄時才猜（否則自動組成）
#' @noRd
kits_guess_map <- function(cols) {
  first <- function(pattern) {
    hit <- cols[grepl(pattern, cols, ignore.case = TRUE)]
    if (length(hit) == 0) "" else hit[[1]]
  }
  ids <- setdiff(kits_guess_id(cols), cols[tolower(cols) == "visit_id"])
  subj <- intersect(c("subj_id", "release_id"), cols)
  list(
    visit_id = first("^visit_id$"),
    subj_id = if (length(subj) > 0) subj[[1]] else if (length(ids) > 0) ids[[1]] else "",
    sex = first("^(sex|gender|性別)$"),
    wave = first("^wave(_label)?$"),
    wave_m = first("^wave_m$"),
    age_m = first("^(age_m|age)$")
  )
}

#' 已對應的欄（不會被當成題目）
#' @param per_file NULL 或具名 list：各檔各選一欄的對應（欄 → 每個檔案一個欄名）
#' @noRd
kits_mapped_cols <- function(map, per_file = NULL) {
  vals <- c(unlist(map, use.names = FALSE), unlist(per_file, use.names = FALSE))
  unique(vals[!is.na(vals) & nzchar(vals)])
}

#' 像 ID 的欄：id、ID、release_id、idno、id_2，也包含 w3id、w4id 這種「波次 + id」
#' @noRd
kits_guess_id <- function(cols) {
  cols[grepl("(^|[_.])id(no)?($|[_.0-9])|^[A-Za-z]{0,3}[0-9]{1,2}_?id$", cols,
             ignore.case = TRUE)]
}

#' 每個檔案各自猜對應的欄（各檔各選一欄的預設）
#' @return 具名 list（KITS_MAP_ROWS$field[per_file]）：每個檔案一個欄名（猜不到是 ""）
#' @noRd
kits_guess_per_file <- function(dfs) {
  fields <- KITS_MAP_ROWS$field[KITS_MAP_ROWS$per_file]
  guesses <- purrr::map(dfs, function(df) kits_guess_map(names(df)))
  stats::setNames(purrr::map(fields, function(field) {
    vapply(guesses, function(g) g[[field]] %||% "", character(1))
  }), fields)
}

#' 每一個對應欄位的來源預設：各檔都有同一個欄 →「某一欄」；欄名各檔不同、但每個檔案
#' 都猜得到 →「各檔各選一欄」；都沒有時必填的欄是「某一欄」，選填的欄是「不用」
#' @return 具名字元向量（column、per_file、none）
#' @noRd
kits_guess_sources <- function(dfs) {
  cols <- unique(unlist(purrr::map(dfs, names)))
  common <- kits_guess_map(cols)
  each <- kits_guess_per_file(dfs)
  vapply(stats::setNames(nm = KITS_MAP_ROWS$field), function(field) {
    row <- KITS_MAP_ROWS[KITS_MAP_ROWS$field == field, ]
    g <- common[[field]]
    in_all <- nzchar(g) && all(vapply(dfs, function(df) g %in% names(df), logical(1)))
    if (in_all) return("column")
    if (row$per_file && length(dfs) > 1 && all(nzchar(each[[field]]))) return("per_file")
    if (nzchar(g) || row$required || field == "subj_id") "column" else "none"
  }, character(1))
}

#' 第 i 個檔案的對應欄位：各檔各選一欄的換成這個檔案選的欄
#' @noRd
kits_file_map <- function(map, per_file, i) {
  for (field in names(per_file)) map[[field]] <- per_file[[field]][i] %||% ""
  map
}


# ── 領域：題目表 ─────────────────────────────────────────────────────────

#' 題目表（item_id、domain，可以再有 item_id_out）整理與檢查
#'
#' item_id 是資料裡的欄名；item_id_out 是作答檔要用的題號（跨波次同一題給同一個題號，
#' 空白時就用欄名）。有 item_id_out 時 domain 可以省略。
#' 欄名不分大小寫；同一題有兩列不同的設定時停下來。
#' @return tibble：item_id、domain、item_id_out（文字，去掉前後空白；空白是 NA）
#' @noRd
kits_item_table <- function(df) {
  names(df) <- tolower(trimws(names(df)))
  has_out <- "item_id_out" %in% names(df)
  miss <- setdiff(c("item_id", if (!has_out) "domain"), names(df))
  if (length(miss) > 0) {
    kit_abort(
      "題目表需要 item_id 與 domain 兩欄，缺少：", paste(miss, collapse = "、"), "。",
      "請用一列一題的表，例如 item_id = lana01、domain = lan",
      "（要改輸出的題號時，再加一欄 item_id_out）。"
    )
  }
  text <- function(x) {
    if (is.null(x)) return(rep(NA_character_, nrow(df)))
    dplyr::na_if(trimws(as.character(unclass(x))), "")
  }
  tab <- tibble::tibble(
    item_id = text(df$item_id), domain = text(df[["domain"]]), item_id_out = text(df[["item_id_out"]])
  ) |>
    dplyr::filter(!is.na(.data$item_id)) |>
    dplyr::distinct()
  dup <- unique(tab$item_id[duplicated(tab$item_id)])
  if (length(dup) > 0) {
    kit_abort(
      "題目表裡同一題有不同的領域或輸出題號：", paste(utils::head(dup, 5), collapse = "、"),
      "。每一題只能有一列。"
    )
  }
  tab
}

# 反向題的欄名規則：反向題查不到領域時，改查去掉 r 的原題（KIT 是結尾 r，例如 cogd01r）
KITS_REVERSE_RULES <- c(
  "結尾 r（例如 cogd01r → cogd01）" = "suffix",
  "開頭 r（例如 ra8 → a8、w1ra8 → w1a8）" = "prefix",
  "不用（反向題的領域要自己給）" = "none"
)

#' 反向題的原題名稱：照規則去掉 r；不是反向題（或規則是 none）時 NA
#' @param rule "suffix"（結尾 r）、"prefix"（開頭 r，可以在波次前綴 w1 之後）或 "none"
#' @noRd
kits_reverse_base <- function(cols, rule = "suffix") {
  out <- rep(NA_character_, length(cols))
  if (identical(rule, "suffix")) {
    hit <- grepl("r$", cols)
    out[hit] <- sub("r$", "", cols[hit])
  } else if (identical(rule, "prefix")) {
    pattern <- "^([A-Za-z]*[0-9]+_?)?r(?=[A-Za-z0-9])"
    hit <- grepl(pattern, cols, perl = TRUE)
    out[hit] <- sub(pattern, "\\1", cols[hit], perl = TRUE)
  }
  out[!is.na(out) & !nzchar(out)] <- NA_character_
  out
}

#' 每一欄的領域：查題目表（反向題查不到時改查原題，規則見 KITS_REVERSE_RULES）
#'
#' @param cols 候選欄
#' @param table NULL 或 kits_item_table()
#' @param reverse 反向題的欄名規則（"suffix"、"prefix"、"none"）
#' @return tibble：column、domain（找不到時 NA）、source（給使用者看的來源）
#' @noRd
kits_item_domains <- function(cols, table = NULL, reverse = "suffix") {
  cols <- as.character(cols)
  domain <- rep(NA_character_, length(cols))
  source <- rep(NA_character_, length(cols))
  if (!is.null(table) && nrow(table) > 0) {
    hit <- match(cols, table$item_id)
    domain <- table$domain[hit]
    source[!is.na(domain)] <- "題目表"
    base <- kits_reverse_base(cols, reverse)
    use_base <- is.na(domain) & !is.na(base) & base %in% table$item_id
    domain[use_base] <- table$domain[match(base[use_base], table$item_id)]
    source[use_base] <- paste0("題目表（原題 ", base[use_base], "）")
  }
  tibble::tibble(column = cols, domain = domain, source = source)
}

#' 候選題目欄：各檔欄位聯集，去掉已對應的欄與世代欄
#' @return list(numeric = 數值欄, text = 文字欄（不能當題目）)
#' @noRd
kits_candidate_cols <- function(dfs, mapped) {
  cols <- setdiff(as.character(unique(unlist(purrr::map(dfs, names)))), mapped)
  is_num <- vapply(cols, function(col) {
    all(purrr::map_lgl(dfs, function(df) {
      !col %in% names(df) || is.numeric(df[[col]]) || all(is.na(df[[col]]))
    }))
  }, logical(1))
  list(numeric = cols[is_num], text = cols[!is_num])
}


# ── 作答分數 ─────────────────────────────────────────────────────────────────

#' 每一題觀察到的最小、最大分數（所有檔案合起來）
#' @param below 只看小於這個值的分數（例如 7777：先不管疑似遺漏值代碼）
#' @return tibble：item_id、min、max、n（有作答的格數）
#' @noRd
kits_item_ranges <- function(dfs, items, below = Inf) {
  purrr::map(dfs, function(df) {
    have <- intersect(items, names(df))
    purrr::map(have, function(col) {
      x <- as.numeric(unclass(df[[col]]))
      x <- x[!is.na(x) & x < below]
      tibble::tibble(item_id = col, min = suppressWarnings(min(x)),
                     max = suppressWarnings(max(x)), n = length(x))
    }) |>
      purrr::list_rbind()
  }) |>
    purrr::list_rbind() |>
    dplyr::summarise(
      min = suppressWarnings(min(.data$min)), max = suppressWarnings(max(.data$max)),
      n = sum(.data$n), .by = "item_id"
    )
}

# ── 遺漏值代碼 ───────────────────────────────────────────────────────────────
#
# ⑥ 不把遺漏值代碼當成分數：下面這幾種值當成代碼，轉換時改成 NA（不寫進作答檔）。
#   1. 值標籤是「遺漏值」「跳答」「不適用」「不知道」「拒答」這類（KITS_CODE_LABEL_PATTERN）
#   2. ② 的代碼清單（「② 遺漏值轉 NA」上方填的代碼；沒有用 ② 時也會看）
#   3. 7777 以上（KIT 的代碼）
#   4. 和分數明顯分開的值：例如分數是 1–5，又有 95–99（kits_separated_codes()）
# 每一題的代碼列在「3. 題目設定」，可以改（「當成遺漏值」「不是遺漏值代碼」）。

#' 一欄的數值（文字欄能讀成數字的也算；帶標籤的欄用代碼）
#' @noRd
kits_as_num <- function(x) {
  if (is.character(x)) return(suppressWarnings(as.numeric(trimws(x))))
  if (is.factor(x)) return(rep(NA_real_, length(x)))
  as.numeric(unclass(x))
}

#' 各檔案這一欄的值標籤（值 → 標籤）；沒有時 NULL
#' @noRd
kits_value_labels <- function(dfs, col) {
  labs <- unlist(purrr::map(unname(dfs), function(df) {
    labels <- attr(df[[col]], "labels", exact = TRUE)
    if (is.numeric(labels)) stats::setNames(names(labels), as.character(unclass(labels)))
  }))
  if (length(labs) == 0) NULL else labs[!duplicated(names(labs))]
}

#' 和分數明顯分開的值：例如 1–5 加上 95–99、0–3 加上 999
#'
#' 由大到小找最大的空隙：空隙上面的值都是整數、不超過 5 種、都在 90 以上、至少是下面
#' 最大值的 4 倍，下面至少有 2 種值時，上面的值就是代碼。可以重複找（95–99 加上 999）。
#' @param vals 這一欄的值（不含 NA）
#' @return 代碼（數字向量）
#' @noRd
kits_separated_codes <- function(vals) {
  vals <- sort(unique(vals[is.finite(vals)]))
  out <- numeric(0)
  for (k in 1:3) {
    n <- length(vals)
    if (n < 3) break
    gaps <- diff(vals)
    cut <- which.max(gaps)
    body <- vals[seq_len(cut)]
    upper <- vals[-seq_len(cut)]
    ok <- length(body) >= 2 && length(upper) <= 5 && all(upper == round(upper)) &&
      min(upper) >= 90 && min(upper) >= 4 * max(max(body), 1)
    if (!ok) break
    out <- c(upper, out)
    vals <- body
  }
  sort(out)
}

#' 一欄（所有檔案合起來）的遺漏值代碼與原因
#'
#' @param vals 觀察到的值（不含 NA）
#' @param labels kits_value_labels()
#' @param na_codes ② 的代碼清單
#' @return tibble：value、why（例如「跳答」「② 的代碼」「7777 以上」「和分數分開」）
#' @noRd
kits_value_codes <- function(vals, labels = NULL, na_codes = NULL) {
  vals <- as.numeric(vals)
  vals <- sort(unique(vals[is.finite(vals)]))
  why <- rep(NA_character_, length(vals))
  if (length(labels) > 0) {
    lab <- labels[match(as.character(vals), names(labels))]
    hit <- !is.na(lab) & grepl(KITS_CODE_LABEL_PATTERN, lab, ignore.case = TRUE)
    why[hit] <- paste0("值標籤「", lab[hit], "」")
  }
  why[is.na(why) & vals %in% na_codes] <- "② 的代碼"
  why[is.na(why) & vals >= KITS_CODE_MIN] <- paste0(KITS_CODE_MIN, " 以上")
  sep <- kits_separated_codes(vals[is.na(why)])
  why[is.na(why) & vals %in% sep] <- "和分數分開"
  keep <- !is.na(why)
  tibble::tibble(value = vals[keep], why = why[keep])
}

#' 代碼的文字：「98（值標籤「跳答」）、99（② 的代碼）」
#' @noRd
kits_codes_text <- function(codes) {
  if (is.null(codes) || nrow(codes) == 0) return("")
  paste0(kits_id_text(codes$value), "（", codes$why, "）", collapse = "、")
}

#' 這幾欄裡是遺漏值代碼的格數（wave_m、age_m 這類不是題目的欄；不看「和分數分開」）
#' @noRd
kits_code_cells <- function(dfs, cols, na_codes = NULL) {
  sum(purrr::map_dbl(cols, function(col) {
    vals <- unlist(purrr::map(dfs, function(df) {
      if (col %in% names(df) && is.numeric(df[[col]])) kits_as_num(df[[col]])
    }))
    if (length(vals) == 0) return(0)
    codes <- kits_value_codes(vals[!is.na(vals)], kits_value_labels(dfs, col), na_codes)
    codes <- codes$value[codes$why != "和分數分開"]
    sum(vals %in% codes)
  }))
}

#' 平移量的寫法：−1、+2
#' @noRd
kits_shift_text <- function(by) {
  if (by < 0) paste0("−", abs(by)) else paste0("+", by)
}


# ── 題目設定：每一題納入與否、領域、計分對照（原分數 → 計分） ───────────────
#
# 設定是一張 tibble，一列一個候選欄：
#   column（題號，就是欄名）、include（納入）、domain（領域，NA 是還沒給）、
#   source（領域從哪裡來）、raw（觀察到的原分數，list 欄）、score（對應的計分，list 欄；
#   NA 是還沒有計分）、has_codes（有沒有遺漏值代碼）、codes（遺漏值代碼與原因，list 欄，
#   每格是 tibble(value, why)）、n_codes（代碼的格數）、code_set（代碼是誰決定的：NA 是
#   自動偵測，「自己設」「設定檔」時資料變了也照用）。
# 原分數不含 NA 與遺漏值代碼（轉換時代碼改成 NA）。
# 下面的批次動作都只改選取的列（rows 是題號），而且都在「目前的計分」上做。

# 空的題目設定
kits_settings_empty <- function() {
  tibble::tibble(
    column = character(0), include = logical(0), domain = character(0),
    source = character(0), raw = list(), score = list(), has_codes = logical(0),
    codes = list(), n_codes = integer(0), code_set = character(0),
    item_id_out = character(0)
  )
}

# 沒有代碼
kits_no_codes <- function() tibble::tibble(value = numeric(0), why = character(0))

#' 由資料建立題目設定：每一欄納入、沒有領域、原分數 = 計分；遺漏值代碼自動偵測
#' @param cols 候選欄（kits_candidate_cols()$numeric）
#' @param na_codes ② 的代碼清單（NULL 時只看值標籤、7777 以上、和分數分開的值）
#' @noRd
kits_item_settings <- function(dfs, cols, na_codes = NULL) {
  if (length(cols) == 0) return(kits_settings_empty())
  rows <- purrr::map(cols, function(col) {
    all_vals <- unlist(purrr::map(dfs, function(df) {
      x <- df[[col]]
      if (is.null(x) || !(is.numeric(x) || is.character(x))) NULL else kits_as_num(x)
    }))
    all_vals <- all_vals[!is.na(all_vals)]
    vals <- unique(all_vals)
    found <- kits_value_codes(vals, kits_value_labels(dfs, col), na_codes)
    observed <- sort(vals[!vals %in% found$value])
    # 注意：tibble() 依序建立欄位，不能用和欄名相同的變數名（raw、codes）
    tibble::tibble(
      column = col, include = TRUE, domain = NA_character_, source = NA_character_,
      raw = list(observed), score = list(observed), has_codes = nrow(found) > 0,
      codes = list(found), n_codes = sum(all_vals %in% found$value),
      code_set = NA_character_, item_id_out = NA_character_
    )
  })
  purrr::list_rbind(rows)
}

#' 改一列的遺漏值代碼：原分數與代碼重新分，計分對照沿用（kits_map_fit()）
#' @param i 第幾列
#' @param codes tibble(value, why)
#' @param set 誰決定的（「自己設」「設定檔」）
#' @noRd
kits_set_codes <- function(settings, i, codes, set) {
  old <- settings$codes[[i]] %||% kits_no_codes()
  seen <- sort(union(settings$raw[[i]], old$value))
  raw <- seen[!seen %in% codes$value]
  settings$score[[i]] <- kits_map_fit(raw, settings$raw[[i]], settings$score[[i]])
  settings$raw[[i]] <- raw
  settings$codes[[i]] <- codes
  settings$has_codes[i] <- nrow(codes) > 0
  settings$code_set[i] <- set
  settings
}

# 舊的計分對照套到新的原分數：對得到的照舊；對不到的，舊的對照沒改過（原分數 = 計分）
# 時也用原分數，否則是 NA（要使用者決定）
kits_map_fit <- function(raw_new, raw_old, score_old) {
  score <- score_old[match(raw_new, raw_old)]
  identity <- length(raw_old) == 0 || isTRUE(all(score_old == raw_old))
  miss <- is.na(match(raw_new, raw_old))
  if (identity) score[miss] <- raw_new[miss]
  score
}

#' 資料或候選欄變了：新的設定沿用舊設定（納入、領域、計分對照）
#'
#' @param old 舊的設定（NULL 時直接用 new）
#' @param new kits_item_settings() 由目前資料建立的設定
#' @param upload NULL 或 kits_settings_import()：新出現的欄套用上傳的題目表或設定檔
#' @param reverse 反向題的欄名規則（kits_item_domains()）
#' @noRd
kits_settings_sync <- function(old, new, upload = NULL, reverse = "suffix") {
  if (nrow(new) == 0) return(new)
  if (!is.null(old) && nrow(old) > 0) {
    hit <- match(new$column, old$column)
    for (i in which(!is.na(hit))) {
      j <- hit[i]
      new$include[i] <- old$include[j]
      new$domain[i] <- old$domain[j]
      new$source[i] <- old$source[j]
      new$item_id_out[i] <- old[["item_id_out"]][j] %||% NA_character_
      if (!is.na(old$code_set[j] %||% NA)) {
        # 使用者決定的代碼：資料變了也照用
        new <- kits_set_codes(new, i, old$codes[[j]], old$code_set[j])
      }
      new$score[[i]] <- kits_map_fit(new$raw[[i]], old$raw[[j]], old$score[[j]])
    }
    fresh <- new$column[is.na(hit)]
  } else {
    fresh <- new$column
  }
  if (!is.null(upload)) new <- kits_settings_apply(new, upload, fresh, reverse)
  new
}

#' 套用上傳的題目表（item_id、domain）或題目設定檔（item_id、include、domain、
#' map_raw_to_score）
#'
#' 題目表只給領域（和輸出題號）；反向題查不到時用原題的領域（規則見 KITS_REVERSE_RULES）。
#' 設定檔還給納入、計分對照與遺漏值代碼。
#' @param cols 只套用這些題（NULL 是全部）
#' @param reverse 反向題的欄名規則（kits_item_domains()）
#' @noRd
kits_settings_apply <- function(settings, upload, cols = NULL, reverse = "suffix") {
  if (is.null(upload) || nrow(settings) == 0) return(settings)
  todo <- if (is.null(cols)) rep(TRUE, nrow(settings)) else settings$column %in% cols
  dom <- kits_item_domains(settings$column, upload$data[c("item_id", "domain")], reverse)
  set <- todo & !is.na(dom$domain)
  settings$domain[set] <- dom$domain[set]
  settings$source[set] <- if (upload$type == "settings") "設定檔" else dom$source[set]
  # 輸出題號：只對欄名完全相同的列（不用反向題的規則）
  if (!is.null(upload$data[["item_id_out"]]) && any(!is.na(upload$data[["item_id_out"]]))) {
    if (is.null(settings[["item_id_out"]])) settings$item_id_out <- NA_character_
    out <- upload$data[["item_id_out"]][match(settings$column, upload$data$item_id)]
    set <- todo & !is.na(out)
    settings$item_id_out[set] <- ifelse(out[set] == settings$column[set], NA, out[set])
  }
  if (upload$type == "settings") {
    hit <- match(settings$column, upload$data$item_id)
    for (i in which(todo & !is.na(hit))) {
      j <- hit[i]
      if (!is.na(upload$data$include[j])) settings$include[i] <- upload$data$include[j]
      codes <- upload$data[["codes"]][[j]]
      if (!is.null(codes)) {
        settings <- kits_set_codes(
          settings, i, tibble::tibble(value = codes, why = rep("設定檔", length(codes))), "設定檔"
        )
      }
      settings$score[[i]] <- kits_map_fit(
        settings$raw[[i]], upload$data$raw[[j]], upload$data$score[[j]]
      )
    }
  }
  settings
}

#' 讀進來的題目表或題目設定檔
#'
#' 有 map_raw_to_score 或 include 欄的是設定檔（⑥ 下載的）；只有 item_id、domain 的是題目表。
#' @return list(type = "table" 或 "settings", data)
#' @noRd
kits_settings_import <- function(df) {
  names(df) <- tolower(trimws(names(df)))
  if (!any(c("map_raw_to_score", "include") %in% names(df))) {
    return(list(type = "table", data = kits_item_table(df)))
  }
  if (!"item_id" %in% names(df)) kit_abort("題目設定檔需要 item_id 欄。")
  text <- function(x) if (is.null(x)) NA_character_ else trimws(as.character(unclass(x)))
  ids <- text(df$item_id)
  include <- text(df$include)
  maps <- purrr::map2(ids, text(df$map_raw_to_score), function(id, m) {
    tryCatch(kits_parse_map(m), error = function(e) {
      kit_abort("題目設定檔裡 ", id, " 的 map_raw_to_score 看不懂：", conditionMessage(e))
    })
  })
  # missing_codes：遺漏值代碼（例如 98;99）。沒有這一欄（舊的設定檔）時沿用自動偵測
  codes <- if (is.null(df[["missing_codes"]])) rep(list(NULL), length(ids)) else {
    purrr::map(text(df$missing_codes), function(m) {
      if (is.na(m) || !nzchar(m)) return(numeric(0))
      parts <- strsplit(m, "[;,，、[:space:]]+")[[1]]
      vals <- suppressWarnings(as.numeric(parts[nzchar(parts)]))
      if (anyNA(vals)) kit_abort("題目設定檔的 missing_codes 要寫數字，用分號隔開，例如 98;99。")
      vals
    })
  }
  data <- tibble::tibble(
    item_id = ids,
    include = dplyr::case_when(
      toupper(include) %in% c("TRUE", "T", "1", "是", "YES") ~ TRUE,
      toupper(include) %in% c("FALSE", "F", "0", "否", "NO") ~ FALSE,
      .default = NA
    ),
    domain = dplyr::na_if(text(df$domain), ""),
    raw = purrr::map(maps, "raw"), score = purrr::map(maps, "score"), codes = codes,
    item_id_out = if (is.null(df[["item_id_out"]])) NA_character_ else
      dplyr::na_if(text(df[["item_id_out"]]), "")
  )
  data <- data[!is.na(data$item_id) & nzchar(data$item_id), ]
  list(type = "settings", data = data)
}

#' 計分對照的文字：「1=0;2=1」（原分數=計分，用分號隔開；還沒有計分的寫「1=」）
#' @noRd
kits_map_text <- function(raw, score) {
  shown <- ifelse(is.na(score), "", format(score, trim = TRUE, drop0trailing = TRUE))
  paste0(format(raw, trim = TRUE, drop0trailing = TRUE), "=", shown, collapse = ";")
}

#' 解析計分對照的文字（kits_map_text() 的反向）
#' @return list(raw, score)
#' @noRd
kits_parse_map <- function(text) {
  if (is.na(text) || !nzchar(trimws(text))) return(list(raw = numeric(0), score = numeric(0)))
  pairs <- strsplit(strsplit(trimws(text), ";", fixed = TRUE)[[1]], "=", fixed = TRUE)
  raw <- suppressWarnings(as.numeric(vapply(pairs, `[`, character(1), 1)))
  score <- suppressWarnings(as.numeric(vapply(pairs, function(p) {
    if (length(p) < 2) NA_character_ else p[2]
  }, character(1))))
  if (anyNA(raw)) stop("要寫成「原分數=計分;原分數=計分」，例如 1=0;2=1")
  list(raw = raw, score = score)
}

#' 下載用的題目設定：item_id、include、domain、map_raw_to_score（原分數=計分）、
#' missing_codes（遺漏值代碼，用分號隔開）、item_id_out（輸出的題號；空白是用欄名）
#' @noRd
kits_settings_export <- function(settings) {
  codes <- settings[["codes"]] %||% rep(list(kits_no_codes()), nrow(settings))
  tibble::tibble(
    item_id = settings$column,
    include = settings$include,
    domain = settings$domain,
    map_raw_to_score = purrr::map2_chr(settings$raw, settings$score, kits_map_text),
    missing_codes = purrr::map_chr(codes, function(x) {
      paste(kits_id_text((x %||% kits_no_codes())$value), collapse = ";")
    }),
    item_id_out = settings[["item_id_out"]] %||% rep(NA_character_, nrow(settings))
  )
}

#' 解析「合併」的原分數：「4,5」「4、5」「4 5」
#' @noRd
kits_parse_values <- function(text) {
  parts <- strsplit(trimws(text %||% ""), "[,，、;；[:space:]]+")[[1]]
  vals <- suppressWarnings(as.numeric(parts[nzchar(parts)]))
  vals[!is.na(vals)]
}

# 批次動作：rows 是要改的題號 ----

#' 設領域
#' @noRd
kits_bulk_domain <- function(settings, rows, domain) {
  hit <- settings$column %in% rows
  settings$domain[hit] <- domain
  settings$source[hit] <- "自己設"
  settings
}

#' 納入或排除
#' @noRd
kits_bulk_include <- function(settings, rows, include) {
  settings$include[settings$column %in% rows] <- include
  settings
}

# 改選取列的計分：fun(raw, score) 回傳新的計分
kits_bulk_score <- function(settings, rows, fun) {
  for (i in which(settings$column %in% rows)) {
    settings$score[[i]] <- fun(settings$raw[[i]], settings$score[[i]])
  }
  settings
}

#' 平移：目前的計分加上 by（例如 −1）
#' @noRd
kits_bulk_shift <- function(settings, rows, by) {
  kits_bulk_score(settings, rows, function(raw, score) score + by)
}

#' 反向：最高分 − 目前的計分；max 是 NA 時用這一題目前的最高計分
#' @noRd
kits_bulk_reverse <- function(settings, rows, max = NA) {
  kits_bulk_score(settings, rows, function(raw, score) {
    top <- if (is.na(max)) suppressWarnings(base::max(score, na.rm = TRUE)) else max
    if (!is.finite(top)) return(score)
    top - score
  })
}

#' 合併：原分數 from（可以好幾個）→ 計分 to
#' @noRd
kits_bulk_merge <- function(settings, rows, from, to) {
  kits_bulk_score(settings, rows, function(raw, score) {
    score[raw %in% from] <- to
    score
  })
}

#' 還原：計分 = 原分數
#' @noRd
kits_bulk_restore <- function(settings, rows) {
  kits_bulk_score(settings, rows, function(raw, score) raw)
}

#' 當成遺漏值：這些原分數改成遺漏值代碼（不輸出）
#' @noRd
kits_bulk_codes <- function(settings, rows, values) {
  for (i in which(settings$column %in% rows)) {
    old <- settings$codes[[i]] %||% kits_no_codes()
    add <- setdiff(values, old$value)
    codes <- dplyr::bind_rows(old, tibble::tibble(value = add, why = rep("自己設", length(add))))
    settings <- kits_set_codes(settings, i, codes[order(codes$value), ], "自己設")
  }
  settings
}

#' 輸出題號：欄名去掉符合 pattern（正規式）的部分，例如 ^w[0-9]+ 讓 w1d5_1、w2d5_1 都是 d5_1
#' @noRd
kits_bulk_out_prefix <- function(settings, rows, pattern) {
  ok <- tryCatch({
    grepl(pattern, "x")
    TRUE
  }, error = function(e) FALSE, warning = function(w) FALSE)
  if (!ok || !nzchar(pattern)) kit_abort("看不懂這個規則（正規式）：", pattern, "。例如 ^w[0-9]+")
  hit <- settings$column %in% rows
  out <- sub(pattern, "", settings$column[hit])
  empty <- settings$column[hit][!nzchar(out)]
  if (length(empty) > 0) {
    kit_abort("去掉後題號會變成空白：", paste(utils::head(empty, 5), collapse = "、"), "。")
  }
  settings$item_id_out[hit] <- ifelse(out == settings$column[hit], NA, out)
  settings
}

#' 輸出題號還原成欄名
#' @noRd
kits_bulk_out_reset <- function(settings, rows) {
  settings$item_id_out[settings$column %in% rows] <- NA_character_
  settings
}

#' 不是遺漏值代碼：這些題的代碼都放回原分數（計分 = 原分數）
#' @noRd
kits_bulk_uncode <- function(settings, rows) {
  for (i in which(settings$column %in% rows)) {
    settings <- kits_set_codes(settings, i, kits_no_codes(), "自己設")
  }
  settings
}

#' 每一列的提醒（給畫面上的「提醒」欄）
#' @return 字元向量（每列一個，沒有提醒時是 ""）
#' @noRd
kits_settings_alerts <- function(settings) {
  codes <- settings[["codes"]] %||% rep(list(NULL), nrow(settings))
  purrr::pmap_chr(
    list(settings$include, settings$domain, settings$raw, settings$score, settings$has_codes,
         codes),
    function(include, domain, raw, score, has_codes, codes) {
      if (!include) return("")
      out <- character(0)
      if (is.na(domain) || !nzchar(domain)) out <- c(out, "沒有領域")
      if (anyNA(score)) {
        out <- c(out, paste0("原分數 ", paste(raw[is.na(score)], collapse = "、"), " 沒有計分"))
      }
      sc <- score[!is.na(score)]
      if (length(sc) > 0) {
        if (any(sc < 0)) out <- c(out, "有負分")
        if (any(sc != round(sc))) out <- c(out, "計分不是整數")
        if (min(sc) > 0) out <- c(out, "沒有 0 分")
        if (all(sc == round(sc)) && !all(seq(min(sc), max(sc)) %in% sc)) {
          like <- kits_code_like(sc)
          out <- c(out, if (length(like) > 0) {
            paste0("分數有缺（", paste(kits_id_text(like), collapse = "、"),
                   " 像遺漏值代碼，請用「當成遺漏值」，不要合併）")
          } else {
            "分數有缺"
          })
        }
      }
      if (any(raw != round(raw))) out <- c(out, "原分數有小數（可能不是題目）")
      if (!is.null(codes) && nrow(codes) > 0) {
        out <- c(out, paste0("遺漏值代碼 ", kits_codes_text(codes), "：不輸出"))
      } else if (is.null(codes) && has_codes) {
        out <- c(out, "有遺漏值代碼")
      }
      paste(out, collapse = "；")
    }
  )
}

#' 分數空隙上面、看起來像遺漏值代碼的值（9、99、95–98、999…）：提醒不要「合併」
#' @param sc 一題的分數（或計分）
#' @noRd
kits_code_like <- function(sc) {
  sc <- sort(unique(sc[is.finite(sc)]))
  if (length(sc) < 2) return(numeric(0))
  cut <- which.max(diff(sc))
  upper <- sc[-seq_len(cut)]
  upper[grepl("^9+[0-9]?$", kits_id_text(upper)) & upper >= 9]
}

#' 要轉的題目（納入的列）：column、domain、raw、score、codes（遺漏值代碼，list 欄）、
#' item_id_out（輸出的題號，NA 是用欄名）
#' @noRd
kits_settings_items <- function(settings) {
  keep <- settings[settings$include, ]
  if (is.null(keep[["codes"]])) keep$codes <- rep(list(kits_no_codes()), nrow(keep))
  if (is.null(keep[["item_id_out"]])) keep$item_id_out <- rep(NA_character_, nrow(keep))
  keep[c("column", "domain", "raw", "score", "codes", "item_id_out")]
}

#' 納入但還沒有領域的題
#' @noRd
kits_settings_undecided <- function(settings) {
  keep <- settings[settings$include, ]
  keep$column[is.na(keep$domain) | !nzchar(keep$domain)]
}

#' 建議：每一個納入的題，最低分都是同一個 m > 0 時回傳 m，否則 NULL
#'
#' 看目前的計分（一開始等於原分數）；按了建議平移之後最低分變 0，建議就不再出現。
#' @noRd
kits_settings_shift_hint <- function(settings) {
  keep <- settings[settings$include, ]
  mins <- purrr::map_dbl(keep$score, function(r) {
    r <- r[!is.na(r)]
    if (length(r) == 0) NA_real_ else min(r)
  })
  mins <- mins[!is.na(mins)]
  if (length(mins) == 0 || length(unique(mins)) != 1 || mins[1] <= 0) return(NULL)
  mins[1]
}

#' 把每一題的計分對照套到寬資料上（只動有改的題）
#'
#' 資料裡出現、但對照裡沒有計分的原分數（例如新的一波多了 0 分）會擋下來。
#' 遺漏值代碼已經先由 kits_apply_codes() 改成 NA。
#' @param items kits_settings_items()（沒有 raw、score 欄時什麼都不做）
#' @return list(dfs, n = 改了幾題, problems)
#' @noRd
kits_apply_maps <- function(dfs, items) {
  if (!all(c("raw", "score") %in% names(items))) return(list(dfs = dfs, n = 0, problems = NULL))
  problems <- character(0)
  n <- 0
  for (i in seq_len(nrow(items))) {
    col <- items$column[i]
    raw <- items$raw[[i]]
    score <- items$score[[i]]
    seen <- unique(unlist(purrr::map(dfs, function(df) {
      x <- df[[col]]
      if (is.null(x)) NULL else unique(as.numeric(unclass(x)))
    })))
    seen <- seen[!is.na(seen)]
    known <- raw[!is.na(score)]
    miss <- setdiff(seen, known)
    if (length(miss) > 0) {
      problems <- c(problems, paste0(
        col, "：原分數 ", paste(sort(miss), collapse = "、"), " 沒有計分。",
        "請在「3. 題目設定」用「合併」或「還原」給它計分。"
      ))
      next
    }
    if (isTRUE(all(score == raw))) next
    n <- n + 1
    dfs <- purrr::map(dfs, function(df) {
      if (col %in% names(df)) df[[col]] <- kits_apply_score_map(df[[col]], raw, score)
      df
    })
  }
  list(dfs = dfs, n = n, problems = problems)
}

#' 一欄的原分數 → 計分（對照裡沒有的值照原樣）
#' @noRd
kits_apply_score_map <- function(x, raw, score) {
  x <- as.numeric(unclass(x))
  m <- match(x, raw)
  hit <- !is.na(m)
  x[hit] <- score[m[hit]]
  x
}


# ── 設定的檢查（執行前） ─────────────────────────────────────────────────────

#' 執行前就能看出來的問題（有問題就不轉換）
#'
#' @param spec kits_spec()
#' @return 中文訊息；沒問題時長度 0
#' @noRd
kits_spec_problems <- function(dfs, file_names, spec) {
  out <- character(0)
  if (length(dfs) == 0) return("請先到「① 上傳檔案」加入檔案。")
  for (i in seq_along(dfs)) {
    co <- spec$cohort[[i]]
    if (is.null(co) || !nzchar(co$value %||% "")) {
      out <- c(out, paste0(file_names[i], "：還沒有填世代（例如 M03）。"))
    } else if (identical(co$type, "column") && !co$value %in% names(dfs[[i]])) {
      out <- c(out, paste0(file_names[i], "：沒有世代欄 ", co$value, "。"))
    }
  }
  map <- spec$map
  per_file <- spec$per_file %||% list()
  has_wave_m <- nzchar(map$wave_m %||% "") || !is.null(spec$typed[["wave_m"]]) ||
    !is.null(per_file[["wave_m"]])
  if (!has_wave_m) {
    out <- c(out, "請對應 wave_m（時間點）：選檔案裡的欄，或每個檔案自己填。kitsUI 一定要這一欄。")
  }
  out <- c(out, kits_typed_problems(file_names, spec$typed %||% list()))
  has_subj <- nzchar(map$subj_id %||% "") || !is.null(per_file[["subj_id"]])
  if (!nzchar(map$visit_id %||% "") && !has_subj) {
    out <- c(out, "請對應 subj_id（孩子編號）：visit_id 要用它組成。")
  }
  out <- c(out, kits_per_file_problems(dfs, file_names, per_file, map))
  for (i in seq_along(dfs)) {
    miss <- setdiff(kits_mapped_cols(map), names(dfs[[i]]))
    if (length(miss) > 0) {
      out <- c(out, paste0(
        file_names[i], "：沒有對應的欄 ", paste(miss, collapse = "、"), "。",
        "各檔欄名不同時，請在「2. 對應欄位」選「各檔各選一欄」。"
      ))
    }
  }
  if (length(spec$undecided) > 0) {
    out <- c(out, paste0(
      "還有 ", length(spec$undecided), " 個納入的題目沒有領域（請設領域或排除）：",
      paste(utils::head(spec$undecided, 8), collapse = "、"),
      if (length(spec$undecided) > 8) "…" else "", "。"
    ))
  }
  if (nrow(spec$items) == 0) {
    out <- c(out, "沒有題目。請在「3. 題目設定」納入題目並給領域。")
  }
  out
}

# ── 轉換 ─────────────────────────────────────────────────────────────────────

#' 轉換的設定
#'
#' @param cohort list（每個檔案一個）：list(type = "text" 或 "column", value)
#' @param map 對應欄位（kits_guess_map() 的形狀）
#' @param items tibble：column、domain，可以再有 raw、score（計分對照，list 欄）；
#'   沒有 raw、score 時原分數 = 計分（kits_settings_items() 的結果）
#' @param undecided 納入但還沒有領域的題
#' @param typed 具名 list：自己填的對應欄位（每個檔案一個值）
#' @param na_codes ② 的代碼清單（items 沒有 codes 欄時用來偵測遺漏值代碼；也用在 wave_m、
#'   age_m）
#' @param per_file 具名 list：各檔各選一欄的對應欄位（欄 → 每個檔案一個欄名）；
#'   這些欄在 map 裡是 ""
#' @noRd
kits_spec <- function(cohort, map, items, undecided = character(0), typed = list(),
                      na_codes = NULL, per_file = list()) {
  list(cohort = cohort, map = map, items = items, undecided = undecided, typed = typed,
       na_codes = na_codes, per_file = per_file)
}

#' 「各檔各選一欄」有沒有問題：選的欄要在那個檔案裡；wave_m 每個檔案都要選；
#' subj_id 沒有 visit_id 欄時每個檔案都要選
#' @noRd
kits_per_file_problems <- function(dfs, file_names, per_file, map) {
  out <- character(0)
  for (field in names(per_file)) {
    row <- KITS_MAP_ROWS[KITS_MAP_ROWS$field == field, ]
    need <- field == "wave_m" || (field == "subj_id" && !nzchar(map$visit_id %||% ""))
    for (i in seq_along(dfs)) {
      col <- per_file[[field]][i] %||% ""
      if (is.na(col) || !nzchar(col)) {
        if (need) {
          out <- c(out, paste0(file_names[i], "：還沒有選", row$label, "（", field, "）的欄。"))
        }
      } else if (!col %in% names(dfs[[i]])) {
        out <- c(out, paste0(file_names[i], "：沒有", row$label, "（", field, "）的欄 ", col, "。"))
      }
    }
  }
  out
}

#' 「自己填」的值有沒有問題：每個檔案都要填；數字的欄（wave_m、age_m）要是數字
#'
#' @param typed 具名 list（對應欄位 → 每個檔案一個值的字元向量）
#' @return 中文訊息；沒問題時長度 0
#' @noRd
kits_typed_problems <- function(file_names, typed) {
  out <- character(0)
  for (field in names(typed)) {
    row <- KITS_MAP_ROWS[KITS_MAP_ROWS$field == field, ]
    vals <- trimws(typed[[field]])
    for (i in seq_along(file_names)) {
      v <- vals[i]
      if (is.na(v) || !nzchar(v)) {
        out <- c(out, paste0(file_names[i], "：還沒有填", row$label, "（", field, "）。"))
      } else if (row$type == "d" && is.na(suppressWarnings(as.numeric(v)))) {
        out <- c(out, paste0(
          file_names[i], "：", row$label, "（", field, "）要填數字（例如 18），現在是「", v, "」。"
        ))
      }
    }
  }
  out
}

#' 「自己填」的值變成每個檔案裡的一欄（每一列同一個值），對應欄位改成這一欄
#'
#' 這樣自己填的值和「檔案裡有一欄、每一列都是這個值」寫出來完全相同。
#' 數字的欄（wave_m、age_m）轉成數字，其他是文字。
#' @return list(dfs, spec)
#' @noRd
kits_apply_typed <- function(dfs, spec) {
  typed <- spec$typed %||% list()
  for (field in names(typed)) {
    col <- paste0(KITS_TYPED_PREFIX, field)
    is_num <- KITS_MAP_ROWS$type[KITS_MAP_ROWS$field == field] == "d"
    dfs <- purrr::map2(dfs, trimws(typed[[field]]), function(df, v) {
      df[[col]] <- rep(if (is_num) suppressWarnings(as.numeric(v)) else v, nrow(df))
      df
    })
    spec$map[[field]] <- col
  }
  list(dfs = dfs, spec = spec)
}

#' 寬格式 → kits 長格式：準備與檢查（不做出整張長表）
#'
#' 計分對照與所有檢查都在寬資料上做（寬資料本來就在記憶體裡，比長格式小很多）；
#' 長格式只在下載時一段一段做出來、寫進 .csv.gz（kits_write_long()），所以幾百萬列也不會
#' 一次佔用大量記憶體。
#'
#' @param dfs 各檔資料
#' @param file_names 檔名（給訊息用）
#' @param spec kits_spec()
#' @param pivot_args NULL 或 mod_pivot_longer_server() 的參數（names_to 等已固定）
#' @return list(prep, problems, warnings, notes, summary, preview, n_rows, n_visits, n_items,
#'   csv_mb, visit_rule)；problems 不是空的時候 prep 是 NULL
#' @noRd
kits_build <- function(dfs, file_names, spec, pivot_args = NULL) {
  prep <- kits_prepare(dfs, file_names, spec, pivot_args)
  if (length(prep$problems) > 0) return(list(prep = NULL, problems = prep$problems))
  checks <- kits_wide_checks(prep, spec)
  ok <- length(checks$problems) == 0
  head_long <- kits_long_head(prep, 2000)
  n_rows <- sum(purrr::map_dbl(prep$files, function(f) sum(f$n_resp)))
  csv_mb <- kits_csv_mb(head_long, n_rows)
  warnings <- c(prep$warnings, checks$warnings)
  if (csv_mb > KITSUI_UPLOAD_MB) {
    warnings <- c(warnings, sprintf(
      "未壓縮的 CSV 約 %.0f MB，超過 kitsUI 單檔上限 %d MB；請下載 .csv.gz（壓縮檔）。",
      csv_mb, KITSUI_UPLOAD_MB
    ))
  }
  list(
    prep = if (ok) prep, problems = checks$problems, warnings = warnings, notes = prep$notes,
    summary = if (ok) kits_summary(prep), preview = utils::head(head_long, KIT_PREVIEW_ROWS * 2),
    n_rows = n_rows, n_visits = checks$n_visits, n_items = checks$n_items,
    csv_mb = csv_mb, visit_rule = prep$visit_rule
  )
}

#' 自己填的值、計分對照與每個檔案的孩子欄、題目欄
#'
#' @return list(files, rename, domain_of, pivot_args, notes, visit_rule, problems)；
#'   files 每個檔案一個 list(persons, scores, have, n_resp)：persons 是每一列的孩子欄
#'   （含 visit_id），scores 是題目欄（數字），n_resp 是每一列有作答的題數
#' @noRd
kits_prepare <- function(dfs, file_names, spec, pivot_args = NULL) {
  problems <- kits_spec_problems(dfs, file_names, spec)
  if (length(problems) > 0) return(list(problems = problems))
  typed <- kits_apply_typed(dfs, spec)
  dfs <- typed$dfs
  spec <- typed$spec
  items <- spec$items$column
  map <- spec$map
  notes <- character(0)
  warnings <- character(0)

  # 遺漏值代碼改成 NA（題目，以及 wave_m、age_m）：不會變成分數
  coded <- kits_apply_codes(dfs, kits_items_with_codes(dfs, spec$items, spec$na_codes))
  dfs <- coded$dfs
  warnings <- c(warnings, coded$warning)
  per_file <- spec$per_file %||% list()
  side <- kits_apply_side_codes(dfs, kits_side_cols(map, per_file), spec$na_codes)
  dfs <- side$dfs
  warnings <- c(warnings, side$warning)

  # 計分對照：原分數 → 計分（每一題自己的對照；沒有改的題不動）
  mapped <- kits_apply_maps(dfs, spec$items)
  if (length(mapped$problems) > 0) return(list(problems = mapped$problems))
  dfs <- mapped$dfs
  if (mapped$n > 0) notes <- c(notes, paste0("計分對照：", mapped$n, " 題的原分數改成計分。"))
  problems <- kits_score_problems(dfs, items)
  if (length(problems) > 0) return(list(problems = problems))
  rename <- kits_item_rename(spec$items)
  clash <- kits_rename_problems(dfs, file_names, spec$items, rename)
  if (length(clash) > 0) return(list(problems = clash))
  warnings <- c(warnings, kits_rename_warning(dfs, spec$items, rename))

  visit_rule <- kits_visit_rule(map, per_file)
  files <- purrr::map(seq_along(dfs), function(i) {
    kits_prepare_file(dfs[[i]], spec$cohort[[i]], kits_file_map(map, per_file, i), items)
  })
  out_ids <- kits_out_ids(items, rename)
  list(
    files = files, rename = rename,
    domain_of = stats::setNames(spec$items$domain, out_ids),
    pivot_args = pivot_args, notes = c(visit_rule$note, notes), visit_rule = visit_rule,
    warnings = warnings, problems = character(0)
  )
}

#' 題目的遺漏值代碼：設定裡有 codes 欄就用它，沒有時（例如自己組的 spec）由資料偵測
#' @noRd
kits_items_with_codes <- function(dfs, items, na_codes = NULL) {
  if (!is.null(items[["codes"]])) return(items)
  items$codes <- purrr::map(items$column, function(col) {
    vals <- unlist(purrr::map(dfs, function(df) {
      x <- df[[col]]
      if (is.null(x) || !(is.numeric(x) || is.character(x))) NULL else kits_as_num(x)
    }))
    kits_value_codes(unique(vals[!is.na(vals)]), kits_value_labels(dfs, col), na_codes)
  })
  items
}

#' 題目裡的遺漏值代碼改成 NA
#' @param items 有 codes 欄（kits_items_with_codes()）
#' @return list(dfs, warning)：warning 列出每一題的代碼（沒有代碼時是 NULL）
#' @noRd
kits_apply_codes <- function(dfs, items) {
  hit <- which(purrr::map_int(items$codes, function(x) nrow(x %||% kits_no_codes())) > 0)
  if (length(hit) == 0) return(list(dfs = dfs, warning = NULL))
  n_cells <- 0
  for (i in hit) {
    col <- items$column[i]
    codes <- items$codes[[i]]$value
    dfs <- purrr::map(dfs, function(df) {
      if (!col %in% names(df)) return(df)
      x <- df[[col]]
      is_code <- kits_as_num(x) %in% codes
      n_cells <<- n_cells + sum(is_code)
      x[is_code] <- NA
      df[[col]] <- x
      df
    })
  }
  shown <- paste0(items$column[hit], "：", purrr::map_chr(items$codes[hit], kits_codes_text))
  list(dfs = dfs, warning = paste0(
    length(hit), " 題有遺漏值代碼，共 ", format(n_cells, big.mark = ","), " 格，",
    "當成沒有作答、不寫進作答檔（",
    paste(utils::head(shown, 5), collapse = "；"), if (length(hit) > 5) "；…", "）。",
    "如果其實是分數，請在「3. 題目設定」選取這些題、按「不是遺漏值代碼」。",
    "其他分頁也要改成 NA 的話，請先到「② 遺漏值轉 NA」處理。"
  ))
}

#' wave_m、age_m 裡的遺漏值代碼（值標籤、② 的代碼、7777 以上；不看「和分數分開」）
#' @param cols 具名 list：wave_m、age_m 對應的欄名
#' @return 具名 list：欄名 → tibble(value, why)（只列有代碼的欄）
#' @noRd
kits_side_codes <- function(dfs, cols, na_codes = NULL) {
  out <- list()
  for (col in unique(unlist(cols))) {
    if (is.null(col) || !nzchar(col)) next
    vals <- unlist(purrr::map(dfs, function(df) {
      if (col %in% names(df) && is.numeric(df[[col]])) unique(kits_as_num(df[[col]]))
    }))
    codes <- kits_value_codes(vals[!is.na(vals)], kits_value_labels(dfs, col), na_codes)
    codes <- codes[codes$why != "和分數分開", ]
    if (nrow(codes) > 0) out[[col]] <- codes
  }
  out
}

#' wave_m、age_m 對應的欄（各檔各選一欄時是每個檔案選的欄）：具名 list，給 kits_side_codes()
#' @noRd
kits_side_cols <- function(map, per_file = list()) {
  purrr::map(c(wave_m = "wave_m", age_m = "age_m"), function(field) {
    unique(c(map[[field]] %||% "", per_file[[field]]))
  })
}

#' wave_m、age_m 裡的遺漏值代碼改成 NA（kits_side_codes()）
#' @noRd
kits_apply_side_codes <- function(dfs, cols, na_codes = NULL) {
  out <- character(0)
  side <- kits_side_codes(dfs, cols, na_codes)
  for (field in names(cols)) for (col in cols[[field]]) {
    codes <- side[[col]]
    if (is.na(col) || !nzchar(col) || is.null(codes)) next
    n <- 0
    dfs <- purrr::map(dfs, function(df) {
      if (!col %in% names(df)) return(df)
      is_code <- kits_as_num(df[[col]]) %in% codes$value
      n <<- n + sum(is_code)
      df[[col]][is_code] <- NA
      df
    })
    out <- c(out, paste0(
      field, "（", col, "）有 ", n, " 格是遺漏值代碼 ", kits_codes_text(codes), "，當成空白。"
    ))
  }
  list(dfs = dfs, warning = if (length(out) > 0) paste(out, collapse = ""))
}

#' 輸出題號的對照：欄名 → 輸出的題號（只列有改的題）
#' @noRd
kits_item_rename <- function(items) {
  out <- items[["item_id_out"]]
  if (is.null(out)) return(character(0))
  keep <- !is.na(out) & nzchar(out) & out != items$column
  stats::setNames(out[keep], items$column[keep])
}

#' 改題號後的問題（擋下）：同一個檔案裡兩欄變成同一個題號、同一個題號有兩個領域
#' @noRd
kits_rename_problems <- function(dfs, file_names, items, rename) {
  if (length(rename) == 0) return(character(0))
  out <- character(0)
  for (i in seq_along(dfs)) {
    have <- intersect(items$column, names(dfs[[i]]))
    ids <- kits_out_ids(have, rename)
    dup <- unique(ids[duplicated(ids)])
    for (d in utils::head(dup, 3)) {
      out <- c(out, paste0(
        file_names[i], "：", paste(have[ids == d], collapse = "、"), " 都輸出成題號 ", d,
        "。同一個檔案裡兩欄不能是同一題（同一個列次會有兩個 ", d, "），請改 item_id_out。"
      ))
    }
  }
  doms <- dplyr::distinct(tibble::tibble(id = kits_out_ids(items$column, rename),
                                         domain = items$domain))
  two <- unique(doms$id[duplicated(doms$id)])
  if (length(two) > 0) {
    out <- c(out, paste0(
      "這些輸出題號對到不同的領域：", paste(utils::head(two, 5), collapse = "、"),
      "。同一個題號只能有一個領域。"
    ))
  }
  out
}

#' 改題號後的提醒：不同的欄（通常是不同波次）合成同一個題號時，列出來請使用者確認
#'
#' 同一個題號在不同波次可能是不同的題（例如 TDCYP W1 的 c21_5 是家扶老師，W3 以後的
#' d32_5 是學校老師），變項標籤不同時特別標出來。
#' @noRd
kits_rename_warning <- function(dfs, items, rename) {
  if (length(rename) == 0) return(NULL)
  ids <- kits_out_ids(items$column, rename)
  groups <- split(items$column, ids)
  groups <- groups[lengths(groups) > 1]
  if (length(groups) == 0) return(NULL)
  label_of <- function(col) {
    for (df in dfs) {
      lab <- attr(df[[col]], "label", exact = TRUE)
      if (!is.null(lab)) return(lab)
    }
    NA_character_
  }
  differ <- names(groups)[vapply(groups, function(cols) {
    labs <- vapply(cols, label_of, character(1))
    labs <- sub("^[A-Za-z]*[0-9]+[:：]\\s*", "", labs[!is.na(labs)])
    length(unique(labs)) > 1
  }, logical(1))]
  shown <- paste0(names(groups), " ← ", vapply(groups, paste, character(1), collapse = "、"))
  paste0(
    length(groups), " 個輸出題號由不同的欄合成（", paste(utils::head(shown, 4), collapse = "；"),
    if (length(groups) > 4) "；…", "）。kits 會把它們當成同一題。",
    "同一個題號在不同波次可能是不同的題（例如改版後題號沿用、內容換了），請對照題目內容確認",
    if (length(differ) > 0) {
      paste0("；這些題的變項標籤不一樣，請特別確認：", paste(utils::head(differ, 5), collapse = "、"))
    },
    "。"
  )
}

# 輸出的題號：有改的題用 item_id_out（kits_item_rename()），其他就是欄名
kits_out_ids <- function(ids, rename) {
  if (length(rename) == 0) return(ids)
  dplyr::coalesce(unname(rename[ids]), ids)
}

# 一個檔案：每一列的孩子欄（含 visit_id）、題目欄、每一列有作答的題數
kits_prepare_file <- function(df, cohort, map, items) {
  n <- nrow(df)
  pick <- function(field) {
    col <- map[[field]] %||% ""
    if (nzchar(col)) df[[col]] else rep(NA, n)
  }
  co <- if (identical(cohort$type, "column")) kits_id_text(df[[cohort$value]]) else
    rep(cohort$value, n)
  persons <- tibble::tibble(
    subj_id = kits_id_text(pick("subj_id")), cohort = co, sex = kits_id_text(pick("sex")),
    wave_m = as.numeric(unclass(pick("wave_m"))), age_m = as.numeric(unclass(pick("age_m")))
  )
  persons$visit_id <- kits_visit_ids(persons, df, map)
  have <- intersect(items, names(df))
  scores <- purrr::map(df[have], function(x) as.numeric(unclass(x)))
  n_resp <- Reduce(`+`, lapply(scores, function(x) !is.na(x)), integer(n))
  list(persons = persons, scores = scores, have = have, n_resp = as.integer(n_resp))
}

#' 一個檔案的幾列寬資料 → 長格式（固定 9 欄）
#'
#' 和整張表一次轉換的結果相同：一樣用 pivot_longer_run()，列的順序是寬資料的列、
#' 每一列裡照題目欄的順序。
#' @param rows 寬資料的列（沒有作答的列會略過）
#' @noRd
kits_long_chunk <- function(prep, i, rows) {
  f <- prep$files[[i]]
  rows <- rows[f$n_resp[rows] > 0]
  if (length(rows) == 0 || length(f$have) == 0) return(NULL)
  wide <- dplyr::bind_cols(
    f$persons[rows, ],
    tibble::as_tibble(lapply(f$scores, function(x) x[rows]))
  )
  args <- utils::modifyList(
    prep$pivot_args %||% list(),
    list(cols = f$have, names_to = "item_id", values_to = "item_score", values_drop_na = TRUE)
  )
  long <- pivot_longer_run(wide, args)
  long$item_id <- kits_out_ids(long$item_id, prep$rename)
  long$domain <- unname(prep$domain_of[long$item_id])
  kits_finish_columns(long)
}

# 每一段的列：1:n 切成每段 chunk_rows 列
kits_chunks <- function(n, chunk_rows) {
  if (n == 0) return(list())
  split(seq_len(n), ceiling(seq_len(n) / chunk_rows))
}

#' 長格式的前 n 列（預覽、估計檔案大小）
#' @noRd
kits_long_head <- function(prep, n) {
  out <- list()
  got <- 0
  for (i in seq_along(prep$files)) {
    for (rows in kits_chunks(length(prep$files[[i]]$n_resp), 200)) {
      part <- kits_long_chunk(prep, i, rows)
      if (is.null(part)) next
      out[[length(out) + 1]] <- part
      got <- got + nrow(part)
      if (got >= n) return(utils::head(dplyr::bind_rows(out), n))
    }
  }
  if (length(out) == 0) return(kits_finish_columns(tibble::tibble()))
  dplyr::bind_rows(out)
}

#' 長格式分段寫進一個連線（第一段寫欄名）
#'
#' @param con 已開啟的連線（gzfile() 或 file()）
#' @param chunk_rows 每段幾列寬資料
#' @param progress NULL 或 function(fraction)
#' @noRd
kits_write_long <- function(prep, con, chunk_rows = 5000, progress = NULL) {
  first <- TRUE
  total <- sum(purrr::map_int(prep$files, function(f) length(f$n_resp)))
  done <- 0
  for (i in seq_along(prep$files)) {
    for (rows in kits_chunks(length(prep$files[[i]]$n_resp), chunk_rows)) {
      part <- kits_long_chunk(prep, i, rows)
      done <- done + length(rows)
      if (!is.null(progress)) progress(done / total)
      if (is.null(part)) next
      readr::write_csv(part, con, na = "", col_names = first)
      first <- FALSE
      rm(part)
      if (isTRUE(getOption("kitjoin.gc_chunks", TRUE))) gc(verbose = FALSE)   # 每段寫完就釋放
    }
  }
  if (first) readr::write_csv(kits_finish_columns(tibble::tibble()), con, na = "")
  invisible(con)
}

#' 作答檔寫成 .csv.gz（UTF-8；遺漏值空白），一段一段寫，不做出整張長表
#'
#' 用 gzfile() 寫，不靠檔名判斷要不要壓縮（下載的暫存檔不一定以 .gz 結尾）。
#' @param dfs,spec 同 kits_build()；也可以直接給 prep（kits_build()$prep）
#' @param chunk_rows 每段幾列寬資料
#' @noRd
write_kits_long_gz <- function(dfs, spec, file, chunk_rows = 5000, prep = NULL,
                               pivot_args = NULL, progress = NULL) {
  prep <- prep %||% kits_prep_or_abort(dfs, spec, pivot_args)
  con <- gzfile(file, open = "wb")
  on.exit(close(con))
  kits_write_long(prep, con, chunk_rows, progress)
  invisible(file)
}

#' 作答檔寫成未壓縮的 CSV（同 write_kits_long_gz()）
#' @noRd
write_kits_long_csv <- function(dfs, spec, file, chunk_rows = 5000, prep = NULL,
                                pivot_args = NULL, progress = NULL) {
  prep <- prep %||% kits_prep_or_abort(dfs, spec, pivot_args)
  con <- file(file, open = "wb")
  on.exit(close(con))
  kits_write_long(prep, con, chunk_rows, progress)
  invisible(file)
}

kits_prep_or_abort <- function(dfs, spec, pivot_args) {
  prep <- kits_prepare(dfs, paste0("檔案 ", seq_along(dfs)), spec, pivot_args)
  if (length(prep$problems) > 0) kit_abort(paste(prep$problems, collapse = "\n"))
  prep
}

#' visit_id 怎麼組：{cohort}_{subj_id}_{波次標籤}；沒有波次標籤時用 wave_m
#' @return list(parts, note)
#' @noRd
kits_visit_rule <- function(map, per_file = list()) {
  if (nzchar(map$visit_id %||% "")) {
    return(list(parts = NULL, note = paste0("visit_id 直接用 ", map$visit_id, " 欄。")))
  }
  if (!is.null(per_file[["wave"]])) {
    return(list(
      parts = c("cohort", "subj_id", "wave"),
      note = "visit_id 由 {cohort}_{subj_id}_{各檔選的波次標籤欄} 組成。"
    ))
  }
  if (nzchar(map$wave %||% "")) {
    shown <- if (startsWith(map$wave, KITS_TYPED_PREFIX)) "自己填的波次標籤" else map$wave
    return(list(
      parts = c("cohort", "subj_id", map$wave),
      note = paste0("visit_id 由 {cohort}_{subj_id}_{", shown, "} 組成。")
    ))
  }
  list(
    parts = c("cohort", "subj_id", "wave_m"),
    note = paste0(
      "沒有對應波次標籤欄，visit_id 改由 {cohort}_{subj_id}_{wave_m} 組成。",
      "如果之前的檔案是用波次標籤（例如 m108g03）組的，請對應波次標籤欄，兩邊才接得起來。"
    )
  )
}

# visit_id：有對應欄時直接用，否則照 kits_visit_rule() 組成（任一部分缺漏時 NA）
kits_visit_ids <- function(persons, df, map) {
  if (nzchar(map$visit_id %||% "")) return(kits_id_text(df[[map$visit_id]]))
  wave <- if (nzchar(map$wave %||% "")) {
    kits_id_text(df[[map$wave]])
  } else {
    kits_id_text(persons$wave_m)
  }
  wave[is.na(persons$wave_m) & !nzchar(map$wave %||% "")] <- NA
  parts <- list(persons$cohort, persons$subj_id, wave)
  out <- do.call(paste, c(parts, sep = "_"))
  bad <- Reduce(`|`, lapply(parts, function(p) is.na(p) | !nzchar(trimws(p))))
  out[bad] <- NA_character_
  out
}

#' 編號、世代、波次標籤轉成文字：數字不寫成科學記號（100000 → "100000"，as.character()
#' 會變成 "1e+05"），有小數的保留 15 位有效數字（0.5 和 0.50000001 不會變成同一個）；NA 保持 NA
#'
#' 「用 R 怎麼做？」裡的 as_id() 和這個函式相同（code_kitsdata_helpers()）。
#' @noRd
kits_id_text <- function(x) {
  x <- as.vector(unclass(x))
  if (!is.numeric(x)) return(as.character(x))
  out <- trimws(formatC(x, format = "fg", digits = 15))
  out[is.na(x)] <- NA_character_
  out
}

# 固定 9 欄、固定順序與型別（KITS_COLUMNS）；沒有的欄補成空白（NA）
kits_finish_columns <- function(long) {
  cast <- list(c = as.character, d = as.numeric, i = function(x) as.integer(round(x)))
  cols <- purrr::map2(KITS_COLUMNS$column, KITS_COLUMNS$type, function(col, type) {
    x <- if (col %in% names(long)) long[[col]] else rep(NA, nrow(long))
    cast[[type]](unclass(x))
  })
  tibble::as_tibble(stats::setNames(cols, KITS_COLUMNS$column))
}

# 分數是數字嗎（文字欄、非整數、負分在轉換前就擋下）
kits_score_problems <- function(dfs, items) {
  out <- character(0)
  bad_type <- unique(unlist(purrr::map(dfs, function(df) {
    have <- intersect(items, names(df))
    have[!vapply(df[have], function(x) is.numeric(x) || all(is.na(x)), logical(1))]
  })))
  if (length(bad_type) > 0) {
    out <- c(out, paste0(
      "這些題目欄不是數字：", paste(utils::head(bad_type, 8), collapse = "、"),
      "。請排除它們，或回原檔確認分數。"
    ))
  }
  ranges <- kits_item_ranges(dfs, setdiff(items, bad_type))
  frac <- unique(unlist(purrr::map(dfs, function(df) {
    have <- intersect(setdiff(items, bad_type), names(df))
    have[vapply(df[have], function(x) {
      x <- as.numeric(unclass(x))
      any(!is.na(x) & x != round(x))
    }, logical(1))]
  })))
  if (length(frac) > 0) {
    out <- c(out, paste0("這些題有不是整數的分數：", paste(utils::head(frac, 8), collapse = "、"), "。"))
  }
  neg <- ranges$item_id[is.finite(ranges$min) & ranges$min < 0]
  if (length(neg) > 0) {
    out <- c(out, paste0(
      "這些題有負分：", paste(utils::head(neg, 8), collapse = "、"),
      "。如果負數（例如 -1）代表遺漏，請先到「② 遺漏值轉 NA」處理；",
      "如果是計分對照造成的，請在「3. 題目設定」修改。"
    ))
  }
  out
}


# ── 檢查（在寬資料上做） ─────────────────────────────────────────────────────

#' 有作答的列的孩子欄（所有檔案接起來）；.n 是這一列有作答的題數
#' @noRd
kits_answered_persons <- function(prep) {
  purrr::map(prep$files, function(f) {
    keep <- f$n_resp > 0
    p <- f$persons[keep, ]
    p$.n <- f$n_resp[keep]
    p
  }) |>
    purrr::list_rbind()
}

#' 每一題（輸出的題號）的作答數、最小、最大、有沒有缺的分數（所有檔案合起來）
#' @noRd
kits_item_stats <- function(prep) {
  cols <- unique(unlist(purrr::map(prep$files, "have")))
  stats <- purrr::map(cols, function(col) {
    vals <- unlist(purrr::map(prep$files, function(f) {
      x <- f$scores[[col]]
      if (is.null(x)) NULL else unique(x[!is.na(x)])
    }))
    n <- sum(purrr::map_int(prep$files, function(f) {
      x <- f$scores[[col]]
      if (is.null(x)) 0L else sum(!is.na(x))
    }))
    vals <- unique(vals)
    if (length(vals) == 0) {
      return(tibble::tibble(column = col, n = 0L, min = NA_real_, max = NA_real_, gap = FALSE,
                            code_like = ""))
    }
    gap <- !all(seq(min(vals), max(vals)) %in% vals)
    tibble::tibble(
      column = col, n = n, min = min(vals), max = max(vals), gap = gap,
      code_like = if (gap) paste(kits_id_text(kits_code_like(vals)), collapse = "、") else ""
    )
  }) |>
    purrr::list_rbind()
  if (nrow(stats) == 0) {
    stats <- tibble::tibble(column = character(0), n = integer(0), min = numeric(0),
                            max = numeric(0), gap = logical(0), code_like = character(0))
  }
  stats$item_id <- kits_out_ids(stats$column, prep$rename)
  stats$domain <- unname(prep$domain_of[stats$item_id])
  stats
}

#' 轉換前的檢查（都在寬資料上做，結果和長格式上檢查相同）
#'
#' problems（紅色，有就不輸出）：必填欄空白、同一個 visit_id 在好幾列（同一列次同一題
#' 會有好幾列）、同一個 visit_id 在兩個世代、同一列次的孩子欄不一致、題目沒有領域。
#' warnings（橘色）：沒有 0 分的題、分數中間有缺、疑似遺漏值代碼、不是錨題的領域、
#' 沒有作答的題、age_m 空白、反向題已以 r 結尾。
#' @return list(problems, warnings, n_visits, n_items)
#' @noRd
kits_wide_checks <- function(prep, spec) {
  problems <- character(0)
  persons <- kits_answered_persons(prep)
  stats <- kits_item_stats(prep)
  answered <- stats[stats$n > 0, ]

  # 必填的欄不能空白：visit_id、cohort、wave_m 看每一列；item_id、item_score 來自題目欄
  blank <- function(x) is.na(x) | (is.character(x) & !nzchar(trimws(x)))
  why <- c(visit_id = "（組成它的世代、孩子編號或波次有缺）", cohort = "（世代欄有空白）")
  for (col in c("visit_id", "cohort", "wave_m")) {
    hit <- blank(persons[[col]])
    if (any(hit)) {
      problems <- c(problems, paste0(
        sum(hit), " 個列次（", format(sum(persons$.n[hit]), big.mark = ","), " 列作答）的 ", col,
        " 是空白", if (col %in% names(why)) why[[col]], "；這一欄是必填。"
      ))
    }
  }
  ok <- persons[!blank(persons$visit_id), ]

  # 同一個 visit_id 在好幾列寬資料 → 同一列次同一題會有好幾列
  twice <- vctrs::vec_duplicate_detect(ok$visit_id)
  if (any(twice)) {
    problems <- c(problems, paste0(
      "同一列次同一題有好幾列：", sum(twice), " 列寬資料的 visit_id 重複（例如 ",
      ok$visit_id[which(twice)[1]], "）。原檔可能有重複的孩子，或 visit_id 沒有分開不同波次。"
    ))
    cross <- dplyr::distinct(ok, .data$visit_id, .data$cohort)
    dup_c <- unique(cross$visit_id[duplicated(cross$visit_id)])
    people <- dplyr::distinct(ok[c("visit_id", "subj_id", "cohort", "sex", "wave_m", "age_m")])
    if (length(dup_c) > 0) {
      problems <- c(problems, paste0(
        "同一個 visit_id 出現在兩個世代（例如 ", dup_c[1], "）。請讓 visit_id 包含世代。"
      ))
    } else if (anyDuplicated(people$visit_id) > 0) {
      v <- people$visit_id[duplicated(people$visit_id)][1]
      problems <- c(problems, paste0(
        "同一個列次（例如 ", v, "）的孩子欄（subj_id、sex、wave_m、age_m）不一致。",
        "請確認原檔沒有重複的孩子。"
      ))
    }
  }
  no_dom <- unique(answered$item_id[blank(answered$domain)])
  if (length(no_dom) > 0) {
    problems <- c(problems, paste0(
      "這些題沒有領域：", paste(utils::head(no_dom, 8), collapse = "、"), "。"
    ))
  }

  list(
    problems = problems,
    warnings = kits_score_warnings(stats, ok, spec),
    n_visits = length(unique(ok$visit_id)),
    n_items = length(unique(answered$item_id))
  )
}

# 分數與其他提醒（橘色）
kits_score_warnings <- function(stats, persons, spec) {
  out <- character(0)
  per_item <- stats[stats$n > 0, ]
  some <- function(x) paste(utils::head(unique(x), 5), collapse = "、")
  no_zero <- unique(per_item$item_id[per_item$min > 0])
  if (length(no_zero) > 0) {
    out <- c(out, paste0(
      length(no_zero), " 題沒有 0 分（例如 ", some(no_zero),
      "）。kits 要有 0 分才能自由校準；分數如果是從 1 開始，請在「3. 題目設定」選取這些題、平移 −1。"
    ))
  }
  gap <- per_item$item_id[per_item$gap]
  like <- per_item$item_id[per_item$gap & per_item$code_like != ""]
  if (length(like) > 0) {
    shown <- paste0(like, "：", per_item$code_like[match(like, per_item$item_id)])
    out <- c(out, paste0(
      length(like), " 題的分數中間有缺，而且最高的值像遺漏值代碼（", some(shown), "）。",
      "請不要用「合併」把它接起來：在「3. 題目設定」選取這些題，在「合併」那一列填這些值、",
      "按「當成遺漏值」，或先到「② 遺漏值轉 NA」處理。"
    ))
    gap <- setdiff(gap, like)
  }
  if (length(gap) > 0) {
    out <- c(out, paste0(
      length(gap), " 題的分數中間有缺（例如 ", some(gap), "）。kits 校準時會停下來，",
      "請在「3. 題目設定」用「合併」把分數接起來。"
    ))
  }
  big <- per_item$item_id[per_item$max >= KITS_CODE_MIN]
  if (length(big) > 0) {
    out <- c(out, paste0(
      length(big), " 題有 7777 以上的分數（例如 ", some(big), "），可能是遺漏值代碼。",
      "請先到「② 遺漏值轉 NA」處理。"
    ))
  }
  doms <- setdiff(unique(stats::na.omit(per_item$domain)), KITS_ANCHOR_DOMAINS)
  if (length(doms) > 0) {
    out <- c(out, paste0(
      "領域 ", paste(doms, collapse = "、"), " 不是 lan、motor、cog；",
      "外部連結的錨題只認這三個名稱（其他領域只做內部連結時沒關係）。"
    ))
  }
  empty <- c(setdiff(spec$items$column, stats$column), stats$column[stats$n == 0])
  if (length(empty) > 0) {
    out <- c(out, paste0(
      length(empty), " 題沒有任何作答，不會出現在作答檔：",
      paste(utils::head(empty, 8), collapse = "、"), if (length(empty) > 8) "…" else "", "。"
    ))
  }
  n_age <- length(unique(persons$visit_id[is.na(persons$age_m)]))
  if (n_age > 0) out <- c(out, paste0(n_age, " 個列次沒有 age_m（題目表的通過年齡要用它）。"))
  out
}

#' 未壓縮 CSV 大約幾 MB（用前幾列長格式的平均長度估計）
#' @param head_long 長格式的前幾列（kits_long_head()）
#' @param n_rows 長格式的總列數
#' @noRd
kits_csv_mb <- function(head_long, n_rows) {
  if (nrow(head_long) == 0 || n_rows == 0) return(0)
  text <- readr::format_csv(head_long, na = "")
  per_row <- nchar(text, type = "bytes") / (nrow(head_long) + 1)
  per_row * (n_rows + 1) / 1024^2
}

#' 摘要：每個世代 × 領域幾個列次、幾題、幾格作答、哪些波次（由寬資料的題目欄計數）
#' @noRd
kits_summary <- function(prep) {
  parts <- purrr::map(prep$files, function(f) {
    if (length(f$have) == 0) return(NULL)
    dom <- unname(prep$domain_of[kits_out_ids(f$have, prep$rename)])
    purrr::map(unique(dom), function(d) {
      cols <- f$have[dom == d]
      n <- Reduce(`+`, lapply(f$scores[cols], function(x) !is.na(x)), integer(nrow(f$persons)))
      keep <- n > 0
      rows <- tibble::tibble(
        cohort = f$persons$cohort[keep], domain = d, visit_id = f$persons$visit_id[keep],
        wave_m = f$persons$wave_m[keep], n = as.integer(n[keep])
      )
      items <- purrr::map(cols, function(col) {
        co <- unique(f$persons$cohort[!is.na(f$scores[[col]])])
        tibble::tibble(cohort = co, domain = rep(d, length(co)),
                       item_id = rep(kits_out_ids(col, prep$rename), length(co)))
      }) |>
        purrr::list_rbind()
      list(rows = rows, items = items)
    })
  }) |>
    purrr::list_flatten()
  rows <- purrr::list_rbind(purrr::map(parts, "rows"))
  items <- purrr::list_rbind(purrr::map(parts, "items"))
  if (is.null(rows) || nrow(rows) == 0) return(NULL)
  n_items <- dplyr::summarise(items, 題數 = dplyr::n_distinct(.data$item_id),
                              .by = c("cohort", "domain"))
  rows |>
    dplyr::summarise(
      列次 = dplyr::n_distinct(.data$visit_id), 作答格數 = sum(.data$n),
      波次 = paste(sort(unique(.data$wave_m)), collapse = ", "),
      .by = c("cohort", "domain")
    ) |>
    dplyr::left_join(n_items, by = c("cohort", "domain")) |>
    dplyr::select("cohort", "domain", "列次", "題數", "作答格數", "波次") |>
    dplyr::arrange(.data$cohort, .data$domain) |>
    dplyr::rename(世代 = "cohort", 領域 = "domain")
}
