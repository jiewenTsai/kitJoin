# 轉成 kits 作答檔：資料來源與轉換前的整理（純函式；畫面在 mod_kitsdata.R）
#
# ⑥ 要的是「一列一個孩子的一個波次、一題一欄」。這裡處理不是這個形狀的資料：
#   kits_detect_wave_suffix()  多波寬檔（soc01_t1、soc01_t2…）：偵測欄名的波次後綴或開頭
#   kits_pivot_waves()         把多波寬檔轉成一列一個波次（題目欄去掉波次，波次放進 wave_label、wave_m）
#   kits_detect_wave_text()    長檔的波次欄是文字（W1、t2）：可以取出數字當時間點
#   kits_source_apply()        套用上面兩種整理（app 與「用 R 怎麼做」寫的是同一件事）
#   code_kitsdata_source()     上面兩種整理的程式碼

# 波次記號：欄名結尾（soc01_t1、soc01.w2、lana01_M12）或開頭（w1q1、w2_d5_1、t1_dep）
KITS_WAVE_SUFFIX_TOKENS <- c("wave", "time", "t", "w", "m")
KITS_WAVE_PREFIX_TOKENS <- c("wave", "t", "w")

#' 不分大小寫的正規式片段：wave → [Ww][Aa][Vv][Ee]（tidyr 的 names_pattern 不能設 ignore.case）
#' @noRd
kits_ci_regex <- function(token) {
  chars <- strsplit(token, "")[[1]]
  paste0("[", toupper(chars), tolower(chars), "]", collapse = "")
}

#' 欄名裡的波次記號：多波寬檔（同一題在好幾個波次各一欄）
#'
#' 欄名結尾是 _t1、_w2、.wave3、_M12，或開頭是 w1、t2_、wave3_；去掉波次記號後的「題幹」
#' 至少要有 2 個在 2 個以上的波次出現，才算多波寬檔（只有 age_t1、age_t2 這種一欄不算）。
#' @param cols 欄名
#' @return NULL，或 list(cols（要轉的欄）、pattern（tidyr names_pattern）、side（"suffix"/"prefix"）、
#'   token（例如 "t"）、waves（波次記號，依數字排）、stems（題幹）、n_stems（跨波次的題幹數）、
#'   label_col、wave_col（轉換後放波次記號、時間點的欄名）)
#' @noRd
kits_detect_wave_suffix <- function(cols) {
  best <- NULL
  try_one <- function(side, token) {
    tk <- kits_ci_regex(token)
    pattern <- if (side == "suffix") {
      paste0("^(.+)[_.](", tk, "[0-9]{1,3})$")
    } else {
      paste0("^(", tk, "[0-9]{1,2})[_.]?(.+)$")
    }
    hit <- cols[grepl(pattern, cols)]
    if (length(hit) < 4) return(NULL)
    stem_i <- if (side == "suffix") 1 else 2
    wave_i <- 3 - stem_i
    parts <- regmatches(hit, regexec(pattern, hit))
    stems <- vapply(parts, `[`, "", stem_i + 1)
    waves <- vapply(parts, `[`, "", wave_i + 1)
    n_waves <- tapply(waves, stems, function(w) length(unique(w)))
    cross <- names(n_waves)[n_waves >= 2]
    if (length(cross) < 2) return(NULL)
    wv <- unique(waves)
    wv <- wv[order(suppressWarnings(as.numeric(gsub("[^0-9]", "", wv))), wv)]
    list(cols = hit, pattern = pattern, side = side, token = token, waves = wv,
         stems = unique(stems), n_stems = length(cross),
         label_col = kits_free_name("wave_label", cols), wave_col = kits_free_name("wave_m", cols))
  }
  for (side in c("suffix", "prefix")) {
    tokens <- if (side == "suffix") KITS_WAVE_SUFFIX_TOKENS else KITS_WAVE_PREFIX_TOKENS
    for (token in tokens) {
      d <- try_one(side, token)
      if (!is.null(d) && (is.null(best) || d$n_stems > best$n_stems)) best <- d
    }
  }
  best
}

#' 新欄名：name 已經有了就加 .kits_ 開頭（不和使用者的欄撞名）
#' @noRd
kits_free_name <- function(name, taken) {
  if (!name %in% taken) name else paste0(".kits_", name)
}

#' 多波寬檔 → 一列一個孩子的一個波次
#'
#' 波次記號放進 wave_label（例如 "t1"），裡面的數字放進 wave_m（例如 1）；題目欄去掉波次記號
#' （soc01_t1、soc01_t2 → soc01）。不分波次的欄（ID、性別）每個波次都留著。
#' 同一個題幹各波次的值標籤不一樣時，那一題不留值標籤。
#' @param det kits_detect_wave_suffix()
#' @return list(data, label_col, wave_col, zap_cols（拿掉值標籤的欄）)
#' @noRd
kits_pivot_waves <- function(df, det) {
  label_col <- det$label_col
  wave_col <- det$wave_col
  zap_cols <- kits_wave_zap_cols(df, det)
  for (col in zap_cols) df[[col]] <- haven::zap_labels(df[[col]])
  names_to <- if (det$side == "suffix") c(".value", label_col) else c(label_col, ".value")
  long <- tidyr::pivot_longer(df, cols = dplyr::all_of(det$cols), names_to = names_to,
                              names_pattern = det$pattern)
  long[[wave_col]] <- readr::parse_number(long[[label_col]])
  list(data = long, label_col = label_col, wave_col = wave_col, zap_cols = zap_cols)
}

#' 同一個題幹各波次的值標籤不一樣：這些欄先拿掉值標籤（不然不能放進同一欄）
#' @noRd
kits_wave_zap_cols <- function(df, det) {
  parts <- regmatches(det$cols, regexec(det$pattern, det$cols))
  stems <- vapply(parts, `[`, "", if (det$side == "suffix") 2 else 3)
  unlist(lapply(split(det$cols, stems), function(cols) {
    labs <- lapply(df[cols], function(x) attr(x, "labels", exact = TRUE))
    if (length(unique(labs)) > 1) cols
  }), use.names = FALSE)
}

#' 長檔的波次欄是文字、每一格剛好有一個數字（W1、t2、wave 3、M12）：可以取出數字當時間點
#'
#' 已經有 wave_m 欄、或波次欄本來就是數字時不用。
#' @return NULL，或 list(col（波次欄）、new（新的時間點欄名）、examples（例如 "W1 → 1"）)
#' @noRd
kits_detect_wave_text <- function(df) {
  cols <- names(df)
  if (any(tolower(cols) == "wave_m")) return(NULL)
  col <- kits_guess_map(cols)$wave
  if (!nzchar(col)) {
    hit <- cols[grepl("^(time|timepoint|occasion|wave_?no)$", cols, ignore.case = TRUE)]
    col <- if (length(hit) > 0) hit[[1]] else ""
  }
  if (!nzchar(col) || is.numeric(df[[col]])) return(NULL)
  v <- unique(trimws(as.character(as.vector(unclass(df[[col]])))))
  v <- v[!is.na(v) & nzchar(v)]
  if (length(v) == 0 || !all(grepl("^[^0-9]*[0-9]+(\\.[0-9]+)?[^0-9]*$", v))) return(NULL)
  v <- v[order(readr::parse_number(v))]
  shown <- utils::head(v, 3)
  list(col = col, new = "wave_m",
       # 原文： →
       # 原文： → 
       examples = paste0(shown, " \u2192 ", kits_id_text(readr::parse_number(shown))))
}

#' 套用整理：kind = "pivot"（多波寬檔轉成一列一個波次）或 "wave_num"（取出波次欄的數字）
#' @return 整理後的資料；det 是 NULL 時照原樣
#' @noRd
kits_source_apply <- function(df, kind, det) {
  if (is.null(det)) return(df)
  if (kind == "pivot") return(kits_pivot_waves(df, det)$data)
  df[[det$new]] <- readr::parse_number(as.character(as.vector(unclass(df[[det$col]]))))
  df
}

#' 整理的程式碼（每個檔案一段；沒有整理的檔案是 NULL）
#' @param objs 物件名稱
#' @param tf 每個檔案一個 NULL 或 list(kind, det)；pivot 的 det 可以有 zap_cols（kits_wave_zap_cols()）
#' @noRd
code_kitsdata_source <- function(objs, tf) {
  per_file <- purrr::map2(objs, tf, function(obj, t) {
    if (is.null(t) || is.null(t$det)) return(NULL)
    d <- t$det
    if (t$kind == "wave_num") {
      return(c(
        # 原文：# 波次直欄  |  是文字：取出裡面的數字當時間點（wave_m）
        # 原文：# 波次直欄  |  是文字：取出裡面的數字當時間點（wave_m）
        paste0("# \u6ce2\u6b21\u76f4\u6b04 ", d$col, " \u662f\u6587\u5b57\uff1a\u53d6\u51fa\u88e1\u9762\u7684\u6578\u5b57\u7576\u6642\u9593\u9ede\uff08wave_m\uff09"),
        paste0(obj, " <- mutate(", obj, ", ", d$new, " = parse_number(as.character(", r_col(d$col), ")))")
      ))
    }
    label_col <- d$label_col
    names_to <- if (d$side == "suffix") c(".value", label_col) else c(label_col, ".value")
    c(
      # 原文：# 多波寬檔：同一題各波次一個直欄（ | 、 | …）→ 一個橫列一個孩子的一個波次；
      # 原文：# 多波寬檔：同一題各波次一個直欄（ | 、 | …）→ 一個橫列一個孩子的一個波次；
      paste0("# \u591a\u6ce2\u5bec\u6a94\uff1a\u540c\u4e00\u984c\u5404\u6ce2\u6b21\u4e00\u500b\u76f4\u6b04\uff08", paste(utils::head(d$cols, 2), collapse = "\u3001"), "\u2026\uff09\u2192 \u4e00\u500b\u6a6b\u5217\u4e00\u500b\u5b69\u5b50\u7684\u4e00\u500b\u6ce2\u6b21\uff1b"),
      # 原文：# 波次記號放進  | ，裡面的數字放進 wave_m
      # 原文：# 波次記號放進  | ，裡面的數字放進 
      paste0("# \u6ce2\u6b21\u8a18\u865f\u653e\u9032 ", label_col, "\uff0c\u88e1\u9762\u7684\u6578\u5b57\u653e\u9032 ", d$wave_col),
      paste0(obj, " <- ", obj, " |>"),
      if (length(d$zap_cols) > 0) {
        # 原文：  # 各波次值標籤不同的題：先拿掉值標籤
        paste0("  mutate(across(all_of(", r_cols_vector(d$zap_cols, quote = TRUE),
               # 原文：), haven::zap_labels)) |>   # 各波次值標籤不同的題：先拿掉值標籤
               "), haven::zap_labels)) |>   # \u5404\u6ce2\u6b21\u503c\u6a19\u7c64\u4e0d\u540c\u7684\u984c\uff1a\u5148\u62ff\u6389\u503c\u6a19\u7c64")
      },
      paste0("  pivot_longer(matches(", r_str(d$pattern), ", ignore.case = FALSE),"),
      paste0("               names_to = c(", paste(vapply(names_to, r_str, ""), collapse = ", "), "),"),
      paste0("               names_pattern = ", r_str(d$pattern), ") |>"),
      paste0("  mutate(", r_col(d$wave_col), " = parse_number(", r_col(label_col), "))")
    )
  })
  per_file <- purrr::compact(per_file)
  if (length(per_file) == 0) return(NULL)
  c("", unlist(lapply(per_file, function(x) c(x, ""))))
}

#' 多波寬檔沒有轉成一列一個波次就挑了題目：同一題各波次會被當成不同的題、放在同一個時間點
#' @param items 納入的題（欄名）
#' @return 提醒（每個檔案一句）或 NULL
#' @noRd
kits_wave_suffix_warning <- function(dfs, file_names, items) {
  out <- purrr::map2_chr(dfs, file_names, function(df, name) {
    det <- kits_detect_wave_suffix(intersect(items, names(df)))
    if (is.null(det)) return(NA_character_)
    paste0(
      # 原文：： | 、 |  這類題目只差在波次記號（ | ），會被當成不同的題、
      # 原文：： | 、 |  這類題目只差在波次記號（ | 、 | ），會被當成不同的題、
      name, "\uff1a", paste(utils::head(det$cols, 2), collapse = "\u3001"), " \u9019\u985e\u984c\u76ee\u53ea\u5dee\u5728\u6ce2\u6b21\u8a18\u865f\uff08", paste(utils::head(det$waves, 3), collapse = "\u3001"), "\uff09\uff0c\u6703\u88ab\u7576\u6210\u4e0d\u540c\u7684\u984c\u3001",
      # 原文：放在同一個時間點。如果它們是同一題的不同波次，請在這一頁最上面把這個檔案「轉成一個橫列一個波次」。
      # 原文：放在同一個時間點。如果它們是同一題的不同波次，請在這一頁最上面把這個檔案「轉成一個橫列一個波次」。
      "\u653e\u5728\u540c\u4e00\u500b\u6642\u9593\u9ede\u3002\u5982\u679c\u5b83\u5011\u662f\u540c\u4e00\u984c\u7684\u4e0d\u540c\u6ce2\u6b21\uff0c\u8acb\u5728\u9019\u4e00\u9801\u6700\u4e0a\u9762\u628a\u9019\u500b\u6a94\u6848\u300c\u8f49\u6210\u4e00\u500b\u6a6b\u5217\u4e00\u500b\u6ce2\u6b21\u300d\u3002"
    )
  })
  out <- out[!is.na(out)]
  if (length(out) == 0) NULL else out
}

#' 一波一檔、題目欄名帶著各檔的波次記號（w1.sav 裡是 soc01_w1，w2.sav 裡是 soc01_w2）
#'
#' 每個檔案裡的題目欄只有一種波次記號、各檔的記號不同，去掉記號後至少 2 題在 2 個以上的檔案
#' 都有：這些欄的輸出題號預設成去掉記號的名字（soc01），同一題跨波次就是同一個題號。
#' 一個檔案裡有好幾種記號的是多波寬檔（kits_detect_wave_suffix()），不在這裡處理。
#' 只看結尾的記號：開頭的（TDCYP 的 w1d5_1、w2d5_1）各波問卷的題號不一定是同一題，
#' 還是由使用者用「輸出題號：去掉欄名的一段」決定。
#' @param file_cols 每個檔案的欄名（list）
#' @return NULL，或 list(out（具名字元向量：欄名 → 去掉記號的題號）、tags（每個檔案的記號）、token、side)
#' @noRd
kits_detect_file_wave_tags <- function(file_cols) {
  if (length(file_cols) < 2) return(NULL)
  best <- NULL
  for (side in "suffix") {
    tokens <- KITS_WAVE_SUFFIX_TOKENS
    for (token in tokens) {
      tk <- kits_ci_regex(token)
      pattern <- if (side == "suffix") paste0("^(.+)[_.](", tk, "[0-9]{1,3})$") else
        paste0("^(", tk, "[0-9]{1,2})[_.]?(.+)$")
      stem_i <- if (side == "suffix") 2 else 3
      per_file <- lapply(file_cols, function(cols) {
        hit <- cols[grepl(pattern, cols)]
        if (length(hit) < 2) return(NULL)
        parts <- regmatches(hit, regexec(pattern, hit))
        tags <- tolower(vapply(parts, `[`, "", 5 - stem_i))
        # 這個檔案大多數的欄是同一個記號（少數別的記號，例如上一波的年齡，不算）
        top <- names(which.max(table(tags)))
        keep <- tags == top
        if (sum(keep) < 2) return(NULL)
        list(tag = top, out = stats::setNames(vapply(parts[keep], `[`, "", stem_i), hit[keep]))
      })
      used <- !vapply(per_file, is.null, logical(1))
      if (sum(used) < 2) next
      tags <- vapply(per_file[used], `[[`, "", "tag")
      if (anyDuplicated(tags)) next
      stems <- unlist(lapply(per_file[used], function(p) unique(p$out)))
      n_shared <- sum(table(stems) >= 2)
      if (n_shared < 2) next
      if (is.null(best) || n_shared > best$n_shared) {
        file_tags <- rep(NA_character_, length(file_cols))
        file_tags[used] <- tags
        best <- list(out = unlist(lapply(per_file[used], `[[`, "out")), tags = file_tags,
                     token = token, side = side, n_shared = n_shared)
      }
    }
  }
  best
}

#' 輸出題號還沒設的欄：套用 kits_detect_file_wave_tags() 的題號
#' @noRd
kits_wave_tag_out <- function(settings, tags) {
  if (is.null(tags) || nrow(settings) == 0) return(settings)
  if (is.null(settings[["item_id_out"]])) settings$item_id_out <- NA_character_
  hit <- settings$column %in% names(tags$out) & is.na(settings$item_id_out)
  settings$item_id_out[hit] <- unname(tags$out[settings$column[hit]])
  settings
}
