# kitJoin 0.4.2

* ⑥ 「2. 對應欄位」: the wave label, `wave_m`, `sex` and `age_m` rows can now come from a
  column, be typed per file (「自己填」), or be left out (optional rows only). Typed values
  get one input per file in section 1 (the wave label is prefilled with the wave name from
  ①); `wave_m` and `age_m` must be numbers. Typed values are written exactly like a column
  holding that value, and `visit_id` uses the typed wave label. This covers one file per
  wave with no wave columns. The allowed sources are defined in `KITS_MAP_ROWS`.
* ⑥ section 3 is now 「題目設定」: one row per candidate item column with include/exclude,
  domain, observed raw scores, a raw → score map (shown as a two-row table: 原分數 / 計分),
  the resulting scores and alerts. Bulk actions on selected rows: set domain, shift,
  reverse (max − score; max defaults to the item's current maximum), merge
  (「原分數 4、5 → 計分 3」), restore, exclude/include; selection helpers 全選 / 全不選 /
  name filter. The regex rules box and the separate 「進階調整」 steps are gone.
* ⑥ item settings can be downloaded (`item_id`, `include`, `domain`, `map_raw_to_score`
  written as `1=0;2=1`) and uploaded again for the next wave; a plain `item_id` + `domain`
  table still works (with the `r` fallback). Maps that were changed and miss a newly
  observed raw score block the run until fixed.
* ⑥ suggests (never applies by itself) "select all and shift −m" when every included item
  starts at the same m > 0. At run time, final scores must be integers ≥ 0.
* ⑥ 「用 R 怎麼做？」 reads the settings file and writes only changed maps, grouped:
  `\(x) x - 1`, `\(x) 4 - x`, or `case_when()`.
* ⑥ 加上「開啟 kitsUI」按鈕 (in the tab guide and next to the downloads; opens kitsUI in a
  new tab).
* ⑥ top callout: several files are merged into one response file; upload only the file
  you need for a single wave or domain.
* ⑥ 「3. 題目設定」 stays fast with 150–300 items: the table is built as one HTML string
  (same markup as before), and the name filter hides rows in the browser instead of
  redrawing the table. The filter is now case-insensitive.
* ⑦ wide → long: 「要轉成長格式的欄（cols）」 is click-only. The `matches()` regex box is
  gone. Pick the columns to convert, or pick the columns to keep (`cols = !c(id, sex)`) and
  convert the rest. 全選 / 清除 and name-group buttons (「lan 開頭（10 欄）」) add whole groups;
  a live line says how many columns convert and how many stay. The R code writes
  `!c(...)`, `starts_with()` (only when it selects exactly the picked columns) or the names.
* ⑦ looks at the file first and says whether it looks long (an ID repeats) or wide (one row
  per ID with groups of item columns). The ID column can be changed. When the chosen
  direction contradicts the guess, an orange note appears by the run button (it does not
  block or switch direction).
* ⑧ downloads keep the file name for non-ASCII names: 第一波.csv.gz → 第一波.sav. Before,
  the browser saved `convert-download_file` without the `.sav` extension (Shiny sends no
  file name for non-ASCII names; the button now carries it in its `download` attribute).
* Code and monospace text use M PLUS Code Latin (Google Fonts, loaded by the browser).
* README: the ⑥ section is complete again (part of it had moved into the ⑦ and ⑧
  sections), with FAQ entries for "every item is one domain" and "no wave column".

# kitJoin 0.4.1

* ②（選用）遺漏值轉 NA: its texts now say that ③–⑦ (not only ③④⑤) use the processed
  data, and the result has its own 「下載處理後的檔案」 section (CSV, CSV.GZ, SAV), so a user
  with one file can fix missing codes and download without going further.
* ① 上傳檔案: with one file, the next-step box lists what works with one file (②, ⑥, ⑦)
  and says ③④⑤ need at least two; the box for two or more files also links to ⑦.
* ⑥ no longer has its own missing-code step; use ②. ⑥'s adjustments are now 1 shift
  scores by -1 and 2 reverse scoring. When item, `wave_m` or `age_m` columns still hold
  values of 7777 or more and ② was not applied, ⑥ shows a warning with a link to ②, and the
  "shift by 1" suggestion ignores those values. The 「用 R 怎麼做？」 code notes when ② was
  applied.

# kitJoin 0.4.0

## New features

* New tab "⑥ 轉成 kits 作答檔" turns wide item data (one row per child and wave, one column
  per item) into the long response file that kits and kitsUI read. The output always has the
  nine kits columns in fixed order and type (`visit_id`, `subj_id`, `cohort`, `sex`, `wave_m`,
  `age_m`, `item_id`, `item_score`, `domain`); unmapped optional columns are written empty.
  - Cohort is typed per file or taken from a column (never guessed from the file name).
  - `visit_id` is built as `{cohort}_{subj_id}_{wave label}`, falling back to `wave_m`
    (with an on-screen note) when no wave-label column is mapped.
  - Domains come from an uploaded item table (`item_id`, `domain`) and/or typed regex
    rules; `r`-suffixed items missing from the table use their base item. Columns without
    a domain must be excluded or assigned one.
  - Opt-in adjustments run in a fixed order: missing codes to NA, shift scores by -1,
    reverse scoring (max - x on the 0-based scale). Shifting to 1-based data is suggested,
    never ticked automatically.
  - Checks block duplicates (visit x item), empty required columns, visit ids shared by two
    cohorts, non-integer or negative scores; warnings flag items without a 0, score gaps,
    likely missing codes, and non-anchor domain names.
  - Memory-light: all checks and adjustments run on the wide data, and the long file is
    streamed to `.csv.gz` in chunks at download, so ~10M long rows peak around 0.6 GB.
  - Downloads one merged `.csv.gz` (or `.csv`); the "用 R 怎麼做？" panel shows tidyverse code
    with only the ticked steps.
* New tab "⑦ 長寬互轉" reshapes one uploaded file with `tidyr::pivot_wider()` or
  `pivot_longer()`. Controls map 1:1 to tidyr arguments with tidyr defaults.
  - Wider: duplicated id x names_from blocks and lists the rows (no `values_fn`); id
    columns that vary within an id are flagged; new names may not collide with id
    columns; SPSS renames are announced.
  - A "套用 kitsUI 分數表預設" button fills (never runs) a preset for kitsUI score tables,
    and an empty-cell table separates "no row" from "row present but empty".
  - The "用 R 怎麼做？" panel prints the `pivot_*()` call with non-default arguments only.
* Shared `pivot_longer()` control module (`mod_pivot_longer_ui()`,
  `mod_pivot_longer_server()`, `pivot_longer_code()`, `pivot_longer_run()`), used by ⑥
  and ⑦: each control maps to one tidyr argument, defaults follow tidyr, and the
  generated code lists only non-default arguments.
* New tab "⑧ 檔案格式轉換" (file format conversion): a standalone converter
  that takes one CSV, CSV.GZ or SPSS `.sav` file and converts it to one of the
  other two formats. CSV <-> CSV.GZ is streamed in chunks without parsing (UTF-8
  bytes are kept as is; BIG5 is re-encoded to UTF-8 line by line), so large
  files are not held in memory. Conversions involving `.sav` use haven; IDs
  with leading zeros stay text, invalid SPSS names are renamed, and SPSS value
  labels are written as codes unless "數值標籤轉成文字" is ticked.
* Every result download area gains a "下載 CSV.GZ" button
  (`readr::write_csv()` to a gzip-compressed file).
* "① 上傳檔案" now tells users who pick a `.csv.gz` file to convert it in ⑧ first.
* tidyr and vctrs are now imported.

## Bug fixes

* ① 上傳檔案: when several files were uploaded at once, only the last file's wave name,
  move and remove buttons worked; the others looked edited but the server kept `t1`, `t2`.
