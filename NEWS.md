# kitJoin 0.7.6

* Uses kitDesign 0.7.1 (the same version as kitsUI, kitverse and kitItemBank). The header still reads
  "kitJoin v0.7.6 · 2026-11-02"; nothing changes on screen.

# kitJoin 0.7.5

* Uses kitDesign 0.7.0 (the same version as kitsUI, kitverse and kitItemBank). Nothing changes on screen.

# kitJoin 0.7.4

* Uses kitDesign 0.6.1: plots and tables inside closed sections are drawn when the section is opened
  (no more squashed plots). Nothing else changes.

# kitJoin 0.7.3

* Uses kitDesign 0.6.0 (the same version as kitsUI and kitverse). Nothing changes on screen.

# kitJoin 0.7.2

## Fixes

* Firefox with some add-ons (for example Google PDF Viewer and other "document viewer" add-ons): clicking
  a download button left the page and the download was "Not Found". The add-on takes over download
  links on the page. Download buttons now come from kitDesign 0.5.2, where they are buttons, not links,
  so there is nothing for an add-on to take over. The buttons look the same.

# kitJoin 0.7.1

## Fixes

* ⑧ 格式轉換: CSV.GZ → CSV now starts the CSV with a BOM when the text is UTF-8, so Windows Excel shows
  Chinese correctly (kitJoin's own .csv.gz files have no BOM, so converting them back never had one).
* File names with a time stamp (for example `long_20261031_194357`) use Taiwan time; on the cloud they
  were in UTC.
* Re-running ② after ③ or ④ had a result no longer prints `Error in if: missing value` in the R
  console (the preview line read an emptied result).
* The cloud version and run.bat sourced `R/` twice at start-up and warned that the folder looks like an
  R package. `R/_disable_autoload.R` now turns shiny's autoload off.

# kitJoin 0.7.0

## Changes you can see

* The app is now called 「KIT 資料處理工具」 (KIT data processing tool) instead of
  「KIT 資料串接工具」 (KIT data join tool): it does more than joining (stacking, attrition,
  missing values, validation, kits response files). The package name stays `kitJoin`.
* The version line in the header starts with the package name: "kitJoin v0.7.0 · 2026-11-01"
  (kitDesign 0.5.0).

## Under the hood

* Uses kitDesign 0.5.0 (`Imports: kitDesign (>= 0.5.0)`, `Remotes: jiewenTsai/kitDesign@v0.5.0`);
  `manifest.json` regenerated.

# kitJoin 0.6.0

## Changes you can see

* ⑥ kits 作答檔: step 2 only picks the item columns. Domain and scoring are set in step 3's
  item table, which is shown open and can be downloaded at any time
  (`kits_item_settings.csv`). The default domain is the item group's name without a
  trailing `_`, `.` or `-`.
* ⑦ The reshape diagram shows wide → long first, with a double arrow: both directions work.
* 「用 R 怎麼做」: code boxes stay inside the page, and every code line fits in 70 columns.
* Danger buttons are salmon, as the KIT design language says.

## Under the hood

* kitJoin now uses **kitDesign** (`Imports: kitDesign (>= 0.4.0)`, installed from GitHub
  `jiewenTsai/kitDesign`) for the page, the shared components and the styles. Its own
  copies (`kit_brite.R`, `code_wrap.R`, `kit_theme.R`, the logo) are gone, about 2,000 lines.
  Every output file is byte-identical to 0.5.0's.
* The download button (`R/download.R`) and the cloud-version check and footer moved into
  kitDesign 0.4.0 (`kit_download_button()`, `kit_is_cloud()`, `kit_cloud_footer()`);
  `R/cloud.R` keeps only kitJoin's text. Same file names, headers and bytes as before.
* The six run tabs (join, stack, attrition, missing values, reshape, file conversion) use
  kitDesign's `kit_run_tab_ui()` / `kit_run_tab_server()` for the page skeleton and the
  status flow (ready line, run with progress, status box, notification, scroll, stale after a
  change, result details, 「用 R 怎麼做」); each tab keeps only its own settings, run and
  result. The ⑥ kits tab uses the server part for its run and status. Same page HTML, ids,
  statuses and output files as before.
* `run.bat` installs or upgrades the packages that are missing or older than DESCRIPTION
  asks for (an old bslib used to break the app on the first screen), including the GitHub
  ones (kitDesign).

# kitJoin 0.5.0

Generality fixes found by running every tab on another panel survey (TDCYP, five waves of
SPSS files whose ID column, item names and missing-value codes differ from KIT's).

## Wrong results that were reported as success

* ⑥ no longer writes suspected missing-value codes as scores without saying so. Values
  labelled 遺漏值 / 跳答 / 不適用 / 拒答 (and similar), ②'s code list, values of 7777 and
  above, and values clearly apart from the item's range (e.g. 95–99 next to 1–5) are listed
  per item in 「3. 題目設定」 and in the result, which then shows 「完成，但請注意」 with a
  link to ②: turn them into NA in 「② 遺漏值轉 NA」 first, then come back to ⑥. ⑥ itself
  never turns values into NA (the data are assumed correct): if you go on, these values are
  written unchanged as scores, and shifting or reversing the scores leaves them alone. The
  gap warning no longer suggests 「合併」 for values that look like codes. The same warning
  covers codes in the sex, `wave_m` and `age_m` columns. For KIT data nothing changes:
  values of 7777 and above are still written unchanged, with the new warning.
* ⑦ wide → long no longer copies the first column's value labels onto every item. When
  value or variable labels differ between the stacked columns they are dropped with a
  visible note.
* ⑤ stops when most baseline IDs are duplicated (e.g. a birth year chosen as the ID)
  instead of reporting 「完成」; ③ asks for confirmation when a join would multiply the rows
  and refuses joins that are far too large. ID choices show each column's share of unique
  values.
* ④ a column named `w` (e.g. a survey weight) no longer overwrites the wave column.
* Numeric IDs of six or more digits are no longer turned into scientific notation
  (`1e+05`) when compared with text IDs or written to `subj_id` / `visit_id`. Text IDs are
  trimmed before matching.
* ② can convert per variable from SPSS value labels: only values whose label says missing
  (遺漏、跳答、跳題、不適用、拒答、未作答、missing、refused, skip…) become NA, so a real
  weight of 99 kg stays. 不知道 / 不清楚 / "don't know" are not in the default keywords
  (they can be a valid answer); add them in ② when you want them turned into NA. ⑥ uses
  the same rule when it looks for suspected codes. This mode is chosen automatically when
  the labels mark missing values and the KIT codes are absent or at least ten times rarer
  (TDCYP has a few dozen real values that happen to equal 9999); when labels mark more
  cells but not that many more, ② only suggests it. The code list is still available.
* ② results are cleared when the codes, excluded columns or text matching change, and
  ③–⑦ say that ② must be run again instead of silently using the raw data.
* SPSS files saved as Big5 without a proper encoding record are re-read as CP950 (①
  can also be set by hand).

## Crashes and broken files

* SAV downloads are read back after writing; when haven cannot read them, problematic
  text value labels are cleaned and the file is rewritten, and the affected columns are
  named on screen (also in ⑧'s notes).
* Columns whose value labels were all missing codes no longer break the preview in ②–⑤.
* 「用 R 怎麼做？」 code uses `data_1`, `data_2`… and runs (and matches the app) with numeric
  or Chinese wave names, quotes, blank or mixed-type IDs and column names with spaces;
  this is now checked by tests that evaluate the generated code.

## Data with other naming schemes

* ③ ⑤ each file can use its own ID column(s) (e.g. `w1_id`, `w2_id`, `w3id`); they are
  renamed to one key before matching. Files that share the ID name work as before.
* ⑥ 「2. 對應欄位」: `subj_id`, the wave label, `wave_m`, `age_m` and `sex` can be chosen per
  file.
* ⑥ common output item IDs across waves: an `item_id_out` column in the item table or
  settings file, or the rule 「欄名去掉 `^w[0-9]+`」. Two columns of one file mapping to the
  same ID are refused; merged IDs are listed, with a warning when their variable labels
  differ.
* ⑥ reverse-item naming rule: trailing `r` (default, as before), leading `r`, or off.
* ⑥ `wave_m` is called 「時間點」 with a selectable unit (months by default). Negative or
  non-numeric values and `visit_id` collisions caused by `_` are refused.
* ⑥ labelled cohort / sex columns can be written as codes or labels; missing codes in them
  are flagged. An item that is text in one file is skipped only in that file. The item
  table / settings parser accepts BOM, SAV label columns, Y/N and `1=0,2=1`, and stops with
  an explanation when it cannot read a row; items with no answers need no domain.
* Other: empty or header-only files are errors; semicolon and tab separated CSV files are
  detected; uploading the same file twice is flagged; wave names that differ only in case
  are refused; ID detection also recognises `w3id`, 學號 and 編號.
* Code and monospace text use JetBrains Mono (Google Fonts, loaded by the browser) instead
  of M PLUS Code Latin.

# kitJoin 0.4.3

* Downloads now always carry the file name and extension, including names with Chinese
  characters, in every browser. The app serves each file itself: the download URL ends with
  the file name, the response has an RFC 6266 `Content-Disposition` (`filename` plus
  `filename*=UTF-8''...`, no redirect), and the button has `download="<name>"`. This covers
  every download (②③④⑤⑦ CSV / CSV.GZ / SAV, ⑥ response file and item settings, ⑧).
  Before, some browsers saved e.g. `missing-download_sav` without `.sav`.

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
