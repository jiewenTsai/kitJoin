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
