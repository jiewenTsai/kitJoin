# kitJoin validation report

Standard: [KIT validation standard](https://github.com/jiewenTsai/kitDesign/blob/main/inst/validation/criteria.md)
(`kitDesign::kit_validation("kitDesign", "criteria")`). Version validated: kitJoin 0.10.1, with the
regression tests, the `names_sort` option and the snapshot of kitJoin 0.10.2. Date: 2026-11-04.

## Summary

kitJoin writes data files, not estimates, so its faults are about rows, IDs and codes. 18 of 18
metamorphic relations hold, after the column order of wide files was settled in 0.10.2 (the tidyr
default, now documented, with `names_sort` as an option). The files of a fixed scenario are
byte-identical across two runs. The existing tests caught 6 of 8 planted faults; both survivors are in missing-code replacement and are now
covered by regression tests. This validation was run after release: no acceptance criteria were
written before generation, and 12 faults were found after 2026-11-01, more than in any other KIT
package. Most of them changed data silently.

## Acceptance criteria (written before generation)

None were written before the code; this report is retrospective. The criteria that the tests now
imply:

1. Missing-code replacement changes only the listed codes, in the listed columns, and never an ID column.
2. A join keeps one row per ID: the union (full), the intersection (inner), the first file (left).
3. Wide to long to wide gives the same cells; the new columns follow their first appearance in the
   data, or are sorted when `names_sort` is ticked.
4. The kits response file has one row per visit and item, scores shifted exactly as asked.
5. IDs keep their exact text: 1 and 001 are different only if the user says so.

## Scope

| Function | Output that reaches a report | Error types asked |
|---|---|---|
| `replace_na_codes`, `label_na_codes_of` | Data with missing codes as NA | Data management, boundary |
| `join_waves` | Merged wide file | Data management, variable mapping |
| `build_attrition` | Attrition flags and counts | Data management, boundary |
| `reshape_wider`, `reshape_longer` | Long and wide files | Variable mapping, data management |
| `stack_waves` | Stacked file with a wave column | Data management, boundary |
| `kits_build` | kits response file | Formula (score shift), variable mapping, boundary |
| `filter_files`, `summary_table` (page 篩選與摘要, added after 0.10.2) | Filtered files passed to the other pages; group summary table | Data management, boundary |

## Engine equivalence

Not applicable: kitJoin has no estimator. The oracle is the metamorphic layer and hand-made data
with known answers.

For the page 篩選與摘要 the R code under 「用 R 怎麼做」 is the second engine. App and code build
each condition from the same text, and the tests run the code on the three example files and on a
SAV with value labels, a column that is blank in a whole group, and column names that need
backticks. The filtered files (rows, row order, columns, value labels) and the summary tables
match the app (`tests/testthat/test-filter_pipeline.R`). Hand-made data with known answers cover
each operator, AND and OR, the rows a condition cannot decide because of a blank (NA, dropped as in
`dplyr::filter()` and counted), the two column modes, sorting with blanks last, a filter that keeps
0 rows, and groups that are all blank (NA, not NaN or Inf). The page was not part of the metamorphic,
snapshot or mutation layers below.

## Metamorphic relations

Script: `kitverse/inst/validation/metamorphic.R`. 6 functions × 3 relations; 18 of 18 hold.

| Function | Relations |
|---|---|
| `replace_na_codes` | Idempotent; code order does not matter; non-codes, excluded and text columns unchanged |
| `join_waves` | Row order; full, inner and left joins give the union, intersection and first-file counts; renaming a wave label renames only the suffix |
| `reshape_wider`, `reshape_longer` | Long → wide → long; wide → long → wide (see below); a missing wave gives NA, and `drop_na` gives the right count |
| `stack_waves` | Row counts per wave; file order; a column in some files only is NA in the others |
| `build_attrition` | Retained + lost + NA = earlier n; later file order and duplicates; an empty later file makes everyone lost |
| `kits_build` | Score shift −1 lowers every score by exactly 1; row order; NA cells give no row |

In kitJoin 0.10.1 the wide → long → wide relation failed: after shuffling the long rows the columns
were `id, t1, t3, t2` instead of `id, t1, t2, t3`, although every cell matched by name.
`pivot_wider()` orders new columns by first appearance (`names_sort = FALSE`). kitJoin keeps the
tidyr default, because its reshape page teaches tidyr and its R code must give the same file, but
0.10.2 says so under the setting and adds the option 「新欄照欄名來源的值排序」 (`names_sort`). The
relation now asks for the cells always, for the original order with `names_sort = TRUE`, and for the
first-appearance order without it. Sorting is by value: numbers by size, text alphabetically
(`t10` before `t2`).

## Report snapshot

Script: `inst/validation/snapshot.R`. Scenario: the three example files in `inst/extdata`
(missing codes 7777 and 9999 in `lang_score`), and simulated wide item data for the kits response
file (two cohorts, three waves, scores shifted 1–5 → 0–4). Each file is written with the app's own
writers and compared with the MD5 in `inst/validation/results/snapshot_md5.txt`.

| File | Step | Lines | Two runs identical |
|---|---|---|---|
| `missing_t2.csv` | Missing codes → NA (ID excluded) | 7 | yes |
| `joined.csv` | Full join of t1–t3 on `release_id`, `sex` constant | 8 | yes |
| `attrition_t2.csv` | Attrition of t1 at t2 | 7 | yes |
| `stacked.csv` | Stacked waves with a wave column | 18 | yes |
| `wide.csv` | Stacked → wide on `lang_score`, `names_sort = TRUE` | 8 | yes |
| `kits_long.csv` | kits response file | 46 | yes |

A release that changes any of these files says why in NEWS.

## Mutation testing

8 mutants in copies of the source; 6 killed (75%). Scripts and results:
`kitverse/inst/validation/mutation/`.

| Error type | Mutants | Killed | Surviving (and the missing test) |
|---|---|---|---|
| Data management | 4 | 2 | J02 (`replace_na_codes` ignored `exclude`), J08 (negative label codes such as −9 dropped): tests added in 0.10.2 |
| Variable mapping | 2 | 2 | |
| Formula | 1 | 1 | |
| Boundary | 1 | 1 | |

## Faults and regression tests

From NEWS and git (`kitverse/inst/validation/results/bugs.csv`): 50 faults recorded since the
AI-assisted work began on 2026-10-22. The faults after 2026-11-01:

| Version | Fault | Error type | Regression test |
|---|---|---|---|
| 0.10.1 | The header language of the variables file also fixed the syntax of labels and codes | Data management | yes |
| 0.9.1 | Per-wave item names (`socC20_w1`, `_w2`) were shown as separate items | Variable mapping | yes |
| 0.9.1 | Picking by column name ignored prefixes containing "s" (`\s` inside `[]`) | Variable mapping | yes |
| 0.9.0 | Merged wide multi-wave files gave a silently wrong response file | Variable mapping | yes |
| 0.9.0 | 9, 98, 99 above a small scale were not flagged; the −1 shift turned 9 into 8 | Boundary | yes |
| 0.8.0 | IDs 1 and 001 were treated as two people; a global 99 erased a real age of 99 | Boundary | yes |
| 0.8.0 | A launcher test still checked bslib | Other (test) | not needed |
| 0.7.7 | A WARNING and 2 NOTEs on r-universe | Other | not needed |
| 0.7.1 | CSV.GZ to CSV wrote no BOM, so Excel garbled Chinese | Data management | yes |
| 0.7.1 | Time-stamped file names used UTC on the cloud | Data management | yes |
| 0.7.1 | Re-running tab 2 printed "Error in if: missing value" | Boundary | yes |
| 0.7.1 | R/ sourced twice at start-up | Install | yes |

How these were caught is not stated in NEWS or the commits for 10 of the 12.

## Metrics

| Metric | Value |
|---|---|
| Acceptance criteria written beforehand | 0 |
| First-generation pass rate | 8 of 16 versions with a behaviour change (50%) |
| Faults caught per layer | Snapshot 0, metamorphic 0 (1 question settled: column order), mutation 2 missing tests; history: unit test 1, r-universe 1, not stated 10 (after release) |
| Faults found after release | 12 (strict: after 2026-11-01), 41 (broad: in any earlier tag) |

## Reproduce

```r
source(system.file("validation", "snapshot.R", package = "kitJoin"))
source(system.file("validation", "metamorphic.R", package = "kitverse"))
```
