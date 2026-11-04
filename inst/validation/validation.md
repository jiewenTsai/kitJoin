# kitJoin validation report

Standard: [KIT validation standard](https://github.com/jiewenTsai/kitDesign/blob/main/inst/validation/criteria.md)
(`kitDesign::kit_validation("kitDesign", "criteria")`). Version validated: kitJoin 0.10.1, with the
regression tests of kitJoin 0.10.2. Date: 2026-11-04.

## Summary

kitJoin writes data files, not estimates, so its faults are about rows, IDs and codes. 17 of 18
metamorphic relations hold; the one that fails is a column order, not a wrong value. The existing
tests caught 6 of 8 planted faults; both survivors are in missing-code replacement and are now
covered by regression tests. This validation was run after release: no acceptance criteria were
written before generation, and 12 faults were found after 2026-11-01, more than in any other KIT
package. Most of them changed data silently.

## Acceptance criteria (written before generation)

None were written before the code; this report is retrospective. The criteria that the tests now
imply:

1. Missing-code replacement changes only the listed codes, in the listed columns, and never an ID column.
2. A join keeps one row per ID: the union (full), the intersection (inner), the first file (left).
3. Wide to long to wide gives the same cells.
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

## Engine equivalence

Not applicable: kitJoin has no estimator. The oracle is the metamorphic layer and hand-made data
with known answers.

## Metamorphic relations

Script: `kitverse/inst/validation/metamorphic.R`. 6 functions × 3 relations; 17 of 18 hold.

| Function | Relations |
|---|---|
| `replace_na_codes` | Idempotent; code order does not matter; non-codes, excluded and text columns unchanged |
| `join_waves` | Row order; full, inner and left joins give the union, intersection and first-file counts; renaming a wave label renames only the suffix |
| `reshape_wider`, `reshape_longer` | Long → wide → long; wide → long → wide (**fails**, see below); a missing wave gives NA, and `drop_na` gives the right count |
| `stack_waves` | Row counts per wave; file order; a column in some files only is NA in the others |
| `build_attrition` | Retained + lost + NA = earlier n; later file order and duplicates; an empty later file makes everyone lost |
| `kits_build` | Score shift −1 lowers every score by exactly 1; row order; NA cells give no row |

The failure: wide → long → wide after shuffling the long rows gives the columns
`id, t1, t3, t2` instead of `id, t1, t2, t3`. Every cell matches by name. `pivot_wider()` orders
new columns by first appearance (`names_sort = FALSE`), so a step that reads columns by position
would break. Open question: sort the new columns, or document the order.

## Report snapshot

Not yet taken. The candidate: the example files in `inst/extdata` through missing codes, join,
reshape and the kits response file.

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
| Faults caught per layer | Metamorphic 0 (1 open question), mutation 2 missing tests; history: unit test 1, r-universe 1, not stated 10 (after release) |
| Faults found after release | 12 (strict: after 2026-11-01), 41 (broad: in any earlier tag) |

## Reproduce

```r
source(system.file("validation", "metamorphic.R", package = "kitverse"))
```
