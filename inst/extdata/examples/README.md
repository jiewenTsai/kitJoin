# 範例資料（CSV + SAV 混合）

| 檔案 | 格式 | 說明 |
|------|------|------|
| `example_wave1.csv` | CSV | 波次 1：`baby_sex`, `int_months`, `pfa0101` |
| `example_wave2.sav` | SAV | 波次 2：`cogc01`, `cogc02`, `lanb01` |
| `example_wave3.csv` | CSV | 波次 3：`socb01`, `health01`, `heigh` |
| `example_wave4.sav` | SAV | 波次 4：`weight`, `growheigh01`, `famc01` |

共同串接鍵：**`release_id`**（50 筆）

## 在 app 中測試

1. `library(joinkit); run_joinkit()`
2. 上傳此資料夾內四個檔案（可混合選取 `.csv` 與 `.sav`）
3. **by 變項**：`release_id`
4. **後綴** 建議：`_t1`, `_t2`, `_t3`, `_t4`
5. **join 方法**：`left_join`（依序合併）

## 取得檔案路徑

```r
example_paths()
```

若缺少 `.sav`，`library(joinkit)` 會依同名的 `.csv` 自動產生（見 `R/zzz.R`）。

維護者也可手動執行：

```bash
Rscript scripts/csv_to_sav.R
```
