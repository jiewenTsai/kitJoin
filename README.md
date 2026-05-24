# joinkit

KIT 多波次資料串接工具（R Shiny）。安裝後在本機執行 `run_joinkit()` 即可使用。

## 安裝

將 `YOUR_GITHUB_USER` 改為你的 GitHub 使用者名稱或組織後：

```r
install.packages("remotes")
remotes::install_github("YOUR_GITHUB_USER/joinkit")
```

本地開發：

```r
devtools::install("/path/to/joinkit")
```

## 使用

```r
library(joinkit)
run_joinkit()
```

## 範例資料（CSV + SAV 混合匯入）

套件內建四個範例檔，示範 **同時上傳 CSV 與 SAV**：

```r
example_paths()
# wave1_csv  → example_wave1.csv
# wave2_sav  → example_wave2.sav
# wave3_csv  → example_wave3.csv
# wave4_sav  → example_wave4.sav
```

在 Shiny 中一次選取上述四個檔案，**by 變項** 設為 `release_id`，後綴可設 `_t1`～`_t4`，再執行串接。

說明見 [`inst/extdata/examples/README.md`](inst/extdata/examples/README.md)。

## 功能

- 多檔 CSV / SAV 上傳（單檔上限 500MB）
- 自訂 by 變項、join 方法、各波後綴與順序
- 串接後列欄數核對
- 匯出 CSV / SAV

## 開發

```r
devtools::load_all()
run_joinkit()
```

發布前可重新產生 SAV 範例：

```bash
cd joinkit
Rscript scripts/csv_to_sav.R
```

## 授權

MIT
