# joinkit

KIT 多波次資料串接工具（R Shiny）。執行 `run_joinkit()` 開啟本機介面，上傳 CSV / SAV 後依 by 變項串接並匯出。

## 安裝

```r
install.packages("remotes")
remotes::install_github("jiewenTsai/joinkit")
```


## 使用

```r
library(joinkit)
run_joinkit()
```

在 App 中選取要上傳的檔案即可（無需其他套件函式）。

## 範例資料

clone 本 repo 後，套件根目錄附四個 KIT M36 範例檔，可於 App 中一次選取測試 **CSV + SAV 混合匯入**：

| 檔案 | 說明 |
|------|------|
| `data36.csv` | M36W36 |
| `data48.sav` | M36W48 |
| `data60.sav` | M36W60 |
| `irt36.csv` | M36W36 IRT |

## 功能

- 多檔 CSV / SAV 上傳（單檔上限 500MB）
- **寬格式串接**：自訂 by 變項、join 方法（left / inner / full）、各波後綴與順序
- **長格式疊加**：於上傳分頁設定各檔「後綴或波次名稱」（疊加時作為 wave），垂直疊加（同名欄對齊；型別不一致時自動轉字元）
- 串接後列欄數核對
- 匯出 CSV / SAV

## 授權

MIT
