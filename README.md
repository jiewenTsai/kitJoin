# kitJoin：KIT 資料串接工具

KIT 風格 Shiny App，支援多波次調查資料的**寬格式串接**、**長格式疊加**與 **Attrition 流失標記**，並提供列欄數核對與匯出。

## 功能

- **上傳**：每個上傳框接受一個 CSV 或 SAV 檔案（單檔上限 500 MB），按「+ 新增上傳框」可增加更多
- **Tab 1 上傳與預覽**：多檔分框上傳、移除、排序、波次名稱設定、資料預覽
- **Tab 2 寬格式串接**：自訂 by 變項、join 方法（left / inner / full）、各波後綴
- **Tab 3 長格式疊加**：垂直疊加各波次（同名欄對齊；型別不一致時自動轉字元）
- **Tab 4 Attrition**：以前期為基準比對後期 ID，標記流失（0 = 留存，1 = 流失），匯出 CSV
- **Tab 5 匯出**：下載寬格式串接 / 長格式疊加結果的 CSV 或 SAV

---

## 推薦：用 `runGitHub()` 啟動（無需 clone）

從 GitHub 直接下載並執行 Shiny App，適合一般使用者。

### 步驟

**1. 安裝相依套件**（首次使用時執行一次即可）

```r
install.packages(c(
  "shiny", "bslib", "dplyr", "readr", "haven", "purrr", "tibble"
))
```

**2. 從 GitHub 啟動 App**

```r
shiny::runGitHub("jiewenTsai/kitJoin")
```

根目錄的 `app.R` 會自動啟動 `inst/shiny-app`，**不必**加 `subdir`。

瀏覽器會自動開啟本機介面。

**3. 使用流程摘要**

| 步驟 | 分頁 | 動作 |
|------|------|------|
| 1 | 上傳與預覽 | 按「+ 新增上傳框」依序上傳各波次檔案，設定波次名稱 |
| 2 | 寬格式串接 | 選 by 變項與 join 方法 → 執行串接 → 檢視核對結果 |
| 3 | 長格式疊加 | 執行疊加 → 檢視核對結果 |
| 4 | Attrition | 選前期 / 後期樣本與 ID 欄位 → 執行 → 下載 CSV |
| 5 | 匯出 | 下載寬格式串接 / 長格式疊加結果（建議用 Chrome） |

> 若下載無反應，請改用 Chrome 或檢查瀏覽器是否封鎖下載。

---

## Attrition 說明

「Attrition」分頁以 **left join（前期樣本為主體）** 製作流失標籤：

| 情況 | attrition |
|------|-----------|
| 前期 ID 在後期也有 | `0`（留存） |
| 前期 ID 在後期沒有 | `1`（流失） |

- 輸出列數等於前期樣本列數
- 輸出欄位 = 前期全部欄位 + `attrition`
- 後期樣本僅用於 ID 比對，不併入其他變項
- 若後期同一 ID 有多列，會自動 distinct 後再比對

---

## 其他啟動方式

### 安裝套件後執行

```r
install.packages("remotes")
remotes::install_github("jiewenTsai/kitJoin")
kitJoin::run_joinkit()
```

### 本機開發（clone 後）

```r
setwd("path/to/kitJoin")   # 專案根目錄
shiny::runApp("app.R")
```

或：

```r
devtools::load_all(".")
run_joinkit()
```

---

## 範例資料

clone 本 repo 後，套件根目錄附四個 KIT M36 範例檔，可於 App 中分框上傳測試：

| 檔案 | 說明 |
|------|------|
| `data36.csv` | M36W36 |
| `data48.sav` | M36W48 |
| `data60.sav` | M36W60 |
| `irt36.csv` | M36W36 IRT |

## 授權

MIT
