# kitJoin：KIT 資料串接工具

支援多波次調查資料的**寬格式串接**、**長格式疊加**與**建立流失樣本**，並提供列欄數核對與下載。可適用於 KIT 資料（或其他任何資料）的串接。
為了資料安全性考量，kitJoin 工具目前只提供本機安裝和執行版本。

## 功能

- **Tab 1 上傳與預覽**：多檔分框上傳、移除、排序、波次名稱設定、資料預覽
- **Tab 2 寬格式串接**：自訂 by 變項、join 方法（left / inner / full）、各波後綴；執行完成後可直接下載 CSV / SAV
- **Tab 3 長格式疊加**：垂直疊加各波次（同名欄對齊；型別不一致時自動轉字元）；執行完成後可直接下載 CSV / SAV
- **Tab 4 建立流失樣本**：以前期為基準比對後期 ID，標記流失（0 = 留存，1 = 流失），下載 CSV


## 使用：快速開始

### 方法1. For Windows

最簡單的方法 (only for windows)

1. 從右邊 `Releases` 位置下載 zip，到本機上解壓縮。 
2. 進去後直接點擊 `run.bat` 執行即可。(或名為 `run` 的檔案)

### 方法2. For Mac/Linux/Windows

在 R 上面操作。(R, RStudio, Positron, Terminal,  ~~Colab~~ 都可以)
(建議安裝 R 4.2 以上)

R 安裝連結：https://cran.csie.ntu.edu.tw/

1. 使用 Pak 下載套件（最穩定,自動安裝依賴套件）

```r
install.packages('pak') # 如果還沒裝 pak
pak::pkg_install('jiewenTsai/kitJoin')
```

2. 裝好後在R中直接執行 

```r
kitJoin::run_kitjoin()
```

> 必要套件安裝完成後，瀏覽器會自動開啟本機介面。

<img width="1860" height="984" alt="image" src="https://github.com/user-attachments/assets/4899c7f4-945d-4db2-9f03-f37c8428e671" />





## 使用流程摘要

| 步驟 | 分頁 | 動作 |
|------|------|------|
| 1 | 上傳與預覽 | 按「+ 新增上傳框」依序上傳各波次檔案，設定波次名稱 |
| 2 | 寬格式串接 | 選 by 變項與 join 方法 → 執行串接 → 檢視核對結果 → **下載 CSV / SAV** |
| 3 | 長格式疊加 | 執行疊加 → 檢視核對結果 → **下載 CSV / SAV** |
| 4 | 建立流失樣本 | 選前期 / 後期樣本與 ID 欄位 → 執行 → **下載 CSV** |

> 下載按鈕位於各功能分頁末尾。若下載無反應，請改用 Chrome 或檢查瀏覽器是否封鎖下載。



## 建立流失樣本說明

「建立流失樣本」分頁以 **left join（前期樣本為主體）** 製作流失標籤：

| 情況 | attrition |
|------|-----------|
| 前期 ID 在後期也有 | `0`（留存） |
| 前期 ID 在後期沒有 | `1`（流失） |

- 輸出列數等於前期樣本列數
- 輸出欄位 = 前期全部欄位 + `attrition`
- 後期樣本僅用於 ID 比對，不併入其他變項
- 若後期同一 ID 有多列，會自動 distinct 後再比對


