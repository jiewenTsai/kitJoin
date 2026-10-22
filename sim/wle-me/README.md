# WLE 測量誤差模擬：天真 WLE vs brms `se()`/`me()`/`mi()` vs Rasch 似然

## 研究問題

第一階段用 Rasch 算出 WLE 與 WLE.se 後，次級分析時：

- **θ 當結果變數**：`brm(wle | se(wle_se, sigma = TRUE) ~ x)` 和天真的 `lm(wle ~ x)` 差多少？
- **θ 當預測變數**：`brm(y ~ me(wle, wle_se) + x1)` 和天真的 `lm(y ~ wle + x1)` 差多少？

先前單次試跑看到的跡象（還需要這個模擬來確認）：

1. 用個人 WLE.se 的 `se()`，斜率反而偏向 0。
2. `me()` 會修正過頭。
3. `me()` 的潛在 θ 和 x1 獨立，所以 x1 的係數有偏誤。

可能的原因：WLE.se 是原始分數的函數，和 θ、和誤差本身都有關聯，並不是 brms 假設的外生已知 SE。

## 設計

| 因子 | 水準 |
|---|---|
| 題數 `I`（難度在 [-2, 2] 等距） | 10, 20, 40 |
| `r_tx` = cor(θ, x1)（只影響預測變數情境） | 0, 0.5 |
| 樣本數 N | 1000（固定） |
| 真值 | β(x→θ) = 0.5、θ 的殘差 SD = √0.75；β(θ→y) = 0.5、β(x1→y) = 0.3 |

每次複製會先用 `TAM` 做 Rasch 校準並算 WLE（第一階段），再比較以下方法。

**θ 當結果變數**（`wle ~ x`）

| 代號 | 方法 |
|---|---|
| `oracle` | `lm(theta ~ x)`，用真實 θ |
| `naive` | `lm(wle ~ x)` |
| `se_het` | `brm(wle \| se(wle_se, sigma = TRUE) ~ x)` |
| `se_con` | 同上，但所有人的 SE 都換成 √mean(SE²) |
| `tam_lr` | TAM 潛在迴歸，直接用題目反應（金標準） |

**θ 當預測變數**（`y ~ θ + x1`）

| 代號 | 方法 |
|---|---|
| `oracle` | `lm(y ~ theta + x1)` |
| `naive` | `lm(y ~ wle + x1)` |
| `me_het` / `me_con` | `brm(y ~ me(wle, SE) + x1)`，分別用個人 SE 和常數 SE |
| `mi_het` / `mi_con` | `bf(wle \| mi(SE) ~ x1) + bf(y ~ mi(wle) + x1)`，潛在 θ 以 x1 為條件 |
| `rasch` | 題目難度固定在第一階段估計值，θ \| x1 與 y 聯合估計（即 MUPPET 第二階段，Stan） |

評估指標：相對偏誤、經驗 SD、平均 SE（SE/SD 比）、RMSE、95% 區間涵蓋率，以及 R-hat 和 divergence。

## 在 Ubuntu 上安裝

```bash
sudo apt update
sudo apt install -y r-base r-base-dev build-essential libcurl4-openssl-dev libssl-dev libxml2-dev libfontconfig1-dev tmux
```

```bash
Rscript -e 'install.packages(c("SimDesign","TAM","brms","posterior","dplyr","ggplot2"), repos="https://cloud.r-project.org")'
```

```bash
Rscript -e 'install.packages("cmdstanr", repos=c("https://stan-dev.r-universe.dev","https://cloud.r-project.org")); cmdstanr::install_cmdstan(cores = 4)'
```

測試時使用的版本：R 4.5.3、brms 2.23.0、SimDesign 2.25。

## 執行

把整個資料夾（`sim_wle_me.R`、`summarise_wle_me.R`、`README.md`）複製到 Ubuntu，然後在該資料夾內操作。

**1. 煙霧測試**：每個條件 2 次，用 2 核。第一次會先編譯 4 個 Stan 模型，約需幾分鐘。

```bash
Rscript sim_wle_me.R 2 2
```

這一步要確認三件事：沒有 `FATAL_TERMINATION`；有產生 `wle_me_sim_R2.rds`；SimDesign 印出的每個條件耗時。用這個耗時乘以 100，就能估計 200 次複製的總時間。

**2. 正式執行**：在 tmux 裡跑，關掉 SSH 也不會中斷。

```bash
tmux new -s wle
```

```bash
Rscript sim_wle_me.R 200 20 2>&1 | tee sim.log
```

- 第二個參數是平行的 worker 數。每個 worker 依序跑 2 條鏈，只佔 1 核，所以設成「實體核心數 − 1」即可。
- 離開 tmux 用 `Ctrl-b d`，回來用 `tmux attach -t wle`。

**3. 中斷後續跑**：在同一個資料夾、同一台機器上重跑同一行指令。SimDesign 會從 `SIMDESIGN-TEMPFILE_<hostname>.rds` 接續，已完成的條件不會重算。

**4. 彙整**：

```bash
Rscript summarise_wle_me.R wle_me_sim_R200.rds
```

會產生以下檔案：

- `results/summary.csv`：所有指標
- `results/diagnostics.csv`：R-hat 與 divergence
- `results/fig_outcome.png`、`results/fig_predictor.png`：相對偏誤和涵蓋率的點圖

## 注意事項

- SimDesign 不會覆寫既有輸出。如果 `wle_me_sim_R200.rds` 已經存在，新檔會叫 `wle_me_sim_R200-1.rds`，彙整時記得指定正確的檔名。
- cmdstan 每次抽樣都會寫 CSV，其中含 N 個潛在值的抽樣。程式讀完就刪除，不會塞滿 `/tmp`。
- SimDesign 把任何 NA 結果視為失敗並重抽，所以沒有 SE 的統計量（例如 lm 的 σ）只存點估計。
- `stan/` 資料夾是自動產生的 Stan 程式與執行檔，可以刪除，下次執行會重建。
- 要帶回來給我看的檔案：`results/` 整個資料夾，以及 `wle_me_sim_R200.rds`（含每次複製的原始結果）。
