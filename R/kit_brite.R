# KIT 的 neo-brutalism 樣式（kitJoin 與 kitsUI 共用同一份；改的時候兩邊一起改）
#
# 以 Bootswatch Brite 為底（kit_theme()），規則：
# - 黑框 2px、零模糊的位移陰影（卡片 3px、按鈕 2px，和圓體字的筆畫粗細相稱）、
#   平塗高彩度色塊配黑字；米白底、圓角 8px。
# - 不要框中有框：只有頂層元素（頁首、提示框、狀態框、圖表、按鈕）有框和陰影，裡面的東西一律平的。
# - 主角與配角：每一頁的主要動作（.btn-primary）、結果的狀態框、小節標題、資料表與圖是主角；
#   「這一頁做什麼」、提示小字、次要按鈕（下載、全選…）是配角，變小、變淡、不加框。
# - 給 40、50 歲的使用者：內文 17px，內容最寬 1140px 置中。

#' 共用的 neo-brutalism CSS（接在 kit_css() 後面，覆蓋前面的設定）
#' @noRd
kit_css_brite <- function() {
  r"---(:root {
  --nb-ink: #141414;
  --nb-page: #faf7f0;
  --nb-card: #ffffff;
  --nb-lime: #c6d033;     /* KIT 點綴色，當主要色塊 */
  --nb-green: #2d7535;    /* KIT 主色，用在連結與小標籤 */
  --nb-sky: #a8dcff;
  --nb-yellow: #ffde59;
  --nb-salmon: #ff9b8f;
  --nb-mint: #b9f0a8;
  --nb-border: 2px solid var(--nb-ink);
  --nb-shadow: 3px 3px 0 0 var(--nb-ink);
  --nb-shadow-sm: 2px 2px 0 0 var(--nb-ink);
  --nb-radius: 8px;
}

body { background: var(--nb-page); }
h1, h2, h3, h4, h5, h6 { font-weight: 700; }
a { color: var(--nb-green); text-decoration-thickness: 2px; text-underline-offset: 2px; }

/* ── 頁首：萊姆色塊＋黑字 ───────────────────────────────────────── */
.kit-header {
  background: var(--nb-lime);
  color: var(--nb-ink);
  border: var(--nb-border);
  border-radius: var(--nb-radius);
  box-shadow: 4px 4px 0 0 var(--nb-ink);
}
.kit-header h1 { color: var(--nb-ink); font-weight: 700; }
.kit-header p,
.kit-subtitle,
.kit-app-meta { color: var(--nb-ink); opacity: 1; }
.kit-subtitle { font-weight: 700; }

/* ── 分頁：一排可以按的方塊，不要大外框 ─────────────────────────── */
.nav-tabs {
  border: 0;
  border-radius: 0;
  padding: 0 0 6px;
  background: transparent;
  gap: 0.6rem;
}
.nav-tabs .nav-link,
.nav-tabs .nav-link:hover {
  border: var(--nb-border);
  border-radius: var(--nb-radius);
  margin: 0;
  background: var(--nb-card);
  color: var(--nb-ink);
  box-shadow: var(--nb-shadow-sm);
  font-weight: 700;
  transition: transform 80ms ease, box-shadow 80ms ease;
}
.nav-tabs .nav-link:hover {
  transform: translate(1px, 1px);
  box-shadow: 1px 1px 0 0 var(--nb-ink);
}
.nav-tabs .nav-link.active,
.nav-tabs .nav-link.active:hover {
  background: var(--nb-lime);
  border-color: var(--nb-ink);
  transform: translate(2px, 2px);
  box-shadow: none;
}
.tab-content,
.tab-content .tab-content {
  border: 0;
  border-radius: 0;
  padding: 1.25rem 0 0;
  background: transparent;
  box-shadow: none;
}
/* 頁面裡面的小分頁（例如垂直連結圖的三個分頁）小一號 */
.tab-content .nav-tabs .nav-link { padding: 0.25rem 0.8rem; font-size: 0.9rem; }

/* ── 頂層卡片：說明框、收合框、提示框、狀態框 ───────────────────── */
.kit-guide,
.kit-fold-box,
.kit-callout,
.kit-callout-warn,
.kit-callout-warning,
.alert {
  border: var(--nb-border);
  border-radius: var(--nb-radius);
  box-shadow: var(--nb-shadow);
  color: var(--nb-ink);
}
.kit-guide { background: var(--nb-card); }
.kit-fold-box { background: var(--nb-card); }
.kit-callout,
.alert-info { background: var(--nb-sky); }
.kit-callout-warn,
.kit-callout-warning,
.alert-warning,
.alert.kit-cloud-banner { background: var(--nb-yellow); color: var(--nb-ink); border: var(--nb-border); }
.alert-success { background: var(--nb-mint); }
.alert-danger { background: var(--nb-salmon); }
.alert a, .kit-callout a { color: var(--nb-ink); font-weight: 700; }

/* 卡片裡面的東西一律平的：不再疊框、不再疊陰影 */
.kit-guide .kit-callout, .kit-guide .alert,
.alert .kit-callout, .kit-callout .alert,
.kit-fold-box .kit-callout, .kit-fold-box .alert, .kit-fold-box .kit-fold-box {
  border: 0;
  box-shadow: none;
  border-radius: 0;
  padding: 0.25rem 0;
  background: transparent;
}

/* ── 圖表：白色卡片 ─────────────────────────────────────────────── */
.js-plotly-plot,
.shiny-plot-output {
  background: var(--nb-card);
  border: var(--nb-border);
  border-radius: var(--nb-radius);
  box-shadow: var(--nb-shadow);
  overflow: hidden;
}
/* 圖的下面留白：.kit-hint 預設有負的上邊距（貼近上面的輸入框），接在圖下面時會壓到 x 軸 */
.html-widget-output,
.shiny-plot-output {
  margin-bottom: 1.2rem;
}
.html-widget-output + .kit-hint,
.shiny-plot-output + .kit-hint {
  margin-top: 0;
}

/* ── 表格：黑色表頭，細黑分隔線，沒有陰影 ─────────────────────── */
.table {
  border: var(--nb-border);
  --bs-table-bg: var(--nb-card);
  --bs-table-border-color: var(--nb-ink);
  --bs-table-striped-bg: #f3f0e6;
}
.table > thead > tr > th,
.table > thead > tr > td {
  background: var(--nb-ink);
  color: #fff;
  border-bottom: 0;
}
.table > :not(caption) > * > * { border-bottom-width: 1.5px; }

/* 程式碼：米白底、黑框，沒有陰影（它通常在收合框裡） */
.kit-code-block,
.kit-code-panel,
.well, pre {
  border: 2px solid var(--nb-ink);
  border-radius: var(--nb-radius);
  box-shadow: none;
  background: #fffdf5;
}

/* ── 按鈕：色塊＋黑框＋硬陰影；滑過時陷一半，按下時陷到底 ─────── */
.btn {
  border: var(--nb-border);
  border-radius: var(--nb-radius);
  color: var(--nb-ink);
  font-weight: 700;
  box-shadow: var(--nb-shadow-sm);
  transition: transform 80ms ease, box-shadow 80ms ease;
}
/* shiny 的 actionButton() 一律帶 .btn-default，所以白底寫在前面，萊姆色寫在後面才蓋得過 */
.btn-default, .btn-secondary, .btn-outline-primary, .btn-outline-secondary,
.btn-light, .btn-file {
  background: var(--nb-card); color: var(--nb-ink); border-color: var(--nb-ink);
}
.btn-primary, .btn-success { background: var(--nb-lime); color: var(--nb-ink); border-color: var(--nb-ink); }
.btn-danger { background: var(--nb-salmon); color: var(--nb-ink); border-color: var(--nb-ink); }
.btn:hover {
  transform: translate(1px, 1px);
  box-shadow: 1px 1px 0 0 var(--nb-ink);
  color: var(--nb-ink);
}
.btn-primary:hover, .btn-success:hover { background: var(--nb-lime); }
.btn:active,
.btn.active {
  transform: translate(2px, 2px);
  box-shadow: 0 0 0 0 var(--nb-ink);
}
.btn:disabled, .btn.disabled {
  box-shadow: none;
  transform: none;
  background: #e8e4d8;
  color: #6b6b6b;
  border-color: #6b6b6b;
}

/* ── 輸入框：黑框、白底，沒有陰影；聚焦時才出現硬陰影 ────────── */
.form-control,
.form-select,
.selectize-input,
.selectize-control.single .selectize-input {
  border: var(--nb-border);
  border-radius: var(--nb-radius);
  background: var(--nb-card);
  box-shadow: none;
}
.form-control:focus,
.form-select:focus,
.selectize-input.focus {
  border-color: var(--nb-ink);
  box-shadow: 2px 2px 0 0 var(--nb-ink);
}
.selectize-dropdown {
  border: var(--nb-border);
  border-radius: var(--nb-radius);
  box-shadow: var(--nb-shadow-sm);
}
.selectize-dropdown .active { background: var(--nb-lime); color: var(--nb-ink); }

/* 鍵盤焦點：黑框上看不到預設的焦點框，另外畫一圈藍色 */
.btn:focus-visible,
.nav-link:focus-visible,
a:focus-visible,
input:focus-visible,
summary:focus-visible {
  outline: 3px solid #0044ff;
  outline-offset: 3px;
}

/* 上傳進度條：細的平塗條 */
.progress,
.shiny-file-input-progress {
  border: 2px solid var(--nb-ink);
  border-radius: 3px;
  box-shadow: none;
  background: var(--nb-card);
  height: 0.7rem;
}
.progress-bar { background: var(--nb-green); border-radius: 0; }

@media (prefers-reduced-motion: reduce) {
  .btn, .nav-tabs .nav-link { transition: none; }
}

/* ── 字級與版面寬度 ────────────────────────────────────────────────
   整體字級放大：rem 的基準 16px → 17px，內文用 1rem（原本 Brite 的內文是 0.875rem = 14px），
   提示、表格這些用 rem 寫的小字也跟著等比例放大。
   版面寬度：內容最寬 1140px、置中，寬螢幕上左右留白，不再拉滿整個視窗。 */
html { font-size: 17px; }
body { font-size: 1rem; line-height: 1.65; }
.container-fluid {
  max-width: 1140px;
  margin-left: auto;
  margin-right: auto;
  padding-left: 2rem;
  padding-right: 2rem;
}
@media (max-width: 768px) {
  .container-fluid { padding-left: 1rem; padding-right: 1rem; }
}

/* ══ 主角與配角（給 40、50 歲的使用者：一眼看出現在該按什麼、結果是什麼）══════
   主角：每一頁的主要動作（.btn-primary，例如「開始校準」「執行串接」「下一步」）、
         結果的狀態框（.alert）、小節標題、資料表與圖。
   配角：「這一頁做什麼」說明、提示小字（.kit-hint）、次要按鈕（下載、全選、範例檔）。
   配角變小、變淡、不加框；主角變大、加粗、有硬陰影。 */

/* 主要動作：大按鈕，萊姆色，這一頁最醒目的東西 */
.btn-primary {
  font-size: 1.1rem;
  padding: 0.65rem 1.6rem;
  box-shadow: var(--nb-shadow);
}
.btn-primary:hover { box-shadow: 1px 1px 0 0 var(--nb-ink); transform: translate(2px, 2px); }

/* 次要按鈕：白底、細框、沒有陰影、小一號（下載、全選、範例檔…） */
.btn-sm,
.btn-outline-primary,
.btn-outline-secondary,
.kit-download {
  font-size: 0.85rem;
  font-weight: 500;
  padding: 0.3rem 0.8rem;
  border-width: 1.5px;
  box-shadow: none;
  background: var(--nb-card);
}
.btn-sm:hover,
.btn-outline-primary:hover,
.btn-outline-secondary:hover,
.kit-download:hover {
  transform: none;
  box-shadow: none;
  background: #f1eee4;
}

/* 「這一頁做什麼」：退成配角，沒有卡片框，小字、淡色，用一條細線和內容分開 */
.kit-guide {
  background: transparent;
  border: 0;
  border-bottom: 1.5px solid #d9d4c5;
  border-radius: 0;
  box-shadow: none;
  padding: 0 0 0.9rem;
  margin-bottom: 1.6rem;
  font-size: 0.88rem;
  color: #4d4d4d;
}
.kit-guide strong:first-child { color: var(--nb-ink); }

/* 提示小字：小、淡，但對比仍夠（#5a5a5a 在米白底上約 6.6:1） */
.kit-hint,
.kit-guide-note,
.form-text {
  font-size: 0.82rem;
  color: #5a5a5a;
}

/* 結果的狀態框：主角，大字、粗體 */
.alert.kit-status,
.kit-status {
  font-size: 1.08rem;
  font-weight: 700;
  padding: 1rem 1.25rem;
}
.kit-status ul, .kit-status p { font-weight: 400; font-size: 0.95rem; }

/* 小節標題：大、粗，上方留白，讓頁面分段清楚 */
.tab-pane h4 {
  font-size: 1.35rem;
  margin-top: 2.4rem;
  margin-bottom: 0.7rem;
}
.tab-pane h5 {
  font-size: 1.1rem;
  margin-top: 1.6rem;
  margin-bottom: 0.5rem;
}

/* 資料表：字不要太小、列高寬鬆 */
.table { font-size: 0.95rem; }
.table > :not(caption) > * > * { padding: 0.55rem 0.75rem; }

/* 收合區塊（「用 R 怎麼做」、「技術說明」…）：配角，小字、灰色標題 */
details > summary {
  font-size: 0.9rem;
  color: #4d4d4d;
}

/* 窄螢幕：分頁不要一顆一行往下堆，改成一排可以左右滑 */
@media (max-width: 768px) {
  .nav-tabs {
    flex-wrap: nowrap;
    overflow-x: auto;
    padding-bottom: 8px;
  }
  .nav-tabs .nav-link { white-space: nowrap; }
}

/* ── 新元件：步驟卡、檢查清單、細節列（kitJoin ⑥ 起） ── */
/* 表格跟著內容寬度，最寬到版面寬度（KIT 設計語言 3.8） */
.table { width: auto; max-width: 100%; }

/* 步驟卡：一頁裡的幾個步驟。現在這一步有框和陰影；做完的收成一行；還不能做的是灰的 */
.kit-stepcard {
  border: 2px solid var(--nb-ink);
  border-radius: 8px;
  background: var(--nb-card);
  margin-bottom: 1rem;
}
.kit-stepcard:has(.kit-stephead.is-current) { box-shadow: 3px 3px 0 var(--nb-ink); }
.kit-stepcard:has(.kit-stephead.is-done) { background: transparent; border-color: #9a9a9a; }
.kit-stepcard:has(.kit-stephead.is-locked) { background: transparent; border-color: #c9c3b6; }
.kit-stephead { display: flex; align-items: center; gap: .8rem; padding: .8rem 1.1rem; flex-wrap: wrap; }
.kit-stepnum {
  flex: none; width: 2rem; height: 2rem; border-radius: 50%;
  border: 2px solid var(--nb-ink); background: var(--nb-card);
  display: flex; align-items: center; justify-content: center; font-weight: 700;
}
.kit-stephead.is-current .kit-stepnum { background: var(--nb-lime); }
.kit-stephead.is-done .kit-stepnum { background: var(--nb-mint); }
.kit-stephead.is-locked .kit-stepnum { border-color: #c9c3b6; color: #8a8a8a; }
.kit-steptitle { font-weight: 700; font-size: 1.1rem; }
.kit-stephead.is-locked .kit-steptitle { color: #8a8a8a; }
.kit-stepsum { flex: 1; color: #4d4d4d; font-size: .92rem; }
.kit-stephead.is-locked .kit-stepsum { color: #8a8a8a; font-size: .85rem; }
.kit-stepedit { font-size: .9rem; font-weight: 700; }
.kit-stepbody { padding: .2rem 1.1rem 1.1rem 3.9rem; }
.kit-stepbody > p:first-child { margin-top: 0; }
@media (max-width: 768px) { .kit-stepbody { padding-left: 1.1rem; } }
/* 卡片裡的表不再加外框（框不疊框） */
.kit-stepbody .table { border: 0; margin-bottom: .4rem; }
.kit-stepbody .table td { vertical-align: middle; }
.kit-stepbody .kit-table-scroll { border: 0; box-shadow: none; }
td.kit-guess .selectize-input { background: #fffbe6; }

/* 檢查清單：一列一件事；! 要決定、✓ 沒問題、· 提醒；按鈕在說明下面 */
.kit-checklist { list-style: none; padding: 0; margin: 0 0 .6rem; }
.kit-check { display: flex; gap: 1rem; align-items: flex-start; padding: .85rem 0;
  border-top: 1px solid #e3ded2; }
.kit-check:first-child { border-top: 0; }
.kit-check-mark { flex: none; width: 1.6rem; font-weight: 900; font-size: 1.1rem; text-align: center; }
.kit-check.todo .kit-check-mark { color: #d9730d; }
.kit-check.ok .kit-check-mark { color: var(--bs-success); }
.kit-check.note .kit-check-mark { color: #5a5a5a; }
.kit-check-text { flex: 1; }
.kit-check-text > strong { display: block; }
.kit-check.ok .kit-check-text > strong { font-weight: 500; }
.kit-check-text > span { display: block; font-size: .92rem; color: #4d4d4d; }
.kit-check-actions { display: flex; gap: .5rem; flex-wrap: wrap; align-items: center; margin-top: .6rem; }
.kit-check-actions .form-group { margin-bottom: 0; }
.kit-problems { color: #8a2a00; font-size: .95rem; margin: .6rem 0; }
.kit-problems ul { margin: .3rem 0 0; }

/* 主要按鈕上面的一行：還差什麼 */
.kit-ready { font-size: .95rem; margin: 1.4rem 0 .5rem; }
.kit-ready.todo { color: #8a4a00; font-weight: 700; }
.btn-primary:disabled, .btn-primary[disabled] { opacity: .5; box-shadow: none; }
.kit-next-link { font-weight: 700; margin-left: .8rem; }
.kit-downloads { display: flex; align-items: center; flex-wrap: wrap; gap: .6rem; margin: .6rem 0 1.4rem; }

/* 細節列：結果或一步下面一排收起來的區塊，一行小字標題 */
.kit-details { display: flex; flex-wrap: wrap; gap: .4rem 1.6rem; margin: 1rem 0;
  padding-top: .8rem; border-top: 1px solid #d8d2c4; }
.kit-detail[open] { flex-basis: 100%; }
.kit-detail > summary { font-size: .9rem; color: #4d4d4d; cursor: pointer; }
.kit-detail-body { padding: .6rem 0 .4rem; }

/* 一排小控制項（標籤在上） */
.kit-inline { display: flex; flex-wrap: wrap; gap: .4rem 1rem; align-items: flex-end; margin: .4rem 0; }
.kit-inline .form-group, .kit-inline .shiny-input-container { margin-bottom: .3rem; }

/* 逐題表 */
.kit-items td { font-size: .92rem; }
.kit-items .kit-flag { color: #8a4a00; font-size: .85rem; }
.kit-items tr.kit-row-off td { color: #9a9a9a; }
.kit-muted { color: #8a8a8a; }
.kit-bulk { margin-top: .6rem; }
)---"
}
