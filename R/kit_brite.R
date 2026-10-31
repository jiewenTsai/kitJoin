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
  --nb-green: #006835;    /* KIT 主色＝標誌的深綠，用在連結與小標籤 */
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
  border: 1px solid var(--nb-ink);
  border-radius: 3px;
  box-shadow: none;
  background: var(--nb-card);
  height: 0.5rem;
}
/* 檔案輸入：不拉滿整頁（最寬約一個長檔名） */
.shiny-input-container:has(.btn-file) { max-width: min(34rem, 100%); }
.progress-bar { background: var(--nb-green); border-radius: 0; }
/* 檔案上傳的進度條：Shiny 設的寬度換算後只剩幾 px，看起來像沒在跑。
   改成：上傳中（.active）是流動的斜紋條；傳完整條填滿 */
.shiny-file-input-progress { height: 1.1rem; }
.shiny-file-input-progress .progress-bar {
  width: 100% !important; font-size: 0.75rem; color: #fff; font-weight: 700;
}
.shiny-file-input-progress.active .progress-bar {
  background: repeating-linear-gradient(-45deg, var(--nb-green) 0 10px, var(--nb-mint) 10px 20px);
  background-size: 28px 28px;
  animation: kit-upload 0.8s linear infinite;
}
@keyframes kit-upload { to { background-position: 28px 0; } }

@media (prefers-reduced-motion: reduce) {
  .btn, .nav-tabs .nav-link { transition: none; }
  .shiny-file-input-progress.active .progress-bar { animation: none; }
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
/* 下載是這一頁的主角時（例如作答檔）：和主要按鈕一樣 */
.kit-download.btn-primary {
  font-size: 1.1rem; font-weight: 700; padding: 0.55rem 1.4rem; border-width: 2px;
  background: var(--nb-lime); box-shadow: var(--nb-shadow);
}
.kit-download.btn-primary:hover { background: var(--nb-lime); transform: translate(1px, 1px); box-shadow: 1px 1px 0 0 var(--nb-ink); }

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

/* ── 新元件：狀態框 ── */
/* 第一行是標籤＋一句結論（大字粗體）；條列最多 3 點，其他收在「還有 N 點」；
   停下來時的「怎麼辦」連結；原始訊息收起來 */
.kit-status-line { font-size: 1.08rem; font-weight: 700; }
.kit-status ul { margin: 0.45rem 0 0; }
.kit-status details { margin-top: 0.4rem; font-weight: 400; }
.kit-status details > summary { font-size: 0.85rem; color: var(--nb-ink); }
.kit-status-action { margin-top: 0.5rem; font-size: 0.95rem; }
.kit-status-notes { font-weight: 400; font-size: 0.88rem; margin-top: 0.4rem; }
.kit-status-raw code { display: block; white-space: pre-wrap; font-size: 0.85rem; }
.kit-status .kit-status-line { margin-bottom: 0.2rem; }
.kit-status .btn { margin-top: 0.4rem; }
.kit-status details { font-weight: 400; }

/* ── 新元件：進階設定、細節、設定摘要、下一步 ── */
/* 進階設定：預設收起，一行小字標題；打開後左邊一條淡線，和其他設定分開（不加框） */
.kit-advanced { margin: 0.8rem 0 0.4rem; }
.kit-advanced > summary { font-size: 0.9rem; color: #4d4d4d; cursor: pointer; }
.kit-advanced-body { margin: 0.5rem 0 0.8rem; padding-left: 1rem; border-left: 1.5px solid #d9d4c5; }
/* 一排細節：結果下面一排收起來的標題（▸ 預覽　▸ 用 R 怎麼做…）；
   打開的那一個佔一整行，內容不加框 */
.kit-details {
  display: flex; flex-wrap: wrap; align-items: baseline; gap: 0.3rem 1.6rem; margin: 1.6rem 0 1rem;
}
.kit-details > .shiny-html-output { display: contents; }
.kit-details .kit-detail[open] { flex-basis: 100%; }
.kit-detail > summary { font-size: 0.9rem; color: #4d4d4d; cursor: pointer; }
.kit-detail-body { padding: 0.6rem 0 1rem; }
.kit-detail-body h6 { margin-top: 0.8rem; }
/* 設定摘要：結果（或主要按鈕）上面一行小字 */
.kit-settings-line { font-size: 0.85rem; color: #5a5a5a; margin: 1.2rem 0 0.6rem; }
/* 下一步：頁面最下面、靠右：一句小字＋「下一步：××× →」 */
.kit-next {
  display: flex; flex-wrap: wrap; justify-content: flex-end; align-items: center; gap: 0.6rem 1rem;
  margin: 2.4rem 0 1rem;
}
.kit-next .kit-hint { margin: 0; flex: 1 1 22rem; }
.kit-next .kit-hint ul { margin: 0.3rem 0 0.3rem 1.2rem; padding: 0; }
/* 下載區：次要按鈕排成一排 */
.kit-downloads .btn { margin: 0 0.5rem 0.4rem 0; }
/* 上傳是這一頁的主要動作時，「選擇檔案…」畫成主要按鈕 */
.kit-file-primary .btn-file {
  background: var(--nb-lime); font-size: 1.1rem; box-shadow: var(--nb-shadow);
}

/* ── 新元件：用 R 怎麼做 ── */
/* 一行小字的收合標題（.kit-detail）；打開後只有程式碼本身有框：
   版本名稱與「複製」在同一行、沒有框，下面的 <pre> 才有框 */
.kit-code-head {
  display: flex; justify-content: flex-start; align-items: center; gap: 0.6rem;
  margin: 0.9rem 0 0.35rem; border: 0; background: transparent;
}
.kit-detail-body pre {
  font-size: 0.85rem; padding: 0.75rem; margin: 0 0 0.6rem; white-space: pre; overflow-x: auto;
}
.kit-detail-body pre code { padding: 0; background: none; color: inherit; }
/* kitsUI：一個版本時，「複製」靠右放在程式碼框上面 */
.kit-code-copy { display: flex; justify-content: flex-end; margin-bottom: 0.3rem; }

/* ── 新元件：步驟分頁的進度 ── */
/* 做完的分頁打 ✓；還不能用的分頁灰色（滑過時 title 說明要先做什麼）；
   要重新執行時 ✓ 拿掉（kit-tab-stale 沒有額外的樣子） */
/* kitJoin 的分組分頁用 ::after 畫 ✓；kitsUI 的分頁裡已經有 .kit-tab-mark，不要再畫一個 */
.nav-tabs .nav-link.kit-tab-done::after { content: none; }
.kit-navgroups .nav-link.kit-tab-done::after { content: " ✓"; color: var(--nb-green); font-weight: 700; }
.kit-navgroups .nav-link.active.kit-tab-done::after { color: var(--nb-ink); }
/* kitsUI 的 ✓ 是分頁裡的 .kit-tab-mark（kit_tabs_js()）；kitJoin 用 ::after */
.nav-tabs .nav-link .kit-tab-mark { margin-left: 0.35rem; font-weight: 900; }
.nav-tabs .nav-link.kit-tab-locked:not(.active):hover { transform: none; box-shadow: none; }
.nav-tabs .nav-link.kit-tab-locked:not(.active) {
  background: #ece8dc; color: #6b6b6b; border-color: #8a8a8a; box-shadow: none;
}

/* ── 新元件：分頁分組 ── */
/* 分頁分成三組，不編號：準備 → 做出資料（選一個）→ 其他工具；每組上面一行灰色小字。
   其他工具（獨立的小工具）用虛線框、沒有陰影 */
.kit-navgroups {
  display: flex; flex-wrap: wrap; gap: 0.8rem 1.6rem; align-items: flex-end; margin: 0 0 1.2rem;
}
.kit-navgroup-label { font-size: 0.82rem; color: #5a5a5a; margin-bottom: 0.3rem; }
.kit-navgroup .nav-tabs { gap: 0.5rem; flex-wrap: wrap; }
.kit-navgroup .nav-link { padding: 0.45rem 0.9rem; }
.kit-navgroup.tools .nav-link,
.kit-navgroup.tools .nav-link:hover { border-style: dashed; box-shadow: none; }
.kit-navgroup.tools .nav-link.active { transform: none; }
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
.kit-file-table td.kit-guess .selectize-control .selectize-input { background: #fffbe6; }

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


/* 一排小控制項（標籤在上） */
.kit-inline { display: flex; flex-wrap: wrap; gap: .4rem 1rem; align-items: flex-end; margin: .4rem 0; }
.kit-inline .form-group, .kit-inline .shiny-input-container { margin-bottom: .3rem; }

/* 逐題表 */
.kit-items td { font-size: .92rem; }
.kit-items .kit-flag { color: #8a4a00; font-size: .85rem; }
.kit-items tr.kit-row-off td { color: #9a9a9a; }
.kit-muted { color: #8a8a8a; }
.kit-bulk { margin-top: .6rem; }
/* 第 2 步：像題目的欄一組一列（勾選、題目內容、領域） */
.kit-groups td { vertical-align: middle; }
.kit-groups .kit-group-label { font-size: .92rem; max-width: 22rem; }
.kit-groups input[type="checkbox"] { width: 1.25rem; height: 1.25rem; }
.kit-groups .form-group { margin-bottom: 0; }

/* ── 新元件：圖解（前 → 後） ── */
/* 「這一頁做什麼」下面的小圖解（kit_diagram()）：幾張小表 → 結果，下面一行白話說明。
   配角：不加框、表格跟著內容寬、1px 細線、淡色表頭（不是大資料表的黑表頭）；
   新加的欄（wave、attrition）淡天藍底，欄名後面寫「（新）」，不只靠顏色；
   空的格子寫灰色的「（空）」。窄螢幕時「前」在上、箭頭朝下、「後」在下。 */
.kit-guide-wrap > .kit-guide-page { margin: 0; }
/* 外層是 .kit-guide 時，「這一頁做什麼」那一行維持原本的小字灰色（不套用說明框裡收合標題的綠色） */
.kit-guide-wrap > .kit-guide-page > summary { font-size: 0.9rem; color: #4d4d4d; }
.kit-diagram { margin: 0.8rem 0 0.1rem; }
.kit-dg-flow { display: flex; flex-wrap: wrap; align-items: center; gap: 0.5rem 1.1rem; }
.kit-dg-before { display: flex; flex-wrap: wrap; align-items: flex-start; gap: 0.5rem 1.1rem; }
.kit-dg-arrow { font-size: 1.35rem; line-height: 1; color: var(--nb-ink); }
.kit-dg-table {
  border-collapse: collapse; width: auto; margin: 0;
  font-size: 0.82rem; line-height: 1.35; color: var(--nb-ink); background: var(--nb-card);
}
.kit-dg-table caption {
  caption-side: top; padding: 0 0 0.2rem; text-align: left;
  font-size: 0.82rem; color: #5a5a5a;
}
.kit-dg-table th, .kit-dg-table td {
  border: 1px solid var(--nb-ink); padding: 0.12rem 0.55rem; white-space: nowrap; text-align: center;
}
.kit-dg-table thead th { background: var(--nb-page); font-weight: 700; }
.kit-dg-table td { font-family: var(--bs-font-monospace); }
.kit-dg-table .kit-dg-added { background: var(--nb-sky); }
.kit-dg-new { font-weight: 400; }
.kit-dg-empty { color: #5a5a5a; font-family: var(--bs-body-font-family); }
.kit-dg-caption { margin: 0.45rem 0 0; font-size: 0.82rem; color: #5a5a5a; }
/* 串接方式的小比較（放在「要保留哪些人？」下面）：文字欄靠左、可以換行 */
.kit-dg-compare { margin: -0.3rem 0 1rem; }
.kit-dg-compare .kit-dg-table th, .kit-dg-compare .kit-dg-table td {
  white-space: normal; text-align: left; font-family: var(--bs-body-font-family);
}
.kit-dg-compare .kit-dg-table tbody th { font-weight: 400; }
.kit-dg-compare .kit-dg-table td:nth-child(2) { white-space: nowrap; }
@media (max-width: 576px) {
  .kit-dg-flow { flex-direction: column; align-items: flex-start; }
  .kit-dg-arrow { transform: rotate(90deg); margin-left: 1rem; }
}

/* ── 審查修正（設計審查 2026-10-31，reports/review/design.md 第 7 節） ── */
/* 2 鍵盤焦點：分頁、檔案按鈕、勾選框都看得到 */
.nav-tabs > li > a.nav-link:focus-visible,
.kit-navgroups a.nav-link:focus-visible { outline: 3px solid #0044ff; outline-offset: 3px; }
.btn-file:focus-within { outline: 3px solid #0044ff; outline-offset: 3px; }
.form-check-input:focus-visible,
input[type="checkbox"]:focus-visible { outline: 3px solid #0044ff; outline-offset: 2px; box-shadow: none; }
/* 3 窄螢幕不要左右捲：輸入元件最寬到版面寬度（蓋過 inline 的 width） */
.shiny-input-container { max-width: 100%; }
/* 4 程式碼不要連字：<- 照原樣顯示（3.11） */
pre, code, .kit-code-block, .kit-dg-table td { font-variant-ligatures: none; font-feature-settings: "liga" 0, "calt" 0; }
/* 內文裡的長程式碼（欄名、路徑）可以斷行，窄螢幕才不會左右捲 */
:not(pre) > code { overflow-wrap: anywhere; }
/* 8 淡的字也要夠清楚（4.5:1 以上） */
.btn-primary:disabled, .btn-primary[disabled] {
  opacity: 1; background: #e8e4d8; color: #5a5a5a; border-color: #8a8a8a; box-shadow: none;
}
.kit-stephead.is-locked .kit-steptitle, .kit-stephead.is-locked .kit-stepsum,
.kit-stephead.is-locked .kit-stepnum, .kit-muted, .kit-items tr.kit-row-off td { color: #6e6a60; }
.kit-stepcard:has(.kit-stephead.is-locked), .kit-stepcard:has(.kit-stephead.is-done) { border-color: #8a8a8a; }
.kit-stephead.is-locked .kit-stepnum { border-color: #8a8a8a; }
.nav-tabs .nav-link.kit-tab-locked:not(.active) { color: #5a5a5a; }
/* 9 手機：頁首小一點；分頁一排左右滑（kitJoin 的分組也一樣） */
.kit-header-logo { height: 5.6rem; width: auto; flex: none; }
@media (max-width: 576px) {
  .kit-header { padding: 0.8rem 1rem; }
  .kit-header-logo { height: 3.6rem; }
  .kit-header h1 { font-size: 1.35rem; }
  .kit-header p { font-size: 0.82rem; }
}
@media (max-width: 768px) {
  .kit-navgroups { flex-wrap: nowrap; overflow-x: auto; padding-bottom: 8px; width: 100%; max-width: 100%; min-width: 0; }
  .kit-navgroup { flex: none; }
  .kit-navgroup .nav-tabs { flex-wrap: nowrap; }
}
/* 10 提示框是配角：沒有陰影（陰影留給結果的狀態框） */
.kit-callout, .kit-callout-warn, .kit-callout-warning { box-shadow: none; }
/* 11 舊的結果摘要框：不要框疊框（過渡期；內容之後併進狀態框） */
.kit-summary { background: transparent; border: 0; border-radius: 0; padding: 0; margin: 0 0 1.2rem; }
/* 13 勾選框、單選一樣大、一樣顏色 */
input[type="checkbox"], input[type="radio"] { width: 1.25rem; height: 1.25rem; accent-color: var(--nb-green); }
.form-check-input:checked { background-color: var(--nb-green); border-color: var(--nb-ink); }
/* 14 第 2 步：長題目不要跑到領域選單底下 */
.kit-groups td.kit-group-label { white-space: normal; min-width: 14rem; max-width: 26rem; }
/* 18 減少動態效果時，滑過不位移 */
@media (prefers-reduced-motion: reduce) {
  .btn:hover, .btn:active, .nav-tabs .nav-link:hover, .kit-download.btn-primary:hover { transform: none; }
}
/* 20 小目標放大一點：收合標題、「改」、小按鈕 */
details > summary { padding: 0.35rem 0; }
.kit-stepedit { display: inline-block; padding: 0.5rem 0.75rem; margin: -0.5rem -0.75rem; }
.btn-sm { min-height: 2.25rem; }
/* 21 檢查清單的記號 */
.kit-check.todo .kit-check-mark { color: #a14a00; }
.kit-check.note .kit-check-mark { font-style: normal; color: #5a5a5a; }
/* 25 間距：頁首離視窗頂端、分頁到內容 */
body > .container-fluid { padding-top: 1rem; }
.kit-navgroups { margin-bottom: 0.6rem; }
.tab-content { padding-top: 0.6rem; }
/* 26 上傳進度條寫中文；上傳中用墨色字 */
.shiny-file-input-progress .progress-bar { font-size: 0; }
.shiny-file-input-progress .progress-bar::after { content: "上傳完成"; font-size: 0.8rem; color: #fff; }
.shiny-file-input-progress.active .progress-bar::after { content: "上傳中…"; color: var(--nb-ink); }
/* 27 圖解裡新加的欄用淡萊姆（天藍是「資訊」的顏色） */
.kit-dg-table .kit-dg-added { background: #e8f0a0; }
/* 28 步驟卡裡的表頭淡一點（黑表頭留給主要資料表） */
.kit-stepbody .table > thead > tr > th { background: #f3f0e6; color: var(--nb-ink); border-bottom: 2px solid var(--nb-ink); }
/* 30 換頁列一行對齊 */
.kit-pager { display: flex; align-items: center; gap: 0.5rem; flex-wrap: nowrap; }
.kit-pager .form-group, .kit-pager .shiny-input-container { margin-bottom: 0; }
/* 31 手機：卡片裡的檔案表一個檔案一塊，每格前面寫欄名 */
@media (max-width: 576px) {
  .kit-file-table, .kit-file-table tbody { display: block; width: 100%; }
  .kit-file-table thead { display: none; }
  .kit-file-table tr { display: block; padding: 0.8rem 0; border-bottom: 1.5px solid #d9d4c5; }
  .kit-file-table td { display: block; border: 0; padding: 0.2rem 0; }
  .kit-file-table td[data-label]::before { content: attr(data-label); display: block; font-size: 0.82rem; color: #5a5a5a; }
}
/* 第 2 步：一組的每一題（打開看）、合成同一組題號 */
.kit-group-more { margin-top: 0.3rem; }
.kit-group-more > summary { font-size: 0.82rem; color: #4d4d4d; }
.kit-group-more ol { padding-left: 1.4rem; margin: 0.3rem 0 0; font-size: 0.85rem; }
.kit-merge { margin: 1.2rem 0 0.6rem; padding-top: 0.8rem; border-top: 1px solid #e3ded2; }
/* 32 程式碼框不能超出版面：細節列是 flex，打開的細節預設 min-width: auto，
   長的程式碼行會把它撐寬、整頁左右捲。程式碼只在自己的框裡左右捲 */
.kit-details .kit-detail, .kit-detail-body { min-width: 0; max-width: 100%; }
pre, .kit-code-block, .kit-code-panel { max-width: 100%; overflow-x: auto; }
)---"
}
