# 轉成 kits 作答檔
#
# 三個步驟，做完一步收成一行（KIT 設計語言：步驟卡）：
#   1. 每個檔案是哪個世代、哪個波次（一列一個檔案）
#   2. 哪些欄是題目（勾組、上傳題目表，或用欄名挑；一開始一題都不納入）。只挑題目，不設領域
#   3. 題目表：領域與計分。先列檢查清單（只列出要決定的事，旁邊就是決定的按鈕），
#      下面直接是題目表（逐題的領域與計分，可以下載，下次在第 2 步上傳）
# 三步都好了才能按「轉換成作答檔」；轉換完，主角換成「下載作答檔」。
# 純函式在 kitsdata_steps.R；轉換與檢查在 kitsdata_pipeline.R。

#' @noRd
mod_kitsdata_ui <- function(id) {
  ns <- shiny::NS(id)
  open_is <- function(k) paste0("output.open == '", k, "'")
  shiny::tagList(
    kit_tab_guide(
      # 原文：把各波次的資料轉成 kitsUI 要的作答檔。
      goal = "\u628a\u5404\u6ce2\u6b21\u7684\u8cc7\u6599\u8f49\u6210 kitsUI \u8981\u7684\u4f5c\u7b54\u6a94\u3002",
      steps = kit_steps(
        # 原文：說清楚每個檔案是哪個世代、哪個波次
        "\u8aaa\u6e05\u695a\u6bcf\u500b\u6a94\u6848\u662f\u54ea\u500b\u4e16\u4ee3\u3001\u54ea\u500b\u6ce2\u6b21",
        # 原文：挑出哪些欄是題目（有題目表就上傳）
        "\u6311\u51fa\u54ea\u4e9b\u6b04\u662f\u984c\u76ee\uff08\u6709\u984c\u76ee\u8868\u5c31\u4e0a\u50b3\uff09",
        # 原文：在題目表看領域與計分，決定要不要改
        "\u5728\u984c\u76ee\u8868\u770b\u9818\u57df\u8207\u8a08\u5206\uff0c\u6c7a\u5b9a\u8981\u4e0d\u8981\u6539",
        # 原文：按「轉換成作答檔」，下載
        "\u6309\u300c\u8f49\u63db\u6210\u4f5c\u7b54\u6a94\u300d\uff0c\u4e0b\u8f09"
      ),
      output = shiny::tagList(
        # 原文：一個作答檔（.csv.gz；多個檔案合併成一個），可以直接上傳到 kitsUI。
        "\u4e00\u500b\u4f5c\u7b54\u6a94\uff08.csv.gz\uff1b\u591a\u500b\u6a94\u6848\u5408\u4f75\u6210\u4e00\u500b\uff09\uff0c\u53ef\u4ee5\u76f4\u63a5\u4e0a\u50b3\u5230 kitsUI\u3002",
        # 原文：開啟 kitsUI
        " ", kit_kitsui_link("\u958b\u555f kitsUI", kind = "secondary", class = "btn-sm")
      ),
      note = paste0(
        # 原文：上傳了好幾個檔案、只要其中幾個時，在這一頁上面勾要轉的檔案。
        "\u4e0a\u50b3\u4e86\u597d\u5e7e\u500b\u6a94\u6848\u3001\u53ea\u8981\u5176\u4e2d\u5e7e\u500b\u6642\uff0c\u5728\u9019\u4e00\u9801\u4e0a\u9762\u52fe\u8981\u8f49\u7684\u6a94\u6848\u3002",
        # 原文：已經合併好的多波寬檔（soc01_t1、soc01_t2…）、長檔、只有一次施測的資料也可以，這一頁會提示要怎麼做。
        "\u5df2\u7d93\u5408\u4f75\u597d\u7684\u591a\u6ce2\u5bec\u6a94\uff08soc01_t1\u3001soc01_t2\u2026\uff09\u3001\u9577\u6a94\u3001\u53ea\u6709\u4e00\u6b21\u65bd\u6e2c\u7684\u8cc7\u6599\u4e5f\u53ef\u4ee5\uff0c\u9019\u4e00\u9801\u6703\u63d0\u793a\u8981\u600e\u9ebc\u505a\u3002",
        # 原文：沒有作答的題不會放進作答檔。
        "\u6c92\u6709\u4f5c\u7b54\u7684\u984c\u4e0d\u6703\u653e\u9032\u4f5c\u7b54\u6a94\u3002"
      ),
      technical = paste0(
        # 原文：每個檔案先把每一題的原分數換成計分（計分對照），
        "\u6bcf\u500b\u6a94\u6848\u5148\u628a\u6bcf\u4e00\u984c\u7684\u539f\u5206\u6578\u63db\u6210\u8a08\u5206\uff08\u8a08\u5206\u5c0d\u7167\uff09\uff0c",
        # 原文：再用 tidyr::pivot_longer() 把題目欄轉成 item_id、item_score，最後接上領域並合併。
        "\u518d\u7528 tidyr::pivot_longer() \u628a\u984c\u76ee\u6b04\u8f49\u6210 item_id\u3001item_score\uff0c\u6700\u5f8c\u63a5\u4e0a\u9818\u57df\u4e26\u5408\u4f75\u3002",
        # 原文：檢查都在寬資料上做；長格式在下載時每 5,000 列寬資料轉一段、寫進 .csv.gz，不會整張放進記憶體。
        "\u6aa2\u67e5\u90fd\u5728\u5bec\u8cc7\u6599\u4e0a\u505a\uff1b\u9577\u683c\u5f0f\u5728\u4e0b\u8f09\u6642\u6bcf 5,000 \u5217\u5bec\u8cc7\u6599\u8f49\u4e00\u6bb5\u3001\u5beb\u9032 .csv.gz\uff0c\u4e0d\u6703\u6574\u5f35\u653e\u9032\u8a18\u61b6\u9ad4\u3002",
        # 原文：visit_id 由 {cohort}_{subj_id}_{波次標籤} 組成；沒有波次標籤欄時改用 wave_m。
        "visit_id \u7531 {cohort}_{subj_id}_{\u6ce2\u6b21\u6a19\u7c64} \u7d44\u6210\uff1b\u6c92\u6709\u6ce2\u6b21\u6a19\u7c64\u6b04\u6642\u6539\u7528 wave_m\u3002",
        # 原文：這一頁不把任何值改成 NA：疑似遺漏值代碼（值標籤是「遺漏值」「跳答」這類、
        "\u9019\u4e00\u9801\u4e0d\u628a\u4efb\u4f55\u503c\u6539\u6210 NA\uff1a\u7591\u4f3c\u907a\u6f0f\u503c\u4ee3\u78bc\uff08\u503c\u6a19\u7c64\u662f\u300c\u907a\u6f0f\u503c\u300d\u300c\u8df3\u7b54\u300d\u9019\u985e\u3001",
        # 原文：「遺漏值處理」的代碼、7777 以上、和分數明顯分開的值）要在第 3 步決定：
        "\u300c\u907a\u6f0f\u503c\u8655\u7406\u300d\u7684\u4ee3\u78bc\u30017777 \u4ee5\u4e0a\u3001\u548c\u5206\u6578\u660e\u986f\u5206\u958b\u7684\u503c\uff09\u8981\u5728\u7b2c 3 \u6b65\u6c7a\u5b9a\uff1a",
        # 原文：先用「遺漏值處理」改成 NA，或照原樣當成分數。
        "\u5148\u7528\u300c\u907a\u6f0f\u503c\u8655\u7406\u300d\u6539\u6210 NA\uff0c\u6216\u7167\u539f\u6a23\u7576\u6210\u5206\u6578\u3002"
      )
    ),
    shiny::uiOutput(ns("source_note")),
    shiny::uiOutput(ns("source_ui")),
    shiny::uiOutput(ns("shape_ui")),
    shiny::conditionalPanel(
      "output.has_files == 'yes'", ns = ns,

      # 第 1 步 ----
      kit_step_card(
        shiny::uiOutput(ns("head1")),
        shiny::conditionalPanel(
          open_is(1), ns = ns, kit_step_body(
          # 原文：已經照欄名猜好了（淡黃色），請看一遍，不對就改。
          shiny::tags$p("\u5df2\u7d93\u7167\u6b04\u540d\u731c\u597d\u4e86\uff08\u6de1\u9ec3\u8272\uff09\uff0c\u8acb\u770b\u4e00\u904d\uff0c\u4e0d\u5c0d\u5c31\u6539\u3002",
                        # 原文：每一格可以選檔案裡的一欄，也可以直接打字填一個值（例如 2009）。
                        "\u6bcf\u4e00\u683c\u53ef\u4ee5\u9078\u6a94\u6848\u88e1\u7684\u4e00\u6b04\uff0c\u4e5f\u53ef\u4ee5\u76f4\u63a5\u6253\u5b57\u586b\u4e00\u500b\u503c\uff08\u4f8b\u5982 2009\uff09\u3002"),
          shiny::uiOutput(ns("files_table")),
          shiny::uiOutput(ns("single_note")),
          # 原文：第一列的世代套用到每一列
          shiny::actionLink(ns("fill_cohort"), "\u7b2c\u4e00\u5217\u7684\u4e16\u4ee3\u5957\u7528\u5230\u6bcf\u4e00\u5217"),
          shiny::tags$div(
            class = "kit-inline",
            # 原文：波次的單位
            shiny::selectInput(ns("wave_unit"), "\u6ce2\u6b21\u7684\u55ae\u4f4d", choices = KITS_WAVE_UNITS,
                               selected = "month", width = "15rem"),
            shiny::uiOutput(ns("visit_note"))
          ),
          shiny::uiOutput(ns("wave_unit_note")),
          kit_detail(
            # 原文：還有性別、實際月齡、波次標籤（不一定要）
            "\u9084\u6709\u6027\u5225\u3001\u5be6\u969b\u6708\u9f61\u3001\u6ce2\u6b21\u6a19\u7c64\uff08\u4e0d\u4e00\u5b9a\u8981\uff09",
            shiny::uiOutput(ns("more_table")),
            shiny::radioButtons(
              # 原文：世代欄、性別欄有值標籤時（SPSS 檔），作答檔寫：
              ns("value_labels"), "\u4e16\u4ee3\u6b04\u3001\u6027\u5225\u6b04\u6709\u503c\u6a19\u7c64\u6642\uff08SPSS \u6a94\uff09\uff0c\u4f5c\u7b54\u6a94\u5beb\uff1a",
              # 原文：代碼（例如 1、2） | 標籤（例如 男、女）
              choices = c("\u4ee3\u78bc\uff08\u4f8b\u5982 1\u30012\uff09" = "code", "\u6a19\u7c64\uff08\u4f8b\u5982 \u7537\u3001\u5973\uff09" = "label"),
              selected = "code", inline = TRUE
            )
          ),
          shiny::uiOutput(ns("step1_problems")),
          # 原文：對，下一步
          shiny::actionButton(ns("ok1"), "\u5c0d\uff0c\u4e0b\u4e00\u6b65", class = "btn-default")
        )),
        title_id = ns("steptitle1")
      ),

      # 第 2 步 ----
      kit_step_card(
        shiny::uiOutput(ns("head2")),
        shiny::conditionalPanel(
          open_is(2), ns = ns, kit_step_body(
          shiny::uiOutput(ns("intro2")),
          shiny::tags$div(
            class = "kit-inline",
            shiny::textInput(ns("grp_search"), NULL, width = "20rem",
                             # 原文：找欄名或題目內容，例如 d5 或 緊張
                             placeholder = "\u627e\u6b04\u540d\u6216\u984c\u76ee\u5167\u5bb9\uff0c\u4f8b\u5982 d5 \u6216 \u7dca\u5f35")
          ),
          shiny::tags$div(
            class = "kit-pager",
            # 原文：上一頁
            shiny::actionButton(ns("grp_prev"), "\u4e0a\u4e00\u9801", class = "btn-sm btn-default"),
            shiny::textOutput(ns("grp_page"), inline = TRUE),
            # 原文：下一頁
            shiny::actionButton(ns("grp_next"), "\u4e0b\u4e00\u9801", class = "btn-sm btn-default")
          ),
          shiny::uiOutput(ns("groups_table")),
          shiny::uiOutput(ns("merge_ui")),
          shiny::uiOutput(ns("items_status")),
          kit_details_row(
            kit_detail(
              # 原文：有題目表就上傳
              "\u6709\u984c\u76ee\u8868\u5c31\u4e0a\u50b3",
              shiny::fileInput(
                # 原文：上傳題目表（之前下載的題目表，或 item_id、domain 兩欄的 CSV/SAV）
                ns("item_file"), "\u4e0a\u50b3\u984c\u76ee\u8868\uff08\u4e4b\u524d\u4e0b\u8f09\u7684\u984c\u76ee\u8868\uff0c\u6216 item_id\u3001domain \u5169\u6b04\u7684 CSV/SAV\uff09",
                # 原文：選擇檔案… | 還沒有上傳
                accept = c(".csv", ".sav"), buttonLabel = "\u9078\u64c7\u6a94\u6848\u2026", placeholder = "\u9084\u6c92\u6709\u4e0a\u50b3",
                width = "34rem"
              ),
              shiny::tags$p(class = "kit-hint",
                # 原文：一列一題：item_id（資料裡的欄名）、domain（領域）；
                "\u4e00\u5217\u4e00\u984c\uff1aitem_id\uff08\u8cc7\u6599\u88e1\u7684\u6b04\u540d\uff09\u3001domain\uff08\u9818\u57df\uff09\uff1b",
                # 原文：可以再加 item_id_out（作答檔的題號：各波次欄名不同、其實是同一題時，給同一個題號）。
                "\u53ef\u4ee5\u518d\u52a0 item_id_out\uff08\u4f5c\u7b54\u6a94\u7684\u984c\u865f\uff1a\u5404\u6ce2\u6b21\u6b04\u540d\u4e0d\u540c\u3001\u5176\u5be6\u662f\u540c\u4e00\u984c\u6642\uff0c\u7d66\u540c\u4e00\u500b\u984c\u865f\uff09\u3002",
                # 原文：之前在第 3 步下載的題目表，會連領域、計分對照一起套用。
                "\u4e4b\u524d\u5728\u7b2c 3 \u6b65\u4e0b\u8f09\u7684\u984c\u76ee\u8868\uff0c\u6703\u9023\u9818\u57df\u3001\u8a08\u5206\u5c0d\u7167\u4e00\u8d77\u5957\u7528\u3002"),
              shiny::selectInput(
                # 原文：反向題的欄名（題目表查不到時，用原題的領域）
                ns("reverse_rule"), "\u53cd\u5411\u984c\u7684\u6b04\u540d\uff08\u984c\u76ee\u8868\u67e5\u4e0d\u5230\u6642\uff0c\u7528\u539f\u984c\u7684\u9818\u57df\uff09",
                choices = KITS_REVERSE_RULES, selected = "suffix", width = "24rem"
              )
            ),
            kit_detail(
              # 原文：用欄名一次挑很多欄
              "\u7528\u6b04\u540d\u4e00\u6b21\u6311\u5f88\u591a\u6b04",
              shiny::tags$div(
                class = "kit-inline",
                # 原文：欄名開頭是
                shiny::textInput(ns("pick_prefix"), "\u6b04\u540d\u958b\u982d\u662f", width = "16rem",
                                 # 原文：例如 w1d5_、w2d5_
                                 placeholder = "\u4f8b\u5982 w1d5_\u3001w2d5_"),
                # 原文：加入這些欄
                shiny::actionButton(ns("pick_add"), "\u52a0\u5165\u9019\u4e9b\u6b04", class = "btn-default")
              ),
              shiny::uiOutput(ns("pick_note")),
              # 原文：全部清掉，重新挑
              shiny::actionLink(ns("pick_clear"), "\u5168\u90e8\u6e05\u6389\uff0c\u91cd\u65b0\u6311")
            ),
            # 原文：看挑出來的題目
            kit_detail("\u770b\u6311\u51fa\u4f86\u7684\u984c\u76ee", shiny::uiOutput(ns("items_list"))),
            # 原文：沒有列在上面的欄
            kit_detail("\u6c92\u6709\u5217\u5728\u4e0a\u9762\u7684\u6b04", shiny::uiOutput(ns("text_cols")))
          ),
          # 原文：對，下一步
          shiny::actionButton(ns("ok2"), "\u5c0d\uff0c\u4e0b\u4e00\u6b65", class = "btn-default")
        )),
        title_id = ns("steptitle2")
      ),

      # 第 3 步 ----
      kit_step_card(
        shiny::uiOutput(ns("head3")),
        shiny::conditionalPanel(
          open_is(3), ns = ns, kit_step_body(
          # 原文：kits 要的分數是 0、1、2…。轉換前，看下面每一件事：
          shiny::tags$p("kits \u8981\u7684\u5206\u6578\u662f 0\u30011\u30012\u2026\u3002\u8f49\u63db\u524d\uff0c\u770b\u4e0b\u9762\u6bcf\u4e00\u4ef6\u4e8b\uff1a"),
          shiny::uiOutput(ns("checks")),
          shiny::tags$div(
            id = ns("item_table"), class = "kit-itemtable",
            shiny::tags$h5(class = "kit-itemtable-title",
                           shiny::textOutput(ns("items_title"), inline = TRUE)),
            shiny::tags$p(class = "kit-hint",
              # 原文：一列一欄。勾選幾列，再用表下面的「對勾選的列」改領域或計分。
              "\u4e00\u5217\u4e00\u6b04\u3002\u52fe\u9078\u5e7e\u5217\uff0c\u518d\u7528\u8868\u4e0b\u9762\u7684\u300c\u5c0d\u52fe\u9078\u7684\u5217\u300d\u6539\u9818\u57df\u6216\u8a08\u5206\u3002"),
            kitsdata_items_tools(ns),
            shiny::uiOutput(ns("items_table")),
            kitsdata_bulk_tools(ns),
            shiny::tags$div(class = "kit-downloads kit-itemtable-dl",
                            shiny::uiOutput(ns("table_dl_ui"), inline = TRUE))
          )
        )),
        title_id = ns("steptitle3")
      ),

      shiny::uiOutput(ns("ready")),
      shiny::uiOutput(ns("run_ui")),
      shiny::uiOutput(ns("status")),
      shiny::uiOutput(ns("result"))
    )
  )
}

# 題目表上面：找欄名、要不要顯示沒挑的欄、換頁、勾選
kitsdata_items_tools <- function(ns) {
  shiny::tagList(
    shiny::tags$div(
      class = "kit-inline",
      # 原文：找欄名，例如 d5
      shiny::textInput(ns("tbl_search"), NULL, placeholder = "\u627e\u6b04\u540d\uff0c\u4f8b\u5982 d5", width = "14rem"),
      # 原文：也顯示沒挑的欄
      shiny::checkboxInput(ns("tbl_all"), "\u4e5f\u986f\u793a\u6c92\u6311\u7684\u6b04", value = FALSE)
    ),
    shiny::tags$div(
      class = "kit-pager",
      # 原文：上一頁
      shiny::actionButton(ns("tbl_prev"), "\u4e0a\u4e00\u9801", class = "btn-sm btn-default"),
      shiny::textOutput(ns("tbl_page"), inline = TRUE),
      # 原文：下一頁
      shiny::actionButton(ns("tbl_next"), "\u4e0b\u4e00\u9801", class = "btn-sm btn-default")
    )
  )
}

# 題目表下面：對勾選的列做一件事
KITS_BULK_ACTIONS <- c(
  # 原文：設領域 | 計分加減（例如 -1） | 反向計分
  "\u8a2d\u9818\u57df" = "domain", "\u8a08\u5206\u52a0\u6e1b\uff08\u4f8b\u5982 -1\uff09" = "shift", "\u53cd\u5411\u8a08\u5206" = "reverse",
  # 原文：合併原分數 | 還原（計分＝原分數） | 不要這幾欄
  "\u5408\u4f75\u539f\u5206\u6578" = "merge", "\u9084\u539f\uff08\u8a08\u5206\uff1d\u539f\u5206\u6578\uff09" = "restore", "\u4e0d\u8981\u9019\u5e7e\u6b04" = "exclude",
  # 原文：要這幾欄 | 輸出題號：去掉欄名的一段 | 輸出題號：還原成欄名
  "\u8981\u9019\u5e7e\u6b04" = "include", "\u8f38\u51fa\u984c\u865f\uff1a\u53bb\u6389\u6b04\u540d\u7684\u4e00\u6bb5" = "out_prefix", "\u8f38\u51fa\u984c\u865f\uff1a\u9084\u539f\u6210\u6b04\u540d" = "out_reset"
)
kitsdata_bulk_tools <- function(ns) {
  when <- function(value, ...) {
    shiny::conditionalPanel(paste0("input.bulk_action == '", value, "'"), ns = ns, ...)
  }
  shiny::tags$div(
    class = "kit-bulk",
    shiny::tags$div(
      class = "kit-inline",
      # 原文：勾選全部符合的列
      shiny::actionLink(ns("tbl_sel_all"), "\u52fe\u9078\u5168\u90e8\u7b26\u5408\u7684\u5217"),
      # 原文：全部不勾
      shiny::actionLink(ns("tbl_sel_none"), "\u5168\u90e8\u4e0d\u52fe"),
      shiny::textOutput(ns("sel_count"), inline = TRUE)
    ),
    shiny::tags$div(
      class = "kit-inline",
      # 原文：對勾選的列
      shiny::selectInput(ns("bulk_action"), "\u5c0d\u52fe\u9078\u7684\u5217", choices = KITS_BULK_ACTIONS,
                         width = "17rem"),
      when("domain", shiny::selectizeInput(
        # 原文：領域
        ns("bulk_domain"), "\u9818\u57df", choices = NULL, width = "12rem",
        # 原文：選或輸入領域
        options = list(create = TRUE, placeholder = "\u9078\u6216\u8f38\u5165\u9818\u57df")
      )),
      # 原文：加減
      when("shift", shiny::numericInput(ns("bulk_shift"), "\u52a0\u6e1b", value = -1, step = 1,
                                        width = "7rem")),
      # 原文：最高分 | 空白＝各題最高分
      when("reverse", shiny::textInput(ns("bulk_max"), "\u6700\u9ad8\u5206", placeholder = "\u7a7a\u767d\uff1d\u5404\u984c\u6700\u9ad8\u5206",
                                       width = "11rem")),
      when("merge",
           # 原文：原分數 | 例如 4、5
           shiny::textInput(ns("merge_from"), "\u539f\u5206\u6578", placeholder = "\u4f8b\u5982 4\u30015", width = "8rem"),
           # 原文：都變成
           shiny::numericInput(ns("merge_to"), "\u90fd\u8b8a\u6210", value = NA, step = 1, width = "7rem")),
      # 原文：去掉（規則）
      when("out_prefix", shiny::textInput(ns("out_pattern"), "\u53bb\u6389\uff08\u898f\u5247\uff09", value = "^w[0-9]+",
                                          width = "9rem")),
      # 原文：套用
      shiny::actionButton(ns("bulk_apply"), "\u5957\u7528", class = "btn-default")
    ),
    shiny::tags$small(
      class = "kit-hint",
      # 原文：加減、反向都在目前的計分上做。遺漏值代碼（例如 98、99）不要用合併接起來，
      "\u52a0\u6e1b\u3001\u53cd\u5411\u90fd\u5728\u76ee\u524d\u7684\u8a08\u5206\u4e0a\u505a\u3002\u907a\u6f0f\u503c\u4ee3\u78bc\uff08\u4f8b\u5982 98\u300199\uff09\u4e0d\u8981\u7528\u5408\u4f75\u63a5\u8d77\u4f86\uff0c",
      # 原文：請先用「遺漏值處理」改成 NA。輸出題號是作答檔的 item_id：各波次欄名不同、其實是同一題時，
      "\u8acb\u5148\u7528\u300c\u907a\u6f0f\u503c\u8655\u7406\u300d\u6539\u6210 NA\u3002\u8f38\u51fa\u984c\u865f\u662f\u4f5c\u7b54\u6a94\u7684 item_id\uff1a\u5404\u6ce2\u6b21\u6b04\u540d\u4e0d\u540c\u3001\u5176\u5be6\u662f\u540c\u4e00\u984c\u6642\uff0c",
      # 原文：給它們同一個題號（例如 w1d5_1、w2d5_1 去掉 ^w[0-9]+ 都是 d5_1）。
      "\u7d66\u5b83\u5011\u540c\u4e00\u500b\u984c\u865f\uff08\u4f8b\u5982 w1d5_1\u3001w2d5_1 \u53bb\u6389 ^w[0-9]+ \u90fd\u662f d5_1\uff09\u3002"
    )
  )
}

#' @param files mod_upload_server() 的回傳值
#' @param working mod_missing_server() 的回傳值
#' @param goto function(tab) 切換分頁（到「遺漏值處理」）
#' @param na_codes NULL 或 reactive：「遺漏值處理」的代碼清單（只用來找疑似遺漏值代碼）
#' @param stack NULL 或 mod_stack_server() 的回傳值（可以改用 ④ 疊加的結果）
#' @noRd
mod_kitsdata_server <- function(id, files, working, goto, na_codes = NULL, stack = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    codes_2 <- shiny::reactive(if (is.null(na_codes)) NULL else na_codes())

    # 資料來源 ----
    # 上傳的檔案（或 ② 處理後的版本；可以只用其中幾個），或 ④ 疊加的結果；
    # 再套用這一頁的整理（多波寬檔轉成一列一個波次、波次欄取出數字）。
    # 下面每一步都只看 data_src()：keys（整理過的檔案 key 會加上整理的種類，第 1 步的格子才會重新猜）、
    # names、labels、dfs，以及 tf（每個檔案的整理，給「用 R 怎麼做」）
    stack_res <- shiny::reactive(if (is.null(stack)) NULL else stack$result())
    use_stack <- shiny::reactive(identical(input$source, "stack") && !is.null(stack_res()))
    skip <- shiny::reactiveVal(character(0))   # 沒有勾的上傳檔案
    shiny::observeEvent(input$use_files, {
      skip(setdiff(files$keys(), input$use_files %||% character(0)))
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
    base <- shiny::reactive({
      if (use_stack()) {
        # 原文：④ 疊加的結果
        return(list(keys = "stack", names = "\u2463 \u758a\u52a0\u7684\u7d50\u679c", labels = "",
                    dfs = list(stack_res()$result)))
      }
      keys <- files$keys()
      on <- !keys %in% skip()
      list(keys = keys[on], names = files$names()[on], labels = files$labels()[on],
           dfs = working$data()[on], idx = which(on))
    })
    tf_on <- shiny::reactiveVal(list())   # key → "pivot" 或 "wave_num"
    shiny::observeEvent(input$tf, {
      t <- input$tf
      on <- tf_on()
      on[[t$key]] <- if (identical(t$kind, "off")) NULL else t$kind
      tf_on(on)
      if (identical(t$kind, "pivot")) {
        det <- detect()[[match(t$key, base()$keys)]]$pivot
        unit <- if (identical(tolower(det$token %||% ""), "m")) "month" else "index"
        shiny::updateSelectInput(session, "wave_unit", selected = unit)
      }
    })
    detect <- shiny::reactive(lapply(base()$dfs, function(df) list(
      pivot = kits_detect_wave_suffix(names(df)), wave_num = kits_detect_wave_text(df)
    )))
    data_src <- shiny::reactive({
      b <- base()
      d <- detect()
      on <- tf_on()
      tf <- purrr::map2(b$keys, d, function(key, det) {
        kind <- on[[key]]
        if (is.null(kind) || is.null(det[[kind]])) return(NULL)
        dt <- det[[kind]]
        list(kind = kind, det = dt)
      })
      for (i in seq_along(tf)) {
        t <- tf[[i]]
        if (is.null(t)) next
        if (t$kind == "pivot") tf[[i]]$det$zap_cols <- kits_wave_zap_cols(b$dfs[[i]], t$det)
        b$dfs[[i]] <- kits_source_apply(b$dfs[[i]], t$kind, t$det)
        b$keys[i] <- paste0(b$keys[i], "__", t$kind)
      }
      b$tf <- tf
      b
    })
    src_keys <- shiny::reactive(data_src()$keys)
    src_names <- shiny::reactive(data_src()$names)
    dfs <- shiny::reactive(data_src()$dfs)
    has_files <- shiny::reactive(length(src_keys()) > 0)
    output$source_ui <- shiny::renderUI({
      keys <- files$keys()
      if (length(keys) == 0) return(NULL)
      has_stack <- !is.null(stack_res())
      shiny::tagList(
        if (has_stack) shiny::radioButtons(
          # 原文：資料來源
          ns("source"), "\u8cc7\u6599\u4f86\u6e90",
          # 原文：上傳的檔案 | ④ 直向波次疊加的結果（一個檔案）
          choices = c("\u4e0a\u50b3\u7684\u6a94\u6848" = "files", "\u2463 \u76f4\u5411\u6ce2\u6b21\u758a\u52a0\u7684\u7d50\u679c\uff08\u4e00\u500b\u6a94\u6848\uff09" = "stack"),
          selected = shiny::isolate(input$source) %||% "files", inline = TRUE
        ),
        if (length(keys) > 1 && !use_stack()) shiny::checkboxGroupInput(
          # 原文：要轉的檔案
          ns("use_files"), "\u8981\u8f49\u7684\u6a94\u6848", choiceNames = files$names(), choiceValues = keys,
          selected = setdiff(keys, shiny::isolate(skip())), inline = TRUE
        ),
        if (length(keys) > 1 && !use_stack() && all(keys %in% skip())) shiny::tags$div(
          class = "kit-callout kit-callout-warn",
          # 原文：請至少勾一個檔案。
          "\u8acb\u81f3\u5c11\u52fe\u4e00\u500b\u6a94\u6848\u3002"
        )
      )
    })
    # 多波寬檔、文字的波次欄：提醒，按一下就整理（再按一下還原）
    output$shape_ui <- shiny::renderUI({
      b <- base()
      d <- detect()
      on <- tf_on()
      if (length(b$keys) == 0) return(NULL)
      tf_btn <- function(key, kind, label, cls = "btn-sm btn-default") {
        shiny::tags$button(
          type = "button", class = paste("btn", cls),
          onclick = sprintf("Shiny.setInputValue('%s', {key: %s, kind: '%s'}, {priority: 'event'})",
                            ns("tf"), jsonlite::toJSON(key, auto_unbox = TRUE), kind),
          label
        )
      }
      out <- purrr::pmap(list(b$keys, b$names, d, b$dfs), function(key, name, det, df) {
        kind <- on[[key]]
        if (!is.null(kind) && !is.null(det[[kind]])) {
          applied <- kits_source_apply(df, kind, det[[kind]])
          return(shiny::tags$div(
            class = "kit-callout",
            shiny::tags$strong(name), "\uff1a",
            if (kind == "pivot") paste0(
              # 原文：已轉成一列一個孩子的一個波次（ |  → 
              "\u5df2\u8f49\u6210\u4e00\u5217\u4e00\u500b\u5b69\u5b50\u7684\u4e00\u500b\u6ce2\u6b21\uff08", kit_dims(nrow(df), ncol(df)), " \u2192 ",
              # 原文：）；波次記號在  |  欄，時間點在  |  欄。
              kit_dims(nrow(applied), ncol(applied)), "\uff09\uff1b\u6ce2\u6b21\u8a18\u865f\u5728 ", det$pivot$label_col, " \u6b04\uff0c\u6642\u9593\u9ede\u5728 ", det$pivot$wave_col, " \u6b04\u3002"
            ) else paste0(
              # 原文：已從  |  欄取出數字，放在 wave_m 欄（ | 、 | ）。
              "\u5df2\u5f9e ", det$wave_num$col, " \u6b04\u53d6\u51fa\u6578\u5b57\uff0c\u653e\u5728 wave_m \u6b04\uff08", paste(det$wave_num$examples, collapse = "\u3001"), "\uff09\u3002"
            ),
            " ",
            # 原文：還原
            tf_btn(key, "off", "\u9084\u539f", "btn-link btn-sm")
          ))
        }
        if (!is.null(det$pivot)) {
          p <- det$pivot
          return(shiny::tags$div(
            class = "kit-callout kit-callout-warn",
            shiny::tags$strong(name),
            # 原文：像是多波寬檔：同一題各波次一欄（ | 、 | …，
            paste0("\u50cf\u662f\u591a\u6ce2\u5bec\u6a94\uff1a\u540c\u4e00\u984c\u5404\u6ce2\u6b21\u4e00\u6b04\uff08", paste(utils::head(p$cols, 2), collapse = "\u3001"), "\u2026\uff0c",
                   # 原文：  個波次、 |  題）。這一頁要的是一列一個孩子的一個波次，不轉的話同一題各波次會被當成不同的題。
                   length(p$waves), " \u500b\u6ce2\u6b21\u3001", p$n_stems, " \u984c\uff09\u3002\u9019\u4e00\u9801\u8981\u7684\u662f\u4e00\u5217\u4e00\u500b\u5b69\u5b50\u7684\u4e00\u500b\u6ce2\u6b21\uff0c\u4e0d\u8f49\u7684\u8a71\u540c\u4e00\u984c\u5404\u6ce2\u6b21\u6703\u88ab\u7576\u6210\u4e0d\u540c\u7684\u984c\u3002"),
            " ",
            # 原文：轉成一列一個波次
            tf_btn(key, "pivot", "\u8f49\u6210\u4e00\u5217\u4e00\u500b\u6ce2\u6b21")
          ))
        }
        if (!is.null(det$wave_num)) {
          w <- det$wave_num
          return(shiny::tags$div(
            class = "kit-callout kit-callout-warn",
            shiny::tags$strong(name),
            # 原文：的波次欄  |  是文字，時間點（wave_m）要是數字。可以取出裡面的數字（ | 、 | ）。
            paste0("\u7684\u6ce2\u6b21\u6b04 ", w$col, " \u662f\u6587\u5b57\uff0c\u6642\u9593\u9ede\uff08wave_m\uff09\u8981\u662f\u6578\u5b57\u3002\u53ef\u4ee5\u53d6\u51fa\u88e1\u9762\u7684\u6578\u5b57\uff08", paste(w$examples, collapse = "\u3001"), "\uff09\u3002"),
            " ",
            # 原文：取出數字當時間點
            tf_btn(key, "wave_num", "\u53d6\u51fa\u6578\u5b57\u7576\u6642\u9593\u9ede")
          ))
        }
        NULL
      })
      shiny::tagList(purrr::compact(out))
    })

    # 步驟：現在打開哪一步（0 = 都收起來，轉換完成時），第 1、2 步按過「對，下一步」沒有
    open <- shiny::reactiveVal(1L)
    ok <- shiny::reactiveValues(s1 = FALSE, s2 = FALSE)
    output$open <- shiny::renderText(as.character(open()))
    output$has_files <- shiny::renderText(if (has_files()) "yes" else "no")
    shiny::outputOptions(output, "open", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "has_files", suspendWhenHidden = FALSE)
    # 換了檔案（或資料來源、整理）：從第 1 步重來
    last_keys <- shiny::reactiveVal(NULL)
    shiny::observe({
      k <- src_keys()
      if (identical(k, shiny::isolate(last_keys()))) return()
      first <- is.null(shiny::isolate(last_keys()))
      last_keys(k)
      if (first) return()
      ok$s1 <- FALSE
      ok$s2 <- FALSE
      open(1L)
    })
    # 「改」連結在標頭裡，標頭重畫時連結也重畫（值從 0 開始）：只有真的按了（> 0）才打開
    clicked <- function(x) isTRUE(x > 0)
    shiny::observeEvent(input$edit1, if (clicked(input$edit1)) open(1L))
    shiny::observeEvent(input$edit2, if (clicked(input$edit2) && ok$s1) open(2L))
    shiny::observeEvent(input$edit3, if (clicked(input$edit3) && ok$s1 && ok$s2) open(3L))

    output$source_note <- shiny::renderUI({
      if (length(files$keys()) == 0) {
        return(shiny::tags$div(
          class = "kit-callout kit-callout-warn",
          # 原文：請先到「檔案上傳」加入寬格式的資料檔（一個或多個）。
          "\u8acb\u5148\u5230\u300c\u6a94\u6848\u4e0a\u50b3\u300d\u52a0\u5165\u5bec\u683c\u5f0f\u7684\u8cc7\u6599\u6a94\uff08\u4e00\u500b\u6216\u591a\u500b\uff09\u3002"
        ))
      }
      if (identical(working$source(), "na")) {
        # 原文：用的是「遺漏值處理」處理後的資料。
        return(shiny::tags$p(class = "kit-hint", "\u7528\u7684\u662f\u300c\u907a\u6f0f\u503c\u8655\u7406\u300d\u8655\u7406\u5f8c\u7684\u8cc7\u6599\u3002"))
      }
      # 「遺漏值處理」的設定或檔案改過、還沒重新執行
      if (identical(working$source(), "stale")) {
        shiny::tags$div(
          class = "kit-callout kit-callout-warn",
          # 原文：「遺漏值處理」的設定改過了，請
          "\u300c\u907a\u6f0f\u503c\u8655\u7406\u300d\u7684\u8a2d\u5b9a\u6539\u904e\u4e86\uff0c\u8acb",
          # 原文：重新執行「遺漏值處理」
          shiny::actionLink(ns("goto_missing_stale"), "\u91cd\u65b0\u57f7\u884c\u300c\u907a\u6f0f\u503c\u8655\u7406\u300d"),
          # 原文：，這一頁才會用到新的結果。
          "\uff0c\u9019\u4e00\u9801\u624d\u6703\u7528\u5230\u65b0\u7684\u7d50\u679c\u3002"
        )
      }
    })
    shiny::observeEvent(input$goto_missing, goto("missing"))
    shiny::observeEvent(input$goto_missing_stale, goto("missing"))
    shiny::observeEvent(input$goto_upload_status, goto("upload"))

    all_cols <- shiny::reactive(unique(unlist(purrr::map(dfs(), names))))

    # 第 1 步 ----
    guess <- shiny::reactive(kits_step1_guess(dfs(), src_names(), data_src()$labels))
    cell_id <- function(field, key) paste0("cell_", field, "_", key)
    cells <- shiny::reactive({
      keys <- src_keys()
      g <- guess()
      fields <- c(KITS_STEP1_MAIN, KITS_STEP1_MORE)
      stats::setNames(purrr::map(fields, function(field) {
        purrr::map2_chr(keys, seq_along(keys), function(key, k) {
          v <- input[[cell_id(field, key)]]
          if (is.null(v)) g[[field]][k] %||% "" else trimws(paste(v, collapse = ""))
        })
      }), fields)
    })
    step1 <- shiny::reactive(kits_step1_spec(dfs(), src_names(), cells()))

    # 一格：選檔案裡的一欄，或打字填一個值
    cell_input <- function(field, key, df, k, width = "11rem", fname = "") {
      id <- cell_id(field, key)
      prev <- shiny::isolate(input[[id]])
      g <- guess()[[field]][k] %||% ""
      value <- prev %||% g
      choices <- c("", names(df))
      if (nzchar(value) && !value %in% choices) choices <- c(choices, value)
      box <- shiny::selectizeInput(
        ns(id), NULL, choices = choices, selected = value, width = width,
        options = list(
          create = TRUE, createOnBlur = TRUE, persist = FALSE,
          # 選單掛在整頁上：表格外框會左右捲動（overflow），選單放在裡面會被切掉
          dropdownParent = "body",
          # 原文：例如 M03 | 選一欄或打字
          placeholder = if (field == "cohort") "\u4f8b\u5982 M03" else "\u9078\u4e00\u6b04\u6216\u6253\u5b57",
          # 表格裡的選單沒有看得到的標籤：給螢幕閱讀器「檔名 的 世代」
          onInitialize = I(sprintf("function() { this.$control_input.attr('aria-label', %s); }",
                                   # 原文： 的
                                   jsonlite::toJSON(paste0(fname, " \u7684", KITS_STEP1_LABELS[[field]]),
                                                    auto_unbox = TRUE))),
          render = I(paste0("{option_create: function(d, esc) {\n",
            # 原文：            return '<div class=\"create\">填入「' + esc(d.input) + '」</div>'; }}
            "            return '<div class=\"create\">\u586b\u5165\u300c' + esc(d.input) + '\u300d</div>'; }}"))
        )
      )
      shiny::tags$td(class = if (is.null(prev) && nzchar(g)) "kit-guess",
                     `data-label` = KITS_STEP1_LABELS[[field]], box)
    }
    file_rows <- function(fields) {
      keys <- src_keys()
      purrr::pmap(list(keys, src_names(), dfs(), seq_along(keys)), function(key, name, df, k) {
        shiny::tags$tr(
          shiny::tags$td(shiny::tags$strong(name), shiny::tags$br(),
                         shiny::tags$small(class = "kit-hint", kit_dims(nrow(df), ncol(df)))),
          purrr::map(fields, function(field) cell_input(field, key, df, k, fname = name))
        )
      })
    }
    file_table <- function(fields) {
      shiny::tags$div(
        class = "kit-table-scroll",
        shiny::tags$table(
          class = "table kit-file-table",
          shiny::tags$thead(shiny::tags$tr(
            # 原文：檔案
            shiny::tags$th("\u6a94\u6848"),
            purrr::map(fields, function(f) shiny::tags$th(KITS_STEP1_LABELS[[f]]))
          )),
          shiny::tags$tbody(file_rows(fields))
        )
      )
    }
    output$files_table <- shiny::renderUI({
      if (!has_files()) return(NULL)
      file_table(KITS_STEP1_MAIN)
    })
    # 單一世代、單一波次：世代與時間點先填好了
    output$single_note <- shiny::renderUI({
      if (!isTRUE(attr(guess(), "single"))) return(NULL)
      shiny::tags$small(
        class = "kit-hint",
        # 原文：只有一個檔案、檔案裡沒有世代與波次：世代先填 S1、時間點先填 0。
        "\u53ea\u6709\u4e00\u500b\u6a94\u6848\u3001\u6a94\u6848\u88e1\u6c92\u6709\u4e16\u4ee3\u8207\u6ce2\u6b21\uff1a\u4e16\u4ee3\u5148\u586b S1\u3001\u6642\u9593\u9ede\u5148\u586b 0\u3002",
        # 原文：kitsUI 要這兩欄，這裡只用來組列次編號；只有一次施測的研究不用改，要改也可以。
        "kitsUI \u8981\u9019\u5169\u6b04\uff0c\u9019\u88e1\u53ea\u7528\u4f86\u7d44\u5217\u6b21\u7de8\u865f\uff1b\u53ea\u6709\u4e00\u6b21\u65bd\u6e2c\u7684\u7814\u7a76\u4e0d\u7528\u6539\uff0c\u8981\u6539\u4e5f\u53ef\u4ee5\u3002"
      )
    })
    output$more_table <- shiny::renderUI({
      if (!has_files()) return(NULL)
      shiny::tagList(
        file_table(KITS_STEP1_MORE),
        shiny::tags$small(class = "kit-hint",
          # 原文：實際月齡：kitsUI 的題目表通過年齡要用它。波次標籤（例如 m108g03）不會輸出，
          "\u5be6\u969b\u6708\u9f61\uff1akitsUI \u7684\u984c\u76ee\u8868\u901a\u904e\u5e74\u9f61\u8981\u7528\u5b83\u3002\u6ce2\u6b21\u6a19\u7c64\uff08\u4f8b\u5982 m108g03\uff09\u4e0d\u6703\u8f38\u51fa\uff0c",
          # 原文：只用來組列次編號；列次編號通常讓它自動組成。
          "\u53ea\u7528\u4f86\u7d44\u5217\u6b21\u7de8\u865f\uff1b\u5217\u6b21\u7de8\u865f\u901a\u5e38\u8b93\u5b83\u81ea\u52d5\u7d44\u6210\u3002")
      )
    })

    wave_unit <- shiny::reactive(input$wave_unit %||% "month")
    output$wave_unit_note <- shiny::renderUI({
      if (identical(wave_unit(), "month")) return(NULL)
      shiny::tags$small(
        class = "kit-hint",
        # 原文：作答檔的 wave_m 只放數字，單位不會寫進去。kitsUI 目前把 wave_m 當成月：
        "\u4f5c\u7b54\u6a94\u7684 wave_m \u53ea\u653e\u6578\u5b57\uff0c\u55ae\u4f4d\u4e0d\u6703\u5beb\u9032\u53bb\u3002kitsUI \u76ee\u524d\u628a wave_m \u7576\u6210\u6708\uff1a",
        # 原文：圖的時間軸、依 wave_m 借題庫參數的規則、age50 這類以月計算的結果，
        "\u5716\u7684\u6642\u9593\u8ef8\u3001\u4f9d wave_m \u501f\u984c\u5eab\u53c3\u6578\u7684\u898f\u5247\u3001age50 \u9019\u985e\u4ee5\u6708\u8a08\u7b97\u7684\u7d50\u679c\uff0c",
        # 原文：用年或波次序號時要自己換算或解讀。
        "\u7528\u5e74\u6216\u6ce2\u6b21\u5e8f\u865f\u6642\u8981\u81ea\u5df1\u63db\u7b97\u6216\u89e3\u8b80\u3002"
      )
    })
    output$visit_note <- shiny::renderUI({
      s <- step1()
      if (!has_files() || length(s$problems) > 0) return(NULL)
      first <- kits_apply_typed(dfs()[1], list(map = s$map, typed = purrr::map(s$typed, 1)))
      example <- kitsdata_visit_example(first$dfs[[1]], s$cohort[[1]],
                                        kits_file_map(first$spec$map, s$per_file, 1))
      if (is.null(example)) return(NULL)
      # 原文：作答檔裡的列次編號會長這樣：
      shiny::tags$small(class = "kit-hint", "\u4f5c\u7b54\u6a94\u88e1\u7684\u5217\u6b21\u7de8\u865f\u6703\u9577\u9019\u6a23\uff1a", shiny::tags$code(example))
    })
    # 第一列的世代套用到每一列（在瀏覽器裡改選單；testServer 不會改 input）
    shiny::observeEvent(input$fill_cohort, {
      v <- cells()$cohort[1]
      if (!nzchar(v)) {
        # 原文：第一列的世代還沒有填。
        shiny::showNotification("\u7b2c\u4e00\u5217\u7684\u4e16\u4ee3\u9084\u6c92\u6709\u586b\u3002", type = "warning")
        return()
      }
      keys <- src_keys()
      for (k in seq_along(keys)[-1]) {
        shiny::updateSelectizeInput(session, cell_id("cohort", keys[k]),
                                    choices = c("", names(dfs()[[k]]), v), selected = v)
      }
    })

    step1_msg <- shiny::reactiveVal(NULL)
    output$step1_problems <- shiny::renderUI({
      msg <- step1_msg()
      if (length(msg) == 0) return(NULL)
      # 原文：還差：
      kit_problems(msg, head = "\u9084\u5dee\uff1a")
    })
    shiny::observeEvent(cells(), step1_msg(NULL), ignoreInit = TRUE)
    shiny::observeEvent(input$ok1, {
      p <- step1()$problems
      if (length(p) > 0) {
        step1_msg(p)
        return()
      }
      step1_msg(NULL)
      ok$s1 <- TRUE
      open(if (ok$s2) 3L else 2L)
    })

    # 第 2 步：哪些欄是題目（一開始一題都不納入） ----
    # 題目設定：每次設定都補上預設領域（組名），所以轉換不用先設領域
    settings_rv <- shiny::reactiveVal(kits_settings_empty())
    settings <- function(value) {
      if (missing(value)) settings_rv() else settings_rv(kits_domain_defaults(value))
    }
    upload <- shiny::reactiveVal(NULL)
    cohort_cols <- shiny::reactive({
      purrr::map_chr(purrr::keep(step1()$cohort, function(x) x$type == "column"), "value")
    })
    # 候選欄與每一欄的題目設定，只在資料或代碼清單變了時算一次（2,000 欄約 1 秒）；
    # 第 1 步改對應欄位時只是從裡面拿掉對應到的欄（不用每打一格就重算）
    all_cand <- shiny::reactive(kits_candidate_cols(dfs(), character(0)))
    all_settings <- shiny::reactive({
      kits_item_settings(dfs(), all_cand()$numeric, codes_2(), include = FALSE)
    })
    mapped_cols <- shiny::reactive({
      s <- step1()
      sort(unique(c(kits_mapped_cols(s$map, s$per_file), cohort_cols())))
    })
    candidates <- shiny::reactive({
      a <- all_cand()
      m <- mapped_cols()
      list(numeric = setdiff(a$numeric, m), text = setdiff(a$text, m),
           mixed = a$mixed[!a$mixed$column %in% m, ])
    })
    reverse_rule <- shiny::reactive(input$reverse_rule %||% "suffix")
    shiny::observe({
      a <- all_settings()
      new <- a[a$column %in% candidates()$numeric, ]
      old <- shiny::isolate(settings())
      # 欄沒變（例如第 1 步只改了世代的值）：不用重新同步
      if (identical(new$column, old$column) && identical(new$raw, old$raw) &&
          identical(new$codes, old$codes)) return()
      up <- shiny::isolate(upload())
      if (inherits(up, "error")) up <- NULL
      settings(kits_settings_sync(old, new, up, shiny::isolate(reverse_rule())))
    })
    # 換規則：拿掉用舊規則從原題來的領域，再用新規則套一次題目表
    shiny::observeEvent(input$reverse_rule, {
      up <- upload()
      if (is.null(up) || inherits(up, "error")) return()
      s <- settings()
      # 原文：題目表（原題
      old <- !is.na(s$source) & startsWith(s$source, "\u984c\u76ee\u8868\uff08\u539f\u984c")
      s$domain[old] <- NA_character_
      s$source[old] <- NA_character_
      settings(kits_settings_apply(s, up, reverse = reverse_rule()))
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$item_file, {
      f <- input$item_file
      up <- tryCatch(
        kits_settings_import(read_data_file(f$datapath, f$name)$data),
        error = function(e) {
          # 只留最裡面那一句中文（不要 rlang 的「In index: 37. Caused by error」）
          while (inherits(e$parent, "condition")) e <- e$parent
          simpleError(conditionMessage(e))
        }
      )
      if (!inherits(up, "error")) up$name <- f$name
      upload(up)
      if (!inherits(up, "error")) {
        settings(kits_settings_apply(settings(), up, reverse = reverse_rule()))
      }
    })
    items <- shiny::reactive(kits_settings_items(settings()))
    counts <- shiny::reactive(kits_step2_counts(settings()))

    output$intro2 <- shiny::renderUI({
      shiny::tagList(
        shiny::tags$p(shiny::tags$strong(
          # 原文：先挑出哪些欄是題目；領域和計分在下一步的題目表設定，之後都還可以改。
          "\u5148\u6311\u51fa\u54ea\u4e9b\u6b04\u662f\u984c\u76ee\uff1b\u9818\u57df\u548c\u8a08\u5206\u5728\u4e0b\u4e00\u6b65\u7684\u984c\u76ee\u8868\u8a2d\u5b9a\uff0c\u4e4b\u5f8c\u90fd\u9084\u53ef\u4ee5\u6539\u3002"
        )),
        shiny::tags$p(class = "kit-hint",
          # 原文：下面是看起來像題目的欄（分數是 0–20 的整數），欄名開頭一樣的放在一組。勾選要轉的組。
          "\u4e0b\u9762\u662f\u770b\u8d77\u4f86\u50cf\u984c\u76ee\u7684\u6b04\uff08\u5206\u6578\u662f 0\u201320 \u7684\u6574\u6578\uff09\uff0c\u6b04\u540d\u958b\u982d\u4e00\u6a23\u7684\u653e\u5728\u4e00\u7d44\u3002\u52fe\u9078\u8981\u8f49\u7684\u7d44\u3002")
      )
    })

    # 像題目的欄一組一列：組的結構只在欄變了時重算（勾選、領域改了不重畫）
    groups_all <- shiny::reactiveVal(kits_item_groups(kits_settings_empty(), list()))
    shiny::observe({
      s <- settings()
      g <- kits_item_groups(s, dfs())
      old <- shiny::isolate(groups_all())
      if (!identical(g$stem, old$stem) || !identical(g$cols, old$cols)) groups_all(g)
    })
    grp_page <- shiny::reactiveVal(1L)
    grp_shown <- shiny::reactive({
      g <- groups_all()
      g$k <- seq_len(nrow(g))
      kits_groups_filter(g, input$grp_search)
    })
    grp_pages <- shiny::reactive(max(1L, ceiling(nrow(grp_shown()) / KITS_GROUPS_PAGE)))
    shiny::observeEvent(input$grp_search, grp_page(1L), ignoreInit = TRUE)
    shiny::observeEvent(input$grp_prev, grp_page(max(1L, grp_page() - 1L)))
    shiny::observeEvent(input$grp_next, grp_page(min(grp_pages(), grp_page() + 1L)))
    grp_rows <- shiny::reactive({
      g <- grp_shown()
      p <- min(grp_page(), grp_pages())
      i <- seq_len(nrow(g))
      g[i > (p - 1) * KITS_GROUPS_PAGE & i <= p * KITS_GROUPS_PAGE, ]
    })
    output$grp_page <- shiny::renderText({
      # 原文： 組 · 第 
      paste0(kit_num(nrow(grp_shown())), " \u7d44 \u00b7 \u7b2c ", min(grp_page(), grp_pages()), " / ",
             # 原文： 頁
             grp_pages(), " \u9801")
    })
    # 勾選框的值是組名（不用列號：組的清單變了也不會對錯組）
    grp_sel_prev <- shiny::reactiveVal(character(0))
    group_on <- function(s, g) {
      g$stem[vapply(g$cols, function(c) all(s$include[match(c, s$column)]), logical(1))]
    }
    output$groups_table <- shiny::renderUI({
      g <- grp_rows()
      if (nrow(g) == 0) {
        return(shiny::tags$p(class = "kit-hint", if (nrow(groups_all()) == 0)
          # 原文：沒有看起來像題目的欄。可以上傳題目表，或用欄名挑（下面）。 | 沒有符合的組。
          "\u6c92\u6709\u770b\u8d77\u4f86\u50cf\u984c\u76ee\u7684\u6b04\u3002\u53ef\u4ee5\u4e0a\u50b3\u984c\u76ee\u8868\uff0c\u6216\u7528\u6b04\u540d\u6311\uff08\u4e0b\u9762\uff09\u3002" else "\u6c92\u6709\u7b26\u5408\u7684\u7d44\u3002"))
      }
      s <- shiny::isolate(settings())
      shiny::isolate(grp_sel_prev(group_on(s, g)))
      rows <- purrr::pmap(g, function(stem, cols, n, range, label, labels, n_in, domain, k) {
        on <- all(s$include[match(cols, s$column)])
        shiny::tags$tr(
          shiny::tags$td(shiny::tags$input(
            type = "checkbox", name = ns("grp_sel"), value = stem,
            # 原文：要
            checked = if (on) NA, `aria-label` = paste("\u8981", stem)
          )),
          shiny::tags$td(shiny::tags$strong(kits_group_cols_text(cols)), shiny::tags$br(),
                         # 原文：分數 
                         shiny::tags$small(class = "kit-hint", paste0("\u5206\u6578 ", range))),
          shiny::tags$td(
            class = "kit-group-label",
            # 原文：（沒有題目內容）
            if (nzchar(label)) label else shiny::tags$span(class = "kit-muted", "\uff08\u6c92\u6709\u984c\u76ee\u5167\u5bb9\uff09"),
            # 一組不只一題：可以打開看每一題（同一組裡也可能混了別的概念）
            if (n > 1 && any(nzchar(labels))) shiny::tags$details(
              class = "kit-group-more",
              # 原文：看全部  |  題
              shiny::tags$summary(paste0("\u770b\u5168\u90e8 ", n, " \u984c")),
              shiny::tags$ol(lapply(seq_along(cols), function(j) shiny::tags$li(
                # 原文：（沒有題目內容）
                shiny::tags$code(cols[j]), " ", if (nzchar(labels[j])) labels[j] else "\uff08\u6c92\u6709\u984c\u76ee\u5167\u5bb9\uff09"
              )))
            )
          )
        )
      })
      shiny::tags$div(
        id = ns("grp_sel"), class = "form-group shiny-input-checkboxgroup shiny-input-container",
        style = "width: auto; max-width: 100%;",
        shiny::tags$div(
          class = "shiny-options-group kit-table-scroll",
          shiny::tags$table(
            class = "table kit-groups",
            shiny::tags$thead(shiny::tags$tr(
              # 原文：要 | 欄 | 題目內容（第一題）
              shiny::tags$th("\u8981"), shiny::tags$th("\u6b04"), shiny::tags$th("\u984c\u76ee\u5167\u5bb9\uff08\u7b2c\u4e00\u984c\uff09")
            )),
            shiny::tags$tbody(rows)
          )
        )
      )
    })
    # 各波欄名不同、其實是同一份量表：勾選的組依題目順序合成同一組題號（bsrs1、bsrs2…）
    # 選單只在「納入了哪些組」變了才重畫（打題號時不會被打斷）
    included_groups <- shiny::reactiveVal(character(0))
    shiny::observe({
      g <- groups_all()
      s <- settings()
      on <- if (nrow(g) == 0) character(0) else group_on(s, g)
      if (!identical(on, shiny::isolate(included_groups()))) included_groups(on)
    })
    output$merge_ui <- shiny::renderUI({
      on <- included_groups()
      if (length(on) < 2) return(NULL)
      g <- shiny::isolate(groups_all())
      g <- g[g$stem %in% on, ]
      shiny::tags$div(
        class = "kit-merge",
        # 原文：各波欄名不同、其實是同一份量表？
        shiny::tags$p(shiny::tags$strong("\u5404\u6ce2\u6b04\u540d\u4e0d\u540c\u3001\u5176\u5be6\u662f\u540c\u4e00\u4efd\u91cf\u8868\uff1f"),
                      # 原文：把每一波的那一組選進來，依題目順序合成同一組題號，kits 才能把各波連起來。
                      "\u628a\u6bcf\u4e00\u6ce2\u7684\u90a3\u4e00\u7d44\u9078\u9032\u4f86\uff0c\u4f9d\u984c\u76ee\u9806\u5e8f\u5408\u6210\u540c\u4e00\u7d44\u984c\u865f\uff0ckits \u624d\u80fd\u628a\u5404\u6ce2\u9023\u8d77\u4f86\u3002"),
        shiny::tags$div(
          class = "kit-inline",
          shiny::selectizeInput(
            # 原文：要合成的組（每一波一組）
            ns("merge_groups"), "\u8981\u5408\u6210\u7684\u7d44\uff08\u6bcf\u4e00\u6ce2\u4e00\u7d44\uff09", multiple = TRUE, width = "26rem",
            # 原文：（ |  題）
            choices = stats::setNames(g$stem, paste0(g$stem, "\uff08", g$n, " \u984c\uff09")),
            selected = kits_merge_suggest(g)
          ),
          # 原文：題號的開頭 | 例如 bsrs
          shiny::textInput(ns("merge_name"), "\u984c\u865f\u7684\u958b\u982d", width = "10rem", placeholder = "\u4f8b\u5982 bsrs"),
          # 原文：合成同一組題號
          shiny::actionButton(ns("merge_apply"), "\u5408\u6210\u540c\u4e00\u7d44\u984c\u865f", class = "btn-default")
        ),
        shiny::tags$small(class = "kit-hint",
          # 原文：例如 w1d5_1、w2d5_1、w3c5_1 都會變成題號 bsrs1。已經合成的題號在「看挑出來的題目」裡看得到。
          "\u4f8b\u5982 w1d5_1\u3001w2d5_1\u3001w3c5_1 \u90fd\u6703\u8b8a\u6210\u984c\u865f bsrs1\u3002\u5df2\u7d93\u5408\u6210\u7684\u984c\u865f\u5728\u300c\u770b\u6311\u51fa\u4f86\u7684\u984c\u76ee\u300d\u88e1\u770b\u5f97\u5230\u3002")
      )
    })
    shiny::observeEvent(input$merge_apply, {
      out <- tryCatch(
        kits_merge_groups(settings(), groups_all(), input$merge_groups, input$merge_name),
        error = function(e) e
      )
      if (inherits(out, "error")) {
        shiny::showNotification(conditionMessage(out), type = "error")
        return()
      }
      settings(out)
      # 原文：已經合成  |  組的題號。
      shiny::showNotification(paste0("\u5df2\u7d93\u5408\u6210 ", length(input$merge_groups), " \u7d44\u7684\u984c\u865f\u3002"),
                              type = "message")
    })

    # 勾選：只改使用者這次勾或取消的組（和上一次的值比），其他組不動
    shiny::observeEvent(input$grp_sel, {
      g <- grp_rows()
      new <- input$grp_sel %||% character(0)
      prev <- grp_sel_prev()
      changed <- intersect(union(setdiff(new, prev), setdiff(prev, new)), g$stem)
      grp_sel_prev(new)
      if (length(changed) == 0) return()
      s <- settings()
      for (stem in changed) {
        cols <- g$cols[[match(stem, g$stem)]]
        s <- kits_bulk_include(s, cols, stem %in% new)
      }
      if (!identical(s, settings())) settings(s)
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
    # 設定在別的地方改了（題目表、用欄名挑、第 3 步的題目表）：把這一頁的勾選跟上
    shiny::observe({
      on <- group_on(settings(), grp_rows())
      if (!setequal(on, shiny::isolate(grp_sel_prev()))) {
        grp_sel_prev(on)
        shiny::updateCheckboxGroupInput(session, "grp_sel", selected = if (length(on)) on else character(0))
      }
    })
    # 用欄名挑
    picked <- shiny::reactive(kits_pick_cols(settings()$column, input$pick_prefix))
    output$pick_note <- shiny::renderUI({
      if (!nzchar(trimws(input$pick_prefix %||% ""))) return(NULL)
      # 原文：符合  |  欄
      shiny::tags$small(class = "kit-hint", paste0("\u7b26\u5408 ", length(picked()), " \u6b04",
        # 原文：： | 、
        if (length(picked()) > 0) paste0("\uff1a", paste(utils::head(picked(), 6), collapse = "\u3001"),
                                        # 原文：… | 。
                                        if (length(picked()) > 6) "\u2026"), "\u3002"))
    })
    shiny::observeEvent(input$pick_add, {
      rows <- picked()
      if (length(rows) == 0) {
        # 原文：沒有符合的欄：請填欄名的開頭，例如 w1d5_。
        shiny::showNotification("\u6c92\u6709\u7b26\u5408\u7684\u6b04\uff1a\u8acb\u586b\u6b04\u540d\u7684\u958b\u982d\uff0c\u4f8b\u5982 w1d5_\u3002", type = "warning")
        return()
      }
      settings(kits_bulk_include(settings(), rows, TRUE))
    })
    shiny::observeEvent(input$pick_clear, {
      settings(kits_bulk_include(settings(), settings()$column, FALSE))
    })
    shiny::observe({
      doms <- sort(unique(stats::na.omit(settings()$domain)))
      for (id in c("bulk_domain", "fix_domain")) {
        shiny::updateSelectizeInput(session, id, choices = doms,
                                    selected = shiny::isolate(input[[id]]))
      }
    })

    output$items_status <- shiny::renderUI({
      up <- upload()
      cnt <- counts()
      s <- settings()
      rename <- kits_item_rename(items())
      clash <- kits_rename_problems(dfs(), src_names(), items(), rename)
      up_line <- if (inherits(up, "error")) {
        # 原文：題目表讀不進來
        kit_check("todo", "\u984c\u76ee\u8868\u8b80\u4e0d\u9032\u4f86", conditionMessage(up))
      } else if (!is.null(up)) {
        miss <- setdiff(tolower(up$data$item_id), tolower(s$column))
        kit_check(
          if (cnt$n_cols > 0) "ok" else "todo",
          # 原文：題目表 | ：
          paste0(up$name %||% "\u984c\u76ee\u8868", "\uff1a", cnt$pick_text),
          paste0(
            # 原文：已套用題目表（納入、領域、計分對照）。
            if (up$type == "settings") "\u5df2\u5957\u7528\u984c\u76ee\u8868\uff08\u7d0d\u5165\u3001\u9818\u57df\u3001\u8a08\u5206\u5c0d\u7167\uff09\u3002",
            # 原文：各波欄名不同的同一題，用 item_id_out 合成一題。
            if (length(rename) > 0) paste0("\u5404\u6ce2\u6b04\u540d\u4e0d\u540c\u7684\u540c\u4e00\u984c\uff0c\u7528 item_id_out \u5408\u6210\u4e00\u984c\u3002"),
            # 原文：題目表有  |  題在資料裡找不到（
            if (length(miss) > 0) paste0("\u984c\u76ee\u8868\u6709 ", length(miss), " \u984c\u5728\u8cc7\u6599\u88e1\u627e\u4e0d\u5230\uff08",
                                         # 原文：、
                                         paste(utils::head(miss, 5), collapse = "\u3001"),
                                         # 原文：… | ）。
                                         if (length(miss) > 5) "\u2026", "\uff09\u3002")
          )
        )
      } else if (cnt$n_cols > 0) {
        kit_check("ok", cnt$pick_text, "")
      }
      shiny::tagList(
        if (!is.null(up_line)) kit_checklist(up_line),
        kit_problems(clash, bullets = FALSE),
        kitsdata_mixed_note(candidates()$mixed, src_names())
      )
    })
    output$items_list <- shiny::renderUI({
      it <- items()
      # 原文：還沒有挑題目。
      if (nrow(it) == 0) return(shiny::tags$p(class = "kit-hint", "\u9084\u6c92\u6709\u6311\u984c\u76ee\u3002"))
      out_id <- ifelse(is.na(it$item_id_out), it$column, it$item_id_out)
      tab <- dplyr::summarise(
        dplyr::group_by(tibble::tibble(item = out_id, col = it$column), .data$item),
        # 原文：、
        cols = paste(.data$col, collapse = "\u3001"), .groups = "drop"
      )
      shiny::tags$div(class = "kit-table-scroll", shiny::tags$table(
        class = "table",
        # 原文：題號 | 來自哪幾欄
        shiny::tags$thead(shiny::tags$tr(shiny::tags$th("\u984c\u865f"), shiny::tags$th("\u4f86\u81ea\u54ea\u5e7e\u6b04"))),
        shiny::tags$tbody(purrr::pmap(tab, function(item, cols) shiny::tags$tr(
          shiny::tags$td(shiny::tags$code(item)), shiny::tags$td(cols)
        )))
      ))
    })
    output$text_cols <- shiny::renderUI({
      tx <- candidates()$text
      # 原文：沒有。
      shiny::tags$p(class = "kit-hint", if (length(tx) == 0) "\u6c92\u6709\u3002" else paste0(
        # 原文：文字的欄不能當題目（ |  欄）： | 、
        "\u6587\u5b57\u7684\u6b04\u4e0d\u80fd\u7576\u984c\u76ee\uff08", length(tx), " \u6b04\uff09\uff1a", paste(utils::head(tx, 12), collapse = "\u3001"),
        # 原文：… | 。
        if (length(tx) > 12) "\u2026", "\u3002"
      ))
    })
    shiny::observeEvent(input$ok2, {
      if (counts()$n_cols == 0) {
        # 原文：還沒有挑題目：請上傳題目表，或用欄名挑。
        shiny::showNotification("\u9084\u6c92\u6709\u6311\u984c\u76ee\uff1a\u8acb\u4e0a\u50b3\u984c\u76ee\u8868\uff0c\u6216\u7528\u6b04\u540d\u6311\u3002", type = "warning")
        return()
      }
      ok$s2 <- TRUE
      open(3L)
    })

    # 第 3 步：題目表：領域與計分 ----
    ack <- shiny::reactiveValues(codes = NULL, shift = NULL, shifted = 0, common = FALSE,
                                 dom_default = NULL)
    side_cells <- shiny::reactive({
      s <- step1()
      kits_code_cells(dfs(), unlist(kits_side_cols(s$map, s$per_file)), codes_2())
    })
    side_codes <- shiny::reactive({
      s <- step1()
      kits_side_codes(dfs(), kits_side_cols(s$map, s$per_file), codes_2())
    })
    checks <- shiny::reactive({
      kits_step3_checks(
        settings(), side_cells(), na_applied = identical(working$source(), "na"),
        ack = list(codes = ack$codes, shift = ack$shift, shifted = ack$shifted, common = ack$common,
                   dom_default = ack$dom_default),
        reverse_used = !is.null(upload()) && !inherits(upload(), "error"),
        side_codes = side_codes(),
        file_cols = lapply(dfs(), names)
      )
    })
    n_todo <- shiny::reactive(sum(vapply(checks(), function(x) x$state == "todo", logical(1))))

    output$checks <- shiny::renderUI({
      ch <- checks()
      # 原文：還沒有挑題目。
      if (length(ch) == 0) return(shiny::tags$p(class = "kit-hint", "\u9084\u6c92\u6709\u6311\u984c\u76ee\u3002"))
      btn <- function(id, label) shiny::actionButton(ns(id), label, class = "btn-default btn-choice")
      actions <- list(
        codes = function(x) if (x$state == "todo") list(
          # 原文：到「遺漏值處理」改掉（建議）
          btn("goto_missing", "\u5230\u300c\u907a\u6f0f\u503c\u8655\u7406\u300d\u6539\u6389\uff08\u5efa\u8b70\uff09"),
          # 原文：它們是真的分數
          btn("ack_codes", "\u5b83\u5011\u662f\u771f\u7684\u5206\u6578")
        # 原文：改回來
        ) else if (!is.null(ack$codes)) list(shiny::actionLink(ns("unack_codes"), "\u6539\u56de\u4f86")),
        shift = function(x) if (x$state == "todo") list(
          # 原文：全部減 
          btn("do_shift", paste0("\u5168\u90e8\u6e1b ", kits_id_text(kits_settings_shift_hint(settings())),
                                 # 原文：（建議）
                                 "\uff08\u5efa\u8b70\uff09")),
          # 原文：不用改
          btn("ack_shift", "\u4e0d\u7528\u6539")
        # 原文：改回來
        ) else if (!is.null(ack$shift)) list(shiny::actionLink(ns("unack_shift"), "\u6539\u56de\u4f86")),
        domain = function(x) if (x$state == "todo") list(
          shiny::selectizeInput(ns("fix_domain"), NULL, width = "12rem",
                                choices = sort(unique(stats::na.omit(settings()$domain))),
                                # 原文：選或輸入領域
                                options = list(create = TRUE, placeholder = "\u9078\u6216\u8f38\u5165\u9818\u57df")),
          # 原文：這幾欄都設成這個領域
          btn("do_fix_domain", "\u9019\u5e7e\u6b04\u90fd\u8a2d\u6210\u9019\u500b\u9818\u57df")
        ),
        dom_default = function(x) if (x$state == "todo") list(
          # 原文：在下面的題目表改領域
          btn("go_dom_table", "\u5728\u4e0b\u9762\u7684\u984c\u76ee\u8868\u6539\u9818\u57df"),
          # 原文：這樣就好
          btn("ack_dom", "\u9019\u6a23\u5c31\u597d")
        # 原文：改回來
        ) else if (!is.null(ack$dom_default)) list(shiny::actionLink(ns("unack_dom"), "\u6539\u56de\u4f86")),
        # 原文：不要這幾欄（建議）
        frac = function(x) if (x$state == "todo") list(btn("do_drop_frac", "\u4e0d\u8981\u9019\u5e7e\u6b04\uff08\u5efa\u8b70\uff09")),
        common = function(x) if (x$state == "todo") list(
          # 原文：回第 2 步合成同一組題號（建議）
          btn("go_merge", "\u56de\u7b2c 2 \u6b65\u5408\u6210\u540c\u4e00\u7d44\u984c\u865f\uff08\u5efa\u8b70\uff09"),
          # 原文：它們不是同一份量表
          btn("ack_common", "\u5b83\u5011\u4e0d\u662f\u540c\u4e00\u4efd\u91cf\u8868")
        # 原文：改回來
        ) else if (isTRUE(ack$common)) list(shiny::actionLink(ns("unack_common"), "\u6539\u56de\u4f86"))
      )
      kit_checklist(lapply(ch, function(x) {
        kit_check(x$state, x$title, x$text, if (!is.null(actions[[x$id]])) actions[[x$id]](x))
      }))
    })
    shiny::observeEvent(input$ack_codes, {
      ch <- checks()$codes
      n <- kits_codes_total(settings(), side_cells())
      if (!is.null(ch)) ack$codes <- n
    })
    shiny::observeEvent(input$unack_codes, ack$codes <- NULL)
    shiny::observeEvent(input$ack_common, ack$common <- TRUE)
    # 領域用組名：「這樣就好」記下當時的欄與領域；之後領域改了，就要再決定一次
    shiny::observeEvent(input$ack_dom, {
      s <- settings()
      ack$dom_default <- kits_domain_default_key(s[s$include & lengths(s$raw) > 0, ])
    })
    shiny::observeEvent(input$unack_dom, ack$dom_default <- NULL)
    # 「在下面的題目表改領域」：捲到題目表，「對勾選的列」先選好「設領域」
    shiny::observeEvent(input$go_dom_table, {
      shiny::updateSelectInput(session, "bulk_action", selected = "domain")
      kit_scroll_to("item_table")
    })
    shiny::observeEvent(input$unack_common, ack$common <- FALSE)
    shiny::observeEvent(input$go_merge, if (ok$s1) open(2L))
    shiny::observeEvent(input$do_shift, {
      m <- kits_settings_shift_hint(settings())
      if (is.null(m)) return()
      settings(kits_bulk_shift(settings(), settings()$column[settings()$include], -m))
      ack$shifted <- m
    })
    shiny::observeEvent(input$ack_shift, ack$shift <- kits_settings_shift_hint(settings()))
    shiny::observeEvent(input$unack_shift, ack$shift <- NULL)
    shiny::observeEvent(input$do_fix_domain, {
      d <- trimws(input$fix_domain %||% "")
      if (!nzchar(d)) {
        # 原文：請先選或輸入領域，例如 distress。
        shiny::showNotification("\u8acb\u5148\u9078\u6216\u8f38\u5165\u9818\u57df\uff0c\u4f8b\u5982 distress\u3002", type = "warning")
        return()
      }
      settings(kits_bulk_domain(settings(), kits_settings_undecided(settings()), d))
    })
    shiny::observeEvent(input$do_drop_frac, {
      s <- settings()
      frac <- s$include & purrr::map_lgl(s$raw, function(r) any(r != round(r)))
      settings(kits_bulk_include(s, s$column[frac], FALSE))
    })

    # 題目表：一頁 25 列；勾選存在伺服器（換頁不會不見）
    sel <- shiny::reactiveVal(character(0))
    page <- shiny::reactiveVal(1L)
    tbl_rows <- shiny::reactive({
      s <- settings()
      if (!isTRUE(input$tbl_all)) s <- s[s$include, ]
      s[s$column %in% kitsdata_filter_cols(s$column, input$tbl_search), ]
    })
    n_pages <- shiny::reactive(max(1L, ceiling(nrow(tbl_rows()) / KITS_ITEMS_PAGE)))
    shiny::observeEvent(list(input$tbl_search, input$tbl_all), page(1L), ignoreInit = TRUE)
    shiny::observeEvent(input$tbl_prev, page(max(1L, page() - 1L)))
    shiny::observeEvent(input$tbl_next, page(min(n_pages(), page() + 1L)))
    page_rows <- shiny::reactive({
      r <- tbl_rows()
      p <- min(page(), n_pages())
      r[seq_len(nrow(r)) > (p - 1) * KITS_ITEMS_PAGE & seq_len(nrow(r)) <= p * KITS_ITEMS_PAGE, ]
    })
    # 原文：第  |  頁
    output$tbl_page <- shiny::renderText(paste0("\u7b2c ", min(page(), n_pages()), " / ", n_pages(), " \u9801"))
    output$items_title <- shiny::renderText({
      cnt <- counts()
      dom <- cnt$by_domain
      # 原文：題目表（ |  欄
      paste0("\u984c\u76ee\u8868\uff08", kit_num(cnt$n_cols), " \u6b04",
             # 原文： ·  |  題
             if (length(dom) > 0) paste0(" \u00b7 ", paste0(names(dom), " ", as.integer(dom), " \u984c",
                                                       # 原文：、 | ）
                                                       collapse = "\u3001")), "\uff09")
    })
    # 題目表的下載（轉換前就可以下載；結果區也有一個，內容一樣）：按下載當下的設定
    table_csv <- function(file) {
      readr::write_excel_csv(kits_settings_export(settings()), file, na = "")
    }
    output$table_dl_ui <- shiny::renderUI(kit_download_button(
      # 原文：下載題目表（CSV）
      session, "table_dl", "\u4e0b\u8f09\u984c\u76ee\u8868\uff08CSV\uff09", KITS_SETTINGS_FILE, table_csv,
      class = "btn-default btn-sm"
    ))
    output$items_table <- shiny::renderUI({
      kits_items_page_html(ns("sel"), page_rows(), shiny::isolate(sel()))
    })
    shiny::observeEvent(input$sel, {
      here <- page_rows()$column
      sel(union(setdiff(sel(), here), intersect(input$sel %||% character(0), here)))
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
    set_sel <- function(rows) {
      sel(rows)
      shiny::updateCheckboxGroupInput(session, "sel", selected = intersect(rows, page_rows()$column))
    }
    shiny::observeEvent(input$tbl_sel_all, set_sel(tbl_rows()$column))
    shiny::observeEvent(input$tbl_sel_none, set_sel(character(0)))
    # 原文：已勾選  |  列
    output$sel_count <- shiny::renderText(paste0("\u5df2\u52fe\u9078 ", length(intersect(sel(), settings()$column)), " \u5217"))

    shiny::observeEvent(input$bulk_apply, {
      rows <- intersect(sel(), settings()$column)
      if (length(rows) == 0) {
        # 原文：請先在表裡勾選要改的列。
        shiny::showNotification("\u8acb\u5148\u5728\u8868\u88e1\u52fe\u9078\u8981\u6539\u7684\u5217\u3002", type = "warning")
        return()
      }
      s <- settings()
      out <- tryCatch(switch(
        input$bulk_action %||% "domain",
        domain = {
          d <- trimws(input$bulk_domain %||% "")
          # 原文：請先選或輸入領域，例如 lan。
          if (!nzchar(d)) kit_abort("\u8acb\u5148\u9078\u6216\u8f38\u5165\u9818\u57df\uff0c\u4f8b\u5982 lan\u3002")
          kits_bulk_domain(s, rows, d)
        },
        shift = {
          by <- input$bulk_shift
          if (is.null(by) || is.na(by) || by != round(by) || by == 0) {
            # 原文：加減要是不等於 0 的整數，例如 -1。
            kit_abort("\u52a0\u6e1b\u8981\u662f\u4e0d\u7b49\u65bc 0 \u7684\u6574\u6578\uff0c\u4f8b\u5982 -1\u3002")
          }
          kits_bulk_shift(s, rows, by)
        },
        reverse = {
          txt <- trimws(input$bulk_max %||% "")
          top <- if (nzchar(txt)) suppressWarnings(as.numeric(txt)) else NA_real_
          # 原文：最高分要是數字，或留空白（用各題目前的最高分）。
          if (nzchar(txt) && is.na(top)) kit_abort("\u6700\u9ad8\u5206\u8981\u662f\u6578\u5b57\uff0c\u6216\u7559\u7a7a\u767d\uff08\u7528\u5404\u984c\u76ee\u524d\u7684\u6700\u9ad8\u5206\uff09\u3002")
          kits_bulk_reverse(s, rows, top)
        },
        merge = {
          from <- kits_parse_values(input$merge_from)
          to <- input$merge_to
          # 原文：請填要合併的原分數，例如 4、5。
          if (length(from) == 0) kit_abort("\u8acb\u586b\u8981\u5408\u4f75\u7684\u539f\u5206\u6578\uff0c\u4f8b\u5982 4\u30015\u3002")
          # 原文：請填合併後的計分，例如 3。
          if (is.null(to) || is.na(to)) kit_abort("\u8acb\u586b\u5408\u4f75\u5f8c\u7684\u8a08\u5206\uff0c\u4f8b\u5982 3\u3002")
          kits_bulk_merge(s, rows, from, to)
        },
        restore = kits_bulk_restore(s, rows),
        exclude = kits_bulk_include(s, rows, FALSE),
        include = kits_bulk_include(s, rows, TRUE),
        out_prefix = kits_bulk_out_prefix(s, rows, trimws(input$out_pattern %||% "")),
        out_reset = kits_bulk_out_reset(s, rows)
      ), error = function(e) e)
      if (inherits(out, "error")) {
        shiny::showNotification(conditionMessage(out), type = "error")
      } else {
        settings(out)
      }
    })

    # 步驟標頭 ----
    # 畫法是 kitDesign 的 kit_step_head()；哪一步打開的規則留在這裡（第 3 步沒有「對，下一步」、
    # 轉換完全部收起、第 1 步要先過檢查），所以不用 kit_stepcards_server()
    head_ui <- function(k, title, summary) {
      state <- if (open() == k) "current" else if (k == 1 && ok$s1 || k == 2 && ok$s2 ||
                                                  k == 3 && open() == 0L) "done" else "locked"
      can_edit <- state == "done" && (k == 1 || ok$s1 && (k == 2 || ok$s2))
      kit_step_head(k, title, state, summary = summary,
                    edit_id = if (can_edit) ns(paste0("edit", k)),
                    title_id = ns(paste0("steptitle", k)))
    }
    output$head1 <- shiny::renderUI(head_ui(
      # 原文：每個檔案是哪個世代、哪個波次
      1, "\u6bcf\u500b\u6a94\u6848\u662f\u54ea\u500b\u4e16\u4ee3\u3001\u54ea\u500b\u6ce2\u6b21",
      kits_step1_summary(cells(), length(src_keys()), wave_unit(), step1())
    ))
    # 原文：哪些欄是題目
    output$head2 <- shiny::renderUI(head_ui(2, "\u54ea\u4e9b\u6b04\u662f\u984c\u76ee", counts()$pick_text))
    # 原文：題目表：領域與計分
    output$head3 <- shiny::renderUI(head_ui(3, "\u984c\u76ee\u8868\uff1a\u9818\u57df\u8207\u8a08\u5206", kits_step3_summary(checks())))

    # 轉換 ----
    spec <- shiny::reactive({
      s <- step1()
      kits_spec(s$cohort, s$map, items(), kits_settings_undecided(settings()), s$typed,
                codes_2(), s$per_file, input$value_labels %||% "code")
    })
    missing_steps <- shiny::reactive({
      # 原文：第 1 步確認
      c(if (!ok$s1) "\u7b2c 1 \u6b65\u78ba\u8a8d",
        # 原文：第 2 步確認 | 第 2 步挑題目
        if (!ok$s2) (if (counts()$n_cols > 0) "\u7b2c 2 \u6b65\u78ba\u8a8d" else "\u7b2c 2 \u6b65\u6311\u984c\u76ee"),
        # 原文：第 3 步有  |  件事要決定
        if (ok$s1 && ok$s2 && n_todo() > 0) paste0("\u7b2c 3 \u6b65\u6709 ", n_todo(), " \u4ef6\u4e8b\u8981\u6c7a\u5b9a"))
    })
    output$ready <- shiny::renderUI({
      if (!is.null(result())) return(NULL)
      m <- missing_steps()
      shiny::tags$p(
        class = paste("kit-ready", if (length(m) > 0) "todo"),
        # 原文：還差： | 、 | 。 | 三步都好了。
        if (length(m) > 0) paste0("\u9084\u5dee\uff1a", paste(m, collapse = "\u3001"), "\u3002") else "\u4e09\u6b65\u90fd\u597d\u4e86\u3002"
      )
    })
    output$run_ui <- shiny::renderUI({
      if (!is.null(result())) return(NULL)
      # 原文：轉換成作答檔
      b <- shiny::actionButton(ns("run"), "\u8f49\u63db\u6210\u4f5c\u7b54\u6a94", class = "btn-primary")
      if (length(missing_steps()) > 0) b <- htmltools::tagAppendAttributes(b, disabled = NA)
      b
    })

    # 主要按鈕、狀態框、通知、結果過期：kit_run_tab_server()（「還差」與按鈕是上面這一頁自己的）
    tab <- kit_run_tab_server(
      # 原文：轉換成作答檔
      input, output, session, "\u8f49\u63db\u6210\u4f5c\u7b54\u6a94",
      stale_on = function() list(spec(), dfs()),
      guard = function() {
        m <- missing_steps()
        # 原文：還不能轉換： | 、 | 。
        if (length(m) > 0) kit_status("error", paste0("\u9084\u4e0d\u80fd\u8f49\u63db\uff1a", paste(m, collapse = "\u3001"), "\u3002"))
      },
      # 原文：正在轉換…
      progress = "\u6b63\u5728\u8f49\u63db\u2026",
      run = function() {
        problems <- step1()$problems
        # 原文：第 1 步還有問題：
        if (length(problems) > 0) return(kit_status("error", "\u7b2c 1 \u6b65\u9084\u6709\u554f\u984c\uff1a", warnings = problems))
        out <- kits_build(dfs(), src_names(), spec())
        if (length(out$problems) > 0) {
          return(kit_status(
            "error",
            # 原文：有  |  個問題要先處理，處理後再按一次「轉換成作答檔」：
            paste0("\u6709 ", length(out$problems), " \u500b\u554f\u984c\u8981\u5148\u8655\u7406\uff0c\u8655\u7406\u5f8c\u518d\u6309\u4e00\u6b21\u300c\u8f49\u63db\u6210\u4f5c\u7b54\u6a94\u300d\uff1a"),
            warnings = out$problems, notes = out$notes
          ))
        }
        # 長格式不在這裡做出來：下載時才一段一段寫進 .csv.gz（省記憶體）
        out
      },
      done = function(out) {
        open(0L)
        kit_done_status(
          paste0(
            # 原文：作答檔  |  列：
            "\u4f5c\u7b54\u6a94 ", kit_num(out$n_rows), " \u5217\uff1a",
            # 原文： 個列次、 |  題。
            kit_num(out$n_visits), " \u500b\u5217\u6b21\u3001", out$n_items, " \u984c\u3002"
          ),
          # 第 3 步已經按了「它們是真的分數」：遺漏值代碼不用再提醒一次
          warnings = if (!is.null(ack$codes)) {
            # 原文：題有疑似遺漏值代碼
            out$warnings[!grepl("\u984c\u6709\u7591\u4f3c\u907a\u6f0f\u503c\u4ee3\u78bc", out$warnings, fixed = TRUE)]
          } else out$warnings,
          notes = out$notes
        )
      },
      # 原文：轉換
      error = function(e) kit_error_status(e, "\u8f49\u63db", kit_upload_todo(ns, length(files$keys()))),
      result_ui = function(res) {
        kits_name <- timestamped_name("kits_long")
        shiny::tagList(
          shiny::tags$div(
            class = "kit-downloads",
            kit_download_button(
              # 原文：下載作答檔（.csv.gz）
              session, "download_gz", "\u4e0b\u8f09\u4f5c\u7b54\u6a94\uff08.csv.gz\uff09", kits_name("csv.gz"),
              function(file) kitsdata_download(res$prep, file, write_kits_long_gz),
              class = "btn-primary"
            ),
            # 原文：開啟 kitsUI，上傳這個檔 →
            kit_kitsui_link("\u958b\u555f kitsUI\uff0c\u4e0a\u50b3\u9019\u500b\u6a94 \u2192", class = "kit-next-link")
          ),
          # 原文：每個世代、每個領域有幾列
          shiny::h5("\u6bcf\u500b\u4e16\u4ee3\u3001\u6bcf\u500b\u9818\u57df\u6709\u5e7e\u5217"),
          kit_table_scroll(shiny::tableOutput(ns("summary"))),
          kit_details_row(
            kit_detail(
              # 原文：作答檔預覽
              "\u4f5c\u7b54\u6a94\u9810\u89bd",
              # 原文：前  |  列。
              shiny::tags$small(class = "kit-hint", paste0("\u524d ", KIT_PREVIEW_ROWS * 2, " \u5217\u3002")),
              shiny::tags$div(class = "kit-table-scroll", shiny::tableOutput(ns("preview")))
            ),
            kit_detail(
              # 原文：這次的題目表（下次在第 2 步上傳）
              "\u9019\u6b21\u7684\u984c\u76ee\u8868\uff08\u4e0b\u6b21\u5728\u7b2c 2 \u6b65\u4e0a\u50b3\uff09",
              shiny::tags$p(class = "kit-hint",
                # 原文：下一波在第 2 步上傳這個檔，就會套用同樣的題目、領域與計分對照。
                "\u4e0b\u4e00\u6ce2\u5728\u7b2c 2 \u6b65\u4e0a\u50b3\u9019\u500b\u6a94\uff0c\u5c31\u6703\u5957\u7528\u540c\u6a23\u7684\u984c\u76ee\u3001\u9818\u57df\u8207\u8a08\u5206\u5c0d\u7167\u3002"),
              shiny::uiOutput(ns("settings_dl_ui"), inline = TRUE)
            ),
            kit_detail(
              # 原文：其他下載
              "\u5176\u4ed6\u4e0b\u8f09",
              kit_download_button(
                # 原文：下載未壓縮的 CSV
                session, "download_csv", "\u4e0b\u8f09\u672a\u58d3\u7e2e\u7684 CSV", kits_name("csv"),
                function(file) kitsdata_download(res$prep, file, write_kits_long_csv),
                class = "btn-default btn-sm"
              ),
              shiny::tags$small(
                class = "kit-hint",
                # 原文： 未壓縮約  | 。
                paste0(" \u672a\u58d3\u7e2e\u7d04 ", kits_size_text(res$csv_mb * 1024^2), "\u3002"),
                # 原文：kitsUI 可以直接讀 .csv.gz（單檔上限  |  MB）。
                "kitsUI \u53ef\u4ee5\u76f4\u63a5\u8b80 .csv.gz\uff08\u55ae\u6a94\u4e0a\u9650 ", KITSUI_UPLOAD_MB, " MB\uff09\u3002"
              )
            ),
            # 原文：kits 作答檔的格式
            kit_detail("kits \u4f5c\u7b54\u6a94\u7684\u683c\u5f0f", kitsdata_spec_ui()),
            kit_code_panel(ns("code"))
          )
        )
      },
      # page_code：同一個函式也交給 app 的流程紀錄（pipeline_log_server()）
      code = page_code <- function() {
        text_fix <- kits_text_fix(dfs(), items()$column)
        s <- data_src()
        if (use_stack()) {
          f <- list(file = "stacked", wave = "stacked", obj = "stacked")
          # 原文：# 先執行「直向波次疊加」的「用 R 怎麼做」，得到 stacked
          read_lines <- "# \u5148\u57f7\u884c\u300c\u76f4\u5411\u6ce2\u6b21\u758a\u52a0\u300d\u7684\u300c\u7528 R \u600e\u9ebc\u505a\u300d\uff0c\u5f97\u5230 stacked"
        } else {
          f <- code_files_from(files)
          if (length(s$idx) > 0 && length(s$idx) < length(f$file)) {
            for (k in c("file", "wave", "cols", "encoding", "types")) {
              if (!is.null(f[[k]])) f[[k]] <- f[[k]][s$idx]
            }
            f$obj <- paste0("data_", seq_along(f$file))
          }
          read_lines <- NULL
        }
        list(tidy = code_kitsdata(
          f, spec(), na_applied = identical(working$source(), "na"),
          text_fix = text_fix, read_lines = read_lines,
          source_lines = code_kitsdata_source(f$obj, s$tf)
        ), base = NULL)
      },
      code_note = KIT_CODE_NOTE_JOIN
    )
    result <- tab$result

    output$summary <- shiny::renderTable(result()$summary, digits = 0, format.args = list(big.mark = ","))
    output$preview <- shiny::renderTable(
      kit_preview_table(result()$preview, n = KIT_PREVIEW_ROWS * 2),
      striped = TRUE, spacing = "s"
    )
    # 檔名固定；內容是按下載當下的設定
    output$settings_dl_ui <- shiny::renderUI(kit_download_button(
      # 原文：下載題目表（CSV）
      session, "settings_dl", "\u4e0b\u8f09\u984c\u76ee\u8868\uff08CSV\uff09", KITS_SETTINGS_FILE, table_csv,
      class = "btn-default btn-sm"
    ))

    # 這一頁的狀態（步驟分頁打 ✓ 用）
    list(state = tab$state, result = tab$result, code = page_code)
  })
}

#' 在某些檔案是文字的題目欄：哪個檔案、哪一欄（那個檔案的這一題不轉）
#' @noRd
kitsdata_mixed_note <- function(mixed, file_names) {
  if (is.null(mixed) || nrow(mixed) == 0) return(NULL)
  # 原文：（ | ）
  shown <- paste0(mixed$column, "\uff08", file_names[mixed$file], "\uff09")
  shiny::tags$p(class = "small text-warning", paste0(
    # 原文： 個題目欄在部分檔案是文字： | 、
    nrow(mixed), " \u500b\u984c\u76ee\u6b04\u5728\u90e8\u5206\u6a94\u6848\u662f\u6587\u5b57\uff1a", paste(utils::head(shown, 8), collapse = "\u3001"),
    # 原文：… | 。這些檔案的這一題不轉，其他檔案照常轉。
    if (nrow(mixed) > 8) "\u2026", "\u3002\u9019\u4e9b\u6a94\u6848\u7684\u9019\u4e00\u984c\u4e0d\u8f49\uff0c\u5176\u4ed6\u6a94\u6848\u7167\u5e38\u8f49\u3002"
  ))
}

#' 第一個檔案第一列的 visit_id 例子（看得出怎麼組）
#' @noRd
kitsdata_visit_example <- function(df, cohort, map) {
  if (nrow(df) == 0 || is.null(cohort) || !nzchar(cohort$value %||% "")) return(NULL)
  if (identical(cohort$type, "column") && !cohort$value %in% names(df)) return(NULL)
  needed <- kits_mapped_cols(map)
  if (!all(needed %in% names(df))) return(NULL)
  one <- df[1, , drop = FALSE]
  co <- if (identical(cohort$type, "column")) kits_id_text(one[[cohort$value]]) else
    cohort$value
  persons <- tibble::tibble(
    cohort = co,
    subj_id = if (nzchar(map$subj_id %||% "")) kits_id_text(one[[map$subj_id]]) else NA,
    wave_m = if (nzchar(map$wave_m %||% "")) as.numeric(unclass(one[[map$wave_m]])) else NA_real_
  )
  out <- kits_visit_ids(persons, one, map)
  if (is.na(out)) NULL else out
}

#' kits 作答檔的欄位表（欄位、必填、意思），放在結果下面的「kits 作答檔的格式」
#' @noRd
kitsdata_spec_ui <- function() {
  rows <- purrr::pmap(KITS_COLUMNS, function(column, type, required, from, meaning) {
    shiny::tags$tr(
      shiny::tags$td(shiny::tags$code(column)),
      # 原文：是 | 否
      shiny::tags$td(if (required) "\u662f" else "\u5426"),
      # 原文：文字 | 數字 | 整數
      shiny::tags$td(c(c = "\u6587\u5b57", d = "\u6578\u5b57", i = "\u6574\u6578")[[type]]),
      shiny::tags$td(meaning)
    )
  })
  shiny::tagList(
    # 原文：作答檔固定 9 個欄位（名稱、順序、型別都固定）：
    shiny::tags$p(class = "kit-hint", "\u4f5c\u7b54\u6a94\u56fa\u5b9a 9 \u500b\u6b04\u4f4d\uff08\u540d\u7a31\u3001\u9806\u5e8f\u3001\u578b\u5225\u90fd\u56fa\u5b9a\uff09\uff1a"),
    shiny::tags$table(
      class = "table table-sm kit-example-table",
      shiny::tags$thead(shiny::tags$tr(
        # 原文：欄位 | 必填 | 型別
        shiny::tags$th("\u6b04\u4f4d"), shiny::tags$th("\u5fc5\u586b"), shiny::tags$th("\u578b\u5225"),
        # 原文：意思
        shiny::tags$th("\u610f\u601d")
      )),
      shiny::tags$tbody(rows)
    ),
    shiny::tags$small(
      class = "kit-hint",
      # 原文：不是必填、也沒有對應的欄（subj_id、sex、age_m）仍會輸出，內容是空白。
      "\u4e0d\u662f\u5fc5\u586b\u3001\u4e5f\u6c92\u6709\u5c0d\u61c9\u7684\u6b04\uff08subj_id\u3001sex\u3001age_m\uff09\u4ecd\u6703\u8f38\u51fa\uff0c\u5167\u5bb9\u662f\u7a7a\u767d\u3002"
    )
  )
}

#' 檔案大小的文字：小於 1 MB 時用 KB
#' @noRd
kits_size_text <- function(bytes) {
  # 原文：—
  if (is.na(bytes)) return("\u2014")
  if (bytes < 1024^2) sprintf("%.0f KB", bytes / 1024) else sprintf("%.1f MB", bytes / 1024^2)
}

# 下載：一段一段寫出長格式，顯示進度
kitsdata_download <- function(prep, file, writer) {
  # 原文：正在寫出作答檔…
  shiny::withProgress(message = "\u6b63\u5728\u5beb\u51fa\u4f5c\u7b54\u6a94\u2026", value = 0, {
    writer(prep = prep, file = file, progress = function(p) shiny::setProgress(p))
  })
}

# 數字的文字（和以前一樣：format(trim, drop0trailing, scientific = FALSE)）
kits_num_format <- function(x) {
  format(x, trim = TRUE, drop0trailing = TRUE, scientific = FALSE)
}

# 整數（不會被寫成科學記號）的分數
kits_num_is_int <- function(x) is.finite(x) & x == round(x) & abs(x) < 1e15

#' 每個數字單獨寫成文字，和 kits_num_format() 一個一個寫的結果相同
#'
#' 整數用 sprintf() 一次寫完（-0 寫成 0）；有小數的才一個一個 format()。
#' @noRd
kits_num_text <- function(x) {
  out <- sprintf("%.0f", x)
  out[!is.na(x) & x == 0] <- "0"
  slow <- which(!kits_num_is_int(x))
  out[slow] <- vapply(x[slow], kits_num_format, character(1))
  out
}

# 依欄名篩選：去掉前後空白、不分大小寫、包含就算（和 kitsdata_filter_js() 一樣）
kitsdata_filter_cols <- function(cols, text) {
  f <- tolower(trimws(text %||% ""))
  if (!nzchar(f)) cols else cols[grepl(f, tolower(cols), fixed = TRUE)]
}

# 計分對照最多直接顯示幾種原分數（再多就收起來）
KITS_MAP_SHOW_MAX <- 12
