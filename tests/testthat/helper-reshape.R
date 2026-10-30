# 模擬的 kitsUI 分數表：一列 = 一次施測（visit）× 一個領域
#
# 兩個世代（M03、M36）、兩個領域（lan、motor）；
# M03_002_24 沒有 motor 的列；M36 的 motor 還沒放上 D 量尺（d_score 空白）。
sim_kitsui_scores <- function() {
  visits <- tibble::tibble(
    visit_id = c("M03_001_12", "M03_001_24", "M03_002_12", "M03_002_24",
                 "M36_011_48", "M36_012_48"),
    subj_id = c("M03_001", "M03_001", "M03_002", "M03_002", "M36_011", "M36_012"),
    cohort = c("M03", "M03", "M03", "M03", "M36", "M36"),
    wave_m = c(12, 24, 12, 24, 48, 48),
    age_m = c(12.3, 24.1, 11.8, 24.4, 48.2, 47.9),
    sex = c(1, 1, 2, 2, 1, 2)
  )
  rows <- tidyr::expand_grid(visits, domain = c("lan", "motor")) |>
    dplyr::filter(!(visit_id == "M03_002_24" & domain == "motor"))
  n <- nrow(rows)
  is_m36 <- rows$cohort == "M36"
  rows |>
    dplyr::mutate(
      round = 1L,
      bank = paste(domain, cohort),
      scale = dplyr::if_else(is_m36 & domain == "motor", "motor M36", "D"),
      raw = seq_len(n) + 10L,
      ability = round(seq(-1.5, 1.5, length.out = n), 3),
      ability_se = 0.3,
      d_score = dplyr::if_else(scale == "D", 40 + wave_m / 2 + seq_len(n), NA_real_),
      d_score_se = dplyr::if_else(scale == "D", 1.5, NA_real_),
      se_link = 0.1,
      se_total = 0.32
    ) |>
    dplyr::relocate(round, .after = domain)
}
