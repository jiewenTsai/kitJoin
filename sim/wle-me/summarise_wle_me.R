# =============================================================================
# Summarise sim_wle_me.R output: tables + figures
# Usage: Rscript summarise_wle_me.R wle_me_sim_R200.rds
# Writes: results/summary.csv, results/diagnostics.csv,
#         results/fig_outcome.png, results/fig_predictor.png
# =============================================================================

suppressPackageStartupMessages({
  library(SimDesign)
  library(dplyr)
  library(ggplot2)
})

args <- commandArgs(trailingOnly = TRUE)
file <- if (length(args) >= 1) args[1] else "wle_me_sim_R200.rds"
dir.create("results", showWarnings = FALSE)

res <- readRDS(file)
raw <- SimResults(res)
if (!is.data.frame(raw))   # older SimDesign: list of {condition, results}
  raw <- bind_rows(lapply(raw, function(x) cbind(as.data.frame(x$condition)[rep(1, nrow(x$results)), c("I", "r_tx")], x$results)))
raw <- as.data.frame(raw)

B_TH <- 0.5; B_X1 <- 0.3
truth <- c(bx = B_TH, sigma = sqrt(1 - B_TH^2), bth = B_TH, bx1 = B_X1)
labels <- c(
  oracle = "Oracle (true θ)",
  naive  = "Naive WLE (lm)",
  se_het = "brms se(wle_se)",
  se_con = "brms se(constant SE)",
  tam_lr = "TAM latent regression (items)",
  me_het = "brms me(wle, wle_se)",
  me_con = "brms me(wle, constant SE)",
  mi_het = "brms mi(wle_se) | x1",
  mi_con = "brms mi(constant SE) | x1",
  rasch  = "Rasch likelihood | x1 (MUPPET)")
par_lab <- c(bx = "β(x → θ)", sigma = "residual SD of θ", bth = "β(θ → y)", bx1 = "β(x1 → y)")

# ---- long format ----
est_cols <- grep("__est$", names(raw), value = TRUE)
long <- bind_rows(lapply(est_cols, function(k) {
  base <- sub("__est$", "", k); p <- strsplit(base, "__")[[1]]
  col <- function(s) if (is.null(raw[[paste0(base, s)]])) NA_real_ else raw[[paste0(base, s)]]
  data.frame(I = raw$I, r_tx = raw$r_tx, method = p[1], param = p[2],
             est = raw[[k]], se = col("__se"), lo = col("__lo"), hi = col("__hi"))
}))
long$case  <- ifelse(long$param %in% c("bx", "sigma"), "outcome", "predictor")
long$truth <- truth[long$param]
# the outcome case does not depend on r_tx: pool it
long$r_tx[long$case == "outcome"] <- NA

summ <- long |>
  group_by(case, param, I, r_tx, method) |>
  summarise(n = n(), truth = first(truth), mean = mean(est),
            relbias = 100 * (mean(est) / first(truth) - 1),
            mcse_relbias = 100 * sd(est) / sqrt(n()) / first(truth),
            empSD = sd(est), meanSE = mean(se), SE_ratio = mean(se) / sd(est),
            RMSE = sqrt(mean((est - first(truth))^2)),
            cover = if (all(is.na(lo))) NA_real_ else 100 * mean(lo <= first(truth) & first(truth) <= hi, na.rm = TRUE),
            .groups = "drop") |>
  mutate(label = labels[method]) |>
  arrange(case, param, r_tx, I, match(method, names(labels)))
write.csv(summ, "results/summary.csv", row.names = FALSE)

# reliability of the WLE per condition
info <- data.frame(I = raw$I, rel_out = raw$info__out__rel__val, rel_pred = raw$info__pred__rel__val) |>
  group_by(I) |> summarise(WLE_rel = mean(c(rel_out, rel_pred)), .groups = "drop")
print(info)

# MCMC diagnostics
diag_cols <- grep("__diag__rhat$", names(raw), value = TRUE)
diag <- bind_rows(lapply(diag_cols, function(k) {
  m <- sub("__diag__rhat$", "", k)
  data.frame(I = raw$I, r_tx = raw$r_tx, method = m,
             rhat = raw[[k]], ndiv = raw[[paste0(m, "__diag__ndiv")]])
})) |>
  group_by(method, I, r_tx) |>
  summarise(max_rhat = max(rhat), pct_rhat_gt_1.01 = 100 * mean(rhat > 1.01),
            pct_any_divergent = 100 * mean(ndiv > 0), .groups = "drop")
write.csv(diag, "results/diagnostics.csv", row.names = FALSE)

options(width = 160)
print(as.data.frame(summ |> select(case, param, I, r_tx, label, relbias, mcse_relbias, SE_ratio, RMSE, cover)), digits = 3)
print(as.data.frame(diag), digits = 3)

# ---- figures ----
pal <- c("10" = "#2a78d6", "20" = "#eb6834", "40" = "#1baf7a")
plot_case <- function(which, file, width, height) {
  d <- summ |> filter(case == which) |>
    mutate(label = factor(label, levels = rev(labels)),
           param = factor(par_lab[param], levels = par_lab),
           I = factor(I),
           rtx = if (which == "predictor") paste0("cor(θ, x1) = ", r_tx) else "pooled")
  pd <- bind_rows(
    d |> transmute(label, param, I, rtx, metric = "Relative bias (%)", value = relbias,
                   lo = relbias - 1.96 * mcse_relbias, hi = relbias + 1.96 * mcse_relbias),
    d |> filter(!is.na(cover)) |>
      transmute(label, param, I, rtx, metric = "95% interval coverage (%)", value = cover,
                lo = NA_real_, hi = NA_real_))
  pd$metric <- factor(pd$metric, levels = c("Relative bias (%)", "95% interval coverage (%)"))
  ref <- data.frame(metric = factor(levels(pd$metric), levels = levels(pd$metric)), x = c(0, 95))
  g <- ggplot(pd, aes(value, label, colour = I)) +
    geom_vline(data = ref, aes(xintercept = x), colour = "#898781", linewidth = 0.4) +
    geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0, linewidth = 0.5,
                   position = position_dodge(width = 0.6), na.rm = TRUE) +
    geom_point(size = 2.2, position = position_dodge(width = 0.6)) +
    scale_colour_manual(values = pal, name = "Items") +
    labs(x = NULL, y = NULL,
         title = if (which == "outcome") "θ as outcome: wle ~ x" else "θ as predictor: y ~ θ + x1") +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(colour = "#e1e0d9", linewidth = 0.3),
          strip.text = element_text(face = "bold"), legend.position = "top",
          panel.spacing.x = unit(1.5, "lines"),
          plot.title = element_text(face = "bold"))
  g <- if (which == "predictor") g + facet_grid(param ~ metric + rtx, scales = "free_x")
       else g + facet_grid(param ~ metric, scales = "free_x")
  ggsave(file, g, width = width, height = height, dpi = 150, bg = "white")
}
plot_case("outcome",   "results/fig_outcome.png",   9, 5)
plot_case("predictor", "results/fig_predictor.png", 13, 7)
cat("Wrote results/summary.csv, results/diagnostics.csv, results/fig_*.png\n")
