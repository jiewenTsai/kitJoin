# =============================================================================
# Naive WLE vs brms se()/me()/mi() vs exact Rasch likelihood
# SimDesign + cmdstanr
#
# Usage:  Rscript sim_wle_me.R <replications> <ncores>
#         Rscript sim_wle_me.R 2 2        # smoke test
#         Rscript sim_wle_me.R 200 20     # full run
#
# Re-running the same command after a crash resumes from SimDesign's temp file.
# =============================================================================

suppressPackageStartupMessages({
  library(SimDesign)
  library(brms)
  library(cmdstanr)
})

args   <- commandArgs(trailingOnly = TRUE)
REPS   <- if (length(args) >= 1) as.integer(args[1]) else 2L
NCORES <- if (length(args) >= 2) as.integer(args[2]) else 2L

# ---- Design ------------------------------------------------------------------
# I    : number of Rasch items (difficulties evenly spaced on [-2, 2])
# r_tx : cor(theta, x1) in the predictor case (outcome case ignores it)
Design <- createDesign(I    = c(10, 20, 40),
                       r_tx = c(0, 0.5))

# ---- Stan models (compiled once, here, before workers start) -----------------
# Explicit priors keep the brms Stan code independent of the data, so one
# executable serves every replication; only standata() is rebuilt per rep.
stan_dir <- file.path(getwd(), "stan")
dir.create(stan_dir, showWarnings = FALSE)

pri_se <- c(prior(normal(0, 5), class = Intercept), prior(normal(0, 5), class = b),
            prior(normal(0, 3), class = sigma))
pri_me <- c(pri_se, prior(normal(0, 5), class = meanme), prior(normal(0, 3), class = sdme))
pri_mi <- c(prior(normal(0, 5), class = Intercept, resp = wle), prior(normal(0, 5), class = b, resp = wle),
            prior(normal(0, 3), class = sigma, resp = wle),
            prior(normal(0, 5), class = Intercept, resp = y), prior(normal(0, 5), class = b, resp = y),
            prior(normal(0, 3), class = sigma, resp = y))

forms <- list(
  # theta as OUTCOME
  se_het = list(f = bf(wle | se(wle_se, sigma = TRUE) ~ x), prior = pri_se, model = "se"),
  se_con = list(f = bf(wle | se(se_c,   sigma = TRUE) ~ x), prior = pri_se, model = "se"),
  # theta as PREDICTOR
  me_het = list(f = bf(y ~ me(wle, wle_se) + x1), prior = pri_me, model = "me"),
  me_con = list(f = bf(y ~ me(wle, se_c)   + x1), prior = pri_me, model = "me"),
  mi_het = list(f = bf(wle | mi(wle_se) ~ x1) + bf(y ~ mi(wle) + x1) + set_rescor(FALSE), prior = pri_mi, model = "mi"),
  mi_con = list(f = bf(wle | mi(se_c)   ~ x1) + bf(y ~ mi(wle) + x1) + set_rescor(FALSE), prior = pri_mi, model = "mi")
)

# MUPPET stage 2 for Rasch: item difficulties fixed at stage-1 estimates,
# theta | x1 structural model, y | theta, x1.
rasch_code <- "
data {
  int<lower=1> N; int<lower=1> K;
  array[K] int<lower=0, upper=1> u; array[K] int<lower=1, upper=N> pid; vector[K] bk;
  vector[N] y; vector[N] x1;
}
parameters {
  vector[N] theta; real g0; real g1; real<lower=0> tau;
  real a; real bt; real bx; real<lower=0> sy;
}
model {
  theta ~ normal(g0 + g1 * x1, tau);
  u ~ bernoulli_logit(theta[pid] - bk);
  y ~ normal(a + bt * theta + bx * x1, sy);
  g0 ~ normal(0, 5); g1 ~ normal(0, 5); a ~ normal(0, 5); bt ~ normal(0, 5); bx ~ normal(0, 5);
  tau ~ normal(0, 3); sy ~ normal(0, 3);
}
"

dummy <- data.frame(y = rnorm(50), x = rnorm(50), x1 = rnorm(50), wle = rnorm(50),
                    wle_se = runif(50, .3, .6), se_c = .45)
stan_files <- c(
  se    = file.path(stan_dir, "brms_se.stan"),
  me    = file.path(stan_dir, "brms_me.stan"),
  mi    = file.path(stan_dir, "brms_mi.stan"),
  rasch = file.path(stan_dir, "rasch_joint.stan"))
for (m in c("se", "me", "mi")) {
  spec <- forms[[paste0(m, "_het")]]
  code <- stancode(spec$f, data = dummy, prior = spec$prior)
  # sanity: heteroscedastic and constant-SE formulas must share one program
  stopifnot(identical(code, stancode(forms[[paste0(m, "_con")]]$f, data = dummy,
                                     prior = forms[[paste0(m, "_con")]]$prior)))
  if (!file.exists(stan_files[[m]]) || !identical(paste(readLines(stan_files[[m]]), collapse = "\n"), as.character(code)))
    writeLines(code, stan_files[[m]])
}
if (!file.exists(stan_files[["rasch"]])) writeLines(rasch_code, stan_files[["rasch"]])
for (f in stan_files) cmdstan_model(f, quiet = TRUE)   # compile now

fixed <- list(N = 1000, B_TH = 0.5, B_X1 = 0.3,
              warmup = 750, iter = 750, forms = forms, stan_files = stan_files)

# ---- Generate ----------------------------------------------------------------
Generate <- function(condition, fixed_objects) {
  N <- fixed_objects$N; I <- condition$I; B <- fixed_objects$B_TH; r <- condition$r_tx
  b_true <- seq(-2, 2, length.out = I)
  calib <- function(theta) {                      # stage 1: Rasch calibration + WLE
    resp <- (matrix(runif(N * I), N) < plogis(outer(theta, b_true, "-"))) * 1L
    mod  <- TAM::tam.mml(resp, verbose = FALSE)
    w    <- TAM::tam.wle(mod, progress = FALSE)
    list(resp = resp, b = mod$xsi$xsi, wle = w$theta, wle_se = w$error, rel = w$WLE.rel[1])
  }

  # theta as OUTCOME: theta = B x + e, var(theta) = 1
  x  <- rnorm(N)
  th <- B * x + rnorm(N, 0, sqrt(1 - B^2))
  co <- calib(th)
  out <- data.frame(x = x, theta = th, wle = co$wle, wle_se = co$wle_se,
                    se_c = sqrt(mean(co$wle_se^2)))

  # theta as PREDICTOR: y = B theta + B_X1 x1 + e, cor(theta, x1) = r_tx
  th <- rnorm(N)
  x1 <- r * th + rnorm(N, 0, sqrt(1 - r^2))
  y  <- B * th + fixed_objects$B_X1 * x1 + rnorm(N)
  cp <- calib(th)
  pred <- data.frame(y = y, x1 = x1, theta = th, wle = cp$wle, wle_se = cp$wle_se,
                     se_c = sqrt(mean(cp$wle_se^2)))

  list(out = out, out_resp = co$resp, out_rel = co$rel,
       pred = pred, pred_resp = cp$resp, pred_b = cp$b, pred_rel = cp$rel)
}

# ---- Analyse -----------------------------------------------------------------
# Result names: <method>__<param>__<stat>, stat in est/se/lo/hi;
#               <method>__diag__rhat / __ndiv for Stan fits.
Analyse <- function(condition, dat, fixed_objects) {
  fo <- fixed_objects
  res <- c()
  # SimDesign treats any NA as a failed replication, so point-only stats store est alone
  put <- function(method, param, est, se = NULL, lo = est - 1.96 * se, hi = est + 1.96 * se) {
    if (is.null(se)) res[paste(method, param, "est", sep = "__")] <<- est
    else res[paste(method, param, c("est", "se", "lo", "hi"), sep = "__")] <<- c(est, se, lo, hi)
  }
  put_lm <- function(method, fit, term, param) {
    ci <- confint(fit)[term, ]
    put(method, param, coef(fit)[[term]], summary(fit)$coef[term, 2], ci[[1]], ci[[2]])
  }
  run_stan <- function(method, file, sdata, pars) {
    mod <- cmdstan_model(file, quiet = TRUE)
    fit <- mod$sample(data = sdata, chains = 2, parallel_chains = 1,
                      iter_warmup = fo$warmup, iter_sampling = fo$iter,
                      refresh = 0, show_messages = FALSE, show_exceptions = FALSE,
                      seed = sample.int(.Machine$integer.max, 1))
    dr   <- fit$draws(variables = names(pars), format = "draws_array")
    ndiv <- sum(fit$diagnostic_summary(diagnostics = "divergences", quiet = TRUE)$num_divergent)
    unlink(fit$output_files())                     # CSVs hold N latent draws; don't let /tmp fill up
    s <- posterior::summarise_draws(dr, "mean", "sd",
                                    ~posterior::quantile2(.x, probs = c(.025, .975)), "rhat")
    for (k in seq_along(pars)) {
      row <- s[s$variable == names(pars)[k], ]
      put(method, pars[[k]], row$mean, row$sd, row$q2.5, row$q97.5)
    }
    res[paste(method, "diag", c("rhat", "ndiv"), sep = "__")] <<- c(max(s$rhat), ndiv)
  }
  brms_fit <- function(method, d, pars) {
    spec <- fo$forms[[method]]
    run_stan(method, fo$stan_files[[spec$model]], standata(spec$f, data = d, prior = spec$prior), pars)
  }

  ## ---- theta as OUTCOME ----
  d <- dat$out
  f <- lm(theta ~ x, d); put_lm("oracle", f, "x", "bx"); put("oracle", "sigma", sigma(f))
  f <- lm(wle ~ x, d);   put_lm("naive",  f, "x", "bx"); put("naive",  "sigma", sigma(f))
  brms_fit("se_het", d, c("b[1]" = "bx", "sigma" = "sigma"))
  brms_fit("se_con", d, c("b[1]" = "bx", "sigma" = "sigma"))
  m  <- TAM::tam.mml(dat$out_resp, Y = matrix(d$x, ncol = 1), verbose = FALSE)
  sb <- TAM::tam.se(m)$beta
  put("tam_lr", "bx", m$beta[2, 1], sb[2, grep("^se", colnames(sb))[1]])
  put("tam_lr", "sigma", sqrt(m$variance[1]))
  res[c("info__out__rel__val", "info__out__meanse2__val")] <- c(dat$out_rel, mean(d$wle_se^2))

  ## ---- theta as PREDICTOR ----
  d <- dat$pred
  f <- lm(y ~ theta + x1, d); put_lm("oracle", f, "theta", "bth"); put_lm("oracle", f, "x1", "bx1")
  f <- lm(y ~ wle + x1, d);   put_lm("naive",  f, "wle",   "bth"); put_lm("naive",  f, "x1", "bx1")
  brms_fit("me_het", d, c("bsp[1]" = "bth", "b[1]" = "bx1"))
  brms_fit("me_con", d, c("bsp[1]" = "bth", "b[1]" = "bx1"))
  brms_fit("mi_het", d, c("bsp_y[1]" = "bth", "b_y[1]" = "bx1"))
  brms_fit("mi_con", d, c("bsp_y[1]" = "bth", "b_y[1]" = "bx1"))
  resp <- dat$pred_resp
  sdata <- list(N = nrow(resp), K = length(resp), u = as.vector(resp),
                pid = rep(seq_len(nrow(resp)), times = ncol(resp)),
                bk = rep(dat$pred_b, each = nrow(resp)), y = d$y, x1 = d$x1)
  run_stan("rasch", fo$stan_files[["rasch"]], sdata, c("bt" = "bth", "bx" = "bx1"))
  res[c("info__pred__rel__val", "info__pred__meanse2__val")] <- c(dat$pred_rel, mean(d$wle_se^2))

  res
}

# ---- Summarise (quick view; full tables come from summarise_wle_me.R) --------
Summarise <- function(condition, results, fixed_objects) {
  truth <- c(bx = fixed_objects$B_TH, sigma = sqrt(1 - fixed_objects$B_TH^2),
             bth = fixed_objects$B_TH, bx1 = fixed_objects$B_X1)
  out <- c()
  for (k in grep("__est$", names(results), value = TRUE)) {
    base <- sub("__est$", "", k)
    tr <- truth[[strsplit(base, "__")[[1]][2]]]
    out[paste0(base, "__relbias")] <- 100 * (mean(results[[k]]) / tr - 1)
    lo <- results[[paste0(base, "__lo")]]; hi <- results[[paste0(base, "__hi")]]
    if (!is.null(lo)) out[paste0(base, "__cover")] <- mean(lo <= tr & tr <= hi)
  }
  out
}

# ---- Run ---------------------------------------------------------------------
res <- runSimulation(design = Design, replications = REPS,
                     generate = Generate, analyse = Analyse, summarise = Summarise,
                     fixed_objects = fixed, seed = 20261022L + seq_len(nrow(Design)),
                     packages = c("TAM", "brms", "cmdstanr", "posterior"),
                     parallel = NCORES > 1, ncores = NCORES,
                     store_results = TRUE,
                     filename = sprintf("wle_me_sim_R%d", REPS))
print(res)
