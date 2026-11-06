# lib/19_lrt_fix.R -- LRT for random intercept (ML + robust optimizer + boundary floor)
suppressPackageStartupMessages({library(nlme)})
source("R/00_setup.R"); source("lib/00_model_defs.R")

reg <- prep_reg_df()
fm  <- readRDS("data/processed/final_model.rds")

# GLs under ML (no random effect)
fit_gls <- gls(fm$formula, data = reg, method = "ML")

# lme with nlminb optimizer (more robust than optim for boundary solutions)
fit_lme <- tryCatch(
  lme(fm$formula, random = ~1 | school, data = reg, method = "ML",
      control = lmeControl(opt = "nlminb", maxIter = 500, msMaxIter = 500,
                           returnObject = TRUE)),
  error = function(e) { cat("lme failed:", conditionMessage(e), "\n"); NULL })

if (is.null(fit_lme)) {
  save_table(data.frame(
    test      = "LRT random intercept: school",
    statistic = NA_real_, df = 1, p_value = NA_real_,
    method = "ML", note = "lme failed to converge"), "lrt_school_random")
  quit(status = 0)
}

ll_gls <- as.numeric(logLik(fit_gls))
ll_lme <- as.numeric(logLik(fit_lme))

# LRT = 2*(ll_lme - ll_gls), floored at 0 (boundary solution).
# When Var(school) -> 0, the optimizer can return a saddle slightly below GLS;
# the correct LRT value is 0 (nested model with extra parameter at boundary).
lr_raw  <- 2 * (ll_lme - ll_gls)
lr_stat <- max(lr_raw, 0)
p_bc    <- 0.5 * pchisq(lr_stat, df = 1, lower.tail = FALSE)

vc   <- as.numeric(VarCorr(fit_lme)[, "Variance"])
icc  <- vc[1] / sum(vc)

cat(sprintf("LRT raw = %.4f | floored = %.4f | p = %.4f\n", lr_raw, lr_stat, p_bc))
cat(sprintf("Var(school) = %.4f | Var(resid) = %.4f | ICC = %.4f\n",
            vc[1], vc[2], icc))
cat(if (lr_raw < 0)
    "NOTE: raw LRT < 0 (boundary solution); floored at 0.\n"
    else "NOTE: LRT well-defined.\n")

save_table(data.frame(
  test       = "LRT random intercept: school",
  statistic  = round(lr_stat, 4),
  df         = 1,
  p_value    = round(p_bc, 4),
  method     = "ML",
  note       = if (lr_raw < 0)
                 "boundary solution: Var(school) near 0, floored at 0"
               else "converged normally",
  lr_raw     = round(lr_raw, 4),
  icc        = round(icc, 4)),
  "lrt_school_random")

cat("Saved tables/lrt_school_random.csv\n")
