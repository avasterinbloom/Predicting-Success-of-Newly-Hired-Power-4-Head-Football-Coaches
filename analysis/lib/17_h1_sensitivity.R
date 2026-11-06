# lib/17_h1_sensitivity.R -- H1 sensitivity table with full inference
suppressPackageStartupMessages({library(dplyr)})
source("R/00_setup.R"); source("lib/00_model_defs.R")

reg <- prep_reg_df()
fm  <- readRDS("data/processed/confirm_model.rds")
fit <- fm$fit
V   <- vcov(fit); b <- coef(fit); df <- df.residual(fit)

# Design-matrix condition number
X <- model.matrix(fit)
Xs <- scale(X[, colnames(X) != "(Intercept)", drop = FALSE])
kappa_val <- kappa(Xs, exact = TRUE)
cat("Condition number of scaled predictors:", round(kappa_val, 1), "\n")

rows <- list()

# (a) Single AllPriorWinPct coefficient in the full model
est <- b["AllPriorWinPct"]; se <- sqrt(V["AllPriorWinPct","AllPriorWinPct"])
t_  <- est/se; p_ <- 2*pt(-abs(t_), df)
rows[[1]] <- data.frame(term = "AllPriorWinPct (in full model)",
                        estimate = est, se = se,
                        CI_lo = est - qt(0.975, df)*se,
                        CI_hi = est + qt(0.975, df)*se,
                        p_raw = p_)

# (b) Single-variable refit (drop PreviousWinPct)
fit_sv <- lm(update(formula(fit), . ~ . - PreviousWinPct),
             data = model.frame(fit))
cf_sv  <- summary(fit_sv)$coefficients
if ("AllPriorWinPct" %in% rownames(cf_sv)) {
  est <- cf_sv["AllPriorWinPct","Estimate"]
  se  <- cf_sv["AllPriorWinPct","Std. Error"]
  rows[[2]] <- data.frame(term = "AllPriorWinPct (PreviousWinPct dropped)",
                          estimate = est, se = se,
                          CI_lo = est - qt(0.975, df.residual(fit_sv))*se,
                          CI_hi = est + qt(0.975, df.residual(fit_sv))*se,
                          p_raw = cf_sv["AllPriorWinPct","Pr(>|t|)"])
}

# (c) Net effect (AllPriorWinPct + PreviousWinPct) with proper covariance
est <- b["AllPriorWinPct"] + b["PreviousWinPct"]
se  <- sqrt(V["AllPriorWinPct","AllPriorWinPct"] +
            V["PreviousWinPct","PreviousWinPct"] +
            2*V["AllPriorWinPct","PreviousWinPct"])
t_  <- est/se; p_ <- 2*pt(-abs(t_), df)
rows[[3]] <- data.frame(term = "Net effect (APWP + PWP)",
                        estimate = est, se = se,
                        CI_lo = est - qt(0.975, df)*se,
                        CI_hi = est + qt(0.975, df)*se,
                        p_raw = p_)

tab <- do.call(rbind, rows)
tab$p_Holm <- p.adjust(tab$p_raw, "holm")
tab$condition_number <- round(kappa_val, 1)
save_table(tab, "h1_sensitivity")
print(tab, digits = 3, row.names = FALSE)
