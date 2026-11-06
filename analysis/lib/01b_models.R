# R/01b_models.R -- supplementary models (confirmatory + optional AIC)
# Run after 01_pipeline_w1_w5.R has produced reg_df.rds / final_model.rds
source("R/00_setup.R")
source("lib/00_model_defs.R")

d <- prep_reg_df()
cat("Sample n:", nrow(d), "\n")

# ---- Confirmatory model: forced-in full formula (for H1-H4 tests) ----
fit_conf <- lm(form_full, data = d)
cat("Confirm model: n =", nobs(fit_conf),
    " R2 =", round(summary(fit_conf)$r.squared, 3), "\n")
stopifnot("AllPriorWinPct" %in% attr(terms(form_full), "term.labels"))
saveRDS(list(formula = form_full, fit = fit_conf),
        "data/processed/confirm_model.rds")
cat("Saved data/processed/confirm_model.rds\n")

# ---- Optional: AIC-selected model (less parsimonious than BIC) ----
n <- nrow(d)
fit0 <- lm(sp_rel_team ~ 1, data = d)
fit_aic <- step(fit0,
                scope = list(lower = ~1, upper = form_full),
                direction = "both", k = 2, trace = 0)
cat("AIC model formula:\n"); print(formula(fit_aic))
saveRDS(list(formula = formula(fit_aic), fit = fit_aic),
        "data/processed/aic_model.rds")
cat("Saved data/processed/aic_model.rds\n")
