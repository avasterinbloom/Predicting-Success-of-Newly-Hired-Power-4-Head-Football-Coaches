# R/05_remedies.R -- Week 3-4 remedies (Box-Cox + WLS)
# Extracted from reference/teammate_W1_W3_reference.R
source("R/00_setup.R")
reg_df <- readRDS(path_reg_df)

# Recreate fit_full with the same formula used in Week 2/3
fit_full <- lm(sp_rel_team ~
                 PrevHCAnyLevel + AllPriorWinPct + PreviousWinPct +
                 PreviousOC + PreviousDC + SideOfBall +
                 ConnectionToHiring.School + PlayedNFL +
                 AgeAtHiringMinus50 + NFLCoachingExperience +
                 PrevHCAnyLevel:sp_prev_rel + WonCCasHCany + Conf.LeavingP4 +
                 team_sp_recent + PromoteWithin,
               data = reg_df)

# --- 1. Box-Cox on the shifted positive response ---
shift_val <- -min(reg_df$sp_rel_team, na.rm = TRUE) + 0.1
reg_df$sp_rel_shifted <- reg_df$sp_rel_team + shift_val

fit_shifted <- lm(update(formula(fit_full), sp_rel_shifted ~ .),
                  data = reg_df)

png(paste0(path_figures, "fig4b_boxcox.png"), width = 800, height = 600)
bc <- boxcox(fit_shifted, lambda = seq(-2, 2, by = 0.1))
dev.off()

lambda_opt <- bc$x[which.max(bc$y)]
cat("Optimal Box-Cox lambda:", round(lambda_opt, 3), "\n")
cat("Shift constant used:", round(shift_val, 3), "\n")

# --- 2. WLS: weights from variance-driving predictors ---
# Using predictors identified from diagnostics (e.g., team_sp_recent, AllPriorWinPct)
aux_var <- lm(residuals(fit_full)^2 ~ team_sp_recent + AllPriorWinPct, data = reg_df)
fitted_var <- pmax(fitted(aux_var), 1e-6)
reg_df$wls_w <- 1 / fitted_var

fit_wls <- lm(formula(fit_full), data = reg_df, weights = wls_w)
summary(fit_wls)

save_table(broom::tidy(fit_wls, conf.int = TRUE), "wls_coefficients")
saveRDS(reg_df, path_reg_df)


cbind(OLS = coef(fit_full), WLS = coef(fit_wls))

