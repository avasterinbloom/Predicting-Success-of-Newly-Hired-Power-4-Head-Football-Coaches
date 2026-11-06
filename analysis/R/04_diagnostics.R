# R/04_diagnostics.R -- Week 3 diagnostics
# Extracted from reference/teammate_W1_W3_reference.R
source("R/00_setup.R")
reg_df <- readRDS(path_reg_df)

fit_full <- lm(sp_rel_team ~ PrevHCAnyLevel + AllPriorWinPct + PreviousWinPct +
                 PreviousOC + PreviousDC + SideOfBall +
                 ConnectionToHiring.School + PlayedNFL +
                 AgeAtHiringMinus50 + NFLCoachingExperience +
                 sp_prev_rel + WonCCasHCany + Conf.LeavingP4 +
                 team_sp_recent + PromoteWithin,
               data = reg_df)
# --- Figure 3: 2x2 residual diagnostics panel ---
png(paste0(path_figures, "fig3_residual_panel.png"),
    width = 1100, height = 900)
par(mfrow = c(2, 2))
plot(fit_full, which = 1) # Residuals vs Fitted
plot(fit_full, which = 2) # Normal Q-Q
plot(fit_full, which = 3) # Scale-Location
plot(fit_full, which = 4) # Cook’s distance
dev.off()
# --- Numeric influence measures ---
cd <- cooks.distance(fit_full)
hv <- hatvalues(fit_full)
dfb <- dfbetas(fit_full)
# Thresholds: Cook’s D > 4/n, leverage > 2p/n, |DFBETA| > 2/sqrt(n)
n <- nobs(fit_full); p <- length(coef(fit_full))
infl_table <- data.frame(
  obs = 1:n,
  cooksd = cd,
  leverage = hv,
  dfbeta_max = apply(abs(dfb), 1, max)
)
infl_table$flag_cook <- infl_table$cooksd > 4/n
infl_table$flag_lev <- infl_table$leverage > 2*p/n
infl_table$flag_dfb <- infl_table$dfbeta_max > 2/sqrt(n)
save_table(infl_table, "influence_diagnostics")
cat("Influential (Cook’s D):", sum(infl_table$flag_cook), "\n")
cat("High leverage:", sum(infl_table$flag_lev), "\n")
cat("Large DFBETA:", sum(infl_table$flag_dfb), "\n")
















## --- Breusch-Pagan ---
bp <- bptest(fit_full)
print(bp)
save_table(data.frame(test = "Breusch-Pagan",
                      statistic = bp$statistic,
                      df = bp$parameter,
                      p_value = bp$p.value),
           "heteroscedasticity_tests")

# --- White’s test (Squares Only) ---
X_mat <- model.matrix(fit_full)[, -1]
e2 <- residuals(fit_full)^2
white_data <- as.data.frame(X_mat)
white_data$.e2 <- e2
col_names <- colnames(X_mat)

# Squares only (no cross-products) to prevent saturating the degrees of freedom
for (j in seq_along(col_names)) {
  white_data[[paste0(col_names[j], "_sq")]] <- X_mat[, j]^2
}

white_aux <- lm(.e2 ~ ., data = white_data)
n_obs <- nrow(white_data)
white_R2 <- summary(white_aux)$r.squared
white_stat <- n_obs * white_R2
white_df <- length(coef(white_aux)) - 1
white_pval <- pchisq(white_stat, df = white_df, lower.tail = FALSE)

cat("White test (Squares Only): stat =", round(white_stat, 3),
    " df =", white_df,
    " p =", round(white_pval, 4), "\n")

# Append White result
white_row <- data.frame(test = "White (squares only)",
                        statistic = white_stat,
                        df = white_df,
                        p_value = white_pval)
write.csv(rbind(read.csv("tables/heteroscedasticity_tests.csv"), white_row),
          "tables/heteroscedasticity_tests.csv", row.names = FALSE)

# --- Figure 4: squared residuals vs fitted ---
png(paste0(path_figures, "fig4_white_aux.png"), width = 800, height = 600)
plot(fitted(fit_full), e2, xlab = "Fitted values", ylab = "Squared residuals",
     main = "White’s test auxiliary regression")
lines(lowess(fitted(fit_full), e2), col = "blue", lwd = 2)
dev.off()





# --- ACF and PACF ---
png(paste0(path_figures, "fig3b_acf_pacf.png"), width = 1000, height = 500)
par(mfrow = c(1, 2))
acf(residuals(fit_full), main = "ACF of residuals")
pacf(residuals(fit_full), main = "PACF of residuals")
dev.off()
# --- Durbin-Watson ---
dw <- dwtest(fit_full)
print(dw)
save_table(data.frame(test = "Durbin-Watson",
                      statistic = dw$statistic,
                      p_value = dw$p.value),
           "autocorrelation_test")
# --- Runs test (optional extra check) ---
if (requireNamespace("randtests", quietly = TRUE)) {
  library(randtests)
  print(runs.test(residuals(fit_full)))
}







##### DETAILS OF THE 8 OUTLIERS

library(dplyr)

# Combine the coach details with your influence diagnostics
outlier_review <- cbind(
  name = reg_df$name,
  school = reg_df$school,
  first_season = reg_df$first_season,
  sp_rel_team = reg_df$sp_rel_team, 
  infl_table
)

# Filter and display the 8 highly influential coaches
flagged_coaches <- outlier_review %>%
  filter(flag_cook == TRUE) %>%
  dplyr::select(name, school, first_season, sp_rel_team, cooksd, flag_lev) %>%
  arrange(desc(cooksd))

print(flagged_coaches)















