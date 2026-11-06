# R/10_simultaneous_inference.R -- Bonferroni / Holm / Scheffe simultaneous bands
# Part C of the reference W5 script. Reloads the BIC-selected model.
source("R/00_setup.R")
source("lib/00_model_defs.R")
reg_df <- prep_reg_df()
n <- nrow(reg_df)
fm_bic <- readRDS("data/processed/final_model.rds")
fit_bic <- fm_bic$fit
m <- length(coef(fit_bic)); dfr <- df.residual(fit_bic); alpha <- 0.05

est <- coef(fit_bic); se <- sqrt(diag(vcov(fit_bic)))
tmult_pw   <- qt(1 - alpha / 2, dfr)
tmult_bonf <- qt(1 - alpha / (2 * m), dfr)
fmult_sch  <- sqrt(m * qf(1 - alpha, m, dfr))      # Scheffe over all linear combinations
simul <- data.frame(
  term = names(est), estimate = est, se = se,
  pw_lo = est - tmult_pw * se,     pw_hi = est + tmult_pw * se,
  bonf_lo = est - tmult_bonf * se, bonf_hi = est + tmult_bonf * se,
  sch_lo = est - fmult_sch * se,   sch_hi = est + fmult_sch * se,
  p_raw = summary(fit_bic)$coefficients[, 4])
simul$p_bonf <- p.adjust(simul$p_raw, "bonferroni")
simul$p_holm <- p.adjust(simul$p_raw, "holm")
simul$p_BH   <- p.adjust(simul$p_raw, "BH")
print(simul, digits = 3)
save_table(simul, "w5_simultaneous_coef_intervals")
cat("Multipliers -> pointwise t:", round(tmult_pw, 3), "| Bonferroni t:", round(tmult_bonf, 3),
    "| Scheffe:", round(fmult_sch, 3), "\n")

# Simultaneous bands for the mean response at all n observed X's
pr <- predict(fit_bic, se.fit = TRUE)
W <- sqrt(m * qf(1 - alpha, m, dfr))                 # Working-Hotelling
B_mult <- qt(1 - alpha / (2 * n), dfr)               # Bonferroni across n points
band_tab <- data.frame(method = c("Pointwise t", "Bonferroni (n points)", "Working-Hotelling"),
                       multiplier = c(tmult_pw, B_mult, W),
                       mean_halfwidth = c(tmult_pw, B_mult, W) * mean(pr$se.fit))
print(band_tab)
save_table(band_tab, "w5_mean_response_bands")

# Pairwise comparisons of mean CPI across hiring conferences (Tukey / Holm)
if ("Conf.Hired" %in% names(reg_df)) {
  big <- names(which(table(reg_df$Conf.Hired) >= 5))
  sub <- droplevels(reg_df[reg_df$Conf.Hired %in% big, ])
  if (nlevels(sub$Conf.Hired) >= 2) {
    print(TukeyHSD(aov(sp_rel_team ~ Conf.Hired, data = sub)))
    print(pairwise.t.test(sub$sp_rel_team, sub$Conf.Hired, p.adjust.method = "holm"))
  }
}

