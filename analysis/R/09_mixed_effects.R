# R/09_mixed_effects.R -- Random-intercept mixed models (school, Conf.Hired)
# Part B of the reference W5 script. Reloads the BIC-selected model from disk.
source("R/00_setup.R")
source("lib/00_model_defs.R")
reg_df <- prep_reg_df()
fm_bic <- readRDS("data/processed/final_model.rds")
fit_bic <- fm_bic$fit

fixed_form <- formula(fit_bic)        # BIC-selected fixed effects (parsimonious)
cat("\nHires per school (top):\n"); print(head(sort(table(reg_df$school), decreasing = TRUE), 8))

fit_gls <- gls(fixed_form, data = reg_df, method = "ML")
mixed_res <- list()
for (g in intersect(c("school", "Conf.Hired"), names(reg_df))) {
  rf <- as.formula(paste("~ 1 |", g))
  fit_lme <- tryCatch(
    lme(fixed_form, random = rf, data = reg_df, method = "REML",
        control = lmeControl(opt = "optim", maxIter = 200, msMaxIter = 200)),
    error = function(e) { message("lme failed for ", g, ": ", e$message); NULL })
  if (is.null(fit_lme)) next
  vc <- as.numeric(VarCorr(fit_lme)[, "Variance"])
  icc <- vc[1] / sum(vc)
  lrt <- anova(fit_gls, fit_lme)
p_bc <- 0.5 * pchisq(as.numeric(lrt$L.Ratio[2]),
                     df = as.numeric(lrt$Df[2]), lower.tail = FALSE)
  cat("\nRandom intercept for", g, ": ICC =", round(icc, 3),
      "| LRT p (boundary -> conservative) =", signif(lrt$`p-value`[2], 3), "\n")
  print(summary(fit_lme)$tTable)
  lrt_row <- data.frame(test = paste0("LRT random intercept: ", g),
                         statistic = as.numeric(lrt$L.Ratio[2]),
                         df        = as.numeric(lrt$Df[2]),
                         p_value   = as.numeric(lrt$`p-value`[2]))
  save_table(lrt_row, "lrt_school_random")
  mixed_res[[g]] <- data.frame(grouping = g, n_groups = nlevels(factor(reg_df[[g]])),
                               sigma2_group = vc[1], sigma2_resid = vc[2], ICC = icc,
                               AIC_lme = AIC(fit_lme), AIC_gls = AIC(fit_gls),
                               LRT_p = lrt$`p-value`[2])
  # BLUPs of group effects
  re <- ranef(fit_lme)
  save_table(data.frame(group = rownames(re), blup = re[, 1]), paste0("w5_blups_", g))
}
if (length(mixed_res)) save_table(do.call(rbind, mixed_res), "w5_mixed_summary")

