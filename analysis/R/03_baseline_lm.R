# R/03_baseline_lm.R
source("R/00_setup.R")
source("lib/00_model_defs.R")
reg_df   <- prep_reg_df()
fit_full <- lm(form_full, data = reg_df)
save_table(broom::tidy(fit_full, conf.int=TRUE), "baseline_coefficients")
save_table(broom::glance(fit_full), "baseline_fit_stats")
tt <- summary(fit_full)$coefficients[, "t value"]
df_res <- fit_full$df.residual
pr <- tt / sqrt(tt^2 + df_res)
save_table(data.frame(term=names(pr), partial_r=round(pr,4)),
           "partial_correlations")
cat("03_baseline_lm.R done\n")
