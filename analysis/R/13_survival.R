# R/13_survival.R -- Kaplan-Meier + Cox PH on full_df
source("R/00_setup.R")
full_df <- readRDS(path_full_df)

# ---- Covariate prep (same imputation rules as the pipeline) ----
full_df$AllPriorWinPct[is.na(full_df$AllPriorWinPct) & full_df$PrevHCAnyLevel == 0] <- 0.50
still_na <- is.na(full_df$AllPriorWinPct) & full_df$PrevHCAnyLevel == 1
if (any(still_na)) {
  full_df$AllPriorWinPct[still_na] <- full_df$PreviousWinPct[still_na]
  cat("Imputed AllPriorWinPct for", sum(still_na), "anomalous prior-HC rows\n")
}

full_df$TenureLength <- full_df$last_season - full_df$first_season + 1
full_df$EventFired   <- full_df$Fired * (1 - full_df$OffFieldIssuesFiring)

surv_df <- full_df %>%
  dplyr::filter(!is.na(TenureLength),
                !is.na(EventFired),
                !is.na(PrevHCAnyLevel),
                !is.na(AllPriorWinPct),
                !is.na(AgeAtHiringMinus50))
cat("Survival n:", nrow(surv_df), "| events:", sum(surv_df$EventFired), "\n")
cat("Censored  :", sum(1 - surv_df$EventFired), "\n")

# ---- Kaplan-Meier by prior HC experience ----
km_fit <- survfit(Surv(TenureLength, EventFired) ~ PrevHCAnyLevel, data = surv_df)
png(paste0(path_figures, "fig2_km_curves.png"), width = 900, height = 600)
plot(km_fit, lty = 1:2, col = c("darkorange","steelblue"),
     xlab = "Seasons", ylab = "Survival Probability",
     main = "Kaplan-Meier: Tenure by Prior HC Experience")
legend("topright", legend = c("No prior HC","Prior HC"),
       lty = 1:2, col = c("darkorange","steelblue"))
dev.off()
cat("Saved figures/fig2_km_curves.png\n")

# ---- Log-rank ----
lr <- survdiff(Surv(TenureLength, EventFired) ~ PrevHCAnyLevel, data = surv_df)
print(lr)
save_table(data.frame(test = "log-rank (PrevHCAnyLevel)",
                      chisq = lr$chisq,
                      df = length(lr$n) - 1,
                      p_value = 1 - pchisq(lr$chisq, df = length(lr$n) - 1)),
           "logrank_priorhc")

# ---- Cox PH ----
cox_fit <- coxph(Surv(TenureLength, EventFired) ~
                   AllPriorWinPct + PrevHCAnyLevel + AgeAtHiringMinus50,
                 data = surv_df)
print(summary(cox_fit))
save_table(broom::tidy(cox_fit, conf.int = TRUE, exponentiate = TRUE),
           "cox_coefficients")

# ---- PH assumption ----
ph_test <- cox.zph(cox_fit)
print(ph_test)
save_table(as.data.frame(ph_test$table), "cox_ph_assumption")
