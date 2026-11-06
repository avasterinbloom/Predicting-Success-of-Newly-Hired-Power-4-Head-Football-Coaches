required <- c(
  paste0("data/processed/", c("reg_df.rds","full_df.rds","final_model.rds",
                              "confirm_model.rds","aic_model.rds")),
  paste0("tables/", c(
    "descriptive_stats.csv","missingness_counts.csv","exclusion_list.csv",
    "allpriorwinpct_imputation_log.csv","baseline_coefficients.csv",
    "baseline_fit_stats.csv","partial_correlations.csv",
    "influence_diagnostics.csv","heteroscedasticity_tests.csv",
    "autocorrelation_test.csv","wls_coefficients.csv","vif_table.csv",
    "subset_selection_profile.csv","lasso_coefficients.csv",
    "model_comparison.csv","model_n_counts.csv","cv_results.csv",
    "lrt_school_random.csv","school_blups.csv",
    "hypothesis_tests_H1_H4.csv","logistic_odds_ratios.csv",
    "hosmer_lemeshow.csv","classification_table.csv",
    "holdout_performance.csv","holdout_sensitivity.csv",
    "cox_coefficients.csv","cox_ph_assumption.csv",
    "logrank_priorhc.csv","coach_hire_score_examples.csv",
    "candidate_profiles.csv")),
  paste0("figures/", c(
    "fig1_cpi_by_hire_type.png","fig2_km_curves.png","fig3_residual_panel.png",
    "fig3b_acf_pacf.png","fig4_white_aux.png","fig4b_boxcox.png",
    "fig5_model_selection_profile.png","fig6_bootstrap_coef.png",
    "fig7_pi_coverage.png")),
  "reports/final_report.Rmd","reports/final_report.html",
  "reports/determinism_report.md",
  "reports/memos/w1_data_memo.md","reports/memos/w3_diagnostics_memo.md",
  "reports/memos/w4_model_selection_memo.md",
  "reports/memos/w5_inference_memo.md","reports/memos/w6_validation_memo.md",
  "README.md",".gitignore","data/raw/data.csv.sha256",
  "slides/final_presentation.pptx")
audit <- data.frame(file=required, exists=file.exists(required),
                    size_kb=round(file.info(required)$size/1024,1))
print(audit)
write.csv(audit,"reports/audit_log.csv",row.names=FALSE)
cat(sprintf("\n%d / %d present\n", sum(audit$exists), nrow(audit)))
if (all(audit$exists) && all(audit$size_kb>0, na.rm=TRUE)) {
  cat("AUDIT PASS\n")
} else {
  cat("AUDIT FAIL — see missing/zero-sized above\n")
}
