# Week 4 — Model selection memo
- VIF: all < 5 in the reduced spec.
- BIC, stepwise, LASSO 1-se all agree: PreviousOC + team_sp_recent.
- **Bootstrap stability:** PreviousOC is selected in **77%** of bootstrap samples; team_sp_recent is selected in only **48%**. PreviousOC is stable and belongs in the model; team_sp_recent is **unstable**, and its BIC inclusion should be read as tentative. (Source: `tables/w5_bootstrap_selection_freq.csv`.)
- Selection table: subset_selection_profile.csv; coefficients: lasso_coefficients.csv.
