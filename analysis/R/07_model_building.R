# R/07_model_selection.R -- Model building and selection (plan 07+08 combined)
# Part A of the reference W5 script: best-subsets, stepwise, LASSO,
# bootstrap selection stability, candidate comparison.
source("R/00_setup.R")
source("lib/00_model_defs.R")
reg_df    <- prep_reg_df()
n         <- nrow(reg_df)
fit_full  <- lm(form_full, data = reg_df)
terms_all <- attr(terms(form_full), "term.labels")

# A1. Best-subsets (exhaustive) with adj-R2, Cp, BIC
rs <- regsubsets(form_full, data = reg_df, nvmax = 20, method = "exhaustive",
                 really.big = FALSE)
rss <- summary(rs)
sel_tab <- data.frame(size = seq_along(rss$cp), adjR2 = rss$adjr2,
                      Cp = rss$cp, BIC = rss$bic)
print(sel_tab)
save_table(sel_tab, "w5_best_subsets_criteria")
best <- c(adjR2 = which.max(rss$adjr2), Cp = which.min(rss$cp), BIC = which.min(rss$bic))
print(best)
for (nm in names(best)) {
  cat("\nBest subset by", nm, "(size", best[nm], "):\n")
  print(round(coef(rs, best[nm]), 4))
}
png(paste0(path_figures, "w5_best_subsets.png"), width = 1200, height = 450)
par(mfrow = c(1, 3))
plot(sel_tab$size, sel_tab$adjR2, type = "b", xlab = "Size", ylab = "Adj R2"); abline(v = best["adjR2"], lty = 2)
plot(sel_tab$size, sel_tab$Cp, type = "b", xlab = "Size", ylab = "Mallows Cp"); abline(0, 1, col = "grey")
plot(sel_tab$size, sel_tab$BIC, type = "b", xlab = "Size", ylab = "BIC"); abline(v = best["BIC"], lty = 2)
dev.off()

# A2. Stepwise (AIC and BIC)
fit0 <- lm(sp_rel_team ~ 1, data = reg_df)
fit_aic <- step(fit0, scope = list(lower = ~1, upper = form_full),
                direction = "both", trace = 0)
fit_bic <- step(fit0, scope = list(lower = ~1, upper = form_full),
                direction = "both", k = log(n), trace = 0)
fit_back <- step(fit_full, direction = "backward", trace = 0)
cat("\nAIC stepwise  :", deparse(formula(fit_aic)), "\n")
cat("BIC stepwise  :", deparse(formula(fit_bic)), "\n")
cat("Backward (AIC):", deparse(formula(fit_back)), "\n")

# A3. Compare candidates: in-sample criteria + PRESS / predicted R2 + 10-fold CV
set.seed(2026)
folds <- sample(rep(1:10, length.out = n))
cv_rmse_lm <- function(form) {
  e <- numeric(n)
  for (f in 1:10) {
    tr <- folds != f
    m <- lm(form, data = reg_df[tr, ])
    e[!tr] <- reg_df$sp_rel_team[!tr] - suppressWarnings(predict(m, reg_df[!tr, ]))
  }
  rmse(e)
}
sst <- sum((reg_df$sp_rel_team - mean(reg_df$sp_rel_team))^2)
cands <- list(full = fit_full, AIC_step = fit_aic, BIC_step = fit_bic, backward = fit_back)
cand_tab <- do.call(rbind, lapply(names(cands), function(nm) {
  m <- cands[[nm]]
  data.frame(model = nm, p = length(coef(m)),
             adjR2 = summary(m)$adj.r.squared, AIC = AIC(m), BIC = BIC(m),
             PRESS = press_stat(m), predR2 = 1 - press_stat(m) / sst,
             CV10_RMSE = cv_rmse_lm(formula(m)))
}))
print(cand_tab)
save_table(cand_tab, "w5_candidate_comparison")

# A4. Penalized selection: LASSO and elastic net
Xg <- model.matrix(form_full, reg_df)[, -1]
yg <- reg_df$sp_rel_team
set.seed(2026)
cv_lasso <- cv.glmnet(Xg, yg, alpha = 1, nfolds = 10)
cv_enet  <- cv.glmnet(Xg, yg, alpha = 0.5, nfolds = 10)
pen_tab <- data.frame(term = rownames(coef(cv_lasso)),
                      lasso_min = as.numeric(coef(cv_lasso, s = "lambda.min")),
                      lasso_1se = as.numeric(coef(cv_lasso, s = "lambda.1se")),
                      enet_min  = as.numeric(coef(cv_enet,  s = "lambda.min")))
print(pen_tab)
save_table(pen_tab, "w5_lasso_enet_coefs")
png(paste0(path_figures, "w5_lasso_cv.png"), width = 1000, height = 450)
par(mfrow = c(1, 2)); plot(cv_lasso, main = "LASSO CV"); plot(cv_enet, main = "Elastic net CV (alpha=.5)")
dev.off()

# A5. Bootstrap: selection stability + optimism of the BIC-selected model
set.seed(2026)
B <- 300
sel_mat <- matrix(0, B, length(terms_all), dimnames = list(NULL, terms_all))
rm_boot <- rm_orig <- numeric(B)
for (b in 1:B) {
  d <- reg_df[sample(n, replace = TRUE), ]
  fb <- step(lm(sp_rel_team ~ 1, data = d), scope = list(lower = ~1, upper = form_full),
             direction = "both", k = log(n), trace = 0)
  sel_mat[b, attr(terms(fb), "term.labels")] <- 1
  rm_boot[b] <- rmse(residuals(fb))
  rm_orig[b] <- rmse(reg_df$sp_rel_team - suppressWarnings(predict(fb, reg_df)))
}
sel_freq <- sort(colMeans(sel_mat), decreasing = TRUE)
print(round(sel_freq, 3))
save_table(data.frame(term = names(sel_freq), inclusion_freq = as.numeric(sel_freq)),
           "w5_bootstrap_selection_freq")
optimism <- mean(rm_orig - rm_boot)
cat("Apparent RMSE (BIC model):", round(rmse(residuals(fit_bic)), 4),
    "| optimism-corrected RMSE:", round(rmse(residuals(fit_bic)) + optimism, 4), "\n")

