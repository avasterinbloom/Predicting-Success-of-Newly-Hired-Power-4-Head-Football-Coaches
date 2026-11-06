# R/05_week4_multicollinearity.R
# Week 4 -- Multicollinearity and Model Building
source("lib/00_model_defs.R")
reg_df <- prep_reg_df()
n <- nrow(reg_df)

fit_full <- lm(form_full, data = reg_df)
cat("Aliased coefficients:", any(is.na(coef(fit_full))), "\n")

# =====================================================================
# 1. Correlation structure of the numeric predictors
# =====================================================================
num_pred <- intersect(c("AllPriorWinPct", "PreviousWinPct", "AgeAtHiringMinus50",
                        "NFLCoachingExperience", "sp_prev_rel",
                        "team_sp_recent", "numb_years_Coord_HC"),
                      names(reg_df)[sapply(reg_df, is.numeric)])
cor_mat <- cor(reg_df[, num_pred], use = "pairwise.complete.obs")
print(round(cor_mat, 3))
save_table(data.frame(variable = rownames(cor_mat), round(cor_mat, 4)),
           "w4_correlation_matrix")

cor_long <- as.data.frame(as.table(cor_mat))
p_cor <- ggplot(cor_long, aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = round(Freq, 2)), size = 3) +
  scale_fill_gradient2(limits = c(-1, 1), low = "firebrick",
                       mid = "white", high = "steelblue") +
  labs(x = NULL, y = NULL, fill = "r", title = "Predictor correlation matrix") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
save_fig(p_cor, "w4_corr_heatmap")

# =====================================================================
# 2. Variance inflation factors
# =====================================================================
X <- model.matrix(fit_full)                 # includes intercept
Xn <- X[, colnames(X) != "(Intercept)", drop = FALSE]
Xn <- Xn[, apply(Xn, 2, sd) > 0, drop = FALSE]
R <- cor(Xn)
vif_manual <- data.frame(term = colnames(Xn),
                         VIF = diag(solve(R)))
vif_manual$flag <- ifelse(vif_manual$VIF > 10, "severe (>10)",
                   ifelse(vif_manual$VIF > 5, "moderate (>5)", ""))
vif_manual <- vif_manual[order(-vif_manual$VIF), ]
print(vif_manual)
save_table(vif_manual, "w4_vif_columns")

# Term-level (generalized) VIF from car, when defined
vif_car <- tryCatch(car::vif(fit_full), error = function(e) NULL)
if (!is.null(vif_car)) {
  print(vif_car)
  save_table(data.frame(term = rownames(as.matrix(vif_car)),
                        as.matrix(vif_car)), "w4_vif_car")
}

# =====================================================================
# 4. Remedies
# =====================================================================
# 4a. Iterative term-level VIF pruning (drops highest-VIF main effect until < 5)
prune_vif <- function(form, data, cutoff = 5) {
  dropped <- character(0)
  repeat {
    fit <- lm(form, data = data)
    v <- tryCatch(suppressWarnings(car::vif(fit)), error = function(e) NULL)
    if (is.null(v)) break
    if (is.matrix(v)) v <- v[, "GVIF^(1/(2*Df))"]^2
    cand <- v[!grepl(":", names(v)) & names(v) != "PrevHCAnyLevel"]
    if (length(cand) == 0 || max(cand) < cutoff) break
    worst <- names(which.max(cand))
    dropped <- c(dropped, worst)
    form <- update(form, as.formula(paste(". ~ . -", worst)))
  }
  list(formula = form, dropped = dropped)
}
pr <- prune_vif(form_full, reg_df, cutoff = 5)
cat("\nTerms dropped by VIF pruning:", ifelse(length(pr$dropped) == 0, "none",
    paste(pr$dropped, collapse = ", ")), "\n")
form_reduced <- pr$formula
fit_reduced <- lm(form_reduced, data = reg_df)

# 4b. Centering/standardizing continuous predictors (helps interaction/polynomial collinearity)
reg_c <- reg_df
for (v in c("AllPriorWinPct", "PreviousWinPct", "AgeAtHiringMinus50",
            "sp_prev_rel", "team_sp_recent")) {
  reg_c[[paste0(v, "_c")]] <- as.numeric(scale(reg_c[[v]], scale = FALSE))
}
fit_centered <- lm(update(form_full, . ~ . - PrevHCAnyLevel:sp_prev_rel +
                            PrevHCAnyLevel:sp_prev_rel_c),
                   data = reg_c)

# 4c. Compare full vs reduced
cmp <- data.frame(
  model = c("full", "VIF-pruned"),
  p = c(length(coef(fit_full)), length(coef(fit_reduced))),
  R2 = c(summary(fit_full)$r.squared, summary(fit_reduced)$r.squared),
  adjR2 = c(summary(fit_full)$adj.r.squared, summary(fit_reduced)$adj.r.squared),
  AIC = c(AIC(fit_full), AIC(fit_reduced)),
  BIC = c(BIC(fit_full), BIC(fit_reduced)),
  PRESS = c(press_stat(fit_full), press_stat(fit_reduced))
)
print(cmp)
save_table(cmp, "w4_full_vs_reduced")
print(anova(fit_reduced, fit_full))      # F-test: are dropped terms jointly needed?

# =====================================================================
# 5. Ridge regression
# =====================================================================
Xr <- model.matrix(form_full, reg_df)[, -1]
yr <- reg_df$sp_rel_team
set.seed(2026)
cv_ridge <- cv.glmnet(Xr, yr, alpha = 0, nfolds = 10, standardize = TRUE)
png(paste0(path_figures, "w4_ridge_trace.png"), width = 1000, height = 500)
par(mfrow = c(1, 2))
plot(glmnet(Xr, yr, alpha = 0, standardize = TRUE), xvar = "lambda", label = FALSE,
     main = "Ridge trace")
plot(cv_ridge, main = "Ridge CV")
dev.off()
cat("Ridge lambda.min:", round(cv_ridge$lambda.min, 4),
    "| lambda.1se:", round(cv_ridge$lambda.1se, 4), "\n")

ridge_cmp <- data.frame(
  term = colnames(model.matrix(form_full, reg_df)),
  OLS = as.numeric(coef(fit_full)),
  ridge_min = as.numeric(coef(cv_ridge, s = "lambda.min")),
  ridge_1se = as.numeric(coef(cv_ridge, s = "lambda.1se"))
)
print(ridge_cmp)
save_table(ridge_cmp, "w4_ridge_vs_ols")

# =====================================================================
# 6. Principal-components regression (10-fold CV to choose k)
# =====================================================================
set.seed(2026)
Xp <- Xr[, apply(Xr, 2, sd) > 0, drop = FALSE]
K <- ncol(Xp)
folds <- sample(rep(1:10, length.out = n))
err <- matrix(NA, n, K)
for (f in 1:10) {
  tr <- folds != f
  mu <- colMeans(Xp[tr, ]); s <- apply(Xp[tr, ], 2, sd); s[s == 0] <- 1
  Ztr <- scale(Xp[tr, ], mu, s)
  Zte <- scale(Xp[!tr, , drop = FALSE], mu, s)
  pcf <- prcomp(Ztr, scale. = FALSE)
  for (k in 1:K) {
    sc_tr <- pcf$x[, 1:k, drop = FALSE]
    fk <- lm(yr[tr] ~ sc_tr)
    pred <- cbind(1, Zte %*% pcf$rotation[, 1:k, drop = FALSE]) %*% coef(fk)
    err[!tr, k] <- yr[!tr] - pred
  }
}
cv_pcr <- apply(err, 2, rmse)
k_best <- which.min(cv_pcr)
cat("PCR: best number of components =", k_best, " CV RMSE =", round(min(cv_pcr), 4), "\n")

# Comparison of CV RMSE: OLS (full), ridge, PCR
set.seed(2026)
err_ols <- numeric(n)
for (f in 1:10) {
  tr <- folds != f
  fo <- lm(form_full, data = reg_df[tr, ])
  err_ols[!tr] <- yr[!tr] - suppressWarnings(predict(fo, reg_df[!tr, ]))
}
cv_tab <- data.frame(method = c("OLS full", "Ridge (lambda.min)", paste0("PCR (k=", k_best, ")")),
                     CV_RMSE = c(rmse(err_ols), sqrt(min(cv_ridge$cvm)), min(cv_pcr)))
print(cv_tab)
save_table(cv_tab, "w4_cv_comparison")

png(paste0(path_figures, "w4_pcr_cv.png"), width = 700, height = 500)
plot(1:K, cv_pcr, type = "b", xlab = "Number of principal components",
     ylab = "10-fold CV RMSE", main = "PCR component selection")
abline(v = k_best, lty = 2, col = "red")
dev.off()

# =====================================================================
# 7. Model building: block F-tests, added-variable plots, curvature checks
# =====================================================================
blocks <- list(
  prior_HC_record   = c("PrevHCAnyLevel", "AllPriorWinPct", "PreviousWinPct",
                        "PrevHCAnyLevel:sp_prev_rel", "WonCCasHCany"),
  coordinator_bg    = c("PreviousOC", "PreviousDC", "SideOfBall"),
  ties_and_pedigree = c("ConnectionToHiring.School", "PlayedNFL",
                        "NFLCoachingExperience", "PromoteWithin"),
  hiring_context    = c("team_sp_recent", "Conf.LeavingP4", "AgeAtHiringMinus50")
)
block_res <- do.call(rbind, lapply(names(blocks), function(b) {
  f_drop <- update(fit_full, as.formula(paste(". ~ . -", paste(blocks[[b]], collapse = " - "))))
  a <- anova(f_drop, fit_full)
  data.frame(block = b, n_terms = length(blocks[[b]]),
             F = a$F[2], df1 = a$Df[2], df2 = a$Res.Df[2], p_value = a$`Pr(>F)`[2])
}))
block_res$p_holm <- p.adjust(block_res$p_value, "holm")
print(block_res)
save_table(block_res, "w4_block_F_tests")

# Added-variable and component+residual plots (main-effects version, so car can handle it)
form_main <- update(form_full, . ~ . - PrevHCAnyLevel:sp_prev_rel + sp_prev_rel)
fit_main <- lm(form_main, data = reg_df)
png(paste0(path_figures, "w4_added_variable.png"), width = 1400, height = 1200)
tryCatch(avPlots(fit_main, ask = FALSE), error = function(e) message("avPlots failed: ", e$message))
dev.off()
png(paste0(path_figures, "w4_crplots.png"), width = 1400, height = 1000)
tryCatch(crPlots(fit_main, ask = FALSE), error = function(e) message("crPlots failed: ", e$message))
dev.off()

# Curvature: add centered squared term for each continuous predictor
curv <- do.call(rbind, lapply(c("AllPriorWinPct", "PreviousWinPct", "AgeAtHiringMinus50",
                                "team_sp_recent", "NFLCoachingExperience"), function(v) {
  reg_df$.sq <- (reg_df[[v]] - mean(reg_df[[v]]))^2
  f2 <- update(fit_full, . ~ . + .sq, data = reg_df)
  a <- anova(fit_full, f2)
  data.frame(variable = v, F = a$F[2], p_value = a$`Pr(>F)`[2])
}))
curv$p_holm <- p.adjust(curv$p_value, "holm")
print(curv)
save_table(curv, "w4_curvature_tests")

# Interaction check: does prior HC experience modify the team_sp_recent effect?
f_int <- update(fit_full, . ~ . + PrevHCAnyLevel:team_sp_recent)
print(anova(fit_full, f_int))

# Save for Week 5
saveRDS(form_reduced, "data/processed/form_reduced.rds")
cat("\nWeek 4 done. Reduced formula saved.\n")

