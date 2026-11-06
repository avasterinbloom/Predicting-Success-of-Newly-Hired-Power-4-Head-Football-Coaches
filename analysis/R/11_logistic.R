# R/11_logistic.R -- Logistic regression for Retained4
# Part D of the reference W5 script.
source("R/00_setup.R")
source("lib/00_model_defs.R")
reg_df <- prep_reg_df()
log_df <- reg_df[!is.na(reg_df$Retained4), ]

cat("\nRetained4 rate:", round(mean(log_df$Retained4), 3), " n =", nrow(log_df), "\n")
form_logit <- update(form_full, Retained4 ~ .)    # pre-hire predictors only
fit_logit <- glm(form_logit, data = log_df, family = binomial)
print(summary(fit_logit))

# Odds ratios + profile-likelihood CIs
or_tab <- data.frame(term = names(coef(fit_logit)), OR = exp(coef(fit_logit)))
ci_or <- tryCatch(exp(confint(fit_logit)), error = function(e) exp(confint.default(fit_logit)))
or_tab$lo <- ci_or[, 1]; or_tab$hi <- ci_or[, 2]
or_tab$p <- summary(fit_logit)$coefficients[, 4]
print(or_tab, digits = 3)
save_table(or_tab, "w5_logit_odds_ratios")

# Global LRT and per-term LRTs
cat("\nGlobal LRT: chi2 =", round(fit_logit$null.deviance - fit_logit$deviance, 2),
    "df =", length(coef(fit_logit)) - 1, "p =",
    signif(pchisq(fit_logit$null.deviance - fit_logit$deviance,
                  length(coef(fit_logit)) - 1, lower.tail = FALSE), 3), "\n")
print(drop1(fit_logit, test = "Chisq"))

# Pseudo-R2
ll1 <- as.numeric(logLik(fit_logit)); ll0 <- as.numeric(logLik(update(fit_logit, . ~ 1)))
nn <- nrow(log_df)
mcf <- 1 - ll1 / ll0
nag <- (1 - exp(2 * (ll0 - ll1) / nn)) / (1 - exp(2 * ll0 / nn))
cat("McFadden R2:", round(mcf, 3), "| Nagelkerke R2:", round(nag, 3), "\n")

# Goodness of fit: Hosmer-Lemeshow
hl <- hoslem.test(fit_logit$y, fitted(fit_logit), g = 10)
print(hl)

# Discrimination: ROC / AUC, Youden threshold, classification table
auc <- function(y, p) {
  r <- rank(p); n1 <- sum(y == 1); n0 <- sum(y == 0)
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}
yhat <- fitted(fit_logit); yy <- fit_logit$y
thr <- sort(unique(c(0, yhat, 1)))
roc <- data.frame(thr = thr,
                  tpr = sapply(thr, function(t) mean(yhat[yy == 1] >= t)),
                  fpr = sapply(thr, function(t) mean(yhat[yy == 0] >= t)))
t_opt <- roc$thr[which.max(roc$tpr - roc$fpr)]
cat("Apparent AUC:", round(auc(yy, yhat), 3), "| Youden threshold:", round(t_opt, 3), "\n")
p_roc <- ggplot(roc, aes(fpr, tpr)) + geom_line(colour = "steelblue", linewidth = 1) +
  geom_abline(linetype = 2) +
  labs(title = paste0("ROC curve (AUC = ", round(auc(yy, yhat), 3), ")"),
       x = "False positive rate", y = "True positive rate")
save_fig(p_roc, "w5_logit_roc")
for (t in c(0.5, t_opt)) {
  cat("\nClassification table at threshold", round(t, 3), "\n")
  tb <- table(Observed = yy, Predicted = as.integer(yhat >= t)); print(tb)
  cat("Accuracy:", round(mean(yy == as.integer(yhat >= t)), 3), "\n")
}

# 10-fold CV error & AUC (honest out-of-sample performance)
set.seed(2026)
cost <- function(r, pi) mean(abs(r - pi) > 0.5)
cv_err <- cv.glm(log_df, fit_logit, cost = cost, K = 10)$delta[1]
fl <- sample(rep(1:10, length.out = nn)); p_cv <- numeric(nn)
for (f in 1:10) {
  mf <- glm(form_logit, data = log_df[fl != f, ], family = binomial)
  p_cv[fl == f] <- suppressWarnings(predict(mf, log_df[fl == f, ], type = "response"))
}
cat("10-fold CV misclassification:", round(cv_err, 3),
    "| baseline (majority class):", round(min(mean(yy), 1 - mean(yy)), 3),
    "| CV AUC:", round(auc(yy, p_cv), 3), "\n")

# Calibration by decile
cal <- log_df %>%
  mutate(p = yhat, bin = ntile(p, 10)) %>%
  group_by(bin) %>% summarise(mean_pred = mean(p), obs_rate = mean(Retained4), n = n())
print(cal); save_table(cal, "w5_logit_calibration")
p_cal <- ggplot(cal, aes(mean_pred, obs_rate)) + geom_point(size = 2) + geom_line() +
  geom_abline(linetype = 2) + labs(x = "Mean predicted", y = "Observed rate",
                                   title = "Calibration (deciles)")
save_fig(p_cal, "w5_logit_calibration")

# Diagnostics: deviance residuals, leverage, Cook's D
dres <- residuals(fit_logit, "deviance"); cdl <- cooks.distance(fit_logit)
png(paste0(path_figures, "w5_logit_diagnostics.png"), width = 1100, height = 450)
par(mfrow = c(1, 2))
plot(yhat, dres, xlab = "Fitted probability", ylab = "Deviance residual"); abline(h = 0, lty = 2)
plot(cdl, type = "h", ylab = "Cook's D", main = "Influence"); abline(h = 4 / nn, col = "red", lty = 2)
dev.off()
cat("Obs with Cook's D > 4/n:", sum(cdl > 4 / nn), "\n")
print(tryCatch(car::vif(fit_logit), error = function(e) "VIF not available (aliasing)"))

# Variable selection (BIC) and penalized logistic (guards against separation)
fit_logit_bic <- step(glm(Retained4 ~ 1, data = log_df, family = binomial),
                      scope = list(lower = ~1, upper = form_logit),
                      direction = "both", k = log(nn), trace = 0)
cat("\nBIC-selected logistic model:", deparse(formula(fit_logit_bic)), "\n")
print(summary(fit_logit_bic)$coefficients)
Xl <- model.matrix(form_logit, log_df)[, -1]
set.seed(2026)
cv_lasso_log <- cv.glmnet(Xl, log_df$Retained4, family = "binomial", alpha = 1,
                          nfolds = 10, type.measure = "auc")
print(coef(cv_lasso_log, s = "lambda.1se"))
cat("LASSO-logistic CV AUC:", round(max(cv_lasso_log$cvm), 3), "\n")

cat("\nWeek 5 done.\n")
