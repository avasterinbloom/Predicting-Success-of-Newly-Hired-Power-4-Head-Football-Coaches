# R/16_model_comparison.R
source("R/00_setup.R")
reg <- readRDS(path_reg_df)
fm_bic <- readRDS("data/processed/final_model.rds")
fm_conf <- readRDS("data/processed/confirm_model.rds")
f_null <- lm(sp_rel_team ~ 1, data = reg)
f_oc <- lm(sp_rel_team ~ PreviousOC, data = reg)
set.seed(2026)
n <- nrow(reg)
folds <- sample(rep(1:10, length.out = n))
cv_err <- function(form) {
  errs <- numeric(n)
  for (f in 1:10) {
    tr <- folds != f
    m <- lm(form, data = reg[tr, ])
    errs[!tr] <- reg$sp_rel_team[!tr] - suppressWarnings(predict(m, reg[!tr, ]))
  }
  errs
}
sst <- sum((reg$sp_rel_team - mean(reg$sp_rel_team, na.rm = TRUE))^2, na.rm = TRUE)
get_cv <- function(form) {
  e <- cv_err(form)
  c(RMSE = sqrt(mean(e^2, na.rm = TRUE)), R2 = 1 - sum(e^2, na.rm = TRUE) / sst)
}
cv <- list(null = get_cv(sp_rel_team ~ 1),
           oc = get_cv(sp_rel_team ~ PreviousOC),
           bic = get_cv(formula(fm_bic$fit)),
           conf = get_cv(formula(fm_conf$fit)))
res <- data.frame(
  Model = c("Null", "PreviousOC only", "BIC-selected", "Confirmatory (full)"),
  p     = c(length(coef(f_null)), length(coef(f_oc)),
            length(coef(fm_bic$fit)), length(coef(fm_conf$fit))),
  R2    = c(summary(f_null)$r.squared, summary(f_oc)$r.squared,
            summary(fm_bic$fit)$r.squared, summary(fm_conf$fit)$r.squared),
  adjR2 = c(summary(f_null)$adj.r.squared, summary(f_oc)$adj.r.squared,
            summary(fm_bic$fit)$adj.r.squared, summary(fm_conf$fit)$adj.r.squared),
  AIC   = c(AIC(f_null), AIC(f_oc), AIC(fm_bic$fit), AIC(fm_conf$fit)),
  BIC   = c(BIC(f_null), BIC(f_oc), BIC(fm_bic$fit), BIC(fm_conf$fit)),
  CV_RMSE = c(cv$null["RMSE"], cv$oc["RMSE"], cv$bic["RMSE"], cv$conf["RMSE"]),
  CV_R2   = c(cv$null["R2"], cv$oc["R2"], cv$bic["R2"], cv$conf["R2"])
)
save_table(res, "model_comparison")
print(res, digits = 3)
