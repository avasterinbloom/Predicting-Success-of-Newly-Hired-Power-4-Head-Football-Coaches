# R/08_model_selection.R -- AIC/BIC/Cp/FPE + final model
source("R/00_setup.R")
source("lib/00_model_defs.R")
reg_df   <- prep_reg_df()
n        <- nrow(reg_df)
fit_full <- lm(form_full, data = reg_df)
fit0     <- lm(sp_rel_team ~ 1, data = reg_df)
fit_bic  <- step(fit0, scope=list(lower=~1, upper=form_full),
                 direction="both", k=log(n), trace=0)
fit_aic  <- step(fit0, scope=list(lower=~1, upper=form_full),
                 direction="both", k=2,     trace=0)
set.seed(2026)
folds <- sample(rep(1:10, length.out=n))
sst <- sum((reg_df$sp_rel_team - mean(reg_df$sp_rel_team, na.rm=TRUE))^2, na.rm=TRUE)
rss_full <- sum(residuals(fit_full)^2)
sigma2_full <- rss_full / (n - length(coef(fit_full)))
cands <- list(Null            = lm(sp_rel_team ~ 1, data=reg_df),
              PreviousOC_only = lm(sp_rel_team ~ PreviousOC, data=reg_df),
              BIC_selected    = fit_bic,
              Confirmatory    = fit_full)
cmp <- do.call(rbind, lapply(names(cands), function(nm) {
  m <- cands[[nm]]; rss <- sum(residuals(m)^2); p <- length(coef(m))
  e <- numeric(n)
  for (f in 1:10) { tr <- folds != f
    mm <- lm(formula(m), data=reg_df[tr, ])
    e[!tr] <- reg_df$sp_rel_team[!tr] - suppressWarnings(predict(mm, reg_df[!tr, ]))
  }
  data.frame(Model=nm, p=p, R2=summary(m)$r.squared,
             adjR2=summary(m)$adj.r.squared, AIC=AIC(m), BIC=BIC(m),
             Cp=rss/sigma2_full-(n-2*p), FPE=(rss/n)*(n+p)/(n-p),
             CV_RMSE=sqrt(mean(e^2,na.rm=TRUE)),
             CV_R2=1-sum(e^2,na.rm=TRUE)/sst)
}))
save_table(cmp, "model_comparison")
saveRDS(list(formula=formula(fit_bic),  fit=fit_bic),  "data/processed/final_model.rds")
saveRDS(list(formula=form_full, fit=fit_full),          "data/processed/confirm_model.rds")
saveRDS(list(formula=formula(fit_aic), fit=fit_aic),    "data/processed/aic_model.rds")
save_table(data.frame(
  model=c("reg_df","Null","PreviousOC only","BIC-selected","Confirmatory (full)"),
  n=c(n, nobs(cands$Null), nobs(cands$PreviousOC_only), nobs(fit_bic), nobs(fit_full)),
  dropped_for_na=c(0, n-nobs(cands$Null), n-nobs(cands$PreviousOC_only),
                   n-nobs(fit_bic), n-nobs(fit_full))),
  "model_n_counts")
cat("08_model_selection.R done\n")
