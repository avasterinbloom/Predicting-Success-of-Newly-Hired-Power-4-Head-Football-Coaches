# R/14_holdout.R -- prospective holdout on 2022+ hires (proper split)
# Deviation from plan: holdout is explicitly EXCLUDED from training
# (anti_join), and both models are refit on train-only.
source("R/00_setup.R")
source("lib/00_model_defs.R")   # provides form_full

full_df <- readRDS(path_full_df)
reg_df  <- readRDS(path_reg_df)

# ---- 1. Same imputation rules as pipeline/survival ----
full_df$AllPriorWinPct[is.na(full_df$AllPriorWinPct) & full_df$PrevHCAnyLevel == 0] <- 0.50
still <- is.na(full_df$AllPriorWinPct) & full_df$PrevHCAnyLevel == 1
if (any(still)) full_df$AllPriorWinPct[still] <- full_df$PreviousWinPct[still]
full_df$PreviousWinPct[is.na(full_df$PreviousWinPct) & full_df$PrevHCAnyLevel == 0] <- 0.50
full_df$sp_prev_rel[is.na(full_df$sp_prev_rel)] <- mean(reg_df$sp_prev_rel, na.rm = TRUE)

# ---- 2. Holdout filter (bias-fix: no StillEmployedAtSchool filter) ----
full_df$seasons_completed <- full_df$last_season - full_df$first_season + 1
test_df <- full_df %>%
  dplyr::filter(first_season >= 2022,
                seasons_completed >= 1,
                !is.na(sp_rel_team))
cat("Holdout n:", nrow(test_df), "\n")
if (nrow(test_df) < 10)
  warning("Holdout n < 10 -- coverage estimates are indicative only")

# ---- 3. Split: training = reg_df minus holdout ----
train_df <- dplyr::anti_join(reg_df, test_df,
                             by = c("name", "school", "first_season"))
cat("Train n (excl. holdout):", nrow(train_df),
    "| overlap removed:", nrow(reg_df) - nrow(train_df), "\n")

# ---- 4. Harmonize types between train_df and test_df ----
auto_num <- function(x) {
  if (is.character(x)) {
    blank <- is.na(x) | x %in% c("", "NA")
    y <- suppressWarnings(as.numeric(x))
    if (sum(is.na(y)) == sum(blank)) return(y)
  }
  x
}
train_df[] <- lapply(train_df, auto_num)
test_df[]  <- lapply(test_df,  auto_num)

cat("\n=== form_full vars in test_df (types + NA) ===\n")
for (v in all.vars(form_full)) {
  if (!v %in% names(test_df)) { cat(sprintf("%-28s NOT IN test_df\n", v)); next }
  cat(sprintf("%-28s type=%-10s NA=%d\n",
              v, class(test_df[[v]])[1], sum(is.na(test_df[[v]]))))
}

# ---- 5. Factor alignment + numeric imputation ----
for (v in all.vars(form_full)) {
  if (!v %in% names(test_df)) next
  # character -> factor, using train levels (avoids silent NA from model.matrix)
  if (is.character(train_df[[v]])) {
    lvls <- sort(unique(train_df[[v]]))
    train_df[[v]] <- factor(train_df[[v]], levels = lvls)
    test_df[[v]]  <- factor(test_df[[v]],  levels = lvls)
    if (any(is.na(test_df[[v]]))) {
      tr_mode <- names(which.max(table(train_df[[v]])))
      n_imp   <- sum(is.na(test_df[[v]]))
      test_df[[v]][is.na(test_df[[v]])] <- tr_mode
      cat(sprintf("  imputed %d NA in test_df$%s <- '%s' (train mode)\n",
                  n_imp, v, tr_mode))
    }
  }
  # numeric -> mean imputation
  if (is.numeric(test_df[[v]]) && any(is.na(test_df[[v]]))) {
    tr_mean <- mean(train_df[[v]], na.rm = TRUE)
    if (!is.nan(tr_mean)) {
      n_imp <- sum(is.na(test_df[[v]]))
      test_df[[v]][is.na(test_df[[v]])] <- tr_mean
      cat(sprintf("  imputed %d NA in test_df$%s <- %.3f (train mean)\n",
                  n_imp, v, tr_mean))
    }
  }
}

cat("\n=== NA check on test_df AFTER imputation ===\n")
rem_na <- sapply(all.vars(form_full), function(v)
  if (v %in% names(test_df)) sum(is.na(test_df[[v]])) else -1L)
print(rem_na)
cat("Complete test rows:", sum(complete.cases(test_df[, all.vars(form_full)])),
    "of", nrow(test_df), "\n")

# ---- 5. Factor alignment ----
if ("SideOfBall" %in% names(test_df))
  test_df$SideOfBall <- factor(test_df$SideOfBall,
                               levels = levels(train_df$SideOfBall))

# ---- 6. Refit both models on train_df ----
n_tr <- nrow(train_df)
fit0_tr <- lm(sp_rel_team ~ 1, data = train_df)
fit_bic_tr <- step(fit0_tr,
                   scope = list(lower = ~1, upper = form_full),
                   direction = "both", k = log(n_tr), trace = 0)
cat("BIC model on train:", deparse(formula(fit_bic_tr)), "\n")
fit_conf_tr <- lm(form_full, data = train_df)
cat("Confirm n on train:", nobs(fit_conf_tr), "\n")

# ---- 7. Predict ----
preds_bic  <- predict(fit_bic_tr,  newdata = test_df,
                      interval = "prediction", level = 0.95)
preds_conf <- predict(fit_conf_tr, newdata = test_df,
                      interval = "prediction", level = 0.95)

# ---- 8. Coverage with guards ----
compute_coverage <- function(test_df, preds, label) {
  in_pi  <- test_df$sp_rel_team >= preds[, "lwr"] &
            test_df$sp_rel_team <= preds[, "upr"]
  n_eval <- sum(!is.na(in_pi))
  if (n_eval == 0) {
    cat("\n=== ", label, " ===\n", sep="")
    cat("n_eval: 0 -- NO valid predictions\n")
    return(list(cov = NA, ci = c(NA, NA), n_eval = 0, rmse = NA, mae = NA))
  }
  hits <- sum(in_pi, na.rm = TRUE)
  cov  <- hits / n_eval
  ci   <- if (hits > 0 && hits < n_eval) binom.test(hits, n_eval)$conf.int
          else                                  c(NA_real_, NA_real_)
  resid <- test_df$sp_rel_team - preds[, "fit"]
  cat("\n=== ", label, " ===\n", sep="")
  cat("n_eval:", n_eval, "| Coverage:", round(cov, 3),
      "| 95% CI [", round(ci[1], 3), ",", round(ci[2], 3), "]\n")
  cat("RMSE:", round(sqrt(mean(resid^2, na.rm = TRUE)), 3),
      "| MAE:", round(mean(abs(resid), na.rm = TRUE), 3), "\n")
  list(cov = cov, ci = ci, n_eval = n_eval,
       rmse = sqrt(mean(resid^2, na.rm = TRUE)),
       mae  = mean(abs(resid), na.rm = TRUE))
}
res_bic  <- compute_coverage(test_df, preds_bic,  "BIC on train, predict holdout")
res_conf <- compute_coverage(test_df, preds_conf, "Confirm on train, predict holdout")

# ---- 9. Save tables ----
save_table(data.frame(
  model       = c("BIC on train", "Confirm on train"),
  n_train     = c(nobs(fit_bic_tr), nobs(fit_conf_tr)),
  n_holdout   = c(nrow(test_df), nrow(test_df)),
  n_eval      = c(res_bic$n_eval, res_conf$n_eval),
  PI_coverage = c(res_bic$cov, res_conf$cov),
  CI_lo       = c(res_bic$ci[1], res_conf$ci[1]),
  CI_hi       = c(res_bic$ci[2], res_conf$ci[2]),
  RMSE        = c(res_bic$rmse, res_conf$rmse),
  MAE         = c(res_bic$mae, res_conf$mae)
), "holdout_performance")

# ---- 10. Figure 7 (BIC model) ----
inside_bic <- test_df$sp_rel_team >= preds_bic[, "lwr"] &
              test_df$sp_rel_team <= preds_bic[, "upr"]
fig7 <- ggplot(data.frame(
  actual = test_df$sp_rel_team,
  fit    = preds_bic[, "fit"],
  lwr    = preds_bic[, "lwr"],
  upr    = preds_bic[, "upr"],
  inside = inside_bic
), aes(x = fit, y = actual, colour = inside)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.1, alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  labs(x = "Predicted CPI", y = "Actual CPI",
       title    = "95% PI Coverage on 2022+ Holdout (BIC on train-only)",
       subtitle = sprintf("n=%d | Coverage=%.3f | 95%% CI [%.3f, %.3f]",
                          res_bic$n_eval, res_bic$cov,
                          res_bic$ci[1], res_bic$ci[2]),
       colour   = "Inside PI") +
  scale_colour_manual(values = c(`TRUE` = "steelblue", `FALSE` = "firebrick"))
save_fig(fig7, "fig7_pi_coverage")
cat("\nSaved figures/fig7_pi_coverage.png\n")
