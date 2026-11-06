# R/12_resampling.R -- Jackknife + Bootstrap for AllPriorWinPct
# Uses confirm_model.rds (form_full), which is guaranteed to contain
# AllPriorWinPct regardless of BIC selection.
source("R/00_setup.R")
reg_df <- readRDS(path_reg_df)
fm     <- readRDS("data/processed/confirm_model.rds")
form   <- fm$formula

if (!"AllPriorWinPct" %in% attr(terms(form), "term.labels"))
  stop("AllPriorWinPct absent from resampling formula; check confirm_model.rds")

fit <- lm(form, data = reg_df)
cat("Resampling model: n =", nobs(fit),
    " | AllPriorWinPct coef =", round(coef(fit)["AllPriorWinPct"], 4), "\n")

# ---- Jackknife ----
jack_fn <- function(idx) {
  tryCatch(coef(lm(form, data = reg_df[idx, ]))["AllPriorWinPct"],
           error = function(e) NA_real_)
}
set.seed(2026)
jack_out <- jackknife(1:nrow(reg_df), jack_fn)
cat("Jackknife bias:", round(jack_out$jack.bias, 6), "\n")
cat("Jackknife SE  :", round(jack_out$jack.se,   6), "\n")

# ---- Bootstrap ----
boot_fn <- function(data, idx) {
  tryCatch(coef(lm(form, data = data[idx, ])),
           error = function(e) rep(NA_real_, length(coef(fit))))
}
set.seed(2026)
boot_out <- boot(reg_df, boot_fn, R = 1000)

target_idx <- which(names(coef(fit)) == "AllPriorWinPct")
cat("Bootstrap CIs (AllPriorWinPct):\n")
print(boot.ci(boot_out, type = c("norm", "perc"), index = target_idx))

# ---- Figure 6 ----
boot_vals <- boot_out$t[, target_idx]
ci_perc   <- quantile(boot_vals, c(0.025, 0.975), na.rm = TRUE)
png(paste0(path_figures, "fig6_bootstrap_coef.png"), width = 900, height = 600)
hist(boot_vals, breaks = 40, col = "lightsteelblue", border = "white",
     main = "Bootstrap distribution: AllPriorWinPct coefficient",
     xlab = "Coefficient value")
abline(v = ci_perc, col = "blue", lwd = 2, lty = 2)
abline(v = coef(fit)["AllPriorWinPct"], col = "red", lwd = 2)
legend("topright", legend = c("95% percentile CI", "OLS estimate"),
       col = c("blue","red"), lty = c(2,1), lwd = 2)
dev.off()
cat("Saved figures/fig6_bootstrap_coef.png\n")

