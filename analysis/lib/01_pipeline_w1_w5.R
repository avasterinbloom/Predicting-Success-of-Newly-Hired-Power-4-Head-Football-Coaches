# R/01_pipeline_w1_w5.R
# Minimal W1-W5 producer: reg_df.rds, full_df.rds, final_model.rds
# Run once:  Rscript R/01_pipeline_w1_w5.R

source("R/00_setup.R")

# ---- 1. Read raw ----
full_df <- read.csv(path_raw, stringsAsFactors = FALSE)
cat("Raw rows:", nrow(full_df), "\n")

# ---- 2. Sample restriction (>= 2 seasons; documented deviation from >= 3) ----
full_df$seasons_completed <- full_df$last_season - full_df$first_season + 1
reg_df <- full_df %>%
  dplyr::filter(!is.na(seasons_completed)) %>%
  dplyr::filter(seasons_completed >= 2) %>%
  dplyr::filter(StillEmployedAtSchool == 0 | first_season <= 2021)
cat("reg_df rows:", nrow(reg_df), "\n")

# ---- 2b. Exclude Sam Pittman (miscoded PrevHCAnyLevel) ----
# Raw CSV flags Pittman (Arkansas, 2020) as a prior HC but records
# Wins.at.Prev=0 and Losses.at.Prev=0 with PreviousWinPct=0.5 (the no-HC
# placeholder). Imputing AllPriorWinPct from PreviousWinPct would be
# circular — imputing a variable from an already-structurally-imputed
# value. Exclude and document the exclusion in the report.
if (any(reg_df$name == "Sam Pittman" & reg_df$school == "Arkansas")) {
  n_before <- nrow(reg_df)
  reg_df <- reg_df[!(reg_df$name == "Sam Pittman" &
                     reg_df$school == "Arkansas"), ]
  cat("Excluded Sam Pittman (miscoded PrevHCAnyLevel); reg_df ",
      n_before, " -> ", nrow(reg_df), "\n", sep = "")
}

# ---- 3. Structural imputation ----
# AllPriorWinPct / PreviousWinPct are NA for non-HCs by construction.
# Impute to 0.50 (neutral prior) so lm() does not drop those rows.
reg_df$AllPriorWinPct[is.na(reg_df$AllPriorWinPct) & reg_df$PrevHCAnyLevel == 0] <- 0.50
reg_df$PreviousWinPct[is.na(reg_df$PreviousWinPct) & reg_df$PrevHCAnyLevel == 0] <- 0.50
# sp_prev_rel: mean for coordinators / first-time HCs; the
# PrevHCAnyLevel:sp_prev_rel interaction zeroes them out in the model.
reg_df$sp_prev_rel[is.na(reg_df$sp_prev_rel)] <- mean(reg_df$sp_prev_rel, na.rm = TRUE)

# ---- 3b. Remaining NA in AllPriorWinPct ----
# Chad Morris (2017 Arkansas), Sam Pittman (2020 Arkansas), Dana Holgorsen
# (2019 Houston): flagged as PrevHCAnyLevel == 1 in the raw CSV but
# AllPriorWinPct is NA (data inconsistency). Fall back to PreviousWinPct,
# which holds their prior HC record. Log the imputation for the report.
still_na <- is.na(reg_df$AllPriorWinPct)
if (any(still_na)) {
  imp_log <- reg_df[still_na, c("name", "school", "first_season",
                                "PrevHCAnyLevel", "PreviousWinPct", "AllPriorWinPct")]
  write.csv(imp_log, "tables/allpriorwinpct_imputation_log.csv", row.names = FALSE)
  cat("Imputing AllPriorWinPct for", sum(still_na), "rows (see tables/allpriorwinpct_imputation_log.csv):\n")
  print(imp_log)
  reg_df$AllPriorWinPct[still_na] <- reg_df$PreviousWinPct[still_na]
}

saveRDS(reg_df,  path_reg_df)
saveRDS(full_df, path_full_df)

# ---- 4. Prep via shared defs ----
source("lib/00_model_defs.R")
d <- prep_reg_df()
cat("After prep: n =", nrow(d),
    "| complete.cases =", sum(complete.cases(d[, all.vars(form_full)])), "\n")

# ---- 5. Baseline fit ----
fit_full <- lm(form_full, data = d)
cat("Baseline: n =", nobs(fit_full),
    " | R2 =", round(summary(fit_full)$r.squared, 3), "\n")

# ---- 6. BIC stepwise -> final model ----
n <- nrow(d)
fit0 <- lm(sp_rel_team ~ 1, data = d)
fit_bic <- step(fit0,
                scope = list(lower = ~1, upper = form_full),
                direction = "both",
                k = log(n), trace = 0)
cat("Final model formula:\n"); print(formula(fit_bic))

saveRDS(list(formula = formula(fit_bic), fit = fit_bic),
        "data/processed/final_model.rds")
cat("Saved data/processed/final_model.rds\n")
