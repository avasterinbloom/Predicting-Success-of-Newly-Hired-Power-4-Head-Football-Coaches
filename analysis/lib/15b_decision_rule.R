# R/15_decision_rule.R -- Coach Hire Score calculator
source("R/00_setup.R")

fm_bic  <- readRDS("data/processed/final_model.rds")
fm_conf <- readRDS("data/processed/confirm_model.rds")

# ---- Core scoring function ----
coach_hire_score <- function(newdata, fm, C = 0) {
  # align SideOfBall to the model's stored levels if present
  if ("SideOfBall" %in% names(newdata)) {
    lvls <- fm$fit$xlevels[["SideOfBall"]]
    if (!is.null(lvls))
      newdata$SideOfBall <- factor(newdata$SideOfBall, levels = lvls)
  }
  pred <- predict(fm$fit, newdata = newdata,
                  interval = "prediction", level = 0.95)
  if (any(is.na(pred[, "fit"])))
    stop("Predict returned NA -- newdata is missing a model variable")
  train_fitted <- fitted(fm$fit)
  score <- sapply(pred[, "fit"],
                  function(x) round(100 * mean(train_fitted <= x), 1))
  data.frame(fit = pred[, "fit"],
             lwr = pred[, "lwr"],
             upr = pred[, "upr"],
             score_0_100 = score)
}

# ---- Worked examples ----
# A: experienced Power-4 HC, winner, moves from a strong program
candidate_A <- data.frame(
  PrevHCAnyLevel = 1, AllPriorWinPct = 0.62, PreviousWinPct = 0.62,
  PreviousOC = 0, PreviousDC = 0, SideOfBall = "O",
  ConnectionToHiring.School = 0, PlayedNFL = 0,
  AgeAtHiringMinus50 = 2, NFLCoachingExperience = 1,
  sp_prev_rel = 12, WonCCasHCany = 1, Conf.LeavingP4 = 1,
  team_sp_recent = 8, PromoteWithin = 0
)

# C: experienced OC, strong coordinator pedigree, arrives at a middling program
candidate_C <- data.frame(
  PrevHCAnyLevel = 0, AllPriorWinPct = 0.50, PreviousWinPct = 0.50,
  PreviousOC = 1, PreviousDC = 0, SideOfBall = "O",
  ConnectionToHiring.School = 1, PlayedNFL = 0,
  AgeAtHiringMinus50 = 5, NFLCoachingExperience = 1,
  sp_prev_rel = 0, WonCCasHCany = 0, Conf.LeavingP4 = 0,
  team_sp_recent = 3, PromoteWithin = 0
)

# B: first-time HC / former coordinator, young, arrives at a weak program
candidate_B <- data.frame(
  PrevHCAnyLevel = 0, AllPriorWinPct = 0.50, PreviousWinPct = 0.50,
  PreviousOC = 1, PreviousDC = 0, SideOfBall = "O",
  ConnectionToHiring.School = 0, PlayedNFL = 0,
  AgeAtHiringMinus50 = -5, NFLCoachingExperience = 1,
  sp_prev_rel = 0, WonCCasHCany = 0, Conf.LeavingP4 = 0,
  team_sp_recent = 2, PromoteWithin = 0
)

cat("\n=== Candidate A (experienced P4 HC) ===\n")
cat("BIC model    :\n"); print(coach_hire_score(candidate_A, fm_bic),  row.names = FALSE)
cat("Confirm model:\n"); print(coach_hire_score(candidate_A, fm_conf), row.names = FALSE)

cat("\n=== Candidate C (experienced OC, middling program) ===\n")
cat("BIC model    :\n"); print(coach_hire_score(candidate_C, fm_bic),  row.names = FALSE)
cat("Confirm model:\n"); print(coach_hire_score(candidate_C, fm_conf), row.names = FALSE)


cat("\n=== Candidate B (first-time HC) ===\n")
cat("BIC model    :\n"); print(coach_hire_score(candidate_B, fm_bic),  row.names = FALSE)
cat("Confirm model:\n"); print(coach_hire_score(candidate_B, fm_conf), row.names = FALSE)

# ---- Save table ----
res <- rbind(
  cbind(candidate = "A_established_HC", model = "BIC",
        coach_hire_score(candidate_A, fm_bic)),
  cbind(candidate = "A_established_HC", model = "Confirm",
        coach_hire_score(candidate_A, fm_conf)),
  cbind(candidate = "C_experienced_OC",  model = "BIC",
        coach_hire_score(candidate_C, fm_bic)),
  cbind(candidate = "C_experienced_OC",  model = "Confirm",
        coach_hire_score(candidate_C, fm_conf)),
  cbind(candidate = "B_first_time_HC",  model = "BIC",
        coach_hire_score(candidate_B, fm_bic)),
  cbind(candidate = "B_first_time_HC",  model = "Confirm",
        coach_hire_score(candidate_B, fm_conf))
)
save_table(res, "coach_hire_score_examples")

# Save candidate profiles for the report's worked-example section
keep <- c("PrevHCAnyLevel","AllPriorWinPct","PreviousOC",
          "ConnectionToHiring.School","team_sp_recent",
          "AgeAtHiringMinus50","WonCCasHCany")
profiles <- rbind(
  cbind(candidate = "A_established_HC", candidate_A[, keep]),
  cbind(candidate = "C_experienced_OC", candidate_C[, keep]),
  cbind(candidate = "B_first_time_HC",  candidate_B[, keep])
)
save_table(profiles, "candidate_profiles")
cat("\nSaved tables/coach_hire_score_examples.csv\n")

# ---- Training distribution (for report context) ----
cat("\nTraining fitted CPI distribution (n =", nobs(fm_bic$fit), "):\n")
print(round(summary(fitted(fm_bic$fit)), 3))
