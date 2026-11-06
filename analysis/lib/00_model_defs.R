# R/00_model_defs.R -- shared definitions for Weeks 4-5
# Usage: source("lib/00_model_defs.R"); reg_df <- prep_reg_df()
source("R/00_setup.R")

# Convert character columns to numeric when every non-missing value is numeric
auto_num <- function(x) {
  if (is.character(x)) {
    blank <- is.na(x) | x %in% c("", "NA")
    y <- suppressWarnings(as.numeric(x))
    if (sum(is.na(y)) == sum(blank)) return(y)
  }
  x
}

prep_reg_df <- function() {
  d <- readRDS(path_reg_df)
  d[] <- lapply(d, auto_num)
  # Same imputation used in Week 2 (non-HCs / NFL-FCS coaches -> sample mean)
  d$sp_prev_rel[is.na(d$sp_prev_rel)] <- mean(d$sp_prev_rel, na.rm = TRUE)
  # Binary outcome for logistic regression (Retained4)
  d$seasons_completed <- d$last_season - d$first_season + 1
  d$Retained4 <- as.integer(d$seasons_completed >= 4 &
                              d$OffFieldIssuesFiring == 0)
  d$SideOfBall <- factor(d$SideOfBall)
  if ("Conf.Hired" %in% names(d)) d$Conf.Hired <- factor(d$Conf.Hired)
  d$school <- factor(d$school)
  d
}

# Week 2/3 full model (interaction version)
form_full <- sp_rel_team ~
  PrevHCAnyLevel + AllPriorWinPct + PreviousWinPct +
  PreviousOC + PreviousDC + SideOfBall +
  ConnectionToHiring.School + PlayedNFL +
  AgeAtHiringMinus50 + NFLCoachingExperience +
  PrevHCAnyLevel:sp_prev_rel + WonCCasHCany + Conf.LeavingP4 +
  team_sp_recent + PromoteWithin

# Helpers
press_stat <- function(fit) {
  h <- hatvalues(fit); e <- residuals(fit)
  sum((e / (1 - h))^2)
}
rmse <- function(e) sqrt(mean(e^2))
