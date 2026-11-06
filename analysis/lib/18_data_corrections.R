# lib/18_data_corrections.R -- document flagged rows + sensitivity survival
suppressPackageStartupMessages({library(survival); library(broom)})
source("R/00_setup.R")

full <- read.csv(path_raw, stringsAsFactors = FALSE)
full$seasons_completed <- full$last_season - full$first_season + 1

# --- 1. Document flagged rows ---
flag_names <- c("Hugh Freeze", "Luke Fickell", "Brian Kelly",
                "Mike Gundy", "James Franklin")
flagged <- full[full$name %in% flag_names & full$first_season < 2026,
                c("name","school","first_season","last_season",
                  "Fired","StillEmployedAtSchool","OffFieldIssuesFiring")]

# Manual issue/action tags per coach (matched by name+school)
tag_issue <- c(
  "Hugh Freeze|Auburn"             = "Internal contradiction: Fired=1 and StillEmployed=1",
  "Luke Fickell|Wisconsin"         = "Internal contradiction: Fired=1 and StillEmployed=1",
  "Brian Kelly|LSU"                = "External record: still LSU HC 2025; StillEmployedAtSchool=0 incorrect",
  "Mike Gundy|Oklahoma State"      = "External record: still OSU HC 2025; Fired=1 and StillEmployed=0 incorrect",
  "James Franklin|Penn State"      = "External record: still PSU HC 2025; Fired=1 and StillEmployed=0 incorrect"
)
tag_action <- c(
  "Hugh Freeze|Auburn"             = "RETAINED as-is in primary; flagged",
  "Luke Fickell|Wisconsin"         = "RETAINED as-is in primary; flagged",
  "Brian Kelly|LSU"                = "EXCLUDED in sensitivity survival",
  "Mike Gundy|Oklahoma State"      = "EXCLUDED in sensitivity survival",
  "James Franklin|Penn State"      = "EXCLUDED in sensitivity survival"
)
key <- paste(flagged$name, flagged$school, sep = "|")
flagged$issue  <- tag_issue[key]
flagged$action <- tag_action[key]

# Keep only rows we actually flag (drop unrelated duplicate hires like Fickell@Cincinnati)
flagged <- flagged[!is.na(flagged$issue), ]
save_table(flagged, "data_corrections")
cat("=== data_corrections.csv ===\n")
print(flagged[, c("name","school","Fired","StillEmployedAtSchool","issue")])

# --- 2. Survival sensitivity: primary vs excluding the 3 external flags ---
full$AllPriorWinPct[is.na(full$AllPriorWinPct) & full$PrevHCAnyLevel == 0] <- 0.50
still <- is.na(full$AllPriorWinPct) & full$PrevHCAnyLevel == 1
full$AllPriorWinPct[still] <- full$PreviousWinPct[still]
full$TenureLength <- full$last_season - full$first_season + 1
full$EventFired   <- full$Fired * (1 - full$OffFieldIssuesFiring)

sd <- subset(full, !is.na(TenureLength) & !is.na(EventFired) &
             !is.na(PrevHCAnyLevel) & !is.na(AllPriorWinPct) &
             !is.na(AgeAtHiringMinus50))

# Exclude Brian Kelly, Mike Gundy, James Franklin (external flag concerns)
sd_ex <- sd[!(sd$name %in% c("Brian Kelly","Mike Gundy","James Franklin")), ]

fit_one <- function(d, tag) {
  lr <- survdiff(Surv(TenureLength, EventFired) ~ PrevHCAnyLevel, data = d)
  cx <- coxph(Surv(TenureLength, EventFired) ~
              AllPriorWinPct + PrevHCAnyLevel + AgeAtHiringMinus50, data = d)
  s  <- summary(cx)$coefficients
  data.frame(
    version        = tag,
    n              = nrow(d),
    events         = sum(d$EventFired),
    logrank_chisq  = round(lr$chisq, 3),
    logrank_p      = round(1 - pchisq(lr$chisq, df = length(lr$n) - 1), 3),
    cox_APWP_HR    = round(exp(s["AllPriorWinPct","coef"]), 3),
    cox_APWP_p     = round(s["AllPriorWinPct","Pr(>|z|)"], 3),
    cox_PrevHC_HR  = round(exp(s["PrevHCAnyLevel","coef"]), 3),
    cox_PrevHC_p   = round(s["PrevHCAnyLevel","Pr(>|z|)"], 3)
  )
}

tab <- rbind(fit_one(sd,    "primary (n=120)"),
             fit_one(sd_ex, "excluding 3 flagged"))
save_table(tab, "survival_sensitivity")
cat("\n=== survival_sensitivity.csv ===\n")
print(tab, row.names = FALSE)

