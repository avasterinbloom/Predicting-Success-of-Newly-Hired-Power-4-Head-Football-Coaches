# R/01_explore.R
source("R/00_setup.R")
full_df <- read.csv(path_raw, stringsAsFactors=FALSE)
full_df$seasons_completed <- full_df$last_season - full_df$first_season + 1
reg_df <- full_df %>%
  dplyr::filter(!is.na(seasons_completed), seasons_completed >= 2,
                StillEmployedAtSchool == 0 | first_season <= 2021)
if (any(reg_df$name=="Sam Pittman" & reg_df$school=="Arkansas")) {
  reg_df <- reg_df[!(reg_df$name=="Sam Pittman" & reg_df$school=="Arkansas"), ]
}
saveRDS(reg_df,  path_reg_df)
saveRDS(full_df, path_full_df)
save_table(reg_df[, c("name","school","first_season","last_season",
                       "seasons_completed","StillEmployedAtSchool")],
           "exclusion_list")
nv <- intersect(c("sp_rel_team","winpct_rel_team","AllPriorWinPct",
                  "PreviousWinPct","AgeAtHiringMinus50","team_sp_recent"),
                names(reg_df))
save_table(data.frame(variable=nv,
  mean  =sapply(reg_df[nv],mean,  na.rm=TRUE),
  sd    =sapply(reg_df[nv],sd,    na.rm=TRUE),
  median=sapply(reg_df[nv],median,na.rm=TRUE)),
  "descriptive_stats")
cat("01_explore.R done\n")
