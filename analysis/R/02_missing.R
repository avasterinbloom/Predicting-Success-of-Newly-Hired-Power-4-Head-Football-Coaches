# R/02_missing.R
source("R/00_setup.R")
reg_df <- readRDS(path_reg_df)
miss <- data.frame(variable=names(reg_df), n_missing=colSums(is.na(reg_df)))
miss <- miss[miss$n_missing > 0, ]
save_table(miss, "missingness_counts")
reg_df$AllPriorWinPct[is.na(reg_df$AllPriorWinPct) & reg_df$PrevHCAnyLevel==0] <- 0.50
reg_df$PreviousWinPct[is.na(reg_df$PreviousWinPct) & reg_df$PrevHCAnyLevel==0] <- 0.50
reg_df$sp_prev_rel[is.na(reg_df$sp_prev_rel)] <- mean(reg_df$sp_prev_rel, na.rm=TRUE)
still_na <- is.na(reg_df$AllPriorWinPct) & reg_df$PrevHCAnyLevel==1
if (any(still_na)) {
  log_df <- reg_df[still_na, c("name","school","first_season",
                                "PrevHCAnyLevel","PreviousWinPct","AllPriorWinPct")]
  save_table(log_df, "allpriorwinpct_imputation_log")
  reg_df$AllPriorWinPct[still_na] <- reg_df$PreviousWinPct[still_na]
}
saveRDS(reg_df, path_reg_df)
cat("02_missing.R done\n")
