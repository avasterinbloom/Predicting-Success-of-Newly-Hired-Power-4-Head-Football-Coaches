
# R/00_setup.R -- run at the top of every analysis script
# --- Libraries ---
library(tidyverse)
library(MASS)
library(car)
library(lmtest)
library(leaps)
library(glmnet)
library(nlme)
library(survival)
library(boot)
library(bootstrap)
library(ResourceSelection)
# --- Reproducibility ---
set.seed(2026)
# --- Paths ---
path_raw <- "data/raw/data.csv"
path_reg_df <- "data/processed/reg_df.rds"
path_full_df <- "data/processed/full_df.rds"
path_figures <- "figures/"
path_tables <- "tables/"
# --- Global options ---
options(stringsAsFactors = FALSE)
theme_set(theme_minimal(base_size = 12))
# --- Helper: save a figure with consistent naming ---
save_fig <- function(plot_obj, name, width = 7, height = 5) {
  ggsave(paste0(path_figures, name, ".png"),
         plot = plot_obj, width = width, height = height, dpi = 300)
}
# --- Helper: save a table ---
save_table <- function(df, name) {
  write.csv(df, paste0(path_tables, name, ".csv"), row.names = FALSE)
}