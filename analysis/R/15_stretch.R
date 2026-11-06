# R/15_stretch.R -- optional sensitivity
source("R/00_setup.R")
source("lib/00_model_defs.R")
reg_df <- prep_reg_df()
print(wilcox.test(sp_rel_team ~ PreviousHC, data = reg_df))
print(kruskal.test(sp_rel_team ~ SideOfBall, data = reg_df))
cat("15_stretch.R done\n")
