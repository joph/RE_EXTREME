# This script creates the table with the statistical indicators of validation_2012 run. 

#setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/ONS validation")
setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA  Initial Data/ONS validation")

se <- read_feather("stats_summary_SE.CO.feather")
s  <- read_feather("stats_summary_SUL.feather")
ne <- read_feather("stats_summary_NE.feather")
n  <- read_feather("stats_summary_N.feather")

statistic_ind <- bind_rows(se,s,ne,n) %>% mutate(reg = c(rep("SE", nrow(se)),
                                                         rep("S", nrow(s)),
                                                         rep("ne", nrow(ne)),
                                                         rep("n", nrow(n)))) %>% select(reg, statistic, correlation, RMSE)

colnames(statistic_ind) <- c("Region", "Technology", "Correlation", "RMSE")
statistic_ind

#setwd("C:/Users/cancella/Google Drive/!IIASA/COPA/runs/Validation_2012/tables")
setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA/runs/Validation_2012/tables")
write_feather(statistic_ind, "static_indicators_transmission_lines_costs_0000135.feather")
##### Comparing with solo scripts ####
se_solo <- read_feather("stats_summary_se_solo.feather")
s_solo  <- read_feather("stats_summary_s_solo.feather")
ne_solo <- read_feather("stats_summary_ne_solo.feather")
n_solo  <- read_feather("stats_summary_n_solo.feather")
statistic_ind_solo <- bind_rows(se_solo,s_solo,ne_solo,n_solo) %>% mutate(reg = c(rep("SE", nrow(se)),
                                                                     rep("S", nrow(s)),
                                                                     rep("ne", nrow(ne)),
                                                                     rep("n", nrow(n)))) %>% select(reg, statistic, correlation, RMSE)
#### Comparing indicators with other runs ####
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA/runs/Validation_2012/tables")
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA/runs/Validation_2012/tables")
# ind_hydro_bonds <- read_feather("static_indicators_summary_hydro_bounds.feather")
# ind_limit_hydro_71 <- read_feather("static_indicators_summary_limit_hydro_71.feather")
# ind_limit_hydro_65 <- read_feather("static_indicators_summary_limit_hydro_65.feather")
# ind_limit_hydro_60 <- read_feather("static_indicators_summary_limit_hydro_60.feather")
# ind_limit_hydro_55 <- read_feather("static_indicators_summary_limit_hydro_55.feather")
# ind_limit_hydro_50 <- read_feather("static_indicators_summary_limit_hydro_50.feather")

