# This script creates the table with the statistical indicators of validation_2012 run. 

setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/ONS validation")
se <- read_feather("stats_summary_se.feather")
s  <- read_feather("stats_summary_s.feather")
ne <- read_feather("stats_summary_ne.feather")
n  <- read_feather("stats_summary_n.feather")

statistic_ind <- bind_rows(se,s,ne,n) %>% mutate(reg = c(rep("SE", nrow(se)),
                                                         rep("S", nrow(s)),
                                                         rep("ne", nrow(ne)),
                                                         rep("n", nrow(n)))) %>% select(reg, statistic, correlation, RMSE)



# continue from here monday :) 
# checking the regional tables with indicators. Probably I'saved something wrong. 
  # 1. check the regional scripts and save it again. 
  # 2. make multiplots with information of interest. 


