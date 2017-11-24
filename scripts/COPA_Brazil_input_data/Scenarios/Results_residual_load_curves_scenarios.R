# 2017/11/24 (yyyy/mm/dd)
# This script accessess the residual load curve

# getting variables from results
#load
load <- results %>% filter(name == "load")
#renewable intermitentt
renew_intermittent <- results %>% filter(name == "x_renew")
#hydro
x_hydro_tot
#term
source("C:/Users/cancella/Google Drive/!IIASA/COPA/RE_EXTREME/scripts/COPA_Brazil_input_data/Scenarios/Results_thermal_sources_scenarios.R")
thermalGenerationJoined

# joining tables
summary_table <- load %>% select(datetime,reg,value)

# full join datetime, reg # continue from here on Monday :)
summary_table <- summary_table %>% full_join(renew_intermittent, by = c("datetime" = "datetime"))

ggplot(load, aes(datetime,value))+ geom_line()

