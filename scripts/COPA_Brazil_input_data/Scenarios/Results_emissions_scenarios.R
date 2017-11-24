# This script access the emissions of thermal production
# For this, we use the thermalGenerationJoined from Results_thermal_sources_scenarios.R that presents the hourly
# thermal generation by source.

source("C:/Users/cancella/Google Drive/!IIASACOPA/RE_EXTREME/scripts/COPA_Brazil_input_data/Scenarios/Results_generation_balance_scenarios.R")
#source("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA/RE_EXTREME/scripts/COPA_Brazil_input_data/Scenarios/Results_generation_balance_scenarios.R")
# activity level from thermal plants: x_term_total

# Accessing the CO2equivalent
# emission factors from III brazilian inventary kg/TJ
# metodological annex from III inventary table A5 (p.19)
# imporing it from a excel file
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/Emissions")
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA  Initial Data/Emissions")
emission_factors_gwp <- read_delim("emission_factors_br.csv", delim = ";", locale = locale(decimal_mark = ".")) %>% 
  select(1:4)
emission_factors_gwp

gwp <- emission_factors_gwp %>% filter(fuel == "GWP")

emission_factors <- emission_factors_gwp[1:9,] %>% gather(gases, value, - fuel)

emissions <- full_join(emission_factors, x_term_total, by = c("fuel" = "Technology"))
colnames(emissions) <- c("fuel", "gases", "emission factors(kg/TJ)", "activity level(MWh)")

# units: emission factors from kg/TJ to t/MWh
emissions$`emission factors(kg/TJ)` <- emissions$`emission factors(kg/TJ)` * (1 / 277.78) * (1e-3)
emissions
colnames(emissions) <- c("fuel", "gases", "emission factors(t/MWh)", "activity level(MWh)")

# processing GWP variable to mutate in emissions table
gwp <- gwp %>% gather(gases) %>% filter(gases != "fuel")
gwp$value <- parse_double(gwp$value)

#adding gwp to emissions table
emissions <- full_join(emissions, gwp, by = c("gases" = "gases"))
colnames(emissions) <- c("fuel", "gases", "emission factors_t/MWh", "activity level_MWh", "gwp")

emissions <- emissions %>% mutate(tCO2eq = `emission factors_t/MWh` * `activity level_MWh` * gwp)
emissions <- emissions %>% mutate(MtCO2eq = tCO2eq / 1e6)

emissions_per_fuel <- emissions %>% group_by(fuel) %>% summarise(em_per_fuel = sum(MtCO2eq)) # MtCO2eq

as_tibble(emissions_per_fuel)

# plotting total emissions per fuel
emissions_wo_zeros <- emissions_per_fuel %>% filter(em_per_fuel != 0)

emission_plot <- ggplot(emissions_wo_zeros, aes(fuel, em_per_fuel, fill = fuel)) + geom_bar(stat = "identity") +
  scale_fill_brewer(palette="Set2") +
  ylab("MTCO2eq") + xlab("") +  ggtitle("Emission per fuel") + 
  theme(plot.title = element_text(hjust = 0.5))
plot(emission_plot)

# saving plot
# setwd("C:/Users/cancella/Google Drive/!IIASA/COPA/results/figures")
# setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA/results/figures")
# png("emission_per_fuel.png")
# emission_plot
# dev.off()

#saving tables: emissions and emissions_per_fuel
# setwd("C:/Users/cancella/Google Drive/!IIASA/COPA/results")
# setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA/results/figures")
# write.xlsx(emissions, "emissions.xlsx")
# write.xlsx(emissions_per_fuel, "emissions_per_fuel.xlsx") # MtCO2eq