# This script creates generation balance (electrical supply mix) for a year run, similar to figure 1 of Gils, et al, 2017

# getting the variables ---------------------------------------------------
source("C:/Users/cancella/Google Drive/!IIASA/COPA/RE_EXTREME/scripts/COPA_Brazil_input_data/Scenarios/Results_thermal_sources_scenarios.R")
#source("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA/RE_EXTREME/scripts/COPA_Brazil_input_data/Scenarios/Results_thermal_sources_scenarios.R")

# thermal generation
x_term_total <- thermalGenerationJoined %>% group_by(Technology) %>% summarise(value=sum(value))

#adjusting thermal technologies names
thermal_names <- c(unique(x_term_total$Technology))
short_names <- c("Biomass", "Coal","Diesel","Gas","GNL","Nuclear","Oil","Waste","Virtual")

for (i in c(1:nrow(x_term_total))){
  x_term_total$Technology[x_term_total$Technology == thermal_names[i]] <- short_names[i]
}
print(x_term_total)

# getting hydro generation
x_hydro_tot <- x_hydro_tot %>% group_by(name) %>% summarise(value = sum(value))

# getting intermittent variables
x_wind <- results %>% filter(name == "x_renew" & iTechnology == "Wind") %>% group_by(iTechnology) %>% 
  summarise(value = sum(value))

x_PV <- results %>% filter(name == "x_renew" & iTechnology == "PV") %>% group_by(iTechnology) %>% 
  summarise(value = sum(value))

# putting all technologies together (values in MWh)
total_supply <- bind_rows(x_term_total, x_hydro_tot, x_wind, x_PV)
total_supply$Technology[total_supply$name == "x_hydro_tot"] <- "Hydro"
total_supply$Technology[total_supply$iTechnology == "Wind"] <- "Wind"
total_supply$Technology[total_supply$iTechnology == "PV"]   <- "Solar PV"

total_supply_100 <- sum(total_supply$value)

# accessing the percentages
total_supply <- total_supply %>% mutate(share = total_supply$value / total_supply_100 * 100, year = c(rep(2025, nrow(total_supply)))) %>% 
  select(Technology, share, year)

# Sintex report epe 2014 (base year 2013)
epe_2013 <- tibble(
  Technology = c("Biomass","Coal","Gas","Hydro","Nuclear","Oil","Wind"),
  share = c(7.6, 2.6, 11.3, 70.6, 2.4, 4.4,1.1),
  year = c(rep(2013, 7))
)
  epe_2013

# plotting
comparation <- bind_rows(epe_2013, total_supply)
  
ggplot(comparation,aes(x = as.character(year), y = share, fill = Technology)) + geom_bar(stat = "identity", position = "stack") +
  scale_fill_brewer(palette="Set3") +
  ylab("Supply share (%)") + xlab("") +  ggtitle("Electrical supply mix") + 
  theme(plot.title = element_text(hjust = 0.5))
