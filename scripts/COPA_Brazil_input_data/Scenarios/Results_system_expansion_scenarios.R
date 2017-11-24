#Results: total investment per source in each region

# System expansion --------------------------------------------------------
#### System expansion #### # TO DO: CHOOSE JUST INVESTMENT COSTS > 0 # continue from here tomorrow ;)
# loading thermal investment costs, energy sources and location of these plants
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA/data/investOptions")
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA/data/investOptions")
# Looking thermal: take the plants that have investment cost >0
plants_invest_cost_positive <- read_delim("investOpts_br_thermal.sources.csv", delim = ";") %>% 
  filter(Variable == "Investment" & Value > 0)

x_invest_thermal_cap <- results %>% filter(name == "x_invest_thermal_cap")

# Adjusting region names ---------------------------------------------------
regNames <- unique(x_invest_thermal_cap$reg)
oldNames <- unique(plants_invest_cost_positive$Region)

for(i in 1:length(regNames)){
  plants_invest_cost_positive$Region[plants_invest_cost_positive$Region == oldNames[i]] <- regNames[i]
}
print(plants_invest_cost_positive)

# Adjusting plant names ---------------------------------------------------
# plantNames <- unique(x_term$p) %>% str_split_fixed(pattern = "0",n = 2)
# print(plantNames[,2])

inputPlantNames <- str_split_fixed(plants_invest_cost_positive$P, pattern = "P", n = 2) 
inputPlantNames <- str_c("P", 0, inputPlantNames[,2])  
print(inputPlantNames)

zeros <- c("01", "02", "03", "04", "05", "06", "07","08", "09")
numbers <- c("001", "002", "003", "004", "005", "006", "007", "008", "009")
for (i in 1:length(zeros)){
  inputPlantNames[inputPlantNames == str_c("P",zeros[i])] = str_c("P", numbers[i])
}
print(inputPlantNames)

plants_invest_cost_positive$P <- inputPlantNames
print(plants_invest_cost_positive)

# Investment per thermal source in each region
thermal_plants_join <- full_join(plants_invest_cost_positive, x_invest_thermal_cap, 
                                 by = c("Region" = "reg", "P" = "p")) %>% 
  select(Region, P, Technology, value) %>% filter(!is.na(Technology))

thermal_names <- c("Thermal.Biomassa", "Thermal.Gas", "Thermal.Carvao")
short_names <- c("Biomass", "Gas", "Coal")

for (i in c(1:length(thermal_plants_join))){
  thermal_plants_join$Technology[thermal_plants_join$Technology == thermal_names[i]] <- short_names[i]
}
print(thermal_plants_join)

# Intermittent investment ------------------------------------------------------------
# loading thermal investment costs, energy sources and location of these plants
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA/data/investOptions")
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA/data/investOptions")
# Looking intermittent: take the plants that have investment cost >0
intermittent_invest_cost_positive <- read_delim("br_intermittent_opts.csv", delim = ";") %>% 
  filter(Variable == "Investment" & Value > 0)
x_invest_intermittent <- results %>% filter(name == "x_invest_intermittent")

# Adjusting region names ---------------------------------------------------
regNames <- unique(x_invest_intermittent$reg)
oldNames <- unique(intermittent_invest_cost_positive$Region)

for(i in 1:length(regNames)){
  intermittent_invest_cost_positive$Region[intermittent_invest_cost_positive$Region == oldNames[i]] <- regNames[i]
}
print(intermittent_invest_cost_positive)


# Adjusting plant names ---------------------------------------------------

intermittent_invest_cost_positive$P <- rep("P002",nrow(intermittent_invest_cost_positive))


# joining intermittent plants ---------------------------------------------

intermittent_plants_join <- full_join(intermittent_invest_cost_positive, x_invest_intermittent,  
                                      by = c("Region" = "reg", "P" = "p")) %>% 
  select(Region, P, iTechnology.x, value) %>% filter(!is.na(iTechnology.x))
print(intermittent_plants_join)

# dealing with the case that investment in intermittent plants are zero
for(i in c(1:nrow(intermittent_plants_join))){
  if (is.na(intermittent_plants_join$value[i]))
  {intermittent_plants_join$value[i] <- 0} 
  else {}
  } 
print(intermittent_plants_join)

# Plotting investment per source in each region ---------------------------
colnames(intermittent_plants_join) <- colnames(thermal_plants_join)
invest <- rbind(thermal_plants_join, intermittent_plants_join)

expansion <- invest %>% group_by(Region, Technology) %>% summarise(exp = sum(value))
sys_exp <- ggplot() + 
  geom_bar(data = expansion, aes(x = Technology, y = exp, fill = Technology),stat = "identity") +
  facet_wrap(~Region) +   scale_fill_brewer(palette="Set2") +
  ylab("MW") + xlab("") +  ggtitle("System expansion - capacity" ) + 
  theme(plot.title = element_text(hjust = 0.5))
plot(sys_exp)
# png("../../results/figures/system_expansion.png")
# sys_exp
# dev.off()
# #ggsave("../results/figures/system_expansion.pdf",sys_exp,width=30,height=20,units="cm")

# writing the investment table in a xlsx file
# setwd("C:/Users/cancella/Google Drive/!IIASA/COPA/results")
# #setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA/results")
# write.xlsx(as_data_frame(expansion), "system_expansion.xlsx")