# 11/10/17 (mm/dd/yy)
# This script makes the correspondence between thermal results (electricity geeneration) and thermal sources
# Results: thermal generation of each source in each hour. It is a importanr information to emissions
# graphs: weekly basis

# loading thermal costs and capacities input file (investOpts)
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA/data/investOptions")
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA/data/investOptions")
thermalSources <- read_delim("investOpts_br_thermal.sources.csv", delim = ";") %>% 
  select(Region,P, Technology) %>% 
  distinct(Region, P, Technology)

# COPA thermal generation -------------------------------------------------
x_term <- results %>% filter(name == "x_term") %>% select(reg, p, value, datetime) 

# Adjusting region names ---------------------------------------------------
regNames <- unique(x_term$reg)
oldNames <- unique(thermalSources$Region)

for(i in 1:length(regNames)){
  thermalSources$Region[thermalSources$Region == oldNames[i]] <- regNames[i]
}
print(thermalSources)

# Adjusting plant names ---------------------------------------------------
# plantNames <- unique(x_term$p) %>% str_split_fixed(pattern = "0",n = 2)
# print(plantNames[,2])
inputPlantNames <- str_split_fixed(thermalSources$P, pattern = "P", n = 2) 
inputPlantNames <- str_c("P", 0, inputPlantNames[,2])  
print(inputPlantNames)

zeros <- c("01", "02", "03", "04", "05", "06", "07","08", "09")
numbers <- c("001", "002", "003", "004", "005", "006", "007", "008", "009")
for (i in 1:length(zeros)){
  inputPlantNames[inputPlantNames == str_c("P",zeros[i])] = str_c("P", numbers[i])
}
print(inputPlantNames)

thermalSources$P <- inputPlantNames

# joing generation and sources --------------------------------------------

thermalGenerationJoined <- full_join(thermalSources, x_term, by = c("Region" = "reg", "P" = "p")) 

# plot thermal generation per region ---------------------------------
# It is taking too long to generate this graph # TO DO:AGGREGATE WEEKLY
thermalRegions <- thermalGenerationJoined %>% group_by(Region, Technology, w=(week(datetime)), y=year(datetime)) %>% 
  summarise(value = sum(value), dat = as.POSIXct(min(datetime)))
print(thermalRegions)

# renaming the technologies
thermal_names <- c(unique(thermalRegions$Technology))
short_names <- c("Coal","Diesel","Gas","Oil","Virtual", "Biomass","GNL","Nuclear","Waste")
for (i in c(1:nrow(thermalRegions))){
  thermalRegions$Technology[thermalRegions$Technology == thermal_names[i]] <- short_names[i]
}
print(thermalRegions)

regionsPlot <- ggplot(thermalRegions, aes(x = dat, y = value, fill = Technology))+ geom_area() + 
  facet_wrap(~Region) +scale_fill_hue(l=60)+ theme(axis.text.x = element_text(angle=45))+ ylab("MWh") + xlab("") + 
  ggtitle("Thermal generation by source and region") + theme(plot.title = element_text(hjust = 0.5))
plot(regionsPlot)

# saving graph
# setwd("C:/Users/cancella/Google Drive/!IIASA/COPA/results/figures")
# # setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA/results/figures")
# png("Regional thermal generation.png")
# regionsPlot
# dev.off()

# plot Brazil thermal generation by source --------------------------------
thermalBrazil <- thermalGenerationJoined %>% group_by(w = week(datetime),y = year(datetime) ,Technology) %>% 
  summarise(value = sum(value), dat = as.POSIXct(min(datetime))) 

#renaming the technologies
thermal_names <- c(unique(thermalBrazil$Technology))
short_names <- c("Biomass","Coal","Diesel","Gas", "GNL", "Nuclear","Oil","Waste", "Virtual")
for (i in c(1:nrow(thermalBrazil))){
  thermalBrazil$Technology[thermalBrazil$Technology == thermal_names[i]] <- short_names[i]
}
print(thermalBrazil)

thermalPlot <- ggplot(thermalBrazil, aes(x = dat, y = value, fill = Technology))+
  geom_area() + scale_fill_hue(l=60)+ ylab("MWh") + xlab("") + 
  ggtitle("Thermal generation by source") + theme(plot.title = element_text(hjust = 0.5))
plot(thermalPlot)
# saving graph
# setwd("C:/Users/cancella/Google Drive/!IIASA/COPA/results/figures")
# # setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA/results/figures")
# png("Brazilian thermal generation.png")
# thermalPlot
# dev.off()





