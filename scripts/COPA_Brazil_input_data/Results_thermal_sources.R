# 11/10/17 (mm/dd/yy)
# This script makes the correspondence between thermal results (electricity geeneration) and thermal sources

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
print(inputPlantNames)
inputPlantNames <- str_c("P", 0, inputPlantNames[,2])  
test <- inputPlantNames

inputPlantNames[inputPlantNames == "P01"] = "P001"
inputPlantNames[inputPlantNames == "P02"] = "P002"
inputPlantNames[inputPlantNames == "P03"] = "P003"
inputPlantNames[inputPlantNames == "P04"] = "P004"
inputPlantNames[inputPlantNames == "P05"] = "P005"
inputPlantNames[inputPlantNames == "P06"] = "P006"
inputPlantNames[inputPlantNames == "P07"] = "P007"
inputPlantNames[inputPlantNames == "P08"] = "P008"
inputPlantNames[inputPlantNames == "P09"] = "P009"
print(inputPlantNames) #### TO DO: A LOOP THAT WORKS!!!   

zeros <- c("01", "02", "03", "04", "05", "06", "07","08", "09")
numbers <- c("001", "002", "003", "004", "005", "006", "007", "008", "009")

thermalSources$P <- inputPlantNames


# joing generation and sources --------------------------------------------

thermalGenerationJoined <- full_join(thermalSources, x_term, by = c("Region" = "reg", "P" = "p")) 


# plot thermal generation per region ---------------------------------
# It is taking too long to generate this graph
ggplot(thermalGenerationJoined, aes(x = datetime, y = value, fill = Technology))+ geom_area() + 
  facet_wrap(~Region) +scale_colour_hue(l=45)+ theme(axis.text.x = element_text(angle=45))+ ylab("MWh") + xlab("") + 
  ggtitle("Thermal generation by source and region") + theme(plot.title = element_text(hjust = 0.5))

# plot Brazil thermal generation by source --------------------------------
themalBrazil <- thermalGenerationJoined %>% group_by(datetime, Technology) %>% summarise(value = sum(value))

themalPlot <- ggplot(themalBrazil, aes(x = datetime, y = value, fill = Technology))+
  geom_area() + scale_fill_hue(l=55)+ ylab("MWh") + xlab("") + 
  ggtitle("Thermal generation by source and region") + theme(plot.title = element_text(hjust = 0.5))
plot(themalPlot)

# saving graph
# setwd("C:/Users/cancella/Google Drive/!IIASA/COPA/runs/Baseline draft/figures")
# setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA/runs/Baseline draft/figures")
# png("Brazilian thermal generation.png")
# themalPlot
# dev.off()





