# 17/11/03 (yy/mm/dd)
# This script compares total wind generation from COPA and ONS
source("C:/Users/cancella/Google Drive/!IIASA/COPA/RE_EXTREME/scripts/COPA_Brazil_input_data/Validation_2012/Understanding_hydro_generation.R")
# We load this script in order to obtain the ONS total wind generation

# selecting ONS wind generation
ONS_wind <- gen_ons_final_tot %>% filter(iTechnology == "wind_ons")
date <- ONS_wind$Date[1:366]
ONS_wind <- ONS_wind %>% group_by(yday(Date)) %>% summarise(production = sum(production)) %>% mutate(type = "ONS")
print(ONS_wind)
colnames(ONS_wind) <- c("Date", "production", "type")

# Selecting COPA results
COPA_wind <- x_wind <- results %>% filter(name == "x_renew") %>% filter(iTechnology == "Wind") %>%  
  select(datetime, reg, value) %>% group_by(yday(datetime)) %>% summarise(production = sum(value)/1e3) %>% mutate(type = "COPA")
print(COPA_wind)
colnames(COPA_wind) <- c("Date", "production", "type")

wind_total <- bind_rows(ONS_wind, COPA_wind)
wind_total$Date <- date

# Plotting 
wind_total_plot <- ggplot(wind_total)+ geom_line(aes(x = Date, y = production, col = type), size = 1)+
  scale_color_hue(l=45)+theme(axis.text.x = element_text(angle=45))+
  ylim(0,max(wind_total$production)) + ylab("GWh") + xlab("") +  ggtitle("Wind total generation") + 
  theme(plot.title = element_text(hjust = 0.5))
plot(wind_total_plot)

# Saving figure
#setwd("PC")
# setwd("C:/Users/cancella/Google Drive/!IIASA/COPA/runs/Validation_2012/figures")
# png("wind_total_generation.png")
# wind_total_plot
# dev.off()
