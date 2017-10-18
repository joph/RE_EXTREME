# This script compares the inflows from "br_shype_hydro.feather" file and of hydro generation from COPA results. 
# It also compares the wind resources of "wind_br.feather" to wind production in COPA. 

setwd("C:/Users/cancella/Google Drive/!IIASA/COPA/data/hydro")
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA/data/hydro")

#### INFLOWS COPA BRAZIL SHYPE_HYDRO_BR ####
#shype_br <- read_feather("br_shype_hydro.feather")
shype_br <- read_feather("br_shype_hydro_2012.feather")
dates_shype <- as_tibble(lubridate::year(shype_br$date))
shype_br_2012 <- shype_br %>% mutate(year = dates_shype$value) %>% filter(year == 2012)
ggplot(shype_br_2012, aes(x = date, y = mwh/1e3, col = region)) +
  geom_line(size = 1) + facet_wrap(~region)

shype_br_2012 <- shype_br_2012 %>% select(date, region, mwh) %>% mutate(type = "input")

# adjusting regions names
namesOrigShype<-c("SE1","SE2","SE3","SE4")
namesNewShype<-c("SE.CO","SUL","NE","N")
for(i in 1:length(namesOrigShype)){
  shype_br_2012$region[shype_br_2012$region == namesOrigShype[i]] <- namesNewShype[i]
  }

#### Load the results on load_data_write_gdx.R ####
# X_HYDRO COPA BRAZIL VALIDATION_2012
x_hydro <- results %>% filter(name == "x_hydro") %>% select(datetime, reg, value) %>% mutate(type = "copa")

# adjusting colnames
colnames(x_hydro) <- colnames(shype_br_2012)

# adjusting dates TO DO!!!
# plotting the time series
hydro_comp <- bind_rows(shype_br_2012, x_hydro)
ggplot() +
  geom_line(data = hydro_comp , aes(x = date, y = mwh/1e3, col = type),size = 1) + facet_wrap(~region)
# We see hydro generation following the available resources.

#### Comparing inflows and ONS observed hydro generation ####
# reading files with hydro generation from ONS
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/ONS validation")
# setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA  Initial Data/ONS validation")
namesNew <- c("SE.CO","SUL","NE","N")
for (i in c(1:length(namesNew))){
  assign(str_c("gen_ons_final", namesNew[i], sep="_"),
         read_csv(str_c("geracao_ONS_",namesNew[i],".csv", sep="")))
}

gen_ons_hydro_tot <- bind_rows(gen_ons_final_SE.CO,gen_ons_final_SUL, gen_ons_final_NE, gen_ons_final_N) 
gen_ons_hydro_tot <- gen_ons_hydro_tot %>% filter(iTechnology == "hydro_ons")
gen_ons_hydro_tot <- gen_ons_hydro_tot %>% 
  mutate(region = c(rep(namesNew[1],nrow(gen_ons_hydro_tot)/4),
                    rep(namesNew[2],nrow(gen_ons_hydro_tot)/4),
                    rep(namesNew[3],nrow(gen_ons_hydro_tot)/4),
                    rep(namesNew[4],nrow(gen_ons_hydro_tot)/4))) %>%  
  mutate(type = c(rep("ONS", nrow(gen_ons_hydro_tot)))) %>% select(Date,region, production, type) %>% arrange(region)

# Passing input data for daily basis
shype_br_2012_daily <- shype_br_2012 %>% group_by(lubridate::day(date), lubridate::month(date), lubridate::year(date), region) %>% 
  summarise(production = sum(mwh)) %>% arrange(region) %>% ungroup()
shype_br_2012_daily$Date <- gen_ons_hydro_tot$Date 
shype_br_2012_daily <- shype_br_2012_daily %>% select(Date, region, production) %>% 
  mutate(production = production/1e3) %>% mutate(type = c(rep("input", nrow(shype_br_2012_daily))))

# Creating table to use in plot
comp_shype_ons <- bind_rows(gen_ons_hydro_tot,shype_br_2012_daily)

comp_input_copa <- ggplot(data = comp_shype_ons) + geom_line(aes(x = Date, y = production, col = type), size = 1) +
    facet_wrap(~region) + ggtitle("Comparation hydro inflows X ONS hydro generation" ) + 
    theme(axis.text.x = element_text(angle=45)) + ylab("GWh") + xlab("") +
    theme(plot.title = element_text(hjust = 0.5)) 
plot(comp_input_copa) # it seems something wrong here.

#### comparing directly from Johannes file - daily basis ####
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/Hydro")
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA  Initial Data/Hydro")
br_profile <- read_delim("daily_inflows_4regions_1979_2014.csv", delim = ";", locale = locale(decimal_mark = ".")) %>% 
  select(-X6, -X7)
br_profile <- br_profile[1:13149,]

br_profile<-br_profile %>% gather(Region,mwh,-Date)
br_profile_2012 <- br_profile %>% filter(lubridate::year(Date) == 2012)
br_profile_2012 <- br_profile_2012 %>% mutate(production = mwh/1e3) %>% select(Date, Region, production)
br_profile_2012$type <- c(rep("input", nrow(br_profile_2012)))
colnames(br_profile_2012) <- colnames(gen_ons_hydro_tot)

namesOrigProfile<-c("SE","S","NE","N")
namesNewProfile<-c("SE.CO","SUL","NE","N")
for(i in 1:length(namesOrigProfile)){
  br_profile_2012$region[br_profile_2012$region == namesOrigProfile[i]] <- namesNewProfile[i]
}

comp_profile_ons <- bind_rows(gen_ons_hydro_tot,br_profile_2012)
plot_comp_profile_ons <- ggplot(data = comp_profile_ons) + geom_line(aes(x = Date, y = production, col = type), size = 1) +
  facet_wrap(~region) + ggtitle("hydro inflows X ONS hydro generation" ) + 
  theme(axis.text.x = element_text(angle=45)) + ylab("GWh") + xlab("") +
  theme(plot.title = element_text(hjust = 0.5)) 
plot(plot_comp_profile_ons) # send to Johannes.

# saving in pdf #
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/ONS validation")
pdf("comparation_ONS_inflows_hydro_generation.pdf")
plot_comp_profile_ons
dev.off()


#### comparing values from Johannes file and input data from COPA ####
br_profile_2012_comp <- br_profile_2012 %>% mutate(hourly_mwh = production*1e3/24)

