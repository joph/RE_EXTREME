# This script compares wind generation from COPA and time series of wind resources from Renewables Ninja (wind_br.feather)

#### WIND COPA BRAZIL FROM WIND_BR.FEATHER ####
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA/data/wind")
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA/data/wind")
wind_br_feather <- read_feather("wind_br.feather")
dates_wind <- as_tibble(year(wind_br_feather$Date))
wind_br_2012 <- wind_br_feather %>% mutate(year = dates_wind$value) %>% filter(year == 2012)

ggplot(wind_br_2012, aes(x = Date, y = capFact, col = R)) +
  geom_line(size = 1) + facet_wrap(~R)

wind_br_2012 <- wind_br_2012 %>% select(Date, R, capFact) %>% mutate(type = "input")

# adjusting regions names
namesOrigShype<-c("SE1","SE2","SE3","SE4")
namesNewShype<-c("SE/CO","SUL","NE","N")
for(i in 1:length(namesOrigShype)){
  wind_br_2012$R[wind_br_2012$R == namesOrigShype[i]] <- namesNewShype[i]
}

#### Load the results on load_data_write_gdx.R ####
# X_RENEW COPA BRAZIL VALIDATION_2012
x_wind <- results %>% filter(name == "x_renew") %>% filter(iTechnology == "Wind") %>%  select(datetime, reg, value) %>% mutate(type = "copa")

# adjusting colnames
colnames(x_wind) <- colnames(wind_br_2012)

# tranforming capFacts in electricity
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA/data/investOptions")
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA/data/investOptions")
power_wind <- read_csv2("br_intermittent_opts_2012_1.csv") %>% filter(Variable == "MaxCap") %>% filter(iTechnology == "Wind")
power_wind$Value <- parse_number(power_wind$Value)

wind_br_2012 <-wind_br_2012 %>%  mutate(gen = c(rep(0,nrow(wind_br_2012))))
for (i in 1:length(namesOrigShype)){
  wind_br_2012$gen[wind_br_2012$R == namesNewShype[i]] <- 
    power_wind$Value[power_wind$Region == namesOrigShype[i]] * wind_br_2012$capFact[wind_br_2012$R == namesNewShype[i]]
}
wind_br_2012

wind_br_2012 <- wind_br_2012 %>% select(Date,R,gen, type)
colnames(x_wind) <- colnames(wind_br_2012)

# adjusting dates TO DO!!!
# plotting the time series
wind_comp <- bind_rows(wind_br_2012, x_wind)
ggplot() +
  geom_line(data = wind_comp , aes(x = Date, y = gen, col = type)) + facet_wrap(~R)

# We see wind power generation following the available resources.