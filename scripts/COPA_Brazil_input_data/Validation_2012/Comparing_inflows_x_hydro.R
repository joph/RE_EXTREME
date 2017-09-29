#setwd("C:/Users/cancella/Google Drive/!IIASA/COPA/data/hydro")
setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA/data/hydro")

#### INFLOWS COPA BRAZIL SHYPE_HYDRO_BR ####
shype_br <- read_feather("br_shype_hydro.feather")
dates_shype <- as_tibble(year(shype_br$date))
shype_br_2012 <- shype_br %>% mutate(year = dates_shype$value) %>% filter(year == 2012)
ggplot(shype_br_2012, aes(x = date, y = mwh/1e3, col = region)) +
  geom_line(size = 1) + facet_wrap(~region)

shype_br_2012 <- shype_br_2012 %>% select(date, region, mwh) %>% mutate(type = "input")


#### Load the results on load_data_write_gdx.R ####
# X_HYDRO COPA BRAZIL VALIDATION_2012
x_hydro <- results %>% filter(name == "x_hydro") %>% select(datetime, reg, value) %>% mutate(type = "copa")

# adjusting regions
x_hydro$reg[x_hydro$reg == "SE001"] <- "SE1"
x_hydro$reg[x_hydro$reg == "SE002"] <- "SE2"
x_hydro$reg[x_hydro$reg == "SE003"] <- "SE3"
x_hydro$reg[x_hydro$reg == "SE004"] <- "SE4"

# adjusting colnames
colnames(x_hydro) <- colnames(shype_br_2012)

# adjusting dates TO DO!!!

# plotting the time series
hydro_comp <- bind_rows(shype_br_2012, x_hydro)
ggplot() +
  geom_line(data = hydro_comp , aes(x = date, y = mwh/1e3, col = type),size = 1) + facet_wrap(~region)

#### WIND COPA BRAZIL FROM WIND_BR.FEATHER ####
#setwd("C:/Users/cancella/Google Drive/!IIASA/COPA/data/wind")
setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA/data/wind")
wind_br_feather <- read_feather("wind_br.feather")
dates_wind <- as_tibble(year(wind_br_feather$Date))
wind_br_2012 <- wind_br_feather %>% mutate(year = dates_wind$value) %>% filter(year == 2012)

ggplot(wind_br_2012, aes(x = Date, y = capFact, col = R)) +
  geom_line(size = 1) + facet_wrap(~R)

wind_br_2012 <- wind_br_2012 %>% select(Date, R, capFact) %>% mutate(type = "input")


#### Load the results on load_data_write_gdx.R ####
# X_RENEW COPA BRAZIL VALIDATION_2012
x_wind <- results %>% filter(name == "x_renew") %>% filter(iTechnology == "Wind") %>%  select(datetime, reg, value) %>% mutate(type = "copa")

# adjusting regions
x_wind$reg[x_wind$reg == "SE001"] <- "SE1"
x_wind$reg[x_wind$reg == "SE002"] <- "SE2"
x_wind$reg[x_wind$reg == "SE003"] <- "SE3"
x_wind$reg[x_wind$reg == "SE004"] <- "SE4"
# adjusting colnames
colnames(x_wind) <- colnames(wind_br_2012)

# tranforming capFacts in electricity
#setwd("C:/Users/cancella/Google Drive/!IIASA/COPA/data/investOptions")
setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA/data/investOptions")
power_wind <- read_csv2("br_intermittent_opts_2012_1.csv") %>% filter(Variable == "MaxCap") %>% filter(iTechnology == "Wind")
power_wind$Value <- parse_number(power_wind$Value)

wind_br_2012 <-wind_br_2012 %>%  mutate(gen = c(rep(0,nrow(wind_br_2012))))
wind_br_2012$gen[wind_br_2012$R == "SE1"] <- power_wind$Value[power_wind$Region == "SE1"] * wind_br_2012$capFact[wind_br_2012$R == "SE1"]
wind_br_2012$gen[wind_br_2012$R == "SE2"] <- power_wind$Value[power_wind$Region == "SE2"] * wind_br_2012$capFact[wind_br_2012$R == "SE2"]
wind_br_2012$gen[wind_br_2012$R == "SE3"] <- power_wind$Value[power_wind$Region == "SE3"] * wind_br_2012$capFact[wind_br_2012$R == "SE3"]
wind_br_2012$gen[wind_br_2012$R == "SE4"] <- power_wind$Value[power_wind$Region == "SE4"] * wind_br_2012$capFact[wind_br_2012$R == "SE4"]

wind_br_2012 <- wind_br_2012 %>% select(Date,R,gen, type)
colnames(x_wind) <- colnames(wind_br_2012)

# adjusting dates TO DO!!!

# plotting the time series
wind_comp <- bind_rows(wind_br_2012, x_wind)
ggplot() +
  geom_line(data = wind_comp , aes(x = Date, y = gen, col = type)) + facet_wrap(~R)

# We see hydro and wind power generation following the available resources.