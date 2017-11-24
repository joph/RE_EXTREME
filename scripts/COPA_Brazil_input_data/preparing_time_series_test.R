# This script prepares time-series to a 16 years run, repeting data.
# The objective of this run is test the computacional effort. 

# inflows time-series
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA/data/hydro")
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA/data/hydro")
shype_hydro_br <- read_feather("br_shype_hydro.feather")

shype_hydro_se <- shype_hydro_br %>% filter(region == "SE1")
shype_hydro_sul <- shype_hydro_br %>% filter(region == "SE2")
shype_hydro_ne <- shype_hydro_br %>% filter(region == "SE3")
shype_hydro_n <- shype_hydro_br %>% filter(region == "SE4")

#selecting dates (from 1999 up to 2014)
dates_se <- shype_hydro_se %>% filter(date >= "1999-01-01 00:00:00" & date <= " 2015-01-01 00:00:00")
dates_sul <- shype_hydro_sul %>% filter(date >= "1999-01-01 00:00:00" & date <= " 2015-01-01 00:00:00")
dates_ne <- shype_hydro_ne %>% filter(date >= "1999-01-01 00:00:00" & date <= " 2015-01-01 00:00:00")
dates_n <- shype_hydro_n %>% filter(date >= "1999-01-01 00:00:00" & date <= " 2015-01-01 00:00:00")

# concatenating regions with selected dates
shype_hydro_test_16_years <- bind_rows(dates_n, dates_ne, dates_sul, dates_se)
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA/data/hydro")
#setwd("C:/Users/Rafael/Desktop/Google Drive@PPE/!IIASA/COPA/data/hydro")
write_feather(shype_hydro_test_16_years,"test_shype_hydro_16years.feather")

# wind --------------------------------------------------------------------
# wind and solar: I have 4 years of observations. I repeated it 4 times to have 16. 
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA/data/wind")
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA/data/wind")
wind_br <- read_feather("wind_br.feather")

#selecting regions
wind_se <- wind_br %>% filter(R == "SE1")
wind_s <- wind_br %>% filter(R == "SE2")
wind_ne <- wind_br %>% filter(R == "SE3")
wind_n <- wind_br %>% filter(R == "SE4")

# generating 16 years - repeting dates_se, since all dates_xx are the same
wind_se_16 <- bind_rows(wind_se,wind_se,wind_se,wind_se)
wind_se_16$Date <- dates_se$date[2:nrow(dates_se)]
wind_s_16 <- bind_rows(wind_s,wind_s,wind_s,wind_s)
wind_s_16$Date <- dates_se$date[2:nrow(dates_se)]
wind_ne_16 <- bind_rows(wind_ne,wind_ne,wind_ne,wind_ne)
wind_ne_16$Date <- dates_se$date[2:nrow(dates_se)]
wind_n_16 <- bind_rows(wind_n,wind_n,wind_n,wind_n)
wind_n_16$Date <- dates_se$date[2:nrow(dates_se)]

# concatenating regions
wind_test_16_years <- bind_rows(wind_ne_16,wind_n_16,wind_se_16,wind_s_16)
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA/data/wind")
#setwd("C:/Users/Rafael/Desktop/Google Drive@PPE/!IIASA/COPA/data/wind")
write_feather(wind_test_16_years,"test_wind_16years.feather")

# solar -------------------------------------------------------------------
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA/data/solar")
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA/data/solar")
solar_br <- read_feather("solar_GAMS_br.feather")

#selecting regions
solar_se <- solar_br %>% filter(R == "SE1")
solar_s <- solar_br %>% filter(R == "SE2")
solar_ne <- solar_br %>% filter(R == "SE3")
solar_n <- solar_br %>% filter(R == "SE4")

# generating 16 years - repeting dates_se, since all dates_xx are the same
solar_se_16 <- bind_rows(wind_se,wind_se,wind_se,wind_se)
solar_se_16$Date <- dates_se$date[2:nrow(dates_se)]
solar_s_16 <- bind_rows(wind_s,wind_s,wind_s,wind_s)
solar_s_16$Date <- dates_se$date[2:nrow(dates_se)]
solar_ne_16 <- bind_rows(wind_ne,wind_ne,wind_ne,wind_ne)
solar_ne_16$Date <- dates_se$date[2:nrow(dates_se)]
solar_n_16 <- bind_rows(wind_n,wind_n,wind_n,wind_n)
solar_n_16$Date <- dates_se$date[2:nrow(dates_se)]

# concatenating regions
solar_test_16_years <- bind_rows(solar_ne_16,solar_n_16,solar_se_16,solar_s_16)
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA/data/solar")
#setwd("C:/Users/Rafael/Desktop/Google Drive@PPE/!IIASA/COPA/data/solar")
write_feather(solar_test_16_years,"test_solar_16years.feather")

# load --------------------------------------------------------------------
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA/data/load")
#setwd("C:/Users/Rafael/Desktop/Google Drive@PPE/!IIASA/COPA/data/load")
load_br <- read_feather("load_Br_2014.feather")

# selecting regions
load_se <- load_br %>% filter(Region == "SE1")
load_s <- load_br %>% filter(Region == "SE2")
load_ne <- load_br %>% filter(Region == "SE3")
load_n <- load_br %>% filter(Region == "SE4")

# generating 16 years - repeting dates_se, since all dates_xx are the same
load_se_16 <- bind_rows(load_se, load_se,load_se,load_se,load_se,load_se,load_se,load_se)
load_se_16$Date <- dates_se$date[1:nrow(load_se_16)]
load_se_16 <- load_se_16 %>% filter(!is.na(Date))

load_s_16 <- bind_rows(load_s, load_s,load_s,load_s,load_s,load_s,load_s,load_s)
load_s_16$Date <- dates_se$date[1:nrow(load_s_16)]
load_s_16 <- load_s_16 %>% filter(!is.na(Date))

load_ne_16 <- bind_rows(load_ne, load_ne,load_ne,load_ne,load_ne,load_ne,load_ne,load_ne)
load_ne_16$Date <- dates_se$date[1:nrow(load_ne_16)]
load_ne_16 <- load_ne_16 %>% filter(!is.na(Date))

load_n_16 <- bind_rows(load_n, load_n,load_n,load_n,load_n,load_n,load_n,load_n)
load_n_16$Date <- dates_se$date[1:nrow(load_n_16)]
load_n_16 <- load_n_16 %>% filter(!is.na(Date))

# concatenating regions
load_test_16_years <- bind_rows(load_ne_16,load_n_16,load_se_16,load_s_16)
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA/data/load")
#setwd("C:/Users/Rafael/Desktop/Google Drive@PPE/!IIASA/COPA/data/load")
write_feather(load_test_16_years,"test_load_16years.feather")
