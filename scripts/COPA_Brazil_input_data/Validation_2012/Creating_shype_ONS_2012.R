# This script creates inflows profile from ONS data for 2012

# Reading the file with Brazilian daily inflows
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/ONS validation")
# setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA  Initial Data/ONS validation")
br_profile12 <- read_csv("Comparativo_Energia_Natural_Afluente_Subsistema_Dia_2012.csv") %>% select(1,2,8) 
colnames(br_profile12) <- c("Date", "Region", "ENA(MWmed)")
br_profile12$mwh <- br_profile12$`ENA(MWmed)` * 24 # passing to mwh
#br_profile12$Date <- as.POSIXct(br_profile12$Date, tz = "UTC", format = "%m/%d/%Y %I:%M:S %p")
dates_day <- c("2012-01-01", "2012-12-31")
s1_day<-seq(as.POSIXct(dates_day[1],tz = "UTC"),as.POSIXct(dates_day[2],tz = "UTC"),by="d")
s1_day <- as_tibble(s1_day)
br_profile12$Date <- s1_day$value

newNames <- c("S", "SE", "N", "NE")
profileRegNames <- c("Sul", "Sudeste/Centro-Oeste", "Norte", "Nordeste")
for (i in c(1:length(profileRegNames))){
  br_profile12$Region[br_profile12$Region == profileRegNames[i]] <- 
    newNames[i]
}

# Creating date column
dates<-c("2012-01-01 00:00:00",
         "2012-12-31 23:00:00")
s1<-seq(as.POSIXct(dates[1],tz = "UTC"),
        as.POSIXct(dates[2],tz = "UTC"),
        by="h")
shype_hydro_ons <- as_tibble(s1)
colnames(shype_hydro_ons, do.NULL = TRUE, prefix = "col")
colnames(shype_hydro_ons) <- c("date")

# doing the time list for each region
shype_hydro_ons <- bind_rows(bind_cols(shype_hydro_ons,tibble(region=rep("N",nrow(shype_hydro_ons)))),
                      bind_cols(shype_hydro_ons,tibble(region=rep("NE",nrow(shype_hydro_ons)))),
                      bind_cols(shype_hydro_ons,tibble(region=rep("S",nrow(shype_hydro_ons)))),
                      bind_cols(shype_hydro_ons,tibble(region=rep("SE",nrow(shype_hydro_ons)))))


fd<-full_join(shype_hydro_ons,br_profile12,by=c("date"="Date","region"="Region"))

fd1<-fd %>% mutate(dd=format(date, format = "%Y-%m-%d")) %>% 
  group_by(region,dd) %>% mutate(mwh1=(max(mwh/24, na.rm=TRUE)),variable=as.character("shype_hydro")) %>% 
  ungroup() 

# Filling the br_hydro
shype_hydro_ons12 <- fd1 %>% select(date, variable,region, mwh=mwh1)
#colnames(br_shype_hydro) <- c("date", "variable", "region", "mwh")

shype_hydro_ons12$region <- c(rep(c("SE4"), nrow(shype_hydro_ons12) / 4),
                              rep(c("SE3"), nrow(shype_hydro_ons12) / 4),
                              rep(c("SE2"), nrow(shype_hydro_ons12) / 4),
                              rep(c("SE1"), nrow(shype_hydro_ons12) / 4))

# writing file(feather and csv) # FULL PERIOD - 1979 TO 2014
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA/data/hydro")
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA/data/hydro")
save_hydro_data_br <- write.table(shype_hydro_ons12, file = "shype_hydro_ons12.csv", sep =";", row.names = FALSE )
write_feather(shype_hydro_ons12, "shype_hydro_ons12.feather")

#### checking data for 2012 ####
shype_br_2012 <- br_shype_hydro %>% mutate(year = lubridate::year(date)) %>% filter(year == 2012)
ggplot(shype_hydro_ons12, aes(x = date, y = mwh/1e3, col = region)) +
  geom_line(size = 1) + facet_wrap(~region)

# multiplying by the calibration factors in order to obtain the sums equals to 2012 ONS data
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/Hydro")
#setwd("PC")
adaptFactor <- read_csv("adapFactors.csv")

shype_br_2012 <- shype_br_2012 %>% select(-year) %>% mutate(mwh12 = c(rep(0,nrow(shype_br_2012))))

newNames <- c("S", "SE", "N", "NE")
shypeRegNames <- c("SE2", "SE1", "SE4", "SE3")
for (i in c(1:length(shypeRegNames))){
  shype_br_2012$mwh12[shype_br_2012$region == shypeRegNames[i]] <- 
    shype_br_2012$mwh[shype_br_2012$region == shypeRegNames[i]] * adaptFactor$adaptFact[adaptFactor$Region == newNames[i]]
}

# checking sums (it is important to have checkONS defined and loaded in "Calibration_inflows.R")
check <- shype_br_2012 %>% group_by(region) %>% summarise(sumCopa = sum(mwh12)) # 10/12/17 sums OK!!

# adjusting shype_br_2012 to save in .csv
shype_br_2012 <- shype_br_2012 %>% select(-mwh)
colnames(shype_br_2012) <- c("date", "variable", "region", "mwh")


setwd("C:/Users/cancella/Google Drive/!IIASA/COPA/data/hydro")
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA/data/hydro")
write.table(shype_br_2012, file = "br_shype_hydro_2012.csv", sep =";", row.names = FALSE )
write_feather(br_shype_hydro, "br_shype_hydro_2012.feather")