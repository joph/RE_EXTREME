# 10/11/2017 (m/d/y)
# This script accesses the calibration factor between inflows from Johannes file and real inflows for 2012. 
# Calibration factor is given by: sum(inflows12) / sum (inflows15)
# We multiply this factor by inflows of Johannes file. 

# reading Johannes inflows file (considering power plants from 2015)
#setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/Hydro")
setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA  Initial Data/Hydro")
br_profile15 <- read_delim("daily_inflows_4regions_1979_2014.csv", delim = ";", locale = locale(decimal_mark = ".")) %>%
  select(-X6, -X7) %>% filter(!is.na(Date)) %>% gather(Region,mwh,-Date) %>% filter(lubridate::year(Date) == 2012) %>% 
  arrange(Region)

br_profile15 <-  br_profile15 %>% arrange(Region)

ggplot(br_profile15, aes(x = Date, y = mwh/1e3, col = Region)) +
  geom_line(size = 1) + facet_wrap(~Region)

# reading ONS website 2012 storable ENAs (MWmedio)
#setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/ONS validation")
 setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA  Initial Data/ONS validation")
br_profile12 <- read_csv("Comparativo_Energia_Natural_Afluente_Subsistema_Dia_2012.csv") %>% select(1,2,8) 
br_profile12$`Data Escala de Tempo 1 ENAS Comp 3` <- br_profile15$Date

# changing region names
oldNames <- c("Sul","Sudeste/Centro-Oeste", "Norte", "Nordeste")
newNames <- c("S", "SE", "N", "NE")
for (i in c(1:length(oldNames))){
  br_profile12$Subsistema[br_profile12$Subsistema == oldNames[i]] <- newNames[i]
}

colnames(br_profile12) <- colnames(br_profile15)
br_profile12 <- br_profile12 %>% arrange(Region)

# passing values from MWmed to MWh
br_profile12$mwh <- br_profile12$mwh * 24

# Calculating the adaptation factor: fc(reg) = sum(reg12) / sum(reg15) 
profiles <- full_join(br_profile12, br_profile15, by = c("Date", "Region"))
adaptFactor <- profiles %>% group_by(Region) %>% summarise(adaptFact = sum(mwh.x) / sum(mwh.y))

# saving adaptation factors in .csv file
#setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/Hydro")
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA  Initial Data/Hydro")
#write_csv(adaptFactor, "adapFactors.csv")

profiles <- profiles %>% mutate(inflows12 = c(rep(0, nrow(profiles))))
# multiplying COPA daily values by adaptation factor
for (i in c(1:length(newNames))){
  profiles$inflows12[profiles$Region == newNames[i]] <- 
    profiles$mwh.y[profiles$Region == newNames[i]] * adaptFactor$adaptFact[adaptFactor$Region == newNames[i]]
}

# checking that adaptation factors are correct
profiles %>% gather(var,val,-Date,-Region) %>% 
  ggplot(aes(Date,val, color = var)) + geom_line() + facet_wrap(~Region)