# This script creates the time series of capacity factor of wind resource for Brazilian version of COPA.
# Reading the swedish file
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA/data/wind")
# setwd(PC)
wind_se <- read_feather("wind_1979_2014.feather")

# Taking information from https://www.renewables.ninja/
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/Wind/Resource data")

# Dates: 4 years: 2012 to 2015
dates<-c("2012-01-01 00:00:00",
         "2015-12-31 23:00:00")
s1<-seq(as.POSIXct(dates[1],tz = "UTC"),
        as.POSIXct(dates[2],tz = "UTC"),
        by="h")

#### Northeast ####
# salvador: (lat,lon) = (-12.8761,-38.3643)
salvador <- as_tibble(read.csv2("ninja_wind_-12.8761_-38.3643_uncorrected_Salvador.csv", header = F, sep = ",")) %>% 
  select(V1, V3)
salvador <- salvador[4:nrow(salvador),]
colnames(salvador) <- c("Date", "capFact")
salvador$Date <- s1
salvador_initial <- salvador
salvador_initial[sapply(salvador_initial, is.factor)] <- lapply(salvador_initial[sapply(salvador_initial, is.factor)], 
                                                                function(x) as.numeric(as.character(x)))
salvador$capFact <- salvador_initial$capFact
ggplot(salvador[1:48,], aes(x = Date[1:48], y = capFact[1:48])) + geom_line()

# Petrolina: (lat,lon) = (-9.4165,-40.5066)
petrolina <- as_tibble(read.csv2("ninja_wind_-9.4165_-40.5066_uncorrected_Petrolina.csv", header = F, sep = ",")) %>% 
  select(V1, V3)
petrolina <- petrolina[4:nrow(petrolina),]
colnames(petrolina) <- c("Date", "capFact")
petrolina$Date <- s1
petrolina_initial <- salvador
petrolina_initial[sapply(petrolina_initial, is.factor)] <- lapply(petrolina_initial[sapply(petrolina_initial, is.factor)], 
                                                                function(x) as.numeric(as.character(x)))
petrolina$capFact <- petrolina_initial$capFact

# Recife: (lat,lon) = (-6.5009,-37.6392)
recife <- as_tibble(read.csv2("ninja_wind_-6.5009_-37.6392_uncorrected_Recife.csv", header = F, sep = ",")) %>% 
  select(V1, V3)
recife <- recife[4:nrow(recife),]
colnames(recife) <- c("Date", "capFact")
recife$Date <- s1
recife_initial <- recife
recife_initial[sapply(recife_initial, is.factor)] <- lapply(recife_initial[sapply(recife_initial, is.factor)], 
                                                                  function(x) as.numeric(as.character(x)))
recife$capFact <- recife_initial$capFact

capFact_mean_NE <- as_tibble((salvador$capFact + salvador$capFact + recife$capFact) / 3)

#### Data from region North: BR4 ####
# Manaus: (lat, lon) = (-3.2283,-59.9414)
manaus <- as_tibble(read.csv2("ninja_wind_-3.2283_-59.9414_uncorrected_Manaus.csv", header = F, sep = ",")) %>% 
  select(V1, V3)
manaus <- manaus[4:nrow(manaus),]
colnames(manaus) <- c("Date", "capFact")
manaus$Date <- s1
manaus_initial <- manaus
manaus_initial[sapply(manaus_initial, is.factor)] <- lapply(manaus_initial[sapply(manaus_initial, is.factor)], 
                                                            function(x) as.numeric(as.character(x)))
manaus$capFact <- manaus_initial$capFact
ggplot(manaus[1:48,], aes(x = Date[1:48], y = capFact[1:48])) + geom_line()

# Macapa (lat, lon) = (0.0439,-51.0535)
macapa <- as_tibble(read.csv2("ninja_wind_0.0439_-51.0535_uncorrected_Macapa.csv", header = F, sep = ",")) %>% 
  select(V1, V3)
macapa <- macapa[4:nrow(macapa),]
colnames(macapa) <- c("Date", "capFact")
macapa$Date <- s1
macapa_initial <- macapa
macapa_initial[sapply(macapa_initial, is.factor)] <- lapply(macapa_initial[sapply(macapa_initial, is.factor)], 
                                                            function(x) as.numeric(as.character(x)))
macapa$capFact <- macapa_initial$capFact
ggplot(macapa[1:48,], aes(x = Date[1:48], y = capFact[1:48])) + geom_line()

# Rio Branco  (lat, lon) = (-10.0338,-67.7856)
rio_branco <- as_tibble(read.csv2("ninja_wind_-10.0338_-67.7856_uncorrected_RioBranco.csv", header = F, sep = ",")) %>% 
  select(V1, V3)
rio_branco <- rio_branco[4:nrow(rio_branco),]
colnames(rio_branco) <- c("Date", "capFact")
rio_branco$Date <- s1
rio_branco_initial <- rio_branco
rio_branco_initial[sapply(rio_branco_initial, is.factor)] <- lapply(rio_branco_initial[sapply(rio_branco_initial, is.factor)], 
                                                            function(x) as.numeric(as.character(x)))
rio_branco$capFact <- rio_branco_initial$capFact
ggplot(rio_branco[1:48,], aes(x = Date[1:48], y = capFact[1:48])) + geom_line()


capFact_mean_norte <- as_tibble((manaus$capFact + macapa$capFact + rio_branco$capFact) / 3)
 
#### Data from region Southeast: BR1 ####
# RJ (lat, lon) = (-22.9078,-43.1982)
rj <- as_tibble(read.csv2("ninja_wind_-22.9078_-43.1982_uncorrected_Rj.csv", header = F, sep = ",")) %>% 
  select(V1, V3)
rj <- rj[4:nrow(rj),]
colnames(rj) <- c("Date", "capFact")
rj$Date <- s1
rj_initial <- rj
rj_initial[sapply(rj_initial, is.factor)] <- lapply(rj_initial[sapply(rj_initial, is.factor)], 
                                                                    function(x) as.numeric(as.character(x)))
rj$capFact <- rj_initial$capFact
ggplot(rj[1:48,], aes(x = Date[1:48], y = capFact[1:48])) + geom_line()

# Cuiaba (lat, lon) = (-15.6230,-56.0522)
cuiaba <- as_tibble(read.csv2("ninja_wind_-15.6230_-56.0522_uncorrected_Cuiaba.csv", header = F, sep = ",")) %>% 
  select(V1, V3)
cuiaba <- cuiaba[4:nrow(cuiaba),]
colnames(cuiaba) <- c("Date", "capFact")
cuiaba$Date <- s1
cuiaba_initial <- cuiaba
cuiaba_initial[sapply(cuiaba_initial, is.factor)] <- lapply(cuiaba_initial[sapply(cuiaba_initial, is.factor)], 
                                                    function(x) as.numeric(as.character(x)))
cuiaba$capFact <- cuiaba_initial$capFact
ggplot(cuiaba_initial[1:48,], aes(x = Date[1:48], y = capFact[1:48])) + geom_line()

# Brasilia (lat, lon) = (-15.8768,-47.8345)
brasilia <- as_tibble(read.csv2("ninja_wind_-15.8768_-47.8345_uncorrected_Brasilia.csv", header = F, sep = ",")) %>% 
  select(V1, V3)
brasilia <- brasilia[4:nrow(brasilia),]
colnames(brasilia) <- c("Date", "capFact")
brasilia$Date <- s1
brasilia_initial <- brasilia
brasilia_initial[sapply(brasilia_initial, is.factor)] <- lapply(brasilia_initial[sapply(brasilia_initial, is.factor)], 
                                                            function(x) as.numeric(as.character(x)))
brasilia$capFact <- brasilia_initial$capFact
ggplot(brasilia[1:48,], aes(x = Date[1:48], y = capFact[1:48])) + geom_line()

capFact_mean_sudeste <- as_tibble((rj$capFact + cuiaba$capFact + brasilia$capFact) / 3)

#### Data from region South: BR2 ####
# Foz do Iguacu (lat, lon) = (-25.5585,-54.5732) 
foz <- as_tibble(read.csv2("ninja_wind_-25.5585_-54.5732_uncorrected_Foz.csv", header = F, sep = ",")) %>% 
  select(V1, V3)
foz <- foz[4:nrow(foz),]
colnames(foz) <- c("Date", "capFact")
foz$Date <- s1
foz_initial <- foz
foz_initial[sapply(foz_initial, is.factor)] <- lapply(foz_initial[sapply(foz_initial, is.factor)], 
                                                                function(x) as.numeric(as.character(x)))
foz$capFact <- foz_initial$capFact
ggplot(foz[1:48,], aes(x = Date[1:48], y = capFact[1:48])) + geom_line()

# Floripa (lat, lon) = (-27.5959,-48.5046)
floripa <- as_tibble(read.csv2("ninja_wind_-27.5959_-48.5046_uncorrected_Floripa.csv", header = F, sep = ",")) %>% 
  select(V1, V3)
floripa <- floripa[4:nrow(floripa),]
colnames(floripa) <- c("Date", "capFact")
floripa$Date <- s1
floripa_initial <- floripa
floripa_initial[sapply(floripa_initial, is.factor)] <- lapply(floripa_initial[sapply(floripa_initial, is.factor)], 
                                                      function(x) as.numeric(as.character(x)))
floripa$capFact <- floripa_initial$capFact
ggplot(floripa[1:48,], aes(x = Date[1:48], y = capFact[1:48])) + geom_line()


# Sta Maria (lat, lon) = (-29.6881,-53.8097)
sta_maria <- as_tibble(read.csv2("ninja_wind_-29.6881_-53.8097_uncorrected_sta_maria.csv", header = F, sep = ",")) %>% 
  select(V1, V3)
sta_maria <- sta_maria[4:nrow(sta_maria),]
colnames(sta_maria) <- c("Date", "capFact")
sta_maria$Date <- s1
sta_maria_initial <- sta_maria
sta_maria_initial[sapply(sta_maria_initial, is.factor)] <- lapply(sta_maria_initial[sapply(sta_maria_initial, is.factor)], 
                                                              function(x) as.numeric(as.character(x)))
sta_maria$capFact <- sta_maria_initial$capFact
ggplot(sta_maria[1:48,], aes(x = Date[1:48], y = capFact[1:48])) + geom_line()

capFact_mean_sul <- as_tibble((foz$capFact + floripa$capFact + sta_maria$capFact) / 3)



#### Creating wind_feather_br ####
# Assumption: 1000 for all obs in column "WindPower"
wind_br <- as_tibble(rep(s1,4))
wind_br[,2] <- c(rep("SE3",nrow(wind_br) / 4),
              rep("SE4",nrow(wind_br) / 4),
              rep("SE1",nrow(wind_br) / 4),
              rep("SE2",nrow(wind_br) / 4))
#wind_br[,3] <- 1000
wind_br[,3] <- c(runif(nrow(wind_br), min =0 , max = 1000))
wind_br[,4] <- "P1"
wind_br[,5] <- 0
colnames(wind_br) <- c("Date", "R", "WindPower","P","capFact")

# Filling the column "capFact" with the mean of each region
wind_br$capFact[wind_br$R == "SE3"] <- capFact_mean_NE$value
wind_br$capFact[wind_br$R == "SE4"] <- capFact_mean_norte$value
wind_br$capFact[wind_br$R == "SE1"] <- capFact_mean_sudeste$value
wind_br$capFact[wind_br$R == "SE2"] <- capFact_mean_sul$value

ggplot(wind_br, aes(x = Date, y = capFact, colour = R)) + 
  geom_line() 

# Saving in feather and csv formats
setwd(PC)
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA/data/wind")
write_feather(wind_br,"wind_br1.feather") # WindPower column has  aleatory numbers 
write_feather(wind_br,"wind_br.feather")
write.table(wind_br,file = "wind_br.csv", sep = ";", row.names = FALSE)
