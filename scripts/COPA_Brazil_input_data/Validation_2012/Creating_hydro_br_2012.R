# This script creates the hydro files for validation of Brazilian version of COPA. 
# hydro_data_br.csv and shype_hydro_nat_ts_br.feather

#### shype_hydro_nat_ts_br.feather ####
# Reading the Swedish inflow file
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA/data/hydro")
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA/data/hydro")
se_profile <- read_feather("shype_hydro_nat_ts.feather")

# Reading the file with Brazilian daily inflows
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/Hydro")
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA  Initial Data/Hydro")
br_profile <- as_tibble(read.csv("daily_inflows_4regions_1979_2014.csv", header = T, sep = ";")) %>% 
  select(-X, -X.1) 
br_profile <- br_profile[1:13149,]
br_profile$Date<-as.POSIXct(paste(br_profile$Date,"00:00:00"))

br_profile<-br_profile %>% gather(Region,mwh,-Date) 


# Creating date column
dates<-c("1979-01-01 00:00:00",
         "2014-12-31 23:00:00")
s1<-seq(as.POSIXct(dates[1],tz = "UTC"),
        as.POSIXct(dates[2],tz = "UTC"),
        by="h")
br_hydro <- as_tibble(s1)
colnames(br_hydro, do.NULL = TRUE, prefix = "col")
colnames(br_hydro) <- c("date")

# doing the time list for each region
br_hydro <- bind_rows(bind_cols(br_hydro,tibble(region=rep("N",nrow(br_hydro)))),
                      bind_cols(br_hydro,tibble(region=rep("NE",nrow(br_hydro)))),
                      bind_cols(br_hydro,tibble(region=rep("S",nrow(br_hydro)))),
                      bind_cols(br_hydro,tibble(region=rep("SE",nrow(br_hydro)))))


fd<-full_join(br_hydro,br_profile,by=c("date"="Date","region"="Region"))

fd1<-fd %>% mutate(dd=format(date, format = "%Y-%m-%d")) %>% 
  group_by(region,dd) %>% mutate(mwh1=(max(mwh/24, na.rm=TRUE)),variable=as.character("shype_hydro")) %>% 
  ungroup() 

# Filling the br_hydro
br_shype_hydro <- fd1 %>% select(date, variable,region, mwh=mwh1)
#colnames(br_shype_hydro) <- c("date", "variable", "region", "mwh")

br_shype_hydro$region <- c(rep(c("SE4"), nrow(br_shype_hydro) / 4),
                           rep(c("SE3"), nrow(br_shype_hydro) / 4),
                           rep(c("SE2"), nrow(br_shype_hydro) / 4),
                           rep(c("SE1"), nrow(br_shype_hydro) / 4))

# writing file(feather and csv)
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA/data/hydro")
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA/data/hydro")
#save_hydro_data_br <- write.table(br_shype_hydro, file = "br_shype_hydro.csv", sep =";", row.names = FALSE )
#write_feather(br_shype_hydro, "br_shype_hydro.feather")

# checking data for 2012
shype_br <- read_feather("br_shype_hydro.feather")
dates_shype <- as_tibble(year(shype_br$date))
  
shype_br_2012 <- shype_br %>% mutate(year = dates_shype$value) %>% filter(year == 2012)

#### hydro_data_br_2012 ####
# Creating the structure of hydro_data_br

# Reading the swedish file
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA/data/hydro")
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA/data/hydro")
se_hydro <- as_tibble(read.csv2("hydro_data - original.csv"))

se_hydro_initial <- se_hydro
se_hydro_initial[sapply(se_hydro_initial, is.factor)] <- lapply(se_hydro_initial[sapply(se_hydro_initial, is.factor)], 
                                                          function(x) as.numeric(as.character(x)))

se_hydro$maxReservoir <- se_hydro_initial$maxReservoir


# Filling file with ONS values. To do: check the assumptions with Johannes
hydro_data_br_2012 <- se_hydro
hydro_data_br_2012$Region <- c("BR1", "BR2", "BR3", "BR4", "BR1")

# maxReservoir
# I've got this data from Julian. He got it from the IPDO dec-2012. This page is not available anymore. 
# It is very similar from may-2017 from ons. 
  # http://www.ons.org.br/tabela_reservatorios/conteudo.asp Acessed: July, 24th, 2017.

maxreservoir_MWh_2012 <- c(149720339.60, 14596657.53,38587628.23, 10613880.13)  
hydro_data_br_2012$maxReservoir <- c(maxreservoir_MWh_2012,0) 

#### MaxHydPower ####
# MaxHydPower: the maximum power that could be generated in each subsystem.
# We will determine it by the aggregation of power information of each subsystem. It came from CadUsh.csv
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/Newave_deck_dec_2012")
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA  Initial Data/Newave_deck_dec_2012")
cad <- as_tibble(read.csv2("CadUsH.csv", header = T, sep = ";"))
# n <- c(1:5)
# test <- paste("X.Maq.", n[1:5], ".", sep ="")
cad <- cad %>% select(Usina, Sistema, X.Maq.1., PotEf.1., 
                      X.Maq.2., PotEf.2.,
                      X.Maq.3., PotEf.3.,
                      X.Maq.4., PotEf.4.,
                      X.Maq.5., PotEf.5.)

cad_initial <- cad
cad_initial[sapply(cad_initial, is.factor)] <- lapply(cad_initial[sapply(cad_initial, is.factor)], 
                                                                  function(x) as.numeric(as.character(x)))  
cad[,3:ncol(cad)] <- cad_initial[,3:ncol(cad_initial)]  
cad <- cad %>% mutate(PotEfUn = X.Maq.1. * PotEf.1. + 
                        X.Maq.2. * PotEf.2. + 
                        X.Maq.3. * PotEf.3. + 
                        X.Maq.4. * PotEf.4. + 
                        X.Maq.5. * PotEf.5.) %>% 
  separate(Sistema, into = c("num", "subsystem"), sep ="-")

maxHydPower <- cad %>% group_by(subsystem) %>% summarise(maxHydPower = sum(PotEfUn))

hydro_data_br_2012$maxHydPower <- c(maxHydPower$maxHydPower[3],
                               maxHydPower$maxHydPower[4],
                               maxHydPower$maxHydPower[1],
                               maxHydPower$maxHydPower[2],
                               0)

#### minFlow ####
# minFlow will be minimum observed value from the daily inflows from 1979 to 2014 (Johannes file).
minFlow <- br_profile %>% group_by(Region) %>% summarise(minFlow = (min(mwh))/24)
hydro_data_br_2012$minFlow <- c(minFlow$minFlow[c(4,3,2,1)],0) 

#### changing region names for the time being
hydro_data_br_2012$Region[hydro_data_br_2012$Region == "BR1"] <- "SE1"
hydro_data_br_2012$Region[hydro_data_br_2012$Region == "BR2"] <- "SE2"
hydro_data_br_2012$Region[hydro_data_br_2012$Region == "BR3"] <- "SE3"
hydro_data_br_2012$Region[hydro_data_br_2012$Region == "BR4"] <- "SE4"
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA/data/hydro")
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA/data/hydro")
write.table(hydro_data_br_2012, file = "hydro_data_br_2012_1.csv", sep =";", row.names = FALSE )


####


# writing table and Saving file 
#setwd("C:/Users/cancella/Google Drive/!IIASA/COPA/data/hydro")
# setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA/data/hydro")
#write.table(hydro_data_br, file = "hydro_data_br.csv", sep =";", row.names = FALSE )
#write_feather(hydro_data_br, "hidro_data_br.feather")

########################################################################################
#### Draft: something old ####
# IPDO ONS: minimum reservoir levels: % of maximum storage capacity
# http://www.ons.org.br/publicacao/ipdo/
# SE: 15,4% on nov, 28th 2014.
# S: 28.9% on april, 25th 2012.
# NE: 5% on jan, 3th 2016.
# N: 15,4% on nov, 28th 2014.
#min_stor <- as_tibble(c(0.154, 0.289, 0.05, 0.278)) 
#min_flow_MWmonth <- MWmonth * min_stor
#min_flow_MWh <- min_flow_MWmonth * 720 * 12 # MWh
#hydro_data_br$minFlow <- c(min_flow_MWh$value,0)

