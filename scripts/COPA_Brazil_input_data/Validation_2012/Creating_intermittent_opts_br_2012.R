# This script reads ANEEL data of wind power plants (existing) and 
# prepares a file with max capacity and investment cost for wind power plants. 
# Assumption: let's use the existing power plants in 2012 and consider zero InvestmentCost for these plants.

#### Existent wind plants ####
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/Wind")
# setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA  Initial Data/Wind")
wp <- as_tibble(read.csv2("wind_power_plants.csv"))

# Reading file of existing wind power plants
wp_initial <- wp
wp_initial[sapply(wp_initial, is.factor)] <- lapply(wp_initial[sapply(wp_initial, is.factor)], 
                                      function(x) as.numeric(as.character(x)))

wp$Potência.Outorgada..kW. <- wp_initial$Potência.Outorgada..kW. * 1e3
wp$Potência.Fiscalizada..kW. <- wp_initial$Potência.Fiscalizada..kW.* 1e3

# adjusting and cleaning data
wp$Potência.Outorgada..kW.[12]  <- wp$Potência.Outorgada..kW.[12] / 1000
wp$Potência.Outorgada..kW.[120] <- 2.24
wp$Potência.Outorgada..kW.[181] <- wp$Potência.Outorgada..kW.[181] / 1000
wp$Potência.Outorgada..kW.[308] <- wp$Potência.Outorgada..kW.[308] / 1000
wp$Potência.Outorgada..kW.[315] <- 22.5
wp$Potência.Outorgada..kW.[426] <- 1.98
wp$Potência.Outorgada..kW.[431] <- 2099.5
wp$Potência.Outorgada..kW.[438] <- 0.16
wp$Potência.Outorgada..kW.[439] <- 0.16
wp$Potência.Outorgada..kW.[440] <- 0.16
wp$Potência.Outorgada..kW.[441] <- 0.00
wp <- wp[1:440,]

wp[,10] <- as_tibble(rep("", nrow(wp))) # adding a new column: subsystem

# checking the adjustment. It has to be equals to the potencia outorgada
sum(wp$Potência.Outorgada..kW.)

# Units: power in KW for now!!!

# Plants categories
cat <- wp %>% group_by(Destino.da.Energia) %>% count()

# Reading file with states of each wind power plant
states <- as_tibble(read.csv2("States_wp.csv", header = T, sep =";"))
wind_plants <- select(wp, Usina, Data.Operação, Potência.Outorgada..kW., value)
wind_plants_states <- bind_cols(wind_plants, states)

# Adding the subsystem in accordance with the location state of each plant
NE <- as_tibble(c("CE", "RN", "PE", "PI", "PB", "BA", "MA", "SE"))
SE <- as_tibble(c("SP", "MG", "RJ"))
SUL <- as_tibble(c("SC", "RS", "PR"))

wind_plants_states <- bind_cols(wind_plants, states)
for (i in 1:nrow(wind_plants_states)){
  if(wind_plants_states$Estado[i] == "CE"|
     wind_plants_states$Estado[i] == "RN"|
     wind_plants_states$Estado[i] == "PE"|
     wind_plants_states$Estado[i] == "PI"|
     wind_plants_states$Estado[i] == "PB"|
     wind_plants_states$Estado[i] == "BA"|
     wind_plants_states$Estado[i] == "MA"|
     wind_plants_states$Estado[i] == "SE") {
    wind_plants_states$value[i] = "BR3"
  }
}


for (i in 1:nrow(wind_plants_states)){
  if(wind_plants_states$Estado[i] == "SC"|
     wind_plants_states$Estado[i] == "RS"|
     wind_plants_states$Estado[i] == "PR") {
    wind_plants_states$value[i] = "BR2"
  }
}

for (i in 1:nrow(wind_plants_states)){
  if(wind_plants_states$Estado[i] == "SP"|
     wind_plants_states$Estado[i] == "MG"|
     wind_plants_states$Estado[i] == "RJ") {
    wind_plants_states$value[i] = "BR1"
  }
}


# Result until now: wind_plants_states
#### Dates: we are going to use only plants that was in operation in 2012. 
# Separating dates
dates <- as_tibble(wind_plants_states$Data.Operação)

# getting the years of each date for the two types of separators
dates$Date <- lubridate::year(strptime(x = as.character(wind_plants_states$Data.Operação),
                       format = "%d/%m/%Y"))

dates$Date2 <- lubridate::year(strptime(x = as.character(wind_plants_states$Data.Operação),
                        format = "%d-%m-%Y"))

# adding a column to merge the values
dates$final_date <- rep("", nrow(dates))

# filling the new column with years of the two types of separators
dates$final_date[!is.na(dates$Date)] <- dates$Date[!is.na(dates$Date)]
dates$final_date[!is.na(dates$Date2)] <- dates$Date2[!is.na(dates$Date2)]
dates$final_date <- as.numeric(dates$final_date)


# There are 4 plants without operation date. 
# Prainha: http://www.creaes.org.br/img/Eolica_Mundo_e_Brasil.pdf - 1999
# Mucuripe: Goncalves, 2016. - 1996
# Macau - http://cerne.org.br/pdfs/CartilhaEolicaCERNE2014.pdf - 2003
# Brejo - nao achei e coloquei ano 2000. sao 6 kW.
# Adding operation date for these plants.
dates$final_date[c(1,5,25,181)] <- c(1999,1996,2003,2000)

# Adding year column to wind_plants_states: table with information of localization of each plant and capacity.
wind_plants_states <- wind_plants_states %>% mutate(op_year = dates$final_date)

wind_plants_final <- wind_plants_states %>% filter(op_year <= 2012) %>% select(Usina, Potência.Outorgada..kW., value, op_year) %>% 
  group_by(value) %>% summarise(MaxCap = sum(Potência.Outorgada..kW.)/1000)


#### Solar ####
#### First category: existent, being_constructed and construction_non_begin (planned) ####
# Reading excel file with this information
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/Solar")
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA  Initial Data/Solar")
# Existente plants
solar_existent <- as_tibble(read.csv2("First_category_plants_operating_being_constructed_planned.csv", header = T, sep =";")) %>% 
  select(Usina, Data.Operação, Potência.Outorgada..kW., Estado)

solar_existent_i <- solar_existent
solar_existent_i[sapply(solar_existent_i, is.factor)] <- lapply(solar_existent_i[sapply(solar_existent_i, is.factor)], 
                                                    function(x) as.numeric(as.character(x)))
solar_existent_i

solar_existent$Potência.Outorgada..kW. <- solar_existent_i$Potência.Outorgada..kW.
solar_existent <- solar_existent[1:50,]

# adding subsystem column

solar_plants <- solar_existent

solar_plants <- mutate(solar_plants, Subsystem = rep("", nrow(solar_plants)))
for (i in 1:nrow(solar_plants)){
  if(solar_plants$Estado[i] == "CE"|
     solar_plants$Estado[i] == "RN"|
     solar_plants$Estado[i] == "PE"|
     solar_plants$Estado[i] == "PI"|
     solar_plants$Estado[i] == "PB"|
     solar_plants$Estado[i] == "BA"|
     solar_plants$Estado[i] == "MA"|
     solar_plants$Estado[i] == "AL"|
     solar_plants$Estado[i] == "SE") {
    solar_plants$Subsystem[i] = "BR3"
  }
}


for (i in 1:nrow(solar_plants)){
  if(solar_plants$Estado[i] == "SC"|
     solar_plants$Estado[i] == "RS"|
     solar_plants$Estado[i] == "PR") {
    solar_plants$Subsystem[i] = "BR2"
  }
}

for (i in 1:nrow(solar_plants)){
  if(solar_plants$Estado[i] == "SP"|
     solar_plants$Estado[i] == "RJ"|
     solar_plants$Estado[i] == "MG"|
     solar_plants$Estado[i] == "MT"|
     solar_plants$Estado[i] == "MS"|
     solar_plants$Estado[i] == "GO"|
     solar_plants$Estado[i] == "ES") {
    solar_plants$Subsystem[i] = "BR1"
  }
}

for (i in 1:nrow(solar_plants)){
  if(solar_plants$Estado[i] == "AC"|
     solar_plants$Estado[i] == "RO"|
     solar_plants$Estado[i] == "RR"|
     solar_plants$Estado[i] == "AP"|
     solar_plants$Estado[i] == "PA"|
     solar_plants$Estado[i] == "AM"|
     solar_plants$Estado[i] == "TO") {
    solar_plants$Subsystem[i] = "BR4"
  }
}

#### solar dates ####
# Identifying operation years of each solar power plant.
solar_op_years <- as_tibble(lubridate::year(lubridate::dmy(solar_plants$Data.Operação, quiet = F, tz = "CET")))

solar_plants <- solar_plants %>% mutate(op_years = solar_op_years$value)

# Five plants don't have any data for start operation. 
# PV Beta Test Site - 2000 - didn't find
# Pituaçu solar - 2012 - http://www.coelba.com.br/Noticias/Pages/Pioneira-na-Am%C3%A9rica-Latina,-usina-solar-do-Est%C3%A1dio-de-Pitua%C3%A7u-completa-3-anos-com-economia-de-R$-400-mil-em-energia-el%C3%A9tric.aspx
# Tanquinho - 2012 - http://exame.abril.com.br/mundo/sao-paulo-ganha-sua-primeira-usina-de-energia-solar/
# Solaris - 2000 - didn't find
# Ilha Grande - 2012, but it is for the isolated system ("Luz para todos" program). Because of that I will put 2013, since it does not has to enter.
  #http://www.guascor.com.br/?p=410
solar_plants$op_years[c(5:7,9,10)] <- c(2000, 2012, 2012, 2000, 2013)

# Accessing the MaxCap by subsystem (in MW)
solar_plants_final <- solar_plants %>% filter(op_years <= 2012) %>% select(Usina, Potência.Outorgada..kW., Subsystem, op_years) %>% 
  group_by(Subsystem) %>% summarise(MaxCap = sum(Potência.Outorgada..kW.)/1000)

#### Creating intermittentOpts_br_2012 ####
# Reading the Swedish file
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA/data/investOptions")
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA/data/investOptions")

swedish_intermittent <- as_tibble(read.csv2("intermittentOpts - original.csv"))
br_intermittent_opts_2012 <- swedish_intermittent
br_intermittent_opts_2012$Region <- rep(paste("BR",c(1,1,2,2,3,3,4,4), sep=""),2)
br_intermittent_opts_2012$P <- rep(paste("P",c(rep(1,8),rep(1,8)),sep = ""))

# Filling investment cost zero for all plants
br_intermittent_opts_2012$Value[br_intermittent_opts_2012$Variable == "Investment"] <- 0

# Filling with MaxCap of each region to wind power plants
br_intermittent_opts_2012$Value[br_intermittent_opts_2012$iTechnology == "Wind" & 
                                  br_intermittent_opts_2012$Variable == "MaxCap"] <- c(wind_plants_final$MaxCap, 0)

# Filling with MaxCap of each region to solar power plants
br_intermittent_opts_2012$Value[br_intermittent_opts_2012$iTechnology == "PV" & 
                                  br_intermittent_opts_2012$Variable == "MaxCap"] <- c(solar_plants_final$MaxCap)


##### For the time being, changing the names of regions from "BR" to "SE"
br_intermittent_opts_2012$Region[br_intermittent_opts_2012$Region == "BR1"] <- "SE1"
br_intermittent_opts_2012$Region[br_intermittent_opts_2012$Region == "BR2"] <- "SE2"
br_intermittent_opts_2012$Region[br_intermittent_opts_2012$Region == "BR3"] <- "SE3"
br_intermittent_opts_2012$Region[br_intermittent_opts_2012$Region == "BR4"] <- "SE4"
#setwd("C:/Users/cancella/Google Drive/!IIASA/COPA/data/investOptions")
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA/data/investOptions")
save_invest_opts_br <- write.table(br_intermittent_opts_2012, file = "br_intermittent_opts_2012_1.csv", sep = ";", row.names = FALSE )
#####

# Saving file 
#setwd("C:/Users/cancella/Google Drive/!IIASA/COPA/data/investOptions")
# setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA/data/investOptions")
#write.table(br_intermittent_opts, file = "br_intermittent_opts.csv", sep = ";", row.names = FALSE)
#write_feather(br_intermittent_opts, "br_intermittent_opts.feather")
