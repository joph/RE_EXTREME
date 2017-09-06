# This script reads ANEEL data of wind power plants (existing, being constructed and non constructed) and 
# prepares a file with max capacity and investment cost for wind power plants. 
# Assumption: 2 categories of plants for each region: 
# 1) Existing and planned: existing, being constructed and non constructed yet -> investment cost zero and capacity of each region
# 2) Non planned: investment cost in each region and a huge potencial (a big number - look for reference after)

#### Existent wind plants ####
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/Wind")
# setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA  Initial Data/Wind")
wp <- as_tibble(read.csv2("wind_power_plants.csv"))
wp1 <- as_tibble(read.csv("daily_wind_power_production_per_wind_farm_2015_07_01-2017-04-18.csv", header = T, sep = ","))

# Visualizing initial data
# ggplot() + 
#  geom_line(data = load_test1, aes(x = id, y = SE, color = "SE"))

ggplot() + 
  geom_line(data = wp1, aes(x = X, y = actual, color = "actual")) +
  geom_line(data = wp1, aes(x = X, y = predicted, color = "predicted"))
  
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

wind_plants <- select(wp, Usina, Potência.Outorgada..kW., value)

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


# Result until now: win_plants_states

#### Being constructed wind plants ####

# Reading files 
# being_constructed
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/Wind")
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA  Initial Data/Wind")
being_constructed <- as_tibble(read.csv2("wind_power_plants_being_constructed.csv", header = T, sep =";"))
being_constructed_initial <- being_constructed
being_constructed_initial[sapply(being_constructed_initial, is.factor)] <- lapply(being_constructed_initial[sapply(being_constructed_initial, is.factor)], 
                                                    function(x) as.numeric(as.character(x)))
being_constructed$Potência.Outorgada..kW. <- being_constructed_initial$Potência.Outorgada..kW.
being_constructed <- being_constructed[1:150,]
being_constructed$Potência.Outorgada..kW. <- being_constructed$Potência.Outorgada..kW. * 1000

being_constructed_selected <- select(being_constructed, Usina, Potência.Outorgada..kW.)
being_constructed_selected[,3] <- as_tibble(rep("", nrow(being_constructed_selected))) # Adding a new column: subsystem

setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/Wind")
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA  Initial Data/Wind")
states_being_constructed <- as_tibble(read.csv2("states_being_constructed.csv", header = T, sep =";"))

# Adding subsystem values
being_constructed_selected_sub <- bind_cols(being_constructed_selected, states_being_constructed)

for (i in 1:nrow(being_constructed_selected_sub)){
  if(being_constructed_selected_sub$Estado[i] == "CE"|
     being_constructed_selected_sub$Estado[i] == "RN"|
     being_constructed_selected_sub$Estado[i] == "PE"|
     being_constructed_selected_sub$Estado[i] == "PI"|
     being_constructed_selected_sub$Estado[i] == "PB"|
     being_constructed_selected_sub$Estado[i] == "BA"|
     being_constructed_selected_sub$Estado[i] == "MA"|
     being_constructed_selected_sub$Estado[i] == "SE") {
     being_constructed_selected_sub$value[i] = "BR3"
  }
}


for (i in 1:nrow(being_constructed_selected_sub)){
  if(being_constructed_selected_sub$Estado[i] == "SC"|
     being_constructed_selected_sub$Estado[i] == "RS"|
     being_constructed_selected_sub$Estado[i] == "PR") {
    being_constructed_selected_sub$value[i] = "BR2"
  }
}

# As we have one observation that the loop does not recognize, 
# we have to assign the subsystem "BR3" to this observation
being_constructed_selected_sub$value[102] = "BR3"

#### construction_non_begin ####

setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/Wind")
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA  Initial Data/Wind")
construction_non_begin <- as_tibble(read.csv2("wind_power_plants_construction_non_begin.csv", header = T, sep =";"))
construction_non_begin_initial <- construction_non_begin
construction_non_begin_initial[sapply(construction_non_begin_initial, is.factor)] <- lapply(construction_non_begin_initial[sapply(construction_non_begin_initial, is.factor)], 
                                                                                  function(x) as.numeric(as.character(x)))

construction_non_begin$Potência.Outorgada..kW. <- construction_non_begin_initial$Potência.Outorgada..kW.
construction_non_begin <- construction_non_begin[1:166,]
construction_non_begin$Potência.Outorgada..kW. <- construction_non_begin$Potência.Outorgada..kW. * 1000

construction_non_begin_selected <- select(construction_non_begin, Usina, Potência.Outorgada..kW.)


#mutate(construction_non_begin_selected, subsistema = rep("", nrow(construction_non_begin_selected)))
#construction_non_begin_selected[,5] <- construction_non_begin_selected$Estado
#construction_non_begin_selected <- select(construction_non_begin_selected, -Estado)
#colnames(construction_non_begin_selected) <- c("Usina", "Potência.Outorgada..kW.","value","X")

# Adding subsystem values in the correspondend column

setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/Wind")
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA  Initial Data/Hydro")
states_non_begin <- as_tibble(read.csv2("states_non_constructed.csv", header = T, sep =";"))

construction_non_begin_selected_sub <- bind_cols(construction_non_begin_selected, states_non_begin)

construction_non_begin_selected_sub[,4] <- construction_non_begin_selected_sub$Estado
colnames(construction_non_begin_selected_sub) <- c("Usina", "Potência.Outorgada..kW.", "value", "Estado")
construction_non_begin_selected_sub$value <- rep("", nrow(construction_non_begin_selected))


for (i in 1:nrow(construction_non_begin_selected_sub)){
  if(construction_non_begin_selected_sub$Estado[i] == "CE"|
     construction_non_begin_selected_sub$Estado[i] == "RN"|
     construction_non_begin_selected_sub$Estado[i] == "PE"|
     construction_non_begin_selected_sub$Estado[i] == "PI"|
     construction_non_begin_selected_sub$Estado[i] == "PB"|
     construction_non_begin_selected_sub$Estado[i] == "BA"|
     construction_non_begin_selected_sub$Estado[i] == "MA"|
     construction_non_begin_selected_sub$Estado[i] == "SE") {
    construction_non_begin_selected_sub$value[i] = "BR3"
  }
}


for (i in 1:nrow(being_constructed_selected_sub)){
  if(construction_non_begin_selected_sub$Estado[i] == "SC"|
     construction_non_begin_selected_sub$Estado[i] == "RS"|
     construction_non_begin_selected_sub$Estado[i] == "PR") {
    construction_non_begin_selected_sub$value[i] = "BR2"
  }
}

#### concatenating three cases: existing, being constructed and construction non begin ####
# We have 3 tables: wind_plants_states (440 plants, existing)
# being_constructed_selected_sub (150, being constructed)
# construction_non_begin_selected_sub (166)
# Let's put together and to aggregate power by subsystem (in MW)
plant_category_one <- bind_rows(wind_plants_states, being_constructed_selected_sub, construction_non_begin_selected_sub) %>% 
  group_by(value) %>% summarise(power_by_sub = sum(Potência.Outorgada..kW.)/1000)

# Max capacity values are ok until here!!! :)

#### Creating inttermitentOpts_br ####
# Reading the Swedish file
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA/data/investOptions")
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA/data/investOptions")

swedish_intermittent <- as_tibble(read.csv2("intermittentOpts - original.csv"))
br_intermittent_opts <- swedish_intermittent
br_intermittent_opts$Region <- rep(paste("BR", c(1,1,2,2,3,3,4,4)),2)
br_intermittent_opts$P <- rep(paste("P",c(rep(1,8),rep(2,8)),sep = ""))
br_intermittent_opts$iTechnology[9:16] <- br_intermittent_opts$iTechnology[1:8]

br_intermittent_opts$Value[br_intermittent_opts$Variable == "Investment" & br_intermittent_opts$P == "P1"] <- 0

br_intermittent_opts$Value[br_intermittent_opts$Variable == "MaxCap" & 
                             br_intermittent_opts$iTechnology == "Wind" &
                             br_intermittent_opts$P == "P1"] <- plant_category_one$power_by_sub[1:3]

#br_intermittent_opts$Value[br_intermittent_opts$Region == "BR4" & 
 #                           br_intermittent_opts$P == "P1" &
  #                           br_intermittent_opts$iTechnology == "Wind" &
   #                        br_intermittent_opts$Variable == "MaxCap"] <- 0

br_intermittent_opts$Value[8] <- 0

#### 2nd category of plants: non-planed
# Investigating investment costs on wind power plants 
# ler 2.4 Energia renovavel (p.251)

# wind potential in Brazil: 350 GW # Reference: http://www.mme.gov.br/documents/10584/3894319/Energia+E%C3%B3lica+-+ano+ref++2015+(3).pdf/f5ca897d-bc63-400c-9389-582cd4f00ea2
# Wind potential - National Atlas 2001 - measurements at 50 meters. 
  # NE: 75 GW, SE: 32.8 GW, N: 12,8 GW, S: 22.8 GW -> total: 143 GW.

# States Atlas - measurements at 100 m (GW)
# NE: -> AL: 0.649, BA: 70.1, CE*: 24.9, PE: 1 , RN: 27.08, PI: NA, MA: NA, SE: NA, 
# S: -> RS: 102.8 , PR:3.37 , SC: NA
# SE: 43.5 -> MG: 39.04 , RJ:2.8 , SP:0.564 , ES: 1.1, MS: NA, MT: NA, GO: NA,
# N: -> AC: NA , RO: NA , RR: , AP: NA, PA: NA, AM: NA, TO: NA
# * 70 m

# For now, let's use the national atlas. 
regions_potential <- as_tibble(c(32800,22800,75000,12800)) # in MW
br_intermittent_opts$Value[br_intermittent_opts$P == "P2" & 
                             br_intermittent_opts$iTechnology == "Wind" &
                             br_intermittent_opts$Variable == "MaxCap"] <- regions_potential$value[1:4]

# Investiment costs
# EPE, 2015(p.288) and Schroder, 2013 (p.85).
# Schroder, 2013: 1269 eur/KW (values of 2010)
# EPE, 2015: Investment cost (R$/KW of 2015)
# 2600 - 5600. As we did in thermal case, we'll use the highest value. 
# Chosen value: 5600 R$ / KW value of 2015. 
# Tomorrow: monetary actualization, finish wind costs and to go to solar plants

#### accessing the annualization factors for 2 years #### 
# FA = {i * (1+i)^t / [(1+i)^t - 1]} 
# Discount rate = 8% a.a. (EPE, 2016, p. 96)
# Plant life: 20 years
FA_wind <- {0.08 * (1.08)^20 / (1.08^20 - 1)}

# Passar para análise mensal. 
# FA_wind_montlhy <- FA_wind / 12

# Monetary actualization: value in R$ dec/2015 we have to pass to R$ april/2017
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/Cambio e inflacao")
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA  Initial Data/Cambio e inflacao")
ipcax <- as_tibble(read.xlsx("IPCA.xlsx", 1)) %>% mutate(ANO, ANO = c(rep(1994 : 2016,each = 12), rep(2017,4)))
colnames(ipcax) <- c("ANO","MES","NUMERO_INDICE")

# Creating a table with the monetary actualization
  # EPE, 2015, p. 288
wind_investment_cost_dec_15 <- 5600
wind_investment_cost_apr_17 <- wind_investment_cost_dec_15 / ipcax$NUMERO_INDICE[ipcax$ANO == 2015 & ipcax$MES == "DEZ"] * ipcax$NUMERO_INDICE[ipcax$ANO == 2017 & ipcax$MES == "ABR"]
wind_investment_cost_apr_17 <- wind_investment_cost_apr_17 * 1e3 # R$ april 2017 / MW :)

# Multiplying by the annualization factor: investment costs in million R$ of april 2017.
wind_investment_cost_apr_17 <- wind_investment_cost_apr_17 * FA_wind / 1e6 # values in millions of reais of april 2017 / MW. 
wind_investment_cost_apr_17

# Filling the second category plants with investment costs
br_intermittent_opts$Value[br_intermittent_opts$P == "P2" & br_intermittent_opts$Variable == "Investment" & br_intermittent_opts$iTechnology == "Wind"] <- wind_investment_cost_apr_17


#### Result: br_intermittent_opts with two plant categories for wind power ####

#### Solar ####
#### First category: existent, being_constructed and construction_non_begin (planned) ####
# Reading excel file with this information
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/Solar")
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA  Initial Data/Solar")
# Existente plants
solar_existent <- as_tibble(read.xlsx("First_category_plants_operating_being_constructed_planned.xlsx", 1)) %>% 
  select(Usina, PotÃªncia.Outorgada..kW., Estado)

solar_existent <- solar_existent[1:50,]

# being_constructed
solar_being_constructed <- as_tibble(read.xlsx("First_category_plants_operating_being_constructed_planned.xlsx", 2)) %>% 
  select(Usina, PotÃªncia.Outorgada..kW., Estado)

solar_being_constructed_initial <- solar_being_constructed
solar_being_constructed_initial[sapply(solar_being_constructed_initial, is.factor)] <- lapply(solar_being_constructed_initial[sapply(solar_being_constructed_initial, is.factor)], 
                                                                                  function(x) as.numeric(as.character(x)))
solar_being_constructed$PotÃªncia.Outorgada..kW. <- solar_being_constructed_initial$PotÃªncia.Outorgada..kW.

solar_being_constructed <- solar_being_constructed[1:38,]

# Construction_non_begin (planned)

solar_planned <- as_tibble(read.xlsx("First_category_plants_operating_being_constructed_planned.xlsx", 3)) %>% 
  select(Usina, PotÃªncia.Outorgada..kW., Estado)
solar_planned <- solar_planned[1:69,]

# Concatenating this plants in a unique variable

solar_plants <- bind_rows(solar_existent,solar_being_constructed,solar_planned) 

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

# Accessing the power by subsystem (in MW)
solar_plant_category_one <- solar_plants %>%  group_by(Subsystem) %>% 
  summarise(power_by_sub = sum(PotÃªncia.Outorgada..kW.)/1000)

# Filling the br_intermittent_opts file with the first category of solar plants
br_intermittent_opts <- bind_rows(br_intermittent_opts,br_intermittent_opts)
br_intermittent_opts$id[17:32] <- c(17:32)
br_intermittent_opts$iTechnology[17:32] <- "PV"

# Investment costs for the first category = 0
br_intermittent_opts$Value[br_intermittent_opts$P == "P1" & 
                             br_intermittent_opts$Variable == "Investment" & 
                             br_intermittent_opts$iTechnology == "PV"] <- 0

# MaxCap for the first category
br_intermittent_opts$Value[br_intermittent_opts$P == "P1" & 
                             br_intermittent_opts$Variable == "MaxCap" &
                             br_intermittent_opts$iTechnology == "PV"] <- solar_plant_category_one$power_by_sub[1:4]

#### Second category for solar plants ####
# Solar PV potencial by subsystem (RADIACAO GLOBAL MEDIA - KWh/m2)
# Solar Atlas potential: http://ftp.cptec.inpe.br/labren/publ/livros/brazil_solar_atlas_R1.pdf p.48
# SE:5.6 + 5.7 , s:5.2, NE:5.9, N:5.5 
# Area information http://www.ibge.gov.br/apps/regioes_geograficas/ (km2)
# SE: 2736909.07, S: 567857.27, NE: 1556128.98, N: 3886576.71

# Potential in KWh/m2
solar_potential_br <- as_tibble(c(11.3,5.2,5.9,5.5))
# Potential in KWh per km2
solar_potential_br <- solar_potential_br * 1e3
# Assumption: 0.1% of region area will be used for PV. 
areas <- as_tibble(c(2736909.07,567857.27,1556128.98,3886576.71)) # km2

#gw available # We usually need 8 km2 to generate 1 GW.
gw_available<-areas*0.001/8

#areas <- areas * 0.01 # km2
# Potential in kWh
solar_potential_br <- solar_potential_br * areas
# Potential in MW
solar_potential_br <- solar_potential_br / (8760 * 1e3)
solar_potential_br

# Filling the br_intermittent_opts with solar potential per subsystem
br_intermittent_opts$Value[br_intermittent_opts$P == "P2" & 
                             br_intermittent_opts$Variable == "MaxCap" &
                             br_intermittent_opts$iTechnology == "PV"] <- solar_potential_br$value[1:4]

#### PV Investment costs: values of today
# Solar
# http://www.solarserver.com/service/pvx-spot-market-price-index-solar-pv-modules.html - pvxchange
  # April, 2017
  # prices of PV modules
  # euros/Wp
  # Europe: 0.46, japan, Korea: 0.53, China: 0.46, Southeast Asia, Taiwan: 0.39

# https://emp.lbl.gov/publications/utility-scale-solar-2015-empirical - planilha "Solar_costs_.xlsx" - Blinger, 2016
  # Sample of US projects
  # Median price: 2.6 $/Wac real values of 2015 p.12 (passar para reais e depois aplicar o IPCA para trazer para abril de 2017)
  # Fixed tilt c-Si 

# SEIA, 2016 - US solar market insight
  # Market research
  # total installed costs for utility fixd tilt
  # 1.33 dez 2015 USD / Wdc 

# IRENA report - Power to change, 2016 p.10
  # https://irena.masdar.ac.ae/GIS/?map=1467 - maps of grid, solar and wind potentials in Brazil
  # 1810 2015 USD / kW (global scale)

# Fu, et al, 2016 - US PV cost benchmark (NREL)
  # 1.42 dec 2016 USD / Wdc
  # Final prices

# EPE, 2015 p.372: 1400-2100 U$/KWp. (values of oct 2014) 
  # costs of Brazilian electricity energy auctions in 2014.

#### Plotting the costs references in order to compare them ####
# Analysys of these costs sources
# Without xchange (considers just PV price)
pv_costs <- as_tibble(c("epe","blinger","seia", "irena", "nrel")) 
costs <- c(2.1, 2.6, 1.33, 1.8, 1.42)
pv_costs$cost <- costs
colnames(pv_costs) <- c("source", "cost(USD/MW)")
pv_costs$publication_year <- c(2015,2016,2016,2016,2016)

mean <- as_tibble(rep(mean(pv_costs$`cost(USD/MW)`),5))
mean$source <- pv_costs$source
colors <- c("#CC3333","#FF9933","#336600","#003366","66600")
ggplot() +
  geom_bar(data = pv_costs, aes(x = pv_costs$source, y = pv_costs$`cost(USD/MW)`, fill = pv_costs$source), stat = "identity") + 
  scale_fill_manual (values = colors) +
  geom_line(data = mean, aes(x = source, y = value, group = 1),  size =1) +
  ylab("cost (USD/MW)") + xlab("sources") + ggtitle("PV investment costs comparation") +
  theme(plot.title = element_text(hjust = 0.5))

# Considering xchange
pv_costs_xchange <- as_tibble(c("epe","xchange", "blinger","seia", "irena", "nrel")) 
costs <- c(2.1, 0.39, 2.6, 1.33, 1.8, 1.42)
pv_costs_xchange$cost <- costs
colnames(pv_costs_xchange) <- c("source", "cost(USD/MW)")
pv_costs_xchange$publication_year <- c(2015,2017,2016,2016,2016,2016)
mean <- as_tibble(rep(mean(pv_costs_xchange$`cost(USD/MW)`),6))
mean$source <-pv_costs_xchange$source
colors <- c("#CC3333","#FF9933","#336600","#003366","66600","#00CC99")
ggplot() +
  geom_bar(data = pv_costs_xchange, aes(x = pv_costs_xchange$source, y = pv_costs_xchange$`cost(USD/MW)`, fill = pv_costs_xchange$source), stat = "identity") + 
  scale_fill_manual (values = colors) +
  geom_line(data = mean, aes(x = source, y = value, group = 1),  size =1) +
  ylab("cost (USD/MW)") + xlab("sources") + ggtitle("PV investment costs comparation") +
  theme(plot.title = element_text(hjust = 0.5))


# Let's consider IRENA value: 1810 2015 USD / kW
# Monetary actualization 
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA  Initial Data/Cambio e inflacao")
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/Cambio e inflacao")
exchange_rate <- as_tibble(read.xlsx("IPCA.xlsx", 3))
exchange_rate <- exchange_rate[,1:4]
# Looking for currency mean tax for dec 2015
er_2015 <- as_tibble(exchange_rate$R..U.[exchange_rate$ano == 2015])
er_2015 <- as_tibble(er_2015$value[!is.na(er_2015$value)]) 
mean_exchange_rate <- mean(er_2015$value)

# Transform the value from 2015 USD to 2015 R$
investment_cost_PV_2015 <- 1800 * mean_exchange_rate # R$ / KWp value of  2015.
investment_cost_PV_apr_2017 <- investment_cost_PV_2015 / ipcax$NUMERO_INDICE[ipcax$ANO == 2015 & ipcax$MES == "DEZ"] * ipcax$NUMERO_INDICE[ipcax$ANO == 2017 & ipcax$MES == "ABR"] # R$/KW apr 2017
investment_cost_PV <- investment_cost_PV_apr_2017 * 1e3 # R$ april 2017 / MW
FA_PV <- {0.08 * (1.08)^25 / (1.08^25 - 1)} # 25 years life time and 8% of descount rate (EPE, 2015, p.372, table 7)
investment_cost_PV <- investment_cost_PV * FA_PV / 1e6 # millions reais april 2017 / MW
investment_cost_PV
# This final cost of pv is bigger than the old one. Is it a problem?

br_intermittent_opts$Value[br_intermittent_opts$P == "P2" & 
                             br_intermittent_opts$iTechnology == "PV" & 
                             br_intermittent_opts$Variable == "Investment"] <- investment_cost_PV

##### For the time being, changing the names of regions from "BR" to "SE"
br_intermittent_opts$Region[br_intermittent_opts$Region == "BR 1"] <- "SE1"
br_intermittent_opts$Region[br_intermittent_opts$Region == "BR 2"] <- "SE2"
br_intermittent_opts$Region[br_intermittent_opts$Region == "BR 3"] <- "SE3"
br_intermittent_opts$Region[br_intermittent_opts$Region == "BR 4"] <- "SE4"
#setwd("C:/Users/cancella/Google Drive/!IIASA/COPA/data/investOptions")
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA/data/investOptions")
save_invest_opts_br <- write.table(br_intermittent_opts, file = "br_intermittent_opts_1.csv", sep = ";", row.names = FALSE )
#####

# Saving file 
#setwd("C:/Users/cancella/Google Drive/!IIASA/COPA/data/investOptions")
# setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA/data/investOptions")
#write.table(br_intermittent_opts, file = "br_intermittent_opts.csv", sep = ";", row.names = FALSE)
#write_feather(br_intermittent_opts, "br_intermittent_opts.feather")

# OLD CODE FOR PV COSTS 
#investment_cost_PV_oct_2014 <- 2100 * 2.35 # R$ / KWp value of  2015.
#investment_cost_PV_apr_2017 <- investment_cost_PV_oct_2014 / ipcax$NUMERO_INDICE[ipcax$ANO == 2014 & ipcax$MES == "OUT"] * ipcax$NUMERO_INDICE[ipcax$ANO == 2017 & ipcax$MES == "ABR"] # R$/KW apr 2017
#investment_cost_PV <- investment_cost_PV_apr_2017 * 1e3 # R$ april 2017 / MW
#FA_PV <- {0.08 * (1.08)^25 / (1.08^25 - 1)} # 25 years life time and 8% of descount rate (EPE, 2015, p.372, table 7)
#investment_cost_PV <- investment_cost_PV * FA_PV / 1e6 # millions reais april 2017