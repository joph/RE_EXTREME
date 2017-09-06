# This script checks which deck plants of May 2017 do not yet exist,
# Which are the fuels of these plants and adds the cost of investment for each of them.

base_dir_IIASA <- setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/Costs/Tables")
# tables_base_dir_casa <- setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA  Initial Data/Costs/Tables")

# Reading the necessary tables
CVU_por_usina <-  read.csv2("CVU por usina.csv", sep =";", header=T)
usinas_agrupadas_por_subsistema <- read.csv2("Usinas_agrupadas_por_subsistema.csv", sep =";", header=T)
usinas_pot_disp <- read.csv2("usinas_pot_disp.csv", sep = ";", header = T)

# New table with the situation and fuel of each plant
comb_sit_inicial <- mutate(CVU_por_usina, Situacao = usinas_agrupadas_por_subsistema[,4])
comb_sit <- select(comb_sit_inicial,NUM:TIPO_COMB.,Situacao)

# Searching for fuels from non-existing plants (situação = NE)
comb_sit_NE <- filter(comb_sit, Situacao == "NE") # These plants will receive the investment costs values. 
# They have pot = 0. We've to look the file expt.dat to add its capacity. 
#comb_sit_EE <- filter(comb_sit, Situacao == "EE")
#comb_sit_EX <- filter(comb_sit, Situacao == "EX")

#The comb_sit_NE table shows that we need to find values for investment cost for biomass, GN and coal.

#### Natural gas ####
# Single cycle and combined cycle
# Reference: EPE, 2016 - "Energia Termelétrica - Gás natural, biomassa, carvão e nuclear", p.87
tipos_ute <- read.csv2("Tipos_UTE_GN.csv", header=T, sep=";")
share_ciclo_aberto <- sum(tipos_ute[,2]) / (sum(tipos_ute[,2]) + sum(tipos_ute[,3]))
share_ciclo_comb <- sum(tipos_ute[,3]) / (sum(tipos_ute[,2]) + sum(tipos_ute[,3]))
print(share_ciclo_comb)
# Since 61% of thermal plants comes from combined cycle, 
# I'm going to assume that all natural gas plants have combined cycle investiment costs.


# Reading the table of investment costs of natural gas thermal plants of p. 94 of EPE reference
# Reference: EPE, 2016 - Energia Termelétrica - Gás natural, biomassa, carvão e nuclear
# Unit: U$/KW.
invest_costs_GN <- read.csv2("investment_costs_GN.csv", header=T, sep=";")
# Let's choose the latest and highest value of the combined cycle gas turbine -> 1289 U $ / KW (2013 dollar)
# Minimum value: 627 US/KW.

#### Biomass ####
# Min = 337 U$/KW and max = 2002 U$/KW updated to values of december 2015 by IPCA (EPE, 2016, p.180).
# Let's use the upper bound to be conservative.

#### Coal ####
# Let's use the value of 5000 U$/KW according to p. 290 (table 19) of EPE, 2016.
# Maximum value of the investment cost limits for the national coal.
# Minimum value: 2000 U$/KW

# Assumption: We always used the highest values to be the more conservative as possible. 
# Note: Schroder, 2013 doesn't consider the infrastructure conection costs. 

#### Doc Schroder, 2013 ####
# discount rate = 9 % per year
# EUR-USD = 1.33 EUR/USD (assumption from the executive summary)

# Units: euros from 2010. I've looked for the values of 2015 in tabble 33 (p.85)
  # combined cycle: 800 eur/KW
  # Biomass: 2424 eur/KW
  # Coal: 1200 eur/KW (PC w/o CCTS)

# Comparing the investment costs data 
initial_table <- read.csv2("investment_costs_comparation.csv", sep = ";", header = T)
comparation_table <- as_tibble(initial_table)

#### accessing the annualization factors for 2 years #### 
  # FA = {i * (1+i)^t / [(1+i)^t - 1]} 
  # Discount rate = 8% a.a. (EPE, 2016, p. 96)
  # Plant lifes: NG = 30 years, biomass = 20 years and coal = 40 years
  # References p. 97 EPE, 2016, p. 187 EPE, 2016 e p. 290 EPE, 2016

FA_GN  <- {0.08 * (1.08)^30 / (1.08^30 - 1)}
FA_bio <- {0.08 * (1.08)^20 / (1.08^20 - 1)}
FA_car <- {0.08 * (1.08)^40 / (1.08^40 - 1)}


#### investiment cost = Investment cost of the fuel * annualization factor ####
# OBS: It is not necessary to multiply by capacity of each plant, because it is done in GAMS code. 
# Reading the table that contains the capacity of each plant
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/Costs/Tables")
#tables_base_dir_casa
pot_usinas <- read.csv2("usinas_pot_disp.csv", sep = ";", header = T, stringsAsFactors = F)
setwd(base_dir_IIASA <- setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/Costs/deck_newave_2017_05"))


# Adding the information of available capacity
add_pot_disp <- mutate(comb_sit, pot = pot_usinas$POT, pot_disp = pot_usinas$Disp)


# Defining the auxiliary table that will be used for the scenario without considering investment costs
# Show the information and the format of the plants in the documentation 
tabela_para_invest_opts_br_sem_CI <- mutate(add_pot_disp, CVU = CVU_por_usina$CUSTO)
indice_duplo <- paste(tabela_para_invest_opts_br_sem_CI$TIPO_COMB., tabela_para_invest_opts_br_sem_CI$Situacao, sep = "." )
sub_usinas <- select(usinas_agrupadas_por_subsistema, SSIS) 
tabela_para_invest_opts_br_sem_CI <- mutate(tabela_para_invest_opts_br_sem_CI, ind = indice_duplo) # add a double index
tabela_para_invest_opts_br_sem_CI <- mutate(tabela_para_invest_opts_br_sem_CI, Subsistema = sub_usinas$SSIS)

# Saving the table to use it on the script that organizes the data in order to COPA reads 
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/Costs/Tables")
tabela_para_invest_opts_br_sem_CI_table <-  write.table(tabela_para_invest_opts_br_sem_CI, file = "tabela_para_invest_opts_br_sem_CI.csv", sep=";", row.names = FALSE)

## Conclusion: if I would like to make a file without the investiment costs, 
## the information of this table would be enough. 


# For the scenario with investment cost of plants, we will have 
# to add the capacity of the nonexistent plants and and its investment costs.
# Plants that do not exist (NE) have a capacity equal to zero.
# Looking at the expt.dat, I have the capacity infos of these plants.
# I did a manual manipulation in this file and 
# I will read in R only the capacity information of each plant that will be considered in the analysis.
  
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/Costs/deck_newave_2017_05")
pot_usinas_NE_inicial <- read.delim("EXPT_POT_NE.DAT", header = TRUE, sep = "", quote = "\"",
                                    dec = ".", fill = TRUE, comment.char = "")
pot_usinas_NE <- as_tibble(pot_usinas_NE_inicial)

# Adding the capacity information for each nonexistent plant. 
i <- c(1,6,9,15,26,29,33,37,39,42,59,61,74,87,93,113,117) # index of lines corresponding to plants that do not exist
j <- c(1:17) # Table index that have the capacity of the plants that do not exist.
m <- pot_usinas_NE[,1] # # identification number of the plants that do not exist

#tab_CI_pot_antiga <- add_pot_disp 
#tab_CI_pot_antiga[i,5] <- pot_usinas_NE[j,3]

vetor_nulo <- tibble(c(rep(0,122))) # vector of zeros that will receive the identification numbers of the plants
vetor_nulo[i,1] <- m # filling the vector with the identification numbers of the nonexistent plants
indices_NE <- vetor_nulo

tab_CI_pot_nova <- add_pot_disp 
tab_CI_pot_nova[which(tab_CI_pot_nova$NUM == indices_NE),5] <- pot_usinas_NE[j,3]

# Adding the column named "investment cost" in the table
Invest_cost <- vector(mode = "integer", length = nrow(tab_CI_pot_nova))
tab_CI_pot_com_CI <- mutate(tab_CI_pot_nova, InsvestCost = Invest_cost)

# Accessing the annualized cost of the investment in R$
# Initial information:
# 1289 U$ / KW (2013 dollar) NG
# 2002 U$ / KW updated to values of ten of 2015 by the IPCA (EPE, 2016, p.180) biomass
# 5000 U$ / KW according to p. 290 (table 19) of EPE, 2016 coal -> consider 2016


#### Monetary correction ####
# To transform the amounts in R$ for May 2017, we will first apply the average exchange rate for the year in question.
# So we will have R$ for the year in question. 
# After that, we will apply the monetary correction for the IPCA (Official Brazilian inflation index)
# Then we will get R$ of May 2017.
# Doing this in the "Atualizacao_monetaria_custos_investimento.xls" worksheet, we have the following values
# R$ / MW of April 2017

# Reading the IPCA time serie
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/Cambio e inflacao")
library(xlsx)
ipcax <- as_tibble(read.xlsx("IPCA.xlsx", 1)) %>% mutate(ANO, ANO = c(rep(1994 : 2016,each = 12), rep(2017,4)))
cambio <- as_tibble(read.xlsx2("IPCA.xlsx", sheetName = "Cambio")) %>% filter(R..U.1 != "")

cambio_initial <- cambio
cambio_initial[sapply(cambio_initial, is.factor)] <- lapply(cambio_initial[sapply(cambio_initial, is.factor)], 
                                                          function(x) as.numeric(as.character(x)))
means <- group_by(cambio_initial, X..1) %>% summarise(avg = mean(R..U.1))


cambio1 <- as_tibble(read.xlsx2("IPCA.xlsx", sheetName = "cambio1")) %>% filter(R..U. != "")

cambio_initial1 <- cambio1
cambio_initial1[sapply(cambio_initial1, is.factor)] <- lapply(cambio_initial1[sapply(cambio_initial1, is.factor)], 
                                                            function(x) as.numeric(as.character(x)))
means <- group_by(cambio_initial1, ano) %>% summarise(avg = mean(R..U.))

# taking the means of years we want to work with
mean_2013 <- filter(means, ano == 2013)
mean_2015 <- filter(means, ano == 2015)
mean_2016 <- filter(means, ano == 2016)

  
# Our values are in dolar 2013 for GN, dolar 2015 for biomass and dolar 2016 for coal
# Creating a table with the monetary actualization

initial_cost_values <- c(1289,2002,5000) # dolar/ kw (GN2013, Biomass2015 and coal2016)

reais <- c((initial_cost_values[1] * mean_2013$avg),(initial_cost_values[2] * mean_2015$avg), 
           (initial_cost_values[3] * mean_2016$avg))

reais_2017 <- c(reais[1] / ipcax$NÃsMERO.Ã.NDICE..DEZ.93...100.[ipcax$ANO == 2013 & ipcax$MÃSS == "DEZ"] * ipcax$NÃsMERO.Ã.NDICE..DEZ.93...100.[ipcax$ANO == 2017 & ipcax$MÃSS == "ABR"],
                reais[2] / ipcax$NÃsMERO.Ã.NDICE..DEZ.93...100.[ipcax$ANO == 2015 & ipcax$MÃSS == "DEZ"] * ipcax$NÃsMERO.Ã.NDICE..DEZ.93...100.[ipcax$ANO == 2017 & ipcax$MÃSS == "ABR"],
                reais[3] / ipcax$NÃsMERO.Ã.NDICE..DEZ.93...100.[ipcax$ANO == 2016 & ipcax$MÃSS == "DEZ"] * ipcax$NÃsMERO.Ã.NDICE..DEZ.93...100.[ipcax$ANO == 2017 & ipcax$MÃSS == "ABR"])

reais_2017_MW <- reais_2017 * 1e3 # R$ april 2017 / MW :)

invest_cost_GN <-  reais_2017_MW[1] # defining the unitary costs for the sources
invest_cost_bio <- reais_2017_MW[2]
invest_cost_carvao <- reais_2017_MW[3] 

# Just to double check
#invest_cost_GN <-  3524326
#invest_cost_bio <-  7182819
#invest_cost_carvao <- 17617312 


#### Adding the investment costs ####
#investiment cost = Investment cost of the fuel * annualization factor
# Filtering the 17 nonexistent plants by fuel
usinas_NE <- filter(tab_CI_pot_com_CI, Situacao == "NE")
usinas_NE_GN <- filter(usinas_NE, TIPO_COMB. == "Gas")
usinas_NE_bio <- filter(usinas_NE, TIPO_COMB. == "Biomassa")
usinas_NE_carvao <- filter(usinas_NE, TIPO_COMB. == "Carvao")

# Calculating investment costs in millions of R$ / MW.
usinas_NE_GN[,7] <- invest_cost_GN * FA_GN/1e6
usinas_NE_bio[,7] <- invest_cost_bio * FA_bio/1e6
usinas_NE_carvao[,7] <-invest_cost_carvao * FA_car/1e6

#######################################################################################
# Acrescentando os valores de custo de investimento: 2 maneiras

# 1 na tabela com todas as informações necessárias para montar o arquivo de entrada do COPA
#indices_GN <- usinas_NE_GN$NUM
#seq_8_GN <- c(1:8) #'indices que correspondem `as usinas NE a GN na tabela usinas_NE_GN
#indices_bio <- usinas_NE_bio$NUM
#seq_8_bio <- c(1:8) #'indices que correspondem `as usinas NE a GN na tabela usinas_NE_bio
#indices_car <- usinas_NE_carvao$NUM
#seq_1_car <- c(1) #'indice que corresponde `a usina NE a carv~ao na tabela usinas_NE_carvao
#linhas_GN_NE  <- c(6,15,39,42,87,93,113,117)  # 'indices na tabela de 122 que representam as usinas a GN que n~ao existem
#linhas_bio_NE <- c(1,9,26,29,33,37,61,74)     # 'indices na tabela de 122 que representam as usinas a biomassa que n~ao existem
#linhas_car_NE <- c(59)                        # 'indice na tabela de 122 que representa a usina a carv~ao que n~ao existe
#usinas_CI_pot_final <- tab_CI_pot_com_CI
#usinas_CI_pot_final[linhas_GN_NE,7] <- usinas_NE_GN[seq_8_GN,7]
#usinas_CI_pot_final[linhas_bio_NE,7] <- usinas_NE_bio[seq_8_bio,7]
#usinas_CI_pot_final[linhas_car_NE,7] <- usinas_NE_carvao[seq_1_car,7]
## Por fim, falta acrescentar o subsistema de cada usina na tabela que ser'a usada para montar o arquivo para o COPA.
#sub_usinas <- select(usinas_agrupadas_por_subsistema, SSIS) 
#usinas_CI_pot_sub_final <- mutate(usinas_CI_pot_final, Subsistema = sub_usinas$SSIS)

#### Testando uma outra forma de acrescentar os custos de investimento ####
# 2 This form adds a new column to the table. This column contains the source and situation information together, in the form of a string
# This allows us to make a filter and find the positions to put the investment costs automatically.

# We add a new column to the table. This column contains the fuel and situation information together.
# It allow us to filter and find the positions to add the investment costs automatically
ic_test <- tab_CI_pot_com_CI
indice_duplo <- paste(ic_test$TIPO_COMB., ic_test$Situacao, sep = "." )
ic_test <- mutate(ic_test, ind = indice_duplo)
seq_8_GN <- c(1:8)  # index of the nonexistent plants and natural gas fuel in the table usinas_NE_GN
seq_8_bio <- c(1:8) # index of the nonexistent plants and biomass fuel in the table usinas_NE_bio
seq_1_car <- c(1)   # index of the nonexistent plants and coal fuel in the table usinas_NE_carvao

ic_test [which(ic_test$ind == "Gas.NE"),7] <- usinas_NE_GN[seq_8_GN,7]         # Adding NG investment cost 
ic_test [which(ic_test$ind == "Biomassa.NE"),7] <- usinas_NE_bio[seq_8_bio,7]  # Adding biomass investment cost
ic_test [which(ic_test$ind == "Carvao.NE"),7] <- usinas_NE_carvao[seq_1_car,7] # Adding coal investment cost

## Adding the subsystem of each plant which will be used to create the invest_opts file for COPA
sub_usinas <- select(usinas_agrupadas_por_subsistema, SSIS) 
usinas_CI_pot_sub_final_v1 <- mutate(ic_test, Subsistema = sub_usinas$SSIS)

# Saving the table with the required information to create the file that COPA is going to read.
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/Costs/Tables")
tabela_para_invest_opts_br_com_CI <- write.table(usinas_CI_pot_sub_final_v1, file = "tabela_para_invest_opts_br_com_CI.csv", sep = ";", row.names = FALSE)

# Saving as feather format
library(feather)
path <- usinas_CI_pot_sub_final_v1.feather
investment_costs <- write_feather(usinas_CI_pot_sub_final_v1, path)


