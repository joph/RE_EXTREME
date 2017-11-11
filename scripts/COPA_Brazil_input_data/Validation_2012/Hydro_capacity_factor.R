# 10/23/2017 (m/d/y)
# This script computes capcity factor of hydro total production and compares it 
# to BEN/EPE. 
# It calculates some hydro indicators as well, like sums of ONS and COPA inflows and 
# hydro generation.

# Loading results and getting total hydro production (sum of COPA hydro generation)
x_hydro_tot <- results %>% filter(name == "x_hydro_tot") %>% group_by(name) %>% summarise(totHydroProd = sum(value))

# Reading the hydro input file (maxHydPower)
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA/data/hydro")
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA/data/hydro")

maxHydPower <- read_delim("hydro_data_br_2012 - only 2012 capacity.csv", delim = ";", locale = locale(decimal_mark = ".")) %>% 
  summarise(maxHyd = sum(maxHydPower)*8760)

FC_hydro <- x_hydro_tot$totHydroProd / maxHydPower$maxHyd
print(FC_hydro)

# BEN https://ben.epe.gov.br/downloads/Relatorio_Final_BEN_2013.pdf
# capacidade instalada hidro 2012 (p.174)
# 84294 MW
# 415342 GWh
FC_hydro_epe <- 415342 / (84294*(8760/1e3))  
print(FC_hydro_epe)

#### Calculating hydro indicators ####
# sum of COPA inflows
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA/data/hydro")
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA/data/hydro")
#COPAInflows <- read_feather("br_shype_hydro_2012.feather") %>% summarise(COPAInf_GWh = sum(mwh) / 1e3) # run J3 - thermalok
COPAInflows <- read_feather("br_shype_hydro_2012_093_adaptFactor.feather") %>% summarise(COPAInf_GWh = sum(mwh) / 1e3) # run 093_adaptFactor
  
# sum of ONS generation
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/ONS validation")
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA  Initial Data/ONS validation")
namesNew <- c("SE.CO","SUL","NE","N")
gen_ons_final_se  <- read_csv(paste("geracao_ONS_",namesNew[1],".csv", sep=""))
gen_ons_final_sul <- read_csv(paste("geracao_ONS_",namesNew[2],".csv", sep=""))
gen_ons_final_ne  <- read_csv(paste("geracao_ONS_",namesNew[3],".csv", sep=""))
gen_ons_final_n   <- read_csv(paste("geracao_ONS_",namesNew[4],".csv", sep=""))
gen_ons_final_tot <- bind_rows(gen_ons_final_se,gen_ons_final_sul, gen_ons_final_ne, gen_ons_final_n)

hydro_ons_tot <- gen_ons_final_tot %>% group_by(Date, iTechnology) %>% 
  summarise(tot_gen = sum(production)) %>% filter(iTechnology == "hydro_ons")

ONSGen <- hydro_ons_tot %>% ungroup() %>% summarise(ONSGen_Gwh = sum(tot_gen))

# sum of ONS inflows
# reading ONS website 2012 storable ENAs (MWmedio)
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/ONS validation")
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA  Initial Data/ONS validation")
br_profile12 <- read_csv("Comparativo_Energia_Natural_Afluente_Subsistema_Dia_2012.csv") %>% select(1,2,8) 
colnames(br_profile12) <- c("date", "region", "mwh")
br_profile12$mwh <- br_profile12$mwh * 24

ONSInflows <- br_profile12 %>% summarise(ONSInf_GWh = sum(mwh) / 1e3)


#### Creating a table with generation and inflows for ONS and COPA ####
sums <- tibble(
  ONS = c(ONSGen$ONSGen_Gwh, ONSInflows$ONSInf_GWh),
  COPA = c(x_hydro_tot$totHydroProd / 1e3, COPAInflows$COPAInf_GWh),
  diffs_ONS_COPA = c((ONSGen$ONSGen_Gwh/(x_hydro_tot$totHydroProd / 1e3)), (ONSInflows$ONSInf_GWh / COPAInflows$COPAInf_GWh)))
row.names(sums) <- c("generation(GWh)", "inflows (GWh)")
print(sums) # run J3 - thermalok

sums1 <- tibble(
  ONS = c(ONSGen$ONSGen_Gwh, ONSInflows$ONSInf_GWh),
  COPA = c(x_hydro_tot$totHydroProd / 1e3, COPAInflows$COPAInf_GWh),
  diffs_ONS_COPA = c((ONSGen$ONSGen_Gwh/(x_hydro_tot$totHydroProd / 1e3)), (ONSInflows$ONSInf_GWh / COPAInflows$COPAInf_GWh)))
row.names(sums1) <- c("generation(GWh)", "inflows (GWh)")
print(sums1) # run 093_adaptFactor

#### Saving sums on .csv file
#setwd("C:/Users/cancella/Google Drive/!IIASA/COPA/runs/Validation_2012/tables")
#setwd(C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA/runs/Validation_2012/tables")
#write.csv2(sums,"sums_gen_inflows.csv", sep =";")
#write.csv(sums1,"sums_gen_inflows.csv", sep =";") #run 093_adaptFactor
