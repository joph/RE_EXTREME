# This script reads ONS website data related to generation on 2012 for each region and cleans it in order to get
# files to be read by "ONS_validation_data_2012_complete.R"

setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/ONS validation")
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA  Initial Data/ONS validation")
#gen_ons <- as_tibble(read.csv2("Comparativo_Geração_de_Energia_2012_SE.csv", header = T, sep = ";"))  # SE/CO
#gen_ons <- as_tibble(read.csv2("Comparativo_Geração_de_Energia_2012_S.csv", header = T, sep = ";"))  # SUL
#gen_ons <- as_tibble(read.csv2("Comparativo_Geração_de_Energia_2012_NE.csv", header = T, sep = ";")) # NE 
gen_ons <- as_tibble(read.csv2("Comparativo_Geração_de_Energia_2012_N.csv", header = T, sep = ";"))  # N

# Cleaning data
colnames(gen_ons) <- c("Date", "iTechnology","Fixed", "Begin", "Date2", "Period", "Week", "value", "production")
gen_ons <- gen_ons %>% select(Date, iTechnology,production)

# Removing rows with total generation and without technology 
gen_ons <- filter(gen_ons,Date != "")
gen_ons <- filter(gen_ons,iTechnology != "")

#namesNew<-c("SE/CO","SUL","NE","N") 
namesNew <- "N"
# adjusting names of technologies. We have 366 values of each technology
if (namesNew == "SE/CO"){
  gen_ons$iTechnology <- plyr::mapvalues(gen_ons$iTechnology, 
                                         from = c(levels(gen_ons$iTechnology)), to = c("a", "hydro", "nuclear", "thermal"))
} else {}

if (namesNew == "SUL" | namesNew == "N"){
  gen_ons$iTechnology <- plyr::mapvalues(gen_ons$iTechnology, 
                                         from = c(levels(gen_ons$iTechnology)), to = c("a","wind", "hydro", "thermal"))
} else {}

if (namesNew == "NE"){
  gen_ons$iTechnology <- plyr::mapvalues(gen_ons$iTechnology, 
                                         from = c(levels(gen_ons$iTechnology)), to = c("a", "wind", "hydro", "solar", "thermal"))
} else {}

# adjusting dates
gen_ons$Date <- as.Date(gen_ons$Date, "%d/%m/%Y %H:%M:%S")

# Aggregating thermal and nuclear
if(namesNew == "SE/CO"){
  gen_thermal <- gen_ons %>% filter(iTechnology == "thermal")
  gen_nuclear <- gen_ons %>% filter(iTechnology == "nuclear")
  gen_thermal_ons <- bind_rows(gen_thermal,gen_nuclear)
  gen_thermal_ons <- gen_thermal_ons %>% group_by(Date) %>% summarise(Thermal_generation = sum(production))
} else {
  gen_thermal_ons <- filter(gen_ons, iTechnology == "thermal") %>% select(Date, production)}

colnames(gen_thermal_ons) <- c("Date", "production")

# Getting ONs hydro data
gen_hydro_ons <- filter(gen_ons, iTechnology == "hydro") %>% select(Date, production)

# Getting ONS wind data
gen_wind_ons <- filter(gen_ons, iTechnology == "wind") %>% select(Date, production)

# Concatenating hydro, thermal and wind (when it is the case) # wind first is important to the if of checking wind production
gen_ons_final <- bind_rows(gen_wind_ons,gen_thermal_ons, gen_hydro_ons) %>% mutate(iTechnology = c(rep("wind_ons", nrow(gen_wind_ons)),
                                                                                                   rep("Thermal_ons",nrow(gen_thermal_ons)),
                                                                                                   rep("hydro_ons", nrow(gen_hydro_ons)))) %>% 
  select(Date,iTechnology, production)

# saving ONS data in .csv file
if(namesNew == "SE/CO"){
  namesNew <- "SE.CO"  
} else {}

path <- "C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/ONS validation"
#path <- "C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA  Initial Data/ONS validation"
path_f <- paste(path, "/geracao_ONS_", namesNew, ".csv",sep="")
save_gen_ons <- write_csv(gen_ons_final, path_f)
#genONSTest <- read_csv("geracao_ONS_SE.CO.csv") # reading it
                          


