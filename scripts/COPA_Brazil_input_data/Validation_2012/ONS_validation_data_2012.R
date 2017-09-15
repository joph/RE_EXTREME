# This script compares results of generation of electricity by source and region of ONS and COPA Brazil.
# Daily values of electricity generation from ONS
# Period: 7/1/2012 to 7/31/2012

#### subsystem: SE ####
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/ONS validation")
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA  Initial Data/ONS validation")
gen_ons_se_2012 <- as_tibble(read.csv2("Comparativo_Geração_de_Energia_2012_SE.csv", header = T, sep = ";"))

# Cleaning data
colnames(gen_ons_se_2012) <- c("Date", "iTechnology","Fixed", "Begin", "Date2", "Period", "Week", "value", "production")
gen_ons_se_2012 <- gen_ons_se_2012 %>% select(Date, iTechnology,production)

# Removing rows with total generation and without technology 
gen_ons_se_2012 <- filter(gen_ons_se_2012,Date != "")
gen_ons_se_2012 <- filter(gen_ons_se_2012,iTechnology != "")

# adjusting names of technologies. We have 366 values of each technology
gen_ons_se_2012$iTechnology <- plyr::mapvalues(gen_ons_se_2012$iTechnology, 
                                               from = c(levels(gen_ons_se_2012$iTechnology)), to = c("a", "hydro", "nuclear", "thermal"))
# adjusting dates
gen_ons_se_2012$Date <- as.Date(gen_ons_se_2012$Date, "%d/%m/%Y %H:%M:%S")
  #strptime(gen_ons_se_2012$Date, format = "%d/%m/%Y %H:%M:%S", tz = "CET")

# Aggregating thermal and nuclear
gen_se_thermal_12 <- gen_ons_se_2012 %>% filter(iTechnology == "thermal")
gen_se_nuclear_12 <- gen_ons_se_2012 %>% filter(iTechnology == "nuclear")
gen_se_thermal_ons_12 <- bind_rows(gen_se_thermal_12,gen_se_nuclear_12)
gen_se_thermal_ons_12 <- gen_se_thermal_ons_12 %>% group_by(Date) %>% summarise(Thermal_generation = sum(production))
colnames(gen_se_thermal_ons_12) <- c("Date", "production")

# Getting ONs hydro data
gen_hydro_ons_12 <- filter(gen_ons_se_2012, iTechnology == "hydro") %>% select(Date, production)

# Concatenating hydro and thermal
gen_se_final_ons_12 <- bind_rows(gen_hydro_ons_12, gen_se_thermal_ons_12) %>% mutate(iTechnology = c(rep("hydro_ons",nrow(gen_hydro_ons_12)),rep("Thermal_ons",nrow(gen_se_thermal_12))))

#### COPA values - dayly basis ####
# Passing to daily basis
hydro_COPA_se_12 <- results %>% filter(name == "x_hydro" & reg == "SE001" )
hydro_COPA_se_12 <- select(hydro_COPA_se_12,datetime, value)
hydro_COPA_se_12 <- hydro_COPA_se_12  %>% group_by(day(datetime), month(datetime)) %>% summarise(sum = sum(value))

term_COPA_se_12 <- results %>% filter(name == "x_term" & reg == "SE001" )
term_COPA_se_12 <- select(term_COPA_se_12,datetime, value)
term_COPA_se_12 <- term_COPA_se_12  %>% group_by(day(datetime), month(datetime)) %>% summarise(sum = sum(value))

# Concatenating COPA's hydro and thermal generation 
gen_se_copa_12 <- bind_rows(hydro_COPA_se_12, term_COPA_se_12)
gen_se_copa_12$iTechnology <- c(rep("hydro_copa",nrow(hydro_COPA_se_12)),rep("Thermal_copa",nrow(term_COPA_se_12)))

gen_se_copa_12 <- gen_se_copa_12 %>% ungroup() %>% mutate(Date = gen_se_final_ons_12$Date) %>% 
  select(Date, sum, iTechnology)
colnames(gen_se_copa_12) <- colnames(gen_se_final_ons_12)

#### Comparation between ONS and COPA #### SE ####
# Units -> everything in GWh
# passing copa values to GWh
gen_se_copa_12$production <- gen_se_copa_12$production / 1e3
gen_se_12 <- bind_rows(gen_se_final_ons_12, gen_se_copa_12)

# Plotting results
ggplot() + 
  geom_line(data = gen_se_12, aes(x = Date, y = production, col = iTechnology ), size = 1) +
  scale_fill_brewer(palette="Set2") + facet_wrap(~iTechnology) +
  ylab("GWh") + xlab("") +  ggtitle("Comparing generation - SE" ) + 
  theme(plot.title = element_text(hjust = 0.5))

# Plotting time-series in points to see the differences
  # hydro
points_hydro_ons  <- gen_se_12 %>% filter(iTechnology == "hydro_ons")
points_hydro_copa <- gen_se_12 %>% filter(iTechnology == "hydro_copa")
points_hydro_se_12 <- bind_rows(points_hydro_ons,points_hydro_copa)

ggplot(data = points_hydro_se_12, aes(x=Date,y=production)) + geom_point(aes(col=iTechnology)) +
  ylab("ONS x COPA") + xlab("") +  ggtitle("Hydro - SE" ) + 
  theme(plot.title = element_text(hjust = 0.5))

points_hydro <- tibble(
  ons =   c(gen_se_12$production[gen_se_12$iTechnology == "hydro_ons"]), 
  copa =  c(gen_se_12$production[gen_se_12$iTechnology == "hydro_copa"]))
ggplot(data = points_hydro, aes(x = ons, y = copa)) + geom_point() +
  ylab("ONS") + xlab("COPA") + ggtitle("ONS x COPA - hydro - SE") + theme(plot.title = element_text(hjust = 0.5))

  # thermal
points_thermal_ons  <- gen_se_12 %>% filter(iTechnology == "Thermal_ons")
points_thermal_copa <- gen_se_12 %>% filter(iTechnology == "Thermal_copa")
points_thermal_se_12 <- bind_rows(points_thermal_ons,points_thermal_copa)
ggplot(data = points_thermal_se_12, aes(x=Date,y=production)) + geom_point(aes(col=iTechnology)) +
  ylab("ONS x COPA") + xlab("") +  ggtitle("Thermal - SE" ) + 
  theme(plot.title = element_text(hjust = 0.5))

points_thermal <- tibble(
  ons =   c(gen_se_12$production[gen_se_12$iTechnology == "Thermal_ons"]), 
  copa =  c(gen_se_12$production[gen_se_12$iTechnology == "Thermal_copa"]))
ggplot(data = points_thermal, aes(x = ons, y = copa)) + geom_point() +
  ylab("ONS") + xlab("COPA") + ggtitle("ONS x COPA - thermal - SE") + theme(plot.title = element_text(hjust = 0.5))

# Correlation between time-series of ONS x COPA
correlation_hydro_12 <- as_tibble(cor(gen_se_12$production[gen_se_12$iTechnology == "hydro_ons"],
                                      gen_se_12$production[gen_se_12$iTechnology == "hydro_copa"]))
correlation_thermal_12 <- as_tibble(cor(gen_se_12$production[gen_se_12$iTechnology == "Thermal_ons"],
                                        gen_se_12$production[gen_se_12$iTechnology == "Thermal_copa"]))
# RMSE
reg <- lm(copa ~ ons , data = points_hydro)
rmse_hydro <- modelr::rmse(reg, points_hydro)

reg_thermal <- lm(copa ~ ons , data = points_thermal)
rmse_thermal <- modelr::rmse(reg, points_thermal)

stats_summary <- tibble(
  statistic = c("hydro", "thermal"),
  correlation = c(correlation_hydro_12$value, correlation_thermal_12$value),
  RMSE = c(rmse_hydro, rmse_thermal)
)

#rmse_hydro <- modelr::rmse(gen_se_12$production[gen_se_12$iTechnology == "hydro_copa"],
              #               gen_se_12$production[gen_se_12$iTechnology == "hydro_ons"]) 

#### Subsystem: NE ####
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/ONS validation")
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA  Initial Data/ONS validation")
gen_ne_ons <- as_tibble(read.csv2("Comparativo_Geração_de_Energia_Jul_2012_NE.csv", header = T, sep = ","))

# Cleaning data
gen_ne_ons <- gen_ne_ons[1:97,]
gen_ne_ons <- bind_rows(gen_ne_ons[2:32,],gen_ne_ons[35:65,],gen_ne_ons[67:97,])
gen_ne_ons <- gen_ne_ons[,c(5,7,9)]
#days <- as_tibble(as.numeric(format(results$datetime, "%d")))
gen_ne_ons_i <- gen_ne_ons
gen_ne_ons_i[sapply(gen_ne_ons_i, is.factor)] <- lapply(gen_ne_ons_i[sapply(gen_ne_ons_i, is.factor)], 
                                                              function(x) as.numeric(as.character(x)))  

gen_ne_ons$Selecione.Tipo.de.GE.Comp.3 <- gen_ne_ons_i$Selecione.Tipo.de.GE.Comp.3
colnames(gen_ne_ons) <- c("Date","iTechnology","value")

# Adding column "Technology"
gen_ne_ons$iTechnology <- c(rep("Thermal",31), rep("hydro", 31), rep("Wind", 31))
ggplot(data = gen_ne_ons) + 
  geom_line(aes(x = as.numeric(Date), y = value, col = iTechnology), size = 1)

#### COPA values - hourly basis ####
# Passing to daily basis # values in MWh
hydro_COPA_ne <- results %>% filter(name == "x_hydro" & reg == "SE003" )
hydro_COPA_ne <- select(hydro_COPA_ne,datetime, value)
hydro_COPA_ne <- hydro_COPA_ne  %>% group_by(day(datetime)) %>% summarise(sum = sum(value))

term_COPA_ne <- results %>% filter(name == "x_term" & reg == "SE003")
term_COPA_ne <- select(term_COPA_ne,datetime, value)
term_COPA_ne <- term_COPA_ne  %>% group_by(day(datetime)) %>% summarise(sum = sum(value))

wind_COPA_ne <- results %>% filter(name == "x_renew" & reg == "SE003" & iTechnology == "Wind")
wind_COPA_ne <- select(wind_COPA_ne,datetime, value)
wind_COPA_ne <- wind_COPA_ne  %>% group_by(day(datetime)) %>% summarise(sum = sum(value))


# Concatenating COPA's hydro, thermal and wind generation 
gen_ne_copa <- bind_rows(hydro_COPA_ne, term_COPA_ne, wind_COPA_ne)
gen_ne_copa$iTechnology <- c(rep("hydro_copa", 31), rep("thermal_copa", 31),rep("Wind_copa", 31))
colnames(gen_ne_copa) <- c("Date","value","iTechnology")

#### Comparation between ONS and COPA ####
gen_ne_ons$Date <- gen_ne_copa$Date

# Units -> everything in GWh
# passing copa values to GWh
gen_ne_copa$value <- gen_ne_copa$value / 1e3
gen_ne <- bind_rows(gen_ne_ons, gen_ne_copa)

# Organizing the order of data
gen_ne_ons_hydro <- filter(gen_ne, iTechnology == "hydro")
gen_ne_ons_thermal <- filter(gen_ne, iTechnology == "Thermal")
gen_ne_ons_wind <- filter(gen_ne, iTechnology == "Wind")
gen_ne_copa_hydro <- filter(gen_ne, iTechnology == "hydro_copa") 
gen_ne_copa_thermal <- filter(gen_ne, iTechnology == "thermal_copa")
gen_ne_copa_wind <- filter(gen_ne, iTechnology == "Wind_copa")
gen_ne_final <- bind_rows(gen_ne_ons_hydro,gen_ne_ons_thermal,gen_ne_ons_wind,gen_ne_copa_hydro,gen_ne_copa_thermal,gen_ne_copa_wind)

# Plotting results
ggplot() + 
  geom_bar(data = gen_ne_final, aes(x = Date, y = value, fill = iTechnology), stat = "identity", position = "dodge") +
  scale_fill_brewer(palette="Set2") + facet_wrap(~iTechnology, ncol = 2) +
  ylab("GWh") + xlab("Julho 2012") +  ggtitle("Comparing generation - NE" ) + 
  theme(plot.title = element_text(hjust = 0.5))

#### Subsystem: S ####
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/ONS validation")
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA  Initial Data/ONS validation")
gen_s_ons <- as_tibble(read.csv2("Comparativo_Geração_de_Energia_Jul_2012_S.csv", header = T, sep = ","))

# Cleaning data
gen_s_ons <- gen_s_ons[1:96,]
gen_s_ons <- bind_rows(gen_s_ons[2:32,],gen_s_ons[34:64,],gen_s_ons[66:96,])
gen_s_ons <- gen_s_ons[,c(5,7,9)]
#days <- as_tibble(as.numeric(format(results$datetime, "%d")))
gen_s_ons_i <- gen_s_ons
gen_s_ons_i[sapply(gen_s_ons_i, is.factor)] <- lapply(gen_s_ons_i[sapply(gen_s_ons_i, is.factor)], 
                                                        function(x) as.numeric(as.character(x)))  

gen_s_ons$Selecione.Tipo.de.GE.Comp.3 <- gen_s_ons_i$Selecione.Tipo.de.GE.Comp.3
colnames(gen_s_ons) <- c("Date","iTechnology","value")

# Adding column "Technology"
gen_s_ons$iTechnology <- c(rep("Thermal",31), rep("hydro", 31), rep("Wind", 31))
ggplot(data = gen_s_ons) + 
  geom_line(aes(x = as.numeric(Date), y = value, col = iTechnology), size = 1)

#### COPA values - hourly basis ####
# Passing to daily basis # values in MWh
hydro_COPA_s <- results %>% filter(name == "x_hydro" & reg == "SE002" )
hydro_COPA_s <- select(hydro_COPA_s,datetime, value)
hydro_COPA_s <- hydro_COPA_s  %>% group_by(day(datetime)) %>% summarise(sum = sum(value))

term_COPA_s <- results %>% filter(name == "x_term" & reg == "SE002")
term_COPA_s <- select(term_COPA_s,datetime, value)
term_COPA_s <- term_COPA_s  %>% group_by(day(datetime)) %>% summarise(sum = sum(value))

wind_COPA_s <- results %>% filter(name == "x_renew" & reg == "SE002" & iTechnology == "Wind")
wind_COPA_s <- select(wind_COPA_s,datetime, value)
wind_COPA_s <- wind_COPA_s  %>% group_by(day(datetime)) %>% summarise(sum = sum(value))


# Concatenating COPA's hydro, thermal and wind generation 
gen_s_copa <- bind_rows(hydro_COPA_s, term_COPA_s, wind_COPA_s)
gen_s_copa$iTechnology <- c(rep("hydro_copa", 31), rep("thermal_copa", 31),rep("Wind_copa", 31))
colnames(gen_s_copa) <- c("Date","value","iTechnology")

# organizing columns
gen_s_copa <- select(gen_s_copa, Date, iTechnology, value)

#### Comparation between ONS and COPA ####
gen_s_ons$Date <- gen_s_copa$Date

# Units -> everything in GWh
# passing copa values to GWh
gen_s_copa$value <- gen_s_copa$value / 1e3
gen_s <- bind_rows(gen_s_ons, gen_s_copa)

# Organizing the order of data
gen_s_ons_hydro <- filter(gen_s, iTechnology == "hydro")
gen_s_ons_thermal <- filter(gen_s, iTechnology == "Thermal")
gen_s_ons_wind <- filter(gen_s, iTechnology == "Wind")
gen_s_copa_hydro <- filter(gen_s, iTechnology == "hydro_copa") 
gen_s_copa_thermal <- filter(gen_s, iTechnology == "thermal_copa")
gen_s_copa_wind <- filter(gen_s, iTechnology == "Wind_copa")
gen_s_final <- bind_rows(gen_s_ons_hydro,gen_s_ons_thermal,gen_s_ons_wind,gen_s_copa_hydro,gen_s_copa_thermal,gen_s_copa_wind)

# Plotting results
ggplot() + 
  geom_bar(data = gen_s_final, aes(x = Date, y = value, fill = iTechnology), stat = "identity", position = "dodge") +
  scale_fill_brewer(palette="Set2") + facet_wrap(~iTechnology, ncol = 2) +
  ylab("GWh") + xlab("Julho 2012") +  ggtitle("Comparing generation - S" ) + 
  theme(plot.title = element_text(hjust = 0.5))

#### Subsystem: N ####
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/ONS validation")
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA  Initial Data/ONS validation")
gen_n_ons <- as_tibble(read.csv2("Comparativo_Geração_de_Energia_Jul_2012_N.csv", header = T, sep = ","))

# Cleaning data
gen_n_ons <- gen_n_ons[1:64,]
gen_n_ons <- bind_rows(gen_n_ons[2:32,],gen_n_ons[34:64,])
gen_n_ons <- gen_n_ons[,c(5,7,9)]
#days <- as_tibble(as.numeric(format(results$datetime, "%d")))
gen_n_ons_i <- gen_n_ons
gen_n_ons_i[sapply(gen_n_ons_i, is.factor)] <- lapply(gen_n_ons_i[sapply(gen_n_ons_i, is.factor)], 
                                                      function(x) as.numeric(as.character(x)))  

gen_n_ons$Selecione.Tipo.de.GE.Comp.3 <- gen_n_ons_i$Selecione.Tipo.de.GE.Comp.3
colnames(gen_n_ons) <- c("Date","iTechnology","value")

# Adding column "Technology"
gen_n_ons$iTechnology <- c(rep("Thermal",31), rep("hydro", 31))
ggplot(data = gen_n_ons) + 
  geom_line(aes(x = as.numeric(Date), y = value, col = iTechnology), size = 1)

#### COPA values - hourly basis ####
# Passing to daily basis # values in MWh
hydro_COPA_n <- results %>% filter(name == "x_hydro" & reg == "SE004" )
hydro_COPA_n <- select(hydro_COPA_n,datetime, value)
hydro_COPA_n <- hydro_COPA_n  %>% group_by(day(datetime)) %>% summarise(sum = sum(value))

term_COPA_n <- results %>% filter(name == "x_term" & reg == "SE004")
term_COPA_n <- select(term_COPA_n,datetime, value)
term_COPA_n <- term_COPA_n  %>% group_by(day(datetime)) %>% summarise(sum = sum(value))

# Concatenating COPA's hydro, thermal and wind generation 
gen_n_copa <- bind_rows(hydro_COPA_n, term_COPA_n)
gen_n_copa$iTechnology <- c(rep("hydro_copa", 31), rep("thermal_copa", 31))
colnames(gen_n_copa) <- c("Date","value","iTechnology")

# organizing columns
gen_n_copa <- select(gen_n_copa, Date, iTechnology, value)

#### Comparation between ONS and COPA ####
gen_n_ons$Date <- gen_n_copa$Date

# Units -> everything in GWh
# passing copa values to GWh
gen_n_copa$value <- gen_n_copa$value / 1e3
gen_n <- bind_rows(gen_n_ons, gen_n_copa)

# Organizing the order of data
gen_n_ons_hydro <- filter(gen_n, iTechnology == "hydro")
gen_n_ons_thermal <- filter(gen_n, iTechnology == "Thermal")
gen_n_copa_hydro <- filter(gen_n, iTechnology == "hydro_copa") 
gen_n_copa_thermal <- filter(gen_n, iTechnology == "thermal_copa")
gen_n_final <- bind_rows(gen_n_ons_hydro,gen_n_ons_thermal, gen_n_copa_hydro,gen_n_copa_thermal)

# Plotting results
ggplot() + 
  geom_bar(data = gen_n_final, aes(x = Date, y = value, fill = iTechnology), stat = "identity", position = "dodge") +
  scale_fill_brewer(palette="Set2") + facet_wrap(~iTechnology, ncol = 2) +
  ylab("GWh") + xlab("Julho 2012") +  ggtitle("Comparing generation - N" ) + 
  theme(plot.title = element_text(hjust = 0.5))
