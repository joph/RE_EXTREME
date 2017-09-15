# This script compares results of generation of electricity by source and region of ONS and COPA Brazil.
# Daily values of electricity generation from ONS
# Period: 7/1/2012 to 7/31/2012

#### subsystem: NE ####
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/ONS validation")
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA  Initial Data/ONS validation")
gen_ons <- as_tibble(read.csv2("Comparativo_Geração_de_Energia_2012_NE.csv", header = T, sep = ";"))

# Cleaning data
colnames(gen_ons) <- c("Date", "iTechnology","Fixed", "Begin", "Date2", "Period", "Week", "value", "production")
gen_ons <- gen_ons %>% select(Date, iTechnology,production)

# Removing rows with total generation and without technology 
gen_ons <- filter(gen_ons,Date != "")
gen_ons <- filter(gen_ons,iTechnology != "")

# adjusting names of technologies. We have 366 values of each technology
gen_ons$iTechnology <- plyr::mapvalues(gen_ons$iTechnology, 
                                       from = c(levels(gen_ons$iTechnology)), to = c("a", "wind", "hydro", "solar", "thermal"))
# adjusting dates
gen_ons$Date <- as.Date(gen_ons$Date, "%d/%m/%Y %H:%M:%S")
#strptime(gen_ons_se_2012$Date, format = "%d/%m/%Y %H:%M:%S", tz = "CET")

# Getting ONs hydro data
gen_hydro_ons <- filter(gen_ons, iTechnology == "hydro") %>% select(Date, production)

# Getting ONs thermal data
gen_thermal_ons <- filter(gen_ons, iTechnology == "thermal") %>% select(Date, production)

# Getting ONs wind data
gen_wind_ons <- filter(gen_ons, iTechnology == "wind") %>% select(Date, production)

# Concatenating hydro and thermal
gen_ons_final <- bind_rows(gen_hydro_ons, gen_thermal_ons, gen_wind_ons) %>% mutate(iTechnology = c(rep("hydro_ons",nrow(gen_hydro_ons)),
                                                                                                    rep("Thermal_ons",nrow(gen_thermal_ons)),
                                                                                                    rep("wind_ons", nrow(gen_wind_ons))))

#### COPA values - dayly basis ####
# Passing to daily basis
hydro_COPA <- results %>% filter(name == "x_hydro" & reg == "SE003" )
hydro_COPA <- select(hydro_COPA,datetime, value)
hydro_COPA <- hydro_COPA  %>% group_by(day(datetime), month(datetime)) %>% summarise(sum = sum(value))

term_COPA <- results %>% filter(name == "x_term" & reg == "SE003" )
term_COPA <- select(term_COPA,datetime, value)
term_COPA <- term_COPA  %>% group_by(day(datetime), month(datetime)) %>% summarise(sum = sum(value))

wind_COPA <- results %>% filter(name == "x_renew" & reg == "SE003" & iTechnology == "Wind")
wind_COPA <- select(wind_COPA,datetime, value)
wind_COPA <- wind_COPA  %>% group_by(day(datetime), month(datetime)) %>% summarise(sum = sum(value))

# Concatenating COPA's hydro and thermal generation 
gen_copa <- bind_rows(hydro_COPA, term_COPA, wind_COPA)
gen_copa$iTechnology <- c(rep("hydro_copa",nrow(hydro_COPA)),
                          rep("Thermal_copa",nrow(term_COPA)),
                          rep("wind_copa", nrow(wind_COPA)))

gen_copa_final <- gen_copa %>% ungroup() %>% mutate(Date = gen_ons_final$Date) %>% 
  select(Date, sum, iTechnology)
colnames(gen_copa_final) <- colnames(gen_ons_final)

#### Comparation between ONS and COPA #### NE ####
# Units -> everything in GWh
# passing copa values to GWh
gen_copa_final$production <- gen_copa_final$production / 1e3
gen_ne_12 <- bind_rows(gen_ons_final, gen_copa_final)

comparing_generation_ne <- ggplot() + 
  geom_line(data = gen_ne_12, aes(x = Date, y = production, col = iTechnology ), size = 1) +
  scale_fill_brewer(palette="Set2") + facet_wrap(~iTechnology, ncol = 2) +
  ylab("GWh") + xlab("") +  ggtitle("Comparing generation - NE" ) + 
  theme(plot.title = element_text(hjust = 0.5))
plot(comparing_generation_ne)
#ggsave("C:/Users/cancella/Google Drive/!IIASA/COPA/runs/Validation_2012/figures/NE",comparing_generation_ne,width=30,height=20,units="cm")

# preparing data for two more plots
gen_ne_12_area <- gen_ne_12
gen_ne_12_area <- gen_ne_12_area %>% mutate(type = c(rep("ons",nrow((gen_ne_12))/2), rep("copa", nrow(gen_ne_12)/2)))
gen_ne_12_area$iTechnology[gen_ne_12_area$iTechnology == "hydro_ons"]  <- "hydro"
gen_ne_12_area$iTechnology[gen_ne_12_area$iTechnology == "hydro_copa"]  <- "hydro"
gen_ne_12_area$iTechnology[gen_ne_12_area$iTechnology == "Thermal_ons"]  <- "thermal"
gen_ne_12_area$iTechnology[gen_ne_12_area$iTechnology == "Thermal_copa"]  <- "thermal"
gen_ne_12_area$iTechnology[gen_ne_12_area$iTechnology == "wind_ons"]  <- "wind"
gen_ne_12_area$iTechnology[gen_ne_12_area$iTechnology == "wind_copa"]  <- "wind"

# lines together
lines_together <- ggplot() + 
  geom_line(data = gen_ne_12_area, aes(x = Date, y = production, col = type), size = 1) +
  scale_fill_brewer(palette="Set2") + facet_wrap(~iTechnology) +
  ylab("GWh") + xlab("") +  ggtitle("Comparing generation_tog - NE" ) + 
  theme(plot.title = element_text(hjust = 0.5))
plot(lines_together)
#ggsave("C:/Users/cancella/Google Drive/!IIASA/COPA/runs/Validation_2012/figures/NE",lines_together,width=30,height=20,units="cm")

# area graph
comparing_generation_ne_area <- ggplot(data = gen_ne_12_area, aes(x = Date, y = production, fill = type)) + 
  geom_area() +
  scale_fill_brewer(palette="Set2") + facet_wrap(~iTechnology) + 
  ylab("GWh") + xlab("") +  ggtitle("Comparing generation_area - NE" ) + 
  theme(plot.title = element_text(hjust = 0.5))
plot(comparing_generation_ne_area)
#ggsave("C:/Users/cancella/Google Drive/!IIASA/COPA/runs/Validation_2012/figures",comparing_generation_ne_area,width=30,height=20,units="cm")

# Plotting time-series in points to see the differences
# hydro
points_hydro_ons  <- gen_ne_12 %>% filter(iTechnology == "hydro_ons")
points_hydro_copa <- gen_ne_12 %>% filter(iTechnology == "hydro_copa")
points_hydro_ne_12 <- bind_rows(points_hydro_ons,points_hydro_copa)

points_hydro_date_ne <- ggplot(data = points_hydro_ne_12, aes(x=Date,y=production)) + geom_point(aes(col=iTechnology)) +
  ylab("ONS x COPA") + xlab("") +  ggtitle("Hydro - NE" ) + 
  theme(plot.title = element_text(hjust = 0.5))
plot(points_hydro_date_ne)
#ggsave("C:/Users/cancella/Google Drive/!IIASA/COPA/runs/Validation_2012/figures",points_hydro_date_se,width=30,height=20,units="cm")

points_hydro <- tibble(
  ons =   c(gen_ne_12$production[gen_ne_12$iTechnology == "hydro_ons"]), 
  copa =  c(gen_ne_12$production[gen_ne_12$iTechnology == "hydro_copa"]))
plot_points_hydro_ne <- ggplot(data = points_hydro, aes(x = ons, y = copa)) + geom_point() +
  ylab("ONS") + xlab("COPA") + ggtitle("ONS x COPA - hydro - NE") + theme(plot.title = element_text(hjust = 0.5))
plot(plot_points_hydro_ne)
#ggsave("C:/Users/cancella/Google Drive/!IIASA/COPA/runs/Validation_2012/figures",plot_points_hydro_se,width=30,height=20,units="cm")

# thermal
points_thermal_ons  <- gen_ne_12 %>% filter(iTechnology == "Thermal_ons")
points_thermal_copa <- gen_ne_12 %>% filter(iTechnology == "Thermal_copa")
points_thermal_ne_12 <- bind_rows(points_thermal_ons,points_thermal_copa)
points_thermal_date_ne <- ggplot(data = points_thermal_ne_12, aes(x=Date,y=production)) + geom_point(aes(col=iTechnology)) +
  ylab("ONS x COPA") + xlab("") +  ggtitle("Thermal - NE" ) + 
  theme(plot.title = element_text(hjust = 0.5))
plot(points_thermal_date_ne)
#ggsave("C:/Users/cancella/Google Drive/!IIASA/COPA/runs/Validation_2012/figures",points_thermal_date_se,width=30,height=20,units="cm")

points_thermal <- tibble(
  ons =   c(gen_ne_12$production[gen_ne_12$iTechnology == "Thermal_ons"]), 
  copa =  c(gen_ne_12$production[gen_ne_12$iTechnology == "Thermal_copa"]))
points_thermal_ne <- ggplot(data = points_thermal, aes(x = ons, y = copa)) + geom_point() +
  ylab("ONS") + xlab("COPA") + ggtitle("ONS x COPA - thermal - NE") + theme(plot.title = element_text(hjust = 0.5))
plot(points_thermal_ne)
#ggsave("C:/Users/cancella/Google Drive/!IIASA/COPA/runs/Validation_2012/figures",points_thermal_se,width=30,height=20,units="cm")

# wind
points_wind_ons  <- gen_ne_12 %>% filter(iTechnology == "wind_ons")
points_wind_copa <- gen_ne_12 %>% filter(iTechnology == "wind_copa")
points_wind_ne_12 <- bind_rows(points_wind_ons,points_wind_copa)
points_wind_date_ne <- ggplot(data = points_wind_ne_12, aes(x=Date,y=production)) + geom_point(aes(col=iTechnology)) +
  ylab("ONS x COPA") + xlab("") +  ggtitle("wind - NE" ) + 
  theme(plot.title = element_text(hjust = 0.5))
plot(points_wind_date_ne)
#ggsave("C:/Users/cancella/Google Drive/!IIASA/COPA/runs/Validation_2012/figures",points_thermal_date_se,width=30,height=20,units="cm")

points_wind <- tibble(
  ons =   c(gen_ne_12$production[gen_ne_12$iTechnology == "wind_ons"]), 
  copa =  c(gen_ne_12$production[gen_ne_12$iTechnology == "wind_copa"]))
points_wind_ne <- ggplot(data = points_wind, aes(x = ons, y = copa)) + geom_point() +
  ylab("ONS") + xlab("COPA") + ggtitle("ONS x COPA - wind - NE") + theme(plot.title = element_text(hjust = 0.5))
plot(points_wind_ne)
#ggsave("C:/Users/cancella/Google Drive/!IIASA/COPA/runs/Validation_2012/figures",points_thermal_se,width=30,height=20,units="cm")
# Correlation between time-series of ONS x COPA
correlation_hydro_12 <- as_tibble(cor(gen_ne_12$production[gen_ne_12$iTechnology == "hydro_ons"],
                                      gen_ne_12$production[gen_ne_12$iTechnology == "hydro_copa"]))
correlation_thermal_12 <- as_tibble(cor(gen_ne_12$production[gen_ne_12$iTechnology == "Thermal_ons"],
                                        gen_ne_12$production[gen_ne_12$iTechnology == "Thermal_copa"]))

correlation_wind_12 <- as_tibble(cor(gen_ne_12$production[gen_ne_12$iTechnology == "wind_ons"],
                                        gen_ne_12$production[gen_ne_12$iTechnology == "wind_copa"]))

# RMSE
reg <- lm(copa ~ ons , data = points_hydro)
rmse_hydro <- modelr::rmse(reg, points_hydro)

reg_thermal <- lm(copa ~ ons , data = points_thermal)
rmse_thermal <- modelr::rmse(reg, points_thermal)

reg_wind <- lm(copa ~ ons , data = points_wind)
rmse_wind <- modelr::rmse(reg, points_wind)

stats_summary_ne <- tibble(
  statistic = c("hydro", "thermal", "wind"),
  correlation = c(correlation_hydro_12$value, correlation_thermal_12$value, correlation_wind_12$value),
  RMSE = c(rmse_hydro, rmse_thermal, rmse_wind)
)
stats_summary_ne
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/ONS validation")
write_feather(stats_summary_ne,"stats_summary_ne.feather")

#rmse_hydro <- modelr::rmse(gen_se_12$production[gen_se_12$iTechnology == "hydro_copa"],
#               gen_se_12$production[gen_se_12$iTechnology == "hydro_ons"])