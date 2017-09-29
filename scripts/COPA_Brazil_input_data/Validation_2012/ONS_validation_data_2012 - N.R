# This script compares results of generation of electricity by source and region of ONS and COPA Brazil.
# Daily values of electricity generation from ONS
# Period: 7/1/2012 to 7/31/2012

#### subsystem: N ####
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/ONS validation")
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA  Initial Data/ONS validation")
gen_ons <- as_tibble(read.csv2("Comparativo_Geração_de_Energia_2012_N.csv", header = T, sep = ";"))

# Cleaning data
colnames(gen_ons) <- c("Date", "iTechnology","Fixed", "Begin", "Date2", "Period", "Week", "value", "production")
gen_ons <- gen_ons %>% select(Date, iTechnology,production)

# Removing rows with total generation and without technology 
gen_ons <- filter(gen_ons,Date != "")
gen_ons <- filter(gen_ons,iTechnology != "")

# adjusting names of technologies. We have 366 values of each technology
gen_ons$iTechnology <- plyr::mapvalues(gen_ons$iTechnology, 
                                       from = c(levels(gen_ons$iTechnology)), to = c("a", "wind", "hydro", "thermal"))
# adjusting dates
gen_ons$Date <- as.Date(gen_ons$Date, "%d/%m/%Y %H:%M:%S")
#strptime(gen_ons_se_2012$Date, format = "%d/%m/%Y %H:%M:%S", tz = "CET")

# Getting ONs hydro data
gen_hydro_ons <- filter(gen_ons, iTechnology == "hydro") %>% select(Date, production)

# Getting ONs thermal data
gen_thermal_ons <- filter(gen_ons, iTechnology == "thermal") %>% select(Date, production)

# Concatenating hydro and thermal
gen_ons_final <- bind_rows(gen_hydro_ons, gen_thermal_ons) %>% mutate(iTechnology = c(rep("hydro_ons",nrow(gen_hydro_ons)),rep("Thermal_ons",nrow(gen_thermal_ons))))

#### COPA values - dayly basis ####
# Passing to daily basis
hydro_COPA <- results %>% filter(name == "x_hydro_tot" & reg == "N")
hydro_COPA <- select(hydro_COPA,datetime, value)
hydro_COPA <- hydro_COPA  %>% group_by(month(datetime), day(datetime)) %>% summarise(sum = sum(value))

term_COPA <- results %>% filter(name == "x_term" & reg == "N")
term_COPA <- select(term_COPA,datetime, value)
term_COPA <- term_COPA  %>% group_by(month(datetime), day(datetime)) %>% summarise(sum = sum(value))

# Concatenating COPA's hydro and thermal generation 
gen_copa <- bind_rows(hydro_COPA, term_COPA)
gen_copa$iTechnology <- c(rep("hydro_copa",nrow(hydro_COPA)),rep("Thermal_copa",nrow(term_COPA)))

gen_copa_final <- gen_copa %>% ungroup() %>% mutate(Date = gen_ons_final$Date) %>% 
  select(Date, sum, iTechnology)
colnames(gen_copa_final) <- colnames(gen_ons_final)

#### Comparation between ONS and COPA 
# Units -> everything in GWh
# passing copa values to GWh
gen_copa_final$production <- gen_copa_final$production / 1e3
gen_n_12 <- bind_rows(gen_ons_final, gen_copa_final)

# preparing data for plots
gen_n_12_area <- gen_n_12
gen_n_12_area <- gen_n_12_area %>% mutate(type = c(rep("ons",nrow((gen_n_12))/2), rep("copa", nrow(gen_n_12)/2)))
gen_n_12_area$iTechnology[gen_n_12_area$iTechnology == "hydro_ons"]  <- "hydro"
gen_n_12_area$iTechnology[gen_n_12_area$iTechnology == "hydro_copa"]  <- "hydro"
gen_n_12_area$iTechnology[gen_n_12_area$iTechnology == "Thermal_ons"]  <- "thermal"
gen_n_12_area$iTechnology[gen_n_12_area$iTechnology == "Thermal_copa"]  <- "thermal"
#gen_n_12_area$iTechnology[gen_n_12_area$iTechnology == "wind_ons"]  <- "wind"
#gen_n_12_area$iTechnology[gen_n_12_area$iTechnology == "wind_copa"]  <- "wind"

# lines together
lines_together <- ggplot() + 
  geom_line(data = gen_n_12_area, aes(x = Date, y = production, col = type), size = 1) +
  scale_fill_brewer(palette="Set2") + facet_wrap(~iTechnology) +
  ylab("GWh") + xlab("") +  ggtitle("Comparing generation_tog - N" ) + 
  theme(plot.title = element_text(hjust = 0.5))
plot(lines_together)
#ggsave("C:/Users/cancella/Google Drive/!IIASA/COPA/runs/Validation_2012/figures/N/lines_tog_n.pdf",lines_together,width=30,height=20,units="cm")

# Plotting time-series in points to see the differences
# hydro
points_hydro_ons  <- gen_n_12 %>% filter(iTechnology == "hydro_ons")
points_hydro_copa <- gen_n_12 %>% filter(iTechnology == "hydro_copa")
points_hydro_n_12 <- bind_rows(points_hydro_ons,points_hydro_copa)

points_hydro <- tibble(
  ons =   c(gen_n_12$production[gen_n_12$iTechnology == "hydro_ons"]),
  copa =  c(gen_n_12$production[gen_n_12$iTechnology == "hydro_copa"]))
plot_points_hydro_n <- ggplot(data = points_hydro, aes(x = ons, y = copa)) + geom_point() +
  ylab("ONS") + xlab("COPA") + ggtitle("ONS x COPA - hydro - N") + theme(plot.title = element_text(hjust = 0.5))
plot(plot_points_hydro_n)
#ggsave("C:/Users/cancella/Google Drive/!IIASA/COPA/runs/Validation_2012/figures/N/points_hydro_n.pdf",plot_points_hydro_n,width=30,height=20,units="cm")

# thermal
points_thermal <- tibble(
  ons =   c(gen_n_12$production[gen_n_12$iTechnology == "Thermal_ons"]), 
  copa =  c(gen_n_12$production[gen_n_12$iTechnology == "Thermal_copa"]))
points_thermal_n <- ggplot(data = points_thermal, aes(x = ons, y = copa)) + geom_point() +
  ylab("ONS") + xlab("COPA") + ggtitle("ONS x COPA - thermal - N") + theme(plot.title = element_text(hjust = 0.5))
plot(points_thermal_n)
#ggsave("C:/Users/cancella/Google Drive/!IIASA/COPA/runs/Validation_2012/figures/N/points_thermal_n.pdf",points_thermal_n,width=30,height=20,units="cm")

#### Multiplot with the more important plots ####
source("C:/Users/cancella/Google Drive/!IIASA/COPA/RE_EXTREME/scripts/COPA_Brazil_input_data/multiplot.R")
#source("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA/RE_EXTREME/scripts/COPA_Brazil_input_data/multiplot.R")
n_summary_plots <- multiplot(lines_together, plot_points_hydro_n,points_thermal_n)
#ggsave("C:/Users/cancella/Google Drive/!IIASA/COPA/runs/Validation_2012/figures/N/summary_n.pdf",n_summary_plots,width=30,height=20,units="cm")

#### statistical indicators ####

# Correlation between time-series of ONS x COPA
correlation_hydro_12 <- as_tibble(cor(gen_n_12$production[gen_n_12$iTechnology == "hydro_ons"],
                                      gen_n_12$production[gen_n_12$iTechnology == "hydro_copa"]))
correlation_thermal_12 <- as_tibble(cor(gen_n_12$production[gen_n_12$iTechnology == "Thermal_ons"],
                                        gen_n_12$production[gen_n_12$iTechnology == "Thermal_copa"]))
# RMSE
reg <- lm(copa ~ ons , data = points_hydro)
rmse_hydro <- modelr::rmse(reg, points_hydro)

reg_thermal <- lm(copa ~ ons , data = points_thermal)
rmse_thermal <- modelr::rmse(reg_thermal, points_thermal)

stats_summary_n <- tibble(
  statistic = c("hydro", "thermal"),
  correlation = c(correlation_hydro_12$value, correlation_thermal_12$value),
  RMSE = c(rmse_hydro, rmse_thermal)
)
stats_summary_n
#### Saving correlation and RMSE in feather format ####
#setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/ONS validation")
#write_feather(stats_summary_n,"stats_summary_n_solo.feather")
