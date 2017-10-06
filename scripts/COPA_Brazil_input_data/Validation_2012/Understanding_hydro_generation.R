# This script trys to understand the how does the hydro generation happens in COPA model.
# data from run "HYDRO_BOUNDS" from 25.9.
# For hydro and thermal plots, we've used results from runs "hydro_bounds" and "Limit_hydro_50".

#### defining hydro variables ####
hydro <- results %>% filter(name == "hydro")

names <- c("h_stor_in","h_stor_lv","h_stor_out", "hyd_up", "hydro", "spill")
for (i in c(1:length(names))){
  assign(str_c("x", names[i], sep="_"),results %>% 
           filter(name == str_c("x", names[i], sep="_"))) 
}

#### Loading hydro parameters ####
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA/data/hydro")
#setwd(PC)
hydroPar <- read_csv2("hydro_data_br_2012_1.csv")
hydroPar$initReservoir <- parse_number(hydroPar$initReservoir)
hydroPar$termReservoir <- parse_number(hydroPar$termReservoir)

hydroPar$minFlow <- c(hydroPar$minFlow[1]/1e5, hydroPar$minFlow[2:4]/1e6, hydroPar$minFlow[5])
hydroPar$maxReservoir <- c(hydroPar$maxReservoir [1]/10, hydroPar$maxReservoir [2:4]/100, hydroPar$maxReservoir[5])

hydroPar$maxHydPower <- c(hydroPar$maxHydPower/10)

#### comparing parameters of input and variable values ####
# maximum storage level
# ..x_h_stor_lv(reg,t,ws,hp)=L= maxReservoir(reg,ws,hp)
#comparing maximum storage level and marReservoir parameter
storMaxComp <- x_h_stor_lv %>% group_by(reg) %>% summarize(max_x_h_stor_lv = max(value)) %>% 
  mutate(maxReservoir = hydroPar$maxReservoir[c(4,3,1,2)]) 
storMaxComp
# all regions uses the maximum capacity of reservoir. Exception: SE/CO: 64% of it.

# plotting storage level per region on the same graph
ggplot() + geom_line(data = x_h_stor_lv, aes(x = datetime, y = value, col = reg), size = 1)

#### Comparing these results with hydro generation from ONS ####
# reading files with hydro generation from ONS
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/ONS validation")
# setwd("/!IIASA/COPA  Initial Data/ONS validation") # PC
namesNew <- c("SE.CO","SUL","NE","N")
gen_ons_final_se  <- read_csv(paste("geracao_ONS_",namesNew[1],".csv", sep=""))
gen_ons_final_sul <- read_csv(paste("geracao_ONS_",namesNew[2],".csv", sep=""))
gen_ons_final_ne  <- read_csv(paste("geracao_ONS_",namesNew[3],".csv", sep=""))
gen_ons_final_n   <- read_csv(paste("geracao_ONS_",namesNew[4],".csv", sep=""))
gen_ons_final_tot <- bind_rows(gen_ons_final_se,gen_ons_final_sul, gen_ons_final_ne, gen_ons_final_n)

hydro_ons_tot <- gen_ons_final_tot %>% group_by(Date, iTechnology) %>% 
  summarise(tot_gen = sum(production)) %>% filter(iTechnology == "hydro_ons")

# COPA values - dayly basis 
# Passing to daily basis
# hydro
hydro_COPA_tot <- results %>% filter(name == "x_hydro_tot")  %>% select(datetime, value) %>% 
  group_by(month(datetime), day(datetime)) %>% summarise(tot_gen = sum(value)/1e3)

hydro_COPA_tot$Date <- hydro_ons_tot$Date
hydro_COPA_tot$iTechnology <- hydro_ons_tot$iTechnology
hydro_COPA_tot$iTechnology <- "hydro_copa"
ungroup(hydro_COPA_tot)

hydro_COPA_tot <- hydro_COPA_tot %>% select(Date, iTechnology, tot_gen)
hydro_COPA_tot <- hydro_COPA_tot[,2:4]

# comparing total hydro ONS x COPA
hydro_tot <- bind_rows(hydro_ons_tot, hydro_COPA_tot)
lines <- ggplot()+geom_line(data = hydro_tot, aes(x = Date, y = tot_gen, col = iTechnology), size = 1)
plot(lines)

points_hydro_tot <- tibble(
  ons =   c(hydro_tot$tot_gen[hydro_tot$iTechnology == "hydro_ons"]), 
  copa =  c(hydro_tot$tot_gen[hydro_tot$iTechnology == "hydro_copa"]))
points <- ggplot()+geom_point(data = points_hydro_tot, aes(x = ons, y = copa))
plot(points)

# saving in pdf 
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA/runs/Validation_2012/figures")
grDevices::pdf("hydro_total.pdf")
lines
points
dev.off()

correlation <- cor(points_hydro_tot$ons, points_hydro_tot$copa)
correlation
reg_hydro <- lm(copa ~ ons , data = points_hydro_tot)
rmse_hydro <- modelr::rmse(reg_hydro, points_hydro_tot)
rmse_hydro

# thermal ONS
thermal_ons_tot <- gen_ons_final_tot %>% group_by(Date, iTechnology) %>% 
  summarise(tot_gen = sum(production)) %>% filter(iTechnology == "Thermal_ons")

#thermal copa
thermal_COPA_tot <- results %>% filter(name == "x_term")  %>% select(datetime, value) %>% 
  group_by(month(datetime), day(datetime)) %>% summarise(tot_gen = sum(value)/1e3)

thermal_COPA_tot$Date <- thermal_ons_tot$Date
thermal_COPA_tot$iTechnology <- thermal_ons_tot$iTechnology
thermal_COPA_tot$iTechnology <- "Thermal_copa"
ungroup(thermal_COPA_tot)

thermal_COPA_tot <- thermal_COPA_tot %>% select(Date, iTechnology, tot_gen)
thermal_COPA_tot <- thermal_COPA_tot[,2:4]
thermal_COPA_tot

thermal_tot <- bind_rows(thermal_ons_tot, thermal_COPA_tot)
lines_thermal <- ggplot()+geom_line(data = thermal_tot, aes(x = Date, y = tot_gen, col = iTechnology), size = 1)
plot(lines_thermal)

points_thermal_tot <- tibble(
  ons =   c(thermal_tot$tot_gen[thermal_tot$iTechnology == "Thermal_ons"]), 
  copa =  c(thermal_tot$tot_gen[thermal_tot$iTechnology == "Thermal_copa"]))
points_thermal <- ggplot()+geom_point(data = points_thermal_tot, aes(x = ons, y = copa))
plot(points_thermal)

# saving thermal plots in pdf
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA/runs/Validation_2012/figures/")
pdf("thermal_total.pdf")
lines_thermal
points_thermal
dev.off()