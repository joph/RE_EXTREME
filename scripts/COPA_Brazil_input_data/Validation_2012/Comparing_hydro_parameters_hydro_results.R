# This script checks some hydro results and compares them to parameters of input data

#### defining hydro variables ####
hydro <- results %>% filter(name == "hydro")

names <- c("h_stor_in","h_stor_lv","h_stor_out", "hyd_up", "hydro", "spill")
for (i in c(1:length(names))){
  assign(str_c("x", names[i], sep="_"),results %>% 
           filter(name == str_c("x", names[i], sep="_"))) 
}
#### Loading hydro parameters ####
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA/data/hydro")
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA/data/hydro")
hydroPar <- read_csv2("hydro_data_br_2012_1.csv")
#hydroPar <- read_csv2("hydro_data_br_2012 - only 2012 capacity.csv")
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