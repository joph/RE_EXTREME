########################################################################################################
#     This script calculates daily power output for all hydrostations based on HYPE river runoff time
#     series.    

#     ToDos
#     add information on maximum Reservoir, min and max flow (if available) 


####################################################################################################### 
#####                                       Load libraries                                        #####
####################################################################################################### 
source("scripts/hydro/00_load_libraries.R")


####################################################################################################### 
#####                                       Load data                                             #####
####################################################################################################### 

# hydropower station data sets
load("data/hydro/hydrostations_sweden_data.Rdata")
load("data/hydro/hydrostations_sweden_spatial.RData")

# load hype datasets
#load(file="data/hydro/se_ehype_basins_hp.RData")
#load(file="data/hydro/se_shype_basins_hp.RData")

### Load Swedisch bidding areas (test file)
se_bidding_areas           <- readOGR(dsn = "gis_data/elomrade", layer = "elomrade_test")

# adjust projections if they are not identical
if(!identical(proj4string(hydrostations_sweden_spatial),proj4string(se_bidding_areas))){
  se_bidding_areas <-  spTransform(se_bidding_areas, CRS(hydrostations_sweden_spatial@proj4string@projargs))  
}

# load hype runoff timeseries
# compare load time feather vs. Rdata (0.18 vs. 1.37 ms)
#system.time(ts_ehype <- read_feather("data/hydro/ts_ehype.feather"))
#system.time(load("data/hydro/ts_ehype.Rdata"))
ts_ehype <- read_feather("data/hydro/ts_ehype.feather")
ts_shype <- read_feather("data/hydro/ts_shype.feather")


####################################################################################################### 
#####                                         Functions                                           #####
####################################################################################################### 
# calculate daily mwh output
# Pa = μ ρ q g h 
# where
# Pa = mwh available (W)
# μ = efficiency (in general in the range 0.75 to 0.95)
# ρ = density (kg/m3) (~ 1000 kg/m3 for water)
# q = water flow (m3/s)
# g = acceleration of gravity (9.81 m/s2)
# h = falling height, head (m)

calc_mwh_output <- function(water_flow,height,capacity){
  # general hydropower parameters
  efficency = 0.9   # turbine efficiency
  density   = 1000  # kg/m3 for water
  g         = 9.81  # acceleration of gravity (9.81 m/s2)
  hours     = 24
  
  # calculate mwh output
  pa <- efficency * density * water_flow * g * height / 1E6 # in MW
  # output limited to plant capacity
  pa[pa>capacity] <- capacity 
  # daily mwh production
  p_daily <- pa * 24
}


####################################################################################################### 
##### EHYPE - power generation time series for all hydro stations                                 #####
####################################################################################################### 

#create dataframes to store mwh output timeseries
ts_ehype_mwh <- ts_ehype %>% 
  filter(basin_id == as.character(hydrostations_sweden_data$ehype_id[1])) %>% 
  transmute(date = as.Date(date))

# ehype
  for (i in seq(hydrostations_sweden_data$ehype_id)){
    water_flow     <- ts_ehype %>% 
      filter(basin_id == as.character(hydrostations_sweden_data$ehype_id[i])) %>% 
      select(runoff)  
    height         <- hydrostations_sweden_data$height[i]  # falling height, head (m)
    capacity       <- hydrostations_sweden_data$capacity[i] / 1000 # installed capacity in MW
    
    ts_ehype_mwh[,i+1] <- calc_mwh_output(water_flow,height,capacity)
    names(ts_ehype_mwh)[i+1] <- as.character(hydrostations_sweden_data$name[i] )
  }

# tidy and save
ts_ehype_mwh <- gather(ts_ehype_mwh,hydrostation,mwh,-date)
save(ts_ehype_mwh, file = "results/hydro/ts_ehype_mwh.RData")
write_feather(ts_ehype_mwh, path = "results/hydro/ts_ehype_mwh.feather")


####################################################################################################### 
##### SHYPE - power generation time series for all hydro stations                                 #####
####################################################################################################### 

# SHYPE time series
shype_model <-  c("total", "corrected", "natural")

for (j in shype_model){
  #create dataframes to store mwh output timeseries
  ts_shype_mwh <- ts_shype %>% 
    filter(type == j & basin_id == as.character(hydrostations_sweden_data$shype_id[1])) %>% 
    transmute(date = as.Date(date))

  
  for (i in seq(hydrostations_sweden_data$shype_id)){
    water_flow     <- 
      ts_shype %>% 
      filter(type == j & basin_id == as.character(hydrostations_sweden_data$shype_id[i])) %>% 
      select(runoff)  
    height         <- hydrostations_sweden_data$height[i]  # falling height, head (m)
    capacity       <- hydrostations_sweden_data$capacity[i] / 1000 # installed capacity in MW
    
    ts_shype_mwh[,i+1] <- calc_mwh_output(water_flow,height,capacity)
    names(ts_shype_mwh)[i+1] <- as.character(hydrostations_sweden_data$name[i] )
  }
  
  ts_shype_mwh <- gather(ts_shype_mwh,hydrostation,mwh,-date)
  save(ts_shype_mwh,          file = paste("results/hydro/ts_shype_mwh_",j,".Rdata",   sep = ""))
  write_feather(ts_shype_mwh, path = paste("results/hydro/ts_shype_mwh_",j,".feather", sep = ""))
  print(paste("Time series saved: ", j))
}

