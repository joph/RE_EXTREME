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
calc_mwh_output <- function(water_flow,height,capacity){
  # Pa = μ * ρ * q * g * h 
  # where
  # Pa = mwh available (W)
  # μ = efficiency (in general in the range 0.75 to 0.95)
  # ρ = density (kg/m3) (~ 1000 kg/m3 for water)
  # q = water flow (m3/s)
  # g = acceleration of gravity (9.81 m/s2)
  # h = falling height, head (m)

  efficency = 0.9   
  density   = 1000  
  g         = 9.81  
  hours     = 24
  
  # calculate mwh output
  pa <- efficency * density * water_flow * g * height / 1E6 # in MW
  # output limited to plant capacity
  pa[pa>capacity] <- capacity 
  # daily production
  pa_daily <- pa * hours
}

# Function -> creates hourly time series from daily time series by adding 23 NA values per day and interpolating NAs
day2hour <- function(ts){
  temp <-  unlist( lapply(ts,FUN=function(x) c(x/24,rep(NA,23)) ) )
  temp <-  data.frame(na.approx(temp, rule = 2))
  names(temp) <- names(ts)
  return(temp)
}



####################################################################################################### 
##### EHYPE - power generation time series for all hydro stations                                 #####
####################################################################################################### 

#create dataframes to store mwh output timeseries
ts_ehype_mwh <- ts_ehype %>% 
  filter(basin_id == as.character(hydrostations_sweden_data$ehype_id[1])) %>% 
  transmute(Date = as.Date(date))

# ehype
  for (i in seq(hydrostations_sweden_data$ehype_id)){
    water_flow     <- ts_ehype %>% 
      filter(basin_id == as.character(hydrostations_sweden_data$ehype_id[i])) %>% 
      select(runoff)  
    height         <- hydrostations_sweden_data$height[i]  # falling height, head (m)
    capacity       <- hydrostations_sweden_data$capacity[i] / 1000 # installed capacity in MW
    
    ts_ehype_mwh[,i+1] <- calc_mwh_output(water_flow,height,capacity)
    names(ts_ehype_mwh)[i+1] <- as.character(paste("SHP",hydrostations_sweden_data$shype_id[i], sep=""))
  }


# create data frame with hourly time format
ts_ehype_mwh_hourly   <- data.frame(Date = ymd_h(paste(rep(ts_ehype_mwh$Date, each=24),rep(0:23,nrow(ts_ehype_mwh)))))
# add hourly run-off data
ts_ehype_mwh_hourly   <- cbind(ts_ehype_mwh_hourly, as.data.frame( apply(ts_ehype_mwh[,-1], 2, FUN=day2hour) )) 

# tidy and save
ts_ehype_mwh        <- ts_ehype_mwh %>% 
  gather(.,hydrostation,mwh,-Date) 

  
ts_ehype_mwh_hourly <- gather(ts_ehype_mwh_hourly,hydrostation,mwh,-Date)

save(ts_ehype_mwh, file = "results/hydro/ts_ehype_mwh.RData")
write_feather(ts_ehype_mwh, path = "results/hydro/ts_ehype_mwh.feather")

save(ts_ehype_mwh_hourly, file = "results/hydro/ts_ehype_mwh_hourly.RData")
write_feather(ts_ehype_mwh_hourly, path = "results/hydro/ts_ehype_mwh_hourly.feather")


####################################################################################################### 
##### SHYPE - power generation time series for all hydro stations                                 #####
####################################################################################################### 

# SHYPE time series
shype_model <-  c("total", "corrected", "natural")

for (j in shype_model){
  #create dataframes to store mwh output timeseries
  ts_shype_mwh <- ts_shype %>% 
    filter(type == j & basin_id == as.character(hydrostations_sweden_data$shype_id[1])) %>% 
    transmute(Date = as.Date(date))

  
  for (i in seq(hydrostations_sweden_data$shype_id)){
    water_flow     <- 
      ts_shype %>% 
      filter(type == j & basin_id == as.character(hydrostations_sweden_data$shype_id[i])) %>% 
      select(runoff)  
    height         <- hydrostations_sweden_data$height[i]  # falling height, head (m)
    capacity       <- hydrostations_sweden_data$capacity[i] / 1000 # installed capacity in MW
    
    ts_shype_mwh[,i+1] <- calc_mwh_output(water_flow,height,capacity)
    names(ts_shype_mwh)[i+1] <- as.character(paste("SHP",hydrostations_sweden_data$shype_id[i], sep=""))
  }

  # create data frame with hourly time format
  ts_shype_mwh_hourly   <- data.frame(Date = ymd_h(paste(rep(ts_shype_mwh$Date, each=24),rep(0:23,nrow(ts_shype_mwh)))))
  # add hourly run-off data
  ts_shype_mwh_hourly   <- cbind(ts_shype_mwh_hourly, as.data.frame( apply(ts_shype_mwh[,-1], 2, FUN=day2hour) )) 
  
  # tidy  
  ts_shype_mwh <- gather(ts_shype_mwh,hydrostation,mwh,-Date)
  ts_shype_mwh_hourly <- gather(ts_shype_mwh_hourly,hydrostation,mwh,-Date)

  # save daily an dhourly time series    
  save(ts_shype_mwh,          file = paste("results/hydro/ts_shype_mwh_",j,".Rdata",   sep = ""))
  write_feather(ts_shype_mwh, path = paste("results/hydro/ts_shype_mwh_",j,".feather", sep = ""))
  save(ts_shype_mwh_hourly,          file = paste("results/hydro/ts_shype_mwh_hourly_",j,".Rdata",   sep = ""))
  write_feather(ts_shype_mwh_hourly, path = paste("results/hydro/ts_shype_mwh_hourly_",j,".feather", sep = ""))
  
  print(paste("Time series saved: ", j))
}


