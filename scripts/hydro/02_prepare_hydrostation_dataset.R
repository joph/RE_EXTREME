########################################################################################################
#     This script finds the next downriver power plant for each hydro station and calculates the 
#     distance in between (sum of river segment lengths)
#     The results are arranged in a separate table for GAMS import

#     ToDos
#     add information on maximum Reservoir, min and max flow (if available) 

####################################################################################################### 
#####                                       Load libraries                                        #####
####################################################################################################### 
source("scripts/hydro/00_load_libraries.R")


####################################################################################################### 
#####                                       Load data                                             #####
#######################################################################################################

load("data/hydro/hydrostations_sweden_data.Rdata")
load("data/hydro/se_ecrins_rivers_data_hp.Rdata")

# load ecrin data sets
load(file="data/hydro/se_ecrins_zhyd.Rdata")
load(file="data/hydro/se_ecrins_rivers.Rdata")
# load hype datasets
load(file="data/hydro/se_ehype_basins_hp.RData")
load(file="data/hydro/se_shype_basins_hp.RData")
# load hydropower station spatial data set
load(file="data/hydro/hydrostations_sweden_spatial.RData")

### Load Swedisch bidding areas (test file)
se_bidding_areas           <- readOGR(dsn = "gis_data/elomrade", layer = "elomrade_test")


####################################################################################################### 
#####                 Identify and assign down river hydro plants                                 #####
####################################################################################################### 

hydrostations_sweden_spatial$down_river_hp       <- NA
hydrostations_sweden_spatial$down_river_hp_dist  <- NA
se_ecrins_rivers$along_hp_river <- NA
sub_basin_NA <- NA
index_NA <- 1

for (i in seq(hydrostations_sweden_spatial)){
  # identify id of downriver segment
  temp <- se_ecrins_rivers@data %>% filter(TR == hydrostations_sweden_spatial@data[i,]$ecr_trunk_id)  %>% select(NXDownID,IS_D)
  down_river_id <- as.character(temp$NXDownID)
  is_down_river <- temp$IS_D
  # define starting values
  check_hydro <- 0
  count       <- 0
  distance    <- 0

    while (check_hydro == 0 & is_down_river == 0 ){
    # check if there is hydropower  
      temp <- se_ecrins_rivers@data %>% filter(TR %in% down_river_id)
      if (nrow(temp) > 0){
        down_river_id <- as.character(temp$NXDownID)
        is_down_river <- temp$IS_D
        check_hydro <- temp$is_hydro_power
        distance <- distance + temp$L_SEG
        count = count + 1
        #print(count)
        #print(distance)
      } else {
        print(paste("sub river basin not found: ", down_river_id ))
        sub_basin_NA[index_NA] <- down_river_id 
        index_NA <- index_NA + 1 
        is_down_river <- 1  
        }
    }
 
  print(i)
  
  if (check_hydro == 1 ){
    temp2 <- hydrostations_sweden_spatial@data %>% filter(ecr_trunk_id == as.character(temp$TR)) %>% select(name)
    hydrostations_sweden_spatial@data[i,]$down_river_hp       <- ifelse (length(temp2$name) > 1,"more_hp",as.character(temp2$name))
    hydrostations_sweden_spatial@data[i,]$down_river_hp_dist  <- distance
    }
}


# save data frames
hydrostations_sweden_data <- data.frame(hydrostations_sweden_spatial@data)
save(hydrostations_sweden_data, file = "data/hydro/hydrostations_sweden_data.RData")
save(hydrostations_sweden_spatial, file = "data/hydro/hydrostations_sweden_spatial.RData")
save(sub_basin_NA, file="data/hydro/sub_basin_NA.Rdata")

remove(temp, temp2)


####################################################################################################### 
#####                       Prepare hydropower data set for GAMS                                  #####
####################################################################################################### 

# function for hydro (m3) to power (MWh) conversion
hydro_power_conv_factor <- function(height){
  efficency = 0.9   # turbine efficiency
  density   = 1000  # kg/m3 for water
  g         = 9.81  # acceleration of gravity (9.81 m/s2)
  hydro_conv_factor = efficency * density *  g * height / 1E6 # m3 -> MWh
}

# prepare dataset for GAMS
hydrostations_sweden_gams <- hydrostations_sweden_data %>% 
  select(shype_id, name, bidding_area, river, capacity, height, down_river_hp_dist, down_river_hp) %>% 
  mutate(shype_id = paste("SHP_",shype_id, sep="")) %>%
  mutate(bidding_area = paste("SE_",bidding_area, sep="")) %>%
  mutate(minFlow = 0) %>%
  mutate(maxFlow = 1E6) %>%
  mutate(maxReservoir = 1E6) %>%
  mutate(hydro_conv_factor = hydro_power_conv_factor(height)) %>%
  mutate(maxHydPower = capacity / 1E3) %>%
  mutate(river = paste("RS_",as.numeric(factor(as.character(river))), sep="")) %>% 
  group_by(bidding_area) %>% 
  mutate(plant = paste("HP_", seq_along(bidding_area) , sep="")) %>% 
  select(-height,-capacity)

# replace downriver plant name by downriver plant ID
temp <- left_join(hydrostations_sweden_gams["down_river_hp"],
        hydrostations_sweden_gams[c("name", "plant")], by= c("down_river_hp" = "name"))
hydrostations_sweden_gams$down_river_hp <- temp$plant
hydrostations_sweden_gams$name <- NULL 

# reorder columns
hydrostations_sweden_gams <- hydrostations_sweden_gams[c("shype_id", "bidding_area", 
                            "river", "plant", "minFlow", "maxFlow", "maxReservoir", "maxHydPower", 
                            "down_river_hp_dist", "hydro_conv_factor", "down_river_hp")]

# rename columns  
names(hydrostations_sweden_gams) <- c("id",	"Region",	"River",	"Plant",	"minFlow",	"maxFlow",	
                                      "maxReservoir",	"maxHydPower",	"runOffDelay",	"hydroConvFact",	"downRiver")

save(hydrostations_sweden_gams, file = "results/hydro/hydrostations_sweden_gams.RData")
write_feather(hydrostations_sweden_gams, path = "results/hydro/hydrostations_sweden_gams.feather")
