########################################################################################################
#     This script loads all ehype and shype river discharge time series for a specific region
#     (i.e. subbasins within a specific region or subbasins that contain hydro power plants)
#     and stores the time series for relevant subbasins in a new file (e.g. "ts_ehype.Rdata)
########################################################################################################

####################################################################################################### 
#####                                       Load libraries                                        #####
####################################################################################################### 
source("scripts/hydro/00_load_libraries.R")


####################################################################################################### 
#####                        Load and prepare spatial data sets                                   #####
####################################################################################################### 

### Load Nuts3 regions data sets
Sweden_Nuts3               <- readOGR(dsn = "gis_data/SE_NUTS3", layer = "SE_NUTS3")

### Load Swedisch bidding areas 
#se_bidding_areas           <- readOGR(dsn = "gis_data/elomrade", layer = "elomrade_test") # (test file)
se_bidding_areas           <- readOGR(dsn = "gis_data/net_top_secret/elomraden", layer = "elomraden")

### Load hype data sets
ehype_subbasins_EU         <- readOGR(dsn = "gis_data/ehype_riverbasins", layer = "e-hype3_subids")
shype_subbasins_Sweden     <- readOGR(dsn = "gis_data/shype_riverbasins", layer = "Sweden_Riverbasins_SubID")


### Load Ecrin data ###
# switch working directory
  mywd <- getwd()
  setwd("~/+data/_Gis_Data/ECRINS")

# import swedish basins (c_zhyd) from shapefile
  se_ecrins_zhyd         <- readOGR(dsn ="c_zhyd", layer = "se_c_zhyd")
# edit Nuts2 field
  se_ecrins_zhyd@data$Nuts2 <- str_replace_all(se_ecrins_zhyd@data$Nuts2,"_","")
  se_ecrins_zhyd@data$Nuts2 <- str_replace(se_ecrins_zhyd@data$Nuts2,"WNT","")
# import swedish rivers (c_zhyd) from shapefile
  se_ecrins_rivers         <- readOGR(dsn ="ecr_riv", layer = "se_ecr_riv")
# reset wd and save 
  setwd(mywd)

### load leif kuhlins hydrostation dataset ###
  hydrostations_sweden_data  <- as_tibble(read.csv(file="data/hydro/vattenkraft_info_1500.csv", sep=";", dec="."))

# create spatial hydrostation dataset
  hydrostations_sweden_spatial <- SpatialPointsDataFrame(coords = hydrostations_sweden_data[,c("Long","Lat")], data = hydrostations_sweden_data, proj4string = CRS(ehype_subbasins_EU@proj4string@projargs))


### adjust projections if they are not identical
if(!identical(proj4string(hydrostations_sweden_spatial),proj4string(se_bidding_areas))){
    se_bidding_areas <-  spTransform(se_bidding_areas, CRS(hydrostations_sweden_spatial@proj4string@projargs))  
  }  
if(!identical(proj4string(Sweden_Nuts3),proj4string(ehype_subbasins_EU))){
  Sweden_Nuts3 <-  spTransform(Sweden_Nuts3, CRS(ehype_subbasins_EU@proj4string@projargs))  
  }
if(!identical(proj4string(hydrostations_sweden_spatial),proj4string(ehype_subbasins_EU))){
  hydrostations_sweden_spatial <-  spTransform(hydrostations_sweden_spatial, CRS(ehype_subbasins_EU@proj4string@projargs))  
  }
if(!identical(proj4string(se_ecrins_rivers),proj4string(ehype_subbasins_EU))){
  se_ecrins_rivers <-  spTransform(se_ecrins_rivers, CRS(ehype_subbasins_EU@proj4string@projargs))  
  }
if(!identical(proj4string(se_ecrins_zhyd),proj4string(ehype_subbasins_EU))){
  se_ecrins_zhyd <-  spTransform(se_ecrins_zhyd, CRS(ehype_subbasins_EU@proj4string@projargs))  
  }
if(!identical(proj4string(se_bidding_areas),proj4string(ehype_subbasins_EU))){
    se_bidding_areas <-  spTransform(se_bidding_areas, CRS(ehype_subbasins_EU@proj4string@projargs))  
  }


####################################################################################################### 
#####            Add IDs to link hydrostation dataframe with ecrins and hype data                 #####
#######################################################################################################    
   
# preselect ehype & shype subbasins that contain hydrostations 
se_ehype_basins_hp = ehype_subbasins_EU[!is.na(over(ehype_subbasins_EU,as(hydrostations_sweden_spatial,"SpatialPoints"))),]
se_shype_basins_hp = shype_subbasins_Sweden[!is.na(over(shype_subbasins_Sweden,as(hydrostations_sweden_spatial,"SpatialPoints"))),]

#test plots
plot(se_ehype_basins_hp)
plot(se_shype_basins_hp)

# select subbasins within certain region
#ehype_subbasins_SE = ehype_subbasins_EU[!is.na(over(as(ehype_subbasins_EU,"SpatialPolygons"),as(Sweden_Nuts3_84,"SpatialPolygons"))),]

# for distance calculations spatial datasets have to be projected to planar coordinate systems
ETRS89_LAEA <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
temp                      <- se_ecrins_rivers[which(se_ecrins_rivers@data$IS_MAIN == 1),]
se_ecrins_rivers_ETRS89   <- spTransform(temp, ETRS89_LAEA)  
hydrostations_sweden_spatial_ETRS89 <-  spTransform(hydrostations_sweden_spatial, ETRS89_LAEA)  
remove(temp)

# create new dataframe for saving information on ecrin river trunk that are closest to hydrostations
se_ecrins_rivers_data_hp <- as_tibble(se_ecrins_rivers_ETRS89@data)[1,]

### save ehype and shype riverbasin subids and ecrin ids into hydrostation dataset
for (i in seq(nrow(hydrostations_sweden_data))){
  # find corresponding ehype and shype subbasin for each hydrostation
  temp  <-  se_ehype_basins_hp[!is.na(over(se_ehype_basins_hp, as(hydrostations_sweden_spatial[i,],"SpatialPoints"))),]
  temp2 <-  se_shype_basins_hp[!is.na(over(se_shype_basins_hp, as(hydrostations_sweden_spatial[i,],"SpatialPoints"))),]
  # find closest river segment (trunk) for each hydrostation 
  closest_id <- which.min(gDistance(as(hydrostations_sweden_spatial_ETRS89[i,],"SpatialPoints"),se_ecrins_rivers_ETRS89, byid = TRUE )) 
  se_ecrins_rivers_data_hp[i,]  <- as_tibble(se_ecrins_rivers_ETRS89@data[closest_id,])
  
  hydrostations_sweden_spatial$ehype_id[i]     <- temp@data$SUBID
  hydrostations_sweden_spatial$shype_id[i]     <- as.character(temp2@data$SUBID)
  hydrostations_sweden_spatial$ecr_basin_id[i] <- as.character(se_ecrins_rivers_data_hp[i,]$Bas0_ID) 
  hydrostations_sweden_spatial$ecr_river_id[i] <- as.character(se_ecrins_rivers_data_hp[i,]$River_ID)
  hydrostations_sweden_spatial$ecr_trunk_id[i] <- as.character(se_ecrins_rivers_data_hp[i,]$TR)
  print(i)
}

# add info to se_ecrins_river if there is a hydro station
se_ecrins_rivers@data <- se_ecrins_rivers@data %>% 
  mutate(is_hydro_power = ifelse(TR %in% se_ecrins_rivers_data_hp$TR, 1, 0))

# remove temporary files
remove(se_ecrins_rivers_ETRS89, hydrostations_sweden_spatial_ETRS89)


####################################################################################################### 
#####   Add price zone information,  preselect hydrostations and prepare GAMS input file        #####
####################################################################################################### 

# add information of bidding zone to hydro station dataset
hydrostations_sweden_spatial@data$bidding_area <- NA

for (i in seq(se_bidding_areas@data$snitt)){
  temp <- se_bidding_areas[which(se_bidding_areas@data$snitt == i),]
  temp_hydro_sel <- hydrostations_sweden_spatial[!is.na(over(hydrostations_sweden_spatial,as(temp,"SpatialPolygons"))),]
  elomrade_hydro_list <- temp_hydro_sel@data$name
  hydrostations_sweden_spatial@data$bidding_area[which(hydrostations_sweden_spatial@data$name %in% elomrade_hydro_list)] <- i
  print(i)
}

# save complete dataset of hydrostations
hydrostations_sweden_data <- data.frame(hydrostations_sweden_spatial@data)
save(hydrostations_sweden_data,   file="data/hydro/hydrostations_sweden_data_all.Rdata")
save(hydrostations_sweden_spatial, file = "data/hydro/hydrostations_sweden_spatial_all.RData")

# pre-select plants - only those with capacity of more than 10MW and calculate share of total production
hydrostations_sweden_spatial@data %>%  
  group_by(capacity > 10000) %>%  
  summarise(total_prod = sum(normal_prod, na.rm=TRUE)) -> total_prod
hydrostations_sweden_spatial@data %>%  
    group_by(capacity > 10000) %>% count() -> count_hydro

hydrostations_sweden_spatial <- hydrostations_sweden_spatial[which(hydrostations_sweden_spatial@data$capacity > 10000),]

print(paste(count_hydro$n[2],"out of", sum(count_hydro$n),  "hydrostations selected that account for", 
            100* round(total_prod$total_prod[2] / sum(total_prod$total_prod),3), "% of annual hydro power production"))

remove(count_hydro,total_prod)

# update data dataframe
hydrostations_sweden_data <- hydrostations_sweden_spatial@data


####################################################################################################### 
#####                                 Save data frames                                            #####
####################################################################################################### 

save(hydrostations_sweden_data,   file="data/hydro/hydrostations_sweden_data.Rdata")
save(se_ecrins_rivers_data_hp,   file="data/hydro/se_ecrins_rivers_data_hp.Rdata")

# save spatial datasets for sweden as Rdata and export to shapefile
save(se_ecrins_zhyd,     file="data/hydro/se_ecrins_zhyd.Rdata")
save(se_ecrins_rivers,   file="data/hydro/se_ecrins_rivers.Rdata")
save(se_ehype_basins_hp, file = "data/hydro/se_ehype_basins_hp.RData")
save(se_shype_basins_hp, file = "data/hydro/se_shype_basins_hp.RData")
save(hydrostations_sweden_spatial, file = "data/hydro/hydrostations_sweden_spatial.RData")

# TODO implement shapefile export
#names(hydrostations_sweden_spatial@data)
#writeOGR(obj=hydrostations_sweden_spatial, dsn="results/gis", layer="hydro_stations_se", driver="ESRI Shapefile", overwrite_layer=TRUE)

# save selected subbasins into download list for time series data from ehype / shype
ehype_subids_download <- as.character(hydrostations_sweden_spatial@data$ehype_id)
shype_subids_download <- as.character(hydrostations_sweden_spatial@data$shype_id)


####################################################################################################### 
#####                         Download and save EHYPE timeseries                                  #####
####################################################################################################### 

# download url
url_ehype <- "http://hypeweb.smhi.se/europehype/time-series/download/"
# save folder
save_folder <- "data/hydro/ehype/"
# filetype of download files
filetype  <- ".xls"
 
### prepare output file
download.file("http://hypeweb.smhi.se/europehype/time-series/download/8801544", "data/hydro/ehype/8801544.xls", mode="wb")
temp <- read_excel("data/hydro/ehype/8801544.xls", 1, col_names = TRUE, col_types = NULL, na = "", skip=0)
ts_ehype <- tibble(date = temp$Date)

### combine timeseries from excel files into one tibble
for ( i in seq_along(ehype_subids_download))
{
  ehype_save <- paste(save_folder,ehype_subids_download[i],filetype,sep="")
  # download file if not available locally
  if (!file.exists(ehype_save)) {
    url_file  <- paste(url_ehype,ehype_subids_download[i], sep = "")
    download.file(url_file, ehype_save, mode="wb")  
  }
  temp <- read_excel(ehype_save, 1, col_names = TRUE, col_types = NULL, na = "", skip=0)
  names(temp) <- c("day",ehype_subids_download[i])
  #store in tibble
  ts_ehype[,i+1] <- temp[2]
}

# tidy and save
ts_ehype <- gather(ts_ehype,basin_id,runoff,-date)
save(ts_ehype, file = "data/hydro/ts_ehype.RData")
write_feather(ts_ehype, path = "data/hydro/ts_ehype.feather")


####################################################################################################### 
#####                         Download and save SHYPE timeseries                                  #####
####################################################################################################### 

# download location
url_shype <- "http://vattenwebb.smhi.se/modelarea/basindownload/"
# save folder
save_folder <- "data/hydro/shype/"
# filetype of download files
filetype  <- ".xls"

# prepare output file
download.file("http://vattenwebb.smhi.se/modelarea/basindownload/31672", "data/hydro/shype/31672.xls", mode="wb")
temp <- read_excel("data/hydro/shype/31672.xls", sheet = "Dygnsvärden", col_names = FALSE, col_types = NULL, na = "NA", skip=3)
shype_data <- tibble(date = temp[[1]])

# different dataframes for all hype time series
ts_shype_tot  <- shype_data
ts_shype_cor  <- shype_data
ts_shype_nat  <- shype_data
remove(shype_data)

# combine timeseries from excel files into one dataframe
for ( i in seq_along(shype_subids_download))
  {
  shype_save <- paste(save_folder,shype_subids_download[i],filetype,sep="")
  
  # download file if not available locally
    if (!file.exists(shype_save)) {
      url_file  <- paste(url_shype,shype_subids_download[i], sep = "")
      download.file(url_file, shype_save, mode="wb")  
    }
  
  temp <- read_excel(shype_save, sheet = "Dygnsvärden", col_names = FALSE, col_types = NULL, na = "NA", skip=3)

  #store in dataframe old - style
  ts_shype_tot[,i+1]  <- temp[,2]
  ts_shype_cor[,i+1]  <- temp[,3]
  ts_shype_nat[,i+1]  <- temp[,4]
  
  # set current river basin subid as column name
  names(ts_shype_tot)[1+i] <- as.character(shype_subids_download[i])
  names(ts_shype_cor)[1+i] <- as.character(shype_subids_download[i])
  names(ts_shype_nat)[1+i] <- as.character(shype_subids_download[i])
  
  #if (file.exists(ehype_save)) file.remove(ehype_save)
}

# tidy and bind
ts_shype_tot <- ts_shype_tot %>% gather(basin_id,runoff,-date)  %>%  mutate(type = "total")
ts_shype_cor <- ts_shype_cor %>% gather(basin_id,runoff,-date)  %>%  mutate(type = "corrected")
ts_shype_nat <- ts_shype_nat %>% gather(basin_id,runoff,-date)  %>%  mutate(type = "natural")
ts_shype <- bind_rows(ts_shype_tot, ts_shype_cor, ts_shype_nat)

# save
write_feather(ts_shype, path = "data/hydro/ts_shype.feather")

####################################################################################################### 
#####          Load and combine scenarios for wind time series from Olauson et al                 #####
####################################################################################################### 

#### import wind farm locations                                                                   #####
wind_farms <- read_delim("~/Dropbox/Formas/HydroSweden/data/wind/WECs.csv", ";", escape_double = FALSE, 
              col_names = FALSE, comment = "%", trim_ws = TRUE)

names(wind_farms) <- c("scenario","east_sweref99tm","north_sweref99tm","capacity_kw","rotor_diameter","hub_height","est_annual_prod_mwh",
  "grid_conection_date","region", "turbine_number", "type", "farm_id",  "latitude_wgs88" ,"longitude_wgs84")

# function for convering Matlab time format
Matlab2Rdate <- function(val) as.Date(val - 1, origin = '0000-01-01') 
# convert grid connection date from matlab time format
wind_farms$grid_conection_date <-  Matlab2Rdate(wind_farms$grid_conection_date)

# spatial wind_farms dataset
wind_farms_spatial <- SpatialPointsDataFrame(coords = wind_farms[,c("longitude_wgs84","latitude_wgs88")], data = wind_farms, proj4string = CRS(hydrostations_sweden_spatial@proj4string@projargs))

# plot wind_farms_spatial
plot(wind_farms_spatial)
plot(Sweden_Nuts3, add=TRUE)

#### import scenarios                                                                             #####
ts_wind_all <- NULL
wind_scenarios <- c(paste("A",seq(1:7),sep = ""),paste("B",seq(1:7),sep = ""),paste("C",seq(1:7),sep = ""), "D1")

ts_wind_time <- read_delim("~/Dropbox/Formas/HydroSweden/data/wind/Time.csv", ";", escape_double = FALSE, 
                         col_names = TRUE, trim_ws = TRUE)
names(ts_wind_time) <- "date"

for (i in seq_along(wind_scenarios)){
  # wind_scenarios <- "A1"
  temp <- read_delim(paste("~/Dropbox/Formas/HydroSweden/data/wind/",wind_scenarios[i],".csv",sep=""), ";", 
                     escape_double = FALSE, col_names = TRUE, trim_ws = TRUE)
  names(temp)[1] <- "SE"
  
  ts_wind <- cbind(ts_wind_time, temp) %>% 
    gather(2:6, key="region",value="mwh") %>% 
    mutate(scenario = wind_scenarios[i]) %>% 
    select(date, scenario, region, mwh)
  
  # save all scenarios in one file
  if(is.null(ts_wind_all)){
    ts_wind_all <- ts_wind
  }else{
    ts_wind_all <- rbind(ts_wind_all,ts_wind)
  }
  print(paste("Scenario", wind_scenarios[i], "added"))

}

write_feather(ts_wind_all, path = "data/wind/ts_wind_all.feather")



