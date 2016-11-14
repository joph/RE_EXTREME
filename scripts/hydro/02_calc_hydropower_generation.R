# ggmap     extends the plotting package ggplot2 for maps
# rgdal     R’s interface to the popular C/C++ spatial data processing library gdal
# rgeos     R’s interface to the powerful vector processing library geos
# maptools  provides various mapping functions
# tmap      a new packages for rapidly creating beautiful maps
# dplyr and tidyr  fast and concise data manipulation packages
#install.packages("broom","tidyr","dplyr")

library(rgdal)
library(readxl)

library(gdata)
library(foreign)
library(reshape2)
library(dplyr)
library(tidyr)
library(broom)
library(purrr)
library(magrittr)
library(maptools)
library(sp)

library(ggplot2)
library(ggmap)
library(plotly)



########################################################################################################
# load spatial dataset
ehype_subbasins_EU         <- readOGR(dsn = "gis_data/ehype_riverbasins", layer = "e-hype3_subids")
shype_subbasins_Sweden     <- readOGR(dsn = "gis_data/shype_riverbasins", layer = "Sweden_Riverbasins_SubID")
Sweden_Nuts3               <- readOGR(dsn = "gis_data/SE_NUTS3", layer = "SE_NUTS3")

hydrostations_sweden_data  <- read.csv(file="data/vattenkraft_info.csv", sep=";" )
xy <- hydrostations_sweden_data[,c("Long","Lat")]
hydrostations_sweden <- SpatialPointsDataFrame(coords = xy, data = hydrostations_sweden_data, proj4string = CRS(ehype_subbasins_EU@proj4string@projargs))
remove(xy)

# adjust projections if they are not identical
if(!identical(proj4string(Sweden_Nuts3_84),proj4string(ehype_subbasins_EU))){
  Sweden_Nuts3_84 <-  spTransform(Sweden_Nuts3, CRS(ehype_subbasins_EU@proj4string@projargs))  
}
if(!identical(proj4string(hydrostations_sweden),proj4string(ehype_subbasins_EU))){
  hydrostations_sweden <-  spTransform(hydrostations_sweden, CRS(ehype_subbasins_EU@proj4string@projargs))  
}


########################################################################################################
# load hype runoff timeseries
load("data/ts_ehype.RData")
load("data/ts_shype_tot.RData")
load("data/ts_shype_cor.RData")
load("data/ts_shype_nat.RData")



########################################################################################################
# select ehype & shype subbasins that contain hydrostations 
ehype_subbasins_SE = ehype_subbasins_EU[!is.na(over(ehype_subbasins_EU,as(hydrostations_sweden,"SpatialPoints"))),]
shype_subbasins_SE = shype_subbasins_Sweden[!is.na(over(shype_subbasins_Sweden,as(hydrostations_sweden,"SpatialPoints"))),]

# select subbasins within certain region
#ehype_subbasins_SE = ehype_subbasins_EU[!is.na(over(as(ehype_subbasins_EU,"SpatialPolygons"),as(Sweden_Nuts3_84,"SpatialPolygons"))),]

ehype_subids_download <- as.character(ehype_subbasins_SE@data$SUBID)
shype_subids_download <- as.character(shype_subbasins_SE@data$SUBID)

plot(ehype_subbasins_SE)
plot(shype_subbasins_SE)


#test
temp <- hydrostations_sweden[which(hydrostations_sweden@data$name == "Letsi"),]
test1 = ehype_subbasins_SE[!is.na(over(ehype_subbasins_SE,as(temp,"SpatialPoints"))),]
test2 = shype_subbasins_SE[!is.na(over(shype_subbasins_SE,as(temp,"SpatialPoints"))),]

ts1 <- as.data.frame( ts_ehype[,as.character(test1@data$SUBID)])
ts2 <- as.data.frame( ts_shype_cor[,as.character(test2@data$SUBID)])


ts1$date <- ts_ehype$date
ts1$year  <- as.numeric(getYear(as.Date(ts1$date)))
ts1 <- filter(ts1, year >= 2000 & year <=2010)

ts2$date <- ts_shype_cor$date
ts2$year  <- as.numeric(getYear(as.Date(ts2$date)))
ts2 <- filter(ts2, year >= 2000 & year <=2010)

#as.character(test2@data$SUBID)
#ts3 <- read_excel("shype_data/33979.xls", sheet = "Dygnsvärden", col_names = FALSE, col_types = NULL, na = "NA", skip=3)
#ts3$year  <- as.numeric(getYear(as.Date(ts3$X1)))
#ts3 <- filter(ts3, year >= 2000 & year <=2010)

ts_final <- data.frame(ts1$date, ts1$`ts_ehype[, as.character(test1@data$SUBID)]`, ts2$`ts_shype_cor[, as.character(test2@data$SUBID)]`)
ts_final$date <- as.Date(ts_final$date)
names(ts_final) <- c("date","ehype","shype_cor")

ggplot(melt(ts_final, id.vars = "date"),aes(date,value, color=variable)) +geom_line()
plot(ts1$ehype,ts1$shype)
cor(ts1$ehype,ts1$shype)

########################################################################################################
########################################################################################################
hydrostations_sweden_results  <- as.data.frame(hydrostations_sweden[,1])

# calculate power output
# Pa = μ ρ q g h 
# where
# Pa = power available (W)
# μ = efficiency (in general in the range 0.75 to 0.95)
# ρ = density (kg/m3) (~ 1000 kg/m3 for water)
# q = water flow (m3/s)
# g = acceleration of gravity (9.81 m/s2)
# h = falling height, head (m)

calc_power_output <- function(water_flow,height,capacity){
  # hydropower output parameters
  efficency = 0.9   # turbine efficiency
  density   = 1000  # kg/m3 for water
  g         = 9.81  # acceleration of gravity (9.81 m/s2)
  hours     = 24
  
  # calculate power output
  pa <- efficency * density * water_flow * g * height / 1E6 # in MW
  # output limited to plant capacity
  pa[pa>capacity] <- capacity 
  # daily power production
  p_daily <- pa * 24
}

#create dataframes to store power output timeseries of hydro stations 
ts_ehype_power      <- data.frame(ts_ehype$date)
ts_shype_tot_power <- data.frame(ts_shype_tot$date)
ts_shype_cor_power <- data.frame(ts_shype_cor$date)
ts_shype_nat_power <- data.frame(ts_shype_nat$date)

names(ts_ehype_power)      <- "date"
names(ts_shype_tot_power)  <- "date"
names(ts_shype_cor_power)  <- "date"
names(ts_shype_nat_power) <- "date"



# ehype
  for (i in seq(hydrostations_sweden_data$ehype_id)){
    water_flow     <- ts_ehype[,as.character(hydrostations_sweden_data$ehype_id[i])]
    height         <- hydrostations_sweden_data$height[i]  # falling height, head (m)
    capacity       <- hydrostations_sweden_data$capacity[i] / 1000 # installed capacity in MW
    
    ts_ehype_power[,i+1] <- calc_power_output(water_flow,height,capacity)
    names(ts_ehype_power)[i+1] <- as.character(hydrostations_sweden_data$name[i] )
  }

ts_ehype_power$year  <- as.numeric(getYear(as.Date(ts_ehype_power$date)))
ts_ehype_power$month <- as.numeric(getMonth(as.Date(ts_ehype_power$date)))
ts_ehype_power$date <- NULL

ts_ehype_power_year <-  ts_ehype_power %>% group_by(year) %>% summarise_each(funs(sum))
ts_ehype_power_year <- filter(ts_ehype_power_year, year >= 2000 & year <=2010)
#ts_ehype_power_year$date <- paste(ts_ehype_power_year$year, ts_ehype_power_year$month, sep = "-")
#ts_ehype_power_year$date <- as.Date(paste(ts_ehype_power_year$date,"-01",sep=""))


# shype tot
for (i in seq(hydrostations_sweden_data$shype_id)){
  water_flow     <- ts_shype_tot[,as.character(hydrostations_sweden_data$shype_id[i])]
  height         <- hydrostations_sweden_data$height[i]  # falling height, head (m)
  capacity       <- hydrostations_sweden_data$capacity[i] / 1000 # installed capacity in MW
  
  ts_shype_tot_power[,i+1] <- calc_power_output(water_flow,height,capacity)
  names(ts_shype_tot_power)[i+1] <- as.character(hydrostations_sweden_data$name[i] )
}

ts_shype_tot_power$year  <- as.numeric(getYear(as.Date(ts_shype_tot_power$date)))
ts_shype_tot_power$month <- as.numeric(getMonth(as.Date(ts_shype_tot_power$date)))
ts_shype_tot_power$date <- NULL

ts_shype_tot_power_year <-  ts_shype_tot_power %>% group_by(year) %>% summarise_each(funs(sum))
ts_shype_tot_power_year <- filter(ts_shype_tot_power_year, year >= 2000 & year <=2010)
#ts_shype_tot_power_year$date <- paste(ts_shype_tot_power_year$year, ts_shype_tot_power_year$month, sep = "-")
#ts_shype_tot_power_year$date <- as.Date(paste(ts_shype_tot_power_year$date,"-01",sep=""))


# shype nat
for (i in seq(hydrostations_sweden_data$shype_id)){
  water_flow     <- ts_shype_nat[,as.character(hydrostations_sweden_data$shype_id[i])]
  height         <- hydrostations_sweden_data$height[i]  # falling height, head (m)
  capacity       <- hydrostations_sweden_data$capacity[i] / 1000 # installed capacity in MW
  
  ts_shype_nat_power[,i+1] <- calc_power_output(water_flow,height,capacity)
  names(ts_shype_nat_power)[i+1] <- as.character(hydrostations_sweden_data$name[i] )
}

ts_shype_nat_power$year  <- as.numeric(getYear(as.Date(ts_shype_nat_power$date)))
ts_shype_nat_power$month <- as.numeric(getMonth(as.Date(ts_shype_nat_power$date)))
ts_shype_nat_power$date <- NULL

ts_shype_nat_power_year <-  ts_shype_nat_power %>% group_by(year) %>% summarise_each(funs(sum))
ts_shype_nat_power_year <- filter(ts_shype_nat_power_year, year >= 2000 & year <=2010 )
#ts_shype_nat_power_year$date <- paste(ts_shype_nat_power_year$year, ts_shype_nat_power_year$month, sep = "-")
#ts_shype_nat_power_year$date <- as.Date(paste(ts_shype_nat_power_year$date,"-01",sep=""))

# shype cor
for (i in seq(hydrostations_sweden_data$shype_id)){
  water_flow     <- ts_shype_cor[,as.character(hydrostations_sweden_data$shype_id[i])]
  height         <- hydrostations_sweden_data$height[i]  # falling height, head (m)
  capacity       <- hydrostations_sweden_data$capacity[i] / 1000 # installed capacity in MW
  
  ts_shype_cor_power[,i+1] <- calc_power_output(water_flow,height,capacity)
  names(ts_shype_cor_power)[i+1] <- as.character(hydrostations_sweden_data$name[i] )
}

ts_shype_cor_power$year  <- as.numeric(getYear(as.Date(ts_shype_cor_power$date)))
ts_shype_cor_power$month <- as.numeric(getMonth(as.Date(ts_shype_cor_power$date)))
ts_shype_cor_power$date <- NULL

ts_shype_cor_power_year <-  ts_shype_cor_power %>% group_by(year) %>% summarise_each(funs(sum))
ts_shype_cor_power_year <- filter(ts_shype_cor_power_year, year >= 2000 & year <=2010 )
#ts_shype_cor_power_year$date <- paste(ts_shype_cor_power_year$year, ts_shype_cor_power_year$month, sep = "-")
#ts_shype_cor_power_year$date <- as.Date(paste(ts_shype_cor_power_year$date,"-01",sep=""))


#Harspranget <- data.frame(ts_ehype_power_year$year,ts_ehype_power_year$Harsprånget,ts_shype_tot_power_year$Harsprånget, ts_shype_nat_power_year$Harsprånget)
#names(Harspranget) <- c("year","ehype","shype_tot","shype_nat")

ts_ehype_power_year$dataset <- "ehype"
ts_shype_tot_power_year$dataset     <- "shype_tot"
ts_shype_nat_power_year$dataset    <- "shype_nat"
ts_shype_cor_power_year$dataset    <- "shype_cor"

ts_hype <- rbind(ts_ehype_power_year,ts_shype_tot_power_year,ts_shype_cor_power_year, ts_shype_nat_power_year)

# total annual production
hydro_prod_annual <- hydrostation_annual_prod  %>% summarise_each(funs(sum))


# select stations from lule river
#summary(hydrostations_sweden_data$river)
#Indalsälven Luleälven Umeälven
hydrostations_lule <- as.character(hydrostations_sweden_data$name[which(hydrostations_sweden_data$river == "Luleälven")])
hydrostations_lule <- c(hydrostations_lule,"dataset","year")
ts_hype_sel <- ts_hype[,match(hydrostations_lule, names(ts_hype))]

ggplot(melt(ts_hype_sel, id.vars = c("year","dataset")), aes(year,value/1E3, color=dataset)) + geom_line() +facet_wrap(~variable)

# annual normal production
hydrostation_annual_prod <- ts_hype %>% group_by(dataset) %>% summarise_each(funs(mean))
temp <- hydrostation_annual_prod$dataset
hydrostation_annual_prod$dataset <- NULL
hydrostation_annual_prod$month   <- NULL
hydrostation_annual_prod$year   <- NULL
hydrostation_annual_prod <- as.data.frame(t(hydrostation_annual_prod))
names(hydrostation_annual_prod) <- temp

hydrostation_annual_prod_rel <- hydrostation_annual_prod  /hydrostations_sweden_data$normal_prod
hydrostation_annual_prod_rel$name <- row.names(hydrostation_annual_prod_rel)

hydrostation_annual_prod$leif <- hydrostations_sweden_data$normal_prod
temp_plot <- melt(hydrostation_annual_prod, id.vars = "leif") 


ggplot(temp_plot, aes(value/1E3,leif/1E3, color=variable)) + geom_jitter() + coord_fixed() +  
  scale_y_continuous( breaks=seq(0,1000,100), limits=c(0, 1000)) +
  scale_x_continuous( breaks=seq(0,1000,100), limits=c(0, 1000)) +
  geom_segment(aes(x = 0, y = 0, xend = 1000, yend = 1000),color="black")

#ggplotly()

