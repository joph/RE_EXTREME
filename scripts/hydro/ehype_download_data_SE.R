# resources
# http://r4ds.had.co.nz/transform.html

# ggmap     extends the plotting package ggplot2 for maps
# rgdal     R’s interface to the popular C/C++ spatial data processing library gdal
# rgeos     R’s interface to the powerful vector processing library geos
# maptools  provides various mapping functions
# tmap      a new packages for rapidly creating beautiful maps
# dplyr and tidyr  fast and concise data manipulation packages
#install.packages("broom")

library(readxl)
library(ggplot2)
library(ggmap)
library(plotly)
library(gdata)
library(foreign)
library(plyr)
library(broom)
library(maptools)
library(sp)
library(rgdal)
library(reshape2)


########################################################################################################
# load dataset
ehype_subbasins_EU         <- readOGR(dsn = "hype_data/ehype", layer = "e-hype3_subids")
shype_subbasins_Sweden     <- readOGR(dsn = "gis_data", layer = "sweden_sub_river_basins")
#hydrostations_sweden       <- readOGR(dsn = "gis_data", layer = "sweden_hydrostation_test")
Sweden_Nuts3               <- readOGR(dsn = "gis_data/SE_NUTS3", layer = "SE_NUTS3")
hydrostations_sweden_data  <- read.csv(file="vattenkraft_info.csv", sep=";" )

xy <- hydrostations_sweden_data[,c("Long","Lat")]
hydrostations_sweden <- SpatialPointsDataFrame(coords = xy, data = hydrostations_sweden_data, proj4string = CRS(ehype_subbasins_EU@proj4string@projargs))
remove(xy)

# select subregion  for smaller test case
#Sweden_sel <- Sweden_Nuts3[which(Sweden_Nuts3@data$NUTS_ID == "SE231"),]

# prepare for ggplot
Sweden_ggmap <- tidy(Sweden_Nuts3)
Sweden_Nuts3_labels <- data.frame(Sweden_Nuts3@data$NUTS_ID,coordinates(Sweden_Nuts3))
names(Sweden_Nuts3_labels) <- c("NUTS3_region","Longitude", "Latitude")

#hydrostations_sweden_data <- as.data.frame(hydrostations_sweden@data)

# define map extent from Nuts3 region shapefile
sbbox <- make_bbox(lon = Sweden_ggmap$long, lat = Sweden_ggmap$lat, f = .1)
# get basemap from google maps
sq_map <- get_map(location = sbbox, zoom=5,  maptype = "terrain", source = "google")
Sweden_map <- ggmap(sq_map)

# plot NUTS3 regions and hydropower stations
Sweden_map +
  geom_polygon(data=Sweden_ggmap, aes(long, lat, group=group), colour='black', fill=NA) +
  geom_point(data=hydrostations_sweden_data,aes(Long, Lat,size=capacity, colour=height)) +
  geom_text(data=Sweden_Nuts3_labels, aes(Longitude, Latitude, label = NUTS3_region), size=2) +
  theme_bw() 

# test plotly
viz <-  ggplot() +
  geom_polygon(data=Sweden_ggmap, aes(long, lat, group=group), colour='darkgrey', fill=NA) +
  geom_point(data=hydrostations_sweden_data,aes(Long, Lat, size=capacity, colour=height, group=name)) +
  theme_bw() + guides(group = FALSE)  + coord_fixed() 
ggplotly()

ggplotly(filename="Sweden_hydro_stations")


########################################################################################################
# check and adjust projections

# check if datasets are already prjected
summary(ehype_subbasins_EU)
summary(hydrostations_sweden)
summary(Sweden_Nuts3)
#proj4string(Sweden_Nuts3)

# reproject Sweden shapefiles to same projection as ehype_data
Sweden_Nuts3_84 <-  spTransform(Sweden_Nuts3, CRS(ehype_subbasins_EU@proj4string@projargs))
Sweden_sel_84 <-  spTransform(Sweden_sel, CRS(ehype_subbasins_EU@proj4string@projargs))

# check if projections are identical
identical(proj4string(Sweden_Nuts3_84),proj4string(ehype_subbasins_EU))


########################################################################################################
# select subbasins that contain hydrostations 
ehype_subbasins_SE = ehype_subbasins_EU[!is.na(over(ehype_subbasins_EU,as(hydrostations_sweden,"SpatialPoints"))),]

# select subbasins within certain region
#ehype_subbasins_SE = ehype_subbasins_EU[!is.na(over(as(ehype_subbasins_EU,"SpatialPolygons"),as(Sweden_Nuts3_84,"SpatialPolygons"))),]

ehype_subids_download <- as.character(ehype_subbasins_SE@data$SUBID)
plot(ehype_subbasins_SE)



########################################################################################################
# download ehype timeseries

# filetype of download files
filetype  <- ".xls"
# download location
url_ehype <- "http://hypeweb.smhi.se/europehype/time-series/download/"

# prepare output file
download.file("http://hypeweb.smhi.se/europehype/time-series/download/8801544", "hype_data/8801544.xls", mode="wb")
temp <- read_excel("hype_data/8801544.xls", 1, col_names = TRUE, col_types = NULL, na = "", skip=0)
ehype_data <- as.data.frame(matrix(rep(NA), nrow = length(temp$Date), ncol = 1))
names(ehype_data) <- "date"
ehype_data$date <- temp$Date
i=1

for ( i in seq_along(ehype_subids_download))
  #for ( i in 1:3)
  {
  ehype_save <- paste("hype_data/",ehype_subids_download[i],filetype,sep="")
  # download file if not available locally
    if (!file.exists(ehype_save)) {
      url_file  <- paste(url_ehype,ehype_subids_download[i], sep = "")
      download.file(url_file, ehype_save, mode="wb")  
    }
  temp <- read_excel(ehype_save, 1, col_names = TRUE, col_types = NULL, na = "", skip=0)
  names(temp) <- c("day",ehype_subids_download[i])
  #store in dataframe old - style
  ehype_data[,i+1] <- temp[2]
  #if (file.exists(ehype_save)) file.remove(ehype_save)
}



########################################################################################################
# calculate power output
# Pa = μ ρ q g h 
# where
# Pa = power available (W)
# μ = efficiency (in general in the range 0.75 to 0.95)
# ρ = density (kg/m3) (~ 1000 kg/m3 for water)
# q = water flow (m3/s)
# g = acceleration of gravity (9.81 m/s2)
# h = falling height, head (m)


test_station <- hydrostations_sweden[1,]
ehype_subbasins_SE_select = ehype_subbasins_EU[!is.na(over(ehype_subbasins_EU,as(test_station,"SpatialPoints"))),]
temp <- as.character(ehype_subbasins_SE_select@data$SUBID)
test_station_runoff <- ehype_data[,temp]

efficency = 0.9
density   = 1000                # kg/m3 for water
g         = 9.81                # acceleration of gravity (9.81 m/s2)
height    = test_station$height  # falling height, head (m)
capacity  = test_station$capacity / 1000 # installed capacity in MW
hours     = 24

pa <- efficency * density * test_station_runoff * g * height
pa <- pa / 1E6 # in MW
pa[pa>capacity] <- capacity
temp        <- data.frame(as.Date(ehype_data$date), pa)
names(temp) <- c("date","pa")
pa_2005     <- filter(temp,date >= "2005-01-01" & date <= "2005-12-31")

temp <- pa_2005$pa * 24
sum(temp)


summary(pa)
sum(pa_2005$pa)


##### plot
plot1 <- melt(ehype_data, id = 1, na.rm = TRUE)
plot1 <- data.frame(ehype_data$date, pa)
plot1$date <- as.Date(plot1$ehype_data.date)


ggplot(plot1, aes(date, pa)) + geom_line() +
  scale_x_date(date_breaks = "5 years", date_minor_breaks = "1 year", date_labels = "%Y") +
  xlab("") + ylab("power output")


ggplot(pa_2005, aes(date, pa)) + geom_line() +
  scale_x_date(date_breaks = "1 months", date_minor_breaks = "1 month", date_labels = "%M") +
  xlab("") + ylab("power output")



# test for ArcMap
mean_runoff <- colMeans(ehype_data[,1:ncol(ehype_data)])
test <- coastal_subbasins_id[1:(ncol(ehype_data)-1)]
arcmaptest <- as.data.frame(test)
arcmaptest$runoff <- mean_runoff

write.table(arcmaptest,file="arcmaptest.csv",sep = ";",dec=",")



#################################
# playing around with shapefiles
library(maptools)
library(rgeos)
library(sp)

# load shapefiles
#coastal_subbasins_EU=readShapePoly("hype_data/ehype/CoastalSUBIDs.shp",verbose=TRUE)
ehype_subbasins_EU=readShapePoly("hype_data/ehype/e-hype3_subids.shp",verbose=TRUE)
subbasins_sweden=readShapePoly("gis_data/sweden_sub_river_basins.shp",verbose=TRUE)
hydrostations_sweden=readShapePoints("gis_data/sweden_hydrostation_test.shp",verbose=TRUE)
Austria_Nuts1=readShapePoly("gis_data/AT_NUTS1/STATISTIK_AUSTRIA_NUTS1_20160101.shp",verbose=TRUE)

ehype_subbasins_data <- as.data.frame(ehype_subbasins_EU)

class(coastal_subbasins_EU)
plot(hydrostations_sweden)
plot(Austria_Nuts1)

# select e_hype subbasins within Austria
test2 = ehype_subbasins_EU[!is.na(over(ehype_subbasins_EU,as(Austria_Nuts1,"SpatialPolygons"))),]

# select subbasins that contain hydrostations
#p <-SpatialPoints(list(lng,lat), proj4string=crswgs84)
#gContains(fdg,pt)
test2 = subbasins_sweden[!is.na(over(subbasins_sweden,as(hydrostations_sweden,"SpatialPoints"))),]
class(test2)
nrow(test2)
plot(test2)

test3 <- as.data.frame(test2)


#gContains(region,SpatialPoints(points[i,1:2],proj4string=CRS(proj4string(region)))))
test <- gContains(subbasins_sweden,hydrostations_sweden[2,])
names(hydrostations_sweden)
hydrostations_sweden[2,]
test


