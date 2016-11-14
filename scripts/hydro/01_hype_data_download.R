# rgdal     R’s interface to the popular C/C++ spatial data processing library gdal
# rgeos     R’s interface to the powerful vector processing library geos
# maptools  provides various mapping functions
# dplyr and tidyr  fast and concise data manipulation packages
#install.packages("broom","tidyr","dplyr")

library(rgdal)
library(readxl)
library(foreign)
library(ncdf4)
library(sp)


########################################################################################################
# load spatial datasets
ehype_subbasins_EU         <- readOGR(dsn = "gis_data/ehype_riverbasins", layer = "e-hype3_subids")
shype_subbasins_Sweden     <- readOGR(dsn = "gis_data/shype_riverbasins", layer = "Sweden_Riverbasins_SubID")
Sweden_Nuts3               <- readOGR(dsn = "gis_data/SE_NUTS3", layer = "SE_NUTS3")

hydrostations_sweden_data  <- read.csv(file="data/vattenkraft_info.csv", sep=";" )
xy <- hydrostations_sweden_data[,c("Long","Lat")]
hydrostations_sweden <- SpatialPointsDataFrame(coords = xy, data = hydrostations_sweden_data, proj4string = CRS(ehype_subbasins_EU@proj4string@projargs))
remove(xy)

save(hydrostations_sweden_data, file = "data/hydrostations_sweden.RData")

# adjust projections if they are not identical
if(!identical(proj4string(Sweden_Nuts3),proj4string(ehype_subbasins_EU))){
  Sweden_Nuts3 <-  spTransform(Sweden_Nuts3, CRS(ehype_subbasins_EU@proj4string@projargs))  
}
if(!identical(proj4string(hydrostations_sweden),proj4string(ehype_subbasins_EU))){
  hydrostations_sweden <-  spTransform(hydrostations_sweden, CRS(ehype_subbasins_EU@proj4string@projargs))  
}

# are shype and ehype identical
identical(proj4string(shype_subbasins_Sweden),proj4string(ehype_subbasins_EU))



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

# save ehype and shype riverbasin subids into hydrostation dataset
for (i in seq(nrow(hydrostations_sweden_data))){
  temp  = ehype_subbasins_SE[!is.na(over(ehype_subbasins_SE,as(hydrostations_sweden[i,],"SpatialPoints"))),]
  temp2 = shype_subbasins_SE[!is.na(over(shype_subbasins_SE,as(hydrostations_sweden[i,],"SpatialPoints"))),]
  hydrostations_sweden_data$ehype_id[i] <- temp@data$SUBID
  hydrostations_sweden_data$shype_id[i] <- as.character(temp2@data$SUBID)
  print(i)
}


remove(ehype_subbasins_EU,shype_subbasins_Sweden)

########################################################################################################
# download ehype timeseries

# filetype of download files
filetype  <- ".xls"
# download location
url_ehype <- "http://hypeweb.smhi.se/europehype/time-series/download/"

# prepare output file
download.file("http://hypeweb.smhi.se/europehype/time-series/download/8801544", "ehype_data/8801544.xls", mode="wb")
temp <- read_excel("ehype_data/8801544.xls", 1, col_names = TRUE, col_types = NULL, na = "", skip=0)
ts_ehype <- as.data.frame(matrix(rep(NA), nrow = length(temp$Date), ncol = 1))
names(ts_ehype) <- "date"
ts_ehype$date <- temp$Date

# combine timeseries from excel files into one dataframe
for ( i in seq_along(ehype_subids_download))
  #for ( i in 1:3)
{
  ehype_save <- paste("ehype_data/",ehype_subids_download[i],filetype,sep="")
  # download file if not available locally
  if (!file.exists(ehype_save)) {
    url_file  <- paste(url_ehype,ehype_subids_download[i], sep = "")
    download.file(url_file, ehype_save, mode="wb")  
  }
  temp <- read_excel(ehype_save, 1, col_names = TRUE, col_types = NULL, na = "", skip=0)
  names(temp) <- c("day",ehype_subids_download[i])
  #store in dataframe old - style
  ts_ehype[,i+1] <- temp[2]
  #if (file.exists(ehype_save)) file.remove(ehype_save)
}

# write.table(ts_ehype, file="data/timeseries_ehype.csv",  qmethod = "double", dec = ".", sep = ";", row.names = FALSE)
save(ts_ehype, file = "data/ts_ehype.RData")

########################################################################################################
# download shype timeseries


# filetype of download files
filetype  <- ".xls"
# download location
url_shype <- "http://vattenwebb.smhi.se/modelarea/basindownload/"

# prepare output file
download.file("http://vattenwebb.smhi.se/modelarea/basindownload/31672", "shype_data/31672.xls", mode="wb")
temp <- read_excel("shype_data/31672.xls", sheet = "Dygnsvärden", col_names = FALSE, col_types = NULL, na = "NA", skip=3)
temp <- as.data.frame(temp)
names(temp) <- c("date","runoff_tot","runoff_cor","runoff_nat","runoff_local","temperature")

shype_data <- as.data.frame(matrix(rep(NA), nrow = nrow(temp), ncol = 1))
shype_data$date <- temp$date
shype_data[,1] <- NULL
ts_shype_tot  <- shype_data
ts_shype_cor  <- shype_data
ts_shype_nat <- shype_data
remove(shype_data)

# combine timeseries from excel files into one dataframe
for ( i in seq_along(shype_subids_download))
  {
  shype_save <- paste("shype_data/",shype_subids_download[i],filetype,sep="")
  
  # download file if not available locally
    if (!file.exists(shype_save)) {
      url_file  <- paste(url_shype,shype_subids_download[i], sep = "")
      download.file(url_file, shype_save, mode="wb")  
    }
  
  temp <- read_excel(shype_save, sheet = "Dygnsvärden", col_names = FALSE, col_types = NULL, na = "NA", skip=3)
  temp <- as.data.frame(temp)
  #names(temp) <- c("date","runoff_tot","runoff_cor","runoff_nat","runoff_local","temperature")
  
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

# write.table(ts_shype_tot, file="data/ts_shype_tot.csv",  qmethod = "double", dec = ".", sep = ";", row.names = FALSE)
# write.table(ts_shype_cor, file="data/ts_shype_cor.csv",  qmethod = "double", dec = ".", sep = ";", row.names = FALSE)
# write.table(ts_shype_nat, file="data/ts_shype_nat.csv",  qmethod = "double", dec = ".", sep = ";", row.names = FALSE)

save(ts_shype_tot, file = "data/ts_shype_tot.RData")
save(ts_shype_cor, file = "data/ts_shype_cor.RData")
save(ts_shype_nat, file = "data/ts_shype_nat.RData")
