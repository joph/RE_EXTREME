###prepare input data
base_dir<-"D:/google drive/Anträge/formas/RE_EXTREME/RE_EXTREME/scripts"
setwd(base_dir)
source("functions.R")

####load input data from files
setwd("../data/wind/SCENARIOS AND TIME SERIES OF FUTURE WIND POWER")
wind<-read.table("B1.csv",sep=";",header=TRUE)
wind_all<-apply(wind,1,sum)
###installed capacities according to scenario B1
installed_wind_capacities<-c(1151,3108,2629,2886)
wind_cap_fact<-t(t(wind[,2:5])/installed_capacities)

setwd(base_dir)
setwd("../data/electricity production/")
elec_prod<-read.table("n_fot2001-2014.csv",sep=";",header=TRUE)

####restrict to certain period
length_wind_dataset<-length(wind_all)
wind_all_2001_2014<-wind_all[(length_wind_dataset-365*24*14-12-59):length_wind_dataset]*1.205

hourly_wind<-wind_cap_fact
hourly_hydro<-elec_prod$vattenkraft
hourly_other<-elec_prod$ospecificerad

intermittent_hourly<-data.frame(hourly_wind)[1:(52560),]
hydropower_hourly<-data.frame(hourly_hydro/1000+hourly_other/1000)[1:(52560),,drop=FALSE]
hourly_load<-elec_prod$produktion[1:52560]/1000

###generate variable length timeseries 
cc<-generateVariableResolutionTimeSeries(hourly_load,intermittent_hourly, hydropower_hourly,10,-1,60,60,TRUE,c(100:8000))
matplot(cc[1:500,1:3],type="l")
nrow(cc)
length(all)
apply(cc,2,sum)

setwd(base_dir)
writeToGDX(cc,4)


###fixed length
cc<-data.frame(renewable_production_hourly_final,hourly_load,1)
writeToGDX(cc)

