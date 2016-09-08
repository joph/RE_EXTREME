###prepare input data
base_dir<-"D:/google drive/Anträge/formas/RE_EXTREME/RE_EXTREME/scripts"
setwd(base_dir)
source("functions.R")

setwd("../data/wind/SCENARIOS AND TIME SERIES OF FUTURE WIND POWER")
wind<-read.table("B1.csv",sep=";",header=TRUE)
wind_all<-apply(wind,1,sum)
setwd(base_dir)
setwd("../data/electricity production/")
elec_prod<-read.table("n_fot2001-2014.csv",sep=";",header=TRUE)

length_wind_dataset<-length(wind_all)
wind_all_2001_2014<-wind_all[(length_wind_dataset-365*24*14-12-59):length_wind_dataset]*1.205

hourly_wind<-t(t(wind[(length_wind_dataset-365*24*14-12-59):length_wind_dataset,2:5]/apply(wind[(length_wind_dataset-365*24*14-12-59):length_wind_dataset,2:5],2,max)))
hourly_hydro<-elec_prod$vattenkraft
hourly_other<-elec_prod$ospecificerad
df_hourly<-data.frame(hourly_wind,hourly_hydro/1000,hourly_other/1000)[1:(52560),]
hourly_load<-elec_prod$produktion[1:52560]/1000

###aggregation and test for run_length
all<-apply(df_hourly*1.5,1,sum)

df_hourly_final<-df_hourly
df_hourly_final[,5]<-df_hourly[,5]+df_hourly[,6]
df_hourly_final<-df_hourly_final[,1:5]

###generate timeseries - HERE THE MAGIC HAPPENS!
cc<-generateVariableResolutionTimeSeries(all,hourly_load,df_hourly_final,0,1)
matplot(cc[,1:3],type="l")
nrow(cc)
length(all)
apply(cc,2,sum)

####variable length
setwd(base_dir)
writeCCToGDX(cc)

###fixed length
cc<-data.frame(df_hourly_final,hourly_load,1)
writeCCToGDX(cc)

