source("scripts/libraries.R")
####Reads and Writes Load data in Tidy format to disk
readAndWriteXLSLoad<-function(){

  #2007-2010
  load<-NULL
  for(i in 2007:2010){
    
    ####read in excel sheet. First read first header line, then second header line.
    sheet1_title<-read_excel(paste("../data/load/timvarden",i,".xls",sep=""),col_names=TRUE)
    sheet1<-read_excel(paste("../data/load/timvarden",i,".xls",sep=""),col_names=TRUE,skip=1)
    
    ####assign names, read values
    names(sheet1)<-paste(names(sheet1_title),names(sheet1),sep=".")
    #load1<-select(sheet1,1,2,11:14)
    load1<-select(sheet1,1,2,3:22)
    load1<-mutate(load1,SE1=apply(load1[,seq(3,22,4)],1,sum)*-1,
                        SE2=apply(load1[,seq(4,22,4)],1,sum)*-1,
                        SE3=apply(load1[,seq(5,22,4)],1,sum)*-1,
                        SE4=apply(load1[,seq(6,22,4)],1,sum)*-1)
    
    names(load1)[1:2]<-c("Date","Time")
    load1<-load1 %>% select(Date,Time,SE1,SE2,SE3,SE4)
    
    
    
    ####correct date
    load1$Date<-ymd(load1$Date)
    load1$Time<-as.numeric(load1$Time)
    s<-sapply(load1$Time,nchar)
    load1$Time[s==1]<-"0000"
    load1$Time[s==3]<-paste("0",load1$Time[s==3],sep="")
    date<-ymd_hm(paste(load1$Date,load1$Time))
    
    ####construct final tibble
    load2<-add_column((load1[,3:6]),date,.before=1)
    names(load2)<-c("Date",names(load2)[2:5])
    load2[,2:5]<-load2[,2:5]
    if(is.null(load)){
      load<-load2
    }else{
      load<-bind_rows(load,load2)
    }
  }
  matplot(load[,2:5],type="l")
  
  #2010-II
  sheet1<-read_excel("../data/load/timvarden2010II.xls",col_names=TRUE)
  names(sheet1)<-c(paste("C",1:ncol(sheet1),sep=""))
  load4<-bind_cols(sheet1[,1],
                   sheet1[,c(2:43)])
  load4_<-mutate(load4,SE1=apply(load4[,c(2,9,13,36)],1,sum)*-1,
                       SE2=apply(load4[,c(3,6,10,14,37)],1,sum)*-1,
                       SE3=apply(load4[,c(4,7,11,15,38)],1,sum)*-1,
                       SE4=apply(load4[,c(5,8,12,16,39)],1,sum)*-1) %>% 
                       select(1,44:47)
  
  
  names(load4_)<-c("Date","SE1","SE2","SE3","SE4")
  load4_$Date<-dmy_hm(load4_$Date)
  load4_[,2:5]<-load4_[,2:5]
  
  load<-bind_rows(load,load4_)
  matplot(load[,2:5],type="l")  
  
  sheet1<-read_excel(paste("../data/load/timvarden",2011,".xls",sep=""),col_names=FALSE,skip=5)
  load1<-bind_cols(sheet1[,1],sheet1[,c(2:12,33:40)])
  load1_<-mutate(load1,SE1=apply(load1[,c(2,9,13,17)],1,sum)*-1,
                 SE2=apply(load1[,c(3,6,10,14,18)],1,sum)*-1,
                 SE3=apply(load1[,c(4,7,11,15,19)],1,sum)*-1,
                 SE4=apply(load1[,c(5,8,12,16,20)],1,sum)*-1) %>% 
                 select(1,21:24)
  
  
  names(load1_)<-c("Date","SE1","SE2","SE3","SE4")
 
  load<-bind_rows(load,load1_)
  matplot(load[,2:5],type="l")  
  
  
  #2011-2015
  for(i in 2012:2015){
    sheet1<-read_excel(paste("../data/load/timvarden",i,".xls",sep=""),col_names=FALSE,skip=5)
    if(i==2012){
      load1<-bind_cols(sheet1[,1],sheet1[,c(2:13,35:42)])
    }else{
      load1<-bind_cols(sheet1[,1],sheet1[,c(2:13,34:41)])
    }
    load1_<-mutate(load1,SE1=apply(load1[,c(2,6,10,14,18)],1,sum)*-1,
                         SE2=apply(load1[,c(3,7,11,15,19)],1,sum)*-1,
                         SE3=apply(load1[,c(4,8,12,16,20)],1,sum)*-1,
                         SE4=apply(load1[,c(5,9,13,17,21)],1,sum)*-1) %>%
                         select(1,22:25)
    
   
    names(load1_)<-c("Date","SE1","SE2","SE3","SE4")
    load<-bind_rows(load,load1_)
  }
  #plot(load[,1],type="l")
  
  load[,1]<-seq(as.POSIXct("2007-01-01 00:00"),as.POSIXct("2015-12-31 23:00"),by="h")
  
  ###for some reason, the first 7 hours are lacking in the model
  ###we replace them with the first 7 hours of the second day
  load[1:7,2:5]<-load[25:31,2:5]
  
  loadTidy<-load %>% gather(2:5,key="Region",value="Load") 
  ggplot(loadTidy, aes(x=Date, y=Load)) +
                geom_line(aes(color=Region)) 
  loadTidy %>% group_by(Date) %>% 
            summarise(totLoad = sum(Load)) %>% 
            ggplot(aes(x=Date,y=totLoad)) +
            geom_line()
  
  write_delim(loadTidy,path="../data/load/load_2007_2015.csv",delim=";")
  write_feather(loadTidy,path="../data/load/load_2007_2015.feather") 
  test<-read_feather("../data/load/load_2007_2015.feather")
  
  
  print("Wrote data/load/load_2007_2015.csv and data/load/load_2007_2015.feather to disk.")
  
}

####read and write wind data in tidy format to disk
readAndWriteWindData<-function(){
  
  wind<-read_delim("../data/wind/B1.csv",delim=";")[,2:5]
  wind<-tibble(seq(as.POSIXct("1979-01-01 00:00"),as.POSIXct("2014-12-31 23:00"),by="h")) %>%
            bind_cols(wind)
  windData<-read_delim("../data/wind/wind_data.csv",delim=";")
  
  windTidy<-wind %>% gather(2:5,key="R",value="WindPower") %>% mutate(P="P1")
  names(windTidy)<-c("Date",names(windTidy)[2:4])
  
  jWind<-full_join(windTidy,windData,by=c("R" = "R", "P" = "P"))
  nrow(jWind)
  
  windTidy<-mutate(windTidy,capFact=WindPower/(jWind)$MaxCapacity)
  
  ggplot(windTidy, aes(x=Date, y=WindPower)) +
    geom_line(aes(color=Region)) 
  windTidy %>% group_by(Date) %>% 
    summarise(totWind = sum(WindPower)) %>% 
    ggplot(aes(x=Date,y=totWind)) +
    geom_line()
  
  
  write_delim(windTidy,path="../data/wind/wind_1979_2014.csv",delim=";")
  write_feather(windTidy,path="../data/wind/wind_1979_2014.feather") 
  print("Wrote ../data/wind/wind_1979_2014.csv and ../data/wind/wind_1979_2014.feather to disk.")

}

readAndWriteGridCapacity<-function()
{
  
  cap<-read_delim("../data/transmission/lineCapacities.csv",delim=";")
  write_feather(cap,path="../data/transmission/lineCapacities.feather")
  
  
}

####create test hydro data
testHydroData<-function(){
  

  shype_hydro_nat <- read_feather("../data/hydro/shype_hydro_nat_ts.feather") %>% 
    mutate(Time=date,Index=as.numeric(as.factor(region)),HydropowerProduction=mwh) %>% 
    select(Time,as.integer(Index),HydropowerProduction) %>% 
    write_feather("../data/hydro/timeseries.feather")
  
 
  
}

