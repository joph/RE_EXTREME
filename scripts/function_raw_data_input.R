source("libraries.R")
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
    load1<-select(sheet1,1,2,11:14)
    names(load1)<-c("Date","Time","SE1","SE2","SE3","SE4")
    
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
    load2[,2:5]<-load2[,2:5]*-1
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
                   sheet1[,2:5])
  names(load4)<-c("Date","SE1","SE2","SE3","SE4")
  load4$Date<-dmy_hm(load4$Date)
  load4[,2:5]<-load4[,2:5]*-1
  load<-bind_rows(load,load4)
  matplot(load[,2:5],type="l")  
  
  
  #2011-2015
  startindex<-c(2,2,2,2,2)
  jj<-1
  for(i in 2011:2015){
    sheet1<-read_excel(paste("../data/load/timvarden",i,".xls",sep=""),col_names=FALSE,skip=5)
    load1<-bind_cols(sheet1[,1],sheet1[,startindex[jj]:(startindex[jj]+3)])
    jj<-jj+1
    names(load1)<-c("Date","SE1","SE2","SE3","SE4")
    load1[,2:5]<-load1[,2:5]*-1
    load<-bind_rows(load,load1)
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
  
  print("Wrote ../data/load/load_2007_2015.csv and ../data/load/load_2007_2015.feather to disk.")
  
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
  write_feater(cap,path="../data/transmission/lineCapacities.feather")
  
  
}

####create test hydro data
testHydroData<-function(){
  
  period<-seq(as.POSIXct("1979-01-01 00:00"),
              as.POSIXct("2015-12-31 23:00"),by="h")
  n<-length(period)
  tb<-tibble("Time"=period,
             "1"=runif(n)*3600,
             "2"=runif(n)*1200,
             "3"=runif(n)*500,
             "4"=runif(n)*300,
             "5"=runif(n)*400,
             "6"=runif(n)*1000)  
  
  gather(tb,key="Index",val="HydropowerProduction",2:7) %>%
    mutate(Index=as.integer(Index)) %>%
    write_feather("../data/hydro/timeseries.feather")
  
}

