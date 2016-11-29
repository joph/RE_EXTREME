###sample test function

test.generateVariableResolutionTimeSeries<-function(){
  ###generate test data
  length<-24*40
  load<-(3*runif(length))
  renewable<-data.frame(runif(length),runif(length))
  hydro<-data.frame(runif(length))
  renewable_scaling<-1
  lower<-0
  upper<-2
  threshhold<-2    
  
  ####checks if sum of production is equal before and after application of function
  res<-generateVariableResolutionTimeSeries(load,renewable,hydro,renewable_scaling,lower,upper,threshhold)
  res_comp<-apply(res[,1:(ncol(res)-1)],2,sum)
  dat_comp<-apply(data.frame(renewable,hydro,load),2,sum)
  res_comp<-unname(res_comp)
  dat_comp<-unname(dat_comp)
  checkEqualsNumeric(res_comp,dat_comp)
  
  ####checks if all data is used if limit is chosen appropriately
  lower<-100
  upper<- -1000
  res<-generateVariableResolutionTimeSeries(load,renewable,hydro,renewable_scaling,lower,upper,threshhold)
  checkEquals(length(load),nrow(res))  
  
  ####checks if all data is reduced if limit is chosen appropriately
  lower<- -10000
  upper<- 10000
  res<-generateVariableResolutionTimeSeries(load,renewable,hydro,renewable_scaling,lower,upper,threshhold)
  checkEquals(length(load)/24,nrow(res))  
  
  
  
}

test.GAMSModel.TC1<-function(){
  
  ####Test case
  ####One region, 24 timesteps
  ####no hydro, no intermittent production
  ####1 thermal power plant
  ####i.e. total supply from thermal power plant should equal load
  ####installed capacity should be equal to maximum load
  setwd(base_dir)
  ###create one hydropower plant without inflows
  tribble(
    ~id, ~Region, ~River, ~Plant, ~minFlow, ~maxFlow, ~maxReservoir, ~maxHydPower,~runOffDelay, ~hydroConvFact, ~downRiver,
    #--|--------|-------|-------|---------|---------|--------------|-------------|-------------|---------------|-----------|
    1,   "SE1"  , "RS1" , "HP1" ,     0   ,    0    ,       0      ,       1000  ,        1    ,        0      ,    "HP0",
    2,   "SE1"  , "RS1" , "HP2" ,     0   ,    0    ,       0      ,       1000  ,        1    ,        0      ,    "HP1"
    
  ) %>%  write_delim(path="../data/test/hydro/hydro_tc1.csv",delim=";")
  
  date<-as.POSIXct("2014-01-01 01:00:00")
  date1<-as.POSIXct("2014-01-01 02:00:00")
  
  tibble(Time=c(date,date),
         Index=c(1,2),
         HydropowerProduction=c(0,0)) %>% 
    write_feather("../data/test/hydro/hydro_ts_tc1.feather")
  
  
  #####one windpower plant without flows
  tibble(Date=c(date,date1),
         R=c("SE1","SE1"),
         WindPower=c(0,0),
         P=c("P1","P1"),
         capFact=c(0,0)) %>% 
    write_feather("../data/test/wind/wind_ts_tc1.feather")
  
  #####load runs from 1 to 48
  tibble(Date=seq(as.POSIXct("2014-01-01 00:00:00"),as.POSIXct("2014-01-02 23:00:00"),by="h"),
         Region=c("SE1"),
         Load=1:48) %>% write_feather("../data/test/load/load_ts_tc1.feather")
  
  # Transmission capacities - 1 region only, so no transfer
  tribble(
    ~id,~Region1,~Region2,~Max,
    #--|--------|--------|----
    1, "SE1"  , "SE2"  ,  0
  ) %>% write_feather("../data/test/transmission/cap_tc1.feather")
  
  #One thermal power plant
  tribble(
    ~id,~Region, ~P ,~Technology,~Val,
    #--|-------|----|-----------|----
    1 , "SE1" ,"P1", "TEC1"    , 1 
  ) %>% write_delim(path="../data/test/investOptions/investOpts_tc1.csv",delim=";")
  
  tribble(
    ~param,~value,
    #-----|------|
    "C1"  ,  50  ,#variable thermal
    "C2"  ,100000,#loss
    "C3"  ,     1,#investment thermal
    "C4"  ,100000,#investment storage
    "C5"  ,500000 #investment intermittent
  ) %>% write_delim(path="../data/test/costs/costs_tc1.csv",delim=";")
  
  
  period<-c("2014-01-01 00:00:00 CET"
            , "2014-01-02 23:00:00 CET")
  
  
  
  prepareFullRun(period,
                 hydFile="../data/test/hydro/hydro_tc1.csv",
                 hydFeather="../data/test/hydro/hydro_ts_tc1.feather",
                 windFeather="../data/test/wind/wind_ts_tc1.feather",
                 loadFeather="../data/test/load/load_ts_tc1.feather",
                 transmissionFeather="../data/test/transmission/cap_tc1.feather",
                 investCSV="../data/test/investOptions/investOpts_tc1.csv",
                 costsCSV="../data/test/costs/costs_tc1.csv")
  
  results<-runGAMSReadResults()
  checkEquals(sum(results$load$value-results$x_term$value),0)
  checkEquals(results$x_invest_thermal_cap$value,48)
  
  
  
  
}

test.GAMSModel.TC2<-function(){
  ##########TEST CASE 2 - switch costs for variable thermal and loss
  ##########should make loss the preferred choice!
  
  tribble(
    ~param,~value,
    #-----|------|
    "C1"  ,100000,#variable thermal
    "C2"  ,  50  ,#loss
    "C3"  ,     1,#investment thermal
    "C4"  ,100000,#investment storage
    "C5"  ,500000 #investment intermittent
  ) %>% write_delim(path="../data/test/costs/costs_tc2.csv",delim=";")
  
  
  period<-c("2014-01-01 00:00:00 CET"
            , "2014-01-02 23:00:00 CET")
  
  prepareFullRun(period,
                 hydFile="../data/test/hydro/hydro_tc1.csv",
                 hydFeather="../data/test/hydro/hydro_ts_tc1.feather",
                 windFeather="../data/test/wind/wind_ts_tc1.feather",
                 loadFeather="../data/test/load/load_ts_tc1.feather",
                 transmissionFeather="../data/test/transmission/cap_tc1.feather",
                 investCSV="../data/test/investOptions/investOpts_tc1.csv",
                 costsCSV="../data/test/costs/costs_tc2.csv")
  
  results<-runGAMSReadResults()
  checkEquals(sum(results$load$value-results$x_loss$value),0)
  checkEquals(nrow(results$x_invest_thermal_cap),0)
  
}

test.GAMSModel.TC3<-function(){

##########TEST CASE 3 - hydropower plant cascade

###create hydropower plants
  tribble(
    ~id, ~Region, ~River, ~Plant, ~minFlow, ~maxFlow, ~maxReservoir, ~maxHydPower,~runOffDelay, ~hydroConvFact, ~downRiver,
    #--|--------|-------|-------|---------|---------|--------------|-------------|-------------|---------------|-----------|
    1,   "SE1"  , "RS1" , "HP1" ,     0   ,    100  ,       0      ,       1000  ,        0      ,    1        ,    "HP2",
    2,   "SE1"  , "RS1" , "HP2" ,     0   ,    100  ,       0      ,       1000  ,        0      ,    1        ,    "HP0"
    
  ) %>%  write_delim(path="../data/test/hydro/hydro_tc3.csv",delim=";")
  

date<-as.POSIXct("2014-01-01 01:00:00")
date1<-as.POSIXct("2014-01-01 02:00:00")

tibble(Time=rep(seq(as.POSIXct("2014-01-01 00:00:00"),as.POSIXct("2014-01-02 23:00:00"),by="h"),2),
       Index=c(rep(1,48),rep(2,48)),
       HydropowerProduction=c(rep(1,96))) %>% 
  write_feather("../data/test/hydro/hydro_ts_tc3.feather")

tibble(Date=seq(as.POSIXct("2014-01-01 00:00:00"),as.POSIXct("2014-01-02 23:00:00"),by="h"),
       Region=c("SE1"),
       Load=c(2,rep(3,47))) %>% write_feather("../data/test/load/load_ts_tc3.feather")

period<-c("2014-01-01 00:00:00 CET"
          , "2014-01-02 23:00:00 CET")

prepareFullRun(period,
               hydFile="../data/test/hydro/hydro_tc3.csv",
               hydFeather="../data/test/hydro/hydro_ts_tc3.feather",
               windFeather="../data/test/wind/wind_ts_tc1.feather",
               loadFeather="../data/test/load/load_ts_tc3.feather",
               transmissionFeather="../data/test/transmission/cap_tc1.feather",
               investCSV="../data/test/investOptions/investOpts_tc1.csv",
               costsCSV="../data/test/costs/costs_tc2.csv")

results<-runGAMSReadResults()
hyd<-results$x_hydro %>% group_by(reg,t,ws) %>% summarize(v1=sum(value))

checkEquals(sum(hyd[,4]-results$load$value),0)
checkEquals(nrow(results$x_invest_thermal_cap),0)

}

test.GAMSModel.TC4<-function(){

##########TEST CASE 4 - hydropower plant cascade with minimum flows

###create two hydropower plants
setwd(base_dir)
  tribble(
    ~id, ~Region, ~River, ~Plant, ~minFlow, ~maxFlow, ~maxReservoir, ~maxHydPower,~runOffDelay, ~hydroConvFact, ~downRiver,
    #--|--------|-------|-------|---------|---------|--------------|-------------|-------------|---------------|-----------|
    1,   "SE1"  , "RS1" , "HP1" ,     1   ,    100  ,       100    ,       1000  ,        1    ,        0      ,    "HP0",
    2,   "SE1"  , "RS1" , "HP2" ,     1   ,    100  ,       100    ,       1000  ,        1    ,        0      ,    "HP1"
    
  ) %>%  write_delim(path="../data/test/hydro/hydro_tc4.csv",delim=";")
  

tibble(Date=seq(as.POSIXct("2014-01-01 00:00:00"),as.POSIXct("2014-01-02 23:00:00"),by="h"),
       Region=c("SE1"),
       Load=c(2,rep(1,45),10,10)) %>% write_feather("../data/test/load/load_ts_tc4.feather")


period<-c("2014-01-01 00:00:00 CET"
          , "2014-01-02 23:00:00 CET")

prepareFullRun(period,
               hydFile="../data/test/hydro/hydro_tc4.csv",
               hydFeather="../data/test/hydro/hydro_ts_tc3.feather",
               windFeather="../data/test/wind/wind_ts_tc1.feather",
               loadFeather="../data/test/load/load_ts_tc4.feather",
               transmissionFeather="../data/test/transmission/cap_tc1.feather",
               investCSV="../data/test/investOptions/investOpts_tc1.csv",
               costsCSV="../data/test/costs/costs_tc2.csv")

results<-runGAMSReadResults()
rds<-calculateCompleteRegionalDS(results) 
fb<- rds %>% calculateFullBalance()

comp<-filter(rds,name=="load") %>% 
  arrange(timenew) %>% 
  .$value %>% 
  neg((arrange(fb,timenew)$fsupply))

checkEquals(sum(abs(comp)),0)

}



neg<-function(a,b)
{return (a-b)}

