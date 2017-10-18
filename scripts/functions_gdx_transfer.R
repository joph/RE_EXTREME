source("scripts/libraries.R")


#costsCSV="../data/costs/costs.csv"

prepareFullRun<-function(period,
                         out="../gms_execute/input_tr.gdx",
                         hydFile="../data/hydro/hydro_data.csv",
                         hydFeather="../data/hydro/shype_hydro_nat_ts.feather",
                         windFeather="../data/wind/wind_1979_2014.feather",
                         solarFeather="../data/solar/solar_GAMS.feather",
                         loadFeather="../data/load/load_2007_2015.feather",
                         transmissionCSV="../data/transmission/lineCapacities.csv",
                         investCSV="../data/investOptions/investOpts.csv",
                         intermittentCSV="../data/investOptions/intermittentOpts.csv"){

  
  print("Preparing hydro data...")
  ptm <- proc.time()
  hydroData<-read_delim(hydFile,delim=";") 
  
  hydroTimeSeries<-read_feather(hydFeather)   %>%
    filter(date>=period[1]&date<=period[2]) %>% 
    mutate(Index=as.numeric(substr(region,3,100))) %>% 
    dplyr::select(date,Index,mwh)
  
  
  
  hydroPs<-prepareHydroParameters(hydroData,hydroTimeSeries)
  
  print(paste("Elapsed",(proc.time() - ptm)))
  ptm <- proc.time()
  print("Preparing intermittent data...")
  wind<-read_feather(windFeather)  %>%
    filter(Date>=period[1]&Date<=period[2]) %>% mutate(iTechnology="Wind") %>% select(Date,R,P,iTechnology,capFact)
  
  pv<-read_feather(solarFeather) %>%
    filter(Date>=period[1]&Date<=period[2]) %>% mutate(iTechnology="PV") %>% select(Date,R,P,iTechnology,capFact)
  
  rf<-bind_rows(wind,pv)
  
  intermittentPs<-prepareIntermittentParameters(rf)
  print(paste("Elapsed",(proc.time() - ptm)))
  ptm <- proc.time()
  print("Preparing load data...")
  load<-read_feather(loadFeather)  %>%   
    filter(Date>=period[1]&Date<=period[2]) 
  
  loadPs<-prepareLoadParameter(load)
  
  cap<-read_delim(transmissionCSV,delim=";") 
  transmissionPs<-prepareTransmissionParameters(cap)
  
  investOpts<-read_delim(investCSV,delim=";")
  investPs<-prepareInvestOptsParameters(investOpts)
  
  intermittentOpts<-read_delim(intermittentCSV,delim=";")
  intermittentOptPs<-prepareIntermittentOptsParameters(intermittentOpts)
  
  ####length & t-index
  s<-seq(as.POSIXct(period[1]),as.POSIXct(period[2]),by="h") 
  setT<-list(format(s,"%Y%m%d%H%M"))
  setUEL<-list(createSET("t",setT))
  length<-tibble(setT[[1]],rep(1,length(s)))
  lengthPs<-tidyToGDXparam("length",length,c(1),2) %>%
    list
  
  ######WHAT LACKS
  ######INCORPORATING VARIABLE TIME LENGTH!!!
  combine<-c(hydroPs,
              intermittentPs,
              loadPs,
              investPs,
              transmissionPs,
              lengthPs,
              intermittentOptPs,
              setUEL)
  
  writeToGDXList(combine,out)
  print(paste("Elapsed",(proc.time() - ptm)))
}

###runs gams file and reads results
runGAMS<-function(){
  
  
  
  setwd(paste(base_dir,"/../gms_execute",sep=""))
  gams("../RE_EXTREME/gms_source/changing_time_resolution.gms")
  setwd(base_dir)
 
}

####helper function
####to remove leading characters of GDX index
substringAd<-function(x,length){
  return(as.numeric(sapply(x,substring,length+1)))
  
}

####write tidy data to GDX
####SPEED UP THIS CODE!
tidyToGDXparam<-function(name,tb,uelColumns,valColumn){
  ###uels->to numeric
  start<-sapply(tb[,uelColumns],
                function(x){
                  as.numeric(factor(unlist(x),
                                    levels=unique(sort(unlist(x)))
                                    ))
                  }
                )
  
  leadingIndexUEL<-as_tibble(start)
  value<-bind_cols(leadingIndexUEL,tibble(tb[[valColumn]]))

  
  ###generate uels
  uels_<-sapply(tb[,uelColumns],function(x){as.character(sort(unique(x)))},simplify=FALSE)
  
  ###create param
  return(createPARAM(name,value,uels_,dim=length(uelColumns)))
  
}

###prepares the basic parameters for hydropower production
prepareHydroParameters<-function(hydroData,hydroTimeSeries){
  hydroData$Region=paste("SE",sprintf("%03d", as.numeric(substring(hydroData$Region,3,100))),sep="")
  hydroData$River=paste("RS",sprintf("%03d", as.numeric(substring(hydroData$River,3,100))),sep="")
  hydroData$downRiver=paste("HP",sprintf("%03d", as.numeric(substring(hydroData$downRiver,3,100))),sep="")
  hydroData$Plant=paste("HP",sprintf("%03d", as.numeric(substring(hydroData$Plant,3,100))),sep="")
  
  
  
  ###simple parameters which are directly extracted from the input table
  names<-list("minFlow",
              "maxFlow",
              "maxHydPower",
              "maxReservoir",
              "runOffDelay",
              "hydroConvFact",
              "initReservoir",
              "termReservoir")
  uels<-2:4
  valColumns<-names
  
  params<-mapply(tidyToGDXparam,
         name=names,
         valColumn=valColumns,
         MoreArgs=list(tb=hydroData,uelColumns=uels),
         SIMPLIFY=FALSE
  )
  

  ###upRiver has to be derived from downriver
  downRiver<-select(hydroData,2:4,downRiver) 
  upRiver<-inner_join(downRiver,downRiver,by=c("Plant" = "downRiver")) %>%
    select(Region.x,River.x,Plant,Plant.y) %>%
    mutate(val=1)
  #u1<-upRiver[,3]
  #upRiver[,3]<-upRiver[,4]
  #upRiver[,4]<-u1
  params[[length(params)+1]]<-tidyToGDXparam(name="upRiver",
                                             tb=upRiver,
                                             uelColumns=1:4,
                                             valColumn="val")
  
  ###join hydro timeseries...
  outTs<-inner_join(hydroData,hydroTimeSeries,by=c("id"="Index")) 
  nmbDivisions<-nrow(unique(outTs[,2:4]))
  TimeIndex<-format(outTs$date,"%Y%m%d%H%M")
  
  outTs<-mutate(outTs,
                TimeIndex=TimeIndex)
  
  
  params[[length(params)+1]]<-tidyToGDXparam(name="hydro",
                 tb=outTs,
                 uelColumns=c(2,ncol(outTs),3:4),
                 valColumn="mwh")
  
  
  
  return(params)
         
}

###prepares the basic parameters for windpower production
prepareIntermittentParameters<-function(rf){
  rf$R=paste("SE",sprintf("%03d", as.numeric(substring(rf$R,3,100))),sep="")
  rf$P=paste("P",sprintf("%03d", as.numeric(substring(rf$P,2,100))),sep="")
  
  TimeIndex<-format(rf$Date,"%Y%m%d%H%M")
  rf<-mutate(rf,
             TimeIndex=TimeIndex) %>% select(R,TimeIndex,P,iTechnology,capFact)
  
  tidyToGDXparam(name="intermittent",
                 tb=rf,
                 uelColumns=c(1:4),
                 valColumn="capFact") %>%
        list() %>%
    return()
}

#prepares the load parameter
prepareLoadParameter<-function(load){
  load$Region=paste("SE",sprintf("%03d", as.numeric(substring(load$Region,3,100))),sep="")
  TimeIndex<-format(load$Date,"%Y%m%d%H%M")
  load<-mutate(load,
             TimeIndex=TimeIndex)
  
  tidyToGDXparam(name="load",
                 tb=load,
                 uelColumns=c(2,ncol(load)),
                 valColumn="Load") %>%
    list() %>%
    return()
}



###prepare transmission parameters
prepareTransmissionParameters<-function(cap){
  cap$Region1=paste("SE",sprintf("%03d", as.numeric(substring(cap$Region1,3,100))),sep="")
  cap$Region2=paste("SE",sprintf("%03d", as.numeric(substring(cap$Region2,3,100))),sep="")
  
  tidyToGDXparam(name="transmissionCap",
                 tb=cap,
                 uelColumns=c(2:3),
                 valColumn="Max") %>%
    list() %>%
    return()
  
}

prepareInvestOptsParameters<-function(io){
  io$Region=paste("SE",sprintf("%03d", as.numeric(substring(io$Region,3,100))),sep="")
  io$P=paste("P",sprintf("%03d", as.numeric(substring(io$P,2,100))),sep="")
  
  tidyToGDXparam(name="investOptions",
                 tb=io,
                 uelColumns=c(2:5),
                 valColumn="Value") %>%
    list() %>%
    return()
  
}

prepareIntermittentOptsParameters<-function(io){
  io$Region=paste("SE",sprintf("%03d", as.numeric(substring(io$Region,3,100))),sep="")
  io$P=paste("P",sprintf("%03d", as.numeric(substring(io$P,2,100))),sep="")
  
  tidyToGDXparam(name="intermittentOptions",
                 tb=io,
                 uelColumns=c(2:5),
                 valColumn="Value") %>%
    list() %>%
    return()
  
}

prepareCostsParameters<-function(costs){
  tidyToGDXparam(name="costs",
                 tb=costs,
                 uelColumns=c(1),
                 uelPrefix=list("C"),
                 valColumn="value") %>%
    list() %>%
    return()
  
  
}

###creates uel index used for wgdx
###prescript:string used to start the GAMS index
###c: vector with elements of GAMS index
createUEL<-function(prescript,c){
  return(list(paste(prescript,c,sep="")))  
}

###create set for GAMS
###name: name of set
###UEL: corresponding uel list(s)
###dim:dimension of set
createSET<-function(name,UEL,dim=1){
  return(list(name=name,type="set",dim=dim,uels=UEL))
}

###create GAMS Parameter
###obj: GDX object
createPARAMObj<-function(obj){
  
  val<-as.matrix(obj$data)
  return(list(name=obj$name,type="parameter",dim=obj$dimension,form="sparse",uels=obj$uels,val=val))
  
}



###create GAMS Parameter
###name: name of parameter
###value: data.frame or matrix with values for parameter
###uels: list of uel lists
##dim: dimension of parameter
createPARAM<-function(name,value,uels,dim=1){
  
  val<-as.matrix(value)
  return(list(name=name,type="parameter",dim=dim,form="sparse",uels=uels,val=val))
  
}

###reads results from a GAMS file and puts it in a list, where list names are
###equal to GAMS variable names
###f is filename
###names is a vector of variable names (GAMS)
###uels is a list of lists: each list element is a list. It is associated with the GAMS variable
###and contains the set(s) for that variable.
readResultsGeneric<-function(f,names){
  
  setwd(paste(base_dir,"/../gms_execute",sep=""))
  res<-sapply(names,function(x,file){return(rgdx(file,squeeze=FALSE,list(name=x)))},f,simplify=FALSE)
  
  names(res)<-names
  setwd(base_dir)
  return(res)
  
}

###reads one symbol and returns tibble with values and index (i.e. uels)
readSingleSymbolGDX<-function(symbol){
  name<-symbol$name
  if(symbol$dim==0){
    if(length(symbol$val)==1){
      return(tibble(name=name,value=as.vector(symbol$val)))
      
    }
    return(tibble(name=name,value=symbol$val))
  }
  
  ###construct data.frame
  out<-tibble(rep(symbol$name,nrow(symbol$val)))
  for(i in 1:(symbol$dim)){

      out<-bind_cols(out,tibble(symbol$uels[[i]][symbol$val[,i]]))
  }
  out<-bind_cols(out,tibble(symbol$val[,i+1]))
  names(out)<-c("name",symbol$domains,"value")
  
  
  return(out)

}


#####read input of file and construct large-data.frame with results

readTimeResults<-function(f,timeResolutionVars,period){
    
  timeVar<-readResultsGeneric(f,timeResolutionVars) %>% sapply(readSingleSymbolGDX,simplify=FALSE)
  
  timeVar<-bind_rows(timeVar) %>% 
    mutate(TT=t) %>% mutate(datetime=as.POSIXct(TT,format="%Y%m%d%H%M")) 
  
  timeVar %>% return()

}


readModelResults<-function(in_,f,period,runname){
  timeResolutionVars<-list("x_term",
                           "x_renew",
                           "x_hydro",
                           "x_spill",
                           "x_h_stor_in",
                           "x_h_stor_out",
                           "x_h_stor_lv",
                           "x_hyd_up",
                           "x_stor_in",
                           "x_stor_out",
                           "x_stor_lev",
                           "x_curtail",
                           "load",
                           "transfer_net",
                           "x_transfer",
                           "x_invest_thermal_cap",
                           "x_invest_storage",
                           "x_invest_intermittent",
                           "bal_",
                           "hydro")
#                          "x_transfer_in",
#                          "x_transfer_out")
#                          "x_slack") 
  
  dir.create(file.path("../runs", runname), showWarnings = FALSE)
  file.copy(paste("../gms_execute/",f,sep=""),paste("../runs/",runname,sep=""))
  file.copy(paste("../gms_execute/",in_,sep=""),paste("../runs/",runname,sep=""))
  file.copy(paste("gms_source/","changing_time_resolution.gms",sep=""),paste("../runs/",runname,sep=""))
  return(readTimeResults(f,timeResolutionVars,period))
}

readModelResultsAG<-function(in_,f,period,runname){
  timeResolutionVars<-list("x_term",
                           "x_renew",
                           "x_hydro",
                           "x_spill",
                           "x_h_stor_in",
                           "x_h_stor_out",
                           "x_h_stor_lv",
                           "x_hyd_up",
                           "x_stor_in",
                           "x_stor_out",
                           "x_stor_lev",
                           "x_curtail",
                           "load",
                           "transfer_net",
                           "x_transfer",
                           "x_invest_thermal_cap",
                           "x_invest_storage",
                           "x_invest_intermittent",
                           "bal_",
                           "hydro")
  
  dir.create(file.path("../runs/", runname), showWarnings = FALSE)
  file.copy(paste("../gms_execute/",f,sep=""),paste("../runs/",runname,sep=""))
  file.copy(paste("../gms_execute/",in_,sep=""),paste("../runs/",runname,sep=""))
  file.copy(paste("gms_source/","changing_time_resolution.gms",sep=""),paste("../runs/",runname,sep=""))
  return(readTimeResults(f,timeResolutionVars,period))
}

readModelResultsTransfer<-function(f,period){
  vars<-list("x_transfer")
  var<-readResultsGeneric(f,vars) %>% sapply(readSingleSymbolGDX,simplify=FALSE)
  return(var)
  #var<-bind_rows(var) %>% 
  #  mutate(TT=t) %>% mutate(Time=as.numeric(substr(TT,2,100)))
  
  #timeSeq<-tibble(datetime=seq(as.POSIXct(period[1]),as.POSIXct(period[2]),"h")) %>% 
  #  mutate(ord=order(datetime))
  
  #inner_join(var,timeSeq,by=c("Time"="ord")) %>% return()
  
  
}



calculateCompleteRegionalDS<-function(results){
  
  #calculate complete hydroproduction
  reg_p_t<-c("x_term",
    "x_renew",
    "x_stor_in",
    "x_stor_out",
    "x_stor_lev",
    "intermittent")
  
  reg_ws_p_t<-c(
    "x_hydro",
    "x_spill",
    "x_h_stor_in",
    "x_h_stor_out",
    "x_h_stor_lv",
    "x_hyd_up",
    "hydro"
    )
  
  reg_t<-c("x_curtail",
         "x_loss",
         "load")
  
  reg_reg_t<-c("x_transfer_in",
               "x_transfer_out")
  
  t<-c("length")
  
  single<-c("x_invest_thermal_cap",
            "x_invest_storage",
            "x_invest_intermittent")
  
  
  
  
  s<-NULL
  for(i in c(reg_p_t,reg_ws_p_t,reg_t,reg_reg_t)){
    s1<-results[[i]] %>% mutate(name=i) %>% group_by(name,reg,t) %>% summarize(value=sum(value))
    s1$timenew<-as.numeric(substr(s1$t,2,nchar(s1$t)))
    s<-bind_rows(s,s1)
  }
  
  return(s)
  
  
}


####calculates the full balance of supply
calculateFullBalance<-function(rds){
  
 rds$value[rds$name=="x_h_stor_out"] <-rds$value[rds$name=="x_h_stor_out"]*0.99
 rds$value[rds$name=="x_stor_out"] <-rds$value[rds$name=="x_stor_out"]*0.9

  sum_supply<-filter(rds,name=="x_hydro"|
          name=="x_h_stor_out"|
          name=="x_term"|
          name=="x_renew"|
          name=="x_stor_out"|
          name=="x_loss"|
          name=="x_transfer") %>% group_by(reg,t,timenew) %>% summarize(sum=sum(value))
  
 sum_negative<-filter(rds,name=="x_curtail"|
                        name=="x_stor_in") %>% group_by(reg,t,timenew) %>% summarize(sum=-1*sum(value))
  
 join<-full_join(sum_supply,sum_negative,by=c("reg","t","timenew"))
 join$sum.x[is.na(join$sum.x)]<-0
 join$sum.y[is.na(join$sum.y)]<-0
 
 fout<-mutate(join,fsupply=sum.x+sum.y)
 return(fout)
  
}







###takes a list of GDX objects and writes them to file
###gdxList: list of objects
###name: file name to write to
writeToGDXList<-function(gdxList,name){
  
  setwd(base_dir)
  do.call(wgdx.lst,c(name,gdxList))
  
}