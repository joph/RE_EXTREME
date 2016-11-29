source("libraries.R")



prepareFullRun<-function(period,
                         hydFile="../data/hydro/hydro_data.csv",
                         hydFeather="../data/hydro/timeseries.feather",
                         windFeather="../data/wind/wind_1979_2014.feather",
                         loadFeather="../data/load/load_2007_2015.feather",
                         transmissionFeather="../data/transmission/lineCapacities.feather",
                         investCSV="../data/investOptions/investOpts.csv",
                         costsCSV="../data/costs/costs.csv"){

  
   hydroData<-read_delim(hydFile,delim=";")
   hydroTimeSeries<-read_feather(hydFeather) %>%
   filter(Time>=period[1]&Time<=period[2]) 
  
  hydroPs<-prepareHydroParameters(hydroData,hydroTimeSeries)
  
  rf<-read_feather(windFeather)  %>%
    filter(Date>=period[1]&Date<=period[2]) 
  
  intermittentPs<-prepareIntermittentParameters(rf)
  
  load<-read_feather(loadFeather)  %>%   
    filter(Date>=period[1]&Date<=period[2]) 
  
  loadPs<-prepareLoadParameter(load)
  
  cap<-read_feather(transmissionFeather)
  transmissionPs<-prepareTransmissionParameters(cap)
  
  investOpts<-read_delim(investCSV,delim=";")
  investPs<-prepareInvestOptsParameters(investOpts)
  
  costs<-read_delim(costsCSV,delim=";")
  costsPs<-prepareCostsParameters(costs)
  
  ####length & t-index
  s<-seq(as.POSIXct(period[1]),as.POSIXct(period[2]),by="h") %>% length
  setT<-createUEL("T",1:s) 
  setUEL<-list(createSET("t",setT))
  length<-tibble(paste("T",1:s,sep=""),rep(1,length(s)))
  lengthPs<-tidyToGDXparam("length",length,c(1),list("T"),2) %>%
    list
  
  ######WHAT LACKS
  ######INCORPORATING VARIABLE TIME LENGTH!!!
  combine<-c(hydroPs,intermittentPs,loadPs,transmissionPs,investPs,lengthPs,setUEL,costsPs)
  writeToGDXList(combine,"../../gms_execute/input_tr.gdx")
}

###runs gams file and reads results
runGAMSReadResults<-function(){
  
  
  
  setwd(paste(base_dir,"/../../gms_execute",sep=""))
  gams("../RE_EXTREME/gms_source/changing_time_resolution.gms")
  setwd(base_dir)
  return(readResults("results_time_resolution.gdx"))
}

####helper function
####to remove leading characters of GDX index
substringAd<-function(x,length){
  return(as.numeric(sapply(x,substring,length+1)))
  
}

####write tidy data to GDX
tidyToGDXparam<-function(name,tb,uelColumns,uelPrefix,valColumn){
  ###uels->to numeric
  start<-sapply(uelPrefix,nchar)
  m<-mapply(substringAd,select(tb,uelColumns),start)
  if(is.vector(m)){
    m<-as.data.frame(t(m))
  }
  leadingIndexUEL<-as_tibble(m)
  value<-bind_cols(leadingIndexUEL,tibble(tb[[valColumn]]))
  
  ###generate uels
  uels_<-sapply(apply(leadingIndexUEL,2,max),seq,from=1,simplify=FALSE)
  uels_<-mapply(paste,uelPrefix,uels_,sep="",SIMPLIFY=FALSE)
  
  ###create param
  return(createPARAM(name,value,uels_,dim=length(uelColumns)))
  
}

###prepares the basic parameters for hydropower production
prepareHydroParameters<-function(hydroData,hydroTimeSeries){
  
  
  
  
  ###simple parameters which are directly extracted from the input table
  names<-list("minFlow",
              "maxFlow",
              "maxHydPower",
              "maxReservoir",
              "runOffDelay",
              "hydroConvFact")
  uels<-2:4
  uelNames<-list("SE","RS","HP")
  valColumns<-names
  
  params<-mapply(tidyToGDXparam,
         name=names,
         valColumn=valColumns,
         MoreArgs=list(tb=hydroData,uelColumns=uels,uelPrefix=uelNames),
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
                                             uelPrefix=list("SE","RS","HP","HP"),
                                             valColumn="val")
  
  ###join hydro timeseries...

  outTs<-inner_join(hydroData,hydroTimeSeries,by=c("id"="Index")) 
  nmbDivisions<-nrow(unique(outTs[,2:4]))
  outTs<-mutate(outTs,
                TimeIndex=paste("T",rep(c(1:(nrow(outTs)/nmbDivisions)),nmbDivisions),
                                sep=""))
  
  
  params[[length(params)+1]]<-tidyToGDXparam(name="hydro",
                 tb=outTs,
                 uelColumns=c(2,ncol(outTs),3:4),
                 uelPrefix=list("SE","T","RS","HP"),
                 valColumn="HydropowerProduction")
  
  
  
  return(params)
         
}

###prepares the basic parameters for windpower production
prepareIntermittentParameters<-function(rf){
  
  rf<-mutate(rf,
             TimeIndex=paste("T",rep(1:(nrow(rf)/length(unique(rf$R))),length(unique(rf$R))),
                             sep=""))
  
  tidyToGDXparam(name="intermittent",
                 tb=rf,
                 uelColumns=c(2,ncol(rf),4),
                 uelPrefix=list("SE","T","P"),
                 valColumn="capFact") %>%
        list() %>%
    return()
}

#prepares the load parameter
prepareLoadParameter<-function(load){
  
  load<-mutate(load,
             TimeIndex=paste("T",rep(1:(nrow(load)/length(unique(load$Region))),length(unique(load$Region))),
                             sep=""))
  
  tidyToGDXparam(name="load",
                 tb=load,
                 uelColumns=c(2,ncol(load)),
                 uelPrefix=list("SE","T"),
                 valColumn="Load") %>%
    list() %>%
    return()
}



###prepare transmission parameters
prepareTransmissionParameters<-function(cap){
  tidyToGDXparam(name="transmissionCap",
                 tb=cap,
                 uelColumns=c(2:3),
                 uelPrefix=list("SE","SE"),
                 valColumn="Max") %>%
    list() %>%
    return()
  
}

prepareInvestOptsParameters<-function(io){
  tidyToGDXparam(name="investOptions",
                 tb=io,
                 uelColumns=c(2:4),
                 uelPrefix=list("SE","P","TEC"),
                 valColumn="Val") %>%
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
  
  setwd(paste(base_dir,"/../../gms_execute",sep=""))
  res<-sapply(names,function(x,file){return(rgdx(file,list(name=x)))},f,simplify=FALSE)
  
  names(res)<-names
  setwd(base_dir)
  return(res)
  
}

###reads one symbol and returns tibble with values and index (i.e. uels)
readSingleSymbolGDX<-function(symbol){
  name<-symbol$name
  if(symbol$dim==0){
    
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
readResults<-function(f){
  
  
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
                        "x_loss",
                        "x_transfer",
                        "load",
                        "length",
                        "hydro",
                        "intermittent",
                        "x_invest_thermal_cap",
                        "x_invest_storage",
                        "x_invest_intermittent")
  
    
  timeVar<-readResultsGeneric(f,timeResolutionVars) %>% sapply(readSingleSymbolGDX,simplify=FALSE)
  
  timeIndex<-select(timeVar[[14]],3) 
  timeIndex<-bind_cols(timeIndex,tibble(as.numeric(substr(timeIndex$t,2,nchar(timeIndex$t)))))
  
  hh<-hash(keys=timeResolutionVars,values=timeVar)
  
  return(hh)
  
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
  
  reg_reg_t<-c("x_transfer")
  
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