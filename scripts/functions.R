library(gdxrrw)
igdx("C:/GAMS/win64/24.2")
  
testf<-function(x){
  return(x)
}

###reads results from a GAMS file and puts it in a list, where list names are
###equal to GAMS variable names
###f is filename
###names is a vector of variable names (GAMS)
###uels is a list of lists: each list element is a list. It is associated with the GAMS variable
###and contains the set(s) for that variable.
readResults<-function(f,names,uels){
  
  setwd(paste(base_dir,"/../../gms_execute",sep=""))
  rgdx.scalar(f,"elapsed")
  names_uels<-list()
  for(i in 1:length(names)){
    names_uels[[length(names_uels)+1]]<-list(name=names[i],form="full",uels=uels[[i]])
  }
  
  res<-sapply(names_uels,
              function(names_uels,file){return(rgdx(file,names_uels)$val)},
              f,simplify=FALSE)
  
  names(res)<-names
  return(res)
  
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
###name: name of parameter
###value: data.frame or matrix with values for parameter
###uels: list of uel lists
##dim: dimension of parameter
createPARAM<-function(name,value,uels,dim=1){
  
  val<-as.matrix(value)
  return(list(name=name,type="parameter",dim=dim,form="sparse",uels=uels,val=val))
  
}

###create GAMS Parameter
###obj: GDX object
createPARAMObj<-function(obj){
  
  val<-as.matrix(obj$data)
  return(list(name=obj$name,type="parameter",dim=obj$dimension,form="sparse",uels=obj$uels,val=val))
  
}

###runs gams file and reads results
runGAMSReadResults<-function(timesteps,nmb_intermittent){
  
  
  
  setwd(paste(base_dir,"/../../gms_execute",sep=""))
  gams("../RE_EXTREME/gms_source/changing_time_resolution.gms")
  
  uels_t<-createUEL("t",1:timesteps)
  uels_loc<-createUEL("l",1:nmb_intermittent)
  
  readResults("results_time_resolution.gdx",
              c("x_invest_intermittent","x_invest_storage","x_invest_thermal_cap","totalCost","elapsed","x_term"),
              list(uels_loc,a,b,c,d,uels_t),
              )
  
}


####generates, from the input data, a timeseries that has variable length
###hourly_load: load per hour. The length has to be a multiple of 24.
###renewable_production_hourly: intermittent renewable production per hour. The length has to be a multiple of 24.
###hydropower_hourly: hydropower inflows per hour. The length has to be a multiple of 24.
###renewable_scaling: determines the installed capacity for renewables
###lower: the lower boundary for residual production (in MW) that defines which days are modelled in detail. Values below that value are taken into account.
###upper: the upper boundary for residual production (in MW) that defines which days are used in detail. Values above that value are taken into account.
###threshhold: number of hours that production has to be below (lower) or above (upper) production
###plot: indicates if a figure shold be plotted
###limits: indicates the x-axis limits for the plot
###returns a data.frame with combined renewable, hydropower and load production in dense format, i.e. aggregation. the last column indicates the aggregation level.
###i.e. the number of hours represented by the value
generateVariableResolutionTimeSeries<-function(hourly_load,intermittent_hourly,hydropower_hourly,renewable_scaling,lower,upper,threshhold,plot=TRUE, limits=c(1:800)){
  
  hydro_intermittent_prod<-data.frame(intermittent_hourly*renewable_scaling,hydropower_hourly)
  all_production<-apply(hydro_intermittent_prod,1,sum)
  
  residual_production<-(all_production-hourly_load)
  le1<-rle(residual_production<lower|residual_production>upper)
  ress<-rep(le1$lengths >= threshhold & le1$values,times = le1$lengths)
 
  if(plot){
    plot(residual_production[limits],type="l")
    residual_production1<-residual_production
    residual_production1[ress==FALSE]<-NA
    lines(residual_production1[limits],col="red")
    lines(c(0,length(limits)),c(lower,lower),lty=2,col="blue")
    lines(c(0,length(limits)),c(upper,upper),lty=2,col="blue")
  }
  
  cc<-NA
  for(i in seq(1,length(all_production),24)){
 
    df<-NA
    if(sum(ress[i:(i+23)]==0)){
      #daily values
      ren<-apply(intermittent_hourly[i:(i+23),],2,sum)
      hyd<-apply(hydropower_hourly[i:(i+23),,drop=FALSE],2,sum)
      df<-data.frame(t(c(ren)),t(c(hyd)),c(sum(hourly_load[i:(i+23)])),24)
    
      }
    else
      {
      #hourly values
      df<-data.frame(intermittent_hourly[i:(i+23),],hydropower_hourly[i:(i+23),,drop=FALSE],hourly_load[i:(i+23)],1)
    
    }
    names(df)<-c(paste(1:ncol(intermittent_hourly),"renewable"),"hydro","load","multi")
    cc<-rbind(cc,df)
  
  }
  
cc<-cc[2:nrow(cc),]
return(cc)
}

###writes model data to disk
writeToGDX<-function(regions,
                     load,
                     intermittent,
                     hydro,
                     timeslots,
                     hydroConvFact,
                     runOffDelay,
                     minFlow,
                     maxFlow,
                     maxReservoir){
  
  nmb_intermittent<-regions
  nmb_hydro<-regions
  
  ####write data to gdx file
  uels_t<-createUEL("t",1:nrow(load))
  t_set <-createSET("t",uels_t)

  uels_p<-createUEL("p",1:1)
  p_set<-createSET("p",uels_p)
  
  uels_reg<-createUEL("reg",1:nmb_intermittent)
  reg_set<-createSET("reg",uels_reg)
  
  
  ###intermittent generation
  val<-NA
  for(i in 1:nmb_intermittent){
    val<-rbind(val,
               data.frame(i,1,1:nrow(intermittent),intermittent[,i]))
      
  }
  val<-val[2:nrow(val),]
  uels<-list(uels_reg[[1]],uels_p[[1]],uels_t[[1]])
  intermittent_p<-createPARAM("intermittent",val,uels,3)
  
  ###hydropower

  
  ###load
  val<-NA
  for(i in 1:regions){
    val<-rbind(val,
               data.frame(i,1:nrow(load),load[,i]))
    
  }
  val<-val[2:nrow(val),]
  uels<-list(uels_reg[[1]],uels_t[[1]])
  load_p<-createPARAM("load",val,uels,2)
  
  ###length of timeslotes
  val<-cbind(1:nrow(load),timeslots)
  length_p<-createPARAM("length",val,uels_t)
  
  ###hydroConvFact
  uels<-list(uels_reg[[1]],uels_p[[1]])
  hydroConvFact_p<-createPARAM("hydroConvFact",hydroConvFact,uels,2)
  
  ###hydroConvFact
  runOffDelay_p<-createPARAM("runOffDelay",runOffDelay,uels,2)
  
  ###minFlow
  minFlow_p<-createPARAM("minFlow",minFlow,uels,2)
 
  ###maxFlow
  maxFlow_p<-createPARAM("maxFlow",maxFlow,uels,2)
  
  ###maxReservoir
  maxReservoir_p<-createPARAM("maxReservoir",maxReservoir,uels,2)
  
  
  setwd(base_dir)
  wgdx.lst("../../gms_execute/input_tr.gdx",reg_set,
           t_set,
           p_set,
           intermittent_p,
           hydro_p,
           load_p,
           length_p,
           hydroConvFact_p,
           runOffDelay_p,
           minFlow_p,
           maxFlow_p,
           maxReservoir_p)
  
}

###takes a list of GDX objects and writes them to file
###gdxList: list of objects
###name: file name to write to
writeToGDXList<-function(gdxList,name){
  
  setwd(base_dir)
  do.call(wgdx.lst,c(name,gdxList))
  
}

###prepares the basic parameters for hydropower production
prepareBasicHydroParameters<-function(hydro_time_series,uels_in){
 
  uels_reg<-uels_in[[1]]
  uels_p<-uels_in[[2]]
  uels_t<-uels_in[[3]]
  uels_ws<-uels_in[[4]]
  
  minFlow<-GAMSObject$new("minFlow",
                          data.frame(1000),
                          list(c(1:4),c(1),c(1)),
                          list("reg","ws","p"))$generateGDX()
  
  runOffDelay<-GAMSObject$new("runOffDelay",data.frame(rep(0,ncol(hydro_time_series))),
                          list(c(1:4),
                               c(1),c(1),c(1)),
                          list("reg","ws","p","p"))$generateGDX()
  
  upRiver<-GAMSObject$new("upRiver",data.frame(rep(0,ncol(hydro_time_series))),
                          list(c(1:4),
                               c(1),c(1),c(1)),
                          list("reg","ws","p","p"))$generateGDX()
  
  minFlow<-GAMSObject$new("minFlow",
                               data.frame(1000),
                               list(c(1:4),c(1),c(1)),
                               list("reg","ws","p"))$generateGDX()
  
  maxFlow<-GAMSObject$new("maxFlow",
                               data.frame(1000),
                               list(c(1:4),c(1),c(1)),
                               list("reg","ws","p"))$generateGDX()
  
  maxReservoir<-GAMSObject$new("maxReservoir",
                            data.frame(1000),
                            list(c(1:4),c(1),c(1)),
                            list("reg","ws","p"))$generateGDX()
  
  hydro_ts<-GAMSObject$new("hydro",
                          hydro_time_series,
                          list(c(1:4),c(1),c(1),c(1:nrow(hydro_time_series))),  
                          list("reg","ws","p","t"))$generateGDX()
  

  
  return(list(hydroConvFact,
              runOffDelay,
              minFlow,
              maxFlow,
              maxReservoir,
              hydro_ts,
              upRiver)
  
}

prepareIntermittentParameters<-function(intermittent,uels_in){
  uels_reg<-uels_in[[1]]
  uels_p<-uels_in[[2]]
  uels_t<-uels_in[[3]]
  
  
  intermittent_p<-GAMSObject$new("intermittent",
                           intermittent,
                           list(c(1:4),c(1),c(1:nrow(intermittent))),  
                           list("reg","p","t"))$generateGDX()
 
  return(list(intermittent_p))
  
  
}

prepareLoad<-function(load,uels_in){
  uels_reg<-uels_in[[1]]
  uels_p<-uels_in[[2]]
  uels_t<-uels_in[[3]]
  
  load_p<-GAMSObject$new("load",
                          load,
                          list(c(1:4),c(1:nrow(intermittent))),  
                          list("reg","t"))$generateGDX()
  return(list(load_p))
  
  
}

prepareSets<-function(uels_in){
  uels_reg<-uels_in[[1]]
  uels_p<-uels_in[[2]]
  uels_t<-uels_in[[3]]
  uels_ws<-uels_in[[4]]
  
  p_set<-createSET("p",uels_p)
  ws_set<-createSET("ws",uels_ws)
  reg_set<-createSET("reg",uels_reg)
  t_set <-createSET("t",uels_t)
  
  return(list(t_set,ws_set,p_set,reg_set))
}

prepareUels<-function(){
  uels_reg<-createUEL("reg",1:nmb_intermittent)
  uels_p<-createUEL("p",1:1)
  uels_t<-createUEL("t",1:100)
  uels_ws<-createUEL("ws",1:1)
  return(list(uels_reg,uels_p,uels_t,uels_ws))
}

prepareTimeSteps<-function(timesteps,uels_in){
  uels_t<-uels_in[[3]]
  val<-cbind(1:nrow(load),timeslots)
  length_p<-createPARAM("length",val,uels_t)
  return(list(length_p))

}

library(reshape)
library(RSQLite)
###This class creates, from basic data, a GAMS parameter that can be written by wgdx
###Name is the name of the GAMS parameter
###data: is a data.frame with all data for this parameter in a data.frame. 
###Each column contains the elements associated with the very last index.
###The order of columns has to follow the order in the uels/uelsNames variables
###uels: a list of vectors that indicate the element of each index that is associated with the parameter
###uelsNames: the name of the UELs associated with the parameter
###Example: create a GAMS Parameter with index P1:P100 and associated values
###i<-GAMSObject$new("Test_P",data.frame(1:100),list(c(1:100)),list("P"))
GAMSObject<-setRefClass("GAMSObject",
                        fields = list(name="vector",
                                      data = "data.frame",
                                      uels="list",
                                      uelsNames="list"),
                        
                        methods = list(                   
                          
                          ####constructor simple
                          initialize = function(in_name,
                                                in_data,
                                                in_uels,
                                                in_uelsNames){
                            
                            ##set data
                            name<<-in_name
                            data<<-in_data
                            uels<<-in_uels
                            uelsNames<<-in_uelsNames
                            
                          },
                          
                          ####what about a sql constructor?

                          ####generate GDX object from data
                          generateGDX=function(){
                            
                            uels1<-uels[length(uels):1]
                            prefix<-NULL
                            for(i in 1:length(uels1)){
                              l_cur<-length(uels1[[i]])
                              l_pref<-nrow(prefix)
                              if(is.null(l_pref)){l_pref=1}
                              prefix<-do.call("rbind", replicate(l_cur, prefix, simplify = FALSE))
                              
                              cur<-rep(uels1[[i]],each=l_pref)
                              prefix<-cbind(prefix,cur)
                              
                              
                            }
                            prefix<-prefix[,ncol(prefix):1]
                            
                            finalData<-cbind(prefix,melt(data)[,2])
                            
                            uels_rd<-mapply(createUEL,uelsNames,uels)
                            
                            return(createPARAM(name,
                                               finalData,
                                               uels_rd,
                                               length(uels)))
                            
                            
                          })
                        
)
                          
                          
                          