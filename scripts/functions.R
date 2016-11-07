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
###cc: all time series data
###nmb_intermittent_ number of intermittent sources modelled
writeToGDX<-function(cc,nmb_intermittent){
  
  ####write data to gdx file
  uels_t<-createUEL("t",1:nrow(cc))
  t_set <-createSET("t",uels_t)

  uels_loc<-createUEL("l",1:nmb_intermittent)
  loc_set<-createSET("l",uels_loc)
  
  
  ###intermittent generation
  val<-NA
  for(i in 1:nmb_intermittent){
    val<-rbind(val,
               data.frame(i,1:nrow(cc),cc[,i]))
      
  }
  val<-val[2:nrow(val),]
  uels<-list(uels_loc[[1]],uels_t[[1]])
  intermittent_p<-createPARAM("intermittent",val,uels,2)
  
  ###hydropower
  val<-cbind(1:nrow(cc),cc[,nmb_intermittent+1])
  hydro_p<-createPARAM("hydro",val,uels_t)
  
  ###load
  val<-cbind(1:nrow(cc),cc[,nmb_intermittent+2])
  load_p<-createPARAM("load",val,uels_t)
  
  ###length of timeslotes
  val<-cbind(1:nrow(cc),cc[,nmb_intermittent+3])
  length_p<-createPARAM("length",val,uels_t)
  
  
  setwd(base_dir)
  wgdx.lst("../../gms_execute/input_tr.gdx",t_set,loc_set,intermittent_p,hydro_p,load_p,length_p)
  
}
