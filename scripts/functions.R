source("libraries.R")
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
