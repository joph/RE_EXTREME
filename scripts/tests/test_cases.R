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