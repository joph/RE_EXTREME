firstRun<-function(w1,w2){
  l<-length(daily)
  uels_refs<<-paste(("r"),c(1:3),sep="")
  uels_days<<-list(paste("d",c(1:l),sep=""))
  prod_add<-cbind(c(1:l),daily)
  prod_lst<<-list(name="proddaily",type="parameter",dim=1,form="sparse",uels=uels_days,val=prod_add)
  load_add<-cbind(c(1:l),load_daily*1000)
  load_lst<<-list(name="loaddaily",type="parameter",dim=1,form="sparse",uels=uels_days,val=load_add)
  
  h<-length(hourly)
  uels_hourly<<-(paste("h",c(1:h),sep=""))
  
  prod_add_hourly1<-cbind(1,1:24,hourly_red[,1])
  prod_add_hourly2<-cbind(2,1:24,hourly_red[,2])
  prod_add_hourly3<-cbind(3,1:h,hourly)
  prod_add_hourly<-rbind(prod_add_hourly1,prod_add_hourly2,prod_add_hourly3)
  
  load_add_hourly1<-cbind(1,1:24,hourly_red[,3])
  load_add_hourly2<-cbind(2,1:24,hourly_red[,4])
  load_add_hourly3<-cbind(3,1:h,hourly_load)
  load_add_hourly<-rbind(load_add_hourly1,load_add_hourly2,load_add_hourly3)
  
  prod_lst_hourly<<-list(name="prodhourly",type="parameter",dim=2,form="sparse",uels=list(uels_refs,uels_hourly),val=prod_add_hourly)
  load_lst_hourly<<-list(name="loadhourly",type="parameter",dim=2,form="sparse",uels=list(uels_refs,uels_hourly),val=load_add_hourly)
  
  sum(hourly)/sum(hourly_red[,1])/2
  sum(hourly)/sum(hourly_red[,2])/2
  
  uels_h<<-paste(("h"),c(1:length(hourly)),sep="")
  h_set<-list(name="hourly",type="set",dim=1,uels=list(uels_h))
  
  uels_r<<-paste(("r"),c(1:3),sep="")
  r_set<-list(name="refs",type="set",dim=1,uels=list(uels_r))
  
  d_set<-list(name="daily",type="set",dim=1,uels=(uels_days))
  
  uels_cnt<-c("weight_hourly","other")
  val_<-as.matrix(data.frame(c(1,2),c(0.5,0.5)))
  cnt_lst<<-list(name="controlparams",type="parameter",dim=1,form="sparse",uels=list(uels_cnt),val=val_)
  val<-as.matrix(data.frame(c(1,2,3),c(w1,w2,1)))
  weights_lst<<-list(name="weights",type="parameter",dim=1,form="sparse",uels=list(uels_r),val=val)
  
  wgdx.lst("../GAMS_models/playmodel/input.gdx",prod_lst,load_lst,cnt_lst,h_set,r_set,prod_lst_hourly,load_lst_hourly,d_set,weights_lst)
  
}

secondRun<-function(){

  r_set<-list(name="refs",type="set",dim=1,uels=list(uels_r))
  
  
  
  h<-length(hourly)
  uels_hourly<<-(paste("h",c(1:h),sep=""))
  h_set<-list(name="hourly",type="set",dim=1,uels=list(uels_hourly))
  
  prod_add_hourly<-cbind(1,1:h,hourly)
  
  load_add_hourly<-cbind(1,1:h,hourly_load)
  
  prod_lst_hourly<<-list(name="prodhourly",type="parameter",dim=2,form="sparse",uels=list(uels_refs,uels_hourly),val=prod_add_hourly)
  load_lst_hourly<<-list(name="loadhourly",type="parameter",dim=2,form="sparse",uels=list(uels_refs,uels_hourly),val=load_add_hourly)
  
  uels_cnt<-c("weight_hourly","other")
  val_<-as.matrix(data.frame(c(1,2),c(1,0.5)))
  cnt_lst<<-list(name="controlparams",type="parameter",dim=1,form="sparse",uels=list(uels_cnt),val=val_)
  val<-as.matrix(data.frame(c(1,2),c(1,0)))
  weights_lst<<-list(name="weights",type="parameter",dim=1,form="sparse",uels=list(uels_r),val=val)
  
  
  wgdx.lst("../GAMS_models/playmodel/input.gdx",prod_lst,load_lst,cnt_lst,h_set,r_set,prod_lst_hourly,load_lst_hourly,d_set,weights_lst)
  
}

readResults<-function(){

thermal_hourly<<-rgdx("../GAMS_models/playmodel/results_mixed.gdx",
                     list(name="x_term_hourly",form="full",uels=list(uels_refs,uels_hourly)))$val

stor_hourly<<-rgdx("../GAMS_models/playmodel/results_mixed.gdx",
                     list(name="x_stor_out_hourly",form="full",uels=list(uels_refs,uels_hourly)))$val

stor_in_hourly<-rgdx("../GAMS_models/playmodel/results_mixed.gdx",
                     list(name="x_stor_in_hourly",form="full",uels=list(uels_refs,uels_hourly)))$val

renew_hourly<<-rgdx("../GAMS_models/playmodel/results_mixed.gdx",
                     list(name="x_renew_hourly",form="full",uels=list(uels_refs,uels_hourly)))$val

load_hourly<<-rgdx("../GAMS_models/playmodel/results_mixed.gdx",
                     list(name="loadhourly",form="full",uels=list(uels_refs,uels_hourly)))$val

curtail_hourly<<-rgdx("../GAMS_models/playmodel/results_mixed.gdx",
                     list(name="x_curtail_hourly",form="full",uels=list(uels_refs,uels_hourly)))$val

df<-data.frame(thermal_hourly[1,],stor_hourly[1,],stor_in_hourly[1,],renew_hourly[1,],load_hourly[1,],curtail_hourly[1,])

hourly_res<<-aggregate(df,by=hourly_ag,sum)
hourly_res<<-hourly_res[,2:ncol(hourly_res)]
names(hourly_res)<-c("thermal","stor","stor_in","renew","load","curtail")

thermal_daily<<-rgdx("../GAMS_models/playmodel/results_mixed.gdx",
                    list(name="x_term_daily",form="full",uels=(uels_days)))$val

stor_daily<<-rgdx("../GAMS_models/playmodel/results_mixed.gdx",
                 list(name="x_stor_out_daily",form="full",uels=(uels_days)))$val

stor_in_daily<<-rgdx("../GAMS_models/playmodel/results_mixed.gdx",
                    list(name="x_stor_in_daily",form="full",uels=(uels_days)))$val

renew_daily<<-rgdx("../GAMS_models/playmodel/results_mixed.gdx",
                  list(name="x_renew_daily",form="full",uels=(uels_days)))$val

load_daily<<-rgdx("../GAMS_models/playmodel/results_mixed.gdx",
                 list(name="loaddaily",form="full",uels=(uels_days)))$val

curtail_daily<<-rgdx("../GAMS_models/playmodel/results_mixed.gdx",
                    list(name="x_curtail_daily",form="full",uels=(uels_days)))$val

}




generateVariableResolutionTimeSeries<-function(all,hourly_load,df_hourly,lower,threshhold){
  p<-(all-hourly_load)
  le1<-rle(p<lower)
  ress<-rep(le1$lengths >= threshhold & le1$values,times = le1$lengths)
  plot(p[1:800],type="l")
  p1<-p
  p1[ress==0]<-NA
  lines(p1,col="red")
  
cc<-NA
for(i in seq(1,length(all),24)){
  #daily values
  df<-NA
  if(sum(ress[i:(i+23)]==0)){
    a<-apply(df_hourly[i:(i+23),],2,sum)
    df<-data.frame(t(c(a)),c(sum(hourly_load[i:(i+23)])),24)
  }else{
    df<-data.frame(df_hourly[i:(i+23),],hourly_load[i:(i+23)],1)
    
  }
  names(df)<-c(paste(1:4,"wind"),"hydro","load","multi")
  cc<-rbind(cc,df)
  
}
cc<-cc[2:nrow(cc),]
return(cc)
}

writeCCToGDX<-function(cc){
  ####write data to gdx file
  uels_t<-list(paste("t",1:nrow(cc),sep=""))
  t_set<-list(name="t",type="set",dim=1,uels=(uels_t))
  
  uels_loc<-list(paste("l",1:4,sep=""))
  loc_set<-list(name="loc",type="set",dim=1,uels=(uels_loc))
  
  val<-NA
  for(i in 1:4){
    val<-rbind(val,
               data.frame(i,1:nrow(cc),cc[,i]))
      
  }
  val<-val[2:nrow(val),]
  val<-as.matrix(val)
  int_lst<-list(name="intermittent",type="parameter",dim=2,form="sparse",uels=list(uels_loc[[1]],uels_t[[1]]),val=val)

  val<-cbind(1:nrow(cc),cc[,5])
  hydro_lst<-list(name="hydro",type="parameter",dim=1,form="sparse",uels=uels_t,val=val)
  
  
  val<-cbind(1:nrow(cc),cc[,6])
  load_lst<-list(name="load",type="parameter",dim=1,form="sparse",uels=uels_t,val=val)
  
  val<-cbind(1:nrow(cc),cc[,7])
  length_lst<-list(name="length",type="parameter",dim=1,form="sparse",uels=uels_t,val=val)
  
  
  wgdx.lst("../gms_source/input_tr.gdx",t_set,loc_set,int_lst,hydro_lst,load_lst,length_lst)
  
}
