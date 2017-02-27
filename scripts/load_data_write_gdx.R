base_dir<-"D:/google drive/Anträge/formas/RE_EXTREME github/RE_EXTREME"
setwd(base_dir)
source("scripts/functions_gdx_transfer.R")
source("scripts/function_raw_data_input.R")

#readAndWriteGridCapacity()

####prepare run - 2 years, high capacities
period<-c("2007-01-01 00:00:00 CET"
          , "2008-12-31 23:00:00 CET")
prepareFullRun(period,intermittentCSV="../data/investOptions/intermittentOpts2years.csv",
               transmissionCSV="../data/transmission/lineCapacities_original.csv",
               investCSV="../data/investOptions/investOpts.csv")

#results<-runGAMS()

results<-readModelResults("input_tr.gdx","results_time_resolution.gdx",
                          period,"lehr_vortrag_keine_imports_x")

agResults<-readResultsGeneric("results_time_resolution.gdx",list("results")) %>% 
  sapply(readSingleSymbolGDX,simplify=FALSE)

totLoad<- agResults[[1]] %>% filter(resCat=="load" & runs=="R1") %>% group_by(resCat) %>% 
  summarize(valueS=sum(value)) %>% select(valueS) %>% unlist()
totLoad<-totLoad[1]

c<-c("1 All Restrictions","2 Min-Flow relaxed", "3 Transmission relaxed","4 Hydro cap relaxed","5 Combined")
c1<-c("Wind","hydro_total","Thermal","curtail_wind","spill")
c2<-c("1 Windpower","2 Hydropower","3 Thermal","4 Wind curtailing","5 Hydro spilling")
for(i in 1:5){
  agResults[[1]]$runs<-gsub(paste("R",i,sep=""),c[i],agResults[[1]]$runs)
  agResults[[1]]$resCat<-gsub(c1[i],c2[i],agResults[[1]]$resCat)
}

agResults[[1]] %>% filter(resCat %in% c2) %>% 
  mutate(value=100*value/totLoad) %>% 
  group_by(name,runs,resCat) %>% summarize(valueS=sum(value)) %>% 
  ggplot() + geom_col(aes(x=resCat,y=valueS,fill=runs),position=position_dodge()) + scale_fill_manual("",values=wes_palette(n=5, name="Zissou"))+
  theme_classic() + xlab("") + ylab("Share of Load (%)")
ggsave("D:/google drive/habil/vortrag/figures/sweden_flexibility.pdf")


r_region<- results %>% filter(!is.na(reg)) %>% group_by(name,reg,datetime) %>% summarize(value=sum(value))

r_region %>% filter(reg=="SE2") %>% filter(name=="x_curtail") %>% ggplot(aes(x=datetime,y=value)) +geom_line()

sum((r_region %>% filter(name=="x_spill") %>% select(value))$value )
r_region %>% filter(name=="x_spill") %>% group_by(name,reg) %>% summarize(s=sum(value))

#adapt negative values
r_region[r_region$name=="x_h_stor_in"|
           r_region$name=="x_curtail"|
           r_region$name=="x_spill"|
           r_region$name=="transfer_out",]$value<-r_region[r_region$name=="x_h_stor_in"|
                                                            r_region$name=="x_curtail"|
                                                            r_region$name=="x_spill"|
                                                            r_region$name=="transfer_out",]$value*-1


r_load <-r_region %>% filter(name=="load")%>% mutate(dat_=as.POSIXct(datetime)) %>% ungroup()  
r_trans <-r_region %>% filter(name=="transfer_net")%>% mutate(dat_=as.POSIXct(datetime)) %>% ungroup()  
fj<-full_join(r_load,r_trans,by=c("reg","datetime","dat_")) %>% select(reg,dat_,datetime,val1=value.x,val2=value.y)  
r_loaddiff<-fj %>% mutate(a=val1-val2) %>% mutate(name="diff")  



###############weekly - load balance###############  

r_region_weekly <- r_region %>% group_by(name,reg,w=week(datetime),y=year(datetime)) %>% 
  summarize(value=mean(value),dat_=as.POSIXct(min(datetime)))
r_load_weekly <- r_region_weekly %>% filter(name=="load") 
r_loaddiff_weekly<- r_loaddiff %>% group_by(name,reg,w=week(datetime),y=year(datetime)) %>% 
  summarize(diff=mean(a),dat_=min(datetime))


dropNames<-c("x_h_stor_lv",
             "load",
             "x_h_stor_in",
             "x_transfer",
             "bal_",
             "x_hyd_up",
             "hydro")

fig01<-r_region_weekly %>% filter(!(name %in% dropNames)) %>% 
  ggplot(aes(x=dat_,y=value,fill=name)) + geom_area() + facet_wrap(~reg) +
  geom_line(data=r_load_weekly,aes(x=dat_,y=value)) +
  geom_line(data=r_loaddiff_weekly,aes(x=dat_,y=diff),col="red") +ylab("Avg. Weekly Cap (MW)")
plot(fig01)
ggsave("results/figures/opt_load_bal.pdf",fig01,width=30,height=20,units="cm")

ggplotly()

###############weekly - hydro balance###############  
includeNames<-c("x_h_stor_in",
                "x_h_stor_out",
                "x_hydro",
                "x_spill")

#adapt negative values
r_region$value[r_region$name %in% 
           c("x_h_stor_in","x_h_stor_out","x_spill")]<-r_region$value[r_region$name %in% 
                                                                        c("x_h_stor_in","x_h_stor_out","x_spill")]*-1


r_region_weekly <- r_region %>% group_by(name,reg,w=week(datetime),y=year(datetime)) %>% 
  summarize(value=mean(value),dat_=as.POSIXct(min(datetime)))

#"x_h_stor_lv",
#"hydro"

r_hydro_weekly <- r_region_weekly %>% filter(name=="hydro") 

fig02<-r_region_weekly %>% filter((name %in% includeNames)) %>% 
  ggplot(aes(x=dat_,y=value,fill=name)) + geom_area() + facet_wrap(~reg) +
  geom_line(data=r_hydro_weekly,aes(x=dat_,y=value))
plot(fig02)
ggsave("results/figures/opt_hydro_bal.pdf",fig02,width=30,height=20,units="cm")

ggplotly()


r_storage_weekly <- r_region_weekly %>% filter(name=="x_h_stor_lv")
r_storage_weekly %>% ggplot(aes(x=dat_,y=value)) + geom_line() + facet_wrap(~reg)
ggplotly()
###############transfer###############

transfer<-readModelResultsTransfer("results_time_resolution.gdx",period)
transfer_weekly<- transfer %>% group_by(week(datetime),reg,reg1) %>% summarize(value=max(value),
                                                                      datetime=min(datetime)
                                                                     )

fig03<-transfer_weekly %>% ggplot(aes(x=datetime,y=value)) +geom_line(aes(col=reg1))  + facet_wrap(~reg)
plot(fig03)
ggsave("results/figures/opt_transfer.pdf",fig03,width=30,height=20,units="cm")
ggplotly()
###############hourly###############







r_region %>% filter(name! %in% dropNames)%>% 
  mutate(dat_=as.POSIXct(datetime)) %>% 
  ggplot(aes(x=dat_,y=value,fill=name)) + geom_area() + facet_wrap(~reg) +
  geom_line(data=r_load,aes(x=dat_,y=value)) +
  geom_line(data=r_loaddiff,aes(x=dat_,y=a),col="red") 
  

r_region %>% filter(name=="load") %>% tail()
#load   SE2 2007-01-02 23:00:00 2007.46
#x_term   SE2 2007-01-02 23:00:00   557
#x_h_stor_out   SE2 2007-01-02 23:00:00 1808.354

r_region %>% filter(name=="load"&reg=="SE2") %>% tail()

#(SUM(p$investOptions(reg,p,"Thermal","VarCost"),x_term.l(reg,t,p))                        +
SUM((ws,hp)$existsHydro(reg,ws,hp),0.99*x_h_stor_out.l(reg,t,ws,hp))                     +
SUM((p)$investOptions(reg,p,"Storage","Investment"),0.9*x_stor_out.l(reg,t,p))           +
SUM((p,iTechnology)$existsIntermittent(reg,p,iTechnology),x_renew.l(reg,t,p,iTechnology))+
SUM((ws,hp)$existsHydro(reg,ws,hp),x_hydro.l(reg,t,ws,hp))                               -
SUM(p$investOptions(reg,p,"Storage","Investment"),x_stor_in.l(reg,t,p))                  +
SUM(reg1$transmissionCap(reg,reg1),x_transfer.l(reg1,reg,t))                             -
SUM(reg1$transmissionCap(reg,reg1),x_transfer.l(reg,reg1,t)));


ggplotly()  

r_region_weekly %>% filter(reg=="SE2") %>% filter(dat_<as.POSIXct("2007-01-02"))

####run and read results

l<-group_by(load,Date) %>% summarize(s=sum(Load))
hourly_load<-data.frame(l$s)

w<-group_by(rf,Date) %>% summarize(s=sum(WindPower))
hourly_load<-
generateVariableResolutionTimeSeries(hourly_load,
                                     intermittent_hourly,
                                     hydropower_hourly,
                                     renewable_scaling,
                                     lower,
                                     upper,
                                     threshhold,
                                     plot=TRUE, 
                                     limits=c(1:800))


rr<-full_join(load,rf,by=c("Date"="Date","Region"="R")) %>% mutate(res=Load-WindPower) %>%
  group_by(Date) %>% summarize(s=sum(res)) %>% mutate(is_extreme=s<0.1*mean(s)|s>1.9*mean(s))

ggplot(rr,aes(x=Date,y=s)) + geom_line() 
ggplot(filter(rr,is_extreme==TRUE),aes(x=Date,y=s),add=TRUE) + geom_line(col="red")


###############hourly - load balance###############

zoom_period<-c(as.POSIXct("2008-01-15"),as.POSIXct("2008-01-16"))
r1<-r_load %>% filter(datetime>=zoom_period[1]&datetime<=zoom_period[2])
r2<-r_loaddiff %>% filter(datetime>=zoom_period[1]&datetime<=zoom_period[2])

fig01<-r_region %>% filter(!(name %in% dropNames)) %>% 
  filter(datetime>=zoom_period[1]&datetime<=zoom_period[2]) %>% 
  ggplot(aes(x=datetime,y=value,fill=name)) + geom_area() + facet_wrap(~reg) +
  geom_line(data=r1,aes(x=datetime,y=value)) +
  geom_line(data=r2,aes(x=datetime,y=a),col="red") +ylab("Avg. Hourly Cap (MW)")
plot(fig01)
ggplotly()

###############hourly - hydro balance###############  



includeNames<-c("x_h_stor_in",
                "x_h_stor_out",
                "x_hydro",
                "x_spill")

#adapt negative values
r_region$value[r_region$name %in% 
                 c("x_h_stor_in","x_h_stor_out","x_spill")]<-r_region$value[r_region$name %in% 
                                                                              c("x_h_stor_in","x_h_stor_out","x_spill")]*-1


#"x_h_stor_lv",
#"hydro"

r_hydro <- r_region %>% filter(name=="hydro") 

fig02<-r_region %>% filter((name %in% includeNames)) %>% 
  ggplot(aes(x=datetime,y=value,fill=name)) + geom_area() + facet_wrap(~reg) +
  geom_line(data=r_hydro,aes(x=datetime,y=value))
plot(fig02)

r_region %>% filter(name=="x_spill") %>% ggplot(aes(x=datetime,y=value)) +geom_line(aes(col=reg))


