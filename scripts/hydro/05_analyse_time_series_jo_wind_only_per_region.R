# todos
# check wind scenario and test alternative scenarios

####################################################################################################### 
#####                                       Load libraries                                        #####
####################################################################################################### 
source("scripts/hydro/00_load_libraries.R")

####################################################################################################### 
#####                      Prepare and load time series data                                      #####
####################################################################################################### 
 
##### Prepare time series    #####

# #load Olausson simulated wind time series
# mywd <- getwd()
# setwd("D:/Documents/Dropbox/github/RE_EXTREME")
# reanalysis_wind <- read_feather("data/wind/wind_1979_2014.feather") %>% 
#   transmute(date=Date,variable = "wind_merra", region=R, mwh = WindPower) %>% 
#   group_by(date, variable, region) %>% 
#   summarise(mwh = sum(mwh)) %>% 
#   ungroup()
# setwd(mywd)
# write_feather(reanalysis_wind, path = "data/reanalysis_wind_ts.feather")
# 
# #load SHYPE hydro corrected power time series
# shype_hydro_cor     <-  read_feather("results/hydro/ts_shype_mwh_hourly_corrected.feather") %>% 
#   inner_join(.,hydrostations_sweden_gams, by=c("hydrostation" = "id")) %>% 
#   select(date=Date, region=Region, hydrostation, mwh) %>% 
#   mutate(variable = "hydro_shype") %>% 
#   group_by(date, variable, region) %>% 
#   summarise(mwh = sum(mwh)) %>% 
#   ungroup()
# write_feather(shype_hydro_cor, path = "data/hydro/shype_hydro_cor_ts.feather")
# 
# 
# #load SHYPE hydro natural power time series
# shype_hydro_nat     <-  read_feather("results/hydro/ts_shype_mwh_hourly_natural.feather") %>% 
#   inner_join(.,hydrostations_sweden_gams, by=c("hydrostation" = "id")) %>% 
#   select(date=Date, region=Region, hydrostation, mwh) %>% 
#   mutate(variable = "hydro_shype") %>% 
#   group_by(date, variable, region) %>% 
#   summarise(mwh = sum(mwh)) %>% 
#   ungroup()
# write_feather(shype_hydro_nat, path = "data/hydro/shype_hydro_nat_ts.feather")


##### Load time series    #####

# Load hydrostation datasets
load("results/hydro/hydrostations_sweden_gams.RData")
load("data/hydro/hydrostations_sweden_spatial.RData")
load("data/hydro/hydrostations_sweden_data.RData")

#load svensk kraftnÃÂ¤t time series
skn_ts_final    <- read_feather("data/svensk_kraftnät/ts_2007_2016.feather")
skn_ts_load     <- read_feather("data/svensk_kraftnät/ts_load_2007_2016.feather")
load<-read_feather("data/load/load_2007_2015.feather")

#load solar data
solar<-read_feather("data/solar/solar_GAMS.feather")

#load Olauson simulated wind time series
reanalysis_wind <- read_feather("data/reanalysis_wind_ts.feather")
reanalysis_wind_all <- read_feather("data/wind/ts_wind_all.feather")



norm<-reanalysis_wind %>% group_by(region) %>% mutate(norm=mwh/max(mwh)) %>% select(date,region,norm) 
norm1<-norm %>% group_by(date) %>% summarize(norm=sum(norm)) %>% mutate(region="Total") %>% 
         select(date,region,norm) %>% mutate(norm=norm/max(norm))

br<-bind_rows(norm,norm1)

bind_rows(norm,norm1) %>% ggplot(aes(norm)) +stat_ecdf(aes(col=region)) +xlab("Normalized Production") + 
  ylab("Probability") +scale_colour_manual(values=wes_palette(n=5, name="Zissou")) + theme_bw()
ggplotly()
plotsummary((br %>% filter(region=="Total"))$norm)


ggsave("../../../habil/vortrag/figures/ecdf_wind.pdf")

lengthOfEvents(br,)

lengthOfEvents<-function(x1,t1,t2,scen){
  x<-unlist(filter(x1,region==scen) %>% dplyr::select(residual))
  date<-(filter(x1,scenario==scen) %>% dplyr::select(date))
  rr<-rle(x>t1&x<t2)
  seq<-unlist(mapply(function(x,y){rep(x,y)},
                     seq(1:length(rr$lengths)),
                     rr$lengths,SIMPLIFY=TRUE))
  value<-unlist(mapply(function(x,y){rep(x,y)},
                       rr$values,
                       rr$lengths,SIMPLIFY=TRUE))
  length<-unlist(mapply(function(x,y){rep(x,y)},
                        rr$lengths,
                        rr$lengths,SIMPLIFY=TRUE))
  td<-tibble(x,seq,value,length) %>% mutate(date=unlist(date$date)) %>% dplyr::select(x,date,seq,value,length)
  
  
  return(list(td,tibble(value=rr$lengths[rr$values])))
}





####->we have to basically provide full backup capacity
####->is this reduced over larger regions? Example Brazil


#load SHYPE hydro corrected power time series
shype_hydro_cor <- read_feather("data/hydro/shype_hydro_cor_ts.feather")

#load SHYPE hydro natural power time series
shype_hydro_nat <- read_feather("data/hydro/shype_hydro_nat_ts.feather")

#thermal_production<-read_excel("data/thermal/bio_wastechp.xlsx",sheet=2) %>% 
#  mutate(prod=`Tot prod`) 
#thermal_production<-tibble(rep(rep(thermal_production$prod,each=24),8))






####################################################################################################### 
#####                                        Analysis                                             #####
####################################################################################################### 



# mean absolute error (MAE) was 2.9 % and 
# the root mean square error (RMSE) was 3.8 % (related to the installed capacity). 
# Correlation was 0.98.

####################################################################################################### 
#####                                         Plots                                               #####
####################################################################################################### 

# define start year and end year
ts_start <- 2007
ts_end   <- 2014

skn_ts_final_agg<-skn_ts_final %>% group_by(date,variable) %>% filter(variable=="hydro") %>% summarize(mwh=sum(mwh)) %>% 
  mutate(variable="hydro",region="Tot") %>% select(date,variable,region,mwh) %>% bind_rows(skn_ts_final)
shype_hydro_cor_agg<-shype_hydro_cor %>% group_by(date,variable) %>% summarize(mwh=sum(mwh)) %>% 
  mutate(variable="hydro_shype",region="Tot") %>% select(date,variable,region,mwh) %>% bind_rows(shype_hydro_cor)

# plot hydro - historic against SHYPE corrected simulated
dataHistoricShypeCombined <-   rbind(skn_ts_final_agg[which(skn_ts_final_agg$variable == "hydro"),],shype_hydro_cor_agg)  %>% 
  filter(between(year(date),ts_start,ts_end)) %>%  
  group_by(year=year(date),month=month(date),day=day(date),region,variable) %>% 
  summarise(mwh = mean(mwh)) %>% 
  ungroup() %>% 
  mutate(date=ymd(paste(year,month,day))) %>% 
  select(date,region,variable,mwh) %>% 
  arrange(date)

f<-function(reg,dat){
  dd<-filter(dat,region==reg) 
  return(cor(filter(dd,variable=="hydro") %>% select(mwh),
             filter(dd,variable=="hydro_shype") %>% select(mwh)))
}

cors<-sapply(c("SE1","SE2","SE3","SE4","Tot"),
       f,
       dataHistoricShypeCombined)

vars<-dataHistoricShypeCombined %>% group_by(variable,region) %>% 
  summarize(var=var(mwh)) %>% spread(key=variable,value=var)%>% mutate(varProp=round(hydro/hydro_shype*100))

means<-dataHistoricShypeCombined %>% group_by(variable,region) %>% 
  summarize(mean=mean(mwh)) %>% spread(key=variable,value=mean)%>% mutate(meanProp=round(hydro/hydro_shype*100))

  


figure01<- dataHistoricShypeCombined %>%
  ggplot(., aes(x=date, y=mwh)) + geom_line(aes(color=variable)) + 
  facet_grid(region~.) +  
  xlab("Time (Date)") + 
  ylab("Daily synthetical Generation (MWh)")+
  scale_x_date(date_breaks = "1 year", date_minor_breaks = "3 months", date_labels = "%Y") + 
  ggtitle("Historic hydropower generation vs. SHYPE corrected") +
  annotate("text",
           label = paste("Cor:",round(cors,2)),
           x=as.Date("2015-01-01"),
           y=10000,
           size=3,colour="black") +
  annotate("text",
           label = paste("Var Proportion: ",vars$varProp,"%",sep=""),
           x=as.Date("2014-09-01"),
           y=8000,
           size=3,colour="black") +
  annotate("text",
         label = paste("Mean Proportion: ",means$meanProp,"%",sep=""),
         x=as.Date("2014-08-15"),
         y=6000,
         size=3,colour="black") 
  
plot(figure01)
ggsave("results/figures/Validation.pdf",figure01,width=30,height=20,units="cm")


# 

# plot hydro - historic against SHYPE natural simulated
figure02 <- 
rbind(skn_ts_final[which(skn_ts_final$variable == "hydro"),],shype_hydro_nat)  %>% 
  filter(between(year(date),ts_start,ts_end)) %>% 
  group_by(year=year(date),month=month(date),region,variable) %>% 
  summarise(mwh = mean(mwh)) %>% 
  ungroup() %>% 
  mutate(date=ymd(paste(year,month,"1"))) %>% 
  select(date,region,variable,mwh) %>% 
  arrange(date) %>%
  ggplot(., aes(x=date, y=mwh)) + 
  geom_line(aes(color=variable)) + facet_grid(region~.) + 
  scale_x_date(date_breaks = "1 year", date_minor_breaks = "3 months", date_labels = "%Y") +
  ggtitle("Historic hydropower generation vs. SHYPE natural")
plot(figure02)


# plot wind - historic wind against Olauson
figure03 <- 
rbind(skn_ts_final[which(skn_ts_final$variable == "wind"),],reanalysis_wind)  %>% 
  filter(between(year(date),2007,2016)) %>% 
  group_by(year=year(date),month=month(date),region,variable) %>% 
  summarise(mwh = mean(mwh)) %>% 
  ungroup() %>% 
  mutate(date=ymd(paste(year,month,"1"))) %>% 
  select(date,region,variable,mwh) %>% 
  arrange(date) %>%
  ggplot(., aes(x=date, y=mwh)) + geom_line(aes(color=variable)) + facet_grid(region~.) +
  scale_x_date(date_breaks = "1 year", date_minor_breaks = "3 months", date_labels = "%Y") +
  ggtitle("Historic windpower generation vs. Olauson Merra")
plot(figure03)

# assess seasonality wind against hydro
figure04 <- bind_rows(shype_hydro_nat,reanalysis_wind)  %>% 
  group_by(variable, region) %>% 
  mutate(norm_prod = mwh / mean(mwh)) %>% 
  ungroup() %>%  
  filter(between(year(date),ts_start,ts_end)) %>% 
  group_by(year=year(date),month=month(date),region,variable) %>% 
  summarise(norm_prod = mean(norm_prod)) %>% 
  ungroup() %>% 
  mutate(date=ymd(paste(year,month,"1"))) %>% 
  select(date,region,variable,norm_prod) %>% 
  arrange(date) %>%
  ggplot(., aes(x=date, y=norm_prod)) + geom_line(aes(color=variable)) + facet_grid(region~.) +
  scale_x_date(date_breaks = "1 year", date_minor_breaks = "3 months", date_labels = "%Y") +
  ggtitle("Simulated hydropower vs. simulated wind power")
plot(figure04)


# assess seasonality wind against hydro
# plot wind - historic against Olausson
figure04 <- reanalysis_wind %>% 
  mutate(mwh = mwh * 2) %>% 
  bind_rows(.,shype_hydro_nat)  %>% 
  select(date,region,variable,mwh) %>% 
  arrange(date) %>%
  ggplot(., aes(x=date, y=mwh)) + geom_line(aes(color=variable)) + facet_grid(region~.) +
  scale_x_date(date_breaks = "1 year", date_minor_breaks = "3 months", date_labels = "%Y") +
  ggtitle("Simulated hydropower vs. simulated wind power")
plot(figure04)

# monthly mean production for all regions
figure04a <- 
  bind_rows(shype_hydro_nat,reanalysis_wind)  %>% 
  group_by(variable, region) %>% 
  mutate(norm_prod = mwh / mean(mwh)) %>% 
  ungroup() %>%  
  filter(between(year(date),2000,2015)) %>% 
  group_by(month=month(date),region,variable) %>% 
  summarise(norm_prod = mean(norm_prod)) %>% 
  ungroup() %>% 
  select(month,region,variable,norm_prod) %>% 
  arrange(month,variable,region) %>%
  ggplot(., aes(x=month, y=norm_prod)) + geom_line(aes(color=variable)) + facet_grid(region~.) +
  scale_x_continuous( breaks=seq(0,12,1)) + scale_y_continuous( breaks=seq(0.25,2.5,0.25)) +
  ggtitle("Seasonality of wind and hydro")
plot(figure04a)

# overall monthly mean production & consumption
load_adapted<-load %>% mutate(variable="load") %>% 
         select(date=Date,variable,region=Region,mwh=Load)

solar_adapted<-solar %>% mutate(variable="solar") %>% 
  select(date=Date,variable,region=R,mwh=capFact)


shype_hydro_cor<-mutate(shype_hydro_cor,variable="hydro_shype_cor")


figure04b <- 
  bind_rows(shype_hydro_nat,shype_hydro_cor,reanalysis_wind,load_adapted,solar_adapted) %>% 
  filter(between(year(date),2007,2014)) %>%  group_by(variable,region) %>% 
  mutate(norm_prod = mwh / (mean(mwh)+0.000001)) %>% 
  ungroup() %>% 
  group_by(month=month(date),variable,region) %>% 
  summarise(norm_prod = mean(norm_prod,na.rm=TRUE)) %>% 
  ungroup() %>% 
  select(month,region,variable,norm_prod) %>% 
  arrange(month,variable) %>%
  ggplot(., aes(x=month, y=norm_prod)) + geom_line(aes(color=variable)) + 
  ggtitle("Seasonality of wind and hydro") + facet_grid(region~.)
plot(figure04b)
ggsave(filename="results/figures/seasonality.pdf",figure04b,width=30,height=20,units="cm")


######residuals - solar - wind mix######
######calculate necessary additional production from renewables first######
addProduction<- bind_rows(load_adapted,shype_hydro_nat,reanalysis_wind,solar_adapted) %>% 
  filter(between(year(date),2007,2014)) %>% 
  group_by(variable,region) %>% 
  summarize(s=sum(mwh)) %>% 
  spread(key=variable,value=s,fill=0) %>% 
  mutate(addProd=load-hydro_shype) %>% 
  mutate(addSolar=addProd/solar,addWind=addProd/wind_merra)

addSolar<-addProduction %>% select(addSolar) 
addWind<-addProduction %>% select(addWind)

addSolar$addSolar[addSolar$addSolar<0]<-0
addWind$addWind[addWind$addWind<0]<-0
load_adapted1<-bind_rows(
                        load_adapted %>% filter(region=="SE1") %>% mutate(addSolar=addSolar$addSolar[1],addWind=addWind$addWind[1]),
                        load_adapted %>% filter(region=="SE2") %>% mutate(addSolar=addSolar$addSolar[2],addWind=addWind$addWind[2]),
                        load_adapted %>% filter(region=="SE3") %>% mutate(addSolar=addSolar$addSolar[3],addWind=addWind$addWind[3]),
                        load_adapted %>% filter(region=="SE4") %>% mutate(addSolar=addSolar$addSolar[4],addWind=addWind$addWind[4])
                        )

######calculate residual generation######
residual_scenarios <-  bind_rows(load_adapted1,reanalysis_wind,solar_adapted)  
residual_scenarios1<-bind_rows(
  residual_scenarios %>% filter(region=="SE1") %>% mutate(addSolar=addSolar$addSolar[1],addWind=addWind$addWind[1]),
  residual_scenarios %>% filter(region=="SE2") %>% mutate(addSolar=addSolar$addSolar[2],addWind=addWind$addWind[2]),
  residual_scenarios %>% filter(region=="SE3") %>% mutate(addSolar=addSolar$addSolar[3],addWind=addWind$addWind[3]),
  residual_scenarios %>% filter(region=="SE4") %>% mutate(addSolar=addSolar$addSolar[4],addWind=addWind$addWind[4])) %>% filter(between(year(date),2007,2014)) %>% 
  group_by(date,variable,region,addSolar,addWind) %>% 
  summarize(sum(mwh)) %>% 
  mutate(mwh=`sum(mwh)`) %>% 
  spread(key=variable,value=mwh,fill=0) %>% 
  group_by(date,region) %>% 
  summarize(Mix=sum(load)-min(addSolar)*0.5*sum(solar)-min(addWind)*0.5*sum(wind_merra),
            Wind=sum(load)-min(addWind)*sum(wind_merra),
            Solar=sum(load)-min(addSolar)*sum(solar)) %>% 
  gather(scenario,residual,Mix,Wind,Solar,-date) 

residual_scenarios1 %>% 
  group_by(scenario,region) %>% 
  summarize(ssum=sum(residual))

#####monthly residual#####
figure05<- residual_scenarios1 %>% 
  group_by(month=month(date),scenario,region) %>% 
  summarize(monthly_residual=sum(residual)) %>% 
  ggplot(aes(x=month,y=monthly_residual)) + geom_line(aes(color=scenario)) + facet_grid(region~.) 
plot(figure05)
ggsave(figure05,filename="results/figures/monthly_residual.pdf",width=30,height=20,units="cm")


#####quarterly residual wind per year#####
figure05.1<- residual_scenarios1 %>% filter(scenario=="Wind") %>% 
  group_by(q=quarter(date),y=year(date),scenario,region) %>% 
  summarize(quarterly_residual=sum(residual)) %>% 
  mutate(ychar=as.character(y)) %>% 
  ggplot(aes(x=q,y=quarterly_residual)) + geom_line(aes(color=ychar)) + facet_grid(region~.) 
plot(figure05.1)
#####2010 rather extreme year!
ggsave(figure05.1,filename="results/figures/quarterly_residuals_wind.pdf",width=30,height=20,units="cm")

#####natural  residual wind + hydro#####
residual_scenarios_natural <-  bind_rows(load_adapted,shype_hydro_nat,reanalysis_wind,solar_adapted)  %>% 
  filter(between(year(date),2007,2014)) %>% 
  group_by(date,variable) %>% 
  summarize(sum(mwh)) %>% 
  mutate(mwh=`sum(mwh)`) %>% 
  spread(key=variable,value=mwh,fill=0) %>% 
  group_by(date) %>% 
  summarize(Mix=sum(load)-sum(hydro_shype)-addSolar*0.5*sum(solar)-addWind*0.5*sum(wind_merra),
            Wind=sum(load)-sum(hydro_shype)-addWind*sum(wind_merra),
            Solar=sum(load)-sum(hydro_shype)-addSolar*sum(solar)) %>% 
  gather(scenario,residual,-date) 

residual_scenarios_natural %>% 
  group_by(scenario) %>% 
  summarize(ssum=sum(residual))

figure05.2<- residual_scenarios_natural %>% filter(scenario=="Wind") %>% 
  group_by(q=quarter(date),y=year(date),scenario) %>% 
  summarize(quarterly_residual=sum(residual)) %>% 
  mutate(ychar=as.character(y)) %>% 
  ggplot(aes(x=q,y=quarterly_residual)) + geom_line(aes(color=ychar))
plot(figure05.2)
ggsave(figure05.2,filename="results/figures/quarterly_residuals_wind_hydro_natural.pdf",width=30,height=20,units="cm")


####deficit...
res_adv<- residual_scenarios_natural %>% filter(scenario=="Wind") %>% mutate(csum=cumsum(-1*residual)/10^6) %>% 
  mutate(min=min(csum),max=max(csum)) %>% mutate(csum=csum-min(min),max=max-min(min),min=0)

maxdiff<-max(res_adv$max)-min(res_adv$min)

figure05.3 <- res_adv%>% 
    ggplot(aes(x=date,y=csum)) + geom_line() + ylab("Cumulative Deficit/Surplus (TWh)") + 
  geom_line(aes(x=date,y=min),col="green") +
  geom_line(aes(x=date,y=max),col="green") +
  geom_line(aes(x=date,y=33),col="red",linetype=3) +
  geom_segment(aes(x = as.POSIXct("2007-01-01"), 
                   y = min(min), 
                   xend = as.POSIXct("2007-01-01"), 
                   yend = max(max)),col="blue",arrow=arrow(ends="both"))
  
plot(figure05.3)
ggsave(figure05.3,filename="results/figures/cumulative_deficit.pdf",width=30,height=20,units="cm")


#####weekly residual#####
figure06<- residual_scenarios %>% 
  group_by(week=week(date),scenario) %>% 
  summarize(weekly_residual=sum(residual)) %>% 
  ggplot(aes(x=week,y=weekly_residual)) + geom_line(aes(color=scenario))
plot(figure06)
ggsave(figure06,filename="results/figures/daily_residual.pdf")


#####hourly residual#####
figure07<- residual_scenarios %>% 
  group_by(hour=hour(date),scenario) %>% 
  summarize(hourly_residual=sum(residual)) %>% 
  ggplot(aes(x=hour,y=hourly_residual)) + geom_line(aes(color=scenario))
plot(figure07)
ggsave(figure07,filename="results/figures/hourly_residual.pdf")

#####calculate duration line######
figure08<-residual_scenarios %>% 
  group_by(scenario) %>% 
  arrange(desc(residual)) %>% 
  mutate(seq=1:n()) %>% 
  ungroup %>% 
  ggplot(aes(x=100*seq/max(seq),y=residual)) + geom_line(aes(col=scenario)) +xlab("Hour") + 
  geom_line(aes(x=100*seq/max(seq),y=16000),col="blue",linetype=3) +
  geom_line(aes(x=100*seq/max(seq),y=20000),col="red",linetype=3) +
  geom_line(aes(x=100*seq/max(seq),y=29000),col="black",linetype=3) +
  annotate("text", x=80, y = 16000, label = "Hydropower",col="blue") +
  annotate("text", x=80, y = 20000, label = "Thermal",col="red") +
  annotate("text", x=80, y = 29000, label = "Interconnections",col="black")
plot(figure08)

ggsave(figure08,filename="results/figures/duration.pdf",width=30,height=20,units="cm")



#####calculate duration line for wind scenario/years######
figure09<-residual_scenarios %>% filter(scenario=="Wind") %>% 
  group_by(y=year(date)) %>% 
  arrange(desc(residual)) %>% 
  mutate(seq=1:n()) %>% 
  ungroup() %>% 
  mutate(ychar=as.character(y)) %>% ggplot(aes(x=seq,y=residual)) + geom_line(aes(color=ychar)) +xlab("Hour")

plot(figure09)
ggsave(figure09,filename="results/figures/duration_wind.pdf",width=30,height=20,units="cm")



#####plot 2010 and 2012#####
figure10<-residual_scenarios %>% filter(scenario=="Wind") %>% 
  mutate(weekYear=as.numeric(strftime(date, format = "%W")))%>% 
  group_by(y=year(date),m=month(date),w=week(date)) %>% 
  summarize(resSum=sum(residual),weekYear=min(weekYear))%>% 
  filter(y==2010|y==2012) %>% 
  mutate(year=as.character(y))%>% 
  ggplot(aes(x=weekYear,y=resSum)) + geom_line(aes(color=year)) +xlab("Week")

plot(figure10)
ggsave(figure10,filename="results/figures/weekly_residuals_10_12.pdf",width=30,height=20,units="cm")

#####boxplots of residuals#####
figure11<-residual_scenarios %>% 
  filter(scenario=="Wind") %>% 
  mutate(y=as.character(year(date))) %>% 
  ggplot(aes(y,residual)) + geom_boxplot()
plot(figure11)

#####show distribution of lack in capacities######
figure12<-residual_scenarios_natural %>% filter(scenario=="Wind") %>% 
  filter(residual>0) %>% ggplot(aes(residual)) +geom_histogram(binwidth=100,col="orange")
plot(figure12)


#####calculate extremes######
lengthOfEvents<-function(x1,t1,t2,scen){
  x<-unlist(filter(x1,scenario==scen) %>% dplyr::select(residual))
  date<-(filter(x1,scenario==scen) %>% dplyr::select(date))
  rr<-rle(x>t1&x<t2)
  seq<-unlist(mapply(function(x,y){rep(x,y)},
         seq(1:length(rr$lengths)),
         rr$lengths,SIMPLIFY=TRUE))
  value<-unlist(mapply(function(x,y){rep(x,y)},
                   rr$values,
                   rr$lengths,SIMPLIFY=TRUE))
  length<-unlist(mapply(function(x,y){rep(x,y)},
                       rr$lengths,
                       rr$lengths,SIMPLIFY=TRUE))
  td<-tibble(x,seq,value,length) %>% mutate(date=unlist(date$date)) %>% dplyr::select(x,date,seq,value,length)
  
  
  return(list(td,tibble(value=rr$lengths[rr$values])))
}



limit1<-16000
limit2<-50000
loe<-lengthOfEvents(residual_scenarios,limit1,limit2,"Wind")

original<-loe[[1]]
extremes<-loe[[2]]

figure12.1<-extremes %>% 
  ggplot(aes(value)) +geom_histogram(binwidth=1,col="orange")
plot(figure12.1)


####show length of events, maximum production, mean production in events
ag_extremes<-original %>% filter(value) %>%  group_by(seq) %>% 
  summarize(mean=mean(x)/1000,max=max(x)/1000,l=mean(length),l1=n())%>% 
  group_by(l) %>% summarize(mean=mean(mean),max=max(max),TotalLength=sum(l1)/10) %>% 
  gather(variable,value,-l)

#min
#ag_extremes<-original %>% filter(value) %>%  group_by(seq) %>% 
#  summarize(mean=mean(x),min=min(x),l=mean(length),l1=mean(length))%>% 
#  group_by(l) %>% summarize(mean=mean(mean)+limit1,min=min(min)+limit1,TotalLength=sum(l1)) %>% 
#  gather(variable,value,-l)

figure13<-ag_extremes %>%  ggplot(aes(x=l,y=value)) + 
  geom_point(aes(col=variable)) +ylab("Length of Events (10h) and Lack in Capacity (GW)") + xlab("Length of Event")
plot(figure13)
ggsave(figure13,filename="results/figures/extreme_events.pdf",width=30,height=20,units="cm")


####show position of extreme events
filteredEvents<-original %>% filter(value) %>% mutate(diff=as.character(seq))

figure14<-ggplot(original,aes(x=date,y=x)) + geom_line() +
  geom_line(data=filteredEvents,aes(x=date,y=x,col=diff)) +
  theme(legend.position="none")
plot(figure14)

filteredEvents<-original %>% filter(value&length>10) %>% mutate(diff=as.character(seq))

figure15<-ggplot(original,aes(x=date,y=x)) + geom_line() +
  geom_line(data=filteredEvents,aes(x=date,y=x,col=diff)) +
  theme(legend.position="none")
plot(figure15)


filteredEvents<-original %>% filter(value&length>80) %>% mutate(diff=as.character(seq))
figure15<-ggplot(original,aes(x=date,y=x)) + geom_line() +
  geom_line(data=filteredEvents,aes(x=date,y=x,col=diff)) +
  theme(legend.position="none")
plot(figure15)


#######show one single extreme event
s1<-reanalysis_wind %>% group_by(date) %>% summarize(m=sum(mwh))
s1<-mean(s1$m)
s2<-shype_hydro_cor %>% group_by(date) %>% summarize(m=sum(mwh))
s2<-mean(s2$m)
s3<-load_adapted %>% group_by(date) %>% summarize(m=sum(mwh))
s3<-mean(s3$m)

avg<-tibble(date=rep(seq(as.POSIXct("2007-01-01 00:00"),as.POSIXct("2007-12-31 23:00"),"h"),3),
       variable=c(rep("Avg Wind",8760),rep("Avg Hydro",8760),rep("Avg Load",8760)),
       region=rep("SE1",3*8760),
       mwh=c(rep(s1,8760),rep(s2,8760),rep(s3,8760)))


figx<-bind_rows(load_adapted,shype_hydro_cor,reanalysis_wind,avg)  %>% 
  filter(between(year(date),2007,2008)) %>% 
  group_by(date,variable) %>% 
  summarize(sum(mwh)) %>% 
  mutate(mwh=`sum(mwh)`) %>%
  filter(date %in% filteredEvents$date) %>% 
  ggplot(aes(x=date,y=mwh)) + geom_line(aes(col=variable))
plot(figx)
ggsave(figurex,filename="results/figures/introduction.pdf",width=30,height=20,units="cm")


#####make extreme event graph from this!
figure16<-ggplot(original,aes(x=date,y=x)) + geom_line() +
  geom_line(data=filteredEvents,aes(x=date,y=x,col=diff)) +
  theme(legend.position="none")
plot(figure16)

####year####
group<-original$length
cc<-seq(60,10,-10)
ii<-1
group[original$length>117]<-0
for(i in seq(60,10,-10)){
  group[original$length<i]<-cc[ii]
  ii<-ii+1
}


figure17<-  original %>%  mutate(group=group) %>% filter(value) %>% 
  group_by(d=as.numeric(strftime(date, format = "%W")),l=seq) %>% 
  summarize(n=min(group)) %>%   mutate(l1=as.character(n)) %>% 
  dplyr::select(d,l1) %>% 
  ggplot(aes(d)) + geom_bar(aes(fill=l1)) + xlab("Week")
plot(figure17)
ggsave(figure17,filename="results/figures/extreme_events_II.pdf",width=30,height=20,units="cm")

#  geom_smooth(aes(col=variable)) 

ggplot()+
  geom_histogram(data=extremes,aes(value),binwidth=1)

  xlab("Length of Event")



extremes %>% ggplot(aes(value)) + geom_histogram(col="orange",binwidth = 1) + xlab("Length of Event")

extremes %>%  filter(key=="extremes") %>% dplyr::select(value) %>% sum()/length(original)*100 %>% round()


distr<-fitdistr(extremes$extremes, densfun="weibull", lower = 0)
rw<-rweibull(nrow(extremes),
             shape=distr$estimate[1],
             scale=distr$estimate[2])

extremes <- extremes %>% mutate(est=rw) %>% 
  gather()


ggsave(p,filename="results/figures/extremes.pdf")




######residuals - wind mix######
figure06 <-  bind_rows(load_adapted,shype_hydro_cor,reanalysis_wind,solar_adapted)  %>% 
  filter(between(year(date),2007,2014)) %>% 
  group_by(date,variable) %>% 
  summarize(sum(mwh)) %>% 
  mutate(mwh=`sum(mwh)`) %>% 
  spread(key=variable,value=mwh,fill=0) %>% 
  group_by(date) %>% 
  summarize(residual=sum(load)-sum(hydro_shype_cor)-2.5*sum(wind_merra)) %>% 
  group_by(month=month(date)) %>% 
  summarize(monthly_residual=sum(residual)) %>% 
  ggplot(aes(x=month,y=monthly_residual)) + geom_line()
plot(figure06)

plot(s$monthly_residual,type="l")
mean(s$residual)


#####show hourly & monthly component of residuals
  
  
mutate(norm_prod = sum(mwh)) %>% 
  
  ungroup() %>% 
  group_by(month=month(date),variable) %>% 
  summarise(norm_prod = mean(norm_prod)) %>% 
  ungroup() %>% 
  select(month,variable,norm_prod) %>% 
  arrange(month,variable) %>%
  ggplot(., aes(x=month, y=norm_prod)) + geom_line(aes(color=variable)) + 
  ggtitle("Seasonality of wind and hydro")
plot(figure04b)
ggsave(filename="results/figures/seasonality.pdf",figure04b)












# compare monthly mean production of wind scenarios for different regions
figure05 <- read_feather("data/wind/ts_wind_all.feather") %>% 
  #filter(scenario %in% c("A1","B1","B2","B3","B4")) %>% 
  group_by(scenario, region) %>% 
  mutate(norm_prod = mwh / mean(mwh)) %>% 
  ungroup() %>%  
  group_by(month=month(date),region,scenario) %>% 
  summarise(norm_prod = mean(norm_prod)) %>% 
  ungroup() %>% 
  select(month,region,scenario,norm_prod) %>% 
  arrange(month,scenario,region) %>%
  ggplot(., aes(x=month, y=norm_prod)) + geom_line(aes(color=scenario)) + facet_grid(region~.) +
  scale_x_continuous( breaks=seq(0,12,1)) + scale_y_continuous( breaks=seq(0.5,1.5,0.1)) +
  ggtitle("Comparison of wind scenarios")
  ggplotly()
plot(figure05)




