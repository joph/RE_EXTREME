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

#load svensk kraftnÃ¤t time series
skn_ts_final    <- read_feather("data/svensk_kraftnät/ts_2007_2016.feather")
skn_ts_load     <- read_feather("data/svensk_kraftnät/ts_load_2007_2016.feather")

#load Olauson simulated wind time series
reanalysis_wind <- read_feather("data/reanalysis_wind_ts.feather")
reanalysis_wind_all <- read_feather("data/wind/ts_wind_all.feather")

#load SHYPE hydro corrected power time series
shype_hydro_cor <- read_feather("data/hydro/shype_hydro_cor_ts.feather")

#load SHYPE hydro natural power time series
shype_hydro_nat <- read_feather("data/hydro/shype_hydro_nat_ts.feather")




##### svensk kraftnÃ¤t data
# plot hydro - historic against SHYPE corrected simulated
skn_ts_final_se <- skn_ts_final  %>% 
  filter(year(date) == 2010) %>% 
  group_by(variable) %>% 
  summarise(mwh = sum(mwh)) %>% 
  ungroup() %>% 
  mutate(region = "SE") %>%
  select(date,variable, region, mwh) %>% 
  rbind(., skn_ts_final)
ggplot(skn_ts_final_se, aes(x=date, y=mwh)) + geom_line(aes(color=variable)) + facet_grid(region~.)

skn_ts_final_se %>% 
  group_by(year=year(date),month=month(date),region,variable) %>% 
  summarise(mwh = sum(mwh)) %>% 
  ungroup() %>% 
  mutate(date=ymd(paste(year,month,"1"))) %>% 
  select(date,region,variable,mwh) %>% 
  arrange(date) %>%
  ggplot(., aes(x=date, y=mwh)) + geom_line(aes(color=variable)) + facet_grid(region~.) 
plot(figure_test)

figure_test2 <- skn_ts_final  %>% 
  group_by(year=year(date),month=month(date),variable) %>% 
  summarise(mwh = mean(mwh)) %>% 
  ungroup() %>% 
  mutate(date=ymd(paste(year,month,"1"))) %>% 
  select(date,variable,mwh) %>% 
  arrange(date) %>%
  ggplot(., aes(x=date, y=mwh)) + geom_line(aes(color=variable)) 
plot(figure_test2)


figure_test3 <- skn_ts_final  %>% 
  group_by(date, variable) %>% 
  summarise(mwh = sum(mwh)) %>% 
  ungroup() %>% 
  group_by(year=year(date),month=month(date),variable) %>% 
  summarise(mwh = mean(mwh)) %>% 
  ungroup() %>% 
  mutate(date=ymd(paste(year,month,"1"))) %>% 
  select(date,variable,mwh) %>% 
  arrange(date) %>%
  ggplot(., aes(x=date, y=mwh)) + geom_line(aes(color=variable)) 
plot(figure_test3)


skn_ts_load_agg <- skn_ts_load  %>% 
  group_by(date,region) %>% 
  summarise(mwh = sum(mwh)) %>% 
  ungroup() %>% 
  mutate(variable = "load") %>% 
  select(date,variable, region, mwh) %>% 
  arrange(date,region)

skn_ts_prod <- skn_ts_final  %>%
  filter(variable != "load") %>% 
  select(date,variable, region, mwh) %>% 
  arrange(date, region)

my_ts <- rbind(skn_ts_load_agg, skn_ts_prod, reanalysis_wind) %>% 
  filter(between(year(date),2010,2010)) %>% 
  ggplot(., aes(x=date, y=mwh)) + geom_line(aes(color=variable)) + facet_grid(region~.)
plot(my_ts)

my_ts <- rbind(skn_ts_load_agg, skn_ts_prod, reanalysis_wind) %>% 
  filter(between(year(date),2010,2010)) %>% 
  group_by(date, variable) %>% 
  summarise(mwh = sum(mwh)) %>% 
  ungroup() %>% 
  ggplot(., aes(x=date, y=mwh)) + geom_line(aes(color=variable)) 
plot(my_ts)


my_ts <- rbind(skn_ts_load_agg, skn_ts_prod) %>% 
  group_by(date, variable) %>% 
  summarise(mwh = sum(mwh)) %>% 
  ungroup() %>%
  spread(variable, mwh) %>% 
  mutate(residual_load = load - hydro - wind) %>% 
  ggplot(., aes(x=date, y=residual_load)) + geom_line() 
plot(my_ts)



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


# plot hydro - historic against SHYPE corrected simulated
dataHistoricShypeCombined <-   rbind(skn_ts_final[which(skn_ts_final$variable == "hydro"),],shype_hydro_cor)  %>% 
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

cors<-sapply(c("SE1","SE2","SE3","SE4"),
       f,
       dataHistoricShypeCombined)



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
           y=6500,
           size=3,colour="black") 
plot(figure01)
ggsave("results/figures/Validation.pdf",figure01)

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
# plot wind - historic against Olausson
figure04 <- 
bind_rows(shype_hydro_nat,reanalysis_wind)  %>% 
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

# overall monthly mean production
figure04b <- 
  bind_rows(shype_hydro_nat,reanalysis_wind)  %>% 
  group_by(variable, region) %>% 
  mutate(norm_prod = mwh / mean(mwh)) %>% 
  ungroup() %>% 
  filter(between(year(date),2000,2015)) %>% 
  group_by(month=month(date),variable) %>% 
  summarise(norm_prod = mean(norm_prod)) %>% 
  ungroup() %>% 
  select(month,variable,norm_prod) %>% 
  arrange(month,variable) %>%
  ggplot(., aes(x=month, y=norm_prod)) + geom_line(aes(color=variable)) + 
  ggtitle("Seasonality of wind and hydro")
plot(figure04b)

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




