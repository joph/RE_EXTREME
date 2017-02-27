####################################################################################################### 
#####                                       Load libraries                                        #####
####################################################################################################### 
source("scripts/hydro/00_load_libraries.R")

####################################################################################################### 
#####                           Compare HYPE time series                                          #####
####################################################################################################### 
# Prepare SHYPE and EHYPE hydro power time series
 # hydro_mwh_ts <- read_feather("results/hydro/ts_ehype_mwh.feather")
 # hydro_mwh_ts$dataset <- "ehype"
 # 
 # shype_model <-  c("total", "corrected", "natural")
 # 
 # for (j in shype_model){
 #   temp <- read_feather(paste("results/hydro/ts_shype_mwh_",j,".feather", sep = ""))
 #   temp$dataset <- paste("shype",j)
 #   hydro_mwh_ts <- bind_rows(hydro_mwh_ts, temp)
 #   print(paste("import completed S-Hype:",j))
 #   }
 # remove(temp)
 # write_feather(hydro_mwh_ts, path = "results/hydro/hydro_mwh_ts_all.feather")

# Load SHYPE and EHYPE time series
hydro_mwh_ts <- read_feather("results/hydro/hydro_mwh_ts_all.feather") %>%  
  mutate(dataset = as.factor(dataset), hydrostation = as.factor(hydrostation)) 

# Load hydrostation datasets
load("results/hydro/hydrostations_sweden_gams.RData")
load("data/hydro/hydrostations_sweden_spatial.RData")
load("data/hydro/hydrostations_sweden_data.RData")


####################################################################################################### 
# compare total annual production of ehype, shype and svenska kraftnät

# total annual hydropower generation
hydro_mwh_ts_year <-  hydro_mwh_ts %>% 
  filter(between(year(Date), 2000, 2010)) %>% 
  group_by(dataset, year(Date)) %>% 
  summarise(TWh = sum(mwh)/1E6) %>% 
  rename(year = `year(Date)`)

# import hydropower energy statistics
SE_energy_statistic <- as_tibble( read.csv(file="data/hydro/SE_statistics_energy_supply.csv", sep=";") )
SE_energy_statistic %>%  
  filter(between(year, 2000, 2010)) %>% 
  select(year, Hydropower) %>% 
  transmute(dataset="Energy_Statistic",year=year, TWh = Hydropower) -> temp

# combine in one data set
hydro_mwh_ts_year <- bind_rows(hydro_mwh_ts_year,temp) 

### plots
ggplot(hydro_mwh_ts_year, aes(year,TWh,col=dataset)) + geom_line() +
  scale_x_continuous(breaks = c(2000:2010)) + theme_bw()

ggplot(filter(hydro_mwh_ts_year,year == 2010), aes(year,TWh,fill=dataset)) + geom_bar(stat="identity", position = "dodge") + 
  scale_x_continuous(breaks = 2010) + theme_bw()

#test <-  hydro_mwh_ts %>%  filter(hydrostation == "Stornorrfors" & between(year,2000,2003) )
#ggplot(test, aes(Date,mwh,col=dataset)) + geom_line() +
#  scale_x_Date(Date_breaks = "1 year", Date_minor_breaks = "3 month", Date_labels = "%Y") 

####################################################################################################### 
##### compare annual hydropower generation (SHYPE) per hydrostation with leif kuhlin data ####
# calculate mean annual production per hydropower plant between 2000 and 2014
hydrostation_shype_mwh <-  read_feather("results/hydro/hydro_mwh_ts_all.feather") %>%  
  mutate(dataset = as.factor(dataset), hydrostation = as.factor(hydrostation))  %>% 
  filter(dataset == "shype corrected", between(year(Date),2000,2014)) %>% 
  group_by(hydrostation, year(Date)) %>% 
  summarise(mwh = sum(mwh)) %>% 
  ungroup() %>% 
  group_by(hydrostation) %>% 
  summarise(mwh = mean(mwh)) %>% 
  ungroup() 

# add name
hydrostation_shype_mwh <- 
  as.data.frame(inner_join(hydrostation_shype_mwh,hydrostations_sweden_gams[c("id","name")], by =  c("hydrostation" = "id")))  

# annual mean production from leif kuhlin dataset
hydrostation_kuhlin_mwh <- hydrostations_sweden_data  %>% 
  filter(as.character(name) %in% as.character(hydrostation_shype_mwh$name)) %>%
  select(name, normal_prod) %>% 
  group_by(name) %>% 
  summarise(mwh = sum(normal_prod)) %>% 
  ungroup() 
 
# plot annual production of Leif Kuhlin against SHYPE simulated production
plot_kuhlin_shype <- left_join(hydrostation_shype_mwh, hydrostation_kuhlin_mwh, by= "name") %>% 
  rename(shype_corr = mwh.x, kuhlin = mwh.y)  %>% 
  group_by(1- shype_corr / kuhlin < 0.5) %>% 
  rename(good_fit = `1 - shype_corr/kuhlin < 0.5`) %>% 
  ungroup()
  ggplot(plot_kuhlin_shype, aes(shype_corr, kuhlin,col=good_fit, label=name)) + 
  geom_point() +
  geom_text(check_overlap = TRUE, vjust = 0.5, nudge_y = 0.5)
  
  # plot annual production of Leif Kuhlin against SHYPE simulated production
  plot_kuhlin_shype <- left_join(hydrostation_shype_mwh, hydrostation_kuhlin_mwh, by= "name") %>% 
    rename(SHYPE_simulated_production = mwh.x, average_annual_production = mwh.y) 
  ggplot(plot_kuhlin_shype, aes(SHYPE_simulated_production, average_annual_production, label=name)) + 
    geom_point() +    geom_text(check_overlap = TRUE, vjust = 0.5, nudge_y = 0.5)
  ggsave("results/figures/validation_individual_powerplants.pdf")
  
  
  
  ggplotly()


# save bad fit station 
hydro_bad_fit <- plot_kuhlin_shype %>% 
  filter(good_fit == FALSE) %>% 
  transmute(name = as.character(name)) %>% 
  as.vector()

hydrostation_bad_fit <- hydrostations_sweden_data  %>% 
  filter(name %in% hydro_bad_fit$name) 

hydrostation_bad_fit$id <-
as.data.frame(inner_join(hydro_bad_fit,hydrostations_sweden_gams[c("id","name")], by =  "name"))["id"] 

temp1 <- hydrostations_sweden_spatial[which(hydrostations_sweden_spatial@data$name %in% hydro_bad_fit$name),] 
plot(temp1)
Sweden_Nuts3               <- readOGR(dsn = "gis_data/SE_NUTS3", layer = "SE_NUTS3")
plot(Sweden_Nuts3, add=TRUE)

shype_noprod <- hydro_mwh_ts %>% 
  filter(hydrostation %in% hydro_bad_fit$name)

############################################################################################
# go to shype simulation data and check whats wrong there!
############################################################################################
  


  tidy() %>% 
  ggplot() + geom_polygon(aes(Long, Lat, group=group), colour='black', fill=NA) 


plot(Sweden_Nuts3, add=TRUE)

geom_text(data=Sweden_Nuts3_labels, aes(Longitude, Latitude, label = hydrostation), size=2)
# plot leif kuhlin normal prod vs. shype simulation
 left_join()
# plot
 
 
 ########################
 test %>% group_by(1- shype_corr / kuhlin < 0.5) %>% 
   rename(good_fit = `1 - shype_corr/kuhlin < 0.5`) %>% 
   ggplot(., aes(shype_corr, kuhlin,col=good_fit)) + geom_point()
 
 
 test <- read_feather(ts_shype_mwh_hourly, path = paste("results/hydro/ts_shype_mwh_hourly_",j,".feather", sep = "")) %>% 
   filter