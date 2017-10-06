base_dir<-"C:/Users/cancella/Google Drive/!IIASA/COPA/RE_EXTREME"
#base_dir<-"C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA/RE_EXTREME"
gams_dir<-"C:/GAMS/win64/24.8"
#gams_dir<-"C:/GAMS/win64/24.7"

setwd(base_dir)
source("scripts/functions_gdx_transfer.R")
source("scripts/function_raw_data_input.R")

####prepare run - 2 years
#period<-c("2007-01-02 00:00:00 CET"
 #          ,"2008-12-31 23:00:00 CET")

period<-c("2012-01-01 01:00:00 CET"
         ,"2012-12-31 23:00:00 CET")

############Create model run
prepareFullRun(period,
               out="../gms_execute/input_tr.gdx",
               hydFile="../data/hydro/hydro_data_br_2012_1.csv",                           # validation 2012
               hydFeather="../data/hydro/br_shype_hydro.feather",                          # validation 2012 - same file
               windFeather="../data/wind/wind_br.feather",                                 # validation 2012 - same file
               solarFeather="../data/solar/solar_GAMS_br.feather",                         # validation 2012 - same file 
               loadFeather="../data/load/load_Br_2014.feather",                            # validation 2012 - same file
               transmissionCSV="../data/transmission/linesCapacities_br_2012_1.csv",       # validation_2012  # linesCapacities_br_2012_1 - transmission_lines_50.csv # 50 % of 2012 capacity for each region
               investCSV="../data/investOptions/investOpts_br_thermal.sources_1_2012.csv", # Validation_2012
               intermittentCSV="../data/investOptions/br_intermittent_opts_2012_1.csv")    # validation 2012

############Run GAMS Manually

############Read results from GDX File
results<-readModelResults("input_tr.gdx",
                          "results_time_resolution.gdx",
                          period,
                          "Validation_2012")

#results %>% group_by(name) %>% summarize(s=sum(value))

#results %>% filter(name=="x_transfer") %>% group_by(datetime) %>% summarize(s=sum(value)) %>% 
 #ggplot(aes(x=datetime,y=s)) + geom_line()

# changing the regions names
namesOrig<-c("SE001","SE002","SE003","SE004","SE005")
namesNew<-c("SE/CO","SUL","NE","N","NF")
for(i in 1:length(namesOrig)){
  results$reg[results$reg == namesOrig[i]] <- namesNew[i]
  results$reg1[results$reg1 == namesOrig[i]] <- namesNew[i]
}


totalCost <- readResultsGeneric("results_time_resolution.gdx", 
                   c("totalCost")) %>% sapply(readSingleSymbolGDX,simplify=FALSE)
totalCost


#############Show results as Figures
r_region<- results %>% filter(!is.na(reg)) %>% 
  group_by(name,reg,datetime) %>% 
  summarize(value=sum(value))

#adapt negative values
r_region[r_region$name=="x_h_stor_in"|
           r_region$name=="x_curtail"|
           r_region$name=="x_spill",]$value<-r_region[r_region$name=="x_h_stor_in"|
                                                            r_region$name=="x_curtail"|
                                                            r_region$name=="x_spill",]$value


r_load <-r_region %>% filter(name=="load")%>% mutate(dat_=as.POSIXct(datetime)) %>% ungroup()  
r_trans <-r_region %>% filter(name=="x_transfer")%>% mutate(dat_=as.POSIXct(datetime)) %>% ungroup()  
fj<-full_join(r_load,r_trans,by=c("reg","datetime","dat_")) %>% select(reg,dat_,datetime,val1=value.x,val2=value.y)  
r_loaddiff<-fj %>% mutate(a=val1-val2) %>% mutate(name="diff")  

###############weekly - load balance###############  


r_region_weekly <- r_region %>% group_by(name,reg,w=week(datetime),y=year(datetime)) %>% 
  summarize(value=mean(value),dat_=as.POSIXct(min(datetime)))
#r_region_weekly$value[r_region_weekly$name=="x_transfer"]<-
#  r_region_weekly$value[r_region_weekly$name=="x_transfer"]*-1

r_region_weekly$value[r_region_weekly$name=="x_spill"]<-
  r_region_weekly$value[r_region_weekly$name=="x_spill"]*-1 # energy going out will be negative in this graph. 

r_load_weekly <- r_region_weekly %>% filter(name=="load") 
r_loaddiff_weekly<- r_loaddiff %>% group_by(name,reg,w=week(datetime),y=year(datetime)) %>% 
  summarize(diff=mean(a),dat_=min(datetime))


dropNames<-c("x_h_stor_lv",
             "load",
             "x_h_stor_in",
#             "x_transfer",
             "transfer_net",
             "bal_",
             "x_hyd_up",
             "hydro",
              "x_invest_intermittent",
              "x_invest_thermal_cap")


fig01<-r_region_weekly %>% filter(!(name %in% dropNames)) %>% 
  ggplot(aes(x=dat_,y=value,fill=name)) + geom_area() + facet_wrap(~reg) +  
  geom_line(data=r_load_weekly,aes(x=dat_,y=value)) +
  geom_line(data=r_loaddiff_weekly,aes(x=dat_,y=diff),col="red") +ylab("Avg. Weekly Cap (MW)")
plot(fig01)
ggsave("../results/figures/opt_load_bal.pdf",fig01,width=30,height=20,units="cm")

###############weekly - hydro balance###############  
includeNames<-c("x_h_stor_in",
                "x_h_stor_out",
                "x_hydro",
                "x_spill") 
                #"x_slack")

#adapt negative values
r_region$value[r_region$name %in% 
           c("x_h_stor_out","x_spill")]<-r_region$value[r_region$name %in% 
                                                                        c("x_h_stor_out","x_spill")]*-1
r_region$value[r_region$name=="x_spill"]<-
  r_region$value[r_region$name=="x_spill"]*-1
  
#"x_h_stor_in",
#"x_h_stor_in",

r_region_weekly <- r_region %>% group_by(name,reg,w=week(datetime),y=year(datetime)) %>% 
  summarize(value=mean(value),dat_=as.POSIXct(min(datetime)))

#"x_h_stor_lv",
#"hydro"

r_hydro_weekly <- r_region_weekly %>% filter(name=="hydro") 

fig02<-r_region_weekly %>% filter((name %in% includeNames)) %>% 
  ggplot(aes(x=dat_,y=value,fill=name)) + geom_area() + facet_wrap(~reg) +
  geom_line(data=r_hydro_weekly,aes(x=dat_,y=value))
plot(fig02)
ggsave("../results/figures/opt_hydro_bal.pdf",fig02,width=30,height=20,units="cm")


###############Storage Level###############
r_storage_weekly <- r_region_weekly %>% filter(name=="x_h_stor_lv")
fig03 <- r_storage_weekly %>% ggplot(aes(x=dat_,y=value)) + geom_line() + facet_wrap(~reg)
plot(fig03)
ggsave("../results/figures/storage_level.pdf",fig03,width=30,height=20,units="cm")


##### defining the total hydro generation #####
# It will be useful on the scripts of comparation between ONS and COPA. 
x_hydro_tot <- results %>% filter(name=="x_hydro" | name=="x_h_stor_out") %>% 
spread(name,value) %>% mutate(value=x_h_stor_out+x_hydro,name="x_hydro_tot") %>% 
select(name,reg,t,p,iTechnology,value,ws,hp,reg1,TT,datetime)
results<-bind_rows(results,x_hydro_tot)


###############transfer###############
#transfer<-readModelResultsTransfer("results_time_resolution.gdx",period)$x_transfer %>% 
 # mutate(t=ymdhm(t))

#transfer_weekly<- x_transfer %>% group_by(week(datetime),reg,reg1) %>% summarize(value=max(value),
#                                                                    datetime=min(datetime)
 #                                                                   )

#fig03<-transfer_weekly %>% ggplot(aes(x=datetime,y=value)) +geom_line(aes(col=reg1), size =1)  + facet_wrap(~reg)
#plot(fig03)
#ggsave("results/figures/opt_transfer.pdf",fig03,width=30,height=20,units="cm")

