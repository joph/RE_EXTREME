  # This script compares results of generation of electricity by source and region of ONS and COPA Brazil.
  # Daily values of electricity generation from ONS
  # Period: 1/1/2012 to 12/31/2012
  
  setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/ONS validation")
  #setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA  Initial Data/ONS validation")
  
  # defining function to put ons and copa information together
  prepareData<-function(gen_ons_copa){
    
    
    gen_ons_copa_tog <- gen_ons_copa
    gen_ons_copa_tog <- gen_ons_copa_tog %>% mutate(type = c(rep("ons",nrow((gen_ons_copa))/2), rep("copa", nrow(gen_ons_copa)/2)))
    gen_ons_copa_tog$iTechnology[gen_ons_copa_tog$iTechnology == "hydro_ons"]    <- "hydro"
    gen_ons_copa_tog$iTechnology[gen_ons_copa_tog$iTechnology == "hydro_copa"]   <- "hydro"
    gen_ons_copa_tog$iTechnology[gen_ons_copa_tog$iTechnology == "Thermal_ons"]  <- "thermal"
    gen_ons_copa_tog$iTechnology[gen_ons_copa_tog$iTechnology == "Thermal_copa"] <- "thermal"
    gen_ons_copa_tog$iTechnology[gen_ons_copa_tog$iTechnology == "wind_ons"]     <- "wind"
    gen_ons_copa_tog$iTechnology[gen_ons_copa_tog$iTechnology == "wind_copa"]    <- "wind"
    
    return(gen_ons_copa_tog)
    
  }
  
  # Defining regions names
  # it is different from the same variable in "load_data_write_gdx.R", because we  don't need the virtual node here
  namesNew<-c("SE/CO","SUL","NE","N") 
  
  for (i in c(1:4)){
    #setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA  Initial Data/ONS validation")
    setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/ONS validation")
    #  i=1
    #### reading ONS data generated on "Cleaning_ONS_validation_data.R" ####
    if(namesNew[i] == "SE/CO"){
      namesNew[i] <- "SE.CO"
      gen_ons_final <- read_csv(paste("geracao_ONS_",namesNew[i],".csv", sep=""))
      namesNew[i] <- "SE/CO"
    } else {gen_ons_final <- read_csv(paste("geracao_ONS_",namesNew[i],".csv", sep=""))}
    
    #### COPA values - dayly basis ####
    # Passing to daily basis
    hydro_COPA <- results %>% filter(name == "x_hydro_tot" & reg == namesNew[i]) %>% select(datetime, value) %>% 
      group_by(month(datetime), day(datetime)) %>% summarise(production = sum(value))
    
    term_COPA <- results %>% filter(name == "x_term" & reg == namesNew[i]) %>% select(datetime, value) %>%
      group_by(month(datetime), day(datetime)) %>% summarise(production = sum(value))
    
    # Concatenating COPA's hydro, thermal and wind (when it is the case) generation 
    gen_copa <- bind_rows(term_COPA, hydro_COPA)
    gen_copa$iTechnology <- c(rep("Thermal_copa",nrow(term_COPA)), rep("hydro_copa",nrow(hydro_COPA)))
    
    # checking if ons data has wind generation
    if (gen_ons_final$iTechnology == "wind_ons") 
    {wind_COPA <- results %>% filter(name == "x_renew" & reg == namesNew[i] & iTechnology == "Wind") %>% select(datetime, value) %>% 
      group_by(month(datetime),day(datetime)) %>% summarise(production = sum(value))
    gen_copa <- bind_rows(wind_COPA, term_COPA, hydro_COPA)
    gen_copa$iTechnology <- c(rep("wind_copa",nrow(wind_COPA)),
                              rep("Thermal_copa",nrow(term_COPA)),
                              rep("hydro_copa", nrow(hydro_COPA)))
    } else {}
    
    gen_copa_final <- gen_copa %>% ungroup() %>% mutate(Date = gen_ons_final$Date) %>% 
      select(Date,iTechnology, production)
    
    #### Comparation between ONS and COPA ####
    # Units -> everything in GWh
    # passing copa values to GWh
    gen_copa_final$production <- gen_copa_final$production / 1e3
    gen_ons_copa <- bind_rows(gen_ons_final, gen_copa_final)
    
    # preparing data for plots
    gen_ons_copa_tog<-prepareData(gen_ons_copa)
    # defining paths for figures 
    path_fig <- "C:/Users/cancella/Google Drive/!IIASA/COPA/runs/Validation_2012/figures"
    #path_fig <- "C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA/runs/Validation_2012/figures"
    if(namesNew[i] == "SE/CO"){
      namesNew[i] <- "SE.CO"
      print(namesNew[i])
    } else {namesNew[i] == namesNew[i]
      print (namesNew[i])
    }
    # lines together
    lines_together <- ggplot() + 
      geom_line(data = gen_ons_copa_tog, aes(x = Date, y = production, col = type), size = 1) +
      scale_fill_brewer(palette="Set2") + facet_wrap(~iTechnology) + scale_color_hue(l=45)+theme(axis.text.x = element_text(angle=45))+
      ylab("GWh") + xlab("") +  ggtitle(paste("Comparing generation - ", namesNew[i], sep = "" )) + 
      theme(plot.title = element_text(hjust = 0.5))
    plot(lines_together)
    #ggsave(paste(path_fig,"/lines_tog_", namesNew[i], ".pdf", sep=""),lines_together,width=30,height=20,units="cm")
    #ggsave("C:/Users/cancella/Google Drive/!IIASA/COPA/runs/Validation_2012/figures/SE/lines_tog_se.pdf",lines_together,width=30,height=20,units="cm")
    
    # Plotting time-series in points to see the differences
    # hydro
    points_hydro <- tibble(
      ons =   c(gen_ons_copa$production[gen_ons_copa$iTechnology == "hydro_ons"]), 
      copa =  c(gen_ons_copa$production[gen_ons_copa$iTechnology == "hydro_copa"]))
    plot_points_hydro <- ggplot(data = points_hydro, aes(x = ons, y = copa)) + geom_point() +
      ylab("ONS") + xlab("COPA") + ggtitle(paste("ONS x COPA - hydro - ", namesNew[i], sep="")) + theme(plot.title = element_text(hjust = 0.5))
    plot(plot_points_hydro)
    #ggsave(paste(path_fig,"/points_hydro_", namesNew[i], ".pdf", sep=""),plot_points_hydro,width=30,height=20,units="cm")
    #ggsave("C:/Users/cancella/Google Drive/!IIASA/COPA/runs/Validation_2012/figures/SE/points_hydro_se.pdf",plot_points_hydro_se,width=30,height=20,units="cm")
    
    # thermal
    points_thermal <- tibble(
      ons =   c(gen_ons_copa$production[gen_ons_copa$iTechnology == "Thermal_ons"]), 
      copa =  c(gen_ons_copa$production[gen_ons_copa$iTechnology == "Thermal_copa"]))
    plot_points_thermal <- ggplot(data = points_thermal, aes(x = ons, y = copa)) + geom_point() +
      ylab("ONS") + xlab("COPA") + ggtitle(paste("ONS x COPA - thermal - ", namesNew[i], sep=""))  + theme(plot.title = element_text(hjust = 0.5))
    plot(plot_points_thermal)
    #ggsave(paste(path_fig,"/points_thermal_", namesNew[i], ".pdf", sep=""),plot_points_thermal,width=30,height=20,units="cm")
    #ggsave("C:/Users/cancella/Google Drive/!IIASA/COPA/runs/Validation_2012/figures/points_thermal_se.pdf",points_thermal_se,width=30,height=20,units="cm")
    
    # wind (if it is the case)
    if(gen_ons_final$iTechnology == "wind_ons"){
      points_wind <- tibble(
        ons =   c(gen_ons_copa$production[gen_ons_copa$iTechnology == "wind_ons"]), 
        copa =  c(gen_ons_copa$production[gen_ons_copa$iTechnology == "wind_copa"]))
      plot_points_wind <- ggplot(data = points_wind, aes(x = ons, y = copa)) + geom_point() +
        ylab("ONS") + xlab("COPA") + ggtitle(paste("ONS x COPA - wind - ", namesNew[i], sep=""))  + theme(plot.title = element_text(hjust = 0.5))
      plot(plot_points_wind)
      #ggsave(paste(path_fig,"/points_wind_", namesNew[i], ".pdf", sep=""),plot_points_wind,width=30,height=20,units="cm")
    } else {}
    
    #### Multiplot with the more important plots ####
    setwd(path_fig)
    #source("C:/Users/cancella/Google Drive/!IIASA/COPA/RE_EXTREME/scripts/COPA_Brazil_input_data/multiplot.R")
    #source("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA/RE_EXTREME/scripts/COPA_Brazil_input_data/multiplot.R")
    if(gen_ons_final$iTechnology == "wind_ons"){
      #summary_plots <- multiplot(lines_together, plot_points_hydro,plot_points_thermal, plot_points_wind)
      pdf(paste("summary_", namesNew[i],".pdf", sep=""))
      plot(lines_together)
      plot(plot_points_hydro)
      plot(plot_points_thermal)
      plot(plot_points_wind)
      dev.off()
      
    } else {#summary_plots <- multiplot(lines_together, plot_points_hydro,plot_points_thermal)
      pdf(paste("summary_", namesNew[i],".pdf", sep=""))
      plot(lines_together)
      plot(plot_points_hydro)
      plot(plot_points_thermal)
      dev.off()
    }
    
    #ggsave(paste(path_fig,"/summary_", namesNew[i],".pdf", sep=""),summary_plots, width=30,height=20,units="cm")
    #ggsave(paste(path_fig,"/summary_", namesNew[i],".pdf", sep=""),summary_plots, width=30,height=20,units="cm")
    
    #### Statistical indicators ####
    # Correlation between time-series of ONS x COPA
    setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/ONS validation")
    #setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA  Initial Data/ONS validation")
    correlation_hydro <- as_tibble(cor(gen_ons_copa$production[gen_ons_copa$iTechnology == "hydro_ons"],
                                       gen_ons_copa$production[gen_ons_copa$iTechnology == "hydro_copa"]))
    correlation_thermal <- as_tibble(cor(gen_ons_copa$production[gen_ons_copa$iTechnology == "Thermal_ons"],
                                         gen_ons_copa$production[gen_ons_copa$iTechnology == "Thermal_copa"]))
    reg_hydro <- lm(copa ~ ons , data = points_hydro)
    rmse_hydro <- modelr::rmse(reg_hydro, points_hydro)
    
    reg_thermal <- lm(copa ~ ons , data = points_thermal)
    rmse_thermal <- modelr::rmse(reg_thermal, points_thermal)
    
    # Generating wind correlation (if it is the case)
    if(gen_ons_final$iTechnology == "wind_ons"){
      correlation_wind <- as_tibble(cor(gen_ons_copa$production[gen_ons_copa$iTechnology == "wind_ons"],
                                        gen_ons_copa$production[gen_ons_copa$iTechnology == "wind_copa"]))
      reg_wind <- lm(copa ~ ons , data = points_wind)
      rmse_wind <- modelr::rmse(reg_wind, points_wind)
      stats_summary <- tibble(
        statistic = c("hydro", "thermal", "wind"),
        correlation = c(correlation_hydro$value, correlation_thermal$value, correlation_wind$value),
        RMSE = c(rmse_hydro, rmse_thermal, rmse_wind))
      write_feather(stats_summary,paste("stats_summary_", namesNew[i], ".feather", sep=""))
    } else 
    {stats_summary <- tibble(
      statistic = c("hydro", "thermal"),
      correlation = c(correlation_hydro$value, correlation_thermal$value),
      RMSE = c(rmse_hydro, rmse_thermal))
    write_feather(stats_summary,paste("stats_summary_", namesNew[i], ".feather", sep=""))}
    
  } # closing the initial loop