####################################################################################################### 
#####                                       Load libraries                                        #####
####################################################################################################### 
source("scripts/hydro/00_load_libraries.R")

####################################################################################################### 
#####                           Load Svensk KraftnÃ¤t historic time series                                         #####
####################################################################################################### 

#######################################################################################################   
skn_ts_final <- NULL
skn_ts_load  <- NULL
regions <- paste("SE",seq(1:4),sep="")



###  read 2007-2010partI data
for(i in 2007:2010){

    # read in excel sheet. First read first header line, then second header line.
    sheet1_colnames<-read_excel(paste("data/svensk_kraftnät/timvarden",i,".xls",sep=""),col_names=TRUE)
    sheet1<-read_excel(paste("data/svensk_kraftnät/timvarden",i,".xls",sep=""),col_names=TRUE,skip=1)
    
    # assign names
    col_names          <- paste(names(sheet1_colnames),names(sheet1),sep=".")
    col_names          <- str_replace_all(col_names, "SN", "SE")
    names(sheet1)      <-col_names
    names(sheet1)[1:2] <- c("date","time")
    remove(sheet1_colnames)
    
    # fill missing values for the first seven hours of 2007
    if (i == 2007) sheet1[1:7,3:44] <- sheet1[8:14,3:44]
    # delete first NA row for 2010
    if (i == 2010) sheet1 <- sheet1[-1,]

    # correct date column
    sheet1$date <- ymd(sheet1$date)
    sheet1$time <- as.numeric(sheet1$time)
    s           <- sapply(sheet1$time,nchar)
    sheet1$time[s==1] <- "0000"
    sheet1$time[s==3] <- paste("0",sheet1$time[s==3],sep="")
    sheet1$date       <- ymd_hm(paste(sheet1$date,sheet1$time))


    
    ####################################################################################
    # read load data ####
    for (j in regions){
      skn_load_detailed <- sheet1 %>%
        select(date=1, matches(j)) %>% 
        select(date=1,
               load_sum    = matches("Summa förbr"),
               load_unspec = matches("Ospec. förbr¶rb"),
               load_avk    = matches("Avk"),
               load_schabl = matches("Schablonförbr"),
               load_schabl_loss = matches("Schablonförbr")) %>% 
        gather(2:ncol(.), key="variable",value="mwh") %>% 
        mutate(date = date, variable = variable, region=as.factor(j), mwh = mwh * -1) %>% 
        arrange(date)
      
      if(is.null(skn_ts_load)){
        skn_ts_load <- skn_load_detailed
      }else{
        skn_ts_load <- rbind(skn_ts_load,skn_load_detailed)
      }
      print(j)
    }
    
    # summarise load datasets
    skn_load <- skn_ts_load %>% 
      group_by(date, region) %>% 
      summarise(mwh = sum(mwh)) %>% 
      ungroup() %>% 
      mutate(date = date, variable = "load", region=as.factor(region), mwh = mwh) %>% 
      select(date, variable, region, mwh) %>% 
      arrange(date, region)
    
    ####################################################################################
    
    
  # read hydro data
    skn_hydro <- sheet1 %>%
      select(date=1, SE = matches("Vattenkraft")) %>%
      mutate(variable="hydro") %>% 
      gather(2:5, key="region",value="mwh") %>% 
      arrange(date)
    
  # read wind data
    skn_wind <- sheet1 %>%
      select(date=1, SE = matches("Vindkraft")) %>%
      mutate(variable="wind") %>% 
      gather(2:5, key="region",value="mwh") %>% 
      arrange(date)
  
  # read thermal data
    skn_thermal <- sheet1 %>%
      select(date=1, SE = matches("Värmekraft")) %>%
      mutate(variable="thermal") %>% 
      gather(2:5, key="region",value="mwh") %>% 
      arrange(date)
    
  # bind datasets and save to tibble
    skn_ts <- rbind(skn_load, skn_hydro,skn_wind,skn_thermal) %>% 
      arrange(date, region)

    if(is.null(skn_ts_final)){
      skn_ts_final <- skn_ts
    }else{
      skn_ts_final <- rbind(skn_ts_final,skn_ts)
    }
    
  }


#######################################################################################################   
### read 2010 part_II NEW
sheet1          <- read_excel("data/svensk_kraftnÃ¤t/timvarden2010II.xls", col_names=FALSE, skip=5)
sheet1_colnames <- read_excel("data/svensk_kraftnÃ¤t/timvarden2010II.xls", col_names=TRUE , skip=1)
sheet1          <- sheet1[,-32] # delete duplicate variable
sheet1_colnames <- sheet1_colnames[,-32] # delete duplicate variable

# assign names
col_names     <- paste(names(sheet1_colnames),sheet1_colnames[1,],sheet1_colnames[2,])
col_names     <- str_replace_all(col_names, "SN", "SE")
names(sheet1) <- col_names
names(sheet1)[1] <- "date"
remove(sheet1_colnames)

sheet1$date <- dmy_h(sheet1$date)

  ####################################################################################
  # read load data ####
  for (j in regions){
    skn_load_detailed <- sheet1 %>%
      select(date=1, matches(j)) %>% 
      select(date=1,
             load_sum    = matches("Summa fÃ¶rbr|TimmÃ¤tt fÃ¶rbr|UppmÃ¤tt fÃ¶rbr"),
             load_unspec = matches("Ospec. fÃ¶rb"),
             load_avk    = matches("avkopplingsb"),
             load_schabl = matches("Schablonleverans fÃ¶rbr"),
             load_schabl_loss = matches("Schablonleverans fÃ¶rl")) %>% 
      gather(2:ncol(.), key="variable",value="mwh") %>% 
      mutate(date = date, variable = variable, region=as.factor(j), mwh = mwh * -1) %>% 
      arrange(date)
    
    if(is.null(skn_ts_load)){
      skn_ts_load <- skn_load_detailed
    }else{
      skn_ts_load <- rbind(skn_ts_load,skn_load_detailed)
    }
    print(j)
  }
  
  # summarise load datasets
  skn_load <- skn_ts_load %>% 
    group_by(date, region) %>% 
    summarise(mwh = sum(mwh)) %>% 
    ungroup() %>% 
    mutate(date = date, variable = "load", region=as.factor(region), mwh = mwh) %>% 
    select(date, variable, region, mwh) %>% 
    arrange(date, region)
  
  ####################################################################################
  
  
  # read hydro data
  skn_hydro <- sheet1 %>%
    select(date=1, SE = matches("Vattenkraft")) %>%
    mutate(variable="hydro") %>% 
    gather(2:5, key="region",value="mwh") %>% 
    arrange(date)
  
  # read wind data
  skn_wind <- sheet1 %>%
    select(date=1, SE = matches("Vindkraft")) %>%
    mutate(variable="wind") %>% 
    gather(2:5, key="region",value="mwh") %>% 
    arrange(date)
  
  # read thermal data
  skn_thermal <- sheet1 %>%
    select(date=1, SE = matches("Värmekraft")) %>%
    mutate(variable="thermal") %>% 
    gather(2:5, key="region",value="mwh") %>% 
    arrange(date)
  
  # bind datasets and save to tibble
  skn_ts <- rbind(skn_load, skn_hydro,skn_wind,skn_thermal) %>% 
    arrange(date, region)
  

  
  if(is.null(skn_ts_final)){
    skn_ts_final <- skn_ts
  }else{
    skn_ts_final <- rbind(skn_ts_final,skn_ts)
  }
  
#######################################################################################################   
###  read 2011-2015 

  

for(i in 2011:2016){ 
  sheet1          <- read_excel(paste("data/svensk_kraftnÃ¤t/timvarden",i,".xls",sep=""),col_names=FALSE,skip=5)
  sheet1_colnames <- read_excel(paste("data/svensk_kraftnÃ¤t/timvarden",i,".xls",sep=""),col_names=TRUE)

  # assign names
  names(sheet1)<-paste(names(sheet1_colnames),sheet1_colnames[1,],sheet1_colnames[2,])
  names(sheet1)[1] <- c("Date")
  remove(sheet1_colnames)

  ####################################################################################
  # read load data ####
for (j in regions){
  skn_load_detailed <- sheet1 %>%
    select(date=1, matches(j)) %>% 
    select(date=1,
           load_sum    = matches("TimmÃ¤tt"),
           load_unspec = matches("Ospec. fÃ¶rb"),
           load_avk    = matches("avkopplingsb"),
           load_schabl = matches("Schablonleverans fÃ¶rbr"),
           load_schabl_loss = matches("Schablonleverans fÃ¶rl")) %>% 
    gather(2:ncol(.), key="variable",value="mwh") %>% 
    mutate(date = date, variable = variable, region=as.factor(j), mwh = mwh * -1) %>% 
    arrange(date)
   
  if(is.null(skn_ts_load)){
    skn_ts_load <- skn_load_detailed
  }else{
    skn_ts_load <- rbind(skn_ts_load,skn_load_detailed)
  }
  print(j)
}

  # summarise load datasets
  skn_load <- skn_ts_load %>% 
    group_by(date, region) %>% 
    summarise(mwh = sum(mwh)) %>% 
    ungroup() %>% 
    mutate(date = date, variable = "load", region=as.factor(region), mwh = mwh) %>% 
    select(date, variable, region, mwh) %>% 
    arrange(date, region)
  
  ####################################################################################
  
  # read hydro data
  skn_hydro <- sheet1 %>%
    select(date=1, SE = matches("Vattenkraft")) %>%
    mutate(variable="hydro") %>% 
    gather(2:5, key="region",value="mwh") %>% 
    arrange(date)
  
  # read wind data
  skn_wind <- sheet1 %>%
    select(date=1, SE = matches("Vindkraft")) %>%
    mutate(variable="wind") %>% 
    gather(2:5, key="region",value="mwh") %>% 
    arrange(date)
  
  # read thermal data
  skn_thermal <- sheet1 %>%
    select(date=1, SE = matches("Värmekraft")) %>%
    mutate(variable="thermal") %>% 
    gather(2:5, key="region",value="mwh") %>% 
    arrange(date)
  
  # bind datasets and save to tibble
  skn_ts <- rbind(skn_load, skn_hydro,skn_wind,skn_thermal) %>% 
    arrange(date, region)
  
  if(is.null(skn_ts_final)){
    skn_ts_final <- skn_ts
  }else{
    skn_ts_final <- rbind(skn_ts_final,skn_ts)
  }
  print(i)
  
  }

####################################################################################################### 

write_feather(skn_ts_final, path = "data/svensk_kraftnÃ¤t/ts_2007_2016.feather")
write_feather(skn_ts_load,  path = "data/svensk_kraftnÃ¤t/ts_load_2007_2016.feather")  
