# This script reads Brazilian hourly load from period 01/01/2012 to 01/01/2014 for the four subsystems of ONS.
# It also manipulate this data to put it in COPA format reading.
# It changes the dates and the region names in order to COPA reads. 
# Actually, SE3 = NE = BR3 and 2014-01-01 = 2015-01-01

# Reading loads: Swedish and Brazilian
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA/data/load")
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA/data/load")
load_se <- as_tibble(read.csv2("load_2007_2015 - original.csv", header = T, sep = ";"))

setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/Carga")
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA  Initial Data/Carga")
# Attention to the format of the content of the files! If the numbers are like general, they come to R like factors 
# and the world is beautiful. Otherwise, if it is like the numbers are like numbers, they come like character 
# and we have issues. 
load <- as_tibble(read.csv2("Carga Horária_01-01-2012 a 01-01-2014.csv", header = T)) %>% 
  filter(Instante != "Média") %>% select(-Instante, -SIN)

#dates<-c("2015-01-01 03:00:00"
 #        ,"2017-01-02 00:00:00")

# That's what I had before real data 2012-2014
#dates<-c("2014-01-01 05:00:00"
 #       ,"2016-01-03 02:00:00")

dates<-c("2012-01-01 01:00:00"
         ,"2014-01-02 00:00:00")

s1<-seq(as.POSIXct(dates[1],tz = "UTC"),
        as.POSIXct(dates[2],tz = "UTC"),
        by="h")
load$Date<-s1

load<-load %>% gather(Region,Load,-Date)

#spread(load,Region,Load)
load_final<-load %>% mutate(Load=as.numeric(as.character(Load)))

regions_final <- (c(rep("SE3",(nrow(load_final)) / 4),
                            rep("SE4",(nrow(load_final)) / 4),
                           rep("SE1",(nrow(load_final)) / 4),
                          rep("SE2",(nrow(load_final)) / 4)))

load_final<-load_final %>% mutate(Region=regions_final)

#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA/Data/load")
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA/Data/load")
write_feather(load_final,"load_Br_2014.feather")
write.table(load_final, file = "load_Br_2014.csv", sep =";", row.names = F)

# Plotting
load_final %>% ggplot(aes(x=Date,y=Load)) + geom_line(aes(col=Region)) 
