# This script reads Brazilian hourly load from period 01/01/2015 until 01/01/2017 for the four subsystems of ONS.
# It also manipulate this data to put it in COPA format reading. 

# Reading loads: Swedish and Brazilian
#setwd("C:/Users/cancella/Google Drive/!IIASA/COPA/data/load")
setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA/data/load")
load_se <- as_tibble(read.csv2("load_2007_2015 - original.csv", header = T, sep = ";"))

# Attention to the format of the content of the files! If the numbers are like general, they come to R like factors 
# and the world is beautiful. Otherwise, if it is like the numbers are like numbers, they come like character 
# and we have issues.
#setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/Carga")
setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA  Initial Data/Carga")
load <- as_tibble(read.csv2("Carga Horária_MWhh_01-01-2015 a 01-01-2017 -csv.csv", header = T)) %>% 
  filter(DATA != "MÉDIA") %>% select(-DATA)


dates<-c("2015-01-01 03:00:00"
       ,"2017-01-02 00:00:00")
s1<-seq(as.POSIXct(dates[1],tz = "UTC"),
        as.POSIXct(dates[2],tz = "UTC"),
    by="h")

load$Date<-s1
load<-load %>% gather(Region,Load,-Date)

#spread(load,Region,Load)
load_final<-load %>% mutate(Load=as.numeric(as.character(Load)))
regions_final <- (c(rep("BR3",(nrow(load_final)) / 4),
                   rep("BR4",(nrow(load_final)) / 4),
                  rep("BR1",(nrow(load_final)) / 4),
                 rep("BR2",(nrow(load_final)) / 4)))

load_final<-load_final %>% mutate(Region=regions_final)

setwd("C:/Users/cancella/Google Drive/!IIASA/COPA/data/load")
write_feather(load_final,"load_Br.feather")
write.table(load_final, file = "load_Br.csv", sep =";", row.names = F)

# Plotting
load_final %>% ggplot(aes(x=Date,y=Load)) + geom_line(aes(col=Region)) 

##############################################################################################################
##### Script can end here ####
load_test <- load
load_test <- filter(load, DATA != "MÉDIA")
load_test[sapply(load_test, is.factor)] <- lapply(load_test[sapply(load_test, is.factor)], 
                                                                  function(x) as.numeric(as.character(x)))

# Replicating load in order to have information for all subsystems.
load_full <- bind_rows(load_test, load_test,load_test,load_test)
load_full$Date <- rep(c(""),nrow(load_full))
load_full$Region <- rep(c(""),nrow(load_full))
load_full$Load <- rep(c(0),nrow(load_full))

regions <- as_tibble(c(rep("BR1",(nrow(load_full)) / 4),
             rep("BR2",(nrow(load_full)) / 4),
             rep("BR3",(nrow(load_full)) / 4),
             rep("BR4",(nrow(load_full)) / 4)))

load_full$Region <- regions$value

load_full %>% select(NE,N,SE,S,) %>% mutate(Date=as.character(load$DATA))


# from each 17566 rows, we change the region.
# BR1 <- 17566
# BR2 <- 17566 + 17566
# BR3 <- 17566 + 17566 + 17566 
# BR4 <- 17566 + 17566 + 17566 + 17566

# Filling the column Load

load_full[1:17566,8] <- load_full[1:17566,4] # SE = BR1
load_full[17567:35132,8] <- load_full[1:17566,5] # S = BR2
load_full[35133:52698,8] <- load_full[1:17566,2] # NE = BR3
load_full[52699:nrow(load_full),8] <- load_full[1:17566,3] # NE = BR3

#### DATES ####
# http://www.statmethods.net/input/dates.html dates in R
# http://blog.revolutionanalytics.com/2009/06/converting-time-zones.html
# Copying some lines from Johannes codes.

dates<-c("2015-01-01 01:00:00 CET"
          ,"2017-01-01 23:00:00 CET")
s1<-as_tibble(seq(as.POSIXct(dates[1],tz = "America/Araguaina"),as.POSIXct(dates[2],tz = "America/Araguaina"),by="h"))
s1_test <- bind_cols(s1, load_full[1:17567,4]) # introduction of BR1 load

# adjusting the summer time in Brazil
s1_test[6962:nrow(s1_test),2] <- load_full[6961:17566,4]
s1_test[15698:nrow(s1_test),2] <- load_full[15696:17566,4] 


s1_full <- bind_rows(s1_test,s1_test,s1_test,s1_test)
s1_full[17568:nrow(s1_full),2] <- 0

load_final <- s1_full
load_final$Load <- s1_full$SE
load_final$SE <- rep(c(""), nrow(load_final))
regions_final <- as_tibble(c(rep("BR1",(nrow(load_final)) / 4),
                       rep("BR2",(nrow(load_final)) / 4),
                       rep("BR3",(nrow(load_final)) / 4),
                       rep("BR4",(nrow(load_final)) / 4)))
load_final[,2] <- regions_final
colnames(load_final, do.NULL = TRUE, prefix = "col")
colnames(load_final) <- c("Date", "Region", "Load")


# Introduction BR2, BR3 and BR4 loads
load_final[17568:24527,3] <- load_full[17567:24526,8] # BR2
load_final[24528:33263,3] <- load_full[24526:33261,8]
load_final[33264:35134,3] <- load_full[33261:35131,8]

load_final[35135:42094,3] <- load_full[35133:42092,8] # BR3 
load_final[42095:50830,3] <- load_full[42092:50827,8]
load_final[50831:52701,3] <- load_full[50827:52698,8]

load_final[52702:59661,3] <- load_full[52699:59658,8] # BR4
load_final[59662:68397,3] <- load_full[59658:68393,8]
load_final[68398:nrow(load_final),3] <- load_full[68393:70263,8]

# saving in a csv file
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA/data/load")
save_load_final <- write.table(load_final, file = "load_2007_2015_br.csv", sep = ";", row.names = FALSE )
# OBS: the Brazilian load file period is 2015-01-01 01:00:00 until 2017-01-01 23:00:00.
# I just saved it with the same name in order to COPA reads. 


#### Ploting: Swedish load x Brazilian load  ####
se1 <- filter(load_se, Region == "SE1")
se1 <- se1[52610:70175,]
se1[sapply(se1, is.factor)] <- lapply(se1[sapply(se1, is.factor)], 
                                                  function(x) as.numeric(as.character(x)))
se3 <- filter(load_se, Region == "SE3")
se3 <- se3[52610:70175,]
se3[sapply(se3, is.factor)] <- lapply(se3[sapply(se3, is.factor)], 
                                      function(x) as.numeric(as.character(x)))

se2 <- filter(load_se, Region == "SE2")
se2 <- se2[52610:70175,]
se2[sapply(se2, is.factor)] <- lapply(se2[sapply(se2, is.factor)], 
                                      function(x) as.numeric(as.character(x)))

se4 <- filter(load_se, Region == "SE4")
se4 <- se4[52610:70175,]
se4[sapply(se4, is.factor)] <- lapply(se4[sapply(se4, is.factor)], 
                                      function(x) as.numeric(as.character(x)))

load_test1 <- load_test
id <- as_tibble(c(1:nrow(load_test1)))
load_test1 <- bind_cols(load_test, se1, se3,se2,se4, id)
colnames(load_test1, do.NULL = TRUE, prefix = "col")
colnames(load_test1) <- c("DATA", "NE", "N", "SE", "S", "Date", "Region", "SE1", "Date1", "Region1", "SE3","Date2","Region2","SE2", "Date4","Region4","SE4", "id")

ggplot() + 
  geom_line(data = load_test1, aes(x = id, y = SE, color = "SE")) +
  geom_line(data = load_test1, aes(x = id, y = S, color = "S")) + 
  geom_line(data = load_test1, aes(x = id, y = NE, color = "NE")) +
  geom_line(data = load_test1, aes(x = id, y = N, color = "N")) + 
  geom_line(data = load_test1, aes(x = id, y = SE1, color = "SE1")) + 
  geom_line(data = load_test1, aes(x = id, y = SE3, color = "SE3")) + 
  geom_line(data = load_test1, aes(x = id, y = SE2, color = "SE2")) + 
  geom_line(data = load_test1, aes(x = id, y = SE4, color = "SE4")) + 
  xlab('Date') +
  ylab('Load')


