##Clean the environment in R studio
rm(list=ls())
install.packages("gdxrrw")

# for gdx
# install.packages("gdxtools")
# library(gdxtools)
library(reshape2)
library(gdxrrw)
library(plyr)

# for tables and graphs
library(ggplot2)
library(dplyr)
library(tibble)
library(tidyverse)

###################### SUMMARIZING DEMANDS BY MONTH######################
#########################################################################

## Charge the variable from GDX file

####### Electricity demand
symbol<-rgdx("C:/Users/Esperanza/OneDrive/Building/Model2050_fullresults.gdx",
             squeeze=FALSE,list(name="edemand"))
## Building the dataframe
out1<-tibble(rep(symbol$name,nrow(symbol$val)))
for(i in 1:(symbol$dim)){
  out1<-bind_cols(out1,tibble(symbol$uels[[i]][symbol$val[,i]]))
}
out1<-bind_cols(out1,tibble(symbol$val[,i+1]))
names(out1)<-c("name",symbol$domains,"value")
edemand<- out1

### Summing up the demand by year, region and income level
edemand_summary<- edemand  %>% group_by(m, r, il) %>% summarise(soma_edemand = sum(value))

## Exporting result to Excel
setwd("C:/Users/Esperanza/OneDrive/Building/SummaryBuilding_R")
write.table(edemand_summary , file = "edemand.xls", sep =";", row.names = F)

### Summing up the demand by month, region and income level

### Set up a vector with data
dates<-c("2015-01-01 00:00:00"
         ,"2015-12-31 23:00:00")
s1<-seq(as.POSIXct(dates[1],tz = "UTC"),
        as.POSIXct(dates[2],tz = "UTC"),
        by="h")

### Extract month from the vector
months <- as_tibble(as.numeric(format(s1, "%m")))

### Set a vector repeating the vector months (I have 100 cases) and add the column with the month
edemand_m <- edemand %>% arrange(r, il)%>% mutate(months = rep(months$value, 100))

### Select the colums I want in the final table
edemand_m <- edemand_m  %>% select(m, months, h, r, il, value)

###Summarize the demand by month
edemand_montly_summary<- edemand_m  %>% group_by(m, months, r, il) %>% summarise(edemand_montly = sum(value))

####### Cooling demand
symbol<-rgdx("C:/Users/Esperanza/OneDrive/Building/Model2050_fullresults.gdx",
             squeeze=FALSE,list(name="cdemand"))
## Building the dataframe
out2<-tibble(rep(symbol$name,nrow(symbol$val)))
for(i in 1:(symbol$dim)){
  out2<-bind_cols(out2,tibble(symbol$uels[[i]][symbol$val[,i]]))
}
out2<-bind_cols(out2,tibble(symbol$val[,i+1]))
names(out2)<-c("name",symbol$domains,"value")
cdemand<- out2

### Summing up the demand by year, region and income level
cdemand_summary<- cdemand  %>% group_by(m, r, il) %>% summarise(soma_cdemand = sum(value))

## Exporting result to Excel
setwd("C:/Users/Esperanza/OneDrive/Building/SummaryBuilding_R")
write.table(cdemand_summary , file = "cdemand.xls", sep =";", row.names = F)

######## Auxiliar table for match the demand when there is not demand in all the hours
tableaux<-as_tibble(edemand_m)
tableaux<-tableaux   %>% select(-value) %>% mutate(ind_conc =  paste(tableaux$m,tableaux$h,tableaux$r, tableaux$il, sep = ""))
colnames(tableaux) <- c("year", "months", "hour", "region", "income_level", "ind_conc")
#######################################################################################

####### Join tables
####### Adding column with year, region and hour concatenated
cdemand_aux<-  cdemand   %>% mutate(ind_conc =  paste(cdemand$m,cdemand$h,cdemand$r, cdemand$il, sep = ""))                                                                 
####### Join the tables
jointest<- join(tableaux, cdemand_aux, by="ind_conc")
jointest<- jointest  %>% select(-m, -name, -ind_conc, -h, -r, -il)
###### Setting zero when result is NA
jointest[["value"]][is.na(jointest[["value"]])] <- 0

cdemand_m <- jointest
colnames(cdemand_m) <- c("m", "months", "h", "r", "il", "value")

###Summarize the demand by month
cdemand_montly_summary<- cdemand_m  %>% group_by(m, months, r, il) %>% summarise(cdemand_montly = sum(value))
