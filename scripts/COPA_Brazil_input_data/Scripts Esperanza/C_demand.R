
## Electricity demand (cooling)

symbol<-rgdx("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/!Scripts/Scripts Esperanza/Model2050_fullresults.gdx",squeeze=FALSE,list(name="cdemand"))
name<-symbol$name
if(symbol$dim==0){
  
  return(tibble(name=name,value=symbol$val))
}
###construct data.frame
out_d2<-tibble(rep(symbol$name,nrow(symbol$val)))
for(i in 1:(symbol$dim)){
  
  out_d2<-bind_cols(out_d2,tibble(symbol$uels[[i]][symbol$val[,i]]))
}
out_d2<-bind_cols(out_d2,tibble(symbol$val[,i+1]))
names(out_d2)<-c("name",symbol$domains,"value")
cdemand<- out_d2

### Summing up the demand by year, region and income level
cdemand_summary<- cdemand  %>% group_by(m, r, il) %>% summarise(soma_cdemand = sum(value))

## Exporting result to Excel
#setwd("C:/Users/Esperanza/OneDrive/Building/SummaryBuilding_R")
#write.table(cdemand_summary , file = "cdemand.xls", sep =";", row.names = F)

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
zeros <- as_tibble(c(rep(0,876000-nrow(cdemand)))) # vector of zeros
cdemand_m <- bind_rows(cdemand,zeros) # binding
cdemand_m <- cdemand_m %>% arrange(r, il) %>% mutate(months = rep(months$value, 100))

### Select the colums I want in the final table
cdemand_m <- cdemand_m  %>% select(m, months, h, r, il, value)

###Summarize the demand by month
cdemand_montly_summary<- cdemand_m  %>% group_by(m, months, r, il) %>% summarise(cdemand_montly = sum(value))
