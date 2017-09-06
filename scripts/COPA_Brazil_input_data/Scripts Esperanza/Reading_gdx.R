# for gdx
# install.packages("gdxtools")
# library(gdxtools)
install.packages("gdxrrw")
library(gdxrrw)

# for tables and graphs
library(ggplot2)
library(dplyr)
library(tibble)
library(tidyverse)

#### Johannes mandou esse codigo ####

#### o gdx está na pasta ../gms_execute/resuts_time_resolution.gdx
#### o nome do variável é x_transfer
#### vcs podem trocar o nome do arquivo e do variável!
#### depois de rodar o código, o variável out contém uma tabela com os valores

#### Installed Capacity PV ####
symbol<-rgdx("I:/cancella/Model2050_fullresults.gdx",squeeze=FALSE,list(name="capPV"))

name<-symbol$name
if(symbol$dim==0){
  
  return(tibble(name=name,value=symbol$val))
}

###construct data.frame
out<-tibble(rep(symbol$name,nrow(symbol$val)))
for(i in 1:(symbol$dim)){
  
  out<-bind_cols(out,tibble(symbol$uels[[i]][symbol$val[,i]]))
}
out<-bind_cols(out,tibble(symbol$val[,i+1]))
names(out)<-c("name",symbol$domains,"value")


cap <- as_tibble(out)
cap <- cap[,2:5] 
cap <- mutate(cap, 2015, 2020, 2030, 2040, 2050)
cap$`2015` <- cap$value[cap$m == 1]
cap$`2020` <- cap$value[cap$m == 2]
cap$`2030` <- cap$value[cap$m == 3]
cap$`2040` <- cap$value[cap$m == 4]
cap$`2050` <- cap$value[cap$m == 5]

new_tab <- cap[1:20,]
new_tab <- new_tab[,2:9]
colnames(new_tab) <- c("Region","Income Level","value",2015,2020,2030,2040,2050)

new_tab <- new_tab %>%  select(-value)

setwd("I:/cancella")
write.table(new_tab, file = "capacidade_solar_pv.csv", sep=";", row.names = F)

graf_tab <- new_tab %>% group_by(il) %>% summarise(media = mean(value))
ggplot(new_tab, aes(`2015`, value)) + 
  geom_bar(stat="identity")
ggplot(graf_tab, aes(il, media, fill = il)) + 
  geom_bar(stat="identity") +
  labs(x="Nível de renda", y="Capacidade instalada média(MW)")

#### Output PV ####
symbol<-rgdx("I:/cancella/Model2050_fullresults.gdx",squeeze=FALSE,list(name="ePV"))

name<-symbol$name
if(symbol$dim==0){
  
  return(tibble(name=name,value=symbol$val))
}

###construct data.frame
out<-tibble(rep(symbol$name,nrow(symbol$val)))
for(i in 1:(symbol$dim)){
  
  out<-bind_cols(out,tibble(symbol$uels[[i]][symbol$val[,i]]))
}
out<-bind_cols(out,tibble(symbol$val[,i+1]))
names(out)<-c("name",symbol$domains,"value")

output_PV_old <- out 
somapv <- output_PV %>% group_by(m,r,il) %>% summarise(soma = sum(value))

output_PV_new <- somapv %>% mutate(2015, 2020, 2030, 2040, 2050)
output_PV_new$`2015` <- output_PV_new$soma[output_PV_new$m == 1]
output_PV_new$`2020` <- output_PV_new$soma[output_PV_new$m == 2]
output_PV_new$`2030` <- output_PV_new$soma[output_PV_new$m == 3]
output_PV_new$`2040` <- output_PV_new$soma[output_PV_new$m == 4]
output_PV_new$`2050` <- output_PV_new$soma[output_PV_new$m == 5]
output_PV_new <- output_PV_new[1:20,]
output_PV_new <- output_PV_new[,2:ncol(output_PV_new)]
output_PV_new <- output_PV_new %>% select(-soma)
colnames(output_PV_new) <- c("Region","Income Level",2015,2020,2030,2040,2050)

output_PV_new$Region <- c(rep("SE",4),rep("S",4),rep("CO",4),rep("N",4),rep("NE",4))

setwd("I:/cancella")
write.table(output_PV_new, file = "output_pv.csv", sep=";", row.names = F)





