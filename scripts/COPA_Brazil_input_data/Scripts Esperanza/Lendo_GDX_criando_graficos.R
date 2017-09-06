##Clean the environment in R studio
rm(list=ls())

# for gdx
# install.packages("gdxtools")
# library(gdxtools)
install.packages("gdxrrw")
library(reshape2)
library(gdxrrw)

# for tables and graphs
library(ggplot2)
library(dplyr)
library(tibble)
library(tidyverse)

#### load balance ####

# Carregando variaveis do GDX
# Vamos carregar: 

symbol<-rgdx("C:/Users/cancella/Desktop/Model2050_fullresults.gdx",squeeze=FALSE,list(name="edemand"))
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
edemand<- out
#
symbol<-rgdx("C:/Users/cancella/Desktop/Model2050_fullresults.gdx",squeeze=FALSE,list(name="ele_demand_wh_c"))
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
ele_demand_wh_c<- out


#
symbol<-rgdx("C:/Users/cancella/Desktop/Model2050_fullresults.gdx",squeeze=FALSE,list(name="eB_out"))
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
eB_out<- out

#
symbol<-rgdx("C:/Users/cancella/Desktop/Model2050_fullresults.gdx",squeeze=FALSE,list(name="eB_in"))
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
eB_in<- out


#
symbol<-rgdx("C:/Users/cancella/Desktop/Model2050_fullresults.gdx",squeeze=FALSE,list(name="eGRID"))
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
grid<- out

#
symbol<-rgdx("C:/Users/cancella/Desktop/Model2050_fullresults.gdx",squeeze=FALSE,list(name="ePV_edemand"))

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
output_pv<- out

# Ate aqui, nos estavamos lendo variaveis.
# Agora construimos as tabelas e fazemos os graficos

eletricidade <- cbind(edemand, ele_demand_wh_c$value) %>% mutate(soma = sum(value,ele_demand_wh_c$value))
colnames(eletricidade) <- c("name", "m","h","r","il","value","ele","soma")
eletricidade$soma <- eletricidade$value + eletricidade$ele 

balance <- as_tibble(eletricidade) %>% select(-name, -value, -ele)
colnames(balance) <- c("year", "hour", "region", "income_level", "electricity_demand")

balance <- balance %>% 
  mutate(battery_charge = eB_in$value*(-1), battery_discharge = eB_out$value, Egrid = grid$value, output_pv = output_pv$value)

# ano01: todas as variavaies do balanco de carga para nivel de renda 1, ano 1 e regiao 1. 96 horas
ano01 <- balance %>% filter(year == 5, region == 1, income_level == 4)
ano01 <- ano01[1:(24*4),]

# ano01_graf: tabela auxiliar usada para construir o grafico
ano01_graf <- as_tibble(c(rep(ano01$hour,4)))
ano01_graf <- ano01_graf %>% mutate(c(ano01$battery_charge, ano01$battery_discharge, ano01$Egrid, ano01$output_pv)) 
ano01_graf <- ano01_graf %>% mutate(technology =  c(rep("battery_charge",96),
                                                    rep("battery_discharge",96),
                                                    rep("eGRID",96),
                                                    rep("coutput_pv",96)))
                                                    
colnames(ano01_graf) <- c("hour","value", "technology")


# Electricity demand
line <- as_tibble(ano01$electricity_demand)
line$hour <- ano01$hour

# Fazendo o grafico

# Funcionando: 1o area e 2o barras
# area
ggplot(ano01_graf, aes(x=as.numeric(ano01_graf$hour), y=value, fill=technology)) + 
  geom_area() +
  geom_line(data = line, aes(x = as.numeric(line$hour), y = value, fill = ""),  size =1) +
  ylab("kW") + xlab("hours") + ggtitle("Load balance - 2050") + 
  theme(plot.title = element_text(hjust = 0.5))

# barras
ggplot() + 
  geom_bar(data = ano01_graf, aes(x=as.numeric(ano01_graf$hour), y=value, fill = technology), stat="identity") +
  geom_line(data = line, aes(x = as.numeric(line$hour), y = value, fill = ""),  size =1) +
  ylab("kW") + xlab("hours") + ggtitle("Load balance - 2050") + 
  theme(plot.title = element_text(hjust = 0.5))

# barras coloridas
# Palheta de cores: http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
colors <- c("#FFFFFF","#003366","#336699","#FFCC00","#990033")
ggplot() + 
  geom_bar(data = ano01_graf, aes(x=as.numeric(ano01_graf$hour), y=value, fill = technology), stat="identity") +
  scale_fill_manual (values = colors) +
  geom_line(data = line, aes(x = as.numeric(line$hour), y = value, fill = ""),  size =1) +
  ylab("kW") + xlab("hours") + ggtitle("Load balance - 2050") + 
  theme(plot.title = element_text(hjust = 0.5))

# theme_economist() 



#########
teste <- edemand
soma <- teste %>% group_by(m, r, il) %>% summarise(soma_ag = sum(value))
colors <- c("#CC3333","#FF9933","#336600","#003366")
ggplot() +
  geom_bar(data = soma, aes(x =m, y = soma_ag, fill = il), stat = "identity", position = "dodge") + 
  facet_wrap(~r) + scale_fill_manual (values = colors) +
  theme(axis.text.x = element_text(angle=45)) +
  ylab("kW ") + xlab("years") + ggtitle("Electricity demand" ) + theme(plot.title = element_text(hjust = 0.5))


dates<-c("2015-01-01 00:00:00"
         ,"2015-12-31 23:00:00")
s1<-seq(as.POSIXct(dates[1],tz = "UTC"),
        as.POSIXct(dates[2],tz = "UTC"),
        by="h")

months <- as_tibble(as.numeric(format(s1, "%m")))

teste <- teste %>% arrange(r, il) %>% mutate(monhts = rep(months$value, 100))

teste1 <- teste %>% select(m, monhts, h, r, il, value)

tab_meses <- teste1 %>% group_by(m, monhts, r, il) %>% summarise(monthly_demand = sum(value))

colors <- c("#CC3333","#FF9933","#336600","#003366")
ggplot() +
  geom_line(data = tab_meses, aes(x = monhts, y = monthly_demand, col = il),  size =1)  +
  facet_wrap(~r) + scale_fill_manual (values = colors) +
  theme(axis.text.x = element_text(angle=45)) +
  ylab("kW ") + xlab("months") + ggtitle("Electricity demand" ) + theme(plot.title = element_text(hjust = 0.5))
