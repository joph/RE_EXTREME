# This script compares the load used in copa and the load of ONS website
# We expect they to be equal

load_copa <- results %>% filter(name == "load")
load_copa <- load_copa %>% select(reg, datetime, value)

# ONS load
#setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/ONS validation")
setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA  Initial Data/ONS validation")
load_ons <- as_tibble(read.csv2("Comparativo_Carga_de_Energia_Dia_Hora_Jul_2012.csv", header = T, sep = ","))
load_ons <- load_ons[1:2884,]
load_ons <- load_ons[,c(1,2,5)]

load_ons_i <- load_ons
load_ons_i[sapply(load_ons_i, is.factor)] <- lapply(load_ons_i[sapply(load_ons_i, is.factor)], 
                                                      function(x) as.numeric(as.character(x)))

load_ons$Selecione.Tipo.de.CDH.Comp.3 <- load_ons_i$Selecione.Tipo.de.CDH.Comp.3
colnames(load_ons) <- c("datetime","reg","value")
load_ons <- load_ons %>% select(reg, datetime, value)

datetime<-as.character(load_ons$datetime) %>% strptime("%m/%d/%Y %I:%M:%S %p")
datetime <-datetime + 3600
head(datetime,24)

load_ons$datetime<-datetime


load_ons$reg2 <- c(rep("SE002", 721), rep("SE001", 721),rep("SE004", 721),rep("SE003", 721))
j<-full_join(load_ons,load_copa,by=c("datetime"="datetime", "reg2"="reg"))

j %>% ggplot(aes(x=value.x,y=value.y)) + geom_point(aes(col=reg2)) +
        ylab("ONS") + xlab("COPA") +  ggtitle("Comparing load" ) + 
        theme(plot.title = element_text(hjust = 0.5))
