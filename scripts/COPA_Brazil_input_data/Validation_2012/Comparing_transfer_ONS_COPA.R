# This script compares ONS daily electricity transfers from 2012  to COPA transfer results

# COPA daily transfer
x_transfer <- results %>% filter(name == "x_transfer")
x_transfer$reg[x_transfer$reg == "NF"] <- "N"
x_transfer$reg1[x_transfer$reg1 == "NF"] <- "N"


transfer_daily<- x_transfer %>% group_by(day(datetime), month(datetime),reg,reg1) %>% summarize(value=sum(value),
                                                                               datetime=min(datetime))

transfer_daily <- transfer_daily  %>% mutate(index = str_c(reg, reg1, sep=".")) %>% arrange(month(datetime))
# %>% filter(value != 0)
# selecting the same regions of ONS
transfer_daily_graph <- transfer_daily %>% filter(index == "NE.SE/CO"|index == "N.NE"|
                                                    index == "N.SE/CO"| index == "SE/CO.SUL") 

COPA <- ggplot(data = transfer_daily_graph, aes(x=datetime,y=value/1e3)) +geom_line(aes(col=index), size =1)
plot(COPA)
# reading ONS transfer
#setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/ONS validation")
setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA  Initial Data/ONS validation")
ONS_transfer <- read_csv("transfer_ONS.csv")
ONS <-ggplot(ONS_transfer)+ geom_line(aes(x = Date, y = value, col = reg), size=1)
plot(ONS)

# joining both graphs
ONS_transfer <- ONS_transfer %>% select(Date, reg, value)

transfer_daily_graph$index[transfer_daily_graph$index== "NE.SE/CO"] <- "Nordeste - Sudeste"
transfer_daily_graph$index[transfer_daily_graph$index== "N.NE"]     <- "Norte - Nordeste"
transfer_daily_graph$index[transfer_daily_graph$index== "N.SE/CO"]  <- "Norte - Sudeste"
transfer_daily_graph$index[transfer_daily_graph$index== "SE/CO.SUL"]<- "Sudeste - Sul"
transfer_daily_graph$value <- transfer_daily_graph$value/1e3

COPA_transfer <- transfer_daily_graph %>% select(datetime, index, value) %>% arrange(index)
COPA_transfer <- COPA_transfer[,4:6]
colnames(COPA_transfer) <- colnames(ONS_transfer)
COPA_transfer$Date <- ONS_transfer$Date

transfer_comp <- bind_rows(ONS_transfer, COPA_transfer)
transfer_comp$type <- c(rep("ONS",nrow(ONS_transfer)), rep("COPA", nrow(COPA_transfer)))

transfer_comparation <- ggplot(transfer_comp) + geom_line(aes(x = Date, y = value, col = reg), size=1) + facet_wrap(~type, ncol = 1) +
                        scale_color_hue(l=45)+theme(axis.text.x = element_text(angle=45)) + ylab("GWh") + xlab("") + 
                        ggtitle("Comparation electricity exchanges" ) + theme(plot.title = element_text(hjust = 0.5))
plot(transfer_comparation)

#### comparing total transfer ####
transfer_comp_t <- transfer_comp
for (i in c(1:nrow(transfer_comp_t))){
  if(transfer_comp_t$value[i] < 0 ){
    transfer_comp_t$value[i] <- transfer_comp_t$value[i] * (-1)
  }else {}
}
transfer_comp_t <- transfer_comp_t %>% group_by(reg, type) %>% summarise(total = sum(value))

total_transfer <- ggplot(data = transfer_comp_t) + geom_bar(aes(x = reg, y = total,fill = type), stat = "identity", position = "dodge") +
  scale_fill_hue(l=45)+ ylab("GWh") + xlab("") + ggtitle("Total transfer") + 
  theme(plot.title = element_text(hjust = 0.5))
plot(total_transfer)  

total_summary <- transfer_comp_t %>% group_by(type) %>% summarise(total = sum(total))
# COPA transfer almost 2x more electricity than ONS does.

#### saving plots in pdf file ####
 #setwd("C:/Users/cancella/Google Drive/!IIASA/COPA/runs/Validation_2012/figures")
 setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA/runs/Validation_2012/figures")
 grDevices::pdf("comparation_electricity_exchanges_ONS_COPA.pdf")
 transfer_comparation
 total_transfer
 dev.off()

