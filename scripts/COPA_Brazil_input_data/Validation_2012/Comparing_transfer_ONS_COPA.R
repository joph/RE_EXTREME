# This script compares ONS daily electricity transfers from 2012  to COPA transfer results

# COPA daily transfer
x_transfer <- results %>% filter(name == "x_transfer")
x_transfer$reg[x_transfer$reg == "NF"] <- "N"
x_transfer$reg1[x_transfer$reg1 == "NF"] <- "N"


transfer_daily<- x_transfer %>% group_by(day(datetime), month(datetime),reg,reg1) %>% summarize(value=sum(value),
                                                                               datetime=min(datetime))

transfer_daily <- transfer_daily %>% filter(value != 0) %>% mutate(index = str_c(reg, reg1, sep=".")) %>% arrange(month(datetime))

# selecting the same regions of ONS
transfer_daily_graph <- transfer_daily %>% filter(index == "NE.SE/CO"|index == "N.NE"|
                                                    index == "N.SE/CO"| index == "SE/CO.SUL") 

COPA <- ggplot(data = transfer_daily_graph, aes(x=datetime,y=value/1e3)) +geom_line(aes(col=index), size =1)
plot(COPA)
# reading ONS transfer
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/ONS validation")
#setwd(PC)
ONS_transfer <- read_csv("transfer_ONS.csv")
ONS <-ggplot(ONS_transfer)+ geom_line(aes(x = Date, y = value, col = reg), size=1)
plot(ONS)

# joining both graphs
ONS_transfer <- ONS_transfer[1:1463,] # removing the last line # TO DO: CHECK THE NROW OF ONS AND COPA TRANSFER TABLES
ONS_transfer <- ONS_transfer %>% select(Date, reg, value)

transfer_daily_graph$index[transfer_daily_graph$index== "NE.SE/CO"] <- "Nordeste - Sudeste"
transfer_daily_graph$index[transfer_daily_graph$index== "N.NE"]     <- "Norte - Nordeste"
transfer_daily_graph$index[transfer_daily_graph$index== "N.SE/CO"]  <- "Norte - Sudeste"
transfer_daily_graph$index[transfer_daily_graph$index== "SE/CO.SUL"]<- "Sudeste - Sul"
transfer_daily_graph$value <- transfer_daily_graph$value/1e3

COPA_transfer <- transfer_daily_graph %>% select(datetime, index, value) %>% arrange(index)
COPA_transfer <- COPA_transfer[,4:6]
colnames(COPA_transfer) <- colnames(ONS_transfer)
COPA_transfer$Date <- ONS_transfer$Date[1:1463]

transfer_comp <- bind_rows(ONS_transfer, COPA_transfer)
transfer_comp$type <- c(rep("ONS",1463), rep("COPA", 1463))


transfer_comparation <- ggplot(transfer_comp) + geom_line(aes(x = Date, y = value, col = reg), size=1) + facet_wrap(~type, ncol = 2) +
                        theme(axis.text.x = element_text(angle=45)) + ylab("GWh") + xlab("") + 
                        ggtitle("Comparation electricity exchanges" ) + theme(plot.title = element_text(hjust = 0.5))
plot(transfer_comparation)
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA/runs/Validation_2012/figures")
grDevices::pdf("comparation_electricity_exchanges_ONS_COPA.pdf")
transfer_comparation
dev.off()