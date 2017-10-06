# This script clean the transfer file from ONS
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/ONS validation")
transfer <- read_delim("Comparativo_Intercâmbio_de_Energia_2012.csv", delim = ",", locale = locale(decimal_mark = "."))
colnames(transfer) <- c("Date", "reg", "Date1", "reg1", "period", "value")

transfer <- transfer %>% select(Date, reg, reg1, value)
transfer$Date <- as.Date(transfer$Date, format ="%m/%d/%Y %H:%M:%S" )

ggplot(transfer)+ geom_line(aes(x = Date, y = value, col = reg), size=1)

#saving in a csv file
write_csv(transfer, "transfer_ONS.csv")
