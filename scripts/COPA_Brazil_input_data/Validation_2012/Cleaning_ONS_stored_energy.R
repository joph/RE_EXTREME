# 10/19/2017 (m/d/y)
# This script reads and cleans data related to stored energy in each subsystem.
# data from ONS website(http://ons.org.br/Paginas/resultados-da-operacao/historico-da-operacao/energia_armazenada.aspx)
# year: 2012, daily values

# Reading ONS daily data 
setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA  Initial Data/ONS validation")
ONS_stored <- read_delim("Comparativo_Energia_Armazenda_Dia_2012.csv", delim = ";", 
                         locale = locale(decimal_mark = ",")) %>% select(1,3,6)
colnames(ONS_stored) <- c("date", "reg", "value")
ONS_stored$date <- parse_datetime(ONS_stored$date, format ="%d/%m/%Y")

ggplot(ONS_stored, aes(date, value, col = reg))+ geom_line(size = 1)
# Reading br_shype_hydro_2012.feather (file with the adapt factors)
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA/data/hydro")
#shype_br <- read_feather("br_shype_hydro_2012.feather")

