# 10/11/2017 (m/d/a)
# This script reads "EXP.DAT" and detects hydro power plants that starts operation after december 2012. 
# These plants will be removed from maxHydPower parameter in "Creating_hydro_br_2012.R".

setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/Newave_deck_dec_2012")
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA  Initial Data/Newave_deck_dec_2012")
#exph12 <- read.delim("EXPH - Copy.DAT", delim = " ",locale = locale(decimal_mark = ".") )
# Before reading this file in R, it demand a little of manual work in order to clean this file. 
# Once it it able to be read, this script starts.
exph12 <- as_tibble(read.delim("EXPH - Copy.DAT", header = TRUE, sep = "", quote = "\"", dec = ".", 
                     fill = TRUE, row.names = NULL, comment.char = ""))
exph12 <- exph12 %>% group_by(COD, NOME) %>% summarise(PotEfUn = sum(POT.))

# saving in a .csv file to be read by "Creating_hydro_br_2012.R"
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/Hydro")
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA  Initial Data/Hydro")
#write_csv(exph12, "exph12.csv")
