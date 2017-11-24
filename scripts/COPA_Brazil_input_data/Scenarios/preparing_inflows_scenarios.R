# 17/11/17 (yy/mm/dd)
# This script reads the inflows file (1979 to 2014) and multiplies it for the 0.93 calibration factor. 
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA/data/hydro")
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA/data/hydro")
br_shype_hydro<- read_feather("br_shype_hydro.feather")

# multiplying by the calibration factor
br_shype_hydro$mwh <- br_shype_hydro$mwh * 0.93

setwd("C:/Users/cancella/Google Drive/!IIASA/COPA/data/hydro")
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA/data/hydro")
write_feather(br_shype_hydro,"br_shype_hydro_corrected.feather")
