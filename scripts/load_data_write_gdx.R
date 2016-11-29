base_dir<-"D:/google drive/Anträge/formas/RE_EXTREME github/RE_EXTREME/scripts"
setwd(base_dir)
source("functions_gdx_transfer.R")

####prepare run
period<-c("2008-01-01 00:00:00 CET"
          , "2014-01-02 23:00:00 CET")
prepareFullRun(period)

results<-runGAMSReadResults()
####run and read results
