base_dir<-"D:/google drive/Anträge/formas/RE_EXTREME github/RE_EXTREME"
setwd(base_dir)
source("scripts/functions_gdx_transfer.R")

           
solDat<-sapply(c(1:4),
               function(i){read_excel("data/solar/SE.xlsx",sheet=i)[-1]},
               simplify=FALSE)

solDat1<-mapply(function(n,i){mutate(solDat[[i]],region=n)},
                c("SE1","SE2","SE3","SE4"),
                c(1:4),
                SIMPLIFY=FALSE)

library(gmp)
library(solaR)



solDatTot<-bind_rows(solDat1) %>% 
           mutate(date=as.character(as.POSIXct(as.character(as.bigz(Time)),format="%Y%m%d%H0000")),
                  G0=`SIS (W/m2)`) %>% select(date,G0,region) %>% as_data_frame() %>% as_tibble()

solDatTot<-mutate(solDatTot,date=as.POSIXct(date))
solDatTot<-data.frame(solDatTot)

lats<-c(57,59,63,66)
regs<-c("SE1","SE2","SE3","SE4")

finalPV<-NULL

for(i in 1:4){

  reg<-regs[i]
  dat<-filter(solDatTot,region==regs[i]) %>% select(date,G0)
  irr<-dfI2Meteo(file=data.frame(dat),
               lat=lats[i],
               time.col="date",
               format='%Y-%m-%d %H:%M:%S')

  p<-prodGCPV(lat=lats[1],
            modeRad='bdI',
            dataRad=irr)




  p@prodI[is.na(p@prodI[,8]),8]<-0

  #divide by kwP (26.5 in prodGCPV standard)
  hourly_pv<-p@prodI[,8]/26.5/1000#####Latitudes: get them later from sennai
  
  finalPV<-bind_rows(finalPV,
            tibble(Date=dat$date,R=reg,P="P1",capFact=hourly_pv))
}

#convert to solar power


finalPV<-mutate(finalPV,Date=as.POSIXct(Date))
write_feather(finalPV,"data/solar/solar_GAMS.feather")

ggplot(finalPV, aes(x=Date, y=capFact)) +  geom_line(aes(color=R))

#ggplot(finalPV, aes(x=Date, y=capFact)) +  geom_line() 
