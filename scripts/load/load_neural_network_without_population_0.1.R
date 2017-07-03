base_dir<-"D:/google drive/Anträge/formas/RE_EXTREME github/RE_EXTREME"
setwd(base_dir)
source("scripts/MERRA_data.R")
source("scripts/libraries.R")


########raster data/clipping########
elomrade <- readOGR("../gis_data/elomrade/elomrade_test.shp")
#pop <- raster("../gis_data/GLOB_population_data/gpw-v4-population-count-adjusted-to-2015-unwpp-country-totals_2015.tif")

######read merra grid
ncname<-"../MERRA/LonLat10.95833_55.33333_24.16667_69.05833/MERRA_10.95833_55.33333_24.16667_69.05833_t2m_19830201.nc4"
ncfile <- nc_open(ncname)

#Longitude
longitude <- ncvar_get(ncfile, "lon", verbose = F)
nlon <- dim(longitude)

#Latitude
latitude <- ncvar_get(ncfile, "lat", verbose = F)
nlat <- dim(latitude)
nc_close(ncfile)

#####make grid for MERRA data and match with elomrade
lonlat <- expand.grid(longitude,latitude)
coordinates(lonlat) <- cbind(lonlat$Var1 , lonlat$Var2)
lonlat$id<-c(1:nrow(lonlat))
proj4string(lonlat) = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
lonlat<-spTransform(lonlat, CRS(proj4string(elomrade)))
sp<-over(lonlat,elomrade) %>% as_tibble() %>% mutate(lon=lonlat$Var1,lat=lonlat$Var2) 
sp_red<-sp[!is.na(sp$OBJECTID),]

#########################load data from MERRA

#date_seq<-seq(as.POSIXct("1980-01-01"),as.POSIXct("2017-03-31"),by="d")
#date_seq<-seq(as.POSIXct("1984-01-01"),as.POSIXct("1984-01-20"),by="d")
#date_seq<-seq(as.POSIXct("2007-01-01"),as.POSIXct("2007-01-02"),by="d")
date_seq<-seq(as.POSIXct("2007-01-01"),as.POSIXct("2017-03-31"),by="d")

########boundary box for downloading MERRA data for Sweden
lon1<-10.95833
lat1<-55.33333
lon2<-24.16667
lat2<-69.05833
setwd(base_dir)

###########download MERRA data
getMERRADataBox(lon1,lat1,lon2,lat2,
                date_seq,c("t2m"),
                "RE_EXTREME",
                "Re_extreme666!",
                FALSE)

###########convert MERRA data to binary format data
convertMerraFeather(lon1,lat1,lon2,lat2,"T2M","t2m",date_seq)

####get data from elomrade 4
pointsToLoad<-sp_red %>% filter(elomrade==4) %>% dplyr::select(lon,lat)

####load temperature data from disk
temperature_data<-mapply(interpolatePoint,"T2M",list(date_seq),
       pointsToLoad$lon,pointsToLoad$lat,
       lon1,lat1,lon2,lat2)

colnames(temperature_data)<-paste("C",1:ncol(temperature_data),sep="")
date_seq_h<-date_seq<-seq(as.POSIXct("2007-01-01"),as.POSIXct("2017-03-31"),by="h")

#####convert data from kelvin to degree celsius
#####filter data between 2007 and 2015
t_dat<-temperature_data %>% as_tibble() %>%  mutate(date=date_seq_h) %>% 
  gather(var,val,-date) %>% mutate(val=val-273.6) %>% 
  spread(var,val) %>% filter(between(year(date),2007,2015))



#####################HERE BEGINS THE MAGIC
#####################REGRESSION MODEL & NEURAL NETWORK

####get load data from disk
loadFeather<-"../data/load/load_2007_2015.feather"
load<-read_feather(loadFeather)

####filter region 4 and create sinus/cosinus cycle
####add day of week and hour of day
load<-read_feather(loadFeather) %>% 
  filter(between(year(Date),2007,2015) & Region=="SE4") %>% 
   mutate(
         seasonalC=cos(2*pi*(1:n())/8760),
         seasonalS=sin(2*pi*(1:n())/8760),
         hour=factor(hour(Date)),
         wday=wday(Date))

####add temperature data
load<-bind_cols(load,t_dat[,2:ncol(t_dat)])

####add lagged load data
lags<- load %>% mutate(lag1=lag(Load),
                       lag24=lag(Load,24),
                       lag2=lag(Load,2),
         lag3=lag(Load,3),
         lag4=lag(Load,4),
         lag5=lag(Load,5),
         lag6=lag(Load,6),
         lag7=lag(Load,7),
         lag8=lag(Load,8),
         lag9=lag(Load,9),
         lag10=lag(Load,10),
         lag11=lag(Load,11),
         lag12=lag(Load,12),
         lag25=lag(Load,25)) %>% dplyr::select(lag1,lag2,lag3,lag4,lag5,lag6,
                                        lag7,lag8,lag9,lag10,lag11,lag12,
                                        lag24,lag25)


#####create weekday, saturday, sunday dummy
load$wday[load$wday>1 & load$wday < 7]<-3
load$wday[load$wday==7]<-2
load<-load %>% mutate(wday=factor(wday))

#####save data for later use
load_<-load

#####bind lags to load

load<-bind_cols(load,lags)


#####omit NA lines
load<-na.omit(load)

#####for regression analysis, we are not interested in first two columns (date & region)
data<-load[,c(3:ncol(load))]

######train on first three years
######test on residual years
index<-1:(3*8760)
train <- data[index,]
test <- data[-index,]

######fit linear regression model
lm.fit <- lm(Load~.,data=train)
summary(lm.fit)

######some statistical testing on residuals
hist(residuals(lm.fit))
pacf(residuals(lm.fit))

ks.test(x=residuals(lm.fit),y='pnorm',alternative='two.sided')
ks.test(x=rnorm(1000),y='pnorm',alternative='two.sided')

qqnorm(residuals(lm.fit))
qqline(residuals(lm.fit), col = 2)

######predict load for test data
pr.lm <- predict(lm.fit,test)

MSE.lm <- sum((pr.lm - test$Load)^2)/nrow(test)
cor(pr.lm,test$Load)

####do some plotting
out<-bind_rows(tibble(ind=1:nrow(test),name="observed",v=test$Load),
               tibble(ind=1:nrow(test),name="lm",v=pr.lm)
               #               tibble(ind=1:nrow(test_),name="nw",v=unlist(pr.nn_[,1])),
               #tibble(ind=1:nrow(test_),name="nw.rsnns",v=unlist(pr.nn.rsnns_))
               )

out %>% ggplot(aes(v)) + stat_ecdf(aes(col=name))

out %>% ggplot(aes(x=ind,y=v)) + geom_line(aes(col=name))

out %>% spread(name,v) %>% gather(variable,val,-ind,-observed) %>% 
  ggplot(aes(x=observed,y=val)) + geom_point(aes(col=variable)) + facet_grid(.~variable)

out %>% ggplot()+geom_boxplot(aes(x=name,y=v))

out %>% spread(name,v) %>% gather(name,value,-ind,-observed) %>% 
  ggplot(aes(x=observed,y=value)) + geom_point(aes(col=name)) +
  facet_grid(.~name) + geom_abline(intercept=0,slope=1,col="black")

out %>% spread(name,v) %>% mutate(lm_diff=lm-observed) %>% 
  gather(variable,val,-ind) %>% 
  filter(variable %in% c("lm_diff","nw_diff","nw.rsnns_diff")) %>% ggplot(aes(x=ind,y=val)) + geom_line(aes(col=variable))

#####again some statistical measures on residuals
residuals<-residuals(lm.fit)
fitt<-fitdist(residuals,distr="norm")

mean(residuals)
var(residuals)
hist(rr,breaks=100)
rr<-rnorm(length(residuals),
          mean=mean(residuals),
          sd=sd(residuals))

hist(residuals,col="red",breaks=100)

plot(ecdf(residuals))
plot(ecdf(rr),add=TRUE,col="red")

plot(residuals,type="l")
lines(rr,col="red")


####simulation of load data

dat<-bind_cols(as_tibble(model.matrix(~.,data=load_[,4:7])[,2:28])
               ,t_dat[,2:ncol(t_dat)])

lags<-c(1:12,24,25)

mod<-lm.fit

tSim<-tibble(Date=load_$Date,load_$Load)
n<-10
for(i in 1:n){
  print(i)
  tSim<-bind_cols(tSim,tibble(simulateTS(lm.fit,lags,dat)))
}
names(tSim)<-c("Date","Load",paste("Sim",1:n))

tSim %>% gather(var,val,-Date) %>% mutate(ind=1:n()) %>% 
  group_by(Date=12*(year(Date)-2007)+month(Date),var) %>% 
  summarize(val=sum(val))%>% 
  ggplot(aes(x=Date,y=val)) + geom_line(aes(col=var))



########this function simulates, given a certain model, the load from temperature data
simulateTS<-function(mod,lags,dat){

  coefs<-summary(mod)$coefficients
  mod_coefs_data<-coefs[1:(ncol(dat)+1),1]
  mod_coefs_lags<-coefs[(ncol(dat)+2):nrow(coefs),1]
  
  residuals<-residuals(mod)
  n_lags<-max(lags)
  fixedPart<-mod_coefs_data[1]+
    apply((t(t(dat)*mod_coefs_data[2:length(mod_coefs_data)])),1,sum)+
    residuals[round(runif(nrow(dat),1,length(residuals)))]

  out<-c(1:nrow(dat))
  
  for(i in (n_lags+1):nrow(dat)){
    out[i]<-fixedPart[i]+sum(mod_coefs_lags*out[i-lags])
  }
    
  #errors<-rnorm(nrow(data),mean=mean(residuals(mod)),sd=sd(residuals(mod)))
   return(out)
  
}



##############HERE STARTS THE NEURAL NETWORK THING
scaled <- read_feather(loadFeather) %>% 
  filter(between(year(Date),2007,2015) & Region=="SE4") %>% 
  mutate(  ind=1:n(),
           seasonalC=cos(2*pi*ind/8760),
           seasonalS=sin(2*pi*ind/8760),
           hour=(hour(Date)),
           wday=wday(Date),
           delta1=c(load$Load[2:n()],0))

scaled<-bind_cols(scaled,t_dat[,2:ncol(t_dat)])

scaled<-scaled[,3:ncol(scaled)]
maxs <- apply(scaled, 2, max) 
mins <- apply(scaled, 2, min)

scaled <- as_tibble(scale(scaled, center = mins, scale = maxs - mins))

train_ <- scaled[index,]
test_ <- scaled[-index,]

library(RSNNS)
library(doParallel)

cl <- makeCluster(detectCores())
registerDoParallel(cl)
t1<-system.time(mod3<-mlp(train_[,2:ncol(train_)], train_[,1], size=10,maxit=1000,linOut=F))
stopCluster(cl)

pr.nn<-predict(mod3,test_[,2:ncol(test_)]) 

pr.nn.rsnns_ <- pr.nn[,1]*(max(data$Load)-min(data$Load))+min(data$Load)
test.r <- (test_$Load)*(max(data$Load)-min(data$Load))+min(data$Load)

MSE.nn.rsnns_ <- sum((pr.nn.rsnns_-test.r)^2)/nrow(test_)


#library(neuralnet)
#n <- names(train_)
#f <- as.formula(paste("Load ~", paste(n[!n %in% "Load"], collapse = " + ")))


#library(doParallel)
#cl <- makeCluster(detectCores())
#registerDoParallel(cl)

#t2<-system.time(nn <- neuralnet(f,
#                                data=train_,
#                                hidden=c(6,5,4,3,2),
#                                linear.output=F,
#                                stepmax=10^3,
#                                lifesign="full",
#                                threshold=5))
#stopCluster(cl)

#pr.nn <- compute(nn,test_[,2:ncol(test_)])

#pr.nn_ <- pr.nn$net.result*(max(data$Load)-min(data$Load))+min(data$Load)

#MSE.nn <- sum((pr.nn_-test.r)^2)/nrow(test_)
#we then compare the two MSEs

#print(paste(MSE.lm,MSE.nn,MSE.nn.rsnns_))
print(paste(MSE.lm,MSE.nn.rsnns_))
#cor(test.r,pr.nn_)
cor(test.r,pr.nn.rsnns_)

out<-bind_rows(tibble(ind=1:nrow(test),name="observed",v=test$Load),
               tibble(ind=1:nrow(test_),name="lm",v=pr.lm),
#               tibble(ind=1:nrow(test_),name="nw",v=unlist(pr.nn_[,1])),
               tibble(ind=1:nrow(test_),name="nw.rsnns",v=unlist(pr.nn.rsnns_)))

out %>% ggplot(aes(v)) + stat_ecdf(aes(col=name))

out %>% ggplot(aes(x=ind,y=v)) + geom_line(aes(col=name))

out %>% spread(name,v) %>% gather(variable,val,-ind,-observed) %>% 
  ggplot(aes(x=observed,y=val)) + geom_point(aes(col=variable)) + facet_grid(.~variable)

out %>% ggplot()+geom_boxplot(aes(x=name,y=v))

out %>% spread(name,v) %>% gather(name,value,-ind,-observed) %>% 
  ggplot(aes(x=observed,y=value)) + geom_point(aes(col=name)) +
  facet_grid(.~name) + geom_abline(intercept=0,slope=1,col="black")

out %>% spread(name,v) %>% mutate(lm_diff=lm-observed,nw_diff=nw-observed,nw.rsnns_diff=nw.rsnns-observed) %>% 
  gather(variable,val,-ind) %>% 
  filter(variable %in% c("lm_diff","nw_diff","nw.rsnns_diff")) %>% ggplot(aes(x=ind,y=val)) + geom_line(aes(col=variable))




ggplot() + geom_line(aes(x=ind,y=lm-observed),col="green") + 
  geom_line(aes(x=ind,y=nw-observed),col="red") 



