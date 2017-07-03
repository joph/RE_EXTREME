###########################################################################################
######################################FUNCTIONS############################################
###########################################################################################

####function to download data from the merra2 dataset
####lon / lat: center of box to download from. data are downloaded from box lon-0.5,lat-0.625,lon+0.5,lat+0.625
####period: period of interest
####params: the respective parameters to be downloaded
####user and pass: username and password for the downloadserver
####get one at: https://urs.earthdata.nasa.gov/home
getMERRAData<-function(i,lons_lats,period,params,user,pass){
  lon<-lons_lats[i,]$long
  lat<-lons_lats[i,]$lat
  original<-getwd()
  dir.create(file.path(getwd(), paste("../MERRA/LonLat",lon,lat,sep="")), showWarnings = FALSE)
  setwd(paste("../MERRA/LonLat",lon,lat,sep=""))
  
  dates<-period %>% format(format="%Y%m%d")
  year<-period %>% format(format="%Y") %>% as.numeric()
  month<-period %>% format(format="%m")
  
  variables<-sapply(params,paste,"%2C",sep="") %>% 
    paste(collapse='') %>% 
    substr(1,nchar(.)-3)
  
  boxes<-c(lon-0.5,lat-0.625,lon+0.5,lat+0.625) %>% 
    sapply(paste,"%2C",sep="") %>% 
    paste(collapse='') %>% 
    substr(1,nchar(.)-3)
  
  fileNamesIn<-rep("",length(period))
  fileNamesOut<-rep("",length(period))
  
  print(paste("downloading:",lon,lat))
  
  for(i in 1:length(period)){
    
    fileName<-paste("MERRA_",lon,"_",lat,"_",variables,"_",dates[i],".nc4",sep="")
    
    fileNamesOut[i]<-fileName
  
      
      fV<-100
      if(between(year[i],1992,2000)){
        fV<-200
      }
      if(between(year[i],2001,2010)){
        fV<-300
      }
      if(between(year[i],2011,2020)){
        fV<-400
      }
      
      
      
      #url<-paste("http://goldsmr4.gesdisc.eosdis.nasa.gov/daac-bin/OTF/HTTP_services.cgi?FILENAME=%2Fdata%2FMERRA2%2FM2T1NXRAD.5.12.4%2F",
      #           year[i],"%2F",
      #           month[i],"%2FMERRA2_",fV,".tavg1_2d_rad_Nx.",
      #           dates[i],".nc4&FORMAT=bmM0Lw&BBOX=",boxes,
      #           "&LABEL=MERRA2_",fV,".tavg1_2d_rad_Nx.",dates[i],
      #           ".SUB.nc4&SHORTNAME=M2T1NXRAD&SERVICE=SUBSET_MERRA2&VERSION=1.02&LAYERS=&VARIABLES=",variables,
      #           sep="")
      
      url<-paste("http://goldsmr4.gesdisc.eosdis.nasa.gov/daac-bin/OTF/HTTP_services.cgi?FILENAME=%2Fdata%2FMERRA2%2FM2I1NXASM.5.12.4%2F",
                 year[i],"%2F",
                 month[i],"%2FMERRA2_",fV,".inst1_2d_asm_Nx.",
                 dates[i],".nc4&FORMAT=bmM0Lw&BBOX=",boxes,
                 "&LABEL=MERRA2_",fV,".inst1_2d_asm_Nx.",dates[i],
               ".SUB.nc4&SHORTNAME=M2I1NXASM&SERVICE=SUBSET_MERRA2&VERSION=1.02&LAYERS=&VARIABLES=",variables,
                 sep="")
      
      
      fileNamesIn[i]<-url
 
  }
  #print(fileNamesIn)
  #return()
  
  no_cores <- detectCores() - 1
  
  # Initiate cluster
  cl <- makeCluster(no_cores)
  clusterEvalQ(cl, library("httr"))
  clusterEvalQ(cl, sink(paste0("d:/temp/output", Sys.getpid(), ".txt")))
    
  files<-data.frame(fileNamesIn,fileNamesOut)
  parSapply(cl,1:length(fileNamesIn),getFileMerraFinal,fileNamesIn, fileNamesOut,user,pass)

  stopCluster(cl)
  setwd(original)
  
}

getFileMerraFinal<-function(i,fileNameIn,fileNameOut,user,pass){
  print(fileNameIn[i])
  print(fileNameOut[i])
    if(!file.exists(fileNameOut[i])){
    print(GET(fileNameIn[i],
              authenticate(user,pass),
              write_disk(fileNameOut[i],overwrite=TRUE)))
  }else{
    print("File exists already")
  }
  
}

####function to download data from the merra2 dataset
####lon / lat: center of box to download from. data are downloaded from box lon-0.5,lat-0.625,lon+0.5,lat+0.625
####period: period of interest
####params: the respective parameters to be downloaded
####user and pass: username and password for the downloadserver
####get one at: https://urs.earthdata.nasa.gov/home
getMERRADataBox<-function(lon1,lat1,lon2,lat2,period,params,user,pass,runParallel=TRUE){
  original<-getwd()
  dir<-paste("../MERRA/LonLat",lon1,"_",lat1,"_",lon2,"_",lat2,sep="")
  dir.create(file.path(getwd(), dir), showWarnings = FALSE)
  setwd(dir)
  
  dates<-period %>% format(format="%Y%m%d")
  year<-period %>% format(format="%Y") %>% as.numeric()
  month<-period %>% format(format="%m")
  
  variables<-sapply(params,paste,"%2C",sep="") %>% 
    paste(collapse='') %>% 
    substr(1,nchar(.)-3)
  
  boxes<-c(lat1,lon1,lat2,lon2) %>% 
    sapply(paste,"%2C",sep="") %>% 
    paste(collapse='') %>% 
    substr(1,nchar(.)-3)
  
  fileNamesIn<-rep("",length(period))
  fileNamesOut<-rep("",length(period))
  
  print(paste("downloading:",lon1,lat1,lon2,lat2))
  
  for(i in 1:length(period)){
    
    fileName<-paste("MERRA_",lon1,"_",lat1,"_",lon2,"_",lat2,"_",variables,"_",dates[i],".nc4",sep="")
    
    fileNamesOut[i]<-fileName
    
    
    fV<-100
    if(between(year[i],1992,2000)){
      fV<-200
    }
    if(between(year[i],2001,2010)){
      fV<-300
    }
    if(between(year[i],2011,2020)){
      fV<-400
    }
    
    
    
    #url<-paste("http://goldsmr4.gesdisc.eosdis.nasa.gov/daac-bin/OTF/HTTP_services.cgi?FILENAME=%2Fdata%2FMERRA2%2FM2T1NXRAD.5.12.4%2F",
    #           year[i],"%2F",
    #           month[i],"%2FMERRA2_",fV,".tavg1_2d_rad_Nx.",
    #           dates[i],".nc4&FORMAT=bmM0Lw&BBOX=",boxes,
    #           "&LABEL=MERRA2_",fV,".tavg1_2d_rad_Nx.",dates[i],
    #           ".SUB.nc4&SHORTNAME=M2T1NXRAD&SERVICE=SUBSET_MERRA2&VERSION=1.02&LAYERS=&VARIABLES=",variables,
    #           sep="")
    
    url<-paste("http://goldsmr4.gesdisc.eosdis.nasa.gov/daac-bin/OTF/HTTP_services.cgi?FILENAME=%2Fdata%2FMERRA2%2FM2I1NXASM.5.12.4%2F",
               year[i],"%2F",
               month[i],"%2FMERRA2_",fV,".inst1_2d_asm_Nx.",
               dates[i],".nc4&FORMAT=bmM0Lw&BBOX=",boxes,
               "&LABEL=MERRA2_",fV,".inst1_2d_asm_Nx.",dates[i],
               ".SUB.nc4&SHORTNAME=M2I1NXASM&SERVICE=SUBSET_MERRA2&VERSION=1.02&LAYERS=&VARIABLES=",variables,
               sep="")
    
    
    fileNamesIn[i]<-url
    
  }
  #print(fileNamesIn)
  #return()
  if(runParallel){
  
  no_cores <- detectCores() - 1
  
  # Initiate cluster
  cl <- makeCluster(no_cores)
  clusterEvalQ(cl, library("httr"))
  clusterEvalQ(cl, sink(paste0("d:/temp/output", Sys.getpid(), ".txt")))
  
  
  files<-data.frame(fileNamesIn,fileNamesOut)
  parSapply(cl,1:length(fileNamesIn),getFileMerraFinal,fileNamesIn, fileNamesOut,user,pass)
  #sapply(1:length(fileNamesIn),getFileMerraFinal,fileNamesIn, fileNamesOut,user,pass)
  stopCluster(cl)
  }else{
    sapply(1:length(fileNamesIn),getFileMerraFinal,fileNamesIn, fileNamesOut,user,pass)
    
    
  }

  setwd(original)
  
}

####reads merra file from disk
####interpolates the data from the neighbouring 4 points with 
####respect to the point given in lon lat
####ncname is the name of the MERRA file
####pname is the name of the parameter to read
readParamMerra <- function(ncname,pname,lon,lat) {
  
  ncfile <- nc_open(ncname)
  
  #Longitude
  longitude <- ncvar_get(ncfile, "lon", verbose = F)
  nlon <- dim(longitude)
  
  #Latitude
  latitude <- ncvar_get(ncfile, "lat", verbose = F)
  nlat <- dim(latitude)
  
  #Time
  time <- ncvar_get(ncfile, "time")
  tunits <- ncatt_get(ncfile, "time", "units")
  ntime <- dim(time)
  
  #read the variable
  m.array <- ncvar_get(ncfile, pname)
  
  #Dataframe
  m.vec.long <- as.vector(m.array)
  m.mat <- matrix(m.vec.long, nrow = nlon * nlat, ncol = ntime)
  lonlat <- expand.grid(longitude,latitude)
  dista<-sqrt(abs((lonlat[,2]-lat)^2+(lonlat[,1]-lon)^2))
  dfm <- (data.frame(cbind(dista,lonlat, m.mat)))
  dfm <- dfm[ order(dfm[,1]), ]
  dfm$weight<-(max(dfm$dista)-dfm$dista)/sum(max(dfm$dista)-dfm$dista)
  fdg<-dfm$weight*dfm[,4:27] 
  fdg<-apply(fdg,2,sum)
  nc_close(ncfile)
  return(fdg)
}

####reads merra file from disk
####creates file of lons lats
####saves all points into single feather files
####joining the respective time-period together
convertMerraFeather<- function(lon1,lat1,lon2,lat2,pname,pname1,date_seq) {

  ncname_head<-paste("../MERRA/LonLat",lon1,"_",lat1,"_",lon2,"_",lat2,"/MERRA_",lon1,"_",lat1,"_",lon2,"_",lat2,"_",pname1,"_",sep="")
  
    
  dates<-format(date_seq,"%Y%m%d")
  ncfiles<-paste(ncname_head,dates,".nc4",sep="")
  ncfile <- nc_open(ncfiles[1])
  
  
  #Longitude
  longitude <- ncvar_get(ncfile, "lon", verbose = F)
  nlon <- dim(longitude)
  
  #Latitude
  latitude <- ncvar_get(ncfile, "lat", verbose = F)
  nlat <- dim(latitude)
  
  lonlat <- expand.grid(longitude,latitude)
  nc_close(ncfile)
  
  res<-sapply(ncfiles,readSingleParam,"T2M",simplify=FALSE)
  df<-bind_rows(res)
  
  ###write merra points for whole period
  dir<-("../MERRA/feather")
  dir.create(file.path(getwd(), dir), showWarnings = FALSE)
  dir<-paste("../MERRA/feather/LonLat",pname,lon1,lat1,lon2,lat2,
             format(date_seq[1],"%Y%m%d"),
             format(date_seq[length(date_seq)],"%Y%m%d"),sep="_")
  dir.create(file.path(getwd(), dir), showWarnings = FALSE)

  ###write files
  for(i in 1:ncol(df)){
    outfile<-paste(dir,"/",lonlat[i,1],"_",lonlat[i,2],".feather",sep="")
    write_feather(df[,i],outfile)
  }

  write_feather(lonlat,paste(dir,"/lonlat.feather",sep=""))
}

######read a single parameter from a single MERRA2-ncfile
readSingleParam<-function(ncfileIn,pname){
  print(ncfileIn)
  ncfile<-nc_open(ncfileIn)
  #Time
  time <- ncvar_get(ncfile, "time")
  tunits <- ncatt_get(ncfile, "time", "units")
  ntime <- dim(time)
  m.array <- ncvar_get(ncfile, pname)
  m.vec.long <- as.vector(m.array)
  m.mat <- t(matrix(m.vec.long, nrow = nlon * nlat, ncol = ntime))
  nc_close(ncfile)
  return(as_tibble(m.mat))
}


#####interpolate data point from feather data
#####pass parameter name, date sequence and latitude and longitude of point to interpolate from
#####also the boundary box from which the feather data is read has to be passed to the file
interpolatePoint<-function(pname,date_seq,lon,lat,lon1,lat1,lon2,lat2){
  dir<-paste("../MERRA/feather/LonLat",pname,lon1,lat1,lon2,lat2,
             format(date_seq[1],"%Y%m%d"),
             format(date_seq[length(date_seq)],"%Y%m%d"),sep="_")
  lonlat<-read_feather(paste(dir,"/lonlat.feather",sep=""))
  dista<-sqrt(abs((lonlat[,2]-lat)^2+(lonlat[,1]-lon)^2))
  dfm <- (data.frame(cbind(dista,lonlat)))
  dfm <- dfm[ order(dfm[,1]), ]
  dfm<-dfm[1:4,]
  names(dfm)<-c("dista","lon","lat")
  dfm$invDista<-abs(70-dfm$dista)
  dfm$weight<-dfm$invDista/sum(dfm$invDista)
  files<-paste(dir,"/",dfm[1:4,]$lon,"_",dfm[1:4,]$lat,".feather",sep="")
  
  out<-NULL
  for(i in files){
   t<-read_feather(i)
   out<-bind_cols(out,t)
  }
  
  fdg<-t(dfm$weight*t(out))
  fdg<-apply(fdg,1,sum)
}



