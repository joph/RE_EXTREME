####prepare run - 7 years, original capacities
period<-c("2007-01-01 00:00:00 CET"
          , "2013-12-31 23:00:00 CET")
prepareFullRun(period,
               out="../gms_execute/input_tr_7years_30euro.gdx",
               intermittentCSV="data/investOptions/intermittentOpts7years.csv",
               transmissionCSV="data/transmission/lineCapacities_original.csv")

####prepare run - 8 years, original capacities
period<-c("2007-01-01 00:00:00 CET"
          , "2014-12-31 23:00:00 CET")
prepareFullRun(period,
               out="../gms_execute/input_tr_8years.gdx",
               intermittentCSV="data/investOptions/intermittentOpts8years.csv",
               transmissionCSV="data/transmission/lineCapacities_original.csv")