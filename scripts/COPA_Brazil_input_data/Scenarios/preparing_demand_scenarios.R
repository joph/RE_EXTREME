# (17/11/17) (yy/mm/dd)
# This script compares 2012 and 2013 natural inflows (vazao afluente) in order to decide what demand use to demand projections. 
# We are using the greater year in terms of amount of water
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/ONS validation")
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA  Initial Data/ONS validation")

# comparing natural inflows 2012 and 2013
inflows <- read_delim("Comparativo_Dados_Hidrológicos_2012_2013.csv", delim = ",",locale =locale(decimal_mark = ".")) %>% 
  select(1,3,5,7)

colnames(inflows) <- c("date", "reservoir", "date1", "inflow")

inflows_date <- inflows %>% group_by(date) %>% summarise(month_sum = sum(inflow))

inflows_date <- inflows_date %>% mutate(dates = c(rep("", nrow(test))))

seq12 <- make_date(year = 2012, month = 1:12)
seq13 <- make_date(year = 2013, month = 1:12)
inflows_date$dates[c(1,3,5,7,9,11,13,15,17,19,21,23)] <- c(month(seq12[4]), month(seq12[8]), month(seq12[12]), month(seq12[2]), 
                                                month(seq12[1]), month(seq12[7]), month(seq12[6]), month(seq12[3]), 
                                                month(seq12[5]), month(seq12[11]), month(seq12[10]), month(seq12[9]))
inflows_date$dates[c(2,4,6,8,10,12,14,16,18,20,22,24)] <- c(month(seq13[4]), month(seq13[8]), month(seq13[12]), month(seq13[2]), 
                                                   month(seq13[1]), month(seq13[7]), month(seq13[6]), month(seq13[3]), 
                                                   month(seq13[5]), month(seq13[11]), month(seq13[10]), month(seq13[9]))
inflows_date$year <- c(rep("",24))
inflows_date$year[c(1,3,5,7,9,11,13,15,17,19,21,23)] <- c(rep(year(seq12),12))
inflows_date$year[c(2,4,6,8,10,12,14,16,18,20,22,24)] <- c(rep(year(seq13),12))

inflows_date$dates <- parse_double(inflows_date$dates)
inflows_date <- inflows_date %>% arrange(dates)


ggplot()+geom_bar(data = inflows_date, aes(x = dates, y = month_sum, fill = year), stat = "identity", position = "dodge") +
  ylim(0,max(inflows_date$month_sum)) + ylab("m3/s") + xlab("") +  ggtitle("Natural inflows 2012 x 2013") + 
  theme(plot.title = element_text(hjust = 0.5))

# sum and mean of each year
inflows_year <- inflows_date %>% group_by(year) %>% summarise(inflow = sum(month_sum), mean = mean(month_sum))
# 2013 has 10% more natural inflows than 2012. Therefore let's keep 2013 demand.

# new_demand --------------------------------------------------------------
#reading 2013 load
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA/data/load")
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA/data/load")
load_1214 <- read_feather("load_Br_2014.feather")

# selecting 2013
load_13 <- load_1214 %>% filter(Date >= "2013-01-01 00:00:00")

# multiplying by 1.5 (assumption) 
load_scenarios <- load_13 %>% mutate(load_scenarios = Load * 1.5)

load_scenarios <- load_scenarios %>% mutate(check = load_scenarios/Load)

load_scenarios <- load_scenarios %>% select(Date, Region, load_scenarios)
colnames(load_scenarios) <- c("Date", "Region", "Load")

#comparing to PDE 2026 projections to verify what year this demand looks like

#sum of projected load
sum_load_scenarios <- load_scenarios %>% summarise(total_load = sum(Load)/8760) # MWmed

#loading PDE 2026 load projections
setwd("C:/Users/cancella/Google Drive/!Tese/Informações setor elétrico/PDE 2026")
PDE <- read.xlsx("Energia_Eletrica_Cap3_Dados.xlsx", 1)
PDE <- PDE[2:11,]
colnames(PDE) <- c("Date", "MWmed", "maxDem")
PDE$MWmed <- parse_double(PDE$MWmed)
PDE <- as_tibble(PDE)

# our projection (150% 2013 demand comes to 2025 PDE projections)
PDE$MWmed[PDE$Date == "2025"] / sum_load_scenarios$total_load 

# saving load projections to use during the 1 year runs
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA/data/load")
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA/data/load")
write_feather(load_scenarios, "load_1year_projected.feather")


