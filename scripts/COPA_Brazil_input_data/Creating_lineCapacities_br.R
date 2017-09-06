# This script creates the lineCapacities with brazilian data -> Newave deck of May 2017. 
# For that, it reads the "system.DAT" from Newave deck and transformes the data 
# in the format that COPA reads. 
# assumption: I choose to use the data from 2018 because it is the  first year that have data for all months of the year

setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/Costs/deck_newave_2017_05")
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA  Initial Data/Costs/deck_newave_2017_05")
system <- as_tibble(read.csv2("transmission - draft.csv", header = T, sep = ""))

##### Preparing data ######
colnames(system, do.NULL = TRUE, prefix = "col")
colnames(system) <- c("year", "jan", "feb", "mar", "apr", "may", 
                       "jun", "jul", "aug", "sep", "oct", "nov", "dec", "dec2")

# Selecting december of each year and information of from and to energy is flowing 
system <- system[,1:13] %>% select(year, jan, dec)

id <- as_tibble(c(1:12))
colnames(id, do.NULL = TRUE, prefix = "col")
colnames(id) <- c("id")

Region1 <- as_tibble(paste("BR",c(1, 2, 1, 11, 3, 11, 4, 11, 1, 3, 1, 4 ), sep = ""))
colnames(Region1, do.NULL = TRUE, prefix = "Region1")
colnames(Region1) <- c("Region1")

Region2 <- as_tibble(paste("BR", c(2, 1, 11, 1, 11, 3, 11, 4, 3, 1, 4, 1), sep = ""))
colnames(Region2, do.NULL = TRUE, prefix = "Region2")
colnames(Region2) <- c("Region2")

Max_dez <- filter(system, year == 2017)

system_br <- bind_cols(id, Region1, Region2) %>% mutate(Max = Max_dez$dec[1:12])

# Ploting the points
ggplot(data=system_br) + geom_point(mapping=aes(x= Region1,y = Max))

##### Changing the names from "BR" to "SE" for the time being
system_br$Region1[system_br$Region1 == "BR1"] <- "SE1"
system_br$Region1[system_br$Region1 == "BR2"] <- "SE2"
system_br$Region1[system_br$Region1 == "BR3"] <- "SE3"
system_br$Region1[system_br$Region1 == "BR4"] <- "SE4"
system_br$Region1[system_br$Region1 == "BR11"] <- "SE5"
system_br$Region2[system_br$Region2 == "BR1"] <- "SE1"
system_br$Region2[system_br$Region2 == "BR2"] <- "SE2"
system_br$Region2[system_br$Region2 == "BR3"] <- "SE3"
system_br$Region2[system_br$Region2 == "BR4"] <- "SE4"
system_br$Region2[system_br$Region2 == "BR11"] <- "SE5"

#setwd("C:/Users/cancella/Google Drive/!IIASA/COPA/data/transmission")
#write.table(system_br, file = "linesCapacities_br_1.csv", sep = ";", row.names = FALSE )
#####

# Saving the invest_opts_br in a .csv file 
#setwd("C:/Users/cancella/Google Drive/!IIASA/COPA/data/transmission")
#save_invest_opts_br <- write.table(system_br, file = "linesCapacities_br.csv", sep = ";", row.names = FALSE )

# The file is ready! Let's analyze it and compare with the Swedish file =)
linesCapacities_se <- as_tibble(read.csv2("lineCapacities - original.csv", header = T, sep = ";"))
se <- as_tibble(c(rep(30000,12)))
colnames(se, do.NULL = TRUE, prefix = "Max")
colnames(se) <- c("Max")
ggplot(data=system_br, aes(x= Region1,y = Max)) + 
  geom_point(aes(color = "BR")) + 
  geom_point(data = se , aes(color = "SE"))
