#### Creating the structure of system_2018 ####
id <- as_tibble(c(1:12))
colnames(id, do.NULL = TRUE, prefix = "col")
colnames(id) <- c("id")

Region1_18 <- as_tibble(paste("BR",c(1, 2, 1, 11, 3, 11, 4, 11, 1, 3, 1, 4 ), sep = ""))
colnames(Region1, do.NULL = TRUE, prefix = "Region1")
colnames(Region1_18) <- c("Region1")

Region2_18 <- as_tibble(paste("BR", c(2, 1, 11, 1, 11, 3, 11, 4, 3, 1, 4, 1), sep = ""))
colnames(Region2, do.NULL = TRUE, prefix = "Region2")
colnames(Region2_18) <- c("Region2")

# preparing system for loop -> separating the year of 2018 and adding the regions of the energy flow. 
system_2018 <- system %>% filter(year == "2018") %>% 
  bind_cols(Region1_18, Region2_18)

# Defining nodes: 14, 41, 34 and 43. 
BR1_BR4 <- sum(system_2018[11,2:13]) + sum(system_2018[3,2:13])  # 14 = 1 to 4 + 1 to 11
BR4_BR1 <- sum(system_2018[4,2:13]) + sum(system_2018[12,2:13])  # 41 = 11 to 1 + 4 to 1
BR3_BR4 <- sum(system_2018[5,2:13]) + sum(system_2018[8,2:13])   # 34 = 3 to 11 + 11 to 4
BR4_BR3 <- sum(system_2018[7,2:13]) + sum(system_2018[6,2:13])   # 43 = 4 to 11 + 11 to 3
new_nodes <- as_tibble(c(BR1_BR4, BR4_BR1, BR3_BR4, BR4_BR3))


# Preparing the structure for lineCapacities_br
id <- as_tibble(c(1:8))
colnames(id, do.NULL = TRUE, prefix = "col")
colnames(id) <- c("id")

Region1 <- as_tibble(paste("BR",c(1, 2, 1, 3, 1, 4, 3, 4 ), sep = ""))
colnames(Region1, do.NULL = TRUE, prefix = "Region1")
colnames(Region1) <- c("Region1")

Region2 <- as_tibble(paste("BR", c(2, 1, 3, 1, 4, 1, 4, 3), sep = ""))
colnames(Region2, do.NULL = TRUE, prefix = "Region2")
colnames(Region2) <- c("Region2")

max <-as_tibble(c(rep("",8))) 
colnames(max, do.NULL = TRUE, prefix = "Max")
colnames(max) <- c("Max")

linesCapacities_br <- bind_cols(id, Region1, Region2, max) 
linesCapacities_br_loop <- linesCapacities_br


# Adding the data in the lineCapacities
for (i in c(1:2)){
  linesCapacities_br[i,4] <- sum(system_2018[i,2:13])  
}

linesCapacities_br[3,4] <- sum(system_2018[9,2:13])
linesCapacities_br[4,4] <- sum(system_2018[10,2:13])
linesCapacities_br[5:8,4] <- new_nodes$value

# Transforming the Max column in double
linesCapacities_br_initial <-  linesCapacities_br
linesCapacities_br_initial[sapply(linesCapacities_br_initial, is.character)] <- lapply(linesCapacities_br_initial[sapply(linesCapacities_br_initial, is.character)], 
                                                                                       function(x) as.numeric(as.character(x)))
linesCapacities_br[,4] <- linesCapacities_br_initial[,4]

# Including transmission losses (3% in accordance to IEA (2014))
linesCapacities_br_net <- linesCapacities_br
linesCapacities_br_net[,4] <- linesCapacities_br[,4] * 0.97

