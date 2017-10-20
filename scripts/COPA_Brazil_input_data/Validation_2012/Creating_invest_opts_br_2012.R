# This script reads the tables that result of reading Newave deck scripts and 
# put the data in the format that COPA reads, creating the file "investOpts_br_2012.csv". 

# Setting the working directories 
  # Reading the investOpts.csv original. That`s the result we intend to achieve.
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA/data/investOptions")
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA/data/investOptions")

invest_opts_sweden <- as_tibble(read.csv2("investOpts - original.csv", header = T, sep =";"))

# Reading the tables with brazilian information of investiment costs, variable costs and max capacities of the thermal plants.
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA  Initial Data/Costs/Tables/validation_2012")
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/Costs/Tables/validation_2012")
br_cvu <-  read_feather("tabela_para_invest_opts_br_sem_CI_2012.feather")
br_cvu <- as_tibble(br_cvu)
# br_ci <- read.csv2("tabela_para_invest_opts_br_com_CI.csv", header = T, sep =";", stringsAsFactors = F)
# br_ci <- as_tibble(br_ci)

# Plotting the merit curve   
br_cvu %>% group_by(Subsistema) %>% arrange(CVU) %>% mutate(potsum=cumsum(pot_disp)) %>% 
    select(potsum,CVU,Subsistema) %>% ggplot(aes(potsum,CVU)) +geom_line()+ facet_wrap(~Subsistema)
  
# adjusting the order of the columns
invest_opts_br <- select(br_cvu, NUM, Subsistema, NOME, TIPO_COMB., Situacao:ind)

# We need 3 lines for each plant. So we need 303 lines (101 plants)
id_br <- as_tibble(c(1:312)) # continue from here :)
regions_br <- as_tibble(rep(c(rep("BR1",3),rep("BR2",3),rep("BR3",3),rep("BR4",3)),26))
invest_opts_br <- as_tibble(cbind(id_br$value,regions_br$value))
# Setting the columns names 
colnames(invest_opts_br, do.NULL = TRUE, prefix = "col")
colnames(invest_opts_br) <- c("id", "Region")

# Reading the archieve containing the numbers of plants in each subsystem
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA  Initial Data/Costs/Tables/validation_2012")
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/Costs/Tables/validation_2012")
plants_ss <- read_feather("usinas_agrupadas_por_subsistema.feather")
  
# Making a correspondence between name of plant, its subsystem and the names that COPA reads. 
corresp <- transmute(br_cvu, nome = NOME, sub = Subsistema) %>% group_by(sub) %>% mutate(P = "")

# Adding the names that COPA reads in each subsystem -> subsystem 1
for (i in c(1:plants_ss$n[1])){
  corresp$P[i] = paste("P",i, sep = "")
}

# subsystem 2
num_plants_sub2 <- c(1:plants_ss$n[2])
for (i in c(1:plants_ss$n[2]) + 36){
  corresp$P[i] = paste("P",num_plants_sub2[i - 36], sep = "")
}

# subsystem 3
num_plants_sub3 <- c(1:plants_ss$n[3])
for (i in c(1:plants_ss$n[3]) + 52){
  corresp$P[i] = paste("P",num_plants_sub3[i - 52], sep = "")
}

# subsystem 4
num_plants_sub4 <- c(1:plants_ss$n[4])
for (i in c(1:plants_ss$n[4]) + 89){
  corresp$P[i] = paste("P",num_plants_sub3[i - 89], sep = "")
}

# Adding a column with the respective subsystem of each plant in the COPA format
subsystems <- as_tibble(c(rep("BR1",plants_ss$n[1]),rep("BR2", plants_ss$n[2]), 
                          rep("BR3",plants_ss$n[3]), rep("BR4",plants_ss$n[4])))
corresp_sub <- bind_cols(corresp, subsystems)
corresp <- corresp_sub
corresp[,3] <- corresp_sub[,4]
corresp[,4] <- corresp_sub[,3]
colnames(corresp, do.NULL = TRUE, prefix = "col")
colnames(corresp) <- c("nome", "sub", "Region", "P")

# Expanding the table to each plant get three entries
exp_corresp <- bind_rows(corresp, corresp, corresp)

# saving correspondence table 
# setwd("C:/Users/cancella/Google Drive")
# corresp <- write_feather(corresp_sub, "corresp.feather")
# Arranging by names in order to get the three entries
exp_corresp <- arrange(exp_corresp, nome)

# I guess I'll mantain the alfabethic order and I'll see if COPA reads in this way. 

#### Building the column technology ####
# Transforming the initial tables in tables of 3 entries
#br_ci_3 <- bind_rows(br_ci, br_ci, br_ci) %>% arrange(NOME)
br_cvu_3 <- bind_rows(br_cvu, br_cvu, br_cvu) %>% arrange(NOME)

# Adding the tecnology column. It'll be different of the swedish model, because it'll difer the thermal technology

tech_vetor <- transmute(br_cvu_3, Technology = paste("Thermal", TIPO_COMB., sep = "."))

exp_corresp <- bind_cols(exp_corresp,tech_vetor)

#### Adding the column "variable"
variable <- as_tibble(rep(c("Investment", "VarCost", "MaxCap"),nrow(exp_corresp)/3))
exp_corresp <- bind_cols(exp_corresp, variable)
colnames(exp_corresp, do.NULL = TRUE, prefix = "col")
colnames(exp_corresp) <- c("nome", "sub", "Region", "P", "Technology", "Variable")


#### Adding values of InvestCost, VarCost and MaxCap

# Adding an empty column
exp_corresp <- mutate(exp_corresp, Value = "")


# Adding VarCosts
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA  Initial Data/Costs/Tables/validation_2012")
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/Costs/Tables/validation_2012")
test_br_cvu <- read_feather("tabela_para_invest_opts_br_sem_CI_2012.feather")
read.csv2("tabela_para_invest_opts_br_sem_CI.csv", header = T, sep = ";", stringsAsFactors = F)
br_cvu_3_test <- bind_rows(br_cvu, br_cvu, br_cvu) %>% arrange(NOME)

br_cvu_3_test <- bind_cols(br_cvu_3, exp_corresp[,6])
exp_corresp1 <- exp_corresp
exp_corresp1$Value[which(exp_corresp1$Variable == "VarCost")] <- br_cvu_3_test$CVU[which(br_cvu_3_test$Variable == "VarCost")] 

#### Adding available capacity
exp_corresp1$Value[which(exp_corresp1$Variable == "MaxCap")] <- br_cvu_3_test$pot_disp[which(br_cvu_3_test$Variable == "MaxCap")] 

#### Adding investment costs = 0 
exp_corresp1$Value[exp_corresp1$Variable == "Investment"] <- 0

##### Including the last plants ####
# It is advisible to include one more plant in each region with 10.000 MW of capacity and with high variable cost. 
last_plants <- invest_opts_sweden[67:nrow(invest_opts_sweden),]

# Changing the id
last_plants[,1] <- as_tibble(c(367:378))

# Changing the regions
last_plants[,2] <- regions_br[1:12,1]

# Changing the plant numbers
last_plants$P <- rep("P99", nrow(last_plants))

# Changing the tecnology column from "Thermal" to "Thermal.Virtual"
last_plants$Technology <- rep("Thermal.Virtual", nrow(last_plants))

###### Creating the invest_options_br ######
id_br_303 <- id_br[1:303,]
invest_opts_br_initial <- bind_cols(id_br_303, exp_corresp1[,3:ncol(exp_corresp)])
colnames(invest_opts_br_initial, do.NULL = TRUE, prefix = "col")
colnames(invest_opts_br_initial) <- c("id", "Region", "P", "Technology", "Variable", "Value")

# converting the column value into integer
value_to_int <- invest_opts_br_initial
value_to_int[sapply(value_to_int, is.character)] <- lapply(value_to_int[sapply(value_to_int, is.character)], 
                                                 function(x) as.numeric(as.character(x)))

invest_opts_br_final <- invest_opts_br_initial
invest_opts_br_final[,6] <- value_to_int[,6]

invest_opts_br <- bind_rows(invest_opts_br_final, last_plants)

# tt<-invest_opts_br %>% group_by(Technology,Variable) %>% summarize(s=sum(Value)) # adjusting order of plants

invest_opts_br <- invest_opts_br %>% arrange(Region,P,Technology)

# Transforming the values of variable costs in millions of R$/MWh. They are in R$/MWh
invest_opts_br$Value[invest_opts_br$Variable == "VarCost"] <- invest_opts_br$Value[invest_opts_br$Variable == "VarCost"] / 1e6


# Saving the invest_opts_br in a .csv file 
#setwd("C:/Users/cancella/Google Drive/!IIASA/COPA/data/investOptions")
#save_invest_opts_br <- write.table(invest_opts_br, file = "investOpts_br_thermal.sources_2012.csv", sep = ";", row.names = FALSE )

##### For the time being, changing the names of regions from "BR" to "SE"
invest_opts_br$Region[invest_opts_br$Region == "BR1"] <- "SE1"
invest_opts_br$Region[invest_opts_br$Region == "BR2"] <- "SE2"
invest_opts_br$Region[invest_opts_br$Region == "BR3"] <- "SE3"
invest_opts_br$Region[invest_opts_br$Region == "BR4"] <- "SE4"
#Technology column = Thermal
invest_opts_br$Technology <- "Thermal"

#setwd("C:/Users/cancella/Google Drive/!IIASA/COPA/data/investOptions")
#save_invest_opts_br <- write.table(invest_opts_br, file = "investOpts_br_thermal.sources_1_2012.csv", sep = ";", row.names = FALSE )
#####


# Plotting
thermal_var_costs<- as_tibble(filter(invest_opts_br, Variable == "VarCost")) 
colors <- c("#CC3333","#FF9933","#336600","#003366","66600","#00CC99", "#6600FF", "#3399FF", "#660000")
ggplot() +
  geom_bar(data = thermal_var_costs, aes(x =P, y = Value, fill = Technology), stat = "identity") + 
  facet_wrap(~Region) + scale_fill_manual (values = colors) +
  theme(axis.text.x = element_text(angle=45)) +
  ylab("cost (R$/MWh)") + xlab("plants") + ggtitle("Variable costs" ) + theme(plot.title = element_text(hjust = 0.5))

mean_per_region <- thermal_var_costs %>% group_by(Region, Technology) %>% summarise(avg = mean(Value))
colors <- c("#CC3333","#FF9933","#336600","#003366","66600","#00CC99", "#6600FF", "#3399FF", "#660000")
ggplot() +
  geom_bar(data = mean_per_region, aes(x =Technology, y = avg, fill = Technology), stat = "identity") + 
  facet_wrap(~Region) + scale_fill_manual (values = colors) +
  theme(axis.text.x = element_text(angle=45)) +
  ylab("cost (R$/MW)") + xlab("plants") + ggtitle("Variable costs" ) + theme(plot.title = element_text(hjust = 0.5))