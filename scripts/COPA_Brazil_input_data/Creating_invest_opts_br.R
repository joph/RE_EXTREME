# This script reads the tables that result of reading Newave deck scripts and 
# put the data in the format that COPA reads, creating the file "investOpts_br.csv". 

# Setting the working directories 
  # Reading the investOpts.csv original. That`s the result we intend to achieve.
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA/data/investOptions")
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA/data/investOptions")

invest_opts_sweden <- read.csv2("investOpts - original.csv", header = T, sep =";")
invest_opts_sweden <- as_tibble(invest_opts_sweden)

# Reading the tables with brazilian information of investiment costs, variable costs and max capacities of the thermal plants.
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA  Initial Data/Costs/Tables")
 setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/Costs/Tables")
  br_cvu <-  read.csv2("tabela_para_invest_opts_br_sem_CI.csv", header = T, sep =";")
  br_cvu <- as_tibble(br_cvu)
  br_ci <- read.csv2("tabela_para_invest_opts_br_com_CI.csv", header = T, sep =";", stringsAsFactors = F)
  br_ci <- as_tibble(br_ci)

# adjusting the order of the columns
invest_opts_br <- select(br_ci, NUM, Subsistema, NOME, TIPO_COMB.)

# We need 3 lines for each plant. So we need 366 lines (122 plants)
id_br <- as_tibble(c(1:372))
regions_br <- as_tibble(rep(c(rep("BR1",3),rep("BR2",3),rep("BR3",3),rep("BR4",3)),31))
invest_opts_br <- cbind(id_br,regions_br)
# Setting the columns names 
colnames(invest_opts_br, do.NULL = TRUE, prefix = "col")
colnames(invest_opts_br) <- c("id", "Region")

# Reading the archieve containing the numbers of plants in each subsystem
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA  Initial Data/Costs/Tables")
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/Costs/Tables")
plants_ss <- read.csv2("usinas_em_cada_subsistema.csv", header = T, sep = ";")

# Making a correspondence between name of plant, its subsystem and the names that COPA reads. 

corresp <- transmute(br_ci, nome = NOME, sub = Subsistema) %>% group_by(sub) %>% mutate(P = "")

# Adding the names that COPA reads in each subsystem -> subsystem 1
for (i in c(1:plants_ss$n[1])){
  corresp$P[i] = paste("P",i, sep = "")
}

# subsystem 2
num_plants_sub2 <- c(1:plants_ss$n[2])
for (i in c(1:plants_ss$n[2]) + 46){
  corresp$P[i] = paste("P",num_plants_sub2[i - 46], sep = "")
}

# subsystem 3
num_plants_sub3 <- c(1:plants_ss$n[3])
for (i in c(1:plants_ss$n[3]) + 62){
  corresp$P[i] = paste("P",num_plants_sub3[i - 62], sep = "")
}

# subsystem 4
num_plants_sub4 <- c(1:plants_ss$n[4])
for (i in c(1:plants_ss$n[4]) + 103){
  corresp$P[i] = paste("P",num_plants_sub3[i - 103], sep = "")
}

# Adding a column with the respective subsystem of each plant in the COPA format
subsystems <- as_tibble(c(rep("BR1",46),rep("BR2", 16), rep("BR3",41), rep("BR4",19)))
corresp_sub <- bind_cols(corresp, subsystems)
corresp <- corresp_sub
corresp[,3] <- corresp_sub[,4]
corresp[,4] <- corresp_sub[,3]
colnames(corresp, do.NULL = TRUE, prefix = "col")
colnames(corresp) <- c("nome", "sub", "Region", "P")


# Expanding the table to each plant get three entries
exp_corresp <- bind_rows(corresp, corresp, corresp)

# Arranging by names in order to get the three entries
exp_corresp <- arrange(exp_corresp, nome)


# I guess I'll mantain the alfabethic order and I'll see if COPA reads in this way. 

#### Building the column technology ####
# Transforming the initial tables in tables of 3 entries
br_ci_3 <- bind_rows(br_ci, br_ci, br_ci) %>% arrange(NOME)
br_cvu_3 <- bind_rows(br_cvu, br_cvu, br_cvu) %>% arrange(NOME)

# Continuar adiciionando uma coluna com o indice duplo para gerar a coluna technology.
# Adding the tecnology column. It'll be different of the swedish model, because it'll difer the thermal technology

tech_vetor <- transmute(br_ci_3, Technology = paste("Thermal", TIPO_COMB., sep = "."))

exp_corresp <- bind_cols(exp_corresp,tech_vetor)

#### Adding the column "variable"
variable <- as_tibble(rep(c("Investment", "VarCost", "MaxCap"),nrow(exp_corresp)/3))

exp_corresp <- bind_cols(exp_corresp, variable)
colnames(exp_corresp, do.NULL = TRUE, prefix = "col")
colnames(exp_corresp) <- c("nome", "sub", "Region", "P", "Technology", "Variable")


#### Adding values of InvestCost, VarCost and MaxCap

# Adding an empty column
exp_corresp <- mutate(exp_corresp, Value = "")

# exp_corresp <- bind_cols(exp_corresp, br_ci_3[,8])


# Adjusting the table br_ci_3, replacing the equals values by zeros. 

br_ci_3_loop <- br_ci_3
for (i in c(1:nrow(br_ci_3))){
  
  if(br_ci_3[i,7] == br_ci_3[i + 1,7]){
    br_ci_3_loop[i + 1,7] = 0 
  } else {br_ci_3_loop[i + 1,7] = br_ci_3_loop[i + 1,7]
  }}

exp_corresp_ci <- exp_corresp
exp_corresp_ci[,7] <- br_ci_3_loop[,7]
#exp_corresp_ci[which(exp_corresp1$Variable == "Investment"),7] <- br_ci_3_loop[,7]

exp_corresp_ci 
# Saving in csv file the exp_corresp_ci
#setwd("C:/Users/cancella/Google Drive/!IIASA/COPA/data/investOptions")
#save_exp_corresp_ci <- write.table(exp_corresp_ci, file = "InvestOpts_br_ci.csv", sep=";", row.names = FALSE)


#### Trying to add varCosts
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA  Initial Data/Costs/Tables")
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/Costs/Tables")
test_br_cvu <- read.csv2("tabela_para_invest_opts_br_sem_CI.csv", header = T, sep = ";", stringsAsFactors = F)
br_cvu_3_test <- bind_rows(br_cvu, br_cvu, br_cvu) %>% arrange(NOME)

br_cvu_3_test <- bind_cols(br_cvu_3, exp_corresp_ci[,6])

# Transforming every column in numeric 
  ## N~ao entendo a sint'axe dessa fun'c~ao, mas ela transforma a coluna que eu quero em double e 
  ## permite que eu adicione essa coluna na tabela que estamos montando
  ## Esta fun'cao so pode ser aplicada a uma tabela auxiliar, pq ela transforma algumas colunas em NAs. 
br_cvu_3_test[sapply(br_cvu_3_test, is.factor)] <- lapply(br_cvu_3_test[sapply(br_cvu_3_test, is.factor)], 
                                                          function(x) as.numeric(as.character(x)))

exp_corresp1 <- exp_corresp_ci
exp_corresp1[which(exp_corresp1$Variable == "VarCost"),7] <- br_cvu_3_test[which(br_cvu_3_test$Variable == "VarCost"),7]

exp_corresp_ci[which(exp_corresp_ci$Variable == "VarCost"),7] <- br_cvu_3_test[which(br_cvu_3_test$Variable == "VarCost"),7]

#### Adding available capacity
# Now our table already has investment costs and variable costs. Now we are in lack of the available capacity. 
# As we don't have information about the available capacity of non existing plants, we'll use the total capacity of each. 
# But for the existing ones, we'll use available capacity


br_ci_3[which(br_ci_3$pot_disp == 0),6] <- br_ci_3[which(br_ci_3$pot_disp == 0),5]

# Adding a column to allow comparation. 
br_ci_3 <- bind_cols(br_ci_3, exp_corresp_ci[,6])

exp_corresp1 <- exp_corresp_ci
exp_corresp_ci[which(exp_corresp_ci$Variable == "MaxCap"),7] <- br_ci_3[which(br_ci_3$Variable == "MaxCap"),6]

# johannes said that is necessary to have at least 1000 GW in each region.  
# Otherwise it could give infeasibilities. Let's verify it. 

#### test
max_cap  <- filter(exp_corresp_ci, Variable == "MaxCap")
max_cap_grouped <- group_by(max_cap, nome)
Region <-  select(max_cap, Region)
Region <- Region[,2]

# Transforming the value column in double
max_cap[sapply(max_cap, is.character)] <- lapply(max_cap[sapply(max_cap, is.character)], 
                                                          function(x) as.numeric(as.character(x)))
table_test <- bind_cols(Region, max_cap[,7])
#grouped_table_test <- group_by(table_test, Region)
#soma_test <- summarise(grouped_table_test, sum(Value))
soma_test <- table_test %>% group_by(Region) %>% summarise(sum(Value))

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
id_br_366 <- id_br[1:366,]


invest_opts_br_initial <- bind_cols(id_br_366, exp_corresp_ci[,3:ncol(exp_corresp_ci)])
colnames(invest_opts_br_initial, do.NULL = TRUE, prefix = "col")
colnames(invest_opts_br_initial) <- c("id", "Region", "P", "Technology", "Variable", "Value")

# converting the column value into integer
value_to_int <- invest_opts_br_initial
value_to_int[sapply(value_to_int, is.character)] <- lapply(value_to_int[sapply(value_to_int, is.character)], 
                                                 function(x) as.numeric(as.character(x)))

invest_opts_br_final <- invest_opts_br_initial
invest_opts_br_final[,6] <- value_to_int[,6]

invest_opts_br <- bind_rows(invest_opts_br_final, last_plants)

tt<-invest_opts_br %>% group_by(Technology,Variable) %>% summarize(s=sum(Value)) # adjusting order of plants

invest_opts_br <- invest_opts_br %>% arrange(Region,P,Technology)

# Transforming the values of variable costs in millions of R$/MWh. They are in R$/MWh
invest_opts_br$Value[invest_opts_br$Variable == "VarCost"] <- invest_opts_br$Value[invest_opts_br$Variable == "VarCost"] / 1e6

# Saving the invest_opts_br in a .csv file 
#setwd("C:/Users/cancella/Google Drive/!IIASA/COPA/data/investOptions")
#save_invest_opts_br <- write.table(invest_opts_br, file = "investOpts_br_thermal.sources.csv", sep = ";", row.names = FALSE )

##### For the time being, changing the names of regions from "BR" to "SE"
invest_opts_br$Region[invest_opts_br$Region == "BR1"] <- "SE1"
invest_opts_br$Region[invest_opts_br$Region == "BR2"] <- "SE2"
invest_opts_br$Region[invest_opts_br$Region == "BR3"] <- "SE3"
invest_opts_br$Region[invest_opts_br$Region == "BR4"] <- "SE4"
#Technology column = Thermal
invest_opts_br$Technology <- "Thermal"

#setwd("C:/Users/cancella/Google Drive/!IIASA/COPA/data/investOptions")
#save_invest_opts_br <- write.table(invest_opts_br, file = "investOpts_br_thermal.sources_1.csv", sep = ";", row.names = FALSE )
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