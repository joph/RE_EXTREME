# This script simply organize the thermal plants data to be used on the "Creating_invest_opts_br_2012"
# Note that we are not interested here in the investment costs, because we want to operate the system with the existent
# plants.

setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/Costs/Tables/validation_2012")
# setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA  Initial Data/Costs/Tables/validation_2012")

# Reading the necessary tables
CVU_por_usina <-  read_feather("cvu_por_usina_2012.feather")
usinas_agrupadas_por_subsistema <- read_feather("usinas_agrupadas_por_subsistema.feather")
usinas_pot_disp <- read_feather("usinas_pot_disp_2012.feather")

# New table with the situation and fuel of each plant
comb_sit_inicial <- CVU_por_usina %>% mutate(Situacao = usinas_agrupadas_por_subsistema$U.EXIS)
comb_sit <- select(comb_sit_inicial,NUM:CLASSE,Situacao, CUSTO)

# Adding the information of available capacity
add_pot_disp <- mutate(comb_sit, pot = usinas_pot_disp$POT, pot_disp = usinas_pot_disp$Disp)
indice_duplo <- paste(add_pot_disp$CLASSE, add_pot_disp$Situacao, sep = "." )
sub_usinas <- select(usinas_agrupadas_por_subsistema, SSIS) 
tabela_para_invest_opts_br_sem_CI <-add_pot_disp %>%  mutate(ind = indice_duplo) %>% 
  mutate(Subsistema = sub_usinas$SSIS) # add a double index and subsystem

# Adjusting the order and renaming the columns
tabela_para_invest_opts_br_sem_CI <- tabela_para_invest_opts_br_sem_CI %>% 
  select(NUM:Situacao, pot:pot_disp,CUSTO, ind:Subsistema)
colnames(tabela_para_invest_opts_br_sem_CI) <- c("NUM", "NOME", "TIPO_COMB.", "Situacao", "pot", 
                                                 "pot_disp","CVU", "ind", "Subsistema")
## Conclusion: if I would like to make a file without the investiment costs, 
## the information of this table would be enough. 

# Saving the table with the required information to create the file that COPA is going to read.
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/Costs/Tables/validation_2012")
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA  Initial Data/Costs/Tables/validation_2012")
write_feather(tabela_para_invest_opts_br_sem_CI, "tabela_para_invest_opts_br_sem_CI_2012.feather") :)