# This script reads the clast.dat file, which contains unitary variable cost (CVU in Portuguese), plants' situation and
# fuel of each one. 
# After reading the file, the script cleans it by removing redundant data and 
# save it in intermediate variables.

# Files to be read:
# clast.dat -> variable costs (CVU in RS/MWh) and fuel 

setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/Newave_deck_dec_2012")
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA  Initial Data/Newave_deck_dec_2012")

#Reading clast.dat
clast<- as_tibble(read.delim("CLAST_new.DAT", header = TRUE, sep = "", quote = "\"",
           dec = ".", fill = TRUE, comment.char = "")) %>% select(NUM:CUSTO)  # values of variable costs (CVU) by plant initial 

# Removing the obeservations that have variable costs zero and the fuel = contrato (contract)
clast_no_zeros <- filter(clast, CUSTO != 0.01)
clast_zeros <- filter(clast, CUSTO == 0.01)
clast_no_zeros_no_contract <- filter(clast_no_zeros, CLASSE != "CONTRATO")
clast_no_zeros_contract   <- filter(clast_no_zeros, CLASSE == "CONTRATO")
usinas_retiradas <- bind_rows(clast_zeros,clast_no_zeros_contract)
CVU_por_usina <- clast_no_zeros_no_contract # values of variable costs (CVU) by plant final


# Organizing by fuel and variable cost
ord_comb <- arrange(CVU_por_usina, CLASSE)
ord_custo <- arrange(CVU_por_usina, CUSTO)


#### Playing with the data ####
ggplot(data=ord_comb) + geom_point(aes(x=NOME, y=CUSTO, color = CLASSE))

# Grouping by fuel and calculating the mean, max and min CVUs per fuel
group_comb <- group_by(ord_comb, CLASSE)
medias <- summarise(group_comb, media = mean(CUSTO))
max <- summarise(group_comb, max = max(CUSTO))
min <- summarise(group_comb, min = min(CUSTO))
group_comb_max <- add_column(medias, max = max$max)
group_comb_full <- add_column(group_comb_max, min = min$min)

#Boxplot per fuel
ggplot(data=group_comb,aes(x=CLASSE, y=CUSTO)) + 
  geom_boxplot(data = group_comb, aes(x=CLASSE, y=CUSTO))

# Ploting mean, maximum and minimum values of the variable costs per fuel
ggplot(group_comb_full) + 
  geom_bar(mapping=aes(x=CLASSE, y=media, fill = CLASSE), stat="identity") +
  geom_point(aes(CLASSE,y = max)) + 
  geom_point(aes(CLASSE,y = min)) + 
  labs(x="Fuel", y="CVU (R$/MWh)")

# Saving tables in .feather and .csv format
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA  Initial Data/Costs/Tables/validation_2012")
#setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/Costs/Tables/validation_2012")
#write_feather(CVU_por_usina,"cvu_por_usina_2012.feather")
#write_feather(group_comb_full, "CVU_por_combustível.feather")
#write_feather(usinas_retiradas, "Usinas_retiradas.feather")
#save_CVU_usinas <- write.table(CVU_por_usina, file = "CVU por usina.csv", sep=";", row.names = FALSE)
#save_CVU_comb <- write.table(group_comb_full, file = "CVU por combustível.csv", sep=";", row.names = FALSE)
#save_usinas_retiradas <- write.table(usinas_retiradas, file = "Usinas retiradas.csv", sep=";", row.names = FALSE)
#Comando para ler as tabelas
#CVU <- read.csv2("CVU por usina.csv", sep =";", header=T)


