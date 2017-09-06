# This script reads the clast.dat file, which contains unitary variable cost (CVU in Portuguese), plants' situation and
# fuel of each one. 
# After reading the file, the script cleans it by removing redundant data and 
# save it in intermediate variables.

# Files to be read:
# clast.dat -> variable costs (CVU in RS/MWh) and fuel 

setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/Costs/deck_newave_2017_05")
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA  Initial Data/Costs/deck_newave_2017_05")
#leitura do clast.dat
clast<- read.delim("CLAST_new.DAT", header = TRUE, sep = "", quote = "\"",
           dec = ".", fill = TRUE, comment.char = "")
clast_tibble <- as_tibble(clast)


# Comparing the column of costs
costs_comparation <- as_tibble(clast_tibble$CUSTO - clast_tibble$CUSTO.1)
costs_comparation_01 <- as_tibble(clast_tibble$CUSTO - clast_tibble$CUSTO.1)
costs_comparation_02 <- as_tibble(clast_tibble$CUSTO - clast_tibble$CUSTO.2)
costs_comparation_03 <- as_tibble(clast_tibble$CUSTO - clast_tibble$CUSTO.3)
costs_comparation_04 <- as_tibble(clast_tibble$CUSTO - clast_tibble$CUSTO.4)

# Removing the columns with the same information 
clast_tibbe_reduzida <- select(clast_tibble, NUM:CUSTO) # values of variable costs (CVU) by plant initial 

# Removing the obeservations that have variable costs zero and the fuel = contrato (contract)
clast_tibbe_reduzida_sem_zeros <- filter(clast_tibbe_reduzida, CUSTO != 0)
clast_tibbe_reduzida_com_zeros <- filter(clast_tibbe_reduzida, CUSTO == 0)
clast_tibbe_reduzida_sem_zeros_sem_contrato <- filter(clast_tibbe_reduzida_sem_zeros, TIPO_COMB. != "CONTRATO")
clast_tibbe_reduzida_sem_zeros_com_contrato <- filter(clast_tibbe_reduzida_sem_zeros, TIPO_COMB. == "CONTRATO")
usinas_retiradas <- bind_rows(clast_tibbe_reduzida_com_zeros,clast_tibbe_reduzida_sem_zeros_com_contrato)
CVU_por_usina <- clast_tibbe_reduzida_sem_zeros_sem_contrato # values of variable costs (CVU) by plant final

# Organizing by fuel and variable cost
ord_comb <- arrange(CVU_por_usina, TIPO_COMB.)
ord_custo <- arrange(CVU_por_usina, CUSTO)


#### Playing with the data ####
ggplot(data=ord_comb) + geom_point(mapping=aes(x=NOME, y=CUSTO, color = TIPO_COMB.))

# Grouping by fuel and calculating the mean, max and min CVUs per fuel
group_comb <- group_by(ord_comb, TIPO_COMB.)
medias <- summarise(group_comb, media = mean(CUSTO))
max <- summarise(group_comb, max = max(CUSTO))
min <- summarise(group_comb, min = min(CUSTO))
group_comb_max <- add_column(medias, max = max$max)
group_comb_full <- add_column(group_comb_max, min = min$min)

#Boxplot per fuel
ggplot(data=group_comb, mapping = aes(x=TIPO_COMB., y=CUSTO)) + 
  geom_boxplot(data = group_comb, mapping = aes(x=TIPO_COMB., y=CUSTO))

# Ploting mean, maximum and minimum values of the variable costs per fuel
ggplot(group_comb_full) + 
  geom_bar(mapping=aes(x=TIPO_COMB., y=media, fill = TIPO_COMB.), stat="identity") +
  geom_point(aes(TIPO_COMB.,y = max)) + 
  geom_point(aes(TIPO_COMB.,y = min)) + 
  labs(x="Fuel", y="CVU (R$/MWh)")

# Saving tables in .csv format
setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA  Initial Data/Costs/Tables")
save_CVU_usinas <- write.table(CVU_por_usina, file = "CVU por usina.csv", sep=";", row.names = FALSE)
save_CVU_comb <- write.table(group_comb_full, file = "CVU por combustível.csv", sep=";", row.names = FALSE)
save_usinas_retiradas <- write.table(usinas_retiradas, file = "Usinas retiradas.csv", sep=";", row.names = FALSE)
#Comando para ler as tabelas
#CVU <- read.csv2("CVU por usina.csv", sep =";", header=T)


