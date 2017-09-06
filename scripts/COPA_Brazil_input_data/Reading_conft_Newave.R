# This script reas the file "confit.dat" of Newave deck used in PMO of May 2017.
# It creates tables to be used in the construction of "investOpts" file for the Brazilian case.
# Information of interest: thermal power plant names, subsystem of each plant and situation of each plant (EE, EX or NE) 

setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/Costs/deck_newave_2017_05")
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA  Initial Data/Costs/deck_newave_2017_05")

# reading conft.dat
conft_t<- as_tibble(read.delim("CONFT_new.DAT", header = TRUE, sep = "", quote = "\"",
           dec = ".", fill = TRUE, comment.char = ""))

# Comparing the columns "num" and "class"
integer_classe <- as.integer(conft_t$CLASSE)
comparation <- as_tibble(conft_t$NUM - integer_classe)


# Removing column "class", because it is equal of "num"
conft_t_reduzida <- select(conft_t, NUM:U.EXIS) # initial table

# Removing the columns with variable cost zero and fuel = contract

# Reading the plants to be removed
# setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA  Initial Data/Costs/Tables")
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/Costs/Tables")

usinas_retiradas <- read.csv2("Usinas retiradas.csv", sep=";",header=T)
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA  Initial Data/Costs/deck_newave_2017_05")
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/Costs/deck_newave_2017_05")

# Loop that generates table with plants to be removed
nomes_usinas_retiradas <- c( "DO_ATLAN_CSA", "C.ROCHA","JARAQUI", "MANAUARA", "PONTA_NEGRA", "SUZANO_MA", "TAMBAQUI", "ARGENTINA_1", 
                             "ARGENTINA_1B", "ARGENTINA_2A","ARGENTINA_2B", "ARGENTINA_2C", "ARGENTINA_2D")
conft_loop <- conft_t_reduzida

for(i in seq_along(nomes_usinas_retiradas)){
  conft_loop <- filter(conft_loop, NOME != nomes_usinas_retiradas[[i]])
  print(conft_loop)
}

#### Playing with tha data ####

# Counting plants by subsystem
usinas_sub <- group_by(conft_loop, SSIS) %>%
  count()

# Couting plants by situation
usinas_sit <- group_by(conft_loop, U.EXIS) %>%
  count()

# Plotting plants by subsystema
ggplot(data = usinas_sub) +
  geom_bar(mapping=aes(x=SSIS, y=n), stat = "identity") +
  labs(x = "Subsistema", y = "Número de usinas")

# Plotting plants by situation
ggplot(data = usinas_sit) +
  geom_bar(mapping=aes(x=U.EXIS, y=n), stat = "identity") +
  labs(x = "Situação", y = "Número de usinas")
  
# Saving tables in .csv format
setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA  Initial Data/Costs/Tables")
save_usinas_agrupadas_subsistema <- write.table(conft_loop, file = "Usinas_agrupadas_por_subsistema.csv", sep=";", row.names = FALSE)
save_usinas_em_cada_subsitema <- write.table(usinas_sub, file = "usinas_em_cada_subsistema.csv", sep=";", row.names = FALSE)
save_usinas_por_situacao <- write.table(usinas_sit, file = "usinas_por_situação.csv", sep=";", row.names = FALSE)
# Command to read the tables
# CVU <- read.csv2("CVU por usina.csv", sep =";", header=T)


