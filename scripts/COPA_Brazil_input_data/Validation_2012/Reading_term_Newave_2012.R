# This script reads the term.dat file from the Newave deck used in the Dec 2012 PMO (Monthly Operation Program)
# It produces the tables and visualization graphs of the information of interest.
# Information of interest in this file: nominal capacity (pot), capacity factor (FC),
  # availability (disponibilidade), minimal generation (ger min)

# reading term.dat
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/Newave_deck_dec_2012")
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA  Initial Data/Newave_deck_dec_2012")
term <- as_tibble(read.delim("TERM_new.DAT", header = TRUE, sep = "", quote = "\"", dec = ".", 
                             fill = TRUE, row.names = NULL, comment.char = ""))

# Removing the lines with plants with variable cost = 0 and fuel type = "contrato" (contract in english)
# Reading the file with the plants to be withdrawn 
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/Costs/Tables/validation_2012")
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA  Initial Data/Costs/Tables/validation_2012")
usinas_retiradas <- read_feather("Usinas_retiradas.feather")

# Making the loop that creates a table containing the plants to be withdrawn.
nomes_usinas_retiradas <- as.character(usinas_retiradas$NOME)
#nomes_usinas_retiradas_1 <- c( "DO_ATLAN_CSA", "C.ROCHA","JARAQUI", "MANAUARA", "PONTA_NEGRA", "SUZANO_MA", "TAMBAQUI", "ARGENTINA_1", 
                   #         "ARGENTINA_1B", "ARGENTINA_2A","ARGENTINA_2B", "ARGENTINA_2C", "ARGENTINA_2D")
term_filter <- term
for(i in seq_along(nomes_usinas_retiradas)){
  term_filter <- filter(term_filter, NOME != nomes_usinas_retiradas[[i]])
  }

print(term_filter)
# After the loop, the table term_filter contains only the plants that will be used in the analysis
term_filter_disp<- select(term_filter, NUM:IP) # infos: capacity (POT), capacity factor (FCMAX), interruptions (TEIF and IP) for each plant
term_filter_GMIN <- select(term_filter, NUM, NOME,JAN.XX:XXX.XX) # infos: minimal generation for each plant in the first year

# Accessing the maximum availability of the thermal power plants 
# and adding a column with this information
# It means the amount of power that a certain plant can generates continuously
# Disp = POt*FC*(1-teif)*(1-ip), onde
  # Pot = nominal capacity of the plant in MW
  # FCmx the percentage of nominal power that the plant can generate continuously in the place where it is installed (capacity factor)
  # TEIF and the expected percentage of forced unavailability
  # IP and the expected percentage of scheduled outage
  # Dúvida: ao multiplicar pelo FC, eu estou transformando potência em energia?

# transforming column POT in double
term_filter_add_disp_i <- term_filter_disp
term_filter_add_disp_i[sapply(term_filter_add_disp_i, is.factor)] <- lapply(term_filter_add_disp_i[sapply(term_filter_add_disp_i, is.factor)], 
                                                                function(x) as.numeric(as.character(x)))
term_filter_add_disp_i
term_filter_disp$POT <- term_filter_add_disp_i$POT

term_filter_add_disp <-  mutate(term_filter_disp, Disp = POT*FCMX/100*(1-TEIF/100)*(1-IP/100))

ggplot(data =term_filter_add_disp) +
  geom_point(mapping=aes(x=NOME, y=POT), stat = "identity") +
  labs(x = "Usinas", y = "Potência (MW)")

# Saving the tables in a .csv format
# setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA  Initial Data/Costs/Tables/validation_2012")")
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/Costs/Tables/validation_2012")
write_feather(term_filter, "term_filter_2012.feather")
write_feather(term_filter_add_disp, "usinas_pot_disp_2012.feather")
write_feather(term_filter_GMIN, "usinas_GTMIN_2012.feather")


#save_usinas_term_filter <- write.table(term_filter, file = "term_filter.csv", sep=";", row.names = FALSE)
#save_usinas_pot_disp <- write.table(term_filter_add_disp, file = "usinas_pot_disp.csv", sep=";", row.names = FALSE)
#save_usinas_GTMIN <- write.table(term_filter_GMIN, file = "usinas_GTMIN.csv", sep=";", row.names = FALSE)
# Command to read the tables 
# CVU <- read.csv2("CVU por usina.csv", sep =";", header=T)


#### About the TEIF and IP of non-existent plants
# Ploting values of existent thermal plants
ggplot(term_filter_disp) + 
  geom_point(aes(term_filter_disp$NUM, term_filter_disp$TEIF)) + 
  geom_point(aes(term_filter_disp$NUM, term_filter_disp$IP, color = "IP"))