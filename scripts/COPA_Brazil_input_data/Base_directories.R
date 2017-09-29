# Newave deck
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/Costs/deck_newave_2017_05")
setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA  Initial Data/Costs/deck_newave_2017_05")

# Tables
setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA  Initial Data/Costs/Tables")
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA  Initial Data/Costs/Tables")
 
#Bibliotecas
library(tidyverse)
library(dplyr)
library(tibble)
library(feather)
library(xlsx)
library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(reshape2)
library(gdxrrw)

# Arquivos do deck a serem lidos:
# conft.dat -> nomes, localização e situação - OK
# term.dat -> pot, FC, disponibilidade, ger min - OK
# clast.dat -> custos CVU (RS/MWh) e combustível - OK
# expt.dat -> expansão das térmicas
# manutt.dat -> data e duração da manutenção de cada unidade geradora das usinas
# sistema.dat Verificar se precisa ler

# verificar o hydredit.

