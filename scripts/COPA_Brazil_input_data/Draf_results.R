# weekly works for a period of one year. 
# transfer_fig <-transfer_weekly %>% ggplot(aes(x=datetime,y=value)) +geom_line(aes(col=reg1), size =1)  + facet_wrap(~reg) +
#   ylab("MW") + xlab("") + theme(axis.text.x = element_text(angle=45)) + 
#   ggtitle("Electricity transfer" ) +  theme(plot.title = element_text(hjust = 0.5))
# plot(transfer_fig)
# ggsave("../results/figures/opt_transfer.pdf",transfer_fig,width=30,height=20,units="cm")


#### APPENDIX ####
#### VarCost thermal power plants ####
# setwd("C:/Users/cancella/Google Drive/!IIASA/COPA/data/investOptions")
# #setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA/data/investOptions")
# thermal_cost <- as_tibble(read.csv2("investOpts_br_thermal.sources_2012.csv", sep = ";", header = T)) %>% 
#   filter(Variable == "VarCost")
# thermal_cost_i <- thermal_cost
# thermal_cost_i[sapply(thermal_cost_i, is.factor)] <- lapply(thermal_cost_i[sapply(thermal_cost_i, is.factor)], 
#                                                             function(x) as.numeric(as.character(x)))
# thermal_cost$Value <- thermal_cost_i$Value
# comparation_var_cost <- thermal_cost %>% group_by(Region, Technology) %>% summarise(avg_varCost = mean(Value))
# tech <- stringr::str_split_fixed(comparation_var_cost$Technology, ".",9) # Separating elements of one column
# comparation_var_cost$Technology <- tech[,9]
# ggplot() + 
#  geom_bar(data = comparation_var_cost, aes(x = Technology, y = avg_varCost, fill = Technology),stat = "identity") +
#  facet_wrap(~Region) +   scale_fill_brewer(palette="Paired") + theme(axis.text.x = element_text(angle=45)) +
#  ylab("R$ / MWh ") + xlab("") +  ggtitle("Comparation variable thermal costs" ) + 
#  theme(plot.title = element_text(hjust = 0.5))