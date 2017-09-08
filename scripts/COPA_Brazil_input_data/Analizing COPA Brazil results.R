# Analizing the results of COPA Brazil runs
# We have to load the results from "load_data_write_gdx.R".

#### Renewable generation ####
renew <- results %>% filter(name == "x_renew")
sum(renew$value)
renew_summary <- renew %>% group_by(reg, datetime, iTechnology) %>% summarise(renew_gen = sum(value))
renew_summary1 <- renew %>% group_by(reg,iTechnology) %>% summarise(renew_gen = sum(value))

renew_gen_fig <- ggplot() + 
  geom_line(data = renew_summary, aes(x = datetime, y = renew_gen, col = iTechnology), size = 1) +
  facet_wrap(~reg) +  ylab("MWh ") + xlab("") + 
  ggtitle("Renewable generation" ) + theme(plot.title = element_text(hjust = 0.5))
plot(renew_gen_fig)
#ggsave("../results/figures/renewable_generation.pdf",renew_gen_fig,width=30,height=20,units="cm")

#### Thermal generation ####
x_term <- results %>% filter(name == "x_term")
summary <- x_term %>% group_by(reg, datetime) %>% summarise(x_term = sum(value))
thermal_gen <- ggplot() +
  geom_line(data = summary, aes(x = datetime, y = x_term, col = reg), size = 1) + # By the time we have many thermal technologies, we change col = reg by col = iTechnology
  facet_wrap(~reg) +   ylab("MWh ") + xlab("") + 
  ggtitle("Thermal generation" ) + theme(plot.title = element_text(hjust = 0.5))
plot(thermal_gen)
#ggsave("../results/figures/thermal_gen.pdf",thermal_gen,width=30,height=20,units="cm")

#### System expansion ####
x_invest_intermittent <- results %>% filter(name == "x_invest_intermittent")
x_invest_thermal_cap <- results %>% filter(name == "x_invest_thermal_cap")
invest <- rbind(x_invest_intermittent, x_invest_thermal_cap) %>% select(name, reg, p, value, iTechnology)
invest$iTechnology[invest$name == "x_invest_thermal_cap"] <- "Thermal"
expansion <- invest %>% group_by(reg, iTechnology) %>% summarise(avg_exp = mean(value))
sys_exp <- ggplot() + 
    geom_bar(data = expansion, aes(x = iTechnology, y = avg_exp, fill = iTechnology),stat = "identity") +
    facet_wrap(~reg) +   scale_fill_brewer(palette="Set2") +
    ylab("MW") + xlab("") +  ggtitle("System expansion - capacity" ) + 
    theme(plot.title = element_text(hjust = 0.5))
plot(sys_exp)
#ggsave("../results/figures/system_expansion.pdf",sys_exp,width=30,height=20,units="cm")


#### APPENDIX ####
#### VarCost thermal power plants ####
#setwd("C:/Users/cancella/Google Drive/!IIASA/COPA/data/investOptions")
setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA/data/investOptions")
thermal_cost <- as_tibble(read.csv2("investOpts_br_thermal.sources.csv", sep = ";", header = T)) %>% 
  filter(Variable == "VarCost")
thermal_cost_i <- thermal_cost
thermal_cost_i[sapply(thermal_cost_i, is.factor)] <- lapply(thermal_cost_i[sapply(thermal_cost_i, is.factor)], 
                                                            function(x) as.numeric(as.character(x)))
thermal_cost$Value <- thermal_cost_i$Value
comparation_var_cost <- thermal_cost %>% group_by(Region, Technology) %>% summarise(avg_varCost = mean(Value))
tech <- stringr::str_split_fixed(comparation_var_cost$Technology, ".",9) # Separating elements of one column
comparation_var_cost$Technology <- tech[,9]
ggplot() + 
 geom_bar(data = comparation_var_cost, aes(x = Technology, y = avg_varCost, fill = Technology),stat = "identity") +
 facet_wrap(~Region) +   scale_fill_brewer(palette="Paired") + theme(axis.text.x = element_text(angle=45)) +
 ylab("R$ / MWh ") + xlab("") +  ggtitle("Comparation variable thermal costs" ) + 
 theme(plot.title = element_text(hjust = 0.5))
