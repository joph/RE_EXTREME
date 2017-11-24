# Analizing the results of COPA Brazil runs
# We have to load the results from "load_data_write_gdx.R".
setwd(base_dir)

# Renewable generation ----------------------------------------------------
renew <- results %>% filter(name == "x_renew")
sum(renew$value)
renew_summary <- renew %>% group_by(reg, datetime, iTechnology) %>% summarise(renew_gen = sum(value))
renew_summary1 <- renew %>% group_by(reg,iTechnology) %>% summarise(renew_gen = sum(value))

renew_gen_fig <- ggplot() + 
  geom_line(data = renew_summary, aes(x = datetime, y = renew_gen, col = iTechnology)) +
  facet_wrap(~reg) +  ylab("MWh ") + xlab("") + 
  ggtitle("Renewable generation" ) + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.text.x = element_text(angle = 45))
plot(renew_gen_fig)
# png("../results/figures/renewable_generation.png")
# renew_gen_fig
# dev.off()
#ggsave("../results/figures/renewable_generation.pdf",renew_gen_fig,width=30,height=20,units="cm")

# Thermal generation (hourly basis)------------------------------------------------------
x_term <- results %>% filter(name == "x_term")
summary <- x_term %>% group_by(reg, datetime) %>% summarise(x_term = sum(value))
thermal_gen <- ggplot() +
  geom_line(data = summary, aes(x = datetime, y = x_term, col = reg)) + # By the time we have many thermal technologies, we change col = reg by col = iTechnology
  facet_wrap(~reg) +   ylab("MWh ") + xlab("") + theme(axis.text.x = element_text(angle = 45)) +
  ggtitle("Thermal generation" ) + theme(plot.title = element_text(hjust = 0.5))
  
plot(thermal_gen)
# png("../results/figures/thermal_gen.png")
# thermal_gen
# dev.off()
#ggsave("../results/figures/thermal_gen.pdf",thermal_gen,width=30,height=20,units="cm")
themal_regional_gen <- x_term %>% select(reg, p, value, datetime) %>% 
  group_by(reg) %>% summarise(reg_sum = sum(value))

# Energy transfer ---------------------------------------------------------
x_transfer <- results %>% filter(name == "x_transfer")
transfer_weekly<- x_transfer %>% group_by(week(datetime),reg,reg1) %>% summarize(value=max(value),
                                                                                 datetime=min(datetime)
)

transfer_fig <-x_transfer %>% ggplot(aes(x=datetime,y=value)) +geom_line(aes(col=reg1))  + facet_wrap(~reg) +
  ylab("MW") + xlab("") + theme(axis.text.x = element_text(angle=45)) + 
  ggtitle("Electricity transfer" ) +  theme(plot.title = element_text(hjust = 0.5))
plot(transfer_fig)

# png("../results/figures/electricity_transfer.png")
# transfer_fig
# dev.off()