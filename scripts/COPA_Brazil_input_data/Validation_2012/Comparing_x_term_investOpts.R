# 10/20/2017
# This script compares themal input file (investOpts_br_thermal.sources_1_2012.csv) and the results from COPA run
# gdx results: "18.results_time_resolution - J2 - last_hour.gdx"

# reading themal input file 
setwd("C:/Users/cancella/Google Drive/!IIASA/COPA/data/investOptions")
#setwd("C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA/data/investOptions")
investOpts <- read_delim("investOpts_br_thermal.sources_1_2012.csv", delim = ";", 
                         locale = locale(decimal_mark = ".") )

# investOpts %>% group_by(Region) %>% arrange(Variable == "VarCost") %>% mutate(potsum=cumsum(pot_disp)) %>% 
#   select(potsum,CVU,Subsistema) %>% ggplot(aes(potsum,CVU)) +geom_line()+ facet_wrap(~Subsistema)

# Defining total capacity per region per month (Gwh)
totalCap <- investOpts %>% filter(Variable == "MaxCap") %>% group_by(Region) %>% summarise(totalCap = sum(Value)/1e3)

ggplot(totalCap, aes(Region, totalCap)) + geom_bar(stat = "identity")

# defining thermal generation
x_term <- results %>% filter(name == "x_term") %>% group_by(reg, datetime) %>% summarise(gwh = sum(value)/1e3)
colnames(x_term) <- c("reg", "datetime","gwh")
# preparing to join totalCap and x_term
namesNew <- c(unique(x_term$reg))
namesNew <- c(namesNew[3], namesNew[4], namesNew[2], namesNew[1])
for (i in c(1: length(namesNew))){
  totalCap$Region[i] <- namesNew[i]   
  }

# data together in order to make plots
compThermal <- full_join(x_term, totalCap, by = c("reg" = "Region"))
compThermal <- gather(compThermal, key = type, value, -reg, -datetime)

# plotting
lines <- ggplot(compThermal, aes(datetime,value, col = type))+ geom_line(size =1) + 
  scale_colour_hue(l=45)+theme(axis.text.x = element_text(angle=45))+ facet_wrap(~reg)+ ylab("GWh") + xlab("") + 
  ggtitle("x_term x MaxCap") + theme(plot.title = element_text(hjust = 0.5))
plot(lines)

#saving pdf
path_fig <- "C:/Users/cancella/Google Drive/!IIASA/COPA/runs/Validation_2012/figures"
#path_fig <- "C:/Users/Rafael/Desktop/Google Drive @PPE/!IIASA/COPA/runs/Validation_2012/figures"
pdf(str_c(path_fig,"/comparing_xterm_maxcap.pdf"))
lines
dev.off()
