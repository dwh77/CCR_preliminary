### CCR Chemistry plots ###

library(tidyverse)
library(ggplot2)
library(ggpubr)

mytheme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                 panel.background = element_blank(), axis.line = element_line(colour = "black"),
                 legend.key = element_blank(),legend.background = element_blank(),
                 axis.text=element_text(size=12),
                 axis.title=element_text(size=12,face="bold"))

#read in data 
data <- read_csv("./CCR_plots/Chemistry_plots/chemistry_EDI_13may2020.csv", col_types = cols(.default = "d",
                                                                                           Reservoir = "c",
                                                                                           DateTime = "T"))


ccr <- data %>% 
  filter(Reservoir == "CCR") %>% 
  mutate(Depth_m = ifelse(Depth_m %in% c(18, 19, 20, 21), 20, Depth_m)) %>%  #combining all hypo depths into one for plotting
  mutate(Depth_m = ifelse(Depth_m %in% c(12,14), 13, Depth_m)) %>%  #combining 12 and 14 (upper hypo) into 13 for plotting
mutate(Depth_m = ifelse(Depth_m %in% c(5, 6, 7), 6, Depth_m)) #combining 5,6,7 (~thermocline) into 6 one for plotting

head(ccr)

columns <- c("TN_ugL", "TP_ugL", "NH4_ugL", "NO3NO2_ugL", "SRP_ugL", "DOC_mgL")

TN <- ggplot(data = ccr, mapping = aes(x = DateTime, y = TN_ugL))+
  geom_point()+
  ggtitle("TN")+
  facet_wrap(~Depth_m)
TN

TP <- ggplot(data = ccr, mapping = aes(x = DateTime, y = TP_ugL))+
  geom_point()+
  ggtitle("TP")+
  facet_wrap(~Depth_m)
TP

NH4 <- ggplot(data = ccr, mapping = aes(x = DateTime, y = NH4_ugL))+
  geom_point()+
  ggtitle("NH4")+
facet_wrap(~Depth_m)
NH4

NO3NO2 <- ggplot(data = ccr, mapping = aes(x = DateTime, y = NO3NO2_ugL))+
  geom_point()+
  ggtitle("NO3NO2")+
  facet_wrap(~Depth_m)
NO3NO2

SRP <- ggplot(data = ccr, mapping = aes(x = DateTime, y = SRP_ugL))+
  geom_point()+
  ggtitle("SRP")+
  facet_wrap(~Depth_m)
SRP

DOC <- ggplot(data = ccr, mapping = aes(x = DateTime, y = DOC_mgL))+
  geom_point()+
  ggtitle("DOC")+
  facet_wrap(~Depth_m)
DOC


#### GRFP #### 

grfp <- data %>% 
  filter(Reservoir == c("FCR", "BVR", "CCR")) %>% 
  filter(Site == 50) 

ccr <- data %>% 
  filter(Reservoir == "CCR",
         Site == 50, 
         Depth_m == 0.1)

ccrplot <- ggplot(data = ccr, mapping = aes(x = DateTime, y = DOC_mgL))+
  geom_point()+
  mytheme
ccrplot
  
fcr <- data %>% 
  filter(Reservoir == "FCR",
         Site == 50, 
         Depth_m == 0.1)

fcrplot <- ggplot(data = fcr, mapping = aes(x = DateTime, y = DOC_mgL))+
  geom_point()+
  mytheme
fcrplot

  
bvr <- data %>% 
  filter(Reservoir == "BVR",
         Site == 50, 
         Depth_m == 0.1)

bvrplot <- ggplot(data = bvr, mapping = aes(x = DateTime, y = DOC_mgL))+
  geom_point()+
  mytheme
bvrplot 
  

fig <- ggarrange(fcrplot, bvrplot, ccrplot,
                          labels = c("a", "b", "c"),
                          ncol = 1, nrow = 3)

fig

mytheme_grfp <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                 panel.background = element_blank(), axis.line = element_line(colour = "black"),
                 axis.text=element_text(size=12),
                 axis.title=element_text(size=12,face="bold"))

test <- rbind(ccr, bvr, fcr)

ggplot(data = test, mapping = aes(x = DateTime, y = DOC_mgL))+
  #geom_line()+
  geom_point()+
  facet_wrap(~Reservoir, nrow = 3, ncol = 1)+
  xlab("Time")+
  ylab(expression(paste("Chl-" , italic("a"), " (" , mu,  "g ", L^-1, ")" )))+
  mytheme_grfp

mean(ccr$DOC_mgL, na.rm = TRUE) #CCR mean DOC is 2.8 mg/L
median(ccr$DOC_mgL, na.rm = TRUE) #CCR median DOC is 2.7 mg/L
mean(fcr$DOC_mgL, na.rm = TRUE) #FCR mean DOC is 3.4 mg/L
median(fcr$DOC_mgL, na.rm = TRUE) #FCR median DOC is 3.0 mg/L
mean(bvr$DOC_mgL, na.rm = TRUE) #BVR mean DOC is 3.8 mg/L
median(bvr$DOC_mgL, na.rm = TRUE) #BVR median DOC is 2.8 mg/L






