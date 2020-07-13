### CCR Chemistry plots ###

library(tidyverse)
library(ggplot2)
library(ggpubr)


#read in data 
data <- read_csv("./Final_Data_Scripts/Data/chemistry_EDI_13may2020.csv", col_types = cols(.default = "d",
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

