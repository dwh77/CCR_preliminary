library(tidyverse)
library(dplyr) #slice function
library(scales) #sclae_x_datetime
library(patchwork) #arrange plots


data <- read.csv("./ccre-waterquality.csv", skip = 1, col_types = cols(.default = "d", TIMESTAMP = "T"))

data <- slice(data, -c(1:2))

mytheme_AS <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank(), axis.line = element_line(colour = "black"),
                    legend.key = element_blank(),legend.background = element_blank(),
                    legend.title = element_text(size = 12),  #all sizes were 10 previously 
                    legend.text=element_text(size=12),
                    axis.text=element_text(size=12),
                    axis.title=element_text(size=12,
                                            #face="bold"
                    ),
                    plot.title = element_text(size = 12, face = "bold", hjust = 0.5))

data$TIMESTAMP <- as.POSIXct(data$TIMESTAMP)

data$fDOM_QSU_9 <- as.double(data$fDOM_QSU_9)
data$doobs_9 <- as.double(data$doobs_9)
data$TDS_9 <- as.double(data$TDS_9)
data$SpCond_9 <- as.double(data$SpCond_9)

data$EXO_wtr_9 <- as.double(data$EXO_wtr_9)


data <- data %>% 
  mutate(fDOM_QSU_9 = ifelse(fDOM_QSU_9 <3, NA, fDOM_QSU_9)) %>% 
  filter(!is.na(fDOM_QSU_9))








fdom <- ggplot(data = data, mapping = aes(x = TIMESTAMP, y = fDOM_QSU_9))+
  geom_line(color = "maroon", size = 1)+
  xlab("Date")+
  # ylab(expression(paste("Flow (",m^3, s^-1,")",)))+
  ylab("fDOM (QSU)")+
  ggtitle("fDOM 1140 ft (9m)")+
  ylim(c(0,30))+
  scale_x_datetime(labels = date_format("%b %d")) +  #removed %Y after %b in date_fomrat
  #scale_x_datetime(date_breaks = "2 month", labels = date_format("%b %Y")) +
  #theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  mytheme_AS

fdom

do <- ggplot(data = data, mapping = aes(x = TIMESTAMP, y = doobs_9))+
  geom_line(color = "blue", size = 1)+
  xlab("Date")+
  # ylab(expression(paste("Flow (",m^3, s^-1,")",)))+
  ylab("DO (mg/L)")+
  ggtitle("Dissolved Oxygen 1140 ft (9m)")+
  ylim(c(0,15))+
  scale_x_datetime(labels = date_format("%b %d")) +  #removed %Y after %b in date_fomrat
  #scale_x_datetime(date_breaks = "2 month", labels = date_format("%b %Y")) +
  #theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  mytheme_AS

do


tds <- ggplot(data = data, mapping = aes(x = TIMESTAMP, y = TDS_9))+
  geom_line(color = "orange", size = 1)+
  xlab("Date")+
  # ylab(expression(paste("Flow (",m^3, s^-1,")",)))+
  ylab("TDS (mg/L)")+
  ggtitle("Total Dissolved Solids 1140 ft (9m)")+
  ylim(c(20,40))+
  scale_x_datetime(labels = date_format("%b %d")) +  #removed %Y after %b in date_fomrat
  #scale_x_datetime(date_breaks = "2 month", labels = date_format("%b %Y")) +
  #theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  mytheme_AS

tds


spcond <- ggplot(data = data, mapping = aes(x = TIMESTAMP, y = SpCond_9))+
  geom_line(color = "red", size = 1)+
  xlab("Date")+
  # ylab(expression(paste("Flow (",m^3, s^-1,")",)))+
  ylab("Sp Cond (uS/cm)")+
  ggtitle("Specific Conductivity 1140 ft (9m)")+
  ylim(c(40,60))+
  scale_x_datetime(labels = date_format("%b %d")) +  #removed %Y after %b in date_fomrat
  #scale_x_datetime(date_breaks = "2 month", labels = date_format("%b %Y")) +
  #theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  mytheme_AS

spcond


plots <- (fdom | tds) / (do | spcond)
plots <- fdom | tds | do | spcond
plots

