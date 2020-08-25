# BVR CTD Heatmaps
# Author: Ryan McClure
# Date last updated: 080718
##Modified 4 June 2020 by DWH for CCR plots 

# Makes heatmaps of the CTD data in Beaverdam reservoir

# load libraries
library(akima)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(reshape2)
library(gridExtra)
library(grid)
library(colorRamps)
library(RColorBrewer)
library(rLakeAnalyzer)
library(lubridate) #for Julian date 
library(tidyr)
library(ggplot2)
library(ggpubr)


### Getting data filtered selected for plots 
july8 <- read_csv("C:/Users/dwh18/OneDrive/Desktop/R_Projects/Reservoirs/Data/DataNotYetUploadedToEDI/Raw_CTD/csv_outputs/070820_ccr50_halfcast.csv")

july15 <- read_csv("C:/Users/dwh18/OneDrive/Desktop/R_Projects/Reservoirs/Data/DataNotYetUploadedToEDI/Raw_CTD/csv_outputs/071520_ccr50.csv")
# which.max(july15$Depth_m) ## old code for when upcast wasn't removed from csv in processing 
# july15a <- july15[-c(2977:4426),] ## old code for when upcast wasn't removed from csv in processing 

july22 <- read_csv("C:/Users/dwh18/OneDrive/Desktop/R_Projects/Reservoirs/Data/DataNotYetUploadedToEDI/Raw_CTD/csv_outputs/072220_ccr50.csv")

july29 <- read_csv("C:/Users/dwh18/OneDrive/Desktop/R_Projects/Reservoirs/Data/DataNotYetUploadedToEDI/Raw_CTD/csv_outputs/072920_ccr50.csv")

aug05 <- read_csv("C:/Users/dwh18/OneDrive/Desktop/R_Projects/Reservoirs/Data/DataNotYetUploadedToEDI/Raw_CTD/csv_outputs/080520_ccr50.csv")

aug19 <- read_csv("C:/Users/dwh18/OneDrive/Desktop/R_Projects/Reservoirs/Data/DataNotYetUploadedToEDI/Raw_CTD/csv_outputs/081920_ccr50.csv")

ccr2020 <- rbind(july8, july15, july22, july29, aug05, aug19)
ccr2020$DOY <- strftime(ccr2020$Date, format = "%j") #create DOY column 
ccr2020$DOY <- as.double(ccr2020$DOY)


###getting heat maps 
#rename dataframe to ctd to fit code below 
ctd <- ccr2020

df.final<-data.frame()

ctd1<-ctd  %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 0.5)))
ctd2<-ctd  %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 1)))
ctd3<-ctd  %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 1.5)))
ctd4<-ctd  %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 2)))
ctd5<-ctd  %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 2.5)))
ctd6<-ctd  %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 3)))
ctd7<-ctd  %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 3.5)))
ctd8<-ctd  %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 4)))
ctd9<-ctd  %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 4.5)))
ctd10<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 5)))
ctd11<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 5.5)))
ctd12<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 6)))
ctd13<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 6.5)))
ctd14<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 7)))
ctd15<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 7.5)))
ctd16<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 8)))
ctd17<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 8.5)))
ctd18<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 9)))
ctd19<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 9.5)))
ctd20<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 10)))
ctd21<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 10.5)))
ctd22<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 11)))
ctd23<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 11.5)))
ctd24<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 12)))
ctd25<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 12.5)))
ctd26<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 13)))
ctd27<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 13.5)))
ctd28<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 14)))
ctd29<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 14.5)))
ctd30<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 15)))
ctd31<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 15.5)))
ctd32<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 16)))
ctd33<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 16.5)))
ctd34<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 17)))
ctd35<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 17.5)))
ctd36<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 18)))
ctd37<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 18.5)))
ctd38<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 19)))
ctd39<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 19.5)))
ctd40<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 20)))
ctd41<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 20.5)))
ctd42<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 21)))
ctd43<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 21.5)))


df.final = rbind(ctd1, ctd2,ctd3,ctd4,ctd5,ctd6,ctd7,ctd8,ctd9,ctd10,ctd11,ctd12,ctd13,ctd14,ctd15,ctd16,ctd17,ctd18,ctd19,
                 ctd20,ctd21,ctd22,ctd23,ctd24,ctd25,ctd26,ctd27,ctd28,ctd29,ctd30,ctd31,ctd32,ctd33,ctd34,ctd35,ctd36,ctd37,
                 ctd38,ctd39,ctd40,ctd41, ctd42, ctd43,
                 deparse.level = 1) # was 1

ctd <- arrange(df.final, Date)
#ctd$Depth_m <- round(as.numeric(ctd$Depth_m), digits = 1) 
ctd$Depth_m <-   round(ctd$Depth_m/0.5)*0.5  # new rounding function to ensure values get to nearest 0.5
ctd <- ctd[!duplicated(ctd),] #removed duplicated rows 




temp <- select(ctd, DOY, Depth_m, Temp_C) 
do <- select(ctd, DOY, Depth_m, DO_mgL)
chla <- select(ctd, DOY, Depth_m, Chla_ugL)




# rLakeAnalyzer for Thermocline Depths

# Pulling just temp, depth and date and going from long to wide. 
temp_RLA <- temp %>%
  select(Date, Depth_m, Temp_C)%>%
  #spread(Depth_m,Temp_C) #spread was retired and giving errors. Using suggested pivot wider now 
  pivot_wider(names_from = Depth_m, values_from = Temp_C, values_fn = list(Temp_C = mean))

# renaming the column names to include wtr_ 
# Otherwise, rLakeAnaylzer will not run!
colnames(temp_RLA)[-1] = paste0('wtr_',colnames(temp_RLA)[-1])

# rename the first column to "datetime"
names(temp_RLA)[1] <- "datetime"

# Calculate thermocline depth
FCR_thermo_18 <- ts.thermo.depth(temp_RLA, na.rm = TRUE)

#rename the datetime name back to Date
names(FCR_thermo_18)[1] <- "Date"

# Using dplyr, rejoin the DOY column from the temp dataframe to the thermocline depth dataframe. 
# this is a bit more ambiguous than it needs to be, but it works. 
FCR_thermo_18 <- left_join(FCR_thermo_18, temp, by = "Date")


# Complete data interpolation for the heatmaps
temp$Date <- as.Date(temp$Date)
do$Date <- as.Date(do$Date)
chla$Date <- as.Date(chla$Date)

#temperature
interp_temp <- interp(x=temp$Date, y = temp$Depth_m, z = temp$Temp_C,  #x was DOY
                      xo = seq(as.Date("2020-07-07"), as.Date("2020-08-19"), by = "day"), 
                      # xo = seq(min(temp$Date), max(temp$Date), by = "day"), 
                      yo = seq(0.1, 21.5, by = 0.01),                     #second seq value was 10.2
                      extrap = F, linear = T, duplicate = "strip")
interp_temp <- interp2xyz(interp_temp, data.frame=T)

datetemp <- interp_temp %>% 
  mutate(Realdate = as.Date(x, origin = "1970-01-01"))


#dissolved oxygen
interp_do <- interp(x=do$Date, y = do$Depth_m, z = do$DO_mgL,        #ditto to lines for temp 
                    xo = seq(as.Date("2020-07-07"), as.Date("2020-08-19"), by = "day"), ##was 2013-08-22 and 2019-06-28
                    yo = seq(0.1, 21.5, by = 0.01),
                    extrap = F, linear = T, duplicate = "strip")
interp_do <- interp2xyz(interp_do, data.frame=T)

interp_chla <- interp(x=chla$Date, y = chla$Depth_m, z = chla$Chla_ugL,
                      xo = seq(as.Date("2020-07-07"), as.Date("2020-08-19"), by = "day"), 
                      yo = seq(0.1, 21.5, by = 0.01),
                      extrap = F, linear = T, duplicate = "strip")
interp_chla <- interp2xyz(interp_chla, data.frame=T)




# Plotting #

# This a theme I have adapted from 
#https://gist.github.com/jslefche/eff85ef06b4705e6efbc
# I LIKE IT!
theme_black = function(base_size = 12, base_family = "") {
  
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    
    theme(
      # Specify axis options
      axis.line = element_line(size = 1, colour = "white"),  
      axis.text.x = element_text(size = base_size*1, color = "white", lineheight = 0.9),  
      axis.text.y = element_text(size = base_size*1, color = "white", lineheight = 0.9),  
      axis.ticks = element_line(color = "white", size  =  1),  
      axis.title.x = element_text(size = base_size, color = "white", margin = margin(0, 10, 0, 0)),  
      axis.title.y = element_text(size = base_size, color = "white", angle = 90, margin = margin(0, 10, 0, 0)),  
      axis.ticks.length = unit(0.5, "lines"),   
      # Specify legend options
      legend.background = element_rect(color = NA, fill = "black"),  
      legend.key = element_rect(color = "white",  fill = "black"),  
      legend.key.size = unit(2, "lines"),  
      legend.key.height = NULL,  
      legend.key.width = NULL,      
      legend.text = element_text(size = base_size*0.8, color = "white"),  
      legend.title = element_text(size = base_size*1.5, face = "bold", hjust = 0, color = "white"),  
      legend.position = "right",  
      legend.text.align = NULL,  
      legend.title.align = NULL,  
      legend.direction = "vertical",  
      legend.box = NULL, 
      # Specify panel options
      panel.background = element_rect(fill = "black", color  =  NA),  
      panel.border = element_rect(fill = NA, color = "black"),  
      panel.grid.major = element_line(color = "black"),  
      panel.grid.minor = element_line(color = "black"),  
      panel.spacing = unit(0, "lines"),   #chagned to panel.spacing from panel.margin in orginal code
      # Specify facetting options
      strip.background = element_rect(fill = "grey30", color = "grey10"),  
      strip.text.x = element_text(size = base_size*0.8, color = "white"),  
      strip.text.y = element_text(size = base_size*0.8, color = "white",angle = -90),  
      # Specify plot options
      plot.background = element_rect(color = "black", fill = "black"),  
      plot.title = element_text(size = base_size*1.5, color = "white"),  
      plot.margin = unit(rep(1, 4), "lines")
      
    )
  
}


#making dates as.Date so they plot 
FCR_thermo_18$Date <- as.Date(FCR_thermo_18$Date)
ctd$Date <- as.Date(ctd$Date)

# Create a pdf so the plots can all be saved in one giant bin!
pdf("./CCR_plots/Heatmaps/2020_CCR_heatmaps/CCR_CTD_2020_to19aug.pdf", width=30, height=10)  

#temperature
p1 <- ggplot(interp_temp, aes(x= as.Date(x, origin = "1970-01-01"), y=y))+
  geom_raster(aes(fill=z))+
  scale_y_reverse()+
  #geom_point(data = ctd, aes(x=DOY, y=Flag, z=NULL), pch = 25, size = 1.5, color = "white", fill = "black")+
  geom_point(data = ctd, aes(x = Date, y = 0.1, z = NULL), pch = 25, size = 3, color = "white", fill = "black")+ #to mark cast dates 
  geom_line(data = FCR_thermo_18, aes(x=Date, y=thermo.depth, z=NULL), color = "black", lwd = 1)+
  geom_point(data = FCR_thermo_18, aes(x=Date, y=thermo.depth, z=NULL), pch = 21, size = 2, color = "white", fill = "black")+
  scale_fill_gradientn(colours = blue2green2red(60), na.value="gray")+
  labs(x = "Date", y = "Depth (m)", title = "CCR 2020 Temperature Heatmap",fill=expression(''*~degree*C*''))+ #x was day of year
  theme_black()

#dissolved oxygen
p5 <- ggplot(interp_do, aes(x= as.Date(x, origin = "1970-01-01"), y=y))+
  geom_raster(aes(fill=z))+
  scale_y_reverse()+
  geom_point(data = ctd, aes(x = Date, y = 0.1, z = NULL), pch = 25, size = 3, color = "white", fill = "black")+ #to mark cast dates 
  geom_line(data = FCR_thermo_18, aes(x=Date, y=thermo.depth, z=NULL), color = "black", lwd = 1)+
  geom_point(data = FCR_thermo_18, aes(x=Date, y=thermo.depth, z=NULL), pch = 21, size = 2, color = "white", fill = "black")+  
  scale_fill_gradientn(colours = rev(blue2green2red(60)), na.value="gray")+
  labs(x = "Date", y = "Depth (m)", title = "CCR 2020 Dissolved Oxygen Heatmap", fill="mg/L")+ #x was day of year 
  theme_black()

#chla 
p2 <- ggplot(interp_chla, aes(x= as.Date(x, origin = "1970-01-01"), y=y))+
  geom_raster(aes(fill=z))+
  scale_y_reverse()+
  geom_point(data = ctd, aes(x = Date, y = 0.1, z = NULL), pch = 25, size = 3, color = "white", fill = "black")+ #to mark cast dates 
  geom_line(data = FCR_thermo_18, aes(x=Date, y=thermo.depth, z=NULL), color = "black", lwd = 1)+
  geom_point(data = FCR_thermo_18, aes(x=Date, y=thermo.depth, z=NULL), pch = 21, size = 2, color = "white", fill = "black")+  
  scale_fill_gradientn(colours = rev(blue2green2red(60)), na.value="gray")+
  labs(x = "Day of year", y = "Depth (m)", title = "CCR 2020 CHLA Heatmap", fill=expression(paste("", mu, "g/L")))+
  theme_black()


# create a grid that stacks all the heatmaps together. 
grid.newpage()
grid.draw(cbind(ggplotGrob(p1),  #would change to cbind for side to side  
                ggplotGrob(p5),
                ggplotGrob(p2),
                size = "first"))
#ggplotGrob(p2), ggplotGrob(p3), ggplotGrob(p4), ggplotGrob(p5), ggplotGrob(p6), ggplotGrob(p7),ggplotGrob(p8),
# size = "first"))
# end the make-pdf function. 
dev.off()




#### Flora plot #### 
#Need to fix this eventually, this is just quick data wrangling and plotting for one date 

data <- read.delim("C:/Users/dwh18/OneDrive/Desktop/R_Projects/Reservoirs/Data/DataNotYetUploadedToEDI/Raw_fluoroprobe/08192020_ccr_50.txt")
view(data)

flora <- data %>% 
  select(Date.Time, Total.conc., Depth) %>% 
  rename("Date" = Date.Time,
         "Total_conc_Phytos" = Total.conc.,
         "Depth_m" = Depth) %>% 
  slice(-1) %>% 
  mutate(Total_conc_Phytos = as.numeric(Total_conc_Phytos),
         Depth_m = as.numeric(Depth_m))

mytheme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                 panel.background = element_blank(), axis.line = element_line(colour = "black"),
                 legend.key = element_blank(),legend.background = element_blank(),
                 axis.text=element_text(size=16),
                 axis.title=element_text(size=16,face="bold"))

floraplot <- ggplot(data = flora, mapping = aes(x = Total_conc_Phytos, y = Depth_m))+
  #geom_point(color = "green", size = 1)+
  geom_line(orientation = "y", color = "green", size = 1)+
  labs(x = "Phytos (ug/L)", y = "Depth (m)")+
  scale_y_continuous(trans = "reverse")+
  scale_x_continuous(breaks = c(1:10))+
  ggtitle("CCR Flora casts 19 August 2020")+
  mytheme

floraplot

ggsave(filename = "./CCR_plots/Heatmaps/Flora_plots/CCR_Flora_19aug2020.tiff", 
       floraplot, device = "tiff", width = 230, height = 140, units = "mm")





