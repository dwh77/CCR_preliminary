# BVR CTD Heatmaps
# Author: Ryan McClure
# Date last updated: 080718
##Modified 3 August 2020 by DWH for SHR plots 

# Makes heatmaps and filter CTD data in Spring Hollow reservoir

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


ctda <- read_csv("C:/Users/dwh18/Downloads/CTD_final_2013_2019.csv", col_types = cols(.default = "d", Reservoir = "c", Date = "T"))
head(ctda)  


shr <- ctda %>% 
  filter(Reservoir == "SHR") %>% 
  select(Reservoir, Site, Date, Depth_m, Temp_C, DO_mgL, Chla_ugL) %>% 
  mutate(Date = as.Date(Date),
         Year = format(as.Date(Date), "%Y"),
         DOY = yday(Date)) 

summary(shr)

shrwvwa <- ctda %>% 
  filter(Reservoir == "SHR") %>% 
  select(-c(14:24)) 

write.csv(shrwvwa, "./SHR/SHR_CTD_data_2013_2019.csv")


#Plot for CCR 13 since only one cast 
ccr13 <-ccr %>% 
  filter(Year == 2013)


###getting heat maps 
#rename dataframe to ctd to fit code below 

ctd <- shr

df.final<-data.frame()

ctd1<-ctd  %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 0.5)))
ctd2<-ctd  %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 1)))
ctd3<-ctd  %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 2)))
ctd4<-ctd  %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 3)))
ctd5<-ctd  %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 4)))
ctd6<-ctd  %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 5)))
ctd7<-ctd  %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 6)))
ctd8<-ctd  %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 7)))
ctd9<-ctd  %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 8)))
ctd10<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 9)))
ctd11<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 9)))
ctd12<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 10)))
ctd13<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 11)))
ctd14<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 12)))
ctd15<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 13)))
ctd16<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 14)))
ctd17<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 15)))
ctd18<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 16)))
ctd19<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 17)))
ctd20<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 18)))
ctd21<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 19)))
ctd22<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 20)))
ctd23<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 21)))
ctd24<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 22)))
ctd25<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 23)))
ctd26<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 24)))
ctd27<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 25)))
ctd28<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 26)))
ctd29<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 27)))
ctd30<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 28)))
ctd31<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 29)))
ctd32<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 30)))
ctd33<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 31)))
ctd34<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 32)))
ctd35<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 33)))
ctd36<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 34)))
ctd37<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 35)))
ctd38<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 36)))
ctd39<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 37)))
ctd40<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 38)))
ctd41<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 39)))
ctd42<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 40)))
ctd43<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 41)))
ctd44<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 42)))
ctd45<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 43)))
ctd46<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 44)))
ctd47<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 45)))
ctd48<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 46)))
ctd49<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 47)))
ctd50<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 48)))
ctd51<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 49)))
ctd52<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 50)))
ctd53<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 51)))
ctd54<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 52)))
ctd55<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 53)))
ctd56<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 54)))
ctd57<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 55)))
ctd58<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 56)))
ctd59<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 57)))
ctd60<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 58)))
ctd61<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 59)))
ctd62<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 60)))
ctd63<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 61)))
ctd64<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 62)))
ctd65<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 63)))
ctd66<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 64)))
ctd67<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 65)))
ctd68<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 66)))
ctd69<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 67)))
ctd70<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 67.5)))






df.final = rbind(ctd1, ctd2,ctd3,ctd4,ctd5,ctd6,ctd7,ctd8,ctd9,ctd10,ctd11,ctd12,ctd13,ctd14,ctd15,ctd16,ctd17,ctd18,ctd19,
                 ctd20,ctd21,ctd22,ctd23,ctd24,ctd25,ctd26,ctd27,ctd28,ctd29,ctd30,ctd31,ctd32,ctd33,ctd34,ctd35,ctd36,ctd37,
                 ctd38,ctd39,ctd40,ctd41, ctd42, ctd43,ctd44, ctd45, ctd46, ctd47, ctd48, ctd49, ctd50, ctd51, ctd52, ctd53, ctd54,
                 ctd55, ctd56, ctd57, ctd58, ctd59, ctd60, ctd61, ctd62, ctd63, ctd64, ctd65, ctd66, ctd67, ctd68, ctd69, ctd70,
                                  deparse.level = 1) # was 1

ctd <- arrange(df.final, Date)
#ctd$Depth_m <- round(as.numeric(ctd$Depth_m), digits = 1)
ctd$Depth_m <-   round(ctd$Depth_m/0.5)*0.5  # new rounding function to ensure values get to nearest 0.5 
ctd <- ctd[!duplicated(ctd),] #removed duplicated rows 



temp <- select(ctd, DOY, Depth_m, Temp_C) 
do <- select(ctd, DOY, Depth_m, DO_mgL)
chla <- select(ctd, DOY, Depth_m, Chla_ugL)
# turb <- select(ctd, DOY, Depth_m, Turb_NTU)
# cond <- select(ctd, DOY, Depth_m, Cond_uScm)
# spccond <- select(ctd, DOY, Depth_m, Spec_Cond_uScm)
# psat <- select(ctd, DOY, Depth_m, DO_pSat)
# ph <- select(ctd, DOY, Depth_m, pH)
# orp <- select(ctd, DOY, Depth_m, ORP)
# par <- select(ctd, DOY, Depth_m, PAR)
# sal <- select(ctd, DOY, Depth_m, Salinity)
# desc <- select(ctd, DOY, Depth_m, `Descent Rate (m/s)`)

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
head(temp)
temp$Date <- as.POSIXct(temp$Date)
head(temp)

#temperature
interp_temp <- interp(x=temp$Date, y = temp$Depth_m, z = temp$Temp_C,  #x was DOY
                      xo = seq(as.Date("2013-08-22"), as.Date("2019-06-28"), by = "day"), 
                      # xo = seq(min(temp$Date), max(temp$Date), by = "day"), 
                      yo = seq(0.1, 67.5, by = 0.01),                     #second seq value was 10.2
                      extrap = F, linear = T, duplicate = "strip")
interp_temp <- interp2xyz(interp_temp, data.frame=T)

# datetemp <- interp_temp %>% 
#   mutate(Realdate = as.Date(x, origin = "1970-01-01"))


#dissolved oxygen
interp_do <- interp(x=do$Date, y = do$Depth_m, z = do$DO_mgL,        #ditto to lines for temp 
                    xo = seq(as.Date("2013-08-22"), as.Date("2019-06-28"), by = "day"), 
                    yo = seq(0.1, 67.5, by = 0.01),
                    extrap = F, linear = T, duplicate = "strip")
interp_do <- interp2xyz(interp_do, data.frame=T)


chla <- chla %>% 
  filter(!is.na(Chla_ugL))

interp_chla <- interp(x=chla$Date, y = chla$Depth_m, z = chla$Chla_ugL,        #ditto to lines for temp 
                    xo = seq(as.Date("2013-08-22"), as.Date("2019-06-28"), by = "day"), 
                    yo = seq(0.1, 67.5, by = 0.01),
                    extrap = F, linear = T, duplicate = "strip")
interp_chla <- interp2xyz(interp_chla, data.frame=T)


#Other variables from CTD not using now 
{
  #chlorophyll a
  interp_chla <- interp(x=chla$DOY, y = chla$Depth_m, z = chla$Chla_ugL,
                        xo = seq(min(chla$DOY), max(chla$DOY), by = .1), 
                        yo = seq(0.1, 10.2, by = 0.01),
                        extrap = F, linear = T, duplicate = "strip")
  interp_chla <- interp2xyz(interp_chla, data.frame=T)
  
  #turbidity
  interp_turb <- interp(x=turb$DOY, y = turb$Depth_m, z = turb$Turb_NTU,
                        xo = seq(min(turb$DOY), max(turb$DOY), by = .1), 
                        yo = seq(0.1, 10.2, by = 0.01),
                        extrap = F, linear = T, duplicate = "strip")
  interp_turb <- interp2xyz(interp_turb, data.frame=T)
  
  #conductivity
  interp_cond <- interp(x=cond$DOY, y = cond$Depth_m, z = cond$Cond_uScm,
                        xo = seq(min(cond$DOY), max(cond$DOY), by = .1), 
                        yo = seq(0.1, 10.2, by = 0.01),
                        extrap = F, linear = T, duplicate = "strip")
  interp_cond <- interp2xyz(interp_cond, data.frame=T)
  
  #specific conductivity
  interp_spccond <- interp(x=spccond$DOY, y = spccond$Depth_m, z = spccond$Spec_Cond_uScm,
                           xo = seq(min(spccond$DOY), max(spccond$DOY), by = .1), 
                           yo = seq(0.1, 10.2, by = 0.01),
                           extrap = F, linear = T, duplicate = "strip")
  interp_spccond <- interp2xyz(interp_spccond, data.frame=T)
  
  #percent saturation
  interp_psat <- interp(x=psat$DOY, y = psat$Depth_m, z = psat$DO_pSat,
                        xo = seq(min(psat$DOY), max(psat$DOY), by = .1), 
                        yo = seq(0.1, 10.2, by = 0.01),
                        extrap = F, linear = T, duplicate = "strip")
  interp_psat <- interp2xyz(interp_psat, data.frame=T)
  
  #pH
  interp_ph <- interp(x=ph$DOY, y = ph$Depth_m, z = ph$pH,
                      xo = seq(min(ph$DOY), max(ph$DOY), by = .1), 
                      yo = seq(0.1, 10.2, by = 0.01),
                      extrap = F, linear = T, duplicate = "strip")
  interp_ph <- interp2xyz(interp_ph, data.frame=T)
  
  #Oxidation reduction pottential
  interp_orp <- interp(x=orp$DOY, y = orp$Depth_m, z = orp$ORP,
                       xo = seq(min(orp$DOY), max(orp$DOY), by = .1), 
                       yo = seq(0.1, 10.2, by = 0.01),
                       extrap = F, linear = T, duplicate = "strip")
  interp_orp <- interp2xyz(interp_orp, data.frame=T)
  
  #photosynthetic active radiation
  interp_par <- interp(x=par$DOY, y = par$Depth_m, z = par$PAR,
                       xo = seq(min(par$DOY), max(par$DOY), by = .1), 
                       yo = seq(0.1, 10.2, by = 0.01),
                       extrap = F, linear = T, duplicate = "strip")
  interp_par <- interp2xyz(interp_par, data.frame=T)
  
  #salinity
  interp_sal <- interp(x=sal$DOY, y = sal$Depth_m, z = sal$Salinity,
                       xo = seq(min(sal$DOY), max(sal$DOY), by = .1), 
                       yo = seq(0.1, 10.2, by = 0.01),
                       extrap = F, linear = T, duplicate = "strip")
  interp_sal <- interp2xyz(interp_sal, data.frame=T)
  
  #descent rate
  interp_desc <- interp(x=desc$DOY, y = desc$Depth_m, z = desc$`Descent Rate (m/s)`,
                        xo = seq(min(desc$DOY), max(desc$DOY), by = .1), 
                        yo = seq(0.1, 10.2, by = 0.01),
                        extrap = F, linear = T, duplicate = "strip")
  interp_desc <- interp2xyz(interp_desc, data.frame=T)
}


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

# Create a pdf so the plots can all be saved in one giant bin!
pdf("./SHR/SHR_CTD_2013_2019_test.pdf", width=10, height=30)  

#temperature
p1 <- ggplot(interp_temp, aes(x= as.Date(x, origin = "1970-01-01"), y=y))+
  geom_raster(aes(fill=z))+
  scale_y_reverse()+
  #geom_point(data = ctd, aes(x=DOY, y=Flag, z=NULL), pch = 25, size = 1.5, color = "white", fill = "black")+
  geom_point(data = ctd, aes(x = Date, y = 0.1, z = NULL), pch = 25, size = 3, color = "white", fill = "black")+ #to mark cast dates 
  # geom_line(data = FCR_thermo_18, aes(x=Date, y=thermo.depth, z=NULL), color = "black", lwd = 1)+
  #geom_point(data = FCR_thermo_18, aes(x=Date, y=thermo.depth, z=NULL), pch = 21, size = 2, color = "white", fill = "black")+
  scale_fill_gradientn(colours = blue2green2red(60), na.value="gray")+
  labs(x = "Date", y = "Depth (m)", title = "SHR Temperature Heatmap",fill=expression(''*~degree*C*''))+ #x was day of year
  theme_black()

#dissolved oxygen
p5 <- ggplot(interp_do, aes(x= as.Date(x, origin = "1970-01-01"), y=y))+
  geom_raster(aes(fill=z))+
  scale_y_reverse()+
  geom_point(data = ctd, aes(x = Date, y = 0.1, z = NULL), pch = 25, size = 3, color = "white", fill = "black")+ #to mark cast dates 
  #geom_point(data = ctd, aes(x=DOY, y=Flag, z=NULL), pch = 25, size = 1.5, color = "white", fill = "black")+
  #geom_line(data = FCR_thermo_18, aes(x=DOY, y=thermo.depth, z=NULL), color = "black", lwd = 1)+
  #geom_point(data = FCR_thermo_18, aes(x=DOY, y=thermo.depth, z=NULL), pch = 21, size = 2, color = "white", fill = "black")+  
  scale_fill_gradientn(colours = rev(blue2green2red(60)), na.value="gray")+
  labs(x = "Date", y = "Depth (m)", title = "SHR Dissolved Oxygen Heatmap", fill="mg/L")+ #x was day of year 
  theme_black()

#chlorophyll a
p2 <- ggplot(interp_chla, aes(x= as.Date(x, origin = "1970-01-01"), y=y))+
  geom_raster(aes(fill=z))+
  scale_y_reverse()+
  geom_point(data = ctd, aes(x=Date, y=0.1, z=NULL), pch = 25, size = 1.5, color = "white", fill = "black")+
  # geom_line(data = FCR_thermo_18, aes(x=DOY, y=thermo.depth, z=NULL), color = "black", lwd = 1)+
  # geom_point(data = FCR_thermo_18, aes(x=DOY, y=thermo.depth, z=NULL), pch = 21, size = 2, color = "white", fill = "black")+  
  scale_fill_gradientn(colours = blue2green2red(60), na.value="gray")+
  labs(x = "Day of year", y = "Depth (m)", title = "FCR CHLA Heatmap", fill=expression(paste("", mu, "g/L")))+
  theme_black()

#other variables 
{
  
  #turbidity
  p3 <- ggplot(interp_turb, aes(x=x, y=y))+
    geom_raster(aes(fill=z))+
    scale_y_reverse()+
    geom_point(data = ctd, aes(x=DOY, y=Flag, z=NULL), pch = 25, size = 1.5, color = "white", fill = "black")+
    geom_line(data = FCR_thermo_18, aes(x=DOY, y=thermo.depth, z=NULL), color = "black", lwd = 1)+
    geom_point(data = FCR_thermo_18, aes(x=DOY, y=thermo.depth, z=NULL), pch = 21, size = 2, color = "white", fill = "black")+  
    scale_fill_gradientn(colours = blue2green2red(60), na.value="gray")+
    labs(x = "Day of year", y = "Depth (m)", title = "FCR Turbidity Heatmap", fill="NTU")+
    theme_black()
  
  #specific conductivity
  p4 <- ggplot(interp_spccond, aes(x=x, y=y))+
    geom_raster(aes(fill=z))+
    scale_y_reverse()+
    geom_point(data = ctd, aes(x=DOY, y=Flag, z=NULL), pch = 25, size = 1.5, color = "white", fill = "black")+
    geom_line(data = FCR_thermo_18, aes(x=DOY, y=thermo.depth, z=NULL), color = "black", lwd = 1)+
    geom_point(data = FCR_thermo_18, aes(x=DOY, y=thermo.depth, z=NULL), pch = 21, size = 2, color = "white", fill = "black")+  
    scale_fill_gradientn(colours = blue2green2red(60), na.value="gray")+
    labs(x = "Day of year", y = "Depth (m)", title = "FCR Specific Conductivity Heatmap",fill=expression(paste("", mu, "S/cm")))+
    theme_black()
  
  #dissolved oxygen
  p5 <- ggplot(interp_do, aes(x=x, y=y))+
    geom_raster(aes(fill=z))+
    scale_y_reverse()+
    geom_point(data = ctd, aes(x=DOY, y=Flag, z=NULL), pch = 25, size = 1.5, color = "white", fill = "black")+
    geom_line(data = FCR_thermo_18, aes(x=DOY, y=thermo.depth, z=NULL), color = "black", lwd = 1)+
    geom_point(data = FCR_thermo_18, aes(x=DOY, y=thermo.depth, z=NULL), pch = 21, size = 2, color = "white", fill = "black")+  
    scale_fill_gradientn(colours = rev(blue2green2red(60)), na.value="gray")+
    labs(x = "Day of year", y = "Depth (m)", title = "FCR Dissolved Oxygen Heatmap", fill="mg/L")+
    theme_black()
  
  #pH
  p6 <- ggplot(interp_ph, aes(x=x, y=y))+
    geom_raster(aes(fill=z))+
    scale_y_reverse()+
    geom_point(data = ctd, aes(x=DOY, y=Flag, z=NULL), pch = 25, size = 1.5, color = "white", fill = "black")+
    geom_line(data = FCR_thermo_18, aes(x=DOY, y=thermo.depth, z=NULL), color = "black", lwd = 1)+
    geom_point(data = FCR_thermo_18, aes(x=DOY, y=thermo.depth, z=NULL), pch = 21, size = 2, color = "white", fill = "black")+  
    scale_fill_gradientn(colours = rev(blue2green2red(60)), na.value="gray")+
    labs(x = "Day of year", y = "Depth (m)", title = "FCR pH Heatmap", fill="pH")+
    theme_black()
  
  #oxidation reduction potential
  p7 <- ggplot(interp_orp, aes(x=x, y=y))+
    geom_raster(aes(fill=z))+
    scale_y_reverse()+
    geom_point(data = ctd, aes(x=DOY, y=Flag, z=NULL), pch = 25, size = 1.5, color = "white", fill = "black")+
    geom_line(data = FCR_thermo_18, aes(x=DOY, y=thermo.depth, z=NULL), color = "black", lwd = 1)+
    geom_point(data = FCR_thermo_18, aes(x=DOY, y=thermo.depth, z=NULL), pch = 21, size = 2, color = "white", fill = "black")+  
    scale_fill_gradientn(colours = rev(blue2green2red(60)), na.value="gray")+
    labs(x = "Day of year", y = "Depth (m)", title = "FCR ORP Heatmap", fill="mV")+
    theme_black()
  
  #descent rate
  p8 <- ggplot(interp_desc, aes(x=x, y=y))+
    geom_raster(aes(fill=z))+
    scale_y_reverse()+
    geom_point(data = ctd, aes(x=DOY, y=Flag, z=NULL), pch = 25, size = 1.5, color = "white", fill = "black")+
    scale_fill_gradientn(colours = rev(blue2green2red(60)), na.value="gray")+
    labs(x = "Day of year", y = "Depth (m)", title = "FCR Descent Rate Heatmap", fill = "m/s")+
    theme_black()
  }

# create a grid that stacks all the heatmaps together. 
grid.newpage()
grid.draw(rbind(ggplotGrob(p1),  #would change to cbind for side to side  
                ggplotGrob(p5),
                ggplotGrob(p2),
                size = "first"))
#ggplotGrob(p2), ggplotGrob(p3), ggplotGrob(p4), ggplotGrob(p5), ggplotGrob(p6), ggplotGrob(p7),ggplotGrob(p8),
# size = "first"))
# end the make-pdf function. 
dev.off()


