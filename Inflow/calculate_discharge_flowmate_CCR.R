# script to calculate discharge from digitized stream width/depth/velocity measurements
#install.packages("tidyverse")
library(tidyverse)

flow <- read_csv("./Inflow/CCR_inflow_flowmate_data.csv") # the location of the discharge_digitized.csv, should come from github
flow$Date <- as.Date(flow$Date)
head(flow)

# first convert the depth to m in a new column (it is always measured in cm in the field)
flow$Depth_m <- flow$Depth_cm/100
head(flow)

#skip next three for now since flowmate is measuring in m/s currently 
  # now convert the velocity to m/s (the flowmeter measures in ft/s)
  #flow$Velocity <- as.numeric(flow$Velocity)
  #flow$Velocity_m.s <- ifelse(flow$Velocity_unit=="ft_s", flow$Velocity*0.3048, flow$Velocity)


# lastly calculate discharge for each interval
flow$Discharge <- flow$Depth_m * flow$Velocity_ms * flow$WidthInterval_m
head(flow)


# now sum by site and date to get the total discharge for that day/site
flowA <-  flow %>% 
  group_by(Site, Date) %>% 
  mutate(Discharge_m3_s = sum(Discharge))

# now subset out only the unique discharge measurements
discharge <- flowA %>%
  select(Date, Site, Discharge_m3_s, Flowmate_ID, Notes)

dischargeA <- discharge[!duplicated(discharge[1:3]),]
dischargeB <- discharge[!duplicated(discharge),]

flowmate_fin <- dischargeA

#### Volumetric flow conversions #### 
#Discharge per site in L/s was calculated in excel sheets before had: file name CCR_VolumetricFlow_discharge_data.xlsx
# xlsx is then saved as a csv when updated so csv can be read in 

volflow <- read_csv("./Inflow/CCR_VolumetricFlow_discharge_data_25aug20.csv")

volflowA <- volflow[c(1,5),]

volflowB <- volflowA %>% 
  mutate(Discharge_m3_s = Discharge_Final_LperSec * 0.001) %>% 
  mutate(Flowmate_ID = "Volumetric_manual") %>% 
  select(Date, Site, Discharge_m3_s, Flowmate_ID, Notes)

volflowB[2,2] <- "TCT"  #changing name from TCT_1 to TCT so that naming convention is same. Was TCT_1 since that row was where averaging and suming for discharge occured in xlsx file

volumetric <- volflowB #renaming to match joining code 


####bind flowmate and volumetric flow together ####

fin <- rbind(flowmate_fin, volumetric)

#write csv, location TBD 
write.csv(fin, './Inflow/Discharge_Data.csv', row.names = FALSE)


