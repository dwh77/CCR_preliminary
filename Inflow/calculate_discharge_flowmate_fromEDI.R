# script to calculate discharge from digitized stream width/depth/velocity measurements
#install.packages("tidyverse")
library(tidyverse)

flow <- read_csv("./Inflow/2019_Discharge_Flowmate_test.csv") # the location of the discharge_digitized.csv, should come from github
flow$Date <- as.Date(flow$Date)
head(flow)

# first convert the depth to m in a new column (it is always measured in cm in the field)
flow$Depth_m <- flow$Depth_cm/100

# now convert the velocity to m/s (the flowmeter measures in ft/s)
#flow$Velocity <- as.numeric(flow$Velocity)
flow$Velocity_m.s <- ifelse(flow$Velocity_unit=="ft_s", flow$Velocity*0.3048, flow$Velocity)


# lastly calculate discharge for each interval
flow$Discharge <- flow$Depth_m * flow$Velocity_m.s * flow$WidthInterval_m

# now sum by site and date to get the total discharge for that day/site
flowA <-  flow %>% 
  group_by(Site, Date) %>% 
  mutate(Discharge_m3_s = sum(Discharge))


# now subset out only the unique discharge measurements
discharge <- flowA %>%
  select(Date, Site, Discharge_m3_s, FlowmeterSensorID, Notes)

dischargeA <- discharge[!duplicated(discharge[1:3]),]
dischargeB <- discharge[!duplicated(discharge),]




wetland <- discharge[discharge$Site=='F200',]
plot(wetland$Date, wetland$Discharge_m3_s)

write.csv(wetland, './Data/DataNotYetUploadedToEDI/Raw_inflow/Wetland_Discharge_Data.csv', row.names = FALSE)
