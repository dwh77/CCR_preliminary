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


#test chaning high suspect inflow C value to 0 
flow_test <- flow
flow_test[61, 7] #veloctiy was 0.49 m/s here when sensor was barely submerged 
flow_test[61, 7] <- 0 #set this value to 0 
#when this value was 0.49 m/s discharged was 0.007 cms, when this was set to 0 discharge was 0.004

# lastly calculate discharge for each interval
flow$Discharge <- flow$Depth_m * flow$Velocity_ms * flow$WidthInterval_m
head(flow)
flow_test$Discharge <- flow_test$Depth_m * flow_test$Velocity_ms * flow_test$WidthInterval_m
head(flow_test)


# now sum by site and date to get the total discharge for that day/site
flowA <-  flow %>% 
  group_by(Site, Date) %>% 
  mutate(Discharge_m3_s = sum(Discharge))


# now subset out only the unique discharge measurements
discharge <- flowA %>%
  select(Date, Site, Discharge_m3_s, Flowmate_ID, Notes)

discharge_test <- flow_test %>% 
  group_by(Site, Date) %>% 
  mutate(Discharge_m3_s = sum(Discharge)) %>% 
  select(Date, Site, Discharge_m3_s, Flowmate_ID, Notes)

test <- discharge_test[61,]


dischargeA <- discharge[!duplicated(discharge[1:3]),]
dischargeB <- discharge[!duplicated(discharge),]


fin <- rbind(dischargeA, test)

#write csv, location TBD 
write.csv(fin, './Inflow/Discharge_Data.csv', row.names = FALSE)


