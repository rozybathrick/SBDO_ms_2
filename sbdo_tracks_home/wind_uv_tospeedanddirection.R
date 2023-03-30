## converting wind data from u and v at King Salmon and Beluga to wind velocity and direction
library(tidyverse)
library(raster)
## datasets are already in R. make sure all the data class is numeric

View(BELUGA_wind_final)
summary(BELUGA_wind_final)

BELUGA_wind_final <- type.convert(BELUGA_wind_final, as.is = TRUE) 
summary(BELUGA_ws)

BELUGA_ws<-type.convert(BELUGA_ws, as.is=TRUE)

KS_wind_final<- type.convert(KS_wind_final, as.is = TRUE) 
KS_ws<-type.convert(KS_ws, as.is = TRUE) 
summary(KS_wind_final)
summary(KS_ws)

View(KS_wind_final)

## first we add a column for wind speed, which is the square root of u squared plus v squared
##then we calculate the wind direction in degrees

##BELUGA
BELUGA_ws<-BELUGA_wind_final %>% 
  mutate(ws=sqrt(as.numeric(BELUGA_wind_final$u)^2 + as.numeric(BELUGA_wind_final$v)^2)) %>% 
  mutate(wind_dir_trig_to = atan2((BELUGA_ws$u/BELUGA_ws$ws), (BELUGA_ws$v/BELUGA_ws$ws))) %>% 
  mutate(wind_dir_trig_to_degrees = wind_dir_trig_to * 180/pi+180)
## next need to add a column that calculates wind profit, given the bearing birds want to leave: look at which birdswe have good first flight for


##KS
KS_ws<-KS_wind_final %>% 
  mutate(ws=sqrt(as.numeric(KS_wind_final$u)^2 + as.numeric(KS_wind_final$v)^2)) %>% 
  mutate(wind_dir_trig_to = atan2((KS_ws$u/KS_ws$ws), (KS_ws$v/KS_ws$ws))) %>% 
  mutate(wind_dir_trig_to_degrees = wind_dir_trig_to * 180/pi+180)
## next need to add a column that calculates wind profit, given the bearing birds want to leave

## 116 degrees between KS and Arcata


