## converting wind data from u and v at King Salmon and Beluga to wind velocity and direction
library(tidyverse)
library(raster)
## datasets are already in R 

View(BELUGA_wind_final)
View(KS_wind_final)

## first we add a column for wind speed, which is the square root of u squared plus v squared

##BELUGA
BELUGA_ws<-BELUGA_wind_final %>% 
  BELUGA_ws$u<-as.numeric(BELUGA_ws$u) %>% 
  BELUGA_ws$v<-as.numeric(BELUGA_ws$v) %>% 
  mutate(ws=sqrt(as.numeric(BELUGA_wind_final$u)^2 + as.numeric(BELUGA_wind_final$v)^2)) %>% 
  mutate(wind_dir_trig_to = atan2((BELUGA_ws$u/BELUGA_ws$ws), (BELUGA_ws$v/BELUGA_ws$ws))) %>% 
  mutate(wind_dir_trig_to_degrees = wind_dir_trig_to * 180/pi+180) %>% 
  ## next need to add a column that calculates wind profit, given the bearing birds want to leave

##KS
KS_ws<-KS_wind_final %>% 
  mutate(ws=sqrt(as.numeric(KS_wind_final$u)^2 + as.numeric(KS_wind_final$v)^2)) %>% 
  mutate(wind_dir_trig_to = atan2((KS_ws$u/KS_ws$ws), (KS_ws$v/KS_ws$ws))) %>% 
  mutate(wind_dir_trig_to_degrees = wind_dir_trig_to * 180/pi+180)


  mutate(KSwind_dir_trig_to_degrees = wind_dir_trig_to * 180/pi+180)




#%>% 
  mutate(wind_dir_trig_to = atan2((KS_ws$u/KS_ws$ws), (KS_ws$v/KS_ws$ws))) #%>% 
  mutate(wind_dir_trig_to_degrees = wind_dir_trig_to * 180/pi+180)
  
## next need to add a column that calculates wind power, given the bearing birds want to leave

## next add a column of 


BELUGA_ws$u<-as.numeric(BELUGA_ws$u)
BELUGA_ws$v<-as.numeric(BELUGA_ws$v)
summary(KS_ws)
class(BELUGA_ws$wind_dir_trig_to)



KS_wind_final$u<-as.numeric(KS_wind_final$u)
KS_wind_final$v<-as.numeric(KS_wind_final$v)
KS_ws$ws<-as.numeric(KS_ws$ws)

summary(KS_ws)
