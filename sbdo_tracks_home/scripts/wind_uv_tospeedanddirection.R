## converting wind data from u and v at King Salmon and Beluga to wind velocity and direction
library(tidyverse)
library(raster)
install.packages("openair")
library(openair)
library(lubridate)

## datasets are already in R. make sure all the data class is numeric

View(BELUGA_wind_final)
summary(BELUGA_wind_final)

BELUGA_wind_final <- type.convert(BELUGA_wind_final, as.is = TRUE) %>% 
  as.Date(BELUGA_wind_final$Timestamp)

summary(BELUGA_ws)
summary(KS_ws)

BELUGA_wind_final$Timestamp<-as.POSIXct(BELUGA_wind_final$Timestamp, format="%Y-%m-%d")

BELUGA_ws<-type.convert(BELUGA_ws, as.is=TRUE)

KS_wind_final<- type.convert(KS_wind_final, as.is = TRUE)
KS_ws<-type.convert(KS_ws, as.is = TRUE) %>% 
  as.Date(KS_wind_final$Timestamp, format="%Y-%m-%d")
KS_wind_final$Date <- as.Date(KS_wind_final$Timestamp, format="%Y-%m-%d")

summary(KS_wind_final)
summary(KS_ws)

View(KS_wind_final)

##BELUGA
BELUGA_ws<-BELUGA_wind_final %>% 
  mutate(ws=sqrt(as.numeric(BELUGA_wind_final$u)^2 + as.numeric(BELUGA_wind_final$v)^2)) %>% 
  mutate(wd=(270-atan2(BELUGA_wind_final$v, BELUGA_wind_final$u)*180/pi)%%360) %>% 
  mutate(date=BELUGA_wind_final$Date <- as.Date(BELUGA_wind_final$Timestamp, format="%Y-%m-%d")) %>%
  mutate(Jdate=yday(date)) %>% 
  mutate(wp=BELUGA_ws$ws*cos(118-BELUGA_ws$wd))#adds wind profit, given the bearing of 118

windRose(BELUGA_ws, 
         ws="ws",
         wd="wd",
         type="month")

##KS
KS_ws<-KS_wind_final %>% 
  mutate(ws=sqrt(as.numeric(KS_wind_final$u)^2 + as.numeric(KS_wind_final$v)^2)) %>% #this is speed in m/s
  mutate(wd=(270-atan2(KS_wind_final$v, KS_wind_final$u)*180/pi)%%360) %>%
  mutate(date=KS_wind_final$Date <- as.Date(KS_wind_final$Timestamp, format="%Y-%m-%d")) %>%
  mutate(Jdate=yday(date)) %>% 
  mutate(wp=KS_ws$ws*cos(116-KS_ws$wd))#adds wind profit, given the bearing of 116


windRose(KS_ws,
         ws = "ws",
         wd = "wd", 
         type="month")

##these plots show the wind profit of every day

ggplot(KS_ws, aes(Jdate, wp))+geom_point()

ggplot(BELUGA_ws, aes(date, wp))+geom_point()



