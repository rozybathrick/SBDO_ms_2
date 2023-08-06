## converting wind data from u and v at King Salmon and Beluga to wind velocity and direction
library(tidyverse)
library(raster)
library(openair)
library(lubridate)
library(viridis)

## Load dataframes that now have AK time zone

## BELGUA 2022
BELUGA_wind_finalak<-read.csv("processed_data/BELUGA_wind_final_aktime.csv")


summary(BELUGA_wind_finalak)

BELUGA_wind_finalak <- type.convert(BELUGA_wind_finalak, as.is = TRUE)

View(BELUGA_wind_finalak)

BELUGA_ws<-type.convert(BELUGA_ws, as.is=TRUE)

## BELUGA 2021
BELUGA_wind_final_21ak <-  read.csv("processed_data/BELUGA_wind_final_21aktime.csv")
BELUGA_wind_final_21ak <- type.convert(BELUGA_wind_final_21ak, as.is = TRUE)
summary(BELUGA_wind_final_21aktime)


## KS
KS_wind_finalak<- read.csv("processed_data/KS_wind_final_aktime.csv")
  
KS_wind_finalak<-  type.convert(KS_wind_finalak, as.is = TRUE)

summary(KS_wind_finalak)
summary(KS_ws)
View(KS_ws)

##BELUGA: adding wind speed, direction, and profit in AK timezone
BELUGA_ws<-BELUGA_wind_finalak %>% 
  mutate(ws=sqrt(as.numeric(BELUGA_wind_finalak$u)^2 + as.numeric(BELUGA_wind_finalak$v)^2)) %>% 
  mutate(wd=(270-atan2(BELUGA_wind_finalak$v, BELUGA_wind_finalak$u)*180/pi)%%360) %>% 
  mutate(date=BELUGA_wind_finalak$Date <- as.Date(BELUGA_wind_finalak$Timestamp_AK, format = '%m/%d/%Y %H:%M')) %>%
  mutate(Jdate=yday(date)) %>% 
  mutate(tw=BELUGA_ws$ws*cos(118-BELUGA_ws$wd)) %>% #adds wind profit, given the bearing of 118M
  filter(Jdate > 152)

View(BELUGA_ws)



windRose(BELUGA_ws, 
         ws="ws",
         wd="wd",
         type="month")



##KS: adding wind speed, direction, and profit in AK timezone
KS_ws<-KS_wind_finalak %>% 
  mutate(ws=sqrt(as.numeric(KS_wind_finalak$u)^2 + as.numeric(KS_wind_finalak$v)^2)) %>% #this is speed in m/s
  mutate(wd=(270-atan2(KS_wind_finalak$v, KS_wind_finalak$u)*180/pi)%%360) %>%
  mutate(date=KS_wind_finalak$date <- as.Date(KS_wind_finalak$Timestamp_AK, format='%m/%d/%Y %H:%M')) %>%
  mutate(Jdate=yday(date)) %>% 
  mutate(tw=KS_ws$ws*cos(116-KS_ws$wd)) %>%  #adds tail wind profit, given the bearing of 116
  filter(Jdate>152)


windRose(KS_ws,
         
         ws = "ws",
         wd = "wd", 
         type="month")

### BELUGA 2021

BELUGA_ws_21<-BELUGA_wind_final_21ak %>% 
  mutate(ws=sqrt(as.numeric(BELUGA_wind_final_21ak$u)^2 + as.numeric(BELUGA_wind_final_21ak$v)^2)) %>% 
  mutate(wd=(270-atan2(BELUGA_wind_final_21ak$v, BELUGA_wind_final_21ak$u)*180/pi)%%360) %>% 
  mutate(date=BELUGA_wind_final_21ak$date <- as.Date(BELUGA_wind_final_21ak$Timestamp_AK, format='%m/%d/%Y %H:%M')) %>%
  mutate(Jdate=yday(date)) %>% 
  mutate(tw=BELUGA_ws_21$ws*cos(118-BELUGA_ws_21$wd))#adds wind profit, given the bearing of 118

View(BELUGA_ws_21)

windRose(BELUGA_ws_21, 
         ws="ws",
         wd="wd",
         type="month")


##these plots show the wind profit of every day

plot1<-ggplot(KS_ws, aes(Jdate, tw))+geom_point(stat="identity", aes(color=tw))+ scale_color_viridis(option = "D")+
  ggtitle("Hourly tw from KS to 116 deg")

plot1

plot2<-ggplot(BELUGA_ws, aes(Jdate, tw))+geom_bar(stat="identity", aes(color=tw)) + scale_color_viridis(option = "A")+
  ggtitle("Hourly tw in Beluga to 118 deg")
plot2


