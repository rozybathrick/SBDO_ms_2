install.packages("rWind")
library(rWind)
library(lubridate)
library(tidyverse)

## playing around with the rWind package
# this is a much cleaner way to derive wind speed and direction, and seems to give slightly different
#results for wind direction than the method by "hand" - maybe I did 

## with rWind, the direction output is the way the wind is blowing TO, not FROM

###BELUGA 2021: calculating direction and windspeed
dir_sp_df<-uv2ds(BELUGA_wind_final_21ak$u, BELUGA_wind_final_21ak$v)
BEL21_wind_rWind<-cbind(BELUGA_wind_final_21ak, dir_sp_df)

BEL21_rWind<- BEL21_wind_rWind %>% 
  mutate(date=as.Date(BELUGA_wind_final_21ak$Timestamp_AK, format='%m/%d/%Y %H:%M')) %>%
  mutate(Jdate=yday(date)) %>% 
  mutate(tw=BEL21_wind_rWind$speed*cos(118-BEL21_wind_rWind$dir))#adds wind profit, given the bearing of 118



###BELUGA 2022
dir_sp_df2<-uv2ds(BELUGA_wind_finalak$u, BELUGA_wind_finalak$v)
BE22L_wind_rWind<-cbind(BELUGA_wind_finalak, dir_sp_df2)

BEL22_rWind<- BE22L_wind_rWind %>% 
  mutate(date=as.Date(BE22L_wind_rWind$Timestamp_AK, format='%m/%d/%Y %H:%M')) %>%
  mutate(Jdate=yday(date)) %>% 
  mutate(tw=BE22L_wind_rWind$speed*cos(118-BE22L_wind_rWind$dir))#adds wind profit, given the bearing of 118

###KS
dir_sp_df3<-uv2ds(KS_wind_finalak$u, KS_wind_finalak$v)
KS_wind_rWind<-cbind(KS_wind_finalak, dir_sp_df3)

KS_rWind<- KS_wind_rWind %>% 
  mutate(date=as.Date(KS_wind_rWind$Timestamp_AK, format='%m/%d/%Y %H:%M')) %>%
  mutate(Jdate=yday(date)) %>% 
  mutate(tw=KS_wind_rWind$speed*cos(118-KS_wind_rWind$dir))#adds wind profit, given the bearing of 118

## i guess I should run the models again lollllll to see if now the tail wind improves



