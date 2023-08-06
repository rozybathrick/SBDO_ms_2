## creating data frames for each bird 

rWind_birds21<-dep_mod1 %>% 
  select(-mean_tw, -mean_ws, -max_ws, -max_ws_sd, -mean_tw_sd, -mean_ws_sd) %>%
  rename(date="date_opt_1",
         Long="win_long",
         Lat="win_lat") %>% 
  filter(year==2021) %>% 
  mutate(date=as.Date(Jdate, origin = as.Date('2021-01-01')))

rWind_birds22<-dep_mod1 %>% 
  select(-mean_tw, -mean_ws, -max_ws, -max_ws_sd, -mean_tw_sd, -mean_ws_sd) %>%
  rename(date="date_opt_1",
         Long="win_long",
         Lat="win_lat") %>% 
  filter(year==2022) %>% 
  mutate(date=as.Date(Jdate, origin = as.Date('2022-01-01')))

rWind_birds<-rbind(rWind_birds21, rWind_birds22)

write.csv(rWind_birds, file="rWind_birds.csv")

getwd()
rWind_birds<-read.csv("rWind_birds.csv")  

  



