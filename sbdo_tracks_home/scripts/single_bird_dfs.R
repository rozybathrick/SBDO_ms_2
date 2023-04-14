### Making dataframes for each birds location / daily wind speed / daily tailwind 

library(tidyverse)

### KING SALMON

KS_bird_locs <- KS_ws %>% 
  filter((Lat == 58.7 & Long == -157.23) | (Lat == 58.95 & Long ==-156.98) | 
           (Lat == 58.7 & Long == -156.98) | (Lat == 58.7 & Long == -156.73)) %>% 
  group_by(Long, Lat, Jdate) %>%
  summarise(mean(ws),
            mean(tw),
            max(ws))

write.csv(KS_bird_locs, file="KS_bird_locs.csv")


### BELUGA 2022

BEL_bird_locs22 <- BELUGA_ws %>% 
  filter((Lat == 61.01 & Long == -151.48) | (Lat == 61.01 & Long ==-150.98) | 
           (Lat == 61.26 & Long == -150.98) | (Lat == 61.26 & Long == -150.73) |
           (Lat == 61.26 & Long == -150.48) | (Lat == 61.26 & Long == -150.98)) %>% 
  group_by(Long, Lat, Jdate) %>%
  summarise(mean(ws),
            mean(tw), 
            max(ws))

write.csv(BEL_bird_locs22, file="BEL_bird_locs22.csv")

### BELUGA 2021
BEL_bird_loc21<-BELUGA_ws_21 %>% 
  filter(Lat == 61.26 & Long == -150.98) %>% 
  group_by(Long, Lat, Jdate) %>%
  summarise(mean(ws),
            mean(tw), 
            max(ws))

write.csv(BEL_bird_loc21, file="BEL_bird_loc21.csv")
