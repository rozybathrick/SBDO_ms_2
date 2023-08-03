### Making dataframes for each birds location / daily wind speed / daily tailwind 

library(tidyverse)

### KING SALMON

KS_bird_locs_rWind <- KS_rWind %>% 
  filter((Lat == 58.7 & Long == -157.23) | (Lat == 58.95 & Long ==-156.98) | 
           (Lat == 58.7 & Long == -156.98) | (Lat == 58.7 & Long == -156.73)) %>% 
  group_by(Long, Lat, Jdate) %>%
  summarise(mean(speed),
            mean(tw),
            max(speed)) %>% 
  mutate(date=as.Date(Jdate, origin = as.Date('2022-01-01')))
write.csv(KS_bird_locs_rWind, file="KS_bird_locs_rWind.csv")  



### BELUGA 2022

BEL22_bird_locs_rWind <- BEL22_rWind %>% 
  filter((Lat == 61.01 & Long == -151.48) | (Lat == 61.01 & Long ==-150.98) | 
           (Lat == 61.26 & Long == -150.98) | (Lat == 61.26 & Long == -150.73) |
           (Lat == 61.26 & Long == -150.48) | (Lat == 61.26 & Long == -150.98)) %>% 
  group_by(Long, Lat, Jdate) %>%
  summarise(mean(speed),
            mean(tw), 
            max(speed)) %>% 
  mutate(date=as.Date(Jdate, origin = as.Date('2022-01-01')))

write.csv(BEL22_bird_locs_rWind, file="BEL22_bird_locs_rWind.csv")

### BELUGA 2021
BEL21_bird_locs_rWind<-BEL21_rWind %>% 
  filter(Lat == 61.26 & Long == -150.98) %>% 
  group_by(Long, Lat, Jdate) %>%
  summarise(mean(speed),
            mean(tw), 
            max(speed)) %>% 
  mutate(date=as.Date(Jdate, origin = as.Date('2021-01-01')))
write.csv(BEL21_bird_locs_rWind, file="BEL21_bird_locs_rWind.csv")

