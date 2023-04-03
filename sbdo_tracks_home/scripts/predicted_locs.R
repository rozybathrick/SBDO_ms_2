library(tidyverse)
library(aniMotum)
library(sf)

### manipulating predicted location dataframe

crw24<-p_bird1 %>% 
  mutate(dist_to_next_m = st_distance(lag(geometry), geometry, by_element = TRUE)) %>%
  mutate(dis_to_next_km = dist_to_next_m / 1000) %>% 
  mutate(timetilnext = difftime(lead(date), date, unit="hours")) %>% 
  mutate(lon=latlon$lon) %>% 
  mutate(lat=latlon$lat)
getwd()

write.csv(crw24, file="crw24_predictedpoints.csv")
  
  
class(p_bird1$geometry)
  
first_flights<-crw24 %>% 
  filter(dis_to_next_km > 200)

latlon<-do.call(rbind, st_geometry(p_bird1$geometry)) %>% 
  as_tibble() %>% setNames(c("lon", "lat"))
