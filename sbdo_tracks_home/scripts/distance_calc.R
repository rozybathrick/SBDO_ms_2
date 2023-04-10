library(tidyverse)
library(sf)
library(lubridate)
install.packages("mapview")
library(mapview)


##load dataset
sbdo_tracks<-read_csv("raw_data/9.29.22_fall21_22.csv")

##rename columns, change time format, omit blanks, turn the coordinates into spatial data
sbdo_sf<-sbdo_tracks %>% 
  select("tag-local-identifier", "timestamp", "location-long", "location-lat", 
         "Breeding-site", "Capture-year", "height-above-ellipsoid") %>% 
  rename(id="tag-local-identifier",
         long="location-long",
         lat="location-lat",
         site="Breeding-site",
         year="Capture-year",
         elevation="height-above-ellipsoid") %>% 
  mutate(timestamp=dmy(timestamp)) %>% 
  na.omit() %>% 
  mutate(Jdate=yday(timestamp)) %>% 
  st_as_sf(coords=c("long", "lat"), crs=4326)

# create a dataset of just King Salmon birds and view
ks<-sbdo_sf %>% 
  filter(site=="King_Salmon")
mapview(ks, zcol = "id")

cook<-sbdo_sf %>% 
  filter(site=="Beluga"| site=="JBER")
mapview(cook, zcol="id")
  

#new dataframe to calculate distance between points

sbdo_distance<-sbdo_sf %>% 
  group_by(id) %>% 
  mutate(dist_to_next = st_distance(lag(geometry), geometry, by_element = TRUE)) %>% 
  mutate(timetilnext = difftime(lead(timestamp), timestamp, unit="hours"))


##create a new sheet with the distances between each dep and arrival date
sbdo_dist_bybird<-sbdo_distance %>% 
  group_by(id) %>% 
  filter(Jfly_dist, Jdep_date:Jarr_date)

##filtering for birds that did 2+ day nonstop flights over ocean

#bail to csv
write.csv(sbdo_distance, file="sbdo_distance.csv")

## Beluga recap bird distance

recap<-read.csv("raw_data/198767_Beluga_recap.csv")

recap1<-recap %>% 
  select("id", "timestamp", "long", "lat", 
         "Altitude") %>% 
  rename(id="id",
         long="long",
         lat="lat",
         elevation="Altitude") %>% 
  mutate(site=(c("Beluga"))) %>% 
  mutate(year=(c("2021"))) %>% 
  mutate(Jdate=yday(timestamp)) %>% 
  st_as_sf(coords=c("long", "lat"), crs=4326) %>% 
  mutate(dist_to_next = st_distance(lag(geometry), geometry, by_element = TRUE)) %>% 
  mutate(timetilnext = difftime(lead(timestamp), timestamp, unit="hours"))

#bail to csv
write.csv(recap1, file="recap1_dist.csv")

