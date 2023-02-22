library(ggplot2)
library(ggspatial)
library(lubridate)
library(patchwork)
library(dplyr)
library(ggmap)

sbdo_dod_fall<-read.csv("raw_data/9.29.22_fall21_22_dod.csv")

##clean up and rename fields, add J date
sbdo_dod<-subset(sbdo_dod_fall,select=c('tag.local.identifier','timestamp','location.long','location.lat', 'Capture.year', 'Breeding.site')) %>% 
  rename(id="tag.local.identifier",
         long="location.long",
         lat="location.lat",
         site="Breeding.site",
         year="Capture.year") %>% 
  mutate(timestamp=dmy(timestamp))


NorAm <- c(left=-160, bottom=20, right=-60,top= 70)
south_AK<-c(left=-160, bottom=57.6, right=-147, top=62)
register_google(key = "[AIzaSyCUjHELr82OAmlqVQgTyMHenOwqy4zRXd4]")

pls_map<-get_map(location=NorAm, source="stamen",zoom=4, maptype=c("toner"),
                 theme(legend.position="bottomleft"), crop=FALSE, color=c("bw"))



breed_map<-get_map(location=south_AK, source="stamen", zoom=6, maptype=c("terrain"), crop=FALSE, color=c("bw"))

ggmap(breed_map)+geom_point(aes(x=long, y=lat, group=id, color=site), data=breedsite,
                            alpha=1, size=2)+scale_color_manual(values=c("darkblue","goldenrod3"))

#mapping beluga 21-22 and ks 22
ggmap(pls_map, legend="left", padding=0.001) +geom_point(aes(x = long, y = lat, group=id, color=site), data = sbdo_dod, 
                                                         alpha = .7, size =1)+geom_line(aes(x = long, y = lat, group=id, color=site), data = sbdo_dod,
                                                                                         alpha = 1, size = 0.5)+scale_color_manual(values=c("brown3","dodgerblue4"))


