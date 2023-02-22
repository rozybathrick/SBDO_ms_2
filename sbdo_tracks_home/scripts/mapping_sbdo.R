library(ggmap) #to get the background maps; note: THIS ALSO LOADS ggplot2
library(sf) #for plotting spatial objects with ggplot2
library(ggspatial) #for adding mappy things (scale, North arrow) to our plots
library(adehabitatHR) #for calculating a home range
library(rgdal) #for bringing in .GPX files
library(viridis) #for nice color scales for plotting
library(mapview)

devtools::install_github("dkahle/ggmap")
install.packages("ggplot2")
install.packages("devtools")
Yes
install.packages("ggmap")
install.packages("ggspatial")
no
install.packages("viridis")

##get an API key base map if I want a google map - register from link in ggmap
# points need to be in lat and long not geometry (pull it out)

NorAm <- c(left=-160, bottom=20, right=-60,top= 70)
south_AK<-c(left=-160, bottom=57.6, right=-147, top=62)
register_google(key = "[AIzaSyCUjHELr82OAmlqVQgTyMHenOwqy4zRXd4]")

pls_map<-get_map(location=NorAm, source="stamen",zoom=4, maptype=c("toner"),
                 theme(legend.position="bottomleft"), crop=FALSE, color=c("bw"))



breed_map<-get_map(location=south_AK, source="stamen", zoom=6, maptype=c("terrain"), crop=FALSE, color=c("bw"))

ggmap(breed_map)+geom_point(aes(x=long, y=lat, group=id, color=site), data=breedsite,
                            alpha=1, size=2)+scale_color_manual(values=c("darkblue","goldenrod3"))

#mapping beluga 21-22 and ks 22
ggmap(pls_map, legend="left", padding=0.001) +geom_point(aes(x = long, y = lat, group=id, color=site), data = sbdo_clean, 
           alpha = .7, size = 5)+geom_line(aes(x = long, y = lat, group=id, color=site), data = sbdo_clean,
                                           alpha = 1, size = 1.2)+scale_color_manual(values=c("brown3","dodgerblue4"))


