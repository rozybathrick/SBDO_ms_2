# library(plyr) Seems like some issues we encounted in the summary loop relate to plyr masking dplyr functions?
library(dplyr)
library(tidyr)
library(psych)
library(ggplot2)
library(ggspatial)
library(lubridate)
install.packages("ggspatial")
yes

# Set working directory, import and modify working file

sbdo_lat<-read.csv("raw_data/9.25.22_allyears.csv")
View(sbdo_lat)

# Refine to necessary fields
sbdo1<-subset(sbdo_lat,select=c('tag.local.identifier','timestamp','location.long','location.lat', 'Capture.year', 'Breeding.site')) %>% 
  rename(id="tag.local.identifier",
         long="location.long",
         lat="location.lat",
         site="Breeding.site",
         year="Capture.year") %>% 
  mutate(timestamp=dmy(timestamp))
View(sbdo1)

# add j date column

sday<- sbdo1 %>% mutate(DayDate=day(timestamp)) %>% mutate(MonthDate=month(timestamp)) %>%
  unite(DayMonth, c("DayDate", "MonthDate"), sep = "-")

sdayJ <- sday %>% 
  mutate(Jdate=yday(timestamp))


#isolate by year
s1<-subset(sdayJ,sdayJ$year==2021)
s2<-subset(sdayJ, sdayJ$year==2022)

##filtering out dead and bad tags

s21_badtags<-c("198753", "198760", "198765", "198762",
               "198770", "198772", "198773", "198768", 
               "198756", "198759", "198761", "198763", 
               "198769", "198764", "198754", "198759")
s22_badtags<-c("233810", "233814", "233812")



##2022 good tags alone
s22_goodtags<-s2 %>% 
  filter(!(id%in%s22_badtags))

sgood<-sdayJ %>% 
  filter(!(id%in%s22_badtags & id%in%s21_badtags ))

#just JBER
JBER<-sgood %>% 
  filter(site=="JBER")
#just Beluga
Bel<-sgood %>% 
  filter(site=="Beluga")

##filtering out JBER to see what just Beluga and KS look like
sgood_belAKN<-sgood %>% filter(site=="Beluga"| site=="King_Salmon", Jdate>=150&Jdate<=300)
view(sgood_belAKN)

view(tags22)
##cook inlet alone
cookin<-sgood %>% filter(site=="Beluga" | site=="JBER")

##creating subset of the journey in three chunks
first_chunk<-tags22_fall %>% 
  filter(lat>=40.7&lat<=62)
second_chunk<-tags22_fall %>% 
  filter(lat>=31&lat<=40.8)
third_chunk<-tags22_fall %>% 
  filter(lat>=20&lat<=31.1)

##creating subset of the dates
early<-tags22_fall %>%
  filter(Jdate>=176&Jdate<=197)

mid.1<-tags22_fall %>% 
  filter(Jdate>=198&Jdate<=219)

mid.2<-tags22_fall %>% 
  filter(Jdate>=220&Jdate<=240)

end<-tags22_fall %>% 
  filter(Jdate>=241&Jdate<=262)

##plotting by year and combined

#first just 2022 good tags
tags2022<-ggplot(s22_goodtags, aes(x=Jdate, y=lat,color=site, group=id))+geom_point()+geom_line()+
scale_x_continuous(breaks = c(150,182,213,244,274,305), labels=c("Jun","July","Aug","Sept","Oct","Nov"))+
  scale_color_manual(values=c("#D55E00","#56B4E9"))+
  labs(x="Month",y="Latitude (degrees)", title="SBDO tracked in 2022 from Beluga and King Salmon")+ylim(20,65)+
  theme(panel.background = element_rect(fill = "white",color = "grey",size = 0.25,linetype='solid', ));tags2022

#2022 good tags and Beluga 2021 good tags
tags2022_bel2021<-ggplot(tags22_fall, aes(x=Jdate, y=lat,color=site, group=id))+geom_point()+geom_line()+
  scale_x_continuous(breaks = c(150,182,213,244,274,305), labels=c("Jun","July","Aug","Sept","Oct","Nov"))+
  scale_color_manual(values=c("#D55E00","#56B4E9"))+
  labs(x="Month",y="Latitude (degrees)", title="Short-billed dowitchers tracked from Beluga and King Salmon, AK, 2021-2022")+ylim(20,65)+
  theme(panel.background = element_rect(fill = "white",color = "grey",size = 0.25,linetype='solid', ));tags2022_bel2021

##just JBER out of curiousity
tagsJBER<-ggplot(JBER, aes(x=Jdate, y=lat, group=id))+geom_point()+geom_line()+
  scale_x_continuous(breaks = c(150,182,213,244,274,305), labels=c("Jun","July","Aug","Sept","Oct","Nov"))+
  scale_color_manual(values=c("#D55E00","#56B4E9"))+
  labs(x="Month",y="Latitude (degrees)", title="SBDO tracked from JBER 2021")+ylim(20,60)+
  theme(panel.background = element_rect(fill = "white",color = "grey",size = 0.25,linetype='solid', ));tagsJBER

#just JBER and Beluga both years
cook<-ggplot(cookin, aes(x=Jdate, y=lat, group=id, color=site))+geom_point()+geom_line()+
  scale_x_continuous(breaks = c(150,182,213,244,274,305), labels=c("Jun","July","Aug","Sept","Oct","Nov"))+
  scale_color_manual(values=c("#D55E00", "#009E73"))+
  labs(x="Month",y="Latitude (degrees)", title="SBDO tracked from Cook Inlet 2021 and 2022")+ylim(20,60)+
  theme(panel.background = element_rect(fill = "white",color = "grey",size = 0.25,linetype='solid', ));cook

#Beluga alone both years
Belu<-ggplot(Bel, aes(x=Jdate, y=lat, group=id, color=factor(year)))+geom_point()+geom_line()+
  scale_x_continuous(breaks = c(31, 60, 91, 120, 150, 182, 213, 244,274,305, 335, 365), 
                     labels=c("Jan", "Feb", "Mar", "Apr","May", "Jun","July","Aug","Sept","Oct","Nov", "Dec"))+
  scale_color_manual(values=c("#CC79A7", "#000000"))+
  labs(x="Month",y="Latitude (degrees)", title="SBDO tracked from Beluga 2021 and 2022")+ylim(20,60)+
  theme(panel.background = element_rect(fill = "white",color = "grey",size = 0.25,linetype='solid', ));Belu

##exploring longitudes for fun
tags2022_lon<-ggplot(s22_goodtags, aes(x=long, y=Jdate, color=site, group=id))+geom_point()+geom_line()+
  scale_y_continuous(breaks = c(150,182,213,244,274,305), labels=c("Jun","July","Aug","Sept","Oct","Nov"))+
  scale_y_reverse(breaks = c(150,182,213,244,274,305), labels=c("Jun","July","Aug","Sept","Oct","Nov"))+
  scale_color_manual(values=c("#D55E00","#56B4E9"))+
  labs(x="Longitude",y="Month", title="SBDO tracked in 2022 from Beluga and King Salmon")+
  theme(panel.background = element_rect(fill = "white",color = "grey",size = 0.25,linetype='solid', ));tags2022_lon

##breaking down the scale into three chunks. doesn't really show good patterns

#leaving AK
leave_ak<-ggplot(first_chunk, aes(x=Jdate, y=lat,color=site, group=id))+geom_point()+geom_line()+
  scale_x_continuous(breaks = c(150,182,213,244,274,305), labels=c("Jun","July","Aug","Sept","Oct","Nov"))+
  scale_color_manual(values=c("#D55E00","#56B4E9"))+
  labs(x="Month",y="Latitude (degrees)", title="Short-billed dowitchers leaving from Beluga and King Salmon, AK, 2021-2022")+ylim(20,65)+
  theme(panel.background = element_rect(fill = "white",color = "grey",size = 0.25,linetype='solid', ));leave_ak

#central flight
central<-ggplot(second_chunk, aes(x=Jdate, y=lat,color=site, group=id))+geom_point()+geom_line()+
  scale_x_continuous(breaks = c(150,182,213,244,274,305), labels=c("Jun","July","Aug","Sept","Oct","Nov"))+
  scale_color_manual(values=c("#D55E00","#56B4E9"))+
  labs(x="Month",y="Latitude (degrees)", title="Short-billed dowitchers central flight from Beluga and King Salmon, AK, 2021-2022")+ylim(20,65)+
  theme(panel.background = element_rect(fill = "white",color = "grey",size = 0.25,linetype='solid', ));central

#final flight
final<-ggplot(third_chunk, aes(x=Jdate, y=lat,color=site, group=id))+geom_point()+geom_line()+
  scale_x_continuous(breaks = c(150,182,213,244,274,305), labels=c("Jun","July","Aug","Sept","Oct","Nov"))+
  scale_color_manual(values=c("#D55E00","#56B4E9"))+
  labs(x="Month",y="Latitude (degrees)", title="Short-billed dowitchers final flight from Beluga and King Salmon, AK, 2021-2022")+ylim(20,65)+
  theme(panel.background = element_rect(fill = "white",color = "grey",size = 0.25,linetype='solid', ));final

##another graph split up by time, not latitude

##first 21 days
early_flight<-ggplot(early, aes(x=Jdate, y=lat,color=site, group=id))+geom_point()+geom_line()+
  scale_color_manual(values=c("#D55E00","#56B4E9"))+
  labs(x="J day",y="Latitude (degrees)", title="First 28 days from when first bird left")+ylim(20,65)+
  theme(panel.background = element_rect(fill = "white",color = "grey",size = 0.25,linetype='solid', ));early_flight

##next 28 days
mid_flight<-ggplot(mid, aes(x=Jdate, y=lat,color=site, group=id))+geom_point()+geom_line()+
  scale_color_manual(values=c("#D55E00","#56B4E9"))+
  labs(x="J day",y="Latitude (degrees)", title="Second 28 days from when first bird left")+ylim(20,65)+
  theme(panel.background = element_rect(fill = "white",color = "grey",size = 0.25,linetype='solid', ));mid_flight

#final 28 days
late_flight<-ggplot(late, aes(x=Jdate, y=lat,color=site, group=id))+geom_point()+geom_line()+
  scale_color_manual(values=c("#D55E00","#56B4E9"))+
  labs(x="J day",y="Latitude (degrees)", title="Final 28 days from when first bird left")+ylim(20,65)+
  theme(panel.background = element_rect(fill = "white",color = "grey",size = 0.25,linetype='solid', ));late_flight

