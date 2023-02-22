## new scripts with cleaned up and updated dataset, 9.29.22

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggspatial)
library(lubridate)
library(patchwork)

sbdo_clean_fall<-read.csv("raw_data/9.29.22_fall21_22.csv")

##clean up and rename fields, add J date
sbdo_clean<-subset(sbdo_clean_fall,select=c('tag.local.identifier','timestamp','location.long','location.lat', 'Capture.year', 'Breeding.site')) %>% 
  rename(id="tag.local.identifier",
         long="location.long",
         lat="location.lat",
         site="Breeding.site",
         year="Capture.year") %>% 
  mutate(timestamp=dmy(timestamp))

scleanJ <- sbdo_clean %>% 
  mutate(Jdate=yday(timestamp))

##creating subset of the journey in three chunks
first_chunk<-scleanJ %>% 
  filter(lat>=40.7&lat<=62)

second_chunk<-scleanJ %>% 
  filter(lat>=31&lat<=41)

third_chunk<-scleanJ %>% 
  filter(lat>=20&lat<=32)

##creating subset of the dates

breedsite<-scleanJ %>%
  filter(Jdate>=166&Jdate<=190)

early<-scleanJ %>%
  filter(Jdate>=176&Jdate<=197)

mid.1<-scleanJ %>% 
  filter(Jdate>=198&Jdate<=219)

mid.2<-scleanJ %>% 
  filter(Jdate>=220&Jdate<=240)

end<-scleanJ %>% 
  filter(Jdate>=241&Jdate<=262)

##subsetting breeding site
bel_lee<-scleanJ %>% 
  filter(site=="Beluga")

ks_lee<-scleanJ %>% 
  filter(site=="King_Salmon")
##graphs by location/time

#leaving AK
leave_ak<-ggplot(first_chunk, aes(x=Jdate, y=lat,color=site, group=id))+geom_point()+geom_line()+
  theme(legend.position="none")+
  scale_x_continuous(limits=c(165, 244), breaks = c(166, 182,196,213,227, 244),
                     labels=c("Jun 15", "July 1","July 15","Aug 1", "Aug 15", "Sept 1"))+
  scale_color_manual(values=c("#D55E00","#56B4E9"))+
  labs(x="Date",y="Latitude (degrees)", title="Breeding site to Arcata")+ylim(20,65)+
  theme(panel.background = element_rect(fill = "white",color = "grey", size = 0.25,linetype='solid')); leave_ak

#central flight
central<-ggplot(second_chunk, aes(x=Jdate, y=lat,color=site, group=id))+geom_point()+geom_line()+
  theme(legend.position="none")+
  scale_x_continuous(limits=c(165, 244), breaks = c(166, 182,196,213,227, 244),
                     labels=c("Jun 15", "July 1","July 15","Aug 1", "Aug 15", "Sept 1"))+
  scale_color_manual(values=c("#D55E00","#56B4E9"))+
  labs(x=NULL,y="Latitude (degrees)", title="Arcata to San Diego")+ylim(20,65)+
  theme(panel.background = element_rect(fill = "white",color = "grey",size = 0.25,linetype='solid', ));central

#final flight
final<-ggplot(third_chunk, aes(x=Jdate, y=lat,color=site, group=id))+geom_point()+geom_line()+
  theme(legend.position="none")+
  scale_x_continuous(limits=c(165, 244), breaks = c(166, 182,196,213,227, 244),
                     labels=c("Jun 15", "July 1","July 15","Aug 1", "Aug 15", "Sept 1"))+
  scale_color_manual(values=c("#D55E00","#56B4E9"))+
  labs(x=NULL, y="Latitude (degrees)", title="San Diego to wintering site")+ylim(20,65)+
  theme(panel.background = element_rect(fill = "white",color = "grey",size = 0.25,linetype='solid', ));final

##all togther now
whole_flight<-ggplot(scleanJ, aes(x=Jdate, y=lat,color=site, group=id))+geom_point()+geom_line()+
  scale_x_continuous(limits=c(165, 259), breaks = c(166, 182,196,213,227, 244, 259),
                     labels=c("Jun 15", "July 1","July 15","Aug 1", "Aug 15", "Sept 1", "Sept 15"))+
  scale_color_manual(values=c("#D55E00","#56B4E9"))+
  labs(x=NULL,y="Latitude (degrees)", title="entire flight")+ylim(20,65)+
  theme(panel.background = element_rect(fill = "white",color = "grey",size = 0.25,linetype='solid', ));whole_flight

##beluga alone

Bel_alone<-ggplot(bel_lee_good, aes(x=Jdate, y=lat,color=site, group=id))+
  theme(legend.position=("none"), plot.title=element_text(hjust=0.5),
        plot.subtitle=element_text(hjust=0.5))+
  geom_point(size=3)+geom_line(size=1)+
  scale_x_continuous(limits=c(165, 259), breaks = c(166, 182,196,213,227, 244, 259),
                     labels=c("Jun 15", "July 1","July 15","Aug 1", "Aug 15", "Sept 1", "Sept 15"))+
  scale_color_manual(values=c("brown3"))+
  labs(x=NULL,y=NULL, title="Beluga", 
       subtitle="2021 n=6, 2022 n=6")+ylim(22,62)+
  theme(axis.title.y = element_text(size = 30), plot.title=element_text(size=40),
        plot.subtitle=element_text(size=24), axis.text=element_text(size=12),
        text = element_text(family = "Times New Roman"))+
  theme(panel.background = element_rect(fill="white", color = "grey",size = 0.25,linetype='solid'));Bel_alone


##removing a couple of lines that make things confusing because there is so much time between points

bel_lee_good<-bel_lee %>% 
  filter(!(id=='198763'
           & Jdate > 243),
         !(id=='198758'
         & Jdate==172))

##king salmon alone

ks_alone<-ggplot(ks_lee, aes(x=Jdate, y=lat,color=site, group=id))+
  theme(legend.position=("none"), plot.title=element_text(hjust=0.5),
        plot.subtitle=element_text(hjust=0.5))+
  geom_point(size=3)+geom_line(size=1)+
  scale_x_continuous(limits=c(165, 259), breaks = c(166, 182,196,213,227, 244, 259),
                     labels=c("Jun 15", "July 1","July 15","Aug 1", "Aug 15", "Sept 1", "Sept 15"))+
  scale_color_manual(values=c("dodgerblue4"))+
  labs(x=NULL,y="Latitude (degrees)", title="King Salmon", 
       subtitle="2022, n=9")+ylim(22,62)+
  theme(axis.title.y = element_text(size = 30), plot.title=element_text(size=40),
        plot.subtitle=element_text(size=24), axis.text=element_text(size=12),
        text = element_text(family = "Times New Roman"))+
  theme(panel.background = element_rect(fill="white", color = "grey",size = 0.25,linetype='solid', ));ks_alone


ks_alone+Bel_alone
tog<-patchwork::patchworkGrob(lee)
gridExtra::grid.arrange(tog, left = NULL, bottom = "Date")



##another graph split up by time, not latitude

##first 21 days
early_flight<-ggplot(early, aes(x=Jdate, y=lat,color=site, group=id))+geom_point()+geom_line()+
  scale_color_manual(values=c("#D55E00","#56B4E9"))+
  labs(x="J day",y="Latitude (degrees)", title="First 28 days from when first bird left")+ylim(20,65)+
  theme(panel.background = element_rect(fill = "white",color = "grey",size = 0.25,linetype='solid', ));early_flight

##next 21 days
mid_flight<-ggplot(mid.1, aes(x=Jdate, y=lat,color=site, group=id))+geom_point()+geom_line()+
  scale_color_manual(values=c("#D55E00","#56B4E9"))+
  labs(x="J day",y="Latitude (degrees)", title="Second 28 days from when first bird left")+ylim(20,65)+
  theme(panel.background = element_rect(fill = "white",color = "grey",size = 0.25,linetype='solid', ));mid_flight

#next 21 days
mid_flight<-ggplot(mid.2, aes(x=Jdate, y=lat,color=site, group=id))+geom_point()+geom_line()+
  scale_color_manual(values=c("#D55E00","#56B4E9"))+
  labs(x="J day",y="Latitude (degrees)", title="Second 28 days from when first bird left")+ylim(20,65)+
  theme(panel.background = element_rect(fill = "white",color = "grey",size = 0.25,linetype='solid', ));mid_flight

#final 21 days
late_flight<-ggplot(end, aes(x=Jdate, y=lat,color=site, group=id))+geom_point()+geom_line()+
  scale_color_manual(values=c("#D55E00","#56B4E9"))+
  labs(x="J day",y="Latitude (degrees)", title="Final 28 days from when first bird left")+ylim(20,65)+
  theme(panel.background = element_rect(fill = "white",color = "grey",size = 0.25,linetype='solid', ));late_flight

