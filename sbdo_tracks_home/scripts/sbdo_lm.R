
library(ggplot2)
install.packages("lme4")
no
library(lme4)
library(tidyverse)
library(sf)
library(lubridate)
library(mapview)
library(units)
library(dplyr)


###comparing significance between sites and years

require(here)
fly_dates<-read.csv(here("raw_data", "flydates_sbdo.csv"))
head(fly_dates)

 ##add J date column for each date
Jfly_datez<- fly_dates %>%
  subset(select=c("id", "dep_day", "lat_arcata_40.8", "lat_sand_32.7", "arr_day", "site", "year")) 
head(Jfly_datez)

Jfly<-Jfly_datez %>% 
  mutate(dep_day=mdy(dep_day)) %>%
  na.pass() %>% 
  mutate(Jdep_date=yday(dep_day)) %>% 
  mutate(lat_arcata_40.8=mdy(lat_arcata_40.8)) %>% 
  na.pass() %>% 
  mutate(Jlat_arcata_40.8=yday(lat_arcata_40.8)) %>% 
  mutate(lat_sand_32.7=mdy(lat_sand_32.7)) %>% 
  na.pass() %>% 
  mutate(Jlat_sand_32.7=yday(lat_sand_32.7)) %>% 
  mutate(arr_day=mdy(arr_day)) %>% 
  na.pass() %>% 
  mutate(Jarr_day=yday(arr_day))

##are there any differences between year for dep date
belmod<-lm(Jdep_date~site+factor(year), data=Jfly); summary(belmod)
anova(belmod)

belmod2<-lm(Jarr_day~site+factor(year), data=Jfly); summary(belmod2)

##no


##departure date by site both years
mod1<-lm(Jdep_date~site, data=Jfly); summary(mod1)
null1<-lm(Jdep_date~1, data=Jfly); summary(null1)



#departure date not sig different by site

##date south of arcata by site
mod2<-lm(Jlat_arcata_40.8~site, data=Jfly); summary(mod2)
null2<-lm(Jlat_arcata_40.8~1, data=Jfly); summary(null2)
##king salmon birds arrived south arcata an average of 23 days earlier than beluga birds

##date south of san diego by site
mod3<-lm(Jlat_sand_32.7~site, data=Jfly); summary(mod3)
null3<-lm(Jlat_sand_32.7~1, data=Jfly); summary(null3)
##king salmon birds arrived south of san diego an average of 22 days earlier than beluga birds 

##arrival date by site
mod4<-lm(Jarr_day~site, data=Jfly); summary(mod4)
null4<-lm(Jarr_day~1, data=Jfly); summary(null4)
##ks birds arrive at wintering sites an avg of 19 days earlier


##boxplots

dep<-ggplot(Jfly, aes(Jdep_date, site))+geom_boxplot(size=0.5, fill=c("brown3", "dodgerblue4"), alpha=0.75)+
  theme(legend.position="none", plot.title=element_text(hjust=0.5))%>% na.omit()+
  scale_x_continuous(limits=c(165, 244), breaks = c(166, 182,196,213,227, 244),
                     labels=c("Jun 15", "July 1","July 15","Aug 1", "Aug 15", "Sept 1"))+
  labs(y=NULL, x=NULL, title="Departure date from breeding site")+
  theme(panel.background = element_rect(fill = "white",color = "grey",size = 0.5,linetype='solid'))+
  theme(plot.title=element_text(size=30), axis.text=element_text(size=16),
        text = element_text(family = "Times New Roman")); dep

arcata<-ggplot(Jfly, aes(Jlat_arcata_40.8, site))+geom_boxplot(size=0.5, fill=c("brown3", "dodgerblue4"), alpha=0.75)+
  theme(legend.position="none", plot.title=element_text(hjust=0.5))%>% na.omit()+
  scale_x_continuous(limits=c(165, 244), breaks = c(166, 182,196,213,227, 244),
                     labels=c("Jun 15", "July 1","July 15","Aug 1", "Aug 15", "Sept 1"))+
  labs(y=NULL, x=NULL, title="Arrival date to Northern CA")+
  theme(panel.background = element_rect(fill = "white",color = "grey",size = 0.5,linetype='solid'))+
  theme(plot.title=element_text(size=30), axis.text=element_text(size=16),
        text = element_text(family = "Times New Roman")); arcata

sand<-ggplot(Jfly, aes(Jlat_sand_32.7, site))+geom_boxplot(size=0.5, fill=c("brown3", "dodgerblue4"), alpha=0.75)+
  theme(legend.position="none", plot.title=element_text(hjust=0.5))%>% na.omit()+
  scale_x_continuous(limits=c(165, 244), breaks = c(166, 182,196,213,227, 244),
                     labels=c("Jun 15", "July 1","July 15","Aug 1", "Aug 15", "Sept 1"))+
  labs(y=NULL, x=NULL, title="Arrival date to Southern CA")+
  theme(panel.background = element_rect(fill = "white",color = "grey",size = 0.5,linetype='solid'))+
  theme(plot.title=element_text(size=30), axis.text=element_text(size=16),
        text = element_text(family = "Times New Roman")); sand


arr<-ggplot(Jfly, aes(Jarr_day, site))+geom_boxplot(size=0.5, fill=c("brown3", "dodgerblue4"), alpha=0.75)+
  theme(legend.position="none", plot.title=element_text(hjust=0.5))%>% na.omit()+
  scale_x_continuous(limits=c(165, 244), breaks = c(166, 182,196,213,227, 244),
                     labels=c("Jun 15", "July 1","July 15","Aug 1", "Aug 15", "Sept 1"))+
  labs(y=NULL, x=NULL, title="Arrival date to wintering site")+
  theme(panel.background = element_rect(fill = "white",color = "grey",size = 0.5,linetype='solid'))+
  theme(plot.title=element_text(size=30), axis.text=element_text(size=16),
        text = element_text(family = "Times New Roman")); arr

