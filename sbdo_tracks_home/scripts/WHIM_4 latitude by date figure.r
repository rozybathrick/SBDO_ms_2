## Whimbrel: plot latitude by date ##
## Jump to line 106 to get to plotting functions; all preceding is summarizing dataset

# library(plyr) Seems like some issues we encounted in the summary loop relate to plyr masking dplyr functions?
library(dplyr)
library(tidyr)
library(psych)
library(ggplot2)
library(lubridate)


# Set working directory, import and modify working file
# Working file is from TLT, and is Douglas-filtered, best-location for each duty cycle (typically a standard loc)

sbdo_lat<-read.csv(file = "9.19.22_allyears.csv")
View(X9_19_22_allyears)

sbdo_lat<-X9_19_22_allyears
# Refine to necessary fields
sbdo1<-subset(sbdo_lat,select=c('tag-local-identifier','timestamp','location-long','location-lat', 'Capture-year', 'Breeding-site')) %>% 
  rename(id="tag-local-identifier",
       long="location-long",
       lat="location-lat",
       site="Breeding-site",
       year="Capture-year") %>% 
  mutate(timestamp=dmy(timestamp))
view(sbdo1)

# So, some different approaches. Could just plot a single year (say, 2011)

sday<- sbdo1 %>% mutate(DayDate=day(timestamp)) %>% mutate(MonthDate=month(timestamp)) %>%
  unite(DayMonth, c("DayDate", "MonthDate"), sep = "-")

sdayJ <- sday %>% 
  mutate(Jdate=yday(timestamp))



s1<-subset(sdayJ,sdayJ$year==2021)
s2<-subset(sdayJ, sdayJ$year==2022)

p1<-ggplot(s2, aes(x=timestamp, y=lat,color=site))+geom_point()+geom_line();p1
p2<-ggplot(s1, aes(x=timestamp, y=lat,color=site))+geom_point()+geom_line();p2
p3<-ggplot(s1, aes(x=Jdate, y=lat,color=site, group=id))+geom_point()+geom_line()+xlim(150,300)+ylim(20, 65);p3
p4<-ggplot(s2, aes(x=Jdate, y=lat,color=site, group=id))+geom_point()+geom_line()+xlim(150,300)+ylim(20, 65);p4


# I think it would be better to determine the daily latitude for each bird, and then estimate
#   mean +/- 95% CI for birds with repeats. This tricky, though; requires filling in latitude for each day of year for each bird.

# Because each bird has a deployment date and final transmission date...Vijay helped
#   with code to split out each bird, and use min and max date values for animal-specific ranges

# Need to 'fill in' latitudes and carry dates in-between reporting cycles
# Code from Vijay; see further, an attempt nabbed from the web that I could not get to work!
whim1$date<-as.Date(whim1$date, "%m/%d/%Y")
whim1$animal<-as.character(whim1$animal);str(whim1)
# Fill missing dates for each bird
animals=unique(whim1$animal)

all<-data.frame() #create new df for results.
for(i in 1:length(animals)) #subset with a loop
{
  sub=whim1[whim1$animal==animals[i],]
  
  sub<- sub %>%
    mutate(date=as.Date(date)) %>%
    complete(date=seq.Date(min(date),max(date),by="day")) %>%
    fill('animal') 
  
  sub<-fill(sub,latitude,longitud)
  
  all<-rbind(all,sub)
  
}

all$ordDate<-yday(all$date)
all$year<-year(all$date)
all$year<-as.factor(all$year)

# Export to csv
write.csv(all, file ="C:/Data/Species/Curlews/Whimbrel/Manuscripts WHIM/WHIM migratory movements/Whimbrel analysis & output/WHIM lat by date.csv",quote=FALSE,append=FALSE,na="NA")

# Calculate mean latitude per day + 95% CI for plotting
# Function from http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/#helper-functions
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summarized
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
#### 09.01.2020: this loop ran w/o issues originally, but now has hiccups; I wrote a work-around beginning on line 107
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  #library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  #datac <- rename(datac, c("mean" = measurevar))
  datac<-rename(datac,setNames("mean",measurevar))
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

whim_summary<-summarySE(all,measurevar = "latitude",groupvars = c("animal","ordDate"))
# Weird. Despite running flawlessly previously, this loop no longer seems to run

# Create summaries by hand via aggregate
ordDayMean<-aggregate(all$latitude,by=list(all$animal,all$ordDate),FUN=mean)
colnames(ordDayMean)[1]<-"animal";colnames(ordDayMean)[2]<-"ordDate";colnames(ordDayMean)[3]<-"latitude"
ordDaySD<-aggregate(all$latitude,by=list(all$animal,all$ordDate),FUN=sd)
colnames(ordDaySD)[1]<-"animal";colnames(ordDaySD)[2]<-"ordDate";colnames(ordDaySD)[3]<-"sd"
ordDayN<-aggregate(all$latitude,by=list(all$animal,all$ordDate),FUN=length)
colnames(ordDayN)[1]<-"animal";colnames(ordDayN)[2]<-"ordDate";colnames(ordDayN)[3]<-"N"
a<-cbind(ordDayMean,ordDaySD);b<-cbind(a,ordDayN);whim_summary<-b[,-c(4,5,7,8)]
whim_summary$se<-(whim_summary$sd/sqrt(whim_summary$N)) # calculate SE
# calculate 95% CIs
whim_summary$ciMult<-qt(.975, whim_summary$N-1);whim_summary$ci<-whim_summary$se*whim_summary$ciMult
# sort 
whim_summary<-whim_summary[order(whim_summary$animal,whim_summary$ordDate),]
## longitude, too: Chris wanted this on 9/15/2021
ordDayMean_long<-aggregate(all$longitud,by=list(all$animal,all$ordDate),FUN=mean)
colnames(ordDayMean_long)[1]<-"animal";colnames(ordDayMean)[2]<-"ordDate";colnames(ordDayMean)[3]<-"longitude"
ordDaySD_long<-aggregate(all$longitud,by=list(all$animal,all$ordDate),FUN=sd)
colnames(ordDaySD_long)[1]<-"animal";colnames(ordDaySD)[2]<-"ordDate";colnames(ordDaySD)[3]<-"long_sd"
ordDayN_long<-aggregate(all$longitud,by=list(all$animal,all$ordDate),FUN=length)
colnames(ordDayN_long)[1]<-"animal";colnames(ordDayN)[2]<-"ordDate";colnames(ordDayN)[3]<-"long_N"

a1<-cbind(ordDayMean_long,ordDaySD_long);b1<-cbind(a1,ordDayN_long);whim_summary_long<-b1[,-c(4,5,7,8)]
a2<-cbind(ordDayMean_long,whim_summary)
a2<-a2[order(a2$animal,a2$ordDate),]

# Extract the record for each bird with the lowest latitude to get each bird's nonbreeding location
nonbreed_loc<-a2[(aggregate(a2$latitude,by=list(a2$animal),FUN=min)),]
nonbreed_loc<-subset(a2,a2$latitude==aggregate(a2$latitude,by=list(a2$animal),FUN=min))
# These don't work...tryign to extract record for each bird where latitude is lowest (ie, its wintering location)
# I'll do this by sorting in excel...wasting too much time...
write.csv(a2, file ="C:/Data/Species/Curlews/Whimbrel/Manuscripts WHIM/WHIM migratory movements/Whimbrel analysis & output/WHIM mean and CI lat long by animal by date.csv",quote=FALSE,append=FALSE,na="NA")

# Export to csv
write.csv(whim_summary, file ="C:/Data/Species/Curlews/Whimbrel/Manuscripts WHIM/WHIM migratory movements/Whimbrel analysis & output/WHIM mean and CI lat by animal by date.csv",quote=FALSE,append=FALSE,na="NA")

### PLOTTING LATITUDE BY DOY ###
# Working file is from TLT, and is Douglas-filtered, best-location for each duty cycle (typically a standard loc)
setwd("C:/Data/Species/Curlews/Whimbrel/Manuscripts WHIM/WHIM migratory movements/Whimbrel analysis & output")
whim<-read.csv(file = "WHIM mean and CI lat by animal by date.csv")
library(ggplot2)
library(extrafont) # To use Calibri font in p3...
loadfonts("win")
# If plot as geom_line, it connects dates non-sensically for birds that go off air in winter,
#   connecting them back to their deployment location...so, I manually added records with an extra date and lat=NA
#   for these birds to keep the lines from connecting
# The cis are quite huge for some birds during some periods when then don't move with consistent timing across years
# The values go off the scale....perhaps try plotting sds or ses?
# So, here it is with CI
p1<-ggplot(whim,aes(x=ordDate,y=latitude,group=animal))+geom_line(size=.5)+
  geom_ribbon(aes(ymin=latitude-ci,ymax=latitude+ci),alpha=.2)
p1
# Try SEs: Better; maintains scale ,
p2<-ggplot(whim,aes(x=ordDate,y=latitude,group=animal,color=popn))+geom_line()+
  scale_x_continuous(breaks = c(1,32,60,91,121,152,182,213,244,274,305,335),labels=c("Jan","","Mar","","May","","Jul","","Sep","","Nov",""))+
  geom_ribbon(aes(ymin=latitude-se,ymax=latitude+se),alpha=.2,color=NA)+# color=NA removes colored borders to ribbon
  scale_color_manual(values=c("#D55E00","#56B4E9","#009E73"))+# red=Colville,blue = kanuti,green=SP; color-blind friendly colors
  labs(x="Month",y="Latitude (Degrees)")+ylim(-45,74)+
  theme(legend.position="none",panel.background = element_rect(fill = "white",color = "grey",size = 0.25,linetype='solid'),
        panel.grid.major = element_line(size = 0.25,linetype='solid',color="grey"),
        panel.grid.minor = element_blank(),axis.text=element_text(size = 14),axis.title=element_text(size=16))
p2

# Try rearranging the scale: start at June 1 (ordinal date 152)
# Create new scale for ordDate
whim$ordDate2<-ifelse(whim$ordDate>151,whim$ordDate-151,whim$ordDate+214)

p3<-ggplot(whim,aes(x=ordDate2,y=latitude,group=animal,color=popn))+geom_line()+
  scale_x_continuous(breaks = c(1,31,62,93,123,154,184,215,246,274,305,335),labels=c("Jun","","Aug","","Oct","","Dec","","Feb","","Apr",""))+
  geom_ribbon(aes(ymin=latitude-se,ymax=latitude+se),alpha=.2,color=NA)+# color=NA removes colored borders to ribbon
  scale_color_manual(values=c("#D55E00","#56B4E9","#009E73"))+# red=Colville,blue = kanuti,green=SP; color-blind friendly colors
  labs(x="Month",y="Latitude (degrees)")+ylim(-45,74)+
  theme(legend.position="none",panel.background = element_rect(fill = "white",color = "grey",size = 0.25,linetype='solid'),
        panel.grid.major = element_line(size = 0.25,linetype='solid',color="grey"),
        panel.grid.minor = element_blank(),axis.text=element_text(size = 14),axis.title=element_text(size=16),
        text=element_text(family="Calibri"))
p3







# How to fill 'missing' dates and latitudes? I found a very helpful script at:
# https://blog.exploratory.io/populating-missing-dates-with-complete-and-fill-functions-in-r-and-exploratory-79f2a321e6b5
# Proofed by Vijay: statements were out of order apparently...
test<-read.csv('test.csv')
#test<-read.csv('date_gapfill_demo.csv') #simple dataset to make sure it is working.
test$date<-as.Date(test$date,"%m/%d/%Y")
test$animal<-as.character(test$animal);str(test)
# Fill missing dates for each bird
test %>%
  mutate(date=as.Date(date)) %>%
  group_by(animal) %>% #group by statement was in the wrong place in the online example.
  complete(date=seq.Date(min(date),max(date),by="day")) %>%
  fill('latitude')


