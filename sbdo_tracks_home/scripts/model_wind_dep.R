### MODELING WIND AND DEPARTURE!! #####

library(lubridate)
library(MASS)
require(ggplot2)
require(GGally)
require(car)
require(AICcmodavg)
library(lme4)



## first need to set up the finished dataframe
getwd()

dep_mod1 <- read.csv("rWind_birds.csv")

head(dep_mod1)

class(dep_mod1$id)
dep_mod1$id<-as.factor(dep_mod1$id)

dep_mod1$day.before.dep<-as.integer(dep_mod1$day.before.dep)

## standardizing tailwind
wind.std <- scale(dep_mod1[c("mean.speed", "mean.tw", "max.speed")])
colnames(wind.std) <- c("mean.speed_sd","mean.tw_sd", "max.speed_sd")
dep_mod1<-cbind(dep_mod1, wind.std)

##first model - does wind differ between site?
mod.wind<-glm(mean.speed_sd~site, data=dep_mod1, family=gaussian)#mean daily wind speed
summary(mod.wind) 

mod.wind2<-glm(max.speed_sd~site, data=dep_mod1, family=gaussian)#mean daily wind speed
summary(mod.wind2)

mod.tw<-glm(mean.tw_sd~site, data=dep_mod1, family=gaussian)
summary(mod.tw)

##yes - King Salmon stronger wind than Beluga, but not more direct tailwinds

### some data visualization 

ggplot(dep_mod1, aes(x = mean_tw, y = day_4, colour = site)) +
  geom_jitter(size = 2, alpha = 0.4, position = position_jitter(height = 0.02)) +
  stat_smooth(method = "loess", color = "blue") +
  theme_light()

ggplot(dep_mod1, aes(x = mean_tw, y = day_4, colour = site)) +
  geom_jitter(size = 2, alpha = 0.4, position = position_jitter(height = 0.02)) +
  stat_smooth(method = "loess", color = "blue") +
  theme_light()



## FIRST MODELS: including 7 days before departure

m1 <- glm(dep_prob ~ site + mean_tw, data = dep_mod1, family = binomial)
vif(m1) #they are not collinear


fitList1 <- list(
  "null"     = glmer(dep_prob~1 + (1|id),              data = dep_mod1, family = binomial), #Null
  "Site"     = glmer(dep_prob~site+ (1|id),             data = dep_mod1, family = binomial), #ANOVA
  "tail wind"     = glmer(dep_prob~mean.tw_sd+ (1|id),      data = dep_mod1, family = binomial), #regression
  "wind speed"    = glmer(dep_prob~mean.speed_sd+ (1|id),      data = dep_mod1, family = binomial), #regression
  "max wind" = glmer(dep_prob~max.speed_sd+ (1|id),      data = dep_mod1, family = binomial), #regression
  "site tail" = glmer(dep_prob~mean.tw_sd+site+(1|id),      data = dep_mod1, family = binomial), #ANCOVA
  "site max wind" = glmer(dep_prob~max.speed_sd+site+(1|id), data = dep_mod1, family = binomial), #ANCOVA
  "site speed"= glmer(dep_prob~mean.speed_sd+site+ (1|id),      data = dep_mod1, family = binomial)  #ANCOVA
)



aictab(fitList1, modnames = names(fitList1))


summary(fitList1[["tail wind"]])

mod.site<-fitList[["Site"]]
summary(mod.site)

mod.ws<-fitList[["wind speed"]]
summary(mod.ws)

mod.sitetw<- fitList[["site tail"]]
summary(mod.sitetw)

mod.null<-fitList[["null"]]
summary(mod.null)

### Try TWO: Including just 3 days before departure 

dep_mod2<-dep_mod1 %>% 
  filter(dep_mod1$day.before.dep <=3)

fitList2 <- list(
  "null"     = glmer(dep_prob~1 + (1|id),              data = dep_mod2, family = binomial), #Null
  "Site"     = glmer(dep_prob~site+ (1|id),             data = dep_mod2, family = binomial), #ANOVA
  "tail wind"     = glmer(dep_prob~mean.tw_sd+ (1|id),      data = dep_mod2, family = binomial), #regression
  "wind speed"    = glmer(dep_prob~mean.speed_sd+ (1|id),      data = dep_mod2, family = binomial), #regression
  "max wind" = glmer(dep_prob~max.speed_sd+ (1|id),      data = dep_mod2, family = binomial), #regression
  "site tail" = glmer(dep_prob~mean.tw_sd+site+(1|id),      data = dep_mod2, family = binomial), #ANCOVA
  "site max wind" = glmer(dep_prob~max.speed_sd+site+(1|id), data = dep_mod2, family = binomial), #ANCOVA
  "site speed"= glmer(dep_prob~mean.speed_sd+site+ (1|id),      data = dep_mod2, family = binomial)  #ANCOVA
)

aictab(fitList2, modnames = names(fitList2))
##null is still the best model

summary(fitList2[["tail wind"]])
summary(fitList2[["site tail"]])
summary(fitList2[["Site"]])

### Try THREE: does wind condition the day before departure matter?

fitList3 <- list(
  "null"     = glmer(day_1~1 + (1|id),              data = dep_mod1, family = binomial), #Null
  "Site"     = glmer(day_1~site+ (1|id),             data = dep_mod1, family = binomial), #ANOVA
  "tail wind"     = glmer(day_1~mean.tw_sd+ (1|id),      data = dep_mod1, family = binomial), #regression
  "wind speed"    = glmer(day_1~mean.speed_sd+ (1|id),      data = dep_mod1, family = binomial), #regression
  "max wind" = glmer(day_1~max.speed_sd+ (1|id),      data = dep_mod1, family = binomial), #regression
  "site tail" = glmer(day_1~mean.tw_sd+site+(1|id),      data = dep_mod1, family = binomial), #ANCOVA
  "site max wind" = glmer(day_1~max.speed_sd+site+(1|id), data = dep_mod1, family = binomial), #ANCOVA
  "site speed"= glmer(day_1~mean.speed_sd+site+ (1|id),      data = dep_mod1, family = binomial)  #ANCOVA
)

aictab(fitList3, modnames = names(fitList3))
summary(fitList3[["max wind"]])
summary(fitList3[["tail wind"]])
summary(fitList3[["wind speed"]])

## close to significant but not really....okay I've got to bail this is driving me insane

### Try FOUR: does wind condition two days before matter?

fitList4 <- list(
  "null"     = glmer(day_2~1 + (1|id),              data = dep_mod1, family = binomial), #Null
  "Site"     = glmer(day_2~site+ (1|id),             data = dep_mod1, family = binomial), #ANOVA
  "tail wind"     = glmer(day_2~mean_tw_sd+ (1|id),      data = dep_mod1, family = binomial), #regression
  "wind speed"    = glmer(day_2~mean_ws_sd+ (1|id),      data = dep_mod1, family = binomial), #regression
  "site tail" = glmer(day_2~mean_tw_sd+site+(1|id),      data = dep_mod1, family = binomial), #ANCOVA
  "max wind speed" = glmer(day_2~max_ws_sd+ (1|id),      data = dep_mod1, family = binomial), #regression
  "site speed"= glmer(day_2~mean_ws_sd+site+ (1|id),      data = dep_mod1, family = binomial)  #ANCOVA
)

aictab(fitList4, modnames = names(fitList4))
summary(fitList4[["tail wind"]])
summary(fitList4[["site tail"]])
## tail wind four days before is slightly significant (0.02)


### Try FIVE: does wind condition three days before matter?
fitList5 <- list(
  "null"     = glmer(day_3~1 + (1|id),              data = dep_mod1, family = binomial), #Null
  "Site"     = glmer(day_3~site+ (1|id),             data = dep_mod1, family = binomial), #ANOVA
  "tail wind"     = glmer(day_3~mean_tw_sd+ (1|id),      data = dep_mod1, family = binomial), #regression
  "wind speed"    = glmer(day_3~mean_ws_sd+ (1|id),      data = dep_mod1, family = binomial), #regression
  "site tail" = glmer(day_3~mean_tw_sd+site+(1|id),      data = dep_mod1, family = binomial), #ANCOVA
  "max wind speed" = glmer(day_3~max_ws_sd+ (1|id),      data = dep_mod1, family = binomial), #regression
  "site speed"= glmer(day_3~mean_ws_sd+site+ (1|id),      data = dep_mod1, family = binomial)  #ANCOVA
)
aictab(fitList5, modnames = names(fitList5))
summary(fitList5[["wind speed"]])

### Try SIX: does wind condition four days before matter?
fitList6 <- list(
  "null"     = glmer(day_4~1 + (1|id),              data = dep_mod1, family = binomial), #Null
  "Site"     = glmer(day_4~site+ (1|id),             data = dep_mod1, family = binomial), #ANOVA
  "tail wind"     = glmer(day_4~mean_tw_sd+ (1|id),      data = dep_mod1, family = binomial), #regression
  "wind speed"    = glmer(day_4~mean_ws_sd+ (1|id),      data = dep_mod1, family = binomial), #regression
  "site tail" = glmer(day_4~mean_tw_sd+site+(1|id),      data = dep_mod1, family = binomial), #ANCOVA
  "max wind speed" = glmer(day_4~max_ws_sd+ (1|id),      data = dep_mod1, family = binomial), #regression
  "site speed"= glmer(day_4~mean_ws_sd+site+ (1|id),      data = dep_mod1, family = binomial)  #ANCOVA
)

aictab(fitList6, modnames = names(fitList6))
summary(fitList6[["tail wind"]])

## tail wind four days before is significant (0.05)

### Try SEVEN: does wind condition five days before matter?
fitList7 <- list(
  "null"     = glmer(day_5~1 + (1|id),              data = dep_mod1, family = binomial), #Null
  "Site"     = glmer(day_5~site+ (1|id),             data = dep_mod1, family = binomial), #ANOVA
  "tail wind"     = glmer(day_5~mean_tw_sd+ (1|id),      data = dep_mod1, family = binomial), #regression
  "wind speed"    = glmer(day_5~mean_ws_sd+ (1|id),      data = dep_mod1, family = binomial), #regression
  "site tail" = glmer(day_5~mean_tw_sd+site+(1|id),      data = dep_mod1, family = binomial), #ANCOVA
  "max wind speed" = glmer(day_5~max_ws_sd+ (1|id),      data = dep_mod1, family = binomial), #regression
  "site speed"= glmer(day_5~mean_ws_sd+site+ (1|id),      data = dep_mod1, family = binomial)  #ANCOVA
)

aictab(fitList7, modnames = names(fitList6))
summary(fitList7[["tail wind"]])

## null is best
