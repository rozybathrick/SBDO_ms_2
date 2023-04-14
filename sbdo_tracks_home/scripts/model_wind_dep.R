### MODELING WIND AND DEPARTURE!! #####

library(lubridate)
library(MASS)
require(ggplot2)
require(GGally)
require(car)
require(AICcmodavg)
library(lme4)



## first need to set up the finished dataframe

dep_mod1 <- read.csv("raw_data/dep_mod_1.csv")

head(dep_mod1)

class(dep_mod1$id)
dep_mod1$id<-as.factor(dep_mod1$id)

dep_mod1$day.before.dep<-as.factor(dep_mod1$day.before.dep)

##first model - does wind differ between site?
mod.wind<-glm(mean_ws_sd~site, data=dep_mod1, family=gaussian)#mean daily wind speed
summary(mod.wind)

mod.wind2<-mod.wind2<-glm(max_ws_sd~site, data=dep_mod1, family=gaussian)#mean daily wind speed
summary(mod.wind2)

mod.tw<-glm(mean_tw_sd~site, data=dep_mod1, family=gaussian)
summary(mod.tw)

##yes - King Salmon has much stronger wind than Beluga

## standardizing tailwind
wind.std <- scale(dep_mod1[c("mean_tw", "mean_ws", "max_ws")])
colnames(wind.std) <- c("mean_tw_sd","mean_ws_sd", "max_ws_sd")
dep_mod1<-cbind(dep_mod1, wind.std)


### some data visualization 

ggplot(dep_mod2, aes(x = mean_ws, y = dep_prob, colour = site)) +
  geom_jitter(size = 2, alpha = 0.4, position = position_jitter(height = 0.02)) +
  stat_smooth(method = "loess", color = "blue") +
  theme_light()

ggplot(dep_mod2, aes(x = mean_tw, y = dep_prob, colour = site)) +
  geom_jitter(size = 2, alpha = 0.4, position = position_jitter(height = 0.02)) +
  stat_smooth(method = "loess", color = "blue") +
  theme_light()



## FIRST MODELS: including 7 days before departure

m1 <- glm(dep_prob ~ site + mean_tw, data = dep_mod1, family = binomial)
vif(m1) #they are not collinear

## i don't know how to include ID as a fixed effect here
fitList <- list(
  "null"     = glmer(dep_prob~1 + (1|id),              data = dep_mod1, family = binomial), #Null
  "Site"     = glmer(dep_prob~site+ (1|id),             data = dep_mod1, family = binomial), #ANOVA
  "tail wind"     = glmer(dep_prob~mean_tw_sd+ (1|id),      data = dep_mod1, family = binomial), #regression
  "wind speed"    = glmer(dep_prob~mean_ws_sd+ (1|id),      data = dep_mod1, family = binomial), #regression
  "max wind" = glmer(dep_prob~max_ws_sd+ (1|id),      data = dep_mod1, family = binomial), #regression
  "site tail" = glmer(dep_prob~mean_tw_sd+site+(1|id),      data = dep_mod1, family = binomial), #ANCOVA
  "site max wind" = glmer(dep_prob~max_ws_sd+site+(1|id), data = dep_mod1, family = binomial), #ANCOVA
  "site speed"= glmer(dep_prob~mean_ws_sd+site+ (1|id),      data = dep_mod1, family = binomial)  #ANCOVA
)



aictab(fitList, modnames = names(fitList))


mod.tw<- fitList[["tail wind"]]
summary(mod.tw)

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
  "tail wind"     = glmer(dep_prob~mean_tw_sd+ (1|id),      data = dep_mod2, family = binomial), #regression
  "wind speed"    = glmer(dep_prob~mean_ws_sd+ (1|id),      data = dep_mod2, family = binomial), #regression
  "site tail" = glmer(dep_prob~mean_tw_sd+site+(1|id),      data = dep_mod2, family = binomial), #ANCOVA
  "max wind speed" = glmer(dep_prob~max_ws_sd+ (1|id),      data = dep_mod2, family = binomial), #regression
  "site speed"= glmer(dep_prob~mean_ws_sd+site+ (1|id),      data = dep_mod2, family = binomial)  #ANCOVA
)

aictab(fitList2, modnames = names(fitList2))
##null is still the best model

summary(fitList2[["tail wind"]])
summary(fitList2[["site tail"]])
summary(fitList2[["Site"]])

### Try THREE: does wind condition the day before departure matter?

dep_mod1<-dep_mod1 %>% 
  mutate(day1=())

glm(day.before.dep~


