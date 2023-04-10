### MODELING WIND AND DEPARTURE!! #####

library(lubridate)
library(MASS)
require(ggplot2)
require(GGally)
require(car)
require(AICcmodavg)



## first need to set up the finished dataframe

dep_mod1 <- read.csv("raw_data/dep_mod_1.csv")

dep_mod1 <- dep_mod1 %>% 
  mutate(Jdate=(yday(date_opt_1)))

dep_mod1$id<-as.integer(dep_mod1$id)
class(dep_mod1$id)

head(dep_mod1)


### some data visualization 

ggplot(dep_mod1, aes(x = mean_ws, y = dep_prob, colour = site)) +
  geom_jitter(size = 2, alpha = 0.4, position = position_jitter(height = 0.02)) +
  stat_smooth(method = "loess", color = "blue") +
  theme_light()

ggplot(dep_mod1, aes(x = mean_tw, y = dep_prob, colour = site)) +
  geom_jitter(size = 2, alpha = 0.4, position = position_jitter(height = 0.02)) +
  stat_smooth(method = "loess", color = "blue") +
  theme_light()

## some models


m1 <- glm(dep_prob ~ site + mean_tw, data = dep_mod1, family = binomial)
vif(m1) #they are not collinear

## i don't know how to include ID as a fixed effect here
fitList <- list(
  "null"     = glm(dep_prob~1,                  data = dep_mod1, family = binomial), #Null
  "Site"     = glm(dep_prob~site + (1|id),      data = dep_mod1, family = binomial), #ANOVA
  "tail wind"     = glm(dep_prob~mean_tw,       data = dep_mod1, family = binomial), #regression
  "wind speed"    = glm(dep_prob~mean_ws,       data = dep_mod1, family = binomial), #regression
  "site tail" = glm(dep_prob~mean_tw+site,      data = dep_mod1, family = binomial), #ANCOVA 
  "site speed"= glm(dep_prob~mean_ws+site,      data = dep_mod1, family = binomial)  #ANCOVA
)

glm(dep~site + (1|id))

aictab(fitList, modnames = names(fitList))
mod.tw<- fitList[["tail wind"]]
mod.site<-fitList[["Site"]]
summary(mod.tw)
mod.ws<-fitList[["wind speed"]]
mod.sitetw<- fitList[["site tail"]]
summary(mod.ws)
summary(mod.site)

#deviance and degrees of freedom
twdev <- summary(mod.tw)$deviance     # deviance
twdof <- summary(mod.tw)$df.residual  #residual deg. of freedom

#Chi-square test (>0.05 indicates adequate fit)
1 - pchisq(twdev, twdof)

#deviance and degrees of freedom
Stwdev <- summary(mod.sitetw)$deviance     # deviance
Stwdof <- summary(mod.sitetw)$df.residual  #residual deg. of freedom
1 - pchisq(Stwdev, Stwdof)

