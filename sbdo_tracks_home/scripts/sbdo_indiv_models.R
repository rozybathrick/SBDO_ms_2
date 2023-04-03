### fitting models to each track indepenently

library(aniMotum)
library(tidyverse)
library(TMB)
library(sf)
library(sp)

##bringing in dataset that I cleaned 3/27 - all data from Movebank, filtered for repeat dates

sbdo_mar<-(read.csv("raw_data/clean_SBDO_2022.csv"))
summary(sbdo_mar)

## running a 24 hour model for all birds to look at AICc together
allcrw_24<-
  fit_ssm(
    x = sbdo_mar,
    model = "crw",
    time.step = 24,
    vmax=20,
    control=ssm_control(verbose=0))
summary(allcrw_24)

plot(allcrw_24, what="predicted")

p_bird1<- grab(allcrw_24, what = "predicted", as_sf=TRUE) %>% 
  st_transform(coords=c("long", "lat"), crs=4326)



allcrw_48<-
  fit_ssm(
    x = sbdo_mar,
    model = "crw",
    time.step = 48,
    vmax=20,
    control=ssm_control(verbose=0))
summary(allcrw_48)

allcrw_36 <-
  fit_ssm(
  x = sbdo_mar,
  model = "crw",
  time.step = 36,
  vmax=20,
  control=ssm_control(verbose=0))
summary(allcrw_36)


## make dataframes for each 2022 bird

B_181624 <- filter(sbdo_mar, id == "181624")
B_181625 <- filter(sbdo_mar, id == "181625")
B_181626 <- filter(sbdo_mar, id == "181626")
B_233811 <- filter(sbdo_mar, id == "233811")
B_233812 <- filter(sbdo_mar, id == "233812")
B_233813 <- filter(sbdo_mar, id == "233813")
KS_233815 <- filter(sbdo_mar, id == "233815")
KS_233816 <- filter(sbdo_mar, id == "233816")
KS_233817 <- filter(sbdo_mar, id == "233817")
KS_233818 <- filter(sbdo_mar, id == "233818")
KS_233819 <- filter(sbdo_mar, id == "233819")
KS_233820 <- filter(sbdo_mar, id == "233820")
KS_233821 <- filter(sbdo_mar, id == "233821")
KS_233823 <- filter(sbdo_mar, id == "233823")

## also df for the two recapped SBDO from 2021 just to see whatsup
B21_198758_recap<-read.csv("raw_data/198758_Beluga_recap.csv")
B21_198767_recap<-read.csv("raw_data/198767_Beluga_recap.csv")


### trying out some models on the individual tracks

### BIRD ONE: Beluga 181624 ####

### first: 24 hour time step ###

## 24 hour time step with max speed of 20
B1_crw24 <-
  fit_ssm(
    x = B_181624,
    model = "crw",
    time.step = 24,
    vmax=20,
    control=ssm_control(verbose=0))

plot(B1_crw24, what="fitted")
plot(B1_crw24, what="predicted")

## 36 hour
B1_crw36 <-
  fit_ssm(
    x = B_181624,
    model = "crw",
    time.step = 36,
    vmax=20,
    control=ssm_control(verbose=0))

plot(B1_crw36, what="fitted")
plot(B1_crw36, what="predicted")

##add an mpm

B1_mpm<- fit_mpm(B1_crw24, 
                 what= "predicted",
                 model="mp",
                 control=mpm_control(verbose=0))
plot(B1_mpm)
map(B1_crw24, B1_mpm, what="predicted", silent=TRUE)

## visualizing as 2D tracks: this maps predicted plots on top of observations
plot(B1_crw24, "p", type=2, alpha=0.1)

## validating model - needs to be done with one-step-ahead-prediction residual
#calculate and plot residuals. 

##interp: the Q-Q plots are concerning, residuals / date look okay

res.bird1<-osar(B1_crw24)

(plot(res.bird1, type="ts")| plot(res.bird1, type="qq")) / (plot(res.bird1, type="acf"))

## 36 hour residuals
res.bird1_36<-osar(B1_crw36)

(plot(res.bird1_36, type="ts")| plot(res.bird1_36, type="qq")) / (plot(res.bird1_36, type="acf"))
summary(B1_crw36)
summary(B1_crw24)

## simulate track: this is mayhem
s1<-sim_fit(B1_crw24, what="p", reps=100)
plot(s1)

## grabbing predicted locations and se, AICc

p_bird1<- grab(B1_crw24, what = "predicted", as_sf=TRUE) %>% 
  st_transform(coords=c("long", "lat"), crs=4326)

summary(B1_crw24)


### Bird One at 12 hours (to compare models)

B1_crw12 <-
  fit_ssm(
    x = B_181624,
    model = "crw",
    time.step = 12,
    vmax=20,
    control=ssm_control(verbose=0))

plot(B1_crw12, what="fitted")
plot(B1_crw12, what="predicted")

##add an mpm

B1_mpm12<- fit_mpm(B1_crw12, 
                 what= "predicted",
                 model="mp",
                 control=mpm_control(verbose=0))
plot(B1_mpm12)
map(B1_crw12, B1_mpm12, what="predicted", silent=TRUE)

## visualizing as 2D tracks: this maps predicted plots on top of observations
plot(B1_crw12, "p", type=2, alpha=0.1)

## validating model - needs to be done with one-step-ahead-prediction residual
#calculate and plot residuals. 

##interp: 

res.bird1_12<-osar(B1_crw12)

(plot(res.bird1_12, type="ts")| plot(res.bird1_12, type="qq")) / (plot(res.bird1_12, type="acf"))


## grabbing predicted locations and se, AICc

p_bird1_12<- ploc <- grab(B1_crw12, what = "predicted", as_sf = TRUE, st_as_sf(coords=c("long", "lat"), crs=4326))
summary(B1_crw12)



## BIRD 1 AT 48

B1_crw48 <-
  fit_ssm(
    x = B_181624,
    model = "crw",
    time.step = 48,
    vmax=20,
    control=ssm_control(verbose=0))

plot(B1_crw48, what="fitted")
plot(B1_crw48, what="predicted")

## 36 hour residuals
res.bird1_48<-osar(B1_crw48)

(plot(res.bird1_48, type="ts")| plot(res.bird1_48, type="qq")) / (plot(res.bird1_48, type="acf"))
summary(B1_crw48)


## BIRD 2 ###

## 24 hour time step with max speed
B2_crw24 <-
  fit_ssm(
    x = KS_233815,
    model = "crw",
    time.step = 24,
    vmax=20,
    control=ssm_control(verbose=0))

plot(B2_crw24, what="fitted")
plot(B2_crw24, what="predicted")
summary(B2_crw24)

B2_crw6 <-
  fit_ssm(
    x = KS_233815,
    model = "crw",
    time.step = 6,
    vmax=20,
    control=ssm_control(verbose=0))

plot(B2_crw6, what="fitted")
plot(B2_crw6, what="predicted")
summary(B2_crw6)

##add an mpm

B2_mpm<- fit_mpm(B2_crw24, 
                 what= "predicted",
                 model="mp",
                 control=mpm_control(verbose=0))
plot(B2_mpm)
map(B1_crw24, B1_mpm, what="predicted", silent=TRUE)

## visualizing as 2D tracks: this maps predicted plots on top of observations
plot(B2_crw24, "p", type=2, alpha=0.1)

## validating model - needs to be done with one-step-ahead-prediction residual

require(patchwork)

#calculate and plot residuals

res.bird1<-osar(B1_crw24)

(plot(res.bird1, type="ts")| plot(res.bird1, type="qq")) / (plot(res.bird1, type="acf"))

## grabbing predicted locations and se, AICc

p_bird1<- ploc <- grab(B1_crw24, what = "predicted", as_sf = TRUE)
summary(p_bird1)

## grabbing fitted and filtered data locations
datacrw24<-grab(allcrw_24, what="data")

write.csv(datacrw24, file="crw24_datapoints.csv")
fittedcrw24<-grab(allcrw_24, what="fitted")
write.csv(fittedcrw24, file="crw24_fittedpoints.csv")
