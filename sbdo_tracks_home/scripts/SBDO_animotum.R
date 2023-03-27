library(aniMotum)
library(tidyverse)
install.packages("TMB", type='source')


R.version


##load 2022 data
sbdo_22<-read.csv("raw_data/9.29.22_fall_22_animotum.csv")


## format, prefilter and fit correlated Random Walk SSM using a 24 h time step

## correlated random walk = Movements are random and correlated in direction and magnitude

##48 hour time step

fitcrw48 <-
  fit_ssm(
    x = sbdo_22,
    model = "crw",
    time.step = 48)

summary(fitcrw48)

## 24 hour time step with max speed (filling in every other day that didn't capture a fix)
fitcrw24_speed <-
  fit_ssm(
    x = sbdo_22,
    model = "crw",
    time.step = 24,
    vmax=20,
    control=ssm_control(verbose=0))

## model visualization

plot(fitcrw24_speed, what="fitted")
plot(fitcrw24_speed, what="predicted")

## visualizing as 2D tracks: this maps predicted plots on top of observations
plot(fitcrw24_speed, "p", type=2, alpha=0.1)

## validating model - needs to be done with one-step-ahead-prediction residual

require(patchwork)

#calculate and plot residuals

res.24<-osar(fitcrw24_speed)

bird1<-filter(res.24, id=="233823_King_Salmon")

  (plot(bird1, type="ts")| plot(bird1, type="qq")) / (plot(bird1, type="acf"))

summary(fitcrw24)

p24loc<- ploc <- grab(fitcrw24, what = "predicted", as_sf = TRUE)
ploc[1:15,]

## 24 hour time step (filling in every other day that didn't capture a fix)
fitcrw24_nospeed <-
  fit_ssm(
    x = sbdo_22,
    model = "crw",
    time.step = 24,
    vmax=15,
    control=ssm_control(verbose=0))

summary(fitcrw24)


## 12 hour time step

fitcrw12 <-
  fit_ssm(
    x = sbdo_22,
    model = "crw",
    time.step = 12,
    vmax=20,
    control=ssm_control(verbose=0))

## 12 hour model visualization

plot(fitcrw12, what="fitted")
plot(fitcrw12, what="predicted")

summary(fitcrw12)

## 6 hour time step

fitcrw6 <-
  fit_ssm(
    x = sbdo_22,
    model = "crw",
    time.step = 6)

summary(fitcrw6)


# plot fitted locations as 1-D timeseries
plot(fitcrw24,
     what = "p", 
     pages = 0,
     type=1,
     ask=FALSE)

map(fitcrw24,
    what = "p", 
    pages = 0,
    ask=FALSE,
    by.id=TRUE)


##filtering just one individual to see track alone
KS_test<-filter(fitcrw12, id == "233821_King_Salmon")

plot(KS_test,
     what = "p", 
     pages = 0,
     type=1,
     ask=FALSE)

map(KS_test,
    what = "p", 
    pages = 0,
    ask=FALSE)



## movement persistence model = Movements are random with correlation in direction and magnitude that varies in time.
## fit_mpm(), which can take as input either location data or SSM-estimated locations from an fit_ssm() model fit object. 
## This approach is generally more appropriate when the data have minimal measurement error


fmp24 <- fit_mpm(fitcrw24_dep, 
               what = "predicted", 
               model = "mpm",
               control = mpm_control(verbose = 0))


# plot fitted locations as 1-D timeseries
plot(fmp, ask = FALSE)

map(fitcrw,
     what = "p", 
     pages = 0,
     ask=FALSE)


map(fitcrw12, fmp, what = "predicted", silent = TRUE)

floc <- grab(fit, what = "fitted")
ploc<-grab(fit, what="predicted", as_sf = TRUE)



#### filtering data for just the first couple of months, to look at Departure window

sbdo_dep<-filter(sbdo_22, date < '2022-08-01 00:00:00')

## 24 hour time step with max speed (filling in every other day that didn't capture a fix)
fitcrw24_dep <-
  fit_ssm(
    x = sbdo_dep,
    model = "crw",
    time.step = 24,
    vmax=20,
    control=ssm_control(verbose=0))

## model visualization

plot(fitcrw24_dep, what="fitted")
plot(fitcrw24_dep, what="predicted")

## visualizing as 2D tracks: this maps predicted plots on top of observations
plot(fitcrw24_dep, "p", type=2, alpha=0.1)

## validating model - needs to be done with one-step-ahead-prediction residual

require(patchwork)

#calculate and plot residuals

res.24_dep<-osar(fitcrw24_dep)

bird1_dep<-filter(res.24_dep, id=="233823_King_Salmon")

(plot(bird1_dep, type="ts")| plot(bird1_dep, type="qq")) / (plot(bird1_dep, type="acf"))



p24loc<- ploc <- grab(fitcrw24, what = "predicted", as_sf = TRUE)
ploc[1:15,]
