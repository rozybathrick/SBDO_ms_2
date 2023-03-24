library(aniMotum)
library(tidyverse)
install.packages("AICcmodavg")
library(AICcmodavg)
install.packages("Rtools")

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
    vmax=20)

summary(fitcrw24)

p24loc<- ploc <- grab(fitcrw24, what = "predicted", as_sf = TRUE)
ploc[1:15,]

## 24 hour time step (filling in every other day that didn't capture a fix)
fitcrw24_nospeed <-
  fit_ssm(
    x = sbdo_22,
    model = "crw",
    time.step = 24,
    vmax=15)

summary(fitcrw24)


## 12 hour time step

fitcrw12 <-
  fit_ssm(
    x = sbdo_22,
    model = "crw",
    time.step = 12,
    vmax=15)

summary(fitcrw12)

## 6 hour time step

fitcrw6 <-
  fit_ssm(
    x = sbdo_22,
    model = "crw",
    time.step = 6)

summary(fitcrw6)


## comparing models #this doesn't work
aictab(fitcrw12, fitcrw24)

aictable<-c(fitcrw12$AICc, fitcrw24$AICc)

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


fmp <- fit_mpm(fitcrw12, 
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
