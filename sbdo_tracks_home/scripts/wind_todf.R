##wind

install.packages("stars")
install.packages("ncmeta")
install.packages("raster")
install.packages("rgdal")
library(stars)
library(dplyr)
library(ncmeta)
library(raster)
install.packages("ncdf4")
library(ncdf4)
library(rgdal) # package for geospatial analysis
library(lubridate)

### KING SALMON ####
getwd()
ks_wind2<-nc_open("raw_data/ks_wind.nc")
print(ks_wind2)

attributes(ks_wind2$var)
attributes(ks_wind2$dim)


# Get latitude and longitude with the ncvar_get function and store each into their own object:
ks_lat <- ncvar_get(ks_wind2, "latitude")
ks_lon <- ncvar_get(ks_wind2, "longitude")
time<-ncvar_get(ks_wind2, "time")

## extract wind data

ks_wind_array_u <- ncvar_get(ks_wind2, "u")
dim(u)

ks_wind_array_v <- ncvar_get(ks_wind2, "v") 

## fix time

time_obs <- as.POSIXct(time*3600, origin = "1900-01-01", tz="GMT")
dim(time_obs)
range(time_obs)

## into dataframe
lonlattime<-as.matrix(expand.grid(lon, lat, time_obs))
u_wind_array<-as.vector(ks_wind_array_u)
v_wind_array<-as.vector(ks_wind_array_v)                       
KS_wind_final<-data.frame(cbind(lonlattime, v_wind_array, u_wind_array))
head(KS_wind_final)


View(KS_wind_final)
colnames(KS_wind_final) <- c("Long", "Lat", "Timestamp", "v", "u")
write.csv(KS_wind_final, file="KS_wind_final.csv")

##### BELUGA ######

getwd()
bel_wind<-nc_open("raw_data/beluga_wind.nc")
print(bel_wind)

attributes(bel_wind$var)
attributes(bel_wind$dim)


# Get latitude and longitude with the ncvar_get function and store each into their own object:
bel_lat <- ncvar_get(bel_wind, "latitude")
bel_lon <- ncvar_get(bel_wind, "longitude")
bel_time<-ncvar_get(bel_wind, "time")

## extract wind data

bel_array_u <- ncvar_get(bel_wind, "u")

bel_array_v <- ncvar_get(bel_wind, "v")

## fix time

bel_time_obs <- as.POSIXct(bel_time*3600, origin = "1900-01-01", tz="GMT")
dim(bel_time_obs)
range(bel_time_obs)

## into dataframe
bel_lonlattime<-as.matrix(expand.grid(bel_lon, bel_lat, bel_time_obs))
bel_u_wind_array<-as.vector(bel_array_u)
bel_v_wind_array<-as.vector(bel_array_v)                       
BELUGA_wind_final<-data.frame(cbind(bel_lonlattime, bel_v_wind_array, bel_u_wind_array))
head(BELUGA_wind_final)


View(BELUGA_wind_final)
colnames(BELUGA_wind_final) <- c("Long", "Lat", "Timestamp", "v", "u")
write.csv(BELUGA_wind_final, file="BELUGA_wind_final.csv")


