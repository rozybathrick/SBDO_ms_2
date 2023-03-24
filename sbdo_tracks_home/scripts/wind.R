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

getwd()
ks_wind <- raster("raw_data/ks_wind.nc")
print(ks_wind)
ks_wind_r <- rotate(ks_wind)
ks_wind_r
plot(ks_wind_r)

df_ks<-as.data.frame(ks_wind, xy=TRUE, time=TRUE)

### trying something new 

nc_ks_wind <- nc_open('raw_data/ks_wind.nc')

# Save the print(nc) dump to a text file
{
  sink('ks_wind.txt')
  print(nc_ks_wind)
  sink()
}

# save attributes independently 
lon <- ncvar_get(nc_ks_wind, "longitude")
lat <- ncvar_get(nc_ks_wind, "latitude", verbose = F)
t <- ncvar_get(nc_ks_wind, "time")

head(lon)
head(lat)
head(t)
dim(lon)
dim(lat)
dim(t)
## create arrays

u <- ncvar_get(nc_ks_wind, "u") # store the data in a 3-dimensional array
dim(u)
v<-ncvar_get(nc_ks_wind, "v") # store the data in a 3-dimensional array
dim(v)

## combining into dataframe

ks_df<-data.frame(lon, lat, t, u, v) #it has so many columns I don't know why
dim(ks_df)

## 2208 = number of time stamps (24 hours * 30 days * 3 months)
## 13,251 = number of columns (don't know why there are so mnany. I thought there should be two: V and U)
## 13251/2208 = 6, so maybe it's giving us 6 values for each hour?