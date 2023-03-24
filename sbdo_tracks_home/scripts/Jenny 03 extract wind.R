library(stars)
library(dplyr)
library(RANN)
library(sp)
library(rgeos)
library(tidyr)
library(lubridate)
library(raster)
library(geosphere)
library(sf)
library(ggplot2)
library(move)
library(sf)
library(lme4)
library(lmerTest)
library(MuMIn)
library(nlme)
library(arm)
library(REdaS)

options(scipen = 999)

getwd()

###############################################
## PREPARE DATA 
###############################################

# data:
# 16 pressure levels: c("500", "550", "600", "650", "700", "750", "775", "800", "825", "850", "875", "900", "925", "950", "975", "1000")
# area: "45/-103/-43/-72"


## READ DATA INTO A MULTI-DIMENSIONAL ARRAY  ------------------------

beluga_wind <- read_stars("raw_data/beluga_adaptor.mars.internal-1678228485.4156082-12592-6-a63fe570-53be-4b4f-852e-fece8e6c6429.nc", proxy=TRUE)
st_crs(beluga_wind) = 4326
head(beluga_wind)

april19 <- read_stars("april2019.nc", proxy = TRUE)
st_crs(april19) = 4326

may19 <- read_stars("may2019.nc", proxy = TRUE)
st_crs(may19) = 4326

april20 <- read_stars("april2020.nc", proxy = TRUE)
st_crs(april20) = 4326

may20 <- read_stars("may2020.nc", proxy = TRUE)
st_crs(may20) = 4326

april21 <- read_stars("april2021.nc", proxy = TRUE)
st_crs(april21) = 4326

may21 <- read_stars("may2021.nc", proxy = TRUE)
st_crs(may21) = 4326


## LOAD GODWIT LOCS   -----------------------------------------------------------

# load godwit movement data

birds <- read.csv("Try2.csv")
birds <- birds[,c(1:3, 5:6)]

birds$time <- mdy_hm(birds$time, tz = "UTC")
birds <- birds %>% mutate(nearest_time = round_date(time, unit = "hour"))
birds2 <- birds
coordinates(birds2) <- ~lon+lat
proj4string(birds2) <- CRS("+proj=longlat +datum=WGS84")



###############################################
## LOOP BUILD - START HERE! 
###############################################

# extract wind data from one pressure level
# to loop, change LEVEL and COLNAME
# watch num obs in case of runaway duplication

april_flat21 <- st_as_stars(april21 %>% slice("level", 1))
may_flat21 <- st_as_stars(may21 %>% slice("level", 1))
wind <- c(april_flat21, may_flat21, along = "time")

wind_u <- wind[1]
wind_v <- wind[2]
rm(wind)

birds_u <- st_extract(wind_u, st_as_sf(birds2), bilinear = FALSE)
birds_v <- st_extract(wind_v, st_as_sf(birds2), bilinear = FALSE)

rm(wind_u, wind_v)

birds_u <- st_as_sf(birds_u)
birds_v <- st_as_sf(birds_v)

birds_u <- birds_u %>% mutate(lon = sf::st_coordinates(.)[,1],
                              lat = sf::st_coordinates(.)[,2])
birds_v <- birds_v %>% mutate(lon = sf::st_coordinates(.)[,1],
                              lat = sf::st_coordinates(.)[,2])


birds_u <- st_set_geometry(birds_u, NULL)
birds_v <- st_set_geometry(birds_v, NULL)

birds_u <- birds_u %>% pivot_longer(1:(ncol(birds_u)-2), 
                                    names_to = "nearest_time", 
                                    values_to = "u_500")
birds_v <-birds_v %>% pivot_longer(1:(ncol(birds_v)-2), 
                                   names_to = "nearest_time", 
                                     values_to = "v_500")
 
# merge dfs

birds_u$nearest_time <- ymd_hms(birds_u$nearest_time)
birds_v$nearest_time <- ymd_hms(birds_v$nearest_time)

birds <- left_join(birds, birds_u)
birds <- left_join(birds, birds_v)

head(birds)
rm(birds_u, birds_v)


## Safety -----------------------------------

write.csv(birds, "Safety2.csv", row.names = FALSE)

## Merge

old <- read.csv("Try2.csv")
new <- read.csv("Safety2.csv")

old$time <- mdy_hm(old$time, tz = "UTC")
new$time <- ymd_hms(new$time, tz = "UTC")


merged <- left_join(old, new)
merged

write.csv(merged, "Merged.csv", row.names = FALSE)
