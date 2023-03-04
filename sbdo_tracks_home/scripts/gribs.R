##importing wind data

install.packages("rNOMADS")
library(rNOMADS)
install.packages("wgrib2")
library(wgrib2)

ReadGrib(file.type="grib2", "raw_data/wind_1.grib")

install.packages("wgrib2")

