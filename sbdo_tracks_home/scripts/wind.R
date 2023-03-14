##wind

install.packages("stars")
install.packages("ncmeta")
library(stars)
library(dplyr)
library(ncmeta)


f <- read_ncdf("raw_data/adaptor.mars.internal-1678135465.0409904-16182-10-76d01213-8782-4cb1-8a80-f0a30401affa.nc",
               var="sst")


