library(tidyverse)
library(sf)
library(lubridate)
library(mapview)
library(units)

#i need to specify that I want to look from first point til Nov 1 not sure how to do this

# defining start date
start_date_2022 <- ymd("2022-06-05")

# defining end date
end_date_2022 <- ymd("2022-11-01")

# generating range of dates
range <- seq(start_date_2022, end_date_2022,"days", na.rm=TRUE)
print(range)

mig_dist22<-sbdo_distance %>%
  group_by(site, year, id) %>% 
  summarize(bird_mig=sum(dist_to_next, na.rm=TRUE)) %>% 
  arrange(desc(bird_mig))
view(mig_dist22)


#just trying to count
count_persite_year<-sbdo_all %>% 
  count(site,year)
count_per_tag<-sbdo_all %>%
  count(site, year, id)
view(count_per_tag)


