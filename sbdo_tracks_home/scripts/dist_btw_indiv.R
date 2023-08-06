library(tidyverse)
library(sf)

# read in beluga data
getwd()
bel_date <- read_rds('scripts/bel_date.rds')

# write a custom function that takes the minimum POSITIVE value, so that 0 is excluded 
min_pos <-  function(x) min(x[x > 0])

# calculate summary stats for distance between individuals on each given date
bel_dist <- bel_date %>%
  group_by(timestamp) %>% 
  # create column to calculate the number of individuals detected on each date
  mutate(n_ind = n()) %>% 
  # filter out dates with only 1 individual detected
  filter(n_ind > 1) %>%
  # get rid of # individuals column
  #select(-n_ind) %>% 
  # nest by date
  nest(data = -timestamp) %>% 
  # calculate mean, sd, max, and positive min
  mutate(avg_distance = map(data, ~mean(as.numeric(st_distance(., by_element = FALSE)))),
         sd_distance = map(data, ~sd(as.numeric(st_distance(., by_element = FALSE)))),
         max_distance = map(data, ~max(as.numeric(st_distance(., by_element = FALSE)))),
         min_distance = map(data, ~min_pos(as.numeric(st_distance(., by_element = FALSE))))) %>% 
  unnest(cols = c(avg_distance, sd_distance, max_distance, min_distance))

# test to make sure class of all distance summary stats is 'numeric'
class(bel_dist$avg_distance)
class(bel_dist$sd_distance)
class(bel_dist$max_distance)                                                                                                                             
class(bel_dist$min_distance)