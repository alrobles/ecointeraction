library(spocc)
library(tidyverse)
library(furrr)
library(future)
plan(multiprocess)

birds_points <- birdsdistance %>%
  dplyr::select(species) %>%
  mutate(pts =  furrr::future_map(species, function(x){
     x %>% occ(., limit = 3*1e3) %>% occ2df
  } ) ) %>% unnest()

birds_points %>% group_by(species) %>% count
birds_points_2000 <- birds_points %>%
  mutate(year = lubridate::year(date),
         month = lubridate::month(date)) %>%
  dplyr::select(species, longitude, latitude, month, year) %>%
  filter(year > 2000) %>%
  distinct()

library(scrubr)
birds_points_2000 <- birds_points_2000 %>%
  dframe() %>%
  coord_impossible() %>%
  coord_incomplete() %>%
  coord_unlikely()


library(sf)
birds_points_sf <- birds_points_2000 %>% group_by(species) %>% st_as_sf(coords = c("longitude", "latitude"))

library(tmap)
data("World")
tm_shape(World) + tm_polygons(birds_points_sf) +
  tm_shape(df) + tm_bubbles("population")

