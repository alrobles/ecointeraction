library(tidyverse)

bat_env_distance <- read_csv("data-raw/bat_env_distance_raw.csv")
batsdistance_raw <- read_csv("data-raw/batsdistance_raw.csv")

bat_geo_distance <- batsdistance_raw %>%
  group_by(item1) %>%
  summarise(distance =  mean(distance)) %>%
  rename(species = item1,
         geo_distance = distance)

download.file(url ="https://raw.githubusercontent.com/n8upham/MamPhy_v1/master/_DATA/MamPhy_fullPosterior_BDvr_Completed_5911sp_topoCons_FBDasZhouEtAl_MCC_v2_target.tre", destfile = "data-raw/mammals.tre")
library(phytools)

mammals <- read.nexus("data-raw/mammals.tre")
mammals$tip.label %>% enframe %>%
  mutate(value = str_split(value, "_"))

mammals$tip.label %>% enframe %>%
  mutate(value = str_split(value, "_")) %>%
  mutate(species = purrr::map_chr(value, function(x) paste(x[1], x[2])))

mammals_tip_label <- mammals$tip.label %>% enframe %>%
  mutate(tip_name = str_split(value, "_")) %>%
  mutate(tip_name_length = purrr::map_dbl(tip_name, function(x) length(x))) %>%
  #filter(value_length == 4) %>%
  mutate(species = purrr::map_chr(tip_name, function(x) paste(x[1], x[2]))) %>%
  mutate(phy_tip = str_replace(species, " ",  "_"))

bat_tips <- select(bat_geo_distance, species) %>%
  inner_join(mammals_tip_label)



bats <- drop.tip(mammals, mammals$tip.label[-na.exclude( match(mammals$tip.label, bat_tips$value ))] )
bats$tip.label <- bat_tips$phy_tip

bat_distance <- cophenetic.phylo(bats) %>% as.dist(upper = TRUE) %>% broom::tidy()

bat_phylo_distance_raw <- bat_distance %>% group_by(item1) %>%
  summarise(distance = mean(distance)) %>%
  rename(species = item1,
         phylo_distance= distance) %>%
  mutate(species = str_replace(species, "_", " "))




bat_distance_raw <- list(bat_phylo_distance_raw,
     bat_geo_distance,
     bat_env_distance) %>%
  purrr::reduce(left_join)

write_csv(bat_distance_raw, "data-raw/bat_distance_raw.csv")
