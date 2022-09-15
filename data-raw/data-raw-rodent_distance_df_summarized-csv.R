## code to prepare `data-raw/rodent_distance_df_summarized.csv` dataset goes here
rodent_distance_raw <- readr::read_csv("data-raw/rodent_distance_df_summarized.csv")
rodent_distance_raw <- rodent_distance_raw %>% dplyr::rename(species = item1)
usethis::use_data(rodent_distance_raw, overwrite = TRUE)
