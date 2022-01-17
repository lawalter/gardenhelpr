library(tidyverse)

path <- "raw_data/"

# Read in raw data from Walden Labs
# https://waldenlabs.com/the-ultimate-companion-planting-guide-chart/
raw_wldata <- 
  read_csv(
    paste0(path, "companion_planting_raw.csv"),
    col_names = F) %>% 
  mutate(
    plant = ifelse(str_detect(X1, "\\stop"), str_extract(X1, ".+(?=\\stop$)"), NA),
    relationship = 
      case_when(
        str_detect(X1, "Companions") ~ "Companions", 
        str_detect(X1, "Antagonists") ~ "Antagonists", 
        TRUE ~ NA_character_),
    comment = ifelse(X1 == "Comments", lead(X1), NA),
    dlete = ifelse(lag(X1) == "Comments", "x", NA)) %>% 
  fill(plant, relationship) %>% 
  filter(!str_detect(X1, "\\stop") & !str_detect(X1, "Companions") & !str_detect(X1, "Antagonists")) %>% 
  filter(is.na(dlete)) %>% 
  select(-dlete) %>% 
  rename(second_plant = X1) %>% 
  relocate(plant, .before = "second_plant")

comments <-
  raw_wldata %>% 
  filter(!is.na(comment)) %>% 
  select(-c("second_plant", "relationship"))

clean_data <-
  raw_wldata %>% 
  filter(is.na(comment)) %>% 
  select(-comment)

write.csv(clean_data, "clean_data/plant_relationships.csv", row.names = F)
