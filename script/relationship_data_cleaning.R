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
  select(-comment) %>% 
  filter(!str_detect(plant, "Fruit Trees") &
           !str_detect(second_plant, "Fruit Trees") &
           !str_detect(plant, "Apple") & 
           !str_detect(second_plant, "Apple") &
           !str_detect(plant, "Apricot") &
           !str_detect(second_plant, "Apricot") &
           !str_detect(plant, "Cherry") &
           !str_detect(second_plant, "Cherry") &
           !str_detect(plant, "Grape") &
           !str_detect(second_plant, "Grape") &
           !str_detect(second_plant, "Blackberries") &
           !str_detect(plant, "Silverbeet") & 
           !str_detect(second_plant, "Silverbeet") & 
           !str_detect(plant, "Roses") &
           !str_detect(second_plant, "Roses") &
           !str_detect(second_plant, "Raspberry") &
           !str_detect(second_plant, "Rose") &
           !str_detect(plant, "Mulberry") &
           !str_detect(second_plant, "Mulberry") &
           !str_detect(plant, "Broad Beans") &
           !str_detect(second_plant, "\\>") & 
           !str_detect(second_plant, "All Plants") & 
           !str_detect(second_plant, "Most Plants") & 
           !str_detect(second_plant, "Most Herbs") & 
           !str_detect(second_plant, "None")) %>%  
  mutate(across(contains("plant"), ~str_replace(., "Aspargus", "Asparagus"))) %>% 
  mutate(across(contains("plant"), ~str_replace(., "Brocolli", "Broccoli"))) %>% 
  mutate(across(contains("plant"), ~str_replace(., "^Pepper$", "Peppers"))) %>% 
  mutate(across(contains("plant"), ~str_replace(., "^Beet$", "Beets"))) %>% 
  mutate(across(contains("plant"), ~str_replace(., "^Brussels Sprout$", "Brussels Sprouts"))) %>% 
  mutate(across(contains("plant"), ~str_replace(., "^Carrot$", "Carrots"))) %>% 
  mutate(across(contains("plant"), ~str_replace(., "^Chive$", "Chives"))) %>% 
  mutate(across(contains("plant"), ~str_replace(., "^Cucumber$", "Cucumbers"))) %>% 
  mutate(across(contains("plant"), ~str_replace(., "^Leek$", "Leeks"))) %>% 
  mutate(across(contains("plant"), ~str_replace(., "^Marigold$", "Marigolds"))) %>% 
  mutate(across(contains("plant"), ~str_replace(., "^Melon$", "Melons"))) %>% 
  mutate(across(contains("plant"), ~str_replace(., "^Parsnip$", "Parsnips"))) %>% 
  mutate(across(contains("plant"), ~str_replace(., "^Radish$", "Radishes"))) %>% 
  mutate(across(contains("plant"), ~str_replace(., "^Tomato$", "Tomatoes"))) %>% 
  mutate(across(contains("plant"), ~str_replace(., "^Pumpkin$", "Pumpkins"))) %>% 
  mutate(across(contains("plant"), ~str_replace(., "^Soybean$", "Soybeans"))) %>% 
  mutate(across(contains("plant"), ~str_replace(., "^Potato$", "Potatoes"))) %>% 
  mutate(across(contains("plant"), ~str_replace(., "^Sunflower$", "Sunflowers"))) %>% 
  mutate(across(contains("plant"), ~str_replace(., "^Strawberry$", "Strawberries"))) %>% 
  mutate(across(contains("plant"), ~str_replace(., "^Nasturtium$", "Nasturtiums"))) %>% 
  mutate(across(contains("plant"), ~str_replace(., "^Onion$", "Onions"))) %>% 
  mutate(across(contains("plant"), ~str_replace(., "^Turnip$", "Turnips"))) %>% 
  mutate(across(contains("plant"), ~str_replace(., "^Chard$", "Swiss Chard"))) %>% 
  mutate(across(contains("plant"), ~str_replace(., "^Sage and many other herbs$", "Sage"))) %>% 
  mutate(
    second_plant =
      case_when(
        second_plant == "Climbing Bean" ~ "Beans (climbing)",
        second_plant == "Climbing Beans" ~ "Beans (climbing)",
        second_plant == "Beans" ~ "Beans (all)",
        second_plant == "Beans (most)" ~ "Beans (all)",
        second_plant == "Bush Beans" ~ "Beans (bush)",
        second_plant == "Beans (Bush & Climbing)" ~  "Beans (all)",
        TRUE ~ second_plant)) %>% 
  mutate(
    plant =
      case_when(
        plant == "Climbing Beans" ~ "Beans (climbing)",
        plant == "Bush Beans" ~ "Beans (bush)",
        plant == "Beans" ~  "Beans (all)",
        TRUE ~ plant))
  

write.csv(clean_data, "clean_data/plant_relationships.csv", row.names = F)
