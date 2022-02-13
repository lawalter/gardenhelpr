# libraries ---------------------------------------------------------------

library(tidyverse)

# read data ---------------------------------------------------------------

path <- "raw_data/"

# Plant list
plants <- read_csv(paste0(path, "plants.csv")) 
  
# Read in raw data from TRY Plant Trait Database
# https://www.try-db.org/TryWeb/dp.php
raw_trydata <- read_tsv(paste0(path, "try_data_raw.txt")) 

# combine raw data --------------------------------------------------------

env_data <- 
  plants %>% 
  left_join(
    raw_trydata %>% rename(latin_name = AccSpeciesName),
    by = "latin_name") %>% 
  select(-contains("ID")) %>% 
  select(-c("LastName", "FirstName", "Dataset")) %>% 
  filter(str_detect(TraitName, "soil|light"))

rm(raw_trydata)

# pH ----------------------------------------------------------------------

pH <-
  env_data %>% 
  filter(str_detect(DataName, "pH")) %>% 
  mutate(
    type =
      case_when(
        str_detect(DataName, "max") ~ "max",
        str_detect(DataName, "min") ~ "min",
        TRUE ~ NA_character_
      )) %>% 
  select(plant, OrigValueStr, type) %>% 
  pivot_wider(plant, names_from = type, values_from = OrigValueStr)

# light -------------------------------------------------------------------

light <-
  env_data %>% 
  filter(str_detect(DataName, "LIGHT"))

# water -------------------------------------------------------------------

water <-
  env_data %>% 
  filter(str_detect(DataName, "moisture"))
