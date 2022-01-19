library(tidyverse)

path <- "raw_data/"

# plants <-
#   read_csv("clean_data/plant_relationships.csv") %>% 
#   select(plant) %>% 
#   distinct() 
# write.csv(plants, "clean_data/plants.csv", row.names = F)

# Sientific names added by hand, list edited (some plants deleted)
plants <- 
  read_csv(paste0(path, "plants.csv")) 
  
# Read in raw data from TRY Plant Trait Database
# https://www.try-db.org/TryWeb/dp.php
raw_trydata <- 
  read_tsv(paste0(path, "try_data_raw.txt")) %>% 
  filter(AccSpeciesName %in% plants$latin_name)
