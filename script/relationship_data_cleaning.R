# Read in raw data from Walden Labs
# https://waldenlabs.com/the-ultimate-companion-planting-guide-chart/
raw_wldata <- 
  readr::read_csv("raw_data/companion_planting_raw.csv", col_names = F) |> 
  dplyr::mutate(
    plant = 
      ifelse(
        stringr::str_detect(X1, "\\stop"), 
        stringr::str_extract(X1, ".+(?=\\stop$)"), 
        NA),
    relationship = 
      dplyr::case_when(
        stringr::str_detect(X1, "Companions") ~ "Companions", 
        stringr::str_detect(X1, "Antagonists") ~ "Antagonists", 
        TRUE ~ NA_character_),
    comment = ifelse(X1 == "Comments", dplyr::lead(X1), NA),
    dlete = ifelse(dplyr::lag(X1) == "Comments", "x", NA)) |> 
  tidyr::fill(plant, relationship) |> 
  dplyr::filter(
    !stringr::str_detect(X1, "\\stop") & 
      !stringr::str_detect(X1, "Companions") & 
      !stringr::str_detect(X1, "Antagonists")) |> 
  dplyr::filter(is.na(dlete)) |> 
  dplyr::select(-dlete) |> 
  dplyr::rename(second_plant = X1) |> 
  dplyr::relocate(plant, .before = "second_plant")

# Reflexive completion
# Not all relationships are included; for example, Basil has Chamomile listed as
# a companion, but Chamomile does not have Basil listed as a companion
completed_raw_data <-
  raw_wldata |> 
  rbind(
    raw_wldata |> 
      dplyr::rename(
        second_plant = plant,
        plant = second_plant) |> 
      dplyr::relocate(plant, .before = "second_plant")
  ) |> 
  dplyr::distinct()

# Define plants unwanted in final output
excluded <-
  c("Fruit Trees", "Apple", "Apricot", "Cherry", "Grape", "Blackberries",
    "Silverbeet", "Roses", "Rose", "Raspberry", "Mulberry", "Broad Beans",
    "All Plants", "Most Plants", "Most Herbs", "None")

# Clean up raw data
clean_data <-
  completed_raw_data |> 
  dplyr::filter(is.na(comment)) |> 
  dplyr::select(-comment) |> 
  # Filter out excluded plants
  dplyr::filter(
    !plant %in% excluded & !second_plant %in% excluded & 
      !stringr::str_detect(second_plant, "\\>")) |>  
  # Spelling fixes
  dplyr::mutate(dplyr::across(
    dplyr::contains("plant"),
    ~ stringr::str_replace(., "Aspargus", "Asparagus")
  )) |>
  dplyr::mutate(dplyr::across(
    dplyr::contains("plant"),
    ~ stringr::str_replace(., "Brocolli", "Broccoli")
  )) |>
  dplyr::mutate(dplyr::across(
    dplyr::contains("plant"),
    ~ stringr::str_replace(., "^Pepper$", "Peppers")
  )) |>
  dplyr::mutate(dplyr::across(
    dplyr::contains("plant"),
    ~ stringr::str_replace(., "^Beet$", "Beets")
  )) |>
  dplyr::mutate(dplyr::across(
    dplyr::contains("plant"),
    ~ stringr::str_replace(., "^Brussels Sprout$", "Brussels Sprouts")
  )) |>
  dplyr::mutate(dplyr::across(
    dplyr::contains("plant"),
    ~ stringr::str_replace(., "^Carrot$", "Carrots")
  )) |>
  dplyr::mutate(dplyr::across(
    dplyr::contains("plant"),
    ~ stringr::str_replace(., "^Chive$", "Chives")
  )) |>
  dplyr::mutate(dplyr::across(
    dplyr::contains("plant"),
    ~ stringr::str_replace(., "^Cucumber$", "Cucumbers")
  )) |>
  dplyr::mutate(dplyr::across(
    dplyr::contains("plant"),
    ~ stringr::str_replace(., "^Leek$", "Leeks")
  )) |>
  dplyr::mutate(dplyr::across(
    dplyr::contains("plant"),
    ~ stringr::str_replace(., "^Marigold$", "Marigolds")
  )) |>
  dplyr::mutate(dplyr::across(
    dplyr::contains("plant"),
    ~ stringr::str_replace(., "^Melon$", "Melons")
  )) |>
  dplyr::mutate(dplyr::across(
    dplyr::contains("plant"),
    ~ stringr::str_replace(., "^Parsnip$", "Parsnips")
  )) |>
  dplyr::mutate(dplyr::across(
    dplyr::contains("plant"),
    ~ stringr::str_replace(., "^Radish$", "Radishes")
  )) |>
  dplyr::mutate(dplyr::across(
    dplyr::contains("plant"),
    ~ stringr::str_replace(., "^Tomato$", "Tomatoes")
  )) |>
  dplyr::mutate(dplyr::across(
    dplyr::contains("plant"),
    ~ stringr::str_replace(., "^Pumpkin$", "Pumpkins")
  )) |>
  dplyr::mutate(dplyr::across(
    dplyr::contains("plant"),
    ~ stringr::str_replace(., "^Soybean$", "Soybeans")
  )) |>
  dplyr::mutate(dplyr::across(
    dplyr::contains("plant"),
    ~ stringr::str_replace(., "^Potato$", "Potatoes")
  )) |>
  dplyr::mutate(dplyr::across(
    dplyr::contains("plant"),
    ~ stringr::str_replace(., "^Sunflower$", "Sunflowers")
  )) |>
  dplyr::mutate(dplyr::across(
    dplyr::contains("plant"),
    ~ stringr::str_replace(., "^Strawberry$", "Strawberries")
  )) |>
  dplyr::mutate(dplyr::across(
    dplyr::contains("plant"),
    ~ stringr::str_replace(., "^Nasturtium$", "Nasturtiums")
  )) |>
  dplyr::mutate(dplyr::across(
    dplyr::contains("plant"),
    ~ stringr::str_replace(., "^Onion$", "Onions")
  )) |>
  dplyr::mutate(dplyr::across(
    dplyr::contains("plant"),
    ~ stringr::str_replace(., "^Turnip$", "Turnips")
  )) |>
  dplyr::mutate(dplyr::across(
    dplyr::contains("plant"),
    ~ stringr::str_replace(., "^Chard$", "Swiss Chard")
  )) |>
  dplyr::mutate(dplyr::across(
    dplyr::contains("plant"),
    ~ stringr::str_replace(., "^Sage and many other herbs$", "Sage")
  )) |> 
  dplyr::mutate(
    second_plant =
      dplyr::case_when(
        second_plant == "Climbing Bean" ~ "Beans (climbing)",
        second_plant == "Climbing Beans" ~ "Beans (climbing)",
        second_plant == "Beans" ~ "Beans (all)",
        second_plant == "Beans (most)" ~ "Beans (all)",
        second_plant == "Bush Beans" ~ "Beans (bush)",
        second_plant == "Beans (Bush & Climbing)" ~  "Beans (all)",
        TRUE ~ second_plant)) |> 
  dplyr::mutate(
    plant =
      dplyr::case_when(
        plant == "Climbing Beans" ~ "Beans (climbing)",
        plant == "Bush Beans" ~ "Beans (bush)",
        plant == "Beans" ~  "Beans (all)",
        TRUE ~ plant)) |> 
  # Remove error: garlic listed as peas companion
  dplyr::filter(
    !(plant == "Garlic" & second_plant == "Peas" & relationship == "Companions"))

# Write out csv for shiny app
write.csv(clean_data, "clean_data/plant_relationships.csv", row.names = F)
