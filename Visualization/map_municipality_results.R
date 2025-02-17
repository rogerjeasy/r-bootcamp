library(leaflet)
library(plotly)
library(dplyr)
library(sf)
library(readr)
library(tidyr)
library(raster)
library(magrittr)
library(htmltools)

setwd("C:/Users/rogej/Documents/hslu/courses/bootcamp/r-bootcamp")
getwd()

election_results <- read_csv("Data/datatable.csv", show_col_types = FALSE)
party_colors <- read_csv("Data/party_colors.csv", show_col_types = FALSE)
canton_symbols <- read_csv("Data/kanton_names.csv", show_col_types = FALSE)

# Define party order explicitly
party_order <- c("PdA_Sol", "GRUENE", "SP", "GLP", "CSP", 
                 "Mitte", "EVP", "FDP", "EDU", "Lega", "MCR", "SVP", "LPS", "Uebrige")

# Read geographical data and transform to WGS84
canton_geo <- read_sf("Shapefiles/g2k23.shp") %>% st_transform(4326)
lake_geo <- read_sf("Shapefiles/g2s23.shp") %>% st_transform(4326)
country_geo <- read_sf("Shapefiles/g2l23.shp") %>% st_transform(4326)
municipality_geo_data <- read_sf("Shapefiles/municipality/K4polg20230101vf_ch2007Poly.shp") %>% 
  st_transform(4326)

# Process the election data
party_cols <- names(election_results)[which(names(election_results) == "CSP_23"):
                                        which(names(election_results) == "Uebrige_23")]

municipality_totals <- election_results %>%
  pivot_longer(
    cols = all_of(party_cols),
    names_to = "Party",
    values_to = "Vote_Percentage"
  ) %>%
  mutate(Actual_Votes = (Vote_Percentage / 100) * vote_num) %>%
  group_by(municipality, Party) %>%
  summarise(
    Total_Votes = sum(Actual_Votes, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(municipality) %>%
  mutate(
    Percentage = round((Total_Votes / sum(Total_Votes, na.rm = TRUE)) * 100, 2)
  ) %>%
  ungroup()

party_colors <- party_colors %>%
  filter(!is.na(Party) & Party != "") %>%  
  distinct(Party, .keep_all = TRUE) %>%  
  mutate(Party = factor(Party, levels = party_order)) %>%  
  filter(!is.na(Party)) %>% 
  arrange(Party)

# Clean up data
municipality_totals <- municipality_totals %>%
  filter(!is.na(municipality) & !(is.na(Party) & is.na(Percentage))) %>%
  mutate(Party = gsub("_23$", "", Party)) %>%
  left_join(party_colors %>% distinct(Party, .keep_all = TRUE), by = "Party")

# Prepare municipality geometry data
municipality_geo <- municipality_geo_data %>%
  mutate(BFS_ID = sprintf("%04d", id)) %>%
  rename(municipality = name,
         municipalityId = BFS_ID) %>%
  left_join(municipality_totals, by = "municipality") %>%
  filter(!is.na(municipality) & !(is.na(Party) & is.na(Percentage)))

# Find winning party for each municipality
municipality_winners <- municipality_geo %>%
  group_by(municipality) %>%
  slice_max(Percentage, n = 1) %>%
  ungroup()

winning_parties <- municipality_winners %>%
  distinct(Party) %>%
  filter(!is.na(Party))  # Ensure no NA values

# Filter party_colors to include only winning parties
legend_colors <- party_colors %>%
  filter(Party %in% winning_parties$Party) %>%
  arrange(Party)

# Create interactive map using leaflet
interactive_map <- leaflet(options = leafletOptions(minZoom = 7, maxZoom = 12)) %>%
  # Set initial view to Switzerland
  setView(lng = 8.2275, lat = 46.8182, zoom = 8) %>%
  
  # Add white background
  addPolygons(
    data = country_geo,
    fillColor = "white",
    fillOpacity = 1,
    weight = 0,
    color = "transparent"
  ) %>%
  
  # Add municipality polygons with improved tooltip
  addPolygons(
    data = municipality_winners,
    fillColor = ~party_colors$Color[match(Party, party_colors$Party)],
    fillOpacity = 0.7,
    weight = 1,
    color = "white",
    label = ~lapply(paste0(
      "<strong>Municipality:</strong> ", municipality, "<br/>",
      "<strong>Winning Party:</strong> ", Party, "<br/>",
      "<strong>Percentage:</strong> ", round(Percentage, 1), "%"
    ), HTML),
    labelOptions = labelOptions(
      style = list(
        "background-color" = "white",
        "border-color" = "#666",
        "border-width" = "1px",
        "border-style" = "solid",
        "padding" = "4px",
        "border-radius" = "4px"
      ),
      textsize = "13px",
      direction = "auto"
    ),
    highlightOptions = highlightOptions(
      weight = 1,
      color = "#666",
      fillOpacity = 0.9,
      bringToFront = TRUE
    )
  ) %>%
  
  # Add lakes
  addPolygons(
    data = lake_geo,
    fillColor = "#D6F1FF",
    fillOpacity = 0.8,
    weight = 0,
    color = "transparent"
  ) %>%
  
  # Add canton borders
  addPolygons(
    data = canton_geo,
    fill = FALSE,
    weight = 1,
    color = "white",
    opacity = 0.8
  ) %>%
  
  # Add Switzerland border
  addPolylines(
    data = country_geo,
    color = "#666",
    weight = 0,
    opacity = 1
  ) %>%
  
  # Add legend
  addLegend(
    position = "bottomleft",
    colors = legend_colors$Color,  # Only winning parties' colors
    labels = as.character(legend_colors$Party),  # Only winning parties' names
    title = "Winning Parties",
    opacity = 0.7,
    group = "legend"
  ) %>%
  
  # Set bounds
  fitBounds(5.9, 45.8, 10.5, 47.8)

# Display the map
interactive_map

saveRDS(interactive_map, file = "Documentation/Plots/map_plot_election_results_municipalities.rds")

# Check for municipalities without data
missing_municipalities <- municipality_geo_data$name[
  !municipality_geo_data$name %in% municipality_totals$municipality
]
print(paste("Number of municipalities without data:", length(missing_municipalities)))

# Check for total area coverage
total_area <- st_area(st_union(country_geo))
municipality_area <- st_area(st_union(municipality_geo_data))
coverage_percentage <- as.numeric(municipality_area/total_area) * 100
print(paste("Municipality coverage percentage:", coverage_percentage))

print(missing_municipalities)
str(municipality_geo_data)
