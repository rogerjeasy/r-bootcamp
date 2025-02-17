library(leaflet)
library(dplyr)
library(sf)
library(readr)
library(tidyr)
library(htmltools)

# Read data files
election_results <- read_csv("Data/datatable.csv", show_col_types = FALSE)
party_colors <- read_csv("Data/party_colors.csv", show_col_types = FALSE)
canton_symbols <- read_csv("Data/kanton_names.csv", show_col_types = FALSE)

# Read and transform ALL geographical data to WGS84 (EPSG:4326) immediately
canton_geo <- read_sf("Shapefiles/g2k23.shp") %>% 
  st_transform(4326)  # Transform to WGS84

lake_geo <- read_sf("Shapefiles/g2s23.shp") %>% 
  st_transform(4326)  # Transform to WGS84

country_geo <- read_sf("Shapefiles/g2l23.shp") %>% 
  st_transform(4326)  # Transform to WGS84

# Verify CRS of all spatial objects
st_crs(canton_geo)  
st_crs(lake_geo)    
st_crs(country_geo)

# Process election data
party_cols <- names(election_results)[which(names(election_results) == "CSP_23"):
                                        which(names(election_results) == "Uebrige_23")]

canton_totals <- election_results %>%
  pivot_longer(cols = all_of(party_cols), names_to = "Party", values_to = "Vote_Percentage") %>%
  mutate(Actual_Votes = (Vote_Percentage / 100) * vote_num) %>%  
  group_by(Kanton, Party) %>%
  summarise(Total_Votes = sum(Actual_Votes, na.rm = TRUE), .groups = "drop") %>%  
  group_by(Kanton) %>%
  mutate(Percentage = round((Total_Votes / sum(Total_Votes)) * 100, 2)) %>% 
  ungroup() %>%
  filter(!is.na(Kanton)) %>%
  mutate(Party = gsub("_23$", "", Party))

# Clean up and join data
party_colors <- party_colors %>%
  distinct(Party, .keep_all = TRUE)

canton_totals <- canton_totals %>%
  left_join(party_colors, by = "Party")

# Prepare geographical data
canton_geo_with_data <- canton_geo %>%
  left_join(canton_symbols, by = "KTNAME") %>%
  rename(Kanton = KTN_SYMB)

# Merge geographical and election data
merged_data <- canton_geo_with_data %>%
  left_join(canton_totals, by = "Kanton")

# Find winning party for each canton
canton_winners <- merged_data %>%
  group_by(Kanton) %>%
  slice_max(Percentage, n = 1) %>%
  ungroup()

# Create interactive map
election_map <- leaflet(options = leafletOptions(minZoom = 7, maxZoom = 12)) %>%
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
  
  # Add canton polygons with winners
  addPolygons(
    data = canton_winners,
    fillColor = ~Color,
    fillOpacity = 0.7,
    weight = 1,
    color = "white",
    label = ~lapply(paste0(
      "<strong>Canton:</strong> ", KTNAME, "<br/>",
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
      weight = 2,
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
    weight = 2,
    color = "white",
    opacity = 0.8
  ) %>%
  
  # Add legend
  addLegend(
    position = "bottomleft",
    colors = party_colors$Color,
    labels = party_colors$Party,
    title = "Political Parties",
    opacity = 0.7
  ) %>%
  
  # Set bounds to Switzerland
  fitBounds(5.9, 45.8, 10.5, 47.8)

# Display the map
election_map

saveRDS(election_map, file = "Documentation/Plots/map_plot_election_results_cantons.rds")
