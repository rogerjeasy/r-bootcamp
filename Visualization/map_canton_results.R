library(leaflet)
library(dplyr)
library(sf)
library(readr)
library(tidyr)
library(htmltools)

setwd("C:/Users/rogej/Documents/hslu/courses/bootcamp/r-bootcamp")
getwd()

# Read data files
election_results <- read_csv("Data/datatable.csv", show_col_types = FALSE)
party_colors <- read_csv("Data/party_colors.csv", show_col_types = FALSE)
canton_symbols <- read_csv("Data/kanton_names.csv", show_col_types = FALSE)

# Define party order explicitly
party_order <- c("PdA_Sol", "GRUENE", "SP", "GLP", "CSP", 
                 "Mitte", "EVP", "FDP", "EDU", "Lega", "MCR", "SVP", "LPS", "Uebrige")

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

party_colors <- party_colors %>%
  filter(!is.na(Party) & Party != "") %>%  
  distinct(Party, .keep_all = TRUE) %>%  
  mutate(Party = factor(Party, levels = party_order)) %>%  
  filter(!is.na(Party)) %>% 
  arrange(Party)


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

winning_parties <- canton_winners %>%
  distinct(Party) %>%
  filter(!is.na(Party))  # Ensure no NA values

# Filter party_colors to only include winning parties
legend_colors <- party_colors %>%
  filter(Party %in% winning_parties$Party) %>%
  arrange(Party)

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
    weight = 0.1,
    color = "white",
    opacity = 0.8
  ) %>%
  
  # Add legend
  addLegend(
    position = "bottomleft",
    colors = legend_colors$Color,  # Only winning parties' colors
    labels = as.character(legend_colors$Party),  # Only winning parties' names
    title = "Winning Parties",
    opacity = 0.7
  ) %>%
  
  # Set bounds to Switzerland
  fitBounds(5.9, 45.8, 10.5, 47.8)

# Display the map
election_map

saveRDS(election_map, file = "Documentation/Plots/map_plot_election_results_cantons.rds")

#######################################################
# Select the top 2 strongest parties for each canton
# Select the top 2 strongest parties for each canton
canton_top2_winners <- merged_data %>%
  group_by(Kanton) %>%
  slice_max(Percentage, n = 2, with_ties = FALSE) %>%  
  mutate(Rank = row_number()) %>%  
  ungroup()

# Merge party colors into canton_top2_winners before pivoting
canton_top2_winners <- canton_top2_winners %>%
  left_join(party_colors, by = "Party")  # Ensure Color column is included

# Convert sf object to a regular data frame before pivoting
canton_top2_pivot <- canton_top2_winners %>%
  st_drop_geometry() %>%  
  dplyr::select(Kanton, Party, Percentage, Color, Rank) %>%  # Include Color
  pivot_wider(
    names_from = Rank, 
    values_from = c(Party, Percentage, Color),  # Ensure Color is pivoted
    names_glue = "Rank_{Rank}_{.value}"  # Corrects column names
  )

# Verify that Color columns exist
print(names(canton_top2_pivot))  # Should contain "Rank_1_Color", "Rank_2_Color"

# Perform a regular left join (non-spatial)
canton_geo_data <- canton_geo_with_data %>%
  st_drop_geometry()

canton_top2_geo <- canton_geo_data %>%
  left_join(canton_top2_pivot, by = "Kanton") %>%
  filter(!is.na(Rank_1_Party))  # Ensure no empty values

# Restore spatial attributes after the join
canton_top2_geo <- canton_geo_with_data %>%
  left_join(canton_top2_geo, by = "Kanton")

# Extract unique top 2 winning parties
winning_parties_top2 <- canton_top2_winners %>%
  distinct(Party) %>%
  filter(!is.na(Party))  # Ensure no NA values

# Filter party_colors to include only top 2 winning parties
legend_colors_top2 <- party_colors %>%
  filter(Party %in% winning_parties_top2$Party) %>%
  arrange(Party)

# Create interactive map with correct color assignment
election_map_top2 <- leaflet(options = leafletOptions(minZoom = 7, maxZoom = 12)) %>%
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
  
  # Add canton polygons with top 2 winners
  addPolygons(
    data = canton_top2_geo,
    fillColor = ~Rank_1_Color,  # Use Rank_1_Color for mapping
    fillOpacity = 0.7,
    weight = 1,
    color = "white",
    label = ~lapply(paste0(
      "<strong>Canton:</strong> ", Kanton, "<br/>",
      "<strong>First Strong Party:</strong> ", Rank_1_Party, " (", Rank_1_Percentage, "%)", "<br/>",
      "<strong>Second Strong Party:</strong> ", Rank_2_Party, " (", Rank_2_Percentage, "%)"
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
    weight = 0.1,
    color = "white",
    opacity = 0.8
  ) %>%
  
  # Add legend for top 2 winning parties
  addLegend(
    position = "bottomleft",
    colors = legend_colors_top2$Color,  
    labels = as.character(legend_colors_top2$Party),
    title = "Top 2 Winning Parties",
    opacity = 0.7
  ) %>%
  
  # Set bounds to Switzerland
  fitBounds(5.9, 45.8, 10.5, 47.8)

# Display the updated map
election_map_top2
