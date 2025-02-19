library(ggplot2)
library(plotly)
library(dplyr)
library(maptiles)
library(sf)
library(tidyterra)
library(osmdata)
library(readr)
library(tidyr)
library(raster)
library(magrittr)

setwd("C:/Users/rogej/Documents/hslu/courses/bootcamp/r-bootcamp")
getwd()

election_results <- read_csv("Data/datatable.csv", show_col_types = FALSE)
party_colors <- read_csv("Data/party_colors.csv", show_col_types = FALSE)
canton_symbols <- read_csv("Data/kanton_names.csv", show_col_types = FALSE)

# Read canton and lake borders
canton_geo <- read_sf("Shapefiles/g2k23.shp") %>% st_transform(3857)
lake_geo <- read_sf("Shapefiles/g2s23.shp") %>% st_transform(st_crs(canton_geo))

# read country borders
country_geo <- read_sf("Shapefiles/g2l23.shp")

# Load the relief raster
relief <- raster("Shapefiles/02-relief-ascii.asc")

relief_fixed <- raster("Shapefiles/02-relief-ascii.asc") %>%
  mask(country_geo) %>%
  rasterToPoints() %>%
  as.data.frame()

swiss_cantons <- election_results %>%
  distinct(Kanton) %>%  
  filter(!is.na(Kanton)) %>%  
  pull(Kanton)

# Identify the party columns dynamically (columns from CSP_23 to Uebrige_23)
party_cols <- names(election_results)[which(names(election_results) == "CSP_23"):which(names(election_results) == "Uebrige_23")]

# Compute total percentage per canton
canton_totals <- election_results %>%
  pivot_longer(cols = all_of(party_cols), names_to = "Party", values_to = "Vote_Percentage") %>%
  mutate(Actual_Votes = (Vote_Percentage / 100) * vote_num) %>%  
  group_by(Kanton, Party) %>%
  summarise(Total_Votes = sum(Actual_Votes, na.rm = TRUE), .groups = "drop") %>%  
  group_by(Kanton) %>%
  mutate(Percentage = round((Total_Votes / sum(Total_Votes)) * 100, 2)) %>% 
  ungroup()

canton_totals <- canton_totals %>%
  filter(!is.na(Kanton))

party_colors %>%
  count(Party) %>%
  filter(n > 1)

party_colors <- party_colors %>%
  distinct(Party, .keep_all = TRUE)


canton_totals <- canton_totals %>%
  mutate(Party = gsub("_23$", "", Party)) %>%  # Remove suffix "_23"
  left_join(party_colors, by = "Party")

# Ensure canton order is maintained
canton_totals$Kanton <- factor(canton_totals$Kanton, levels = unique(election_results$Kanton))

canton_geo_with_data <- canton_geo %>%
  left_join(canton_symbols, by = "KTNAME")

canton_geo_with_data <- canton_geo_with_data %>%
  rename(Kanton=KTN_SYMB)

merged_data <- canton_geo_with_data %>%
  left_join(canton_totals, by = "Kanton")

merged_data <- merged_data %>%
  mutate(tooltip_text = sprintf("Canton: %s\nParty: %s\nPercentage: %.1f%%", 
                                Kanton, Party, Percentage))

canton_winners <- merged_data %>%
  group_by(Kanton) %>%
  slice_max(Percentage, n = 1) %>%
  ungroup()

# Create the canton-level plot
canton_result_map <- ggplot() +
  # Relief layer
  geom_raster(
    data = relief_fixed,
    aes(x = x, y = y, alpha = X02.relief.ascii),
    inherit.aes = FALSE
  ) +
  scale_alpha(name = "",
              range = c(0.1, 1),
              guide = "none") +
  # Canton layer with all party results
  geom_sf(
    data = merged_data,
    mapping = aes(fill = Party, alpha = Percentage, text = tooltip_text),
    color = "white",
    size = 0.1
  ) +
  # Top layer with winning parties
  geom_sf(data = canton_winners,
          aes(fill = Party, 
              text = tooltip_text),
          color = "white",
          size = 0.1) +
  scale_fill_manual(
    values = setNames(party_colors$Color, party_colors$Party),
    name = "Political Parties"
  ) +
  # Canton borders
  geom_sf(
    data = canton_geo,
    fill = "transparent",
    color = "white",
    size = 0.5
  ) +
  # Lake layer
  geom_sf(
    data = lake_geo,
    fill = "#D6F1FF",
    color = "transparent"
  ) +
  labs(x = NULL,
       y = NULL,
       title = "Swiss Election Results 2023",
       subtitle = "Results by Canton") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

# Convert to interactive plotly plot

interactive_map <- ggplotly(canton_result_map, tooltip = "text") %>%
  layout(
    hoverlabel = list(
      bgcolor = "white",
      font = list(family = "Arial", size = 12)
    )
  )

# Display the interactive map
interactive_map

saveRDS(canton_result_map, file = "Documentation/Plots/map_election_results.rds")


################# Results of Election by Municipality
municipality_prod_geo <- read_sf("Shapefiles/gde-1-1-15.shp")

municipality_prod_geo %<>%
  mutate(BFS_ID = sprintf("%04d", BFS_ID))

municipality_prod_geo %<>%
  rename(municipalityId = BFS_ID)


municipality_prod_geo %<>%
  left_join(election_results, by = "municipalityId")


municipality_election_data <- municipality_prod_geo

party_colors <- party_colors %>%
  add_row(Party = "Uebrige", Color = "#808080")

# Identify the party columns dynamically (columns from CSP_23 to Uebrige_23)
party_cols <- names(election_results)[which(names(election_results) == "CSP_23"):which(names(election_results) == "Uebrige_23")]

# Compute total percentage per municipality
municipality_totals <- municipality_election_data %>%
  # Do pivot_longer only once
  pivot_longer(
    cols = all_of(party_cols), 
    names_to = "Party", 
    values_to = "Vote_Percentage"
  ) %>%
  # Calculate actual votes
  mutate(Actual_Votes = (Vote_Percentage / 100) * vote_num) %>%
  # Group by municipality name and Party
  group_by(municipality, Party) %>%
  # Summarize to get totals
  summarise(
    Total_Votes = sum(Actual_Votes, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Calculate percentages per municipality
  group_by(municipality) %>%
  mutate(
    Percentage = round((Total_Votes / sum(Total_Votes, na.rm = TRUE)) * 100, 2)
  ) %>%
  ungroup()

# Remove rows with NA municipalityId
municipality_totals <- municipality_totals %>%
  filter(!is.na(municipality))


municipality_totals <- municipality_totals %>%
  mutate(Party = gsub("_23$", "", Party)) %>%  # Remove suffix "_23"
  left_join(party_colors, by = "Party")

municipality_geo_with_data <- municipality_totals %>%
  mutate(tooltip_text = sprintf("Municipality: %s\nParty: %s\nPercentage: %.1f%%", 
                                municipality, Party, Percentage))


municipality_winners <- municipality_geo_with_data %>%
  group_by(municipality) %>%
  slice_max(order_by = Percentage, n = 1) %>%
  ungroup()

#####################################
muni_plot <- ggplot() +
  # Base layer with relief and country outline
  geom_sf(
    data = country_geo,
    fill = "darkgrey",
    color = "transparent"
  ) +
  # Relief layer
  geom_raster(
    data = relief_fixed,
    aes(x = x, y = y, alpha = X02.relief.ascii),
    inherit.aes = FALSE
  ) +
  scale_alpha(name = "",
              range = c(0.1, 1),
              guide = "none") +
  # Top layer with winning parties
  geom_sf(data = municipality_winners,
          aes(fill = Party, 
              text = tooltip_text),
          color = "white",
          size = 0.1) +
  scale_fill_manual(
    values = setNames(party_colors$Color, party_colors$Party),
    name = "Political Parties"
  ) +
  # Lake layer
  geom_sf(
    data = lake_geo,
    fill = "#D6F1FF",
    color = "transparent"
  ) +
  labs(x = NULL,
       y = NULL,
       title = "Swiss Election Results 2023",
       subtitle = "Results by Canton") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )
muni_plot

interactive_plot <- ggplotly(muni_plot, tooltip = "text") %>%
  layout(
    hoverlabel = list(
      bgcolor = "white",
      font = list(size = 12),
      bordercolor = "gray"
    )
  )

# Display the interactive plot
interactive_plot


########################################
# First, convert sf object to regular coordinates
municipality_winners <- municipality_winners %>%
  mutate(poly_id = row_number())

municipality_coords <- municipality_winners %>%
  mutate(id = row_number()) %>%  # Add unique identifier
  st_cast("POLYGON") %>%         # Cast multipolygons to polygons
  st_coordinates() %>%
  as.data.frame()

# Join back the attributes
municipality_data <- municipality_winners %>%
  mutate(id = row_number()) %>%
  st_drop_geometry()

# Create a new ggplot without the raster layer first
muni_plot_simple <- ggplot() +
  geom_polygon(data = municipality_coords,
               aes(x = X, y = Y, 
                   group = L2,  # Use the group identifier from st_coordinates
                   fill = municipality_winners$Party[L2],
                   text = municipality_winners$tooltip_text[L2]),
               color = "black",
               size = 0.1) +
  scale_fill_manual(
    values = setNames(party_colors$Color, party_colors$Party),
    name = "Political Parties"
  ) +
  labs(x = NULL,
       y = NULL,
       title = "Swiss Election Results 2023",
       subtitle = "Results by Canton") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )
# Convert to plotly
interactive_plot <- ggplotly(muni_plot_simple, tooltip = "text") %>%
  layout(
    hoverlabel = list(
      bgcolor = "white",
      font = list(size = 12),
      bordercolor = "gray"
    )
  )
# Display the interactive plot
interactive_plot

################################
muni_plot_new <- ggplot() +
  # Canton layer with all party results
  geom_sf(
    data = merged_data,
    mapping = aes(fill = Party, alpha = Percentage, text = tooltip_text),
    color = "white",
    size = 0.1
  ) +
  geom_sf(data = municipality_winners,
          aes(fill = Party, 
              text = tooltip_text),
          color = "white",
          size = 0.1) +
  scale_fill_manual(
    values = setNames(party_colors$Color, party_colors$Party),
    name = "Political Parties"
  ) +
  # Canton borders
  geom_sf(
    data = canton_geo,
    fill = "transparent",
    color = "white",
    size = 0.5
  ) +
  # Lake layer
  geom_sf(
    data = lake_geo,
    fill = "#D6F1FF",
    color = "transparent"
  ) +
  labs(x = NULL,
       y = NULL,
       title = "Swiss Election Results 2023",
       subtitle = "Results by Canton") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )
muni_plot_new
