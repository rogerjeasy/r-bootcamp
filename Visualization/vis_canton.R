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
  pivot_longer(cols = all_of(party_cols), names_to = "Party", values_to = "Votes") %>%
  group_by(Kanton, Party) %>%
  summarise(Total_Votes = sum(Votes), .groups = "drop") %>%
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
ggplot() +
  # Relief layer
  geom_raster(
    data = relief_fixed,
    aes(x = x, y = y, alpha = X02.relief.ascii),
    inherit.aes = FALSE
  ) +
  scale_alpha(name = "",
              range = c(0, 100),
              guide = "none") +
  # Canton layer with all party results
  geom_sf(
    data = merged_data,
    mapping = aes(fill = Party, alpha = Percentage, text = tooltip_text),
    color = "white",
    size = 0.1
  ) +
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

# Create interactive map for cantons
interactive_map_canton_results <- ggplotly(canton_result_map, tooltip = "text")
interactive_map_canton_results <- layout(interactive_map_canton_results,
                                         hoverlabel = list(
                                           bgcolor = "white",
                                           font = list(family = "Arial", size = 12)
                                         ))

# Display the interactive map
interactive_map_canton_results













# Convert sf objects to regular dataframes with coordinates
coords_check <- merged_data %>%
  st_cast("POLYGON") %>%
  st_coordinates()

# Print first few rows to see the column names
head(coords_check)

merged_data_coords <- merged_data %>%
  st_transform(4326) %>%
  st_cast("POLYGON") %>%
  mutate(id = row_number()) %>%
  broom::tidy(region = "id") %>%
  rename(group = id)

# Join with original attributes
merged_data_final <- merged_data_coords %>%
  left_join(
    merged_data %>% 
      st_drop_geometry() %>%
      mutate(group = row_number()),
    by = "group"
  )

# Join with the original attribute data
merged_data_attrs <- merged_data %>%
  st_drop_geometry() %>%
  mutate(group = row_number())

merged_data_final <- merged_data_coords %>%
  left_join(merged_data_attrs, by = "group")

# Create the modified plot
canton_result_map <- ggplot() +
  # Relief layer
  geom_raster(
    data = relief_fixed,
    aes(x = x, y = y, alpha = X02.relief.ascii),
    inherit.aes = FALSE
  ) +
  scale_alpha(name = "",
              range = c(0.1, 0.9),
              guide = "none") +
  # Canton layer with all party results
  geom_polygon(
    data = merged_data_final,
    mapping = aes(x = long, y = lat, fill = Party, 
                  alpha = Percentage/100, group = group,
                  text = tooltip_text),
    color = "white",
    size = 0.1
  ) +
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

# Create interactive plot
interactive_map_canton_results <- ggplotly(canton_result_map, tooltip = "text")