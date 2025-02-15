rm(list = ls())

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

######### Create the plot with winning parties
p <- ggplot() +
  # Base layer with all results
  geom_sf(data = merged_data, 
          aes(fill = Party, 
              text = tooltip_text),  
          color = "white", 
          size = 0.1,
          alpha = 0.3) +
  # Lake layer
  geom_sf(data = lake_geo, 
          fill = "white",
          color = "black") +
  # Top layer with winning parties
  geom_sf(data = canton_winners,
          aes(fill = Party, 
              text = tooltip_text),
          color = "white",
          size = 0.1) +
  scale_fill_manual(values = setNames(party_colors$Color, party_colors$Party)) +
  theme_minimal() +
  labs(title = "Swiss Election Results 2023",
       subtitle = "Results per canton",
       fill = "Party") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

# Create interactive map
interactive_map <- ggplotly(p, tooltip = "text") %>%
  layout(
    hoverlabel = list(
      bgcolor = "white",
      font = list(family = "Arial", size = 12)
    )
  )

# Display the interactive map
interactive_map

####################################################
# read productive area (2324 municipalities)
municipality_prod_geo <- read_sf("Shapefiles/gde-1-1-15.shp")
View(municipality_prod_geo)

municipality_prod_geo %<>%
  mutate(BFS_ID = sprintf("%04d", BFS_ID))

municipality_prod_geo %<>%
  rename(municipalityId = BFS_ID)


municipality_prod_geo %<>%
  left_join(election_results, by = "municipalityId")

View(municipality_prod_geo)

municipality_election_data <- municipality_prod_geo
View(municipality_election_data)

# Identify the party columns dynamically (columns from CSP_23 to Uebrige_23)
party_cols <- names(municipality_election_data)[which(names(municipality_election_data) == "CSP_23"):which(names(municipality_election_data) == "Uebrige_23")]

# Compute total percentage per municipality
municipality_totals <- municipality_election_data %>%
  pivot_longer(cols = all_of(party_cols), names_to = "Party", values_to = "Votes") %>%
  group_by(municipalityId, Party) %>%
  summarise(Total_Votes = sum(Votes, na.rm = TRUE), .groups = "drop") %>%
  group_by(municipalityId) %>%
  mutate(Percentage = round((Total_Votes / sum(Total_Votes)) * 100, 2)) %>%
  ungroup()

# Remove rows with NA municipalityId
municipality_totals <- municipality_totals %>%
  filter(!is.na(municipalityId))

View(municipality_totals)

# Join with party colors
municipality_totals <- municipality_totals %>%
  mutate(Party = gsub("_23$", "", Party)) %>%  # Remove suffix "_23"
  left_join(party_colors, by = "Party")

# Create tooltip text
municipality_geo_with_data <- municipality_totals %>%
  mutate(tooltip_text = sprintf("Municipality: %s\nParty: %s\nPercentage: %.1f%%", 
                                municipalityId, Party, Percentage))

# Identify winning party per municipality
municipality_winners <- municipality_geo_with_data %>%
  group_by(municipalityId) %>%
  slice_max(Percentage, n = 1) %>%
  ungroup()

# Plot the results with municipalities
p_municipalities <- ggplot() +
  # Base layer with all results
  geom_sf(data = municipality_geo_with_data, 
          aes(fill = Party, 
              text = tooltip_text),  
          color = "white", 
          size = 0.1,
          alpha = 0.3) +
  # Lake layer
  geom_sf(data = lake_geo, 
          fill = "white",
          color = "black") +
  # Top layer with winning parties
  geom_sf(data = municipality_winners,
          aes(fill = Party, 
              text = tooltip_text),
          color = "white",
          size = 0.1) +
  scale_fill_manual(values = setNames(party_colors$Color, party_colors$Party)) +
  theme_minimal() +
  labs(title = "Swiss Election Results 2023",
       subtitle = "Results per municipality",
       fill = "Party") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

# Create interactive map for municipalities
interactive_map_municipalities <- ggplotly(p_municipalities, tooltip = "text") %>%
  layout(
    hoverlabel = list(
      bgcolor = "white",
      font = list(family = "Arial", size = 12)
    )
  )

# Display the interactive map
interactive_map_municipalities

######### Working #############

municipality_totals <- municipality_totals %>%
  mutate(tooltip_text = case_when(
    is.na(municipalityId) | is.na(Party) | is.na(Percentage) ~ "Data unavailable",
    TRUE ~ sprintf("Municipality: %s\nParty: %s\nPercentage: %.1f%%", 
                   municipalityId, Party, Percentage)
  ))

# Check if the mutation worked
print("New structure after adding tooltip:")
str(municipality_totals)

# First ensure municipality_prod_geo has the correct ID format
municipality_prod_geo <- municipality_prod_geo %>%
  mutate(municipalityId = sprintf("%04d", BFS_ID))

# Convert municipality_totals to a regular data frame if it's an sf object
municipality_totals_df <- municipality_geo_with_data
if (inherits(municipality_geo_with_data, "sf")) {
  municipality_totals_df <- st_drop_geometry(municipality_geo_with_data)
}

# Now perform the join
municipality_spatial_data <- municipality_prod_geo %>%
  left_join(municipality_totals_df, by = "municipalityId")
  mutate(Percentage = as.numeric(Percentage))

# Alternative way to process relief
relief_fixed <- raster("Shapefiles/02-relief-ascii.asc") %>%
  mask(country_geo) %>%
  rasterToPoints() %>%
  as.data.frame()

# Check if municipality_spatial_data has the Party column
print(names(municipality_spatial_data))

# Check if there are any values in the Party column
print(table(municipality_spatial_data$Party, useNA = "always"))

# Check if the join worked properly
print(sum(!is.na(municipality_spatial_data$Party)))

# Now try the complete plot with the fixed relief
interactive_map_muni_results <- ggplot() +
  # Relief layer with correct column name
  geom_raster(
    data = relief_fixed,
    aes(x = x, y = y, alpha = X02.relief.ascii),  # Using correct column name
    inherit.aes = FALSE
  ) +
  scale_alpha(name = "",
              range = c(0.1, 0.9),
              guide = "none") +
  # Municipality layer
  geom_sf(
    data = municipality_spatial_data,
    mapping = aes(fill = Party, alpha = Percentage, text = tooltip_text),
    color = "white",
    size = 0.1
  ) +
  scale_fill_manual(
    values = setNames(party_colors$Color, party_colors$Party),
    name = "Political Parties"
  ) +
  geom_sf(
    data = canton_geo,
    fill = "transparent",
    color = "white",
    size = 0.5
  ) +
  geom_sf(
    data = lake_geo,
    fill = "#D6F1FF",
    color = "transparent"
  ) +
  labs(x = NULL,
       y = NULL,
       title = "Swiss Election Results 2023",
       subtitle = "Results by Municipality") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

# Create interactive map for municipalities
interactive_map_muni_results <- ggplotly(interactive_map_muni_results, tooltip = "text") %>%
  layout(
    hoverlabel = list(
      bgcolor = "white",
      font = list(family = "Arial", size = 12)
    )
  )

# Display the interactive map
interactive_map_muni_results

