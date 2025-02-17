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

municipality_geo_data <- read_sf("Shapefiles/municipality/K4polg20230101vf_ch2007Poly.shp")

# Load the relief raster
relief <- raster("Shapefiles/02-relief-ascii.asc")

relief_fixed <- raster("Shapefiles/02-relief-ascii.asc") %>%
  mask(country_geo) %>%
  rasterToPoints() %>%
  as.data.frame()

# Identify the party columns dynamically (columns from CSP_23 to Uebrige_23)
party_cols <- names(election_results)[which(names(election_results) == "CSP_23"):which(names(election_results) == "Uebrige_23")]


municipality_totals <- election_results %>%
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


municipality_totals <- municipality_totals %>%
  filter(!is.na(municipality) & !(is.na(Party) & is.na(Percentage)))

party_colors %>%
  count(Party) %>%
  filter(n > 1)

party_colors <- party_colors %>%
  distinct(Party, .keep_all = TRUE)


municipality_totals <- municipality_totals %>%
  mutate(Party = gsub("_23$", "", Party)) %>%  # Remove suffix "_23"
  left_join(party_colors, by = "Party")

# Ensure canton order is maintained
municipality_totals$municipality <- factor(municipality_totals$municipality, levels = unique(election_results$municipality))

municipality_geo <- municipality_geo_data

municipality_geo %<>%
  mutate(BFS_ID = sprintf("%04d", id))

municipality_geo %<>%
  rename(municipality = name)

municipality_geo %<>%
  rename(municipalityId = BFS_ID)


municipality_geo %<>%
  left_join(municipality_totals, by = "municipality")

municipality_geo %<>%
  filter(!is.na(municipality) & !(is.na(Party) & is.na(Percentage)))

municipality_geo <- municipality_geo %>%
  mutate(tooltip_text = sprintf("Municipality: %s\nParty: %s\nPercentage: %.1f%%", 
                                municipality, Party, Percentage))

municipality_winners <- municipality_geo %>%
  filter(!is.na(municipality) & !(is.na(Party) & is.na(Percentage))) %>% 
  group_by(municipality) %>%
  slice_max(Percentage, n = 1) %>%
  ungroup()

# Create the canton-level plot
municipality_result_map <- ggplot() +
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
              range = c(0.3, 0.8),
              guide = "none") +
  # Canton layer with all party results
  geom_sf(
    data = municipality_geo,
    mapping = aes(fill = Party, alpha = Percentage, text = tooltip_text),
    color = "white",
    size = 0.1
  ) +
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
       subtitle = "Results by Municipality") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )
# At the end of your code, after creating municipality_result_map, add:
interactive_map <- ggplotly(municipality_result_map, tooltip = "text") %>%
  layout(
    hoverlabel = list(
      bgcolor = "white",
      font = list(family = "Arial", size = 12)
    )
  )

# Display the interactive map
interactive_map


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
