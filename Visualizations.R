library(ggplot2)
library(dplyr)
library(maptiles)
library(sf)
library(tidyterra)
library(osmdata)
library(readr)
library(tidyr)
library(raster)

setwd("C:/Users/rogej/Documents/hslu/courses/bootcamp/r-bootcamp")
getwd()


election_map <- read_csv("datatable.csv")
party_colors <- read_csv("party_colors.csv")
View(party_colors)

swiss_cantons <- election_map %>%
  distinct(Kanton) %>%  
  filter(!is.na(Kanton)) %>%  
  pull(Kanton)

# Identify the party columns dynamically (columns from CSP_23 to Uebrige_23)
party_cols <- names(election_map)[which(names(election_map) == "CSP_23"):which(names(election_map) == "Uebrige_23")]

# Compute total percentage per canton
canton_totals <- election_map %>%
  pivot_longer(cols = all_of(party_cols), names_to = "Party", values_to = "Votes") %>%
  group_by(Kanton, Party) %>%
  summarise(Total_Votes = sum(Votes), .groups = "drop") %>%
  group_by(Kanton) %>%
  mutate(Percentage = (Total_Votes / sum(Total_Votes)) * 100) %>%
  ungroup()

party_colors %>%
  count(Party) %>%
  filter(n > 1)

party_colors <- party_colors %>%
  distinct(Party, .keep_all = TRUE)

canton_totals <- canton_totals %>%
  mutate(Party = gsub("_23$", "", Party)) %>%  # Remove suffix "_23"
  left_join(party_colors, by = "Party")

# Ensure canton order is maintained
canton_totals$Kanton <- factor(canton_totals$Kanton, levels = swiss_cantons)

View(canton_totals)

# Create the stacked bar plot
ggplot(canton_totals, aes(x = Kanton, y = Percentage, fill = Party)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = setNames(party_colors$Color, party_colors$Party)) + # Apply correct colors
  labs(
    title = "Results of the 2023 Federal Elections by Canton",
    x = "Cantons",
    y = "Vote Percentage",
    fill = "Party"  
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "bottom"
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))



#install.packages("devtools")
#devtools::install_github("ropensci/rnaturalearthhires")


# Load Switzerland canton boundaries (GeoJSON format)
swiss_cantons_map <- ne_states(country = "Switzerland", returnclass = "sf")

# Merge election results with the map data
swiss_cantons_map <- swiss_cantons_map %>%
  mutate(
    code_hasc = sub("CH\\.", "", code_hasc),
    Kanton = code_hasc  
  ) 

# Now perform the join using the extracted canton code
canton_results_map <- swiss_cantons_map %>%
  left_join(canton_totals, by = "Kanton")

View(canton_results_map)

leaflet(canton_results_map) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~colorQuantile("YlOrRd", Percentage)(Percentage),  # Color by percentage
    weight = 1,
    opacity = 1,
    color = "white",
    fillOpacity = 0.7,
    highlight = highlightOptions(weight = 3, color = "#666", fillOpacity = 0.9),
    label = ~paste0(name, ": ", round(Percentage, 1), "%"),
    popup = ~paste0("<b>", name, "</b><br>Percentage: ", round(Percentage, 1), "%")
  ) %>%
  addLegend(pal = colorQuantile("YlOrRd", canton_totals$Percentage), 
            values = canton_totals$Percentage,
            title = "Vote Percentage",
            position = "bottomright")


# canton borders
canton_geo <- read_sf("Shapefiles/g2k23.shp")

# read country borders
country_geo <- read_sf("Shapefiles/g2l23.shp")

# read lakes
lake_geo <- read_sf("Shapefiles/g2s23.shp")

# read productive area (2324 municipalities)
municipality_prod_geo <- read_sf("Shapefiles/gde-1-1-15.shp")

relief <- raster("Shapefiles/02-relief-ascii.asc") %>%
  # hide relief outside of Switzerland by masking with country borders
  mask(country_geo) %>%
  as("SpatialPixelsDataFrame") %>%
  as.data.frame() %>%
  rename(value = `X02.relief.ascii`)

# clean up
rm(country_geo)

library(terra)

# Load the relief raster
relief <- raster("Shapefiles/02-relief-ascii.asc")

# Check and reproject country borders if necessary
if (crs(relief) != st_crs(country_geo)) {
  country_geo <- st_transform(country_geo, crs(relief))
}

# Crop and mask the relief raster
cropped_relief <- crop(relief, country_geo)
masked_relief <- mask(cropped_relief, country_geo)

# Convert to SpatialPixelsDataFrame and then to a data frame
relief_df <- as(masked_relief, "SpatialPixelsDataFrame") %>%
  as.data.frame() %>%
  rename(value = `X02.relief.ascii`)

# Clean up
rm(country_geo)