library(ggplot2)
library(dplyr)
library(maptiles)
library(sf)
library(tidyterra)
library(osmdata)
library(readr)
library(tidyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(leaflet)

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
































# Create a vector of all municipalities from your data
municipalities_list <- election_map$municipality

# Get boundaries for all municipalities
boarders <- purrr::map_df(municipalities_list, function(muni) {
  tryCatch({
    opq(muni) %>% 
      add_osm_feature(key = "boundary", 
                      value = c("administrative")) %>% 
      osmdata_sf() %>% 
      .$osm_multipolygons %>%
      filter(admin_level == "8")  # Filter for municipality level
  }, error = function(e) NULL)
}) %>%
  st_as_sf()

# Get background tiles for the entire region
tiles_background <- get_tiles(boarders, 
                              provider = "OpenStreetMap", 
                              zoom = 12)

# Join your election data with the spatial data
election_map_final <- boarders %>%
  left_join(election_map, by = c("name" = "Municipality"))

# Create the visualization
ggplot() +
  # Add background map
  geom_spatraster_rgb(data = tiles_background, 
                      maxcell = 5e6) +
  # Add municipality boundaries with SVP results
  geom_sf(data = election_map_final,
          aes(fill = SVP_result),
          colour = "black", 
          linewidth = 0.5, 
          alpha = 0.7) +
  scale_fill_gradient(low = "white", 
                      high = "darkgreen",
                      name = "SVP Result (%)") +
  # Set map bounds
  coord_sf(xlim = st_bbox(boarders)[c(1, 3)],
           ylim = st_bbox(boarders)[c(2, 4)],
           expand = FALSE) +
  # Customize theme
  theme_minimal() +
  labs(title = "SVP Election Results by Municipality",
       subtitle = "Districts of Affoltern and Andelfingen") +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "right"
  )