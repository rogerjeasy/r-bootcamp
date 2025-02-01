library(ggplot2)
library(dplyr)
library(maptiles)
library(sf)
library(tidyterra)
library(osmdata)


# Elections
election_map <- read.csv("datatable.csv",
                         header = TRUE,
                         sep = ";")

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