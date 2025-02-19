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
library(viridis)
library(scales)

setwd("C:/Users/rogej/Documents/hslu/courses/bootcamp/r-bootcamp")

# Define canton colors
canton_colors <- tibble::tibble(
  KTNAME = c(
    "Zürich", "Bern / Berne", "Luzern", "Uri", "Schwyz", "Obwalden", "Nidwalden",
    "Glarus", "Zug", "Fribourg / Freiburg", "Solothurn", "Basel-Stadt", "Basel-Landschaft",
    "Schaffhausen", "Appenzell Ausserrhoden", "Appenzell Innerrhoden",
    "St. Gallen", "Graubünden / Grigioni / Grischun", "Aargau", "Thurgau", "Ticino", "Vaud",
    "Valais / Wallis", "Neuchâtel", "Genève", "Jura"
  ),
  color = c(
    "#0038A8",  # Zürich - Blue
    "#FF0000",  # Bern - Red
    "#0038A8",  # Luzern - Blue and White
    "#000000",  # Uri - Black and Yellow
    "#FF0000",  # Schwyz - Red
    "#FF0000",  # Obwalden - Red and White
    "#FF0000",  # Nidwalden - Red and White
    "#FF0000",  # Glarus - Red and Black
    "#FFFFFF",  # Zug - White and Blue
    "#000000",  # Fribourg - Black and White
    "#FF0000",  # Solothurn - Red and White
    "#000000",  # Basel-Stadt - Black
    "#FF0000",  # Basel-Landschaft - Red and White
    "#000000",  # Schaffhausen - Black
    "#000000",  # Appenzell Ausserrhoden - Black and White
    "#000000",  # Appenzell Innerrhoden - Black and White
    "#008000",  # St. Gallen - Green and White
    "#808080",  # Graubünden - Gray
    "#000000",  # Aargau - Black
    "#008000",  # Thurgau - Green and White
    "#FF0000",  # Ticino - Red and Blue
    "#008000",  # Vaud - Green and White
    "#FF0000",  # Valais - Red and White
    "#008000",  # Neuchâtel - Green and White
    "#FF0000",  # Genève - Red and Yellow
    "#FF0000"   # Jura - Red and White
  )
)

# Error handling for data loading
tryCatch({
  canton_geo <- read_sf("Shapefiles/g2k23.shp") %>% 
    st_transform(3857) %>%
    st_buffer(0) %>%
    # Join with canton colors
    left_join(canton_colors, by = "KTNAME")
  
  lake_geo <- read_sf("Shapefiles/g2s23.shp") %>% 
    st_transform(st_crs(canton_geo)) %>%
    st_buffer(0)
  
  country_geo <- read_sf("Shapefiles/g2l23.shp") %>%
    st_transform(st_crs(canton_geo))
  
  relief <- raster("Shapefiles/02-relief-ascii.asc")
  
  relief_fixed <- relief %>%
    mask(country_geo) %>%
    rasterToPoints() %>%
    as.data.frame() %>%
    mutate(elevation_normalized = scales::rescale(X02.relief.ascii))
  
}, error = function(e) {
  stop("Error loading geographical data: ", e$message)
})

# Create enhanced map
swiss_map <- ggplot() +
  # Base relief layer
  geom_raster(
    data = relief_fixed,
    aes(x = x, y = y, alpha = elevation_normalized),
    fill = "grey50",
    inherit.aes = FALSE
  ) +
  scale_alpha(
    name = "Elevation",
    range = c(0.2, 0.8),
    guide = "none"
  ) +
  # Canton borders with official colors
  geom_sf(
    data = canton_geo,
    aes(fill = color),  
    color = "black",    
    size = 0.5,
    alpha = 0.6
  ) +
  scale_fill_identity() + 
  # Lake layer
  geom_sf(
    data = lake_geo,
    fill = alpha("#D6F1FF", 0.8),
    color = "#ADD8E6",
    size = 0.3
  ) +
  # Styling
  labs(
    title = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    plot.caption = element_text(size = 8, color = "grey50"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(1, 1, 1, 1, "cm")
  ) +
  coord_sf(
    crs = st_crs(canton_geo),
    datum = st_crs(canton_geo)
  )

# Display the map
swiss_map

# Save static version
ggsave(
  "Documentation/swiss_map.png",
  plot = swiss_map,
  width = 12,
  height = 8,
  dpi = 300
)


saveRDS(swiss_map, file = "Documentation/Plots/switzerland_map.rds")
