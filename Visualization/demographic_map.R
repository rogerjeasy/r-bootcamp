library(leaflet)
library(dplyr)
library(sf)
library(readr)
library(scales)
library(htmltools)

setwd("C:/Users/rogej/Documents/hslu/courses/bootcamp/r-bootcamp")
getwd()

# Read geographical data
canton_geo <- read_sf("Shapefiles/g2k23.shp") %>% 
  st_transform(4326)
canton_symbols <- read_csv("Data/kanton_names.csv", show_col_types = FALSE)

# Read demographic dataset focusing on income per capita
demographic_data <- read_csv("Data/combined_regression_results.csv", show_col_types = FALSE)


lake_geo <- read_sf("Shapefiles/g2s23.shp") %>% 
  st_transform(4326)  # Transform to WGS84

country_geo <- read_sf("Shapefiles/g2l23.shp") %>% 
  st_transform(4326)  # Transform to WGS84

st_crs(country_geo)

######################## INCOME

# Process income data with error handling
income_by_canton <- demographic_data %>%
  filter(term == "incomePerCapita_scl") %>%
  select(Canton, Party, estimate, p.value) %>%
  mutate(
    significant = p.value < 0.05,
    estimate = ifelse(is.na(estimate), 0, estimate)  # Replace NA with 0
  ) %>%
  group_by(Canton) %>%
  summarise(
    incomePerCapita = mean(estimate, na.rm = TRUE),
    sig_effects = sum(significant, na.rm = TRUE),
    total_parties = n()
  ) %>%
  filter(!is.na(incomePerCapita))  # Remove any remaining NA values

# Join with geometry
canton_geo_with_data <- canton_geo %>%
  left_join(canton_symbols, by = "KTNAME") %>%
  rename(Kanton = KTN_SYMB) %>%
  left_join(income_by_canton, by = c("Kanton" = "Canton"))

# Print summary to check data
print("Summary of joined data:")
print(summary(canton_geo_with_data$incomePerCapita))

# Create color palette with error handling
domain_values <- canton_geo_with_data$incomePerCapita[!is.na(canton_geo_with_data$incomePerCapita)]
pal <- colorNumeric(
  palette = "RdYlBu",
  domain = range(domain_values),
  na.color = "#808080"
)

# Create the map
income_map <- leaflet(options = leafletOptions(minZoom = 7, maxZoom = 12)) %>%
  setView(lng = 8.2275, lat = 46.8182, zoom = 8) %>%
  
  # Add white background
  addPolygons(
    data = country_geo,
    fillColor = "white",
    fillOpacity = 1,
    weight = 0,
    color = "transparent"
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
  
  # Add cantons
  addPolygons(
    data = canton_geo_with_data,
    fillColor = ~pal(incomePerCapita),
    fillOpacity = 0.7,
    weight = 1,
    color = "white",
    label = ~lapply(paste0(
      "<strong>Canton:</strong> ", Kanton, "<br/>",
      "<strong>Income Effect:</strong> ", 
      ifelse(is.na(incomePerCapita), "No data", 
             round(incomePerCapita, 2)), "<br/>",
      "<strong>Significant Effects:</strong> ", 
      ifelse(is.na(sig_effects), "No data",
             paste0(sig_effects, "/", total_parties)), " parties"
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
  )

# Add legend only if we have valid data
if(length(domain_values) > 0) {
  income_map <- income_map %>%
    addLegend(
      position = "topright",
      pal = pal,
      values = domain_values,
      title = "Income Per Capita<br/>(scaled)",
      opacity = 0.7,
      labFormat = labelFormat(transform = function(x) round(x, 2))
    
    ) %>%
  
  # Set bounds to Switzerland
  fitBounds(5.9, 45.8, 10.5, 47.8)
}

income_map

saveRDS(income_map, file = "Documentation/Plots/map_income_plot_cantons.rds")

######################## EDUCATION

# Process education data with error handling
education_by_canton <- demographic_data %>%
  filter(term == "edusec_scl") %>%
  select(Canton, Party, estimate, p.value) %>%
  mutate(
    significant = p.value < 0.05,
    estimate = ifelse(is.na(estimate), 0, estimate)  # Replace NA with 0
  ) %>%
  group_by(Canton) %>%
  summarise(
    education = mean(estimate, na.rm = TRUE),
    sig_effects = sum(significant, na.rm = TRUE),
    total_parties = n()
  ) %>%
  filter(!is.na(education))  # Remove any remaining NA values

# Join with geometry
canton_geo_with_data <- canton_geo %>%
  left_join(canton_symbols, by = "KTNAME") %>%
  rename(Kanton = KTN_SYMB) %>%
  left_join(education_by_canton, by = c("Kanton" = "Canton"))

# Print summary to check data
print("Summary of joined data:")
print(summary(canton_geo_with_data$education))

# Create color palette with error handling
domain_values <- canton_geo_with_data$education[!is.na(canton_geo_with_data$education)]
pal <- colorNumeric(
  palette = "RdYlBu",
  domain = range(domain_values),
  na.color = "#808080"
)

# Create the map
education_map <- leaflet(options = leafletOptions(minZoom = 7, maxZoom = 12)) %>%
  setView(lng = 8.2275, lat = 46.8182, zoom = 8) %>%
  
  # Add white background
  addPolygons(
    data = country_geo,
    fillColor = "white",
    fillOpacity = 1,
    weight = 0,
    color = "transparent"
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
  
  # Add cantons
  addPolygons(
    data = canton_geo_with_data,
    fillColor = ~pal(education),
    fillOpacity = 0.7,
    weight = 1,
    color = "white",
    label = ~lapply(paste0(
      "<strong>Canton:</strong> ", Kanton, "<br/>",
      "<strong>Education Effect:</strong> ", 
      ifelse(is.na(education), "No data", 
             round(education, 2)), "<br/>",
      "<strong>Significant Effects:</strong> ", 
      ifelse(is.na(sig_effects), "No data",
             paste0(sig_effects, "/", total_parties)), " parties"
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
  )

# Add legend only if we have valid data
if(length(domain_values) > 0) {
  education_map <- education_map %>%
    addLegend(
      position = "topright",
      pal = pal,
      values = domain_values,
      title = "Education<br/>(scaled)",
      opacity = 0.7,
      labFormat = labelFormat(transform = function(x) round(x, 2))
      
    ) %>%
    
    # Set bounds to Switzerland
    fitBounds(5.9, 45.8, 10.5, 47.8)
}

education_map

saveRDS(education_map, file = "Documentation/Plots/map_education_plot_cantons.rds")
