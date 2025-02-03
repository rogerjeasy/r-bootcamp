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


election_results <- read_csv("Data/datatable.csv")
View(election_results)
party_colors <- read_csv("Data/party_colors.csv")
canton_symbols <- read_csv("Data/kanton_names.csv")

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
canton_totals$Kanton <- factor(canton_totals$Kanton, levels = unique(election_map$Kanton))
View(canton_totals)

# canton borders
canton_geo <- read_sf("Shapefiles/g2k23.shp")
canton_geo_with_data <- canton_geo %>%
  left_join(canton_symbols, by = "KTNAME")

canton_geo_with_data <- canton_geo_with_data %>%
  rename(Kanton=KTN_SYMB)
View(canton_geo_with_data)

merged_data <- canton_geo_with_data %>%
  left_join(canton_totals, by = "Kanton")

canton_winners <- merged_data %>%
  group_by(Kanton) %>%
  slice_max(Percentage, n = 1) %>%
  ungroup()

######### Create the plot with winning parties
p <- ggplot() +
  # Base layer with all results
  geom_sf(data = merged_data, 
          aes(fill = Party, 
              text = sprintf("Canton: %s\nParty: %s\nPercentage: %.1f%%", 
                             Kanton, Party, Percentage)),
          color = "white", 
          size = 0.1,
          alpha = 0.3) +
  # Top layer with winning parties
  geom_sf(data = canton_winners,
          aes(fill = Party),
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

# Make the map interactive with custom tooltip
interactive_map <- ggplotly(p, tooltip = "text") %>%
  layout(
    hoverlabel = list(
      bgcolor = "white",
      font = list(family = "Arial", size = 12)
    )
  )

# Display the interactive map
interactive_map
