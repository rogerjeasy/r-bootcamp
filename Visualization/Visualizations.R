library(ggplot2)
library(dplyr)
library(maptiles)
library(sf)
library(tidyterra)
library(osmdata)
library(readr)
library(tidyr)
library(raster)
library(magrittr)
library(plotly)

setwd("C:/Users/rogej/Documents/hslu/courses/bootcamp/r-bootcamp")
getwd()


election_map <- read_csv("Data/datatable.csv")
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

# Create the stacked bar plot
p <- ggplot(canton_totals, aes(x = Kanton, y = Percentage, fill = Party, 
                               text = paste("Canton:", Kanton,
                                            "<br>Party:", Party,
                                            "<br>Percentage:", Percentage, "%"))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = setNames(party_colors$Color, party_colors$Party)) +
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

# Convert to interactive plot
interactive_plot <- ggplotly(p, tooltip = "text") %>%
  layout(legend = list(orientation = "h", y = -0.2))

# Display the interactive plot
interactive_plot

# canton borders
canton_geo <- read_sf("Shapefiles/g2k23.shp")

# read country borders
country_geo <- read_sf("Shapefiles/g2l23.shp")

# read lakes
lake_geo <- read_sf("Shapefiles/g2s23.shp")
