# 1. Import ####################################################################

# Description: Boxplots to visualise the income distribution in Switzerland and 
#              and per cantons.

library(ggplot2)
library(plotly)
library(dplyr)

custom_colors <- c("#FF9999", "#D4A76A", "#91C169", "#3DC280", "#49C7E3", "#47A8FF", "#C28BEE", "#FF85D8")
data <- read.csv("../Data/datatable.csv", stringsAsFactors = FALSE)

data <- data %>%
  filter(incomePerCapita <= quantile(incomePerCapita, 0.99, na.rm = TRUE))

# 2. Exports ###################################################################

## 2.1: Consolidated for all of Switzerland ####################################

boxplot_CH <- ggplot(data, aes(x = "Switzerland", y = incomePerCapita, weight = population)) +
  geom_boxplot(fill = custom_colors[1], color = "black", outlier.color = "red", outlier.shape = 10) +
  labs(title = "Taxable Income Per Capita (Switzerland)",
       x = "Switzerland",
       y = "Taxable Income Per Capita") +
  theme_minimal()

boxplot_CH <- ggplotly(boxplot_CH, width = 600, height = 300)
boxplot_CH


## 2.2: 26 Cantons with custom colors ##########################################

boxplot_cantons <- ggplot(data, aes(x = Kanton, y = incomePerCapita, weight = population, fill = Kanton)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 10) +
  scale_fill_manual(values = rep(custom_colors, length.out = length(unique(data$Kanton)))) +
  labs(title = "Taxable Income Per Capita by Canton",
       x = "Canton",
       y = "Taxable Income Per Capita") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

boxplot_cantons <- ggplotly(boxplot_cantons, width = 800, height = 300)
boxplot_cantons


# 3. Exports ###################################################################

saveRDS(boxplot_CH, "../Documentation/Plots/boxplot_CH.rds")
saveRDS(boxplot_cantons, "../Documentation/Plots/boxplot_cantons.rds")
