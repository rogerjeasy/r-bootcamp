# 1. Import #######################################################################

# Description: The scatter plots and bar charts visualise the main indicators
#              for a comparative perspective.


library(tidyr)
library(dplyr)
library(Hmisc)
library(ggplot2)
library(plotly)
library(broom)
library(scales) 

data <- read.csv("../Data/datatable.csv", stringsAsFactors = FALSE)
data <- data %>% filter(!is.na(Kanton))
data <- data %>%
  mutate(
    incomePerCapita_scl = as.numeric(scale(incomePerCapita, center = TRUE, scale = TRUE)),
    agequota_scl = as.numeric(scale(agequota_pct, center = TRUE, scale = TRUE)),
    edulow_scl = as.numeric(scale(edulow_pct, center = TRUE, scale = TRUE)),
    edusec_scl = as.numeric(scale(edusec_pct, center = TRUE, scale = TRUE)),
    eduter_scl = as.numeric(scale(edulow_pct, center = TRUE, scale = TRUE)),
    nswisspop_scl = as.numeric(scale(nswisspop_pct, center = TRUE, scale = TRUE)),
    naturalization_pct = (100/nswisspop_num*naturalization_num),
    naturalization_scl = as.numeric(scale((naturalization_pct), center = TRUE, scale = TRUE))
  )

combined_regression_results <- read.csv("../Data/combined_regression_results.csv", stringsAsFactors = FALSE)

weighted_data <- read.csv("../Data/weighted_data.csv", stringsAsFactors = FALSE)
# Create a pivoted version specifically for demographic factors
weighted_data_pivot <- weighted_data %>%
  pivot_longer(
    cols = starts_with("w_"),
    names_to = "Factor",
    values_to = "Value",
    names_repair = "unique"
  ) %>%
  mutate(Factor = ifelse(Factor %in% names(column_labels), column_labels[Factor], Factor))

# 2. Templates #####################################################################
# 2.1 Colors #####################################################################
party_colors <- c(
  "SVP_23" = "#008000",
  "SP_23" = "#BB0000",
  "FDP_23" = "#00529F",
  "Mitte_23" = "#E39D2B",
  "GRUENE_23" = "#83AD20",
  "GLP_23" = "#C4D600",
  "EVP_23" = "#FFD700",
  "Lega_23" = "#4876FF",
  "CSP_23" = "#008B8B", 
  "PdA_Sol_23" = "#FF4500",
  "MCR_23" = "#DAA520",
  "Sol_23" = "#DC143C",
  "EDU_23" = "#C71585"
)


canton_colors <- c(
  "GE" = "#7B001C",  
  "VD" = "#9E2A1F",  
  "NE" = "#C04000",  
  "JU" = "#C71585", 
  "FR" = "#DC143C",  
  "BE" = "#FF8C00",  
  "AG" = "#DAA520",  
  "SO" = "#B8860B",  
  "BL" = "#6B8E23", 
  "BS" = "#FFC107",  
  "TG" = "#6CA82E",  
  "ZH" = "#FFD700",  
  "SH" = "#008000", 
  "LU" = "#4682B4",  
  "ZG" = "#5F9EA0",
  "SZ" = "#0073E6", 
  "UR" = "#4169E1", 
  "OW" = "#6495ED", 
  "NW" = "#4682B4",  
  "SG" = "#228B22",  
  "GR" = "#006400", 
  "GL" = "#2E8B57", 
  "AR" = "#20B2AA",  
  "AI" = "#008B8B",  
  "TI" = "#800080",  
  "VS" = "#8B0000"   
)

# 2.2 Labels #######################################################################

column_labels <- c(
  "population" = "Population",
  "w_agequota_pct" = "Age Quota",
  "agequota_pct" = "Age Quota",
  "agequota_scl" = "Age Quota",
  "w_edulow_pct" = "Low Education",
  "edulow_pct" = "Low Education",
  "edulow_scl" = "Low Education",
  "w_edusec_pct" = "Secondary Education",
  "edusec_pct" = "Secondary Education",
  "edusec_scl" = "Secondary Education", 
  "w_eduter_pct" = "Tertiary Education",
  "eduter_pct" = "Tertiary Education",
  "eduter_scl" = "Tertiary Education",
  "w_incomePerCapita" = "Tax Income per Capita",
  "incomePerCapita" = "Tax Income per Capita",
  "incomePerCapita_scl" = "Tax Income per Capita", 
  "w_naturalization_pct" = "Naturalization Rate",
  "naturalization_pct" = "Naturalization Rate",
  "naturalization_pct" = "Naturalization Rate", 
  "naturalization_scl" = "Naturalization Rate",
  "w_swisspop_pct" = "Swiss Population",
  "swisspop_pct" = "Swiss Population",
  "w_nswisspop_pct" = "Non-Swiss Population",
  "nswisspop_scl" = "Non-Swiss Population", 
  "nswisspop_pct" = "Non-Swiss Population"
)

# 3. Plots #####################################################################
## 3.1 Percentage of Demographic Subset (Totals) ###############################
# Description: Scatterplot showing all demographic facets used in the analysis,
#              based on the weighted data (unmodelled).

# Reshape the data into long format

scatter_data <- weighted_data_pivot %>%
  mutate(Factor = ifelse(Factor %in% names(column_labels), column_labels[Factor], Factor))

p <- ggplot(scatter_data, aes(
  x = Kanton, 
  y = Value, 
  size = total_population, 
  text = paste("Canton:", Kanton, "<br>Factor:", Factor, "<br>Value:", round(Value, 2))
)) +
  geom_point(alpha = 0.8, aes(color = Factor)) +
  facet_wrap(~ Factor, ncol = 1, scales = "free_y") + 
  labs(
    title = "Weighted Demographic Factors by Canton",
    x = "Canton",
    y = "Weighted Percentage",
    color = "Factor",
    size = "Total Votes"
  ) +
  scale_size(range = c(1, 12)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "right"
  )

interactive_scatterplot_dem <- ggplotly(p, tooltip = "text", width = 800, height = 900)

## 3.2 Percentage of demographic Subset (Cantons) #############################
# Description: Scatterplot showing the main demographic facets used in the 
#              analysis, based on the weighted data (unmodelled).

# Apply readable labels to `Factor`

weighted_data_pivot <- weighted_data_pivot %>%
  mutate(Kanton = factor(Kanton, levels = c("CH", setdiff(unique(Kanton), "CH")))) %>%
  mutate(Factor = ifelse(Factor %in% names(column_labels), column_labels[Factor], Factor))  # Apply labels

# Filter for selected factors
scatterplot_cantons <- weighted_data_pivot %>%
  filter(Factor %in% c(column_labels["w_agequota_pct"], column_labels["w_edusec_pct"], column_labels["w_nswisspop_pct"]))

# Create the scatter plot
p <- ggplot(scatterplot_cantons, aes(
  x = Factor, 
  y = Value, 
  size = total_population, 
  text = paste("Factor:", Factor, "<br>Value:", round(Value, 2))
)) +
  geom_point(alpha = 0.8, aes(color = Factor)) +
  facet_wrap(~ Kanton, ncol = 4, scales = "fixed") + 
  labs(
    title = "Weighted Demographic Factors by Canton",
    x = "Demographic Factor",
    y = "Weighted Percentage",
    color = "Factor",
    size = "Total Votes"
  ) +
  scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, by = 10)) +  # Set y-axis range
  scale_size(range = c(3, 12)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "right",
    strip.text = element_text(size = 8, face = "bold")  # Adjust facet label size
  )

interactive_scatterplot_canton <- ggplotly(p, tooltip = "text", width = 800, height = 900)

## 3.3 SVP Success versus demographic indicators (not modelled) ################
# Description: Scatterplot showing the main demographic facets vs SVP election 
#              result, based on the weighted data (unmodelled).
#              Indicator "education" only available for districts, that's why 
#              some additional aggregation was necessary.

create_interactive_plot <- function(data,x_var,y_var, group_var = "Kanton", title, tooltip_vars = c("municipality", "Kanton"), 
                                    jitter = FALSE, log_scale = FALSE, fix_negative = FALSE, y_range = NULL) {
  
  data <- data %>%
    mutate(across(all_of(group_var), as.factor))  # Convert Kanton to factor
  
  # Use column_labels to get readable labels
  x_label <- ifelse(x_var %in% names(column_labels), column_labels[[x_var]], x_var)
  y_label <- ifelse(y_var %in% names(column_labels), column_labels[[y_var]], y_var)
  
  # Construct tooltip text dynamically with readable labels
  tooltip_text <- paste0(tooltip_vars[1], ": ", data[[tooltip_vars[1]]], "<br>",
                         tooltip_vars[2], ": ", data[[tooltip_vars[2]]], "<br>",
                         x_label, ": ", round(data[[x_var]], 2), "<br>",
                         y_label, ": ", round(data[[y_var]], 2))
  
  p <- ggplot(data, aes_string(x = x_var, y = y_var, text = "tooltip_text")) +
    geom_point(aes(
      color = Kanton,  
      size = !!sym("population")  
    ), 
    alpha = 0.5, 
    position = if (jitter) position_jitter(width = 0.2, height = 0.2) else position_identity()) +
    scale_size(range = c(1, 10)) +  
    scale_color_manual(values = canton_colors) +  
    labs(x = x_label, y = y_label, title = title) + 
    theme_minimal() +
    theme(
      legend.position = "bottom",
      panel.grid.major = element_line(color = "gray80", size = 0.5),
      panel.grid.minor = element_blank(),
      panel.border = element_blank()
    )
  
  # Apply log scale only if log_scale = TRUE
  if (log_scale) {
    p <- p + scale_y_log10(labels = scales::comma)  
  } else {
    p <- p + scale_y_continuous(labels = scales::comma)  # 
  }
  
  ggplotly(p, tooltip = "text", width = 800, height = 400)
}

plot_SVP23vsAge <- create_interactive_plot(data, "SVP_23", "agequota_pct", NULL, "SVP Election Result 2023 and Age Quota by Municipality", 
                                           tooltip_vars = c("municipality", "Kanton"), jitter = TRUE)

plot_SVP23vsIncome <- create_interactive_plot(data, "SVP_23", "incomePerCapita", NULL, "SVP Election Result 2023 and Income Per Capita by Municipality", 
                                              tooltip_vars = c("municipality", "Kanton"), jitter = TRUE, y_range = c(0, 50000))

plot_SVP23vsNSwissPop <- create_interactive_plot(data, "SVP_23", "nswisspop_pct", NULL, "SVP Election Result 2023 and Non-Swiss Population (%) by Municipality", 
                                                 tooltip_vars = c("municipality", "Kanton"), jitter = TRUE)

plot_SVP23vsNaturalization <- create_interactive_plot(data, "SVP_23", "naturalization_pct", NULL, "SVP Election Result 2023 and Naturalization Rate (%) by Municipality", 
                                                      tooltip_vars = c("municipality", "Kanton"), jitter = TRUE, fix_negative = TRUE, y_range = c(0, 5))

plot_SVPvsPopSize <- create_interactive_plot(data, "SVP_23", "population", NULL, "SVP Election Result 2023 and Population by Municipality", 
                                             tooltip_vars = c("municipality", "Kanton"), log_scale = TRUE, y_range = c(0, 10000))
plot_SVPvsPopSize

data_district <- data %>%
  group_by(districtId, districtName, Kanton) %>%
  summarise(
    SVP_23 = mean(SVP_23, na.rm = TRUE),
    edusec_pct = mean(edusec_pct, na.rm = TRUE),
    population = sum(population, na.rm = TRUE) 
  ) %>%
  ungroup() %>%
  mutate(Kanton = as.factor(Kanton))

plot_SVPvsEduLvl <- create_interactive_plot(data_district, "SVP_23", "edusec_pct", NULL, 
                                            "SVP Election Result 2023 and Secondary Education (%) by District", tooltip_vars = c("districtId","districtName", "Kanton"))
plot_SVPvsEduLvl
data <- data %>%
  filter(SVP_19 > 0 & SVP_23 > 0) %>% 
  mutate(Change_SVP = SVP_23 - SVP_19)  

plot_SVP_Change <- create_interactive_plot(
  data, 
  x_var = "SVP_23", 
  y_var = "Change_SVP", 
  group_var = "Kanton", 
  title = "SVP Vote Gains and Losses by Municipality (2019 vs. 2023)", 
  tooltip_vars = c("municipality", "Kanton")
)

## 3.4 Facet Scatter Plot ######################################################
# Description: Scatterplot showing the regression estimates demographic facets 
#              vs election outcomes of all parties (regression model, scaled).


scatter_data <- combined_regression_results %>%
  mutate(term = ifelse(term %in% names(column_labels), column_labels[term], term)) %>%
  ungroup() %>%
  mutate(Canton = factor(Canton, levels = c("CH", sort(setdiff(unique(Canton), "CH")))))

p <- ggplot(scatter_data, aes(
  x = estimate, 
  y = term,  
  color = Party, 
  text = paste("Canton:", Canton, "<br>Party:", Party, "<br>Regression Estimate:", round(estimate, 2))
)) +
  geom_jitter(width = 0.1, height = 0, size = 2) +  
  facet_wrap(~ Canton, ncol = 4 ) +
  scale_x_continuous(
    trans = scales::pseudo_log_trans(base = 2),  
    limits = c(-10, 10)
  ) +
  scale_color_manual(values = party_colors) +  
  labs(title = "Regression Results Across Cantons", x = "Regression Estimate (Log Scaled)", y = "Factor") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 8),  # Reduce x-axis text size
    axis.text.y = element_text(size = 8)   # Reduce y-axis text size
  )

regr_scatter <- ggplotly(p, tooltip = "text", width = 800, height = 800) %>%
  layout(
    hoverlabel = list(
      bgcolor = "white",
      font = list(size = 12)
    ),
    legend = list(
      orientation = "v",  
      x = 1.05,          
      y = 0.5,           
      xanchor = "left"   
    ),
    margin = list(l = 50, r = 200, t = 50, b = 50) 
  )

## 3.5 Migrant Populations #####################################################
# Description: Bar plots with migrant populations per canton (unmodelled).

migrstructure_cantons <- weighted_data %>%
  select(Kanton, w_nswisspop_pct, w_swisspop_pct) %>%
  pivot_longer(cols = c(w_nswisspop_pct, w_swisspop_pct), 
               names_to = "Migrant_Population", 
               values_to = "Weighted_Percentage") %>%
  mutate(Migrant_Population = ifelse(Migrant_Population %in% names(column_labels), 
                                     column_labels[Migrant_Population], Migrant_Population)) %>%  # Apply labels
  group_by(Kanton) %>%
  mutate(Weighted_Percentage = 100 * Weighted_Percentage / sum(Weighted_Percentage, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Kanton = factor(Kanton, levels = c("CH", sort(setdiff(unique(Kanton), "CH")))))

migrstructure <- ggplot(migrstructure_cantons, aes(x = Kanton, y = Weighted_Percentage, fill = Migrant_Population)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Weighted Migrant Population Distribution by Canton",
       x = "Canton",
       y = "Weighted Percentage (%)",
       fill = "Migrant Population") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

migrstructure_interactive <- ggplotly(migrstructure, width = 800, height = 300)
migrstructure_interactive

## 3.6 Educational Structure ################################################### 
# Description: Barplot percentage of population only holding secondary level 
#              education (Berufslehre/Aprentissage) per canton (unmodelled).

edustructure_cantons <- weighted_data %>%
  select(Kanton, w_edulow_pct, w_edusec_pct, w_eduter_pct) %>%
  pivot_longer(cols = c(w_edulow_pct, w_edusec_pct, w_eduter_pct), 
               names_to = "Education_Level", 
               values_to = "Weighted_Percentage") %>%
  mutate(Education_Level = ifelse(Education_Level %in% names(column_labels), 
                                  column_labels[Education_Level], Education_Level)) %>% 
  group_by(Kanton) %>%
  mutate(Weighted_Percentage = 100 * Weighted_Percentage / sum(Weighted_Percentage, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Kanton = factor(Kanton, levels = c("CH", sort(setdiff(unique(Kanton), "CH")))))

edustructure <- ggplot(edustructure_cantons, aes(x = Kanton, y = Weighted_Percentage, fill = Education_Level)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Weighted Education Level Distribution by Canton",
       x = "Canton",
       y = "Weighted Percentage (%)",
       fill = "Education Level") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

edustructure_interactive <- ggplotly(edustructure, width = 800, height = 300)
edustructure_interactive



# 4. Exports ###################################################################

# 3.1 / 3.2
saveRDS(interactive_scatterplot_dem, "../Documentation/Plots/demogr_scatter.rds")
saveRDS(interactive_scatterplot_canton, "../Documentation/Plots/cantons_scatter.rds")

# 3.3
saveRDS(plot_SVP23vsAge, "../Documentation/Plots/plot_SVP23vsAge.rds")
saveRDS(plot_SVP23vsIncome, "../Documentation/Plots/plot_SVP23vsIncome.rds")
saveRDS(plot_SVP23vsNSwissPop, "../Documentation/Plots/plot_SVP23vsNSwissPop.rds")
saveRDS(plot_SVP23vsNaturalization, "../Documentation/Plots/plot_SVP23vsNaturalization.rds")
saveRDS(plot_SVPvsPopSize, "../Documentation/Plots/plot_SVPvsPopSize.rds")
saveRDS(plot_SVPvsEduLvl, "../Documentation/Plots/plot_SVPvsEduLvl.rds")
saveRDS(plot_SVP_Change, "../Documentation/Plots/plot_SVP_Change.rds")

# 3.4 - 3.5
saveRDS(regr_scatter, "../Documentation/Plots/regr_scatter.rds")
saveRDS(migrstructure_interactive, "../Documentation/Plots/migrstructure.rds")
saveRDS(edustructure_interactive, "../Documentation/Plots/edustructure.rds")

