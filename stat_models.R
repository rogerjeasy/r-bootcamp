#############################
####### Import ##############
#############################

library(tidyr)
library(dplyr)
library(purrr)
library(Hmisc)
library(ggplot2)
library(plotly)
library(broom)
library(weights)
library(scales) 

data <- read.csv("Data/datatable.csv", stringsAsFactors = FALSE)
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

#############################
####### Models ##############
#############################

## Basic Correlation 
cor_result <- cor(data$SVP_23, data$nswisspop_pct, use = "complete.obs", method = "pearson")

## Weighted Correlation 
wtd_cor_result <- wtd.cor(data$SVP_23, data$nswisspop_pct, weight = data$vote_num)

## Basic Correlation per canton
get_correlation <- function(df) {
  cor(df$SVP_23, df$nswisspop_pct, use = "pairwise.complete.obs")
}

cor_by_canton <- data %>%
  group_by(Kanton) %>%
  group_split() %>%
  map_dbl(get_correlation)

names(cor_by_canton) <- unique(data$Kanton)

## Weighted Correlation per canton

get_wtd_correlation <- function(df) {
  wtd_cor_result <- tryCatch(
    wtd.cor(df$SVP_23, df$nswisspop_pct, weight = df$vote_num),
    error = function(e) return(NA_real_)
  )
  
  if (is.matrix(wtd_cor_result) && "correlation" %in% colnames(wtd_cor_result)) {
    return(as.numeric(wtd_cor_result["Y", "correlation"]))  
  } else {
    return(NA_real_)
  }
}

split_data <- data %>% group_by(Kanton) %>% group_split()

wtd_cor_by_canton <- split_data %>%
  map_dbl(~ get_wtd_correlation(.x))

names(wtd_cor_by_canton) <- map_chr(split_data, ~ unique(.x$Kanton))


###  Correlations per canton but every municipality = 1 unit (non-adjusted) ###
cor_result
print(cor_by_canton[order(names(cor_by_canton))])


### Correlations per canton but adjusted for population ###
print(wtd_cor_result)
print(wtd_cor_by_canton[order(names(wtd_cor_by_canton))])

model <- lm(SVP_23 ~ incomePerCapita + agequota_pct + edusec_pct + nswisspop_pct + naturalization_pct, 
            data = data, 
            weights = vote_num)

### Anova and Interaction Model ###

anova_result <- anova(model)
anova_result
model_interaction <- lm(SVP_23 ~ incomePerCapita * edusec_pct + incomePerCapita * nswisspop_pct + agequota_pct  + naturalization_pct, 
                        data = data,
                        weights = vote_num)

model_interaction

### Regression Models ###

# Unified regression function

# Unique Cantons & Parties
cantons <- unique(data$Kanton)
parties <- c("CSP_23","EDU_23", "EVP_23","FDP_23","FGA_23","GLP_23",
             "GRUENE_23","Lega_23","LPS_23","MCR_23", "Mitte_23",
             "PdA_Sol_23","SD_23","SP_23", "SVP_23","Uebrige_23")

# Filter valid canton-party pairs
valid_canton_party <- data %>%
  select(Kanton, all_of(parties)) %>%
  pivot_longer(-Kanton, names_to = "Party", values_to = "Votes") %>%
  group_by(Kanton, Party) %>%
  summarise(Total_Votes = sum(Votes, na.rm = TRUE), .groups = "drop") %>%
  filter(Total_Votes > 0) %>%
  select(Kanton, Party)

# Regression function (handles both Switzerland & Cantons)
run_regression <- function(df, party, canton = NULL) {
  df <- df %>%
    filter(if (!is.null(canton)) Kanton == canton else TRUE, !is.na(!!sym(party))) %>%
    select(all_of(party), naturalization_scl, incomePerCapita_scl, agequota_scl, edusec_scl, nswisspop_scl, vote_num) %>%
    na.omit()
  
  if (nrow(df) < 3) return(tibble(term = names(df)[-c(1, length(df))], estimate = NA, std.error = NA, statistic = NA, p.value = NA, Party = party, Canton = canton %||% "CH"))
  
  lm(as.formula(paste0(party, " ~ naturalization_scl + incomePerCapita_scl + agequota_scl + edusec_scl + nswisspop_scl")),
     data = df, weights = vote_num) %>%
    tidy() %>%
    mutate(Party = party, Canton = canton %||% "CH")
}

# Run regressions for all Canton-Party combinations
regression_results <- valid_canton_party %>%
  group_by(Kanton, Party) %>%
  group_split() %>%
  map_dfr(~ run_regression(data, .x$Party[1], .x$Kanton[1]))
View(regression_results)
# Run regressions for all of Switzerland
regression_results_switzerland <- map_dfr(parties, ~ run_regression(data, .x))

# Combine results
combined_regression_results <- bind_rows(regression_results, regression_results_switzerland) %>%
  filter(term != "(Intercept)") %>%
  mutate(Canton = factor(Canton, levels = c("CH", sort(setdiff(unique(Canton), "CH")))))


#############################
##### DATA PLOTS ############
#### (NOT MODELLED) #########
#############################

# Compute weighted demographic factors for both Cantons and Switzerland
weighted_data <- data %>%
  group_by(Kanton) %>%
  summarise(
    across(
      c(incomePerCapita, agequota_pct, edulow_pct, edusec_pct, eduter_pct, nswisspop_pct, swisspop_pct),
      ~ sum(.x * population, na.rm = TRUE) / sum(population, na.rm = TRUE),
      .names = "w_{.col}"
    ),
    w_naturalization_pct = sum(naturalization_pct * nswisspop_num, na.rm = TRUE) / sum(nswisspop_num, na.rm = TRUE),
    total_population = sum(population, na.rm = TRUE),
    .groups = "drop"
  )

# Compute weighted data for all of Switzerland and append it
weighted_data <- weighted_data %>%
  bind_rows(
    data %>%
      summarise(
        across(
          c(incomePerCapita, agequota_pct, edulow_pct, edusec_pct, eduter_pct, nswisspop_pct, swisspop_pct),
          ~ sum(.x * population, na.rm = TRUE) / sum(population, na.rm = TRUE),
          .names = "w_{.col}"
        ),
        w_naturalization_pct = sum(naturalization_pct * nswisspop_num, na.rm = TRUE) / sum(nswisspop_num, na.rm = TRUE),
        total_population = sum(population, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(Kanton = "CH")
  ) %>%
  mutate(Kanton = factor(Kanton, levels = c("CH", setdiff(unique(Kanton), "CH"))))

# Reshape the data into long format
scatter_data <- weighted_data %>%
  pivot_longer(
    cols = starts_with("w_"),
    names_to = "Factor",
    values_to = "Value"
  )

##### MIGRANT POPULATIONS ##### 

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

##### EDUCATIONAL STRUCTURE ##### 

edustructure_cantons <- weighted_data %>%
  select(Kanton, w_edulow_pct, w_edusec_pct, w_eduter_pct) %>%
  pivot_longer(cols = c(w_edulow_pct, w_edusec_pct, w_eduter_pct), 
               names_to = "Education_Level", 
               values_to = "Weighted_Percentage") %>%
  mutate(Education_Level = ifelse(Education_Level %in% names(column_labels), 
                                  column_labels[Education_Level], Education_Level)) %>%  # Apply labels
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


##### PERCENTAGE OF DEMOGRAPHIC SUBSETS PER CANTON ##### 
##### FACETS: DEMOGRAPHIC SUBSETS ##### 

scatter_data <- scatter_data %>%
  mutate(Factor = ifelse(Factor %in% names(column_labels), column_labels[Factor], Factor))

p <- ggplot(scatter_data, aes(
  x = Kanton, 
  y = Value, 
  size = total_population, 
  text = paste("Canton:", Kanton, "<br>Factor:", Factor, "<br>Value:", round(Value, 2))
)) +
  geom_point(alpha = 0.8, aes(color = Factor)) +
  facet_wrap(~ Factor, ncol = 1, scales = "free_y") +  # Facet labels now display readable names
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
interactive_scatterplot_dem


##### PERCENTAGE OF DEMOGRAPHIC SUBSETS PER CANTON ##### 
##### FACETS: CANTONS ##### 

# Apply readable labels to `Factor`
weighted_data <- weighted_data %>%
  mutate(Kanton = factor(Kanton, levels = c("CH", setdiff(unique(Kanton), "CH")))) %>%
  pivot_longer(
    cols = c(w_incomePerCapita, w_agequota_pct, w_edusec_pct, w_nswisspop_pct, w_naturalization_pct),
    names_to = "Factor",
    values_to = "Value"
  ) %>%
  mutate(Factor = ifelse(Factor %in% names(column_labels), column_labels[Factor], Factor))  # Apply labels

# Filter for selected factors
scatterplot_cantons <- weighted_data %>%
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

# Convert to interactive plot
interactive_scatterplot_canton <- ggplotly(p, tooltip = "text", width = 800, height = 900)
interactive_scatterplot_canton


##### SVP Success versus demographic indicators (not modelled) ##### 
##### FACETS: MUNICIPALITES ##### 
create_interactive_plot <- function(data, x_var, y_var, group_var = "Kanton", title, tooltip_vars = c("municipality", "Kanton"), 
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
    labs(x = x_label, y = y_label, title = title) +  # Apply readable labels
    theme_minimal() +
    theme(
      legend.position = "bottom",
      panel.grid.major = element_line(color = "gray80", size = 0.5),
      panel.grid.minor = element_blank(),
      panel.border = element_blank()
    )
  
  # Apply log scale only if log_scale = TRUE
  if (log_scale) {
    p <- p + scale_y_log10(labels = scales::comma)  # Ensures raw numbers display in log scale
  } else {
    p <- p + scale_y_continuous(labels = scales::comma)  # Keeps normal numeric labels
  }
  
  ggplotly(p, tooltip = "text", width = 800, height = 400)
}

# Adjusting y-axis focus ranges for specific plots
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
    population = sum(population, na.rm = TRUE)  # Summing population for districts
  ) %>%
  ungroup() %>%
  mutate(Kanton = as.factor(Kanton))  # Ensure Kanton remains a factor

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

plot_SVP_Change

#############################
##### DATA PLOTS ############
#### (STAT. MODELS) #########
#############################

# Handle missing estimates
missing_income <- combined_regression_results %>%
  filter(term == "incomePerCapita" & is.na(estimate))
print(missing_income)

combined_regression_results <- combined_regression_results %>%
  mutate(estimate = ifelse(is.na(estimate), 0, estimate))

write.csv(combined_regression_results, 
          file = "Data/combined_regression_results.csv", 
          row.names = FALSE)

# Confirm the file was saved
file.exists("Data/combined_regression_results.csv")

##### HEATMAP WITH LABELS #####

heatmap_data <- combined_regression_results %>% 
  filter(term != "(Intercept)") %>%
  mutate(term = ifelse(term %in% names(column_labels), column_labels[term], term))  # Apply readable labels

heatmap <- ggplot(heatmap_data, 
                  aes(x = Party, y = term, fill = estimate)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "red", mid = "white", high = "blue", midpoint = 0,
    limits = c(-2, 2),  
    oob = scales::squish,  
    name = "Weighted Effect Size"
  ) +
  facet_wrap(~ Canton, ncol = 4 ) + 
  labs(
    title = "Weighted Regression Coefficients of Demographic Factors by Party and Canton",
    x = "Party",
    y = "Demographic Factor"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 7),
    axis.text.y = element_text(size = 7),  
    legend.position = "bottom", 
    legend.key.size = unit(0.4, "cm"), 
    legend.text = element_text(size = 7), 
    legend.title = element_text(size = 8), 
    strip.text = element_text(size = 8, face = "bold"), 
    plot.title = element_text(size = 10, hjust = 0.5),  
    plot.subtitle = element_text(size = 8, hjust = 0.5) 
  )

regr_heatmap <- ggplotly(heatmap, tooltip = c("x", "y", "fill"), width = 800, height = 600) %>%
  layout(
    hoverlabel = list(
      bgcolor = "white",
      font = list(size = 12)
    ),
    legend = list(
      orientation = "h",  
      y = -0.2,
      x = 0.5,
      xanchor = "center"
    )
  )

regr_heatmap

##### FACET SCATTER PLOT WITH LABELS #####

scatter_data <- combined_regression_results %>%
  mutate(term = ifelse(term %in% names(column_labels), column_labels[term], term))  # Apply readable labels

p <- ggplot(scatter_data, aes(
  x = estimate, 
  y = term,  
  color = Party, 
  text = paste("Canton:", Canton, "<br>Party:", Party, "<br>Effect Size:", round(estimate, 2))
)) +
  geom_jitter(width = 0.1, height = 0, size = 2) +  
  facet_wrap(~ Canton, ncol = 4 ) +
  scale_x_continuous(
    trans = scales::pseudo_log_trans(base = 2),  
    limits = c(-10, 10)
  ) +
  scale_color_manual(values = party_colors) +  
  labs(title = "Regression Results Across Cantons", x = "Effect Size (Log Scaled)", y = "Factor") +
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

regr_scatter


##### SAVONG THE PLOTS ##### 
saveRDS(plot_SVP23vsAge, "Documentation/Plots/plot_SVP23vsAge.rds")
saveRDS(plot_SVP23vsIncome, "Documentation/Plots/plot_SVP23vsIncome.rds")
saveRDS(plot_SVP23vsNSwissPop, "Documentation/Plots/plot_SVP23vsNSwissPop.rds")
saveRDS(plot_SVP23vsNaturalization, "Documentation/Plots/plot_SVP23vsNaturalization.rds")
saveRDS(plot_SVPvsPopSize, "Documentation/Plots/plot_SVPvsPopSize.rds")
saveRDS(plot_SVPvsEduLvl, "Documentation/Plots/plot_SVPvsEduLvl.rds")
saveRDS(plot_SVP_Change, "Documentation/Plots/plot_SVP_Change.rds")
saveRDS(migrstructure_interactive, "Documentation/Plots/migrstructure.rds")
saveRDS(edustructure_interactive, "Documentation/Plots/edustructure.rds")
saveRDS(interactive_scatterplot_dem, "Documentation/Plots/demogr_scatter.rds")
saveRDS(interactive_scatterplot_canton, "Documentation/Plots/cantons_scatter.rds")
saveRDS(regr_scatter, "Documentation/Plots/regr_scatter.rds")
#saveRDS(regr_heatmap, "Documentation/Plots/regr_heatmap.rds")

