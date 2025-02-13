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


# Party Colors
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

migrstructure_interactive <- ggplotly(migrstructure)
migrstructure_interactive



##### EDUCATIONAL STRUCTURE ##### 
edustructure_cantons <- weighted_data %>%
  select(Kanton, w_edulow_pct, w_edusec_pct, w_eduter_pct) %>%
  pivot_longer(cols = c(w_edulow_pct, w_edusec_pct, w_eduter_pct), 
               names_to = "Education_Level", 
               values_to = "Weighted_Percentage") %>%
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

edustructure_interactive <- ggplotly(edustructure)
edustructure_interactive

##### PERCENTAGE OF DEMOGRAPHIC SUBSETS PER CANTON ##### 
##### FACETS: DEMOGRAPHIC SUBSETS ##### 

p <- ggplot(scatter_data, aes(x = Kanton, y = Value, size = total_population, text = paste("Canton:", Kanton, "<br>Value:", round(Value, 2)))) +
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

interactive_scatterplot_dem <- ggplotly(p, tooltip = "text", width = 800, height = 800)
interactive_scatterplot_dem

##### PERCENTAGE OF DEMOGRAPHIC SUBSETS PER CANTON ##### 
##### FACETS: CANTONS ##### 

weighted_data <- weighted_data %>%
  mutate(Kanton = factor(Kanton, levels = c("CH", setdiff(unique(Kanton), "CH")))) %>%
  pivot_longer(
    cols = c(w_incomePerCapita, w_agequota_pct, w_edusec_pct, w_nswisspop_pct, w_naturalization_pct),
    names_to = "Factor",
    values_to = "Value"
  )

scatterplot_cantons <- weighted_data %>%
  filter(Factor %in% c("w_agequota_pct", "w_edusec_pct", "w_nswisspop_pct"))

scatterplot_cantons
# Create the scatter plot with each canton as a facet and fixed y-axis
p <- ggplot(scatterplot_cantons, aes(x = Factor, y = Value, size = total_population, text = paste("Factor:", Factor, "<br>Value:", round(Value, 2)))) +
  geom_point(alpha = 0.8, aes(color = Factor)) +
  facet_wrap(~ Kanton, ncol = 4, scales="fixed" ) + 
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
interactive_scatterplot_canton <- ggplotly(p, tooltip = "text", width = 800, height = 800)
interactive_scatterplot_canton

##### SVP Success versus demographic indicators (not modelled) ##### 
##### FACETS: MUNICIPALITES ##### 

create_interactive_plot <- function(data, x_var, y_var, group_var = NULL, title, tooltip_vars = c("municipality", "Canton"), 
                                    jitter = FALSE, log_scale = FALSE, fix_negative = FALSE, y_range = NULL) {
  
  data <- data %>% mutate(across(all_of(group_var), as.factor))  # Convert group_var to factor if provided
  
  # Construct tooltip text dynamically
  tooltip_text <- paste0(tooltip_vars[1], ": ", data[[tooltip_vars[1]]], "<br>",
                         tooltip_vars[2], ": ", data[[tooltip_vars[2]]], "<br>",
                         x_var, ": ", round(data[[x_var]], 2), "<br>",
                         y_var, ": ", round(data[[y_var]], 2))
  
  p <- ggplot(data, aes_string(x = x_var, y = y_var, text = "tooltip_text")) +
    geom_point(aes(color = if (!is.null(group_var)) as.factor(get(group_var)) else NULL), 
               alpha = 0.5, 
               position = if (jitter) position_jitter(width = 0.2, height = 0.2) else position_identity()) +
    labs(title = title, x = x_var, y = y_var) +  
    theme_minimal() +
    theme(
      legend.position = "bottom",
      panel.grid.major = element_line(color = "gray80", size = 0.5),  # Subtle grid lines
      panel.grid.minor = element_blank(),
      panel.border = element_blank()
    )

  ggplotly(p, tooltip = "text", width = 600, height = 400)
}

# Adjusting y-axis focus ranges for specific plots
plot_SVP23vsAge <- create_interactive_plot(data, "SVP_23", "agequota_pct", NULL, "SVP_23 vs Age Quota (%)", 
                                 tooltip_vars = c("municipality", "Canton"), jitter = TRUE)

plot_SVP23vsIncome <- create_interactive_plot(data, "SVP_23", "incomePerCapita", NULL, "SVP_23 vs Income Per Capita", 
                                 tooltip_vars = c("municipality", "Canton"), jitter = TRUE, y_range = c(0, 50000))

plot_SVP23vsNSwissPop <- create_interactive_plot(data, "SVP_23", "nswisspop_pct", NULL, "SVP_23 vs Non-Swiss Population (%)", 
                                 tooltip_vars = c("municipality", "Canton"), jitter = TRUE)

plot_SVP23vsNaturalization <- create_interactive_plot(data, "SVP_23", "naturalization_pct", NULL, "SVP_23 vs Naturalization Rate (%)", 
                                 tooltip_vars = c("municipality", "Canton"), jitter = TRUE, fix_negative = TRUE, y_range = c(0, 5))

plot_SVPvsPopSize <- create_interactive_plot(data, "SVP_23", "population", NULL, "SVP_23 vs Population", 
                                 tooltip_vars = c("municipality", "Canton"), log_scale = TRUE, y_range = c(0, 10000))

data_district <- data %>%
  group_by(districtId) %>%
  summarise(SVP_23 = mean(SVP_23, na.rm = TRUE),
            edusec_pct = mean(edusec_pct, na.rm = TRUE)) %>%
  ungroup()

plot_SVPvsEduLvl <- create_interactive_plot(data_district, "SVP_23", "edusec_pct", NULL, 
                                 "SVP_23 vs Secondary Education (%) (Grouped by District)", tooltip_vars = c("districtId", "Canton"))


#############################
##### DATA PLOTS ############
#### (STAT. MODELS) #########
#############################

missing_income <- combined_regression_results %>%
  filter(term == "incomePerCapita" & is.na(estimate))
print(missing_income)

combined_regression_results <- combined_regression_results %>%
  mutate(estimate = ifelse(is.na(estimate), 0, estimate))

##### HEAT MAP ALL CANTONS ##### 

heatmap <- ggplot(combined_regression_results %>% filter(term != "(Intercept)"), 
                  aes(x = Party, y = term, fill = estimate)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "red", mid = "white", high = "blue", midpoint = 0,
    limits = c(-2, 2),  # Set meaningful limits for the color scale
    oob = scales::squish,  # Squish extreme values to the limits
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

regr_heatmap <- ggplotly(heatmap, tooltip = c("x", "y", "fill"), width = 800, height = 800) %>%
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


##### FACET SCATTER PLOT ##### 

p <- ggplot(combined_regression_results, aes(
  x = estimate, 
  y = term, 
  color = Party, 
  text = paste("Canton:", Canton, "<br>Party:", Party, "<br>Effect Size:", round(estimate, 2))
  )) +
  geom_jitter(width = 0.1, height = 0, size = 2) +  # Jitter instead of static points
  facet_wrap(~ Canton) +
  scale_x_continuous(
    trans = scales::pseudo_log_trans(base = 2),  # Use base 2 for smoother scaling
    limits = c(-10, 10)
  ) +
  scale_color_manual(values = party_colors) +  
  labs(title = "Regression Results Across Cantons", x = "Effect Size (Log Scaled)", y = "Factor") +
  theme_minimal()


regr_scatter <- ggplotly(p, tooltip = "text", width = 800, height = 800) %>%
  layout(
    hoverlabel = list(
      bgcolor = "white",
      font = list(size = 12)
    ),
    legend = list(
      orientation = "v", # Vertical legend
      x = 1.05,          # Place legend to the right of the plot
      y = 0.5,           # Center the legend vertically
      xanchor = "left"   # Align legend to the left
    ),
    margin = list(l = 50, r = 200, t = 50, b = 50) # Add space for the legend
  )

regr_scatter

##### SAVONG THE PLOTS ##### 
saveRDS(plot_SVP23vsAge, "Documentation/Plots/plot_SVP23vsAge.rds")
saveRDS(plot_SVP23vsIncome, "Documentation/Plots/plot_SVP23vsIncome.rds")
saveRDS(plot_SVP23vsNSwissPop, "Documentation/Plots/plot_SVP23vsNSwissPop.rds")
saveRDS(plot_SVP23vsNaturalization, "Documentation/Plots/plot_SVP23vsNaturalization.rds")
saveRDS(plot_SVPvsPopSize, "Documentation/Plots/plot_SVPvsPopSize.rds")
saveRDS(plot_SVPvsEduLvl, "Documentation/Plots/plot_SVPvsEduLvl.rds")
saveRDS(migrstructure_interactive, "Documentation/Plots/migrstructure.rds")
saveRDS(edustructure_interactive, "Documentation/Plots/edustructure.rds")
saveRDS(interactive_scatterplot_dem, "Documentation/Plots/demogr_scatter.rds")
saveRDS(interactive_scatterplot_canton, "Documentation/Plots/cantons_scatter.rds")
saveRDS(regr_scatter, "Documentation/Plots/regr_scatter.rds")
saveRDS(regr_heatmap, "Documentation/Plots/regr_heatmap.rds")

