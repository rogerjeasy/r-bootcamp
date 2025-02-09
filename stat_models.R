#############################
####### Import ##############
#############################

library(dplyr)
library(purrr)
library(Hmisc)
library(ggplot2)
library(plotly)
library(broom)
library(plotly)
library(weights)

data <- read.csv("Data/datatable.csv", stringsAsFactors = FALSE)
data <- data %>% filter(!is.na(Kanton))
data <- data %>%
  mutate(
    incomePerCapita_scl = as.numeric(scale(incomePerCapita, center = TRUE, scale = TRUE)),
    agequota_scl = as.numeric(scale(agequota_pct, center = TRUE, scale = TRUE)),
    edulow_scl = as.numeric(scale(edulow_pct, center = TRUE, scale = TRUE)),
    nswisspop_scl = as.numeric(scale(nswisspop_pct, center = TRUE, scale = TRUE))
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

model <- lm(SVP_23 ~ incomePerCapita + agequota_pct + edulow_pct + nswisspop_pct, 
            data = data, 
            weights = vote_num)

### Anova and Interaction Model ###

anova_result <- anova(model)
anova_result
model_interaction <- lm(SVP_23 ~ incomePerCapita * edulow_pct + incomePerCapita * nswisspop_pct + agequota_pct, 
                        data = data,
                        weights = vote_num)

model_interaction

### Regression Models ###

# Unified regression function

library(dplyr)
library(broom)
library(tidyr)

cantons <- unique(data$Kanton)
parties <- c("CSP_23","EDU_23", "EVP_23","FDP_23","FGA_23","GLP_23",         
             "GRUENE_23","Lega_23","LPS_23","MCR_23", "Mitte_23",
             "PdA_Sol_23","SD_23","SP_23", "SVP_23","Uebrige_23")

valid_canton_party <- data %>%
  select(Kanton, all_of(parties)) %>%
  pivot_longer(-Kanton, names_to = "Party", values_to = "Votes") %>%
  group_by(Kanton, Party) %>%
  summarise(Total_Votes = sum(Votes, na.rm = TRUE), .groups = "drop") %>%
  filter(Total_Votes > 0) %>%
  select(Kanton, Party)

full_regression_df <- expand.grid(
  Canton = unique(valid_canton_party$Kanton),
  Party = unique(valid_canton_party$Party),
  term = c("nswisspop_scl", "edulow_scl", "agequota_scl", "incomePerCapita_scl"),
  stringsAsFactors = FALSE
) %>%
  inner_join(valid_canton_party, by = c("Canton" = "Kanton", "Party")) %>%
  mutate(
    estimate = NA_real_,
    std.error = NA_real_,
    statistic = NA_real_,
    p.value = NA_real_
  )

run_regression <- function(canton, party) {
  df <- data %>%
    filter(Kanton == canton, !is.na(!!sym(party))) %>%
    select(all_of(party), incomePerCapita_scl, agequota_scl, edulow_scl, nswisspop_scl, vote_num) %>%
    na.omit()
  
  if (nrow(df) < 3) {
    return(tibble(
      term = c("nswisspop_scl", "edulow_scl", "agequota_scl","incomePerCapita_scl"),
      estimate = NA_real_,
      std.error = NA_real_,
      statistic = NA_real_,
      p.value = NA_real_,
      Canton = canton,
      Party = party
    ))
  }
  
  lm(as.formula(paste0(party, " ~ incomePerCapita_scl + agequota_scl + edulow_scl + nswisspop_scl")),
     data = df, weights = vote_num) %>%
    tidy() %>%
    mutate(Canton = canton, Party = party)
}

regression_results <- bind_rows(lapply(unique(valid_canton_party$Kanton), function(canton) {
  bind_rows(lapply(valid_canton_party$Party[valid_canton_party$Kanton == canton], function(party) run_regression(canton, party)))
})) %>%
  filter(term != "(Intercept)")

regression_results <- full_regression_df %>%
  left_join(regression_results, by = c("Canton", "Party", "term")) %>%
  mutate(
    estimate = coalesce(estimate.y, estimate.x),
    std.error = coalesce(std.error.y, std.error.x),
    statistic = coalesce(statistic.y, statistic.x),
    p.value = coalesce(p.value.y, p.value.x)
  ) %>%
  select(-ends_with(".x"), -ends_with(".y"))

run_regression_switzerland <- function(party) {
  df <- data %>%
    select(all_of(party), incomePerCapita_scl, agequota_scl, edulow_scl, nswisspop_scl, vote_num) %>%
    na.omit()
  
  if (nrow(df) < 3) {
    return(tibble(
      term = c("nswisspop_scl", "edulow_scl", "agequota_scl", "incomePerCapita_scl"),
      estimate = NA_real_,
      std.error = NA_real_,
      statistic = NA_real_,
      p.value = NA_real_,
      Party = party
    ))
  }
  
  lm(as.formula(paste0(party, " ~ incomePerCapita_scl + agequota_scl + edulow_scl + nswisspop_scl")),
     data = df, weights = vote_num) %>%
    tidy() %>%
    mutate(Party = party)
}

regression_results_switzerland <- bind_rows(lapply(parties, function(party) run_regression_switzerland(party))) %>%
  filter(term != "(Intercept)") %>%
  mutate(Canton = "CH")

combined_regression_results <- bind_rows(regression_results, regression_results_switzerland)

combined_regression_results$Canton <- factor(
  combined_regression_results$Canton, 
  levels = c("CH", sort(setdiff(unique(combined_regression_results$Canton), "CH")))
)


#############################
##### DATA PLOTS ############
#### (NOT MODELLED) #########
#############################

weighted_data <- data %>%
  group_by(Kanton) %>%
  summarise(
    w_incomePerCapita = sum(incomePerCapita * population, na.rm = TRUE) / sum(population, na.rm = TRUE),
    w_agequota_pct = sum(agequota_pct * population, na.rm = TRUE) / sum(population, na.rm = TRUE),
    w_edulow_pct = sum(edulow_pct * population, na.rm = TRUE) / sum(population, na.rm = TRUE),
    w_nswisspop_pct = sum(nswisspop_pct * population, na.rm = TRUE) / sum(population, na.rm = TRUE),
    total_population = sum(population, na.rm = TRUE)  # To scale point sizes
  )
weighted_data


# Reshape the data
scatter_data <- weighted_data %>%
  pivot_longer(
    cols = c(w_incomePerCapita, w_agequota_pct, w_edulow_pct, w_nswisspop_pct),
    names_to = "Factor",
    values_to = "Value"
  )

# Weighted data for all of Switzerland
weighted_data_switzerland <- data %>%
  summarise(
    w_incomePerCapita = sum(incomePerCapita * population, na.rm = TRUE) / sum(population, na.rm = TRUE),
    w_agequota_pct = sum(agequota_pct * population, na.rm = TRUE) / sum(population, na.rm = TRUE),
    w_edulow_pct = sum(edulow_pct * population, na.rm = TRUE) / sum(population, na.rm = TRUE),
    w_nswisspop_pct = sum(nswisspop_pct * population, na.rm = TRUE) / sum(population, na.rm = TRUE),
    total_population = sum(population, na.rm = TRUE)  # Total population
  ) %>%
  mutate(Kanton = "CH")  # Add "CH" for Switzerland

scatter_data_switzerland <- weighted_data_switzerland %>%
  pivot_longer(
    cols = c(w_incomePerCapita, w_agequota_pct, w_edulow_pct, w_nswisspop_pct),
    names_to = "Factor",
    values_to = "Value"
  )

# Join CH total into Cantons

scatter_data <- bind_rows(scatter_data, scatter_data_switzerland)

scatter_data <- scatter_data %>%
  mutate(Kanton = factor(Kanton, levels = c("CH", setdiff(unique(Kanton), "CH"))))


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

interactive_scatterplot_dem <- ggplotly(p, tooltip = "text", width = 1000, height = 1000)
interactive_scatterplot_dem

##### PERCENTAGE OF DEMOGRAPHIC SUBSETS PER CANTON ##### 
##### FACETS: CANTONS ##### 

weighted_data <- bind_rows(weighted_data, weighted_data_switzerland)

weighted_data <- weighted_data %>%
  mutate(Kanton = factor(Kanton, levels = c("CH", setdiff(unique(Kanton), "CH"))))
View(scatter_data)

# Reshape the data to make demographic factors the x-axis
weighted_data <- weighted_data %>%
  pivot_longer(
    cols = c(w_incomePerCapita, w_agequota_pct, w_edulow_pct, w_nswisspop_pct),
    names_to = "Factor",
    values_to = "Value"
  )

# Create the scatter plot with each canton as a facet and fixed y-axis
p <- ggplot(weighted_data, aes(x = Factor, y = Value, size = total_population, text = paste("Factor:", Factor, "<br>Value:", round(Value, 2)))) +
  geom_point(alpha = 0.8, aes(color = Factor)) +
  facet_wrap(~ Kanton, ncol = 4, scales="fixed" ) + 
  labs(
    title = "Weighted Demographic Factors by Canton",
    x = "Demographic Factor",
    y = "Weighted Percentage",
    color = "Factor",
    size = "Total Votes"
  ) +
  scale_y_continuous(limits = c(10, 50), breaks = seq(10, 50, by = 10)) +  # Set y-axis range
  scale_size(range = c(3, 12)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "right",
    strip.text = element_text(size = 8, face = "bold")  # Adjust facet label size
  )

# Convert to interactive plot
interactive_scatterplot_canton <- ggplotly(p, tooltip = "text", width = 1000, height = 1000)
interactive_scatterplot_canton


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

regr_heatmap <- ggplotly(heatmap, tooltip = c("x", "y", "fill"), width = 1000, height = 1000) %>%
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
  labs(title = "Regression Results Across Cantons", x = "Effect Size (Log Scaled)", y = "Factor") +
  theme_minimal()


regr_scatter <- ggplotly(p, tooltip = "text", width = 1000, height = 1000) %>%
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
saveRDS(interactive_scatterplot_dem, "Documentation/Plots/demogr_scatter.rds")
saveRDS(interactive_scatterplot_canton, "Documentation/Plots/cantons_scatter.rds")
saveRDS(regr_scatter, "Documentation/Plots/regr_scatter.rds")
saveRDS(regr_heatmap, "Documentation/Plots/regr_heatmap.rds")

