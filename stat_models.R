library(dplyr)
library(purrr)
library(Hmisc)
library(ggplot2)
library(broom)

data <- read.csv("Data/datatable.csv", stringsAsFactors = FALSE)
data <- data %>% filter(!is.na(Kanton))

## Basic Correlation 
cor_result <- cor(data$SVP_23, data$nswisspop_pct, use = "complete.obs", method = "pearson")

## Weighted Correlation 
library(weights)
wtd_cor_result <- wtd.cor(data$SVP_23, data$nswisspop_pct, weight = data$swisspop_num)

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
    wtd.cor(df$SVP_23, df$nswisspop_pct, weight = df$swisspop_num),
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

# Correlations per canton but every municipality = 1 unit (non-adjusted)
cor_result
print(cor_by_canton[order(names(cor_by_canton))])

# Correlations per canton but adjusted for population
print(wtd_cor_result)
print(wtd_cor_by_canton[order(names(wtd_cor_by_canton))])


head(data)

model <- lm(SVP_23 ~ incomePerCapita + agequota_pct + edulow_pct + nswisspop_pct, 
            data = data, 
            weights = population)

anova_result <- anova(model)
summary(model)
print(anova_result)

colnames(data)

model_interaction <- lm(SVP_23 ~ incomePerCapita * edulow_pct + incomePerCapita * nswisspop_pct + agequota_pct, data = data)
summary(model_interaction)


# Regression for all Cantons
run_regression <- function(canton, party) {
  df <- data %>%
    filter(data$Kanton == canton) %>%  
    select(all_of(party), incomePerCapita, agequota_pct, edulow_pct, nswisspop_pct) %>% 
    na.omit() 
  model <- lm(as.formula(paste0(party, " ~ incomePerCapita + agequota_pct + edulow_pct + nswisspop_pct")), data = df)
  return(tidy(model))  
}

cantons <- unique(data$Kanton)
parties <- c("SP_23", "SVP_23", "FDP_23", "GRUENE_23", "GLP_23", "Mitte_23")  

results <- lapply(cantons, function(canton) {
  lapply(parties, function(party) {
    result <- run_regression(canton, party)
    result$Canton <- canton
    result$Party <- party
    return(result)
  })
})



# Heatmap for all cantons
run_regression <- function(canton, party) {
  df <- data %>%
    filter(Kanton == canton) %>%  
    select(all_of(party), incomePerCapita, agequota_pct, edulow_pct, nswisspop_pct) %>% 
    na.omit() 
  
  if (nrow(df) < 3) {  # Include regressions with fewer rows
    message(paste("Few data points for canton:", canton, "party:", party))
  }
  
  model <- lm(as.formula(paste0(party, " ~ incomePerCapita + agequota_pct + edulow_pct + nswisspop_pct")), data = df)
  return(tidy(model))  
}

# Generate results
results <- lapply(unique(data$Kanton), function(canton) {
  lapply(c("SP_23", "SVP_23", "FDP_23", "GRUENE_23", "GLP_23", "Mitte_23"), function(party) {
    result <- run_regression(canton, party)
    if (!is.null(result)) {
      result$Canton <- canton
      result$Party <- party
      return(result)
    }
  })
})

# Flatten results and ensure no missing values
regression_results <- do.call(rbind, unlist(results, recursive = FALSE)) %>%
  filter(!is.na(estimate))  %>% # Ensure valid estimates
  mutate(term = factor(term, levels = c("nswisspop_pct", "edulow_pct", "agequota_pct","incomePerCapita")))


# Debug missing cantons
missing_cantons <- setdiff(unique(data$Kanton), unique(regression_results$Canton))
print(missing_cantons)

# Heatmap for all results (non-significant included)
heatmap <- ggplot(regression_results %>% filter(term != "(Intercept)"), aes(x = Party, y = term, fill = estimate)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "red", mid = "white", high = "blue", midpoint = 0, 
    name = "Effect Size"
  ) +
  facet_wrap(~ Canton, ncol = 4) +  
  labs(
    title = "Regression Coefficients of Demographic Factors by Party and Canton",
    x = "Party",
    y = "Demographic Factor"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), 
    strip.text = element_text(size = 10, face = "bold"), 
    legend.position = "right"
  )

print(heatmap)


# Aggregated Heatmap Switzerland
aggregated_results <- regression_results %>%
  filter(term != "(Intercept)") %>%
  group_by(term, Party) %>%
  summarise(
    avg_estimate = mean(estimate, na.rm = TRUE),
    significant = any(p.value < 0.05),  # Mark as significant if significant in any canton
    .groups = "drop"
  )

heatmap <- ggplot(aggregated_results, aes(x = Party, y = term, fill = avg_estimate)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "red", mid = "white", high = "blue", midpoint = 0, 
    name = "Avg. Effect Size"
  ) +
  labs(
    title = "Average Regression Coefficients of Demographic Factors",
    subtitle = "Across All Cantons",
    x = "Party",
    y = "Demographic Factor"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )
print(heatmap)


