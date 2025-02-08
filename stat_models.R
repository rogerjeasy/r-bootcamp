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
run_regression <- function(canton, party) {
  df <- data %>%
    filter(Kanton == canton) %>%
    select(all_of(party), incomePerCapita, agequota_pct, edulow_pct, nswisspop_pct, vote_num) %>%
    na.omit()
  
  if (nrow(df) < 3) {
    message(paste("Few data points for canton:", canton, "party:", party))
    return(NULL)
  }
  
  lm(as.formula(paste0(party, " ~ incomePerCapita + agequota_pct + edulow_pct + nswisspop_pct")),
     data = df, weights = vote_num) %>%
    tidy() %>%
    mutate(Canton = canton, Party = party)
}

# Run regressions for all cantons and parties
cantons <- unique(data$Kanton)
parties <- c("CSP_23","EDU_23",
             "EVP_23","FDP_23","FGA_23","GLP_23",         
             "GRUENE_23","Lega_23","LPS_23","MCR_23",      
             "Mitte_23","PdA_Sol_23","SD_23","SP_23",
             "SVP_23","Uebrige_23")

regression_results <- do.call(rbind, lapply(cantons, function(canton) {
  do.call(rbind, lapply(parties, function(party) run_regression(canton, party)))
})) %>%
  filter(term != "(Intercept)") %>% 
  filter(!is.na(estimate)) %>%
  filter(!is.na(statistic)) %>%
  filter(!is.na(p.value)) %>%
  mutate(term = factor(term, levels = c("nswisspop_pct", "edulow_pct", "agequota_pct", "incomePerCapita")))
regression_results

# Regression function for all of Switzerland, adjusted for vote_num
run_regression_switzerland <- function(party) {
  df <- data %>%
    select(all_of(party), incomePerCapita, agequota_pct, edulow_pct, nswisspop_pct, vote_num) %>%
    na.omit()
  
  if (nrow(df) < 3) {  # Check if there is sufficient data
    message(paste("Few data points for party:", party))
    return(NULL)
  }
  
  lm(as.formula(paste0(party, " ~ incomePerCapita + agequota_pct + edulow_pct + nswisspop_pct")),
     data = df, weights = vote_num) %>%
    tidy() %>%
    mutate(Party = party)
}


# Run regressions for all parties across all of Switzerland
regression_results_switzerland <- do.call(rbind, lapply(parties, function(party) run_regression_switzerland(party))) %>%
  filter(term != "(Intercept)") %>% 
  filter(!is.na(estimate)) %>%
  filter(!is.na(statistic)) %>%
  filter(!is.na(p.value)) %>%
  mutate(
    term = factor(term, levels = c("nswisspop_pct", "edulow_pct", "agequota_pct", "incomePerCapita")),
    Canton = "CH"  
  )

combined_regression_results <- bind_rows(regression_results, regression_results_switzerland)
combined_regression_results$Canton <- factor(
  combined_regression_results$Canton, 
  levels = c("CH", sort(setdiff(unique(combined_regression_results$Canton), "CH")))
)

View(combined_regression_results)
#head(regression_results_switzerland)
#head(regression_results)

### HEAT MAP ALL CANTONS ###

heatmap <- ggplot(combined_regression_results %>% filter(term != "(Intercept)"), 
                  aes(x = Party, y = term, fill = estimate)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "red", mid = "white", high = "blue", midpoint = 0,
    name = "Weighted Effect Size"
  ) +
  facet_wrap(~ Canton, ncol = 5 ) + 
  labs(
    title = "Weighted Regression Coefficients of Demographic Factors by Party and Canton",
    x = "Party",
    y = "Demographic Factor"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    axis.text.y = element_text(size = 7),  
    legend.position = "bottom", 
    legend.key.size = unit(0.4, "cm"), 
    legend.text = element_text(size = 7), 
    legend.title = element_text(size = 8), 
    strip.text = element_text(size = 8, face = "bold"), 
    plot.title = element_text(size = 10, hjust = 0.5),  
    plot.subtitle = element_text(size = 8, hjust = 0.5) 
  )

interactive_heatmap <- ggplotly(heatmap, tooltip = c("x", "y", "fill"), width = 1000, height = 800) %>%
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

interactive_heatmap


### FACET SCATTER PLOT ###

p <- ggplot(combined_regression_results, aes(x = estimate, y = term, color = Party, text = paste("Canton:", Canton))) +
  geom_point(size = 3) +
  facet_wrap(~ Canton) +
  labs(title = "Regression Results Across Cantons", x = "Effect Size", y = "Factor") +
  theme_minimal()

ggplotly(p, tooltip = "text", width = 1000, height = 800) %>%
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


