library(dplyr)
library(purrr)
library(Hmisc)

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

# Print results
cor_result
print(wtd_cor_result)

# Sort correlations by canton name
print(cor_by_canton[order(names(cor_by_canton))])
print(wtd_cor_by_canton[order(names(wtd_cor_by_canton))])


