# 1. Import ####################################################################


library(tidyr)
library(dplyr)
library(purrr)
library(Hmisc)
library(broom)
library(weights)
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

# 2. Models ####################################################################
## 2.1 Basic Models ############################################################

# The basic models and advanced models below were used for orientation in the 
# data set and are not included in the final product.
# For models used in the analysis (plots etc.) see chapter 2.3 and 2.4.

### 2.1.1 Correlation ##########################################################

cor_result <- cor(data$SVP_23, data$nswisspop_pct, use = "complete.obs", method = "pearson")

### 2.1.2 Weighted Correlation #################################################

wtd_cor_result <- wtd.cor(data$SVP_23, data$nswisspop_pct, weight = data$vote_num)

### 2.1.3 Correlation per Canton ###############################################

get_correlation <- function(df) {
  cor(df$SVP_23, df$nswisspop_pct, use = "pairwise.complete.obs")
}

cor_by_canton <- data %>%
  group_by(Kanton) %>%
  group_split() %>%
  map_dbl(get_correlation)

names(cor_by_canton) <- unique(data$Kanton)

### 2.1.4 Weighted Correlation per canton ###################################### 

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

cor_result
print(cor_by_canton[order(names(cor_by_canton))])

print(wtd_cor_result)
print(wtd_cor_by_canton[order(names(wtd_cor_by_canton))])

## 2.2 Advanced Models #########################################################
### 2.2.1 Linear Model #########################################################

model <- lm(SVP_23 ~ incomePerCapita + agequota_pct + edusec_pct + nswisspop_pct + naturalization_pct, 
            data = data, 
            weights = vote_num)

### 2.2.2 Anova and Interaction Model ##########################################

anova_result <- anova(model)
anova_result
model_interaction <- lm(SVP_23 ~ incomePerCapita * edusec_pct + incomePerCapita * nswisspop_pct + agequota_pct  + naturalization_pct, 
                        data = data,
                        weights = vote_num)

model_interaction

## 2.3 Regression Function #####################################################
# Multiple Linear Regression Model with all parties and demographic factors
# All data was scaled in order normalize the different data types and to account
# for some of the indicators that have smaller (but notable) variance.


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

# Handle missing estimates
missing_income <- combined_regression_results %>%
  filter(term == "incomePerCapita" & is.na(estimate))
print(missing_income)

combined_regression_results <- combined_regression_results %>%
  mutate(estimate = ifelse(is.na(estimate), 0, estimate))


## 2.4. Weighted Data (Unmodelled) #############################################
# Export of the indicators weighted based on populations without further 
# modelling (for some introductory plots).

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

# 3. Export ####################################################################

write.csv(combined_regression_results, 
          file = "../Data/combined_regression_results.csv", 
          row.names = FALSE)

# Confirm the file was saved
file.exists("../Data/combined_regression_results.csv")


write.csv(weighted_data, 
          file = "../Data/weighted_data.csv", 
          row.names = FALSE)

# Confirm the file was saved
file.exists("../Data/weighted_data.csv")


