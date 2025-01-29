data <- read.csv("datatable.csv", stringsAsFactors = FALSE)
cor(data$SVP_result, data$percentage_non_swiss_pop, use = "complete.obs", method = "pearson")

library(weights)
wtd_cor_result <- wtd.cor(data$SVP_result, data$percentage_non_swiss_pop, weight = data$population)
print(wtd_cor_result)
