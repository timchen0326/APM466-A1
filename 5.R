# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)

# Load bond data
bonds <- read.csv("filtered_data/filtered_bonds.csv")

# Extract relevant yield and forward rate columns
closing_price_columns <- grep("^X2025", colnames(bonds), value = TRUE)

# Compute log-returns for yield rates
compute_log_returns <- function(rates_matrix) {
  log_returns <- matrix(0, nrow = nrow(rates_matrix), ncol = (ncol(rates_matrix) - 1))
  
  for (i in 1:nrow(rates_matrix)) {
    for (j in 1:(ncol(rates_matrix) - 1)) {
      log_returns[i, j] <- log(rates_matrix[i, j+1] / rates_matrix[i, j])
    }
  }
  return(log_returns)
}

# Compute covariance matrix
compute_covariance_matrix <- function(log_returns_matrix) {
  return(cov(log_returns_matrix))
}

# Process Yield Rates (5 maturities)
yield_rates <- bonds %>% select(all_of(closing_price_columns)) %>% slice(1:5)
yield_log_returns <- compute_log_returns(as.matrix(yield_rates))
yield_cov_matrix <- compute_covariance_matrix(yield_log_returns)

# Process Forward Rates (1yr-1yr to 1yr-4yr)
forward_rates <- bonds %>% select(all_of(closing_price_columns)) %>% slice(6:9)
forward_log_returns <- compute_log_returns(as.matrix(forward_rates))
forward_cov_matrix <- compute_covariance_matrix(forward_log_returns)

# Print results
print("Covariance Matrix for Yield Log-Returns:")
print(yield_cov_matrix)

print("Covariance Matrix for Forward Rate Log-Returns:")
print(forward_cov_matrix)