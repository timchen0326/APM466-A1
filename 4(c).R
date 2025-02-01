# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readr)

# Load bond data
bonds <- read.csv("filtered_data/filtered_bonds.csv")

# Convert dates and compute time to maturity
bonds$Maturity.Date <- as.Date(bonds$Maturity.Date, format="%m/%d/%Y")
bonds$Issue.Date <- as.Date(bonds$Issue.Date, format="%m/%d/%Y")
bonds$Time_to_Maturity <- as.numeric(difftime(bonds$Maturity.Date, Sys.Date(), units="days")) / 365

# Sort bonds by maturity
bonds <- bonds %>% arrange(Time_to_Maturity)

# Convert Coupon to numeric format
bonds$Coupon <- as.numeric(sub("%", "", bonds$Coupon)) / 100  # Convert percentage to decimal

# Extract closing price columns (dates as variables)
closing_price_columns <- grep("^X2025", colnames(bonds), value = TRUE)

# Compute 1-year forward rates from spot rates
compute_forward_rates <- function(bond_data, closing_prices) {
  forward_curve_list <- list()
  
  for (date_col in closing_prices) {
    bond_data$Close_Price <- bond_data[[date_col]]
    spot_rates <- numeric(nrow(bond_data))
    
    # Compute spot rates using bootstrapping
    for (i in 1:nrow(bond_data)) {
      bond <- bond_data[i, ]
      P <- bond$Close_Price
      C <- bond$Coupon / 2 * 100  # Semi-annual coupon payment
      F <- 100  # Face value
      T <- bond$Time_to_Maturity
      
      if (i == 1) {
        # Zero-coupon bond formula for 1-year bond
        spot_rates[i] <- -log(P / F) / T
      } else {
        # Solve for spot rate iteratively
        cashflows <- sum(C / (1 + spot_rates[1:(i-1)] / 2)^(1:(i-1) * 2))
        spot_rates[i] <- ((F + C) / (P - cashflows))^(1 / (T * 2)) - 1
      }
    }
    
    # Compute forward rates
    forward_rates <- c(
      ((1 + spot_rates[2]/2)^(2*2) / (1 + spot_rates[1]/2)^(2*1))^(1/2) - 1,
      ((1 + spot_rates[3]/2)^(2*3) / (1 + spot_rates[1]/2)^(2*1))^(1/3) - 1,
      ((1 + spot_rates[4]/2)^(2*4) / (1 + spot_rates[1]/2)^(2*1))^(1/4) - 1,
      ((1 + spot_rates[5]/2)^(2*5) / (1 + spot_rates[1]/2)^(2*1))^(1/5) - 1
    )
    
    forward_curve_list[[date_col]] <- data.frame(Forward_Term = c("1yr-1yr", "1yr-2yr", "1yr-3yr", "1yr-4yr"),
                                                 Forward_Rate = forward_rates * 100,
                                                 Date = as.Date(sub("X", "", date_col), format="%Y.%m.%d"))
  }
  
  return(do.call(rbind, forward_curve_list))
}

# Compute forward rates for each date
forward_curve_all <- compute_forward_rates(bonds, closing_price_columns)

# Plot the forward curve for each date
ggplot(forward_curve_all, aes(x = Forward_Term, y = Forward_Rate, color = as.factor(Date), group = Date)) +
  geom_line(size = 1, alpha = 0.7) +
  geom_point(size = 2) +
  labs(title = "1-Year Forward Curve Across Days", x = "Forward Term", y = "Forward Rate (%)", color = "Date") +
  theme_minimal()



ggsave("forward_curve.png", width = 10, height = 6, dpi = 300)