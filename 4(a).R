# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readr)

# Install and load necessary package
if (!require(rootSolve)) install.packages("rootSolve", dependencies=TRUE)
library(rootSolve)  # For numerical root finding

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

# Function to compute YTM iteratively
ytm_function <- function(y, P, C, F, N) {
  return(P - sum(C / (1 + y/2)^(1:N)) - F / (1 + y/2)^N)
}

# Compute YTM for Each Bond and Each Day
compute_ytm <- function(bond_data, closing_prices) {
  ytm_curve_list <- list()
  
  for (date_col in closing_prices) {
    bond_data$Close_Price <- bond_data[[date_col]]
    ytm_values <- numeric(nrow(bond_data))
    
    for (i in 1:nrow(bond_data)) {
      bond <- bond_data[i, ]
      P <- bond$Close_Price  # Closing price of bond
      C <- bond$Coupon / 2 * 100  # Semi-annual coupon payment
      F <- 100  # Assume face value is 100
      N <- bond$Time_to_Maturity * 2  # Convert to semi-annual periods
      
      # Solve for YTM using numerical root-finding
      ytm_values[i] <- uniroot(ytm_function, c(0, 1), P = P, C = C, F = F, N = N)$root * 100  # Convert to %
    }
    
    ytm_curve_list[[date_col]] <- data.frame(Maturity = bond_data$Maturity.Date, YTM = ytm_values, Date = as.Date(sub("X", "", date_col), format="%Y.%m.%d"))
  }
  
  return(do.call(rbind, ytm_curve_list))
}

# Compute YTM for each date
ytms_all <- compute_ytm(bonds, closing_price_columns)

# Save the YTM table for further use
write.csv(ytms_all, "ytm_results.csv", row.names = FALSE)

# Plot the YTM Curve
ggplot(ytms_all, aes(x = Maturity, y = YTM, color = as.factor(Date))) +
  geom_line(size = 1, alpha = 0.7) +
  geom_point(size = 2) +
  scale_x_date(breaks = unique(ytms_all$Maturity), date_labels = "%m/%d/%Y") +  # Show all Maturity Dates
  labs(title = "5-Year Yield Curve Across Days", x = "Maturity Date", y = "Yield to Maturity (%)", color = "Date") +
  theme_minimal()

ggsave("yield_curve.png", width = 10, height = 6, dpi = 300)