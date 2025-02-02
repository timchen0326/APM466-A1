
library(ggplot2)
library(dplyr)
library(readr)

bonds <- read.csv("filtered_data/filtered_bonds.csv")


bonds$Maturity.Date <- as.Date(bonds$Maturity.Date, format="%m/%d/%Y")
bonds$Issue.Date <- as.Date(bonds$Issue.Date, format="%m/%d/%Y")
bonds$Time_to_Maturity <- as.numeric(difftime(bonds$Maturity.Date, Sys.Date(), units="days")) / 365


bonds <- bonds %>% arrange(Time_to_Maturity)

bonds$Coupon <- as.numeric(sub("%", "", bonds$Coupon)) / 100  


closing_price_columns <- grep("^X2025", colnames(bonds), value = TRUE)

compute_spot_rates <- function(bond_data, closing_prices) {
  spot_curve_list <- list()
  
  for (date_col in closing_prices) {
    bond_data$Close_Price <- bond_data[[date_col]]
    spot_rates <- numeric(nrow(bond_data))
    
    for (i in 1:nrow(bond_data)) {
      bond <- bond_data[i, ]
      P <- bond$Close_Price 
      C <- bond$Coupon / 2 * 100 
      F <- 100 
      T <- bond$Time_to_Maturity
      
      if (i == 1) {
        spot_rates[i] <- -log(P / F) / T
      } else {
        cashflows <- sum(C / (1 + spot_rates[1:(i-1)] / 2)^(1:(i-1) * 2))
        spot_rates[i] <- ((F + C) / (P - cashflows))^(1 / (T * 2)) - 1
      }
    }
    
    spot_curve_list[[date_col]] <- data.frame(Maturity = bond_data$Maturity.Date, Spot_Rate = spot_rates * 100, Date = as.Date(sub("X", "", date_col), format="%Y.%m.%d"))
  }
  
  return(do.call(rbind, spot_curve_list))
}


spot_curve_all <- compute_spot_rates(bonds, closing_price_columns)


ggplot(spot_curve_all, aes(x = Maturity, y = Spot_Rate, color = as.factor(Date))) +
  geom_line(size = 1, alpha = 0.7) +
  geom_point(size = 2) +
  scale_x_date(breaks = unique(spot_curve_all$Maturity), date_labels = "%m/%d/%Y") + 
  labs(title = "5-Year Spot Curve Across Days", x = "Maturity Date", y = "Spot Rate (%)", color = "Date") +
  theme_minimal()

ggsave("spot_curve.png", width = 10, height = 6, dpi = 300)
