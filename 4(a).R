library(ggplot2)
library(dplyr)
library(lubridate)
library(rootSolve)

bonds <- read.csv("modified.csv", stringsAsFactors = FALSE)

bonds$Coupon <- as.numeric(sub("%", "", bonds$Coupon)) / 100
bonds$Maturity.Date <- as.Date(bonds$Maturity.Date, format="%Y-%m-%d")

settlement_date <- as.Date("2025-01-06")

accrued_interest <- function(issue_date, maturity_date, coupon_rate, settlement_date) {
  coupon_period <- 6
  last_coupon_date <- seq(maturity_date, by = "-6 months", length.out = ceiling(as.numeric(difftime(maturity_date, issue_date, units="days")) / 180))
  last_coupon_date <- max(last_coupon_date[last_coupon_date <= settlement_date])
  
  n_days <- as.numeric(difftime(settlement_date, last_coupon_date, units="days"))
  accrued <- (n_days / 365) * (coupon_rate * 100)
  return(accrued)
}

compute_ytm <- function(dirty_price, face_value, coupon, maturity, settlement) {
  time_to_maturity <- as.numeric(difftime(maturity, settlement, units="days")) / 365
  
  bond_price_eq <- function(r) {
    sum_coupon <- sum(coupon * face_value * exp(-r * (1:floor(2 * time_to_maturity) / 2)))
    price <- sum_coupon + face_value * exp(-r * time_to_maturity)
    return(price - dirty_price)
  }
  
  ytm_solution <- tryCatch({
    uniroot(bond_price_eq, c(-0.05, 0.5))$root
  }, error = function(e) NA)
  
  return(ytm_solution)
}

ytm_results <- data.frame()
debug_data <- data.frame()

for (date_col in names(bonds)[6:15]) {
  settlement_date <- as.Date(sub("X", "", date_col), format="%Y.%m.%d")
  
  for (i in 1:nrow(bonds)) {
    clean_price <- as.numeric(bonds[i, date_col])
    accrued <- accrued_interest(bonds$Issue.Date[i], bonds$Maturity.Date[i], bonds$Coupon[i], settlement_date)
    dirty_price <- clean_price + accrued
    ytm <- compute_ytm(dirty_price, 100, bonds$Coupon[i] / 2, bonds$Maturity.Date[i], settlement_date)
    
    if (is.na(ytm) || ytm > 0.2) {
      print(paste("⚠️ Potential Issue: Bond", bonds$ISIN[i], "on", settlement_date,
                  "| Clean Price:", clean_price, "| Accrued:", accrued,
                  "| Dirty Price:", dirty_price, "| YTM:", ytm))
    }
    
    ytm_results <- rbind(ytm_results, data.frame(Date=settlement_date, ISIN=bonds$ISIN[i], Maturity=bonds$Maturity.Date[i], YTM=ytm))
    debug_data <- rbind(debug_data, data.frame(Date=settlement_date, ISIN=bonds$ISIN[i], CleanPrice=clean_price, Accrued=accrued, DirtyPrice=dirty_price, YTM=ytm))
  }
}

print(debug_data)

ggplot(ytm_results, aes(x=Maturity, y=YTM, color=factor(Date))) +
  geom_line() +
  geom_point() +
  scale_x_date(breaks = as.Date(c("2025-03-01", "2025-09-01", 
                                  "2026-03-01", "2026-09-01", 
                                  "2027-03-01", "2027-09-01", 
                                  "2028-03-01", "2028-09-01", 
                                  "2029-03-01", "2029-09-01")), 
               date_labels = "%b %Y") +
  labs(title="5-Year Yield Curve (YTM)", 
       x="Maturity Date", 
       y="Yield-to-Maturity (Continuous Compounding)", 
       color="Date") +
  theme_minimal()

ggsave("yield_curve.png", width = 10, height = 6, dpi = 300)
