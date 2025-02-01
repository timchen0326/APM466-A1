# Load necessary libraries
library(dplyr)

# Example ISIN values to filter
isin_values <- c("CA135087Q491", "CA135087P576", "CA135087R895", 
                 "CA135087Q988", "CA135087K528", "CA135087K940", 
                 "CA135087N837", "CA135087L518", "CA135087L930", 
                 "CA135087M847")

# Load your dataset (replace 'apm466_data.csv' with your actual data file path)
apm466_data <- read.csv("data/APM466 Data.csv")

# Filter the dataset to get the 10 bonds
filtered_bonds <- apm466_data %>%
  filter(ISIN %in% isin_values)

# Print the filtered data
print(filtered_bonds)

# Save the filtered bonds to a new CSV (optional)
write.csv(filtered_bonds, "filtered_data/filtered_bonds.csv", row.names = FALSE)
