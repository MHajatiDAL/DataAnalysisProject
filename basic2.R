# Load necessary libraries
library(dplyr)
library(readr)
library(tidyr)
library(corrplot)

# Load datasets
dimension_country <- read_csv("Data/dimension_country.csv")
dimension_indicator <- read_csv("Data/dimension_indicator.csv")
facttable <- read_csv("Data/facttable.csv")

# Merge facttable with indicator names
merged_data <- facttable %>%
  left_join(dimension_indicator, by = "Indicator Code")

# Aggregate data: Compute the average value per indicator per country
aggregated_data <- merged_data %>%
  select(`Country Code`, `Indicator Name`, `2000`:`2021`) %>%
  pivot_longer(cols = `2000`:`2021`, names_to = "Year", values_to = "Value") %>%
  group_by(`Country Code`, `Indicator Name`) %>%
  summarise(Average_Value = mean(Value, na.rm = TRUE), .groups = "drop")

# Reshape data: Pivot so indicators become columns without years
reshaped_data <- aggregated_data %>%
  pivot_wider(names_from = `Indicator Name`, values_from = Average_Value)

# Remove non-numeric columns
numeric_data <- reshaped_data %>% select(-`Country Code`)

# Ensure only indicators are present
numeric_data <- numeric_data %>% select(where(is.numeric))

# Compute correlation matrix
cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs")

# Print correlation matrix
print(cor_matrix)

# Generate a correlation matrix diagram
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.8, tl.col = "black", addCoef.col = "black")
