# Load necessary libraries
library(dplyr)
library(readr)
library(tidyr)
library(corrplot)
library(MASS)  # Added for stepwise regression

# Load datasets
dimension_country <- read_csv("Data/dimension_country.csv")
dimension_indicator <- read_csv("Data/dimension_indicator.csv")
facttable <- read_csv("Data/facttable.csv")

# Merge facttable with indicator names
merged_data <- facttable %>%
  left_join(dimension_indicator, by = "Indicator Code")

# Convert all value columns to numeric (handle non-numeric data)
merged_data <- merged_data %>%
  mutate(across(`2000`:`2021`, as.numeric, .names = "clean_{.col}"))

# Reshape data: Convert from wide to long format
fact_long <- merged_data %>%
  pivot_longer(cols = `2000`:`2021`, names_to = "Year", values_to = "Value")

# Aggregate: Compute the average value per indicator per country
fact_avg <- fact_long %>%
  group_by(`Country Code`, `Indicator Name`) %>%
  summarise(Average_Value = mean(Value, na.rm = TRUE), .groups = "drop")

# Reshape data: Convert back to wide format where indicators are columns
fact_pivot <- fact_avg %>%
  pivot_wider(names_from = `Indicator Name`, values_from = Average_Value)

# Standardize column names to remove spaces/special characters
colnames(fact_pivot) <- make.names(colnames(fact_pivot))

# Remove non-numeric columns
numeric_data <- fact_pivot %>% select(-one_of("Country.Code"))

# Compute correlation matrix
cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs")

# Print correlation matrix
print(cor_matrix)

# Generate a properly formatted correlation matrix diagram
par(mar = c(20, 20, 20, 20), pin = c(30, 30))  # Increase box size a little more
corrplot(cor_matrix, method = "color", type = "full",
         tl.cex = 0.2, tl.col = "black", tl.srt = 45, addCoef.col = "black",
         number.cex = 0.2, col = colorRampPalette(c("blue", "white", "red"))(200),
         sig.level = 0.05, insig = "blank", number.digits = 2,
         addgrid.col = "black", rect.col = "black", border = "black")

# -----------------------------------
# Stepwise AIC Regression - Fixes Applied
# -----------------------------------

# Ensure numeric_data has no missing values
numeric_data <- numeric_data[, colSums(is.na(numeric_data)) < nrow(numeric_data)]

# Remove columns with zero variance
numeric_data <- numeric_data[, sapply(numeric_data, function(x) var(x, na.rm = TRUE)) > 0]

# Ensure numeric_data is not empty after filtering
if (ncol(numeric_data) <= 1) {
  stop("Insufficient numeric variables for regression after preprocessing.")
}

# Ensure sufficient data points
if (ncol(numeric_data) > nrow(numeric_data)) {
  numeric_data <- numeric_data[, 1:min(ncol(numeric_data), nrow(numeric_data) - 1)]
}

# Select a dependent variable (First column, but can be changed manually)
dependent_var <- paste0("`", colnames(numeric_data)[1], "`")
independent_vars <- paste0("`", colnames(numeric_data)[-1], "`")

# Create the regression formula
formula <- as.formula(paste(dependent_var, "~", paste(independent_vars, collapse = " + ")))

# Fit the initial full model
full_model <- lm(formula, data = numeric_data)

# Check if AIC is finite before applying stepwise regression
if (!is.finite(AIC(full_model))) {
  stop("AIC cannot be calculated. Check your data for multicollinearity or missing values.")
}

# Perform stepwise regression using AIC
stepwise_model <- stepAIC(full_model, direction = "both", trace = TRUE)

# Print the final model summary
summary(stepwise_model)
