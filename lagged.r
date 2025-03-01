# Load necessary libraries
library(dplyr)
library(ggplot2)
library(broom)

# Load cleaned data
df_clean <- read_csv("Data/cleaned_data.csv")

# Convert Year column to numeric
df_clean$Year <- as.numeric(df_clean$Year)

# Step 1: Create a 10-Year Lag in the Data
df_lagged <- df_clean %>%
  arrange('Country Code', Year) %>%
  group_by('Country Code') %>%
  mutate(Govt_Expenditure_Education_Lagged = lag(Govt_Expenditure_Education, n = 10)) %>%
  ungroup() %>%
  drop_na()  # Remove rows where lagged values are NA

# Step 2: Fit Time-Based Regression Models
# Model 1: Male Unemployment ~ Lagged Education Expenditure
model_male_lagged <- lm(Unemployment_Male ~ Govt_Expenditure_Education_Lagged, data = df_lagged)
summary(model_male_lagged)  # Print summary

# Model 2: Female Unemployment ~ Lagged Education Expenditure
model_female_lagged <- lm(Unemployment_Female ~ Govt_Expenditure_Education_Lagged, data = df_lagged)
summary(model_female_lagged)  # Print summary

# Save regression results
sink("Data/Lagged_Regression_Results.txt")
print("Male Unemployment Model (10-Year Lag):")
print(summary(model_male_lagged))
print("Data/Female Unemployment Model (10-Year Lag):")
print(summary(model_female_lagged))
sink()

# Step 3: Visualizing the Lagged Regression Models
ggplot(df_lagged, aes(x = Govt_Expenditure_Education_Lagged, y = Unemployment_Male)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  ggtitle("10-Year Lagged Regression: Gov Expenditure vs Male Unemployment") +
  xlab("Gov Expenditure on Education (t-10)") + ylab("Male Unemployment Rate (t)")
ggsave("Plots/Lagged_Regression_Male_Unemployment.png")

ggplot(df_lagged, aes(x = Govt_Expenditure_Education_Lagged, y = Unemployment_Female)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  ggtitle("10-Year Lagged Regression: Gov Expenditure vs Female Unemployment") +
  xlab("Gov Expenditure on Education (t-10)") + ylab("Female Unemployment Rate (t)")
ggsave("Plots/Lagged_Regression_Female_Unemployment.png")
