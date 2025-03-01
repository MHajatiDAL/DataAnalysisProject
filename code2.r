# Load necessary libraries
library(ggplot2)
library(dplyr)
library(broom)  # For model summary
library(ggpubr)  # For diagnostic plots

# Step 1: Load Cleaned Data
df_clean <- read_csv("Data/cleaned_data.csv")

# Step 2: Correlation Analysis
cor_matrix <- cor(df_clean %>% select(Govt_Expenditure_Education, Unemployment_Male, Unemployment_Female))
print(cor_matrix)  # Print correlation values

# Step 3: Linear Regression Models
# Model 1: Government Expenditure on Education vs Male Unemployment
model_male <- lm(Unemployment_Male ~ Govt_Expenditure_Education, data = df_clean)
summary(model_male)  # Print model summary

# Model 2: Government Expenditure on Education vs Female Unemployment
model_female <- lm(Unemployment_Female ~ Govt_Expenditure_Education, data = df_clean)
summary(model_female)  # Print model summary

# Step 4: Save Regression Summaries to a File
sink("Data/Regression_Results.txt")
print("Male Unemployment Model:")
print(summary(model_male))
print("Female Unemployment Model:")
print(summary(model_female))
sink()  # Stop saving output

# Step 5: Diagnostic Plots for Regression Models
# Residual Plot for Male Unemployment Model
ggplot(model_male, aes(.fitted, .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype="dashed") +
  ggtitle("Residual Plot: Male Unemployment Model") +
  xlab("Fitted Values") + ylab("Residuals")
ggsave("Plots/Residuals_Male_Unemployment.png")

# Residual Plot for Female Unemployment Model
ggplot(model_female, aes(.fitted, .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype="dashed") +
  ggtitle("Residual Plot: Female Unemployment Model") +
  xlab("Fitted Values") + ylab("Residuals")
ggsave("Plots/Residuals_Female_Unemployment.png")

# Step 6: ANOVA Test to Check Model Significance
anova_male <- anova(model_male)
anova_female <- anova(model_female)

# Save ANOVA Results
sink("Data/ANOVA_Results.txt")
print("ANOVA Results for Male Unemployment Model:")
print(anova_male)
print("ANOVA Results for Female Unemployment Model:")
print(anova_female)
sink()  # Stop saving output

# Step 7: Visualization of Regression Models
ggplot(df_clean, aes(x = Govt_Expenditure_Education, y = Unemployment_Male)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  ggtitle("Regression Model: Gov Expenditure vs Male Unemployment") +
  xlab("Gov Expenditure on Education") + ylab("Male Unemployment Rate")
ggsave("Plots/Regression_Male_Unemployment.png")

ggplot(df_clean, aes(x = Govt_Expenditure_Education, y = Unemployment_Female)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  ggtitle("Regression Model: Gov Expenditure vs Female Unemployment") +
  xlab("Gov Expenditure on Education") + ylab("Female Unemployment Rate")
ggsave("Plots/Regression_Female_Unemployment.png")
