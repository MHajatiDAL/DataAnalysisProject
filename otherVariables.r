# Load necessary libraries
library(dplyr)
library(ggplot2)
library(broom)
library(corrplot)

# Load cleaned data
df_clean <- read_csv("Data/cleaned_data.csv")

# Step 1: Check available predictors
print(colnames(df_clean))

# Step 2: Select additional predictors (based on dataset availability)
df_regression <- df_clean %>%
  select(Unemployment_Male, Unemployment_Female, Govt_Expenditure_Education,
         GDP, Inflation, Employment_to_Population_Ratio, R&D_Expenditure, High_Tech_Exports)

# Step 3: Check correlations between predictors
cor_matrix <- cor(df_regression, use = "complete.obs")
corrplot(cor_matrix, method = "color", tl.col = "black", tl.srt = 45)

# Step 4: Fit Multiple Linear Regression Models
# Model 1: Male Unemployment Prediction
model_male_multi <- lm(Unemployment_Male ~ ., data = df_regression)
summary(model_male_multi)

# Model 2: Female Unemployment Prediction
model_female_multi <- lm(Unemployment_Female ~ ., data = df_regression)
summary(model_female_multi)

# Step 5: Save Regression Summaries
sink("Multiple_Regression_Results.txt")
print("Male Unemployment Model:")
print(summary(model_male_multi))
print("Female Unemployment Model:")
print(summary(model_female_multi))
sink()

# Step 6: Residual Analysis & Diagnostic Plots
# Male Model Residual Plot
ggplot(model_male_multi, aes(.fitted, .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype="dashed") +
  ggtitle("Residual Plot: Male Unemployment Model") +
  xlab("Fitted Values") + ylab("Residuals")
ggsave("Residuals_Male_Multi.png")

# Female Model Residual Plot
ggplot(model_female_multi, aes(.fitted, .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype="dashed") +
  ggtitle("Residual Plot: Female Unemployment Model") +
  xlab("Fitted Values") + ylab("Residuals")
ggsave("Residuals_Female_Multi.png")

# Step 7: Model Comparison
anova_male <- anova(model_male_multi)
anova_female <- anova(model_female_multi)

sink("Multiple_Regression_ANOVA.txt")
print("ANOVA Results for Male Unemployment Model:")
print(anova_male)
print("ANOVA Results for Female Unemployment Model:")
print(anova_female)
sink()
