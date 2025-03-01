# Load necessary libraries
library(dplyr)      # Data manipulation
library(ggplot2)    # Visualization
library(corrplot)   # Correlation analysis
library(tidyr)      # Data wrangling
library(readr)      # Reading CSV files

# Step 1: Load the datasets
df_country <- read_csv("Data/dimension_country.csv")     # Country codes and names
df_indicator <- read_csv("Data/dimension_indicator.csv") # Indicator codes and names
df_facttable <- read_csv("Data/facttable.csv")           # Actual indicator data

# Step 2: Identify Relevant Indicators
education_code <- df_indicator %>%
  filter(grepl("Government expenditure on education", `Indicator Name`, ignore.case = TRUE)) %>%
  pull(`Indicator Code`) %>% as.character()

unemployment_code_male <- df_indicator %>%
  filter(grepl("Unemployment, male", `Indicator Name`, ignore.case = TRUE)) %>%
  pull(`Indicator Code`) %>% as.character()

unemployment_code_female <- df_indicator %>%
  filter(grepl("Unemployment, female", `Indicator Name`, ignore.case = TRUE)) %>%
  pull(`Indicator Code`) %>% as.character()

# Step 3: Extract Relevant Data from Fact Table
df_facttable$`Indicator Code` <- as.character(df_facttable$`Indicator Code`)
df_facttable$`Indicator Code` <- trimws(df_facttable$`Indicator Code`)

df_selected <- df_facttable %>%
  filter(`Indicator Code` %in% c(education_code, unemployment_code_male, unemployment_code_female))

# Merge with Country Names
df_selected <- df_selected %>%
  left_join(df_country, by = "Country Code") %>%
  select(`Country Name`, `Country Code`, `Indicator Code`, everything())

# Step 4: Pivot Data to Long Format for Years
df_long <- df_selected %>%
  pivot_longer(cols = starts_with("20"), names_to = "Year", values_to = "Value") %>%
  pivot_wider(names_from = `Indicator Code`, values_from = Value)

# Step 5: Rename Columns for Clarity
colnames(df_long)[colnames(df_long) == education_code] <- "Govt_Expenditure_Education"
colnames(df_long)[colnames(df_long) == unemployment_code_male] <- "Unemployment_Male"
colnames(df_long)[colnames(df_long) == unemployment_code_female] <- "Unemployment_Female"

# Convert Year to Numeric
df_long$Year <- as.numeric(df_long$Year)

# Step 6: Handle Missing Values
df_clean <- df_long %>%
  drop_na()  # Remove rows with missing values

# Step 7: Normalizing Data
df_clean <- df_clean %>%
  mutate(across(c(Govt_Expenditure_Education, Unemployment_Male, Unemployment_Female),
                ~ (.-min(.))/(max(.)-min(.))))  # Normalize between 0 and 1

# Step 8: Summary Statistics
summary(df_clean)

# Step 9: Outlier Detection using Boxplots
ggplot(df_clean, aes(x = "", y = Govt_Expenditure_Education)) +
  geom_boxplot() +
  ggtitle("Boxplot for Government Expenditure on Education")
ggsave("Plots/Boxplot Gov Expenditure on Education.png")

ggplot(df_clean, aes(x = "", y = Unemployment_Male)) +
  geom_boxplot() +
  ggtitle("Boxplot for Male Unemployment Rate")
ggsave("Plots/Boxplot Male Unemployment Rate.png")

ggplot(df_clean, aes(x = "", y = Unemployment_Female)) +
  geom_boxplot() +
  ggtitle("Boxplot for Female Unemployment Rate")
ggsave("Plots/Boxplot Female Unemployment Rate.png")

# Step 10: Correlation Analysis
cor_matrix <- cor(df_clean %>% select(Govt_Expenditure_Education, Unemployment_Male, Unemployment_Female))
corrplot(cor_matrix, method = "color", tl.col = "black", tl.srt = 45)

# Step 11: Scatterplot Visualization
ggplot(df_clean, aes(x = Govt_Expenditure_Education, y = Unemployment_Male)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  ggtitle("Government Education Expenditure vs Male Unemployment")
ggsave("Plots/Boxplot Gov EXP vs Male Unemployment.png")

ggplot(df_clean, aes(x = Govt_Expenditure_Education, y = Unemployment_Female)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  ggtitle("Government Education Expenditure vs Female Unemployment")
ggsave("Plots/Boxplot Gov EXP vs Female Unemployment.png")

# Step 12: Save Cleaned Data for Further Analysis
write_csv(df_clean, "Data/cleaned_data.csv")
