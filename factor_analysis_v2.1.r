# ---------------------------------------------
# Final Analysis: Cancer Survivorship & Demographics
# ---------------------------------------------
# Author: Auto-generated (Updated for Boah Kim)
# Description:
#   - Load, clean, and merge datasets
#   - Create derived variables
#   - Perform KMO test and factor analysis
#   - Fit regression on factor scores with residual diagnostics

# Load necessary libraries
library(tidyverse)  # for data wrangling
library(psych)      # for KMO test and factor analysis
library(caret)      # for train/test split

# -----------------------------
# 1. Load CSV files
# -----------------------------
edu <- read.csv("3. Education_by_County.csv")
income <- read.csv("4. Income_by_County.csv")
population <- read.csv("5. County_Population_by_Characteristics.csv")

# -----------------------------
# 2. Clean and process datasets
# -----------------------------

# --- Education Data ---
# Select relevant columns and rename
edu <- edu[3:nrow(edu), c("GEO_ID", "NAME", "S1501_C01_002E", "S1501_C01_003E", "S1501_C01_004E", "S1501_C01_005E")]
colnames(edu) <- c("GEO_ID", "County", "Less_HS", "HS_Grad", "Some_College", "Bachelor_Plus")
# Simplify county names
edu$County <- gsub(" County, New York", "", edu$County)
# Convert percentage strings to numeric
edu <- edu %>% mutate(across(Less_HS:Bachelor_Plus, as.numeric))

# --- Income Data ---
# Select only median income
income <- income[3:nrow(income), c("GEO_ID", "NAME", "S1903_C02_001E")]
colnames(income) <- c("GEO_ID", "County", "Median_Income")
# Simplify county name
income$County <- trimws(gsub(" County.*", "", income$County))
# Convert to numeric and summarize duplicates (e.g. multiple years)
income$Median_Income <- as.numeric(income$Median_Income)
income <- income %>% group_by(County) %>% summarize(Median_Income = mean(Median_Income, na.rm=TRUE))

# --- Population Data ---
# Keep latest year per county, and average population stats
pop <- population %>% group_by(CTYNAME) %>%
  filter(YEAR == max(YEAR)) %>%
  summarize(
    Total_Population = mean(POPESTIMATE, na.rm=TRUE),
    Male_Population = mean(POPEST_MALE, na.rm=TRUE),
    Female_Population = mean(POPEST_FEM, na.rm=TRUE),
    Median_Age = mean(MEDIAN_AGE_TOT, na.rm=TRUE)
  )
# Clean county names
pop$County <- gsub(" County", "", pop$CTYNAME)
# Create gender ratio as a new feature
pop$Gender_Ratio <- pop$Male_Population / pop$Female_Population

# -----------------------------
# 3. Merge datasets and create features
# -----------------------------
# Combine education, income, and population into one table by County
merged <- reduce(list(edu, income, pop), full_join, by = "County")

# Create:
# 1) Edu_Index: weighted average of educational attainment
# 2) Income_per_Capita: median income divided by population size
merged <- merged %>%
  mutate(
    Edu_Index = 1*Less_HS + 2*HS_Grad + 3*Some_College + 4*Bachelor_Plus,
    Income_per_Capita = Median_Income / Total_Population
  )

# -----------------------------
# 4. Prepare data for analysis
# -----------------------------
# Select numeric variables for factor analysis and regression
data <- merged %>%
  select(Edu_Index, Income_per_Capita, Median_Age, Gender_Ratio, Total_Population) %>%
  na.omit()  # Remove rows with missing values

# -----------------------------
# 5. KMO test for factor analysis suitability
# -----------------------------
kmo_result <- KMO(cor(data[,1:4]))  # Check whether factor analysis is appropriate
print("KMO Test Result:")
print(kmo_result)

# -----------------------------
# 6. Factor analysis with varimax rotation
# -----------------------------
# Reduce 4 variables to 2 latent factors
fa_result <- fa(r = cor(data[,1:4]), nfactors = 2, rotate = "varimax")
print("Factor Analysis Result:")
print(fa_result)

# Extract factor scores
fa_scores <- as.data.frame(fa_result$scores)
colnames(fa_scores) <- c("Factor1", "Factor2")
# Keep Total_Population as dependent variable
fa_scores$Total_Population <- data$Total_Population




# I used the total population column in our dataset. However, I think we need to confirm if this data is the incidence population.
# -----------------------------
# 7. Train/test split for regression
# -----------------------------
# 80% training, 20% test
#set.seed(42)
#train_indices <- createDataPartition(fa_scores$Total_Population, p = 0.8, list = FALSE)
#train_data <- fa_scores[train_indices, ]
#test_data <- fa_scores[-train_indices, ]

# -----------------------------
# 8. Fit linear regression model
# -----------------------------
# Model cancer survivor count using latent factors
#lm_model <- lm(Total_Population ~ Factor1 + Factor2, data = train_data)
#summary(lm_model)

# -----------------------------
# 9. Evaluate model with residual plots
# -----------------------------
# Predict on test set
#test_data$predicted <- predict(lm_model, newdata = test_data)
#test_data$residuals <- test_data$Total_Population - test_data$predicted

# Plot residuals vs predicted values
#plot(test_data$predicted, test_data$residuals,
#     main = "Residual Plot",
#     xlab = "Predicted Total Population",
#     ylab = "Residuals")
#abline(h = 0, col = "red")

# Histogram of residuals
#hist(test_data$residuals,
#     main = "Histogram of Residuals",
#     xlab = "Residuals",
#     col = "lightblue")
