# Import libraries
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(viridis)
library(psych)

# Read data
df_county <- read.csv('Data/GeoID_County_Mapping.csv')
df_cancer <- read.csv('Data/NYSDOH_CancerMapping_Data.csv')
df_pop <- read.csv('Data/County_Population_by_Characteristics.csv')
df_edu <- read.csv('Data/Education_by_County.csv')
df_income <- read.csv('Data/Income_by_County.csv')

# Basic data exploration for datasets
str(df_county)
str(df_cancer)
str(df_pop)
str(df_edu)
str(df_income)

# Count NA values in each dataframe, per variable
na_counts_per_col <- list(
  df_county = colSums(is.na(df_county)),
  df_cancer = colSums(is.na(df_cancer)),
  df_pop = colSums(is.na(df_pop)),
  df_edu = colSums(is.na(df_edu)),
  df_income = colSums(is.na(df_income))
)

print(na_counts_per_col)
# basic data exploration for datasets
str(df_county)
str(df_cancer)
str(df_pop)
str(df_edu)
str(df_income)

# Count NA values in each dataframe, per variable
na_counts_per_col <- list(
  df_county = colSums(is.na(df_county)),
  df_cancer = colSums(is.na(df_cancer)),
  df_pop = colSums(is.na(df_pop)),
  df_edu = colSums(is.na(df_edu)),
  df_income = colSums(is.na(df_income))
  )

print(na_counts_per_col)
# We identify there were several N/A values, but it is in variables that
# we will use. Therefore we choose to delete and ignore them

# Clean cancer dataset:
#   extract first 5 numbers which represents county 
#   aggregate cancer data on county level
df_cancer$geoid_county <- as.numeric(substr(df_cancer$Dohregion, 1, 5))

df_cancer <- df_cancer %>% 
  select(grep("observed_", names(df_cancer)),
         grep("expected_", names(df_cancer)),
         geoid_county
  ) %>% 
  group_by(geoid_county) %>% 
  summarise(across(everything(), sum))

# Clean population dataset:
#   keep columns of interest
#   remove the string county, filter to latest year
colnames(df_pop)[colnames(df_pop) == "COUNTY"] <- "County_ID"
colnames(df_pop)[colnames(df_pop) == "CTYNAME"] <- "County"

df_pop <- df_pop %>%
  mutate(County = str_remove(County, " County")) %>% 
  filter(YEAR == 12) %>% 
  select(-c(SUMLEV, STATE, County_ID, STNAME, YEAR))

# Clean county dataset:
#   get distinct geo_id and county names
colnames(df_county)[colnames(df_county) == "GEOID.For.County"] <- "geoid_county"

df_county <- df_county %>% 
  select(geoid_county, County) %>% 
  distinct()

# Clean education dataset:
#   format text and columns
#   perform aggregation on county level
colnames(df_edu) <- as.character(df_edu[1,])

df_edu <- df_edu %>%
  slice(-c(1, 2)) %>%
  separate(`Geographic Area Name`, into = "County", sep =",") %>%
  mutate(County = str_remove(County, " County"),
         across(.cols = -c(1, 2), .fns = as.numeric)) %>%
  group_by(County) %>%
  summarize(across(!c(Geography))) %>% 
  select(-2, -3)

# Clean income dataset:
#   tidy columns and text format
#   perform aggregation on county level
colnames(df_income) <- as.character(df_income[1,])

df_income <- df_income %>%
  slice(-c(1, 2)) %>%
  separate(`Geographic Area Name`, into = c("City", "County", "State"), sep =",") %>%
  mutate(County = str_remove(County, " County")) %>%
  filter(Geography != "Geography") %>%
  mutate(`Total!!Estimate!!Households` = as.numeric(`Total!!Estimate!!Households`),
         `Median income (dollars)!!Estimate!!Households` = as.numeric(`Median income (dollars)!!Estimate!!Households`)) %>%
  mutate_if(is.character, str_trim) %>% 
  group_by(County) %>%
  summarise(
    total_households = sum(`Total!!Estimate!!Households`, na.rm = TRUE),
    median_income = mean(`Median income (dollars)!!Estimate!!Households`, na.rm = TRUE)
  )

# Join datasets to get cancer, county, population, income, and education level
df_cancer <- merge(x = df_cancer, y = df_county, by = "geoid_county", all = TRUE)
df_cancer <- merge(x = df_cancer, y = df_pop, by = "County", all = TRUE)
df_cancer <- merge(x = df_cancer, y = df_edu, by = "County", all = TRUE)
df_cancer <- merge(x = df_cancer, y = df_income, by = "County", all = TRUE)

# Clean cancer dataset for observed_total only
df_cancer <- df_cancer %>% 
  select(observed_Total, geoid_county) %>% 
  group_by(geoid_county) %>% 
  summarise(observed_total = sum(observed_Total, na.rm = TRUE))

# Clean population dataset: extract median age and gender ratio
df_pop <- df_pop %>%
  select(County, POPESTIMATE, POPEST_MALE, POPEST_FEM, MEDIAN_AGE_TOT) %>%
  group_by(County) %>%
  summarise(
    total_population = mean(POPESTIMATE, na.rm = TRUE),
    median_age = mean(MEDIAN_AGE_TOT, na.rm = TRUE),
    gender_ratio = mean(POPEST_MALE / POPEST_FEM, na.rm = TRUE)
  )


# Clean education dataset and calculate education_level index
char_cols <- df_edu %>%
  select(where(is.character)) %>%
  select(-County) %>%
  colnames()

df_edu <- df_edu %>%
  mutate(across(all_of(char_cols), ~ as.numeric(str_replace_all(., "[^0-9\\.]", "")))) %>%
  group_by(County) %>%
  summarize(
    less_than_hs = mean(`Total!!Estimate!!Less than high school graduate`, na.rm = TRUE),
    hs_grad = mean(`Total!!Estimate!!High school graduate (includes equivalency)`, na.rm = TRUE),
    some_college = mean(`Total!!Estimate!!Some college or associate's degree`, na.rm = TRUE),
    bachelor_plus = mean(`Total!!Estimate!!Bachelor's degree or higher`, na.rm = TRUE)
  ) %>%
  mutate(education_level = (1*less_than_hs + 2*hs_grad + 3*some_college + 4*bachelor_plus) /
           (less_than_hs + hs_grad + some_college + bachelor_plus)) %>%
  select(County, education_level)
colnames(df_edu)


# Merge all datasets into a single dataset for analysis
df_merged <- df_cancer %>%
  left_join(df_county, by = "geoid_county") %>%
  left_join(df_pop, by = "County") %>%
  left_join(df_edu, by = "County") %>%
  left_join(df_income, by = "County")

 colnames(df_merged)
  
  # Extract columns for factor analysis
  df_factor<-df_merged %>%
    select(County,median_age,gender_ratio,education_level,median_income)
  
  head(df_factor)
  
  # Extract the incidence rate by county
  df_incidence_rate <- df_merged %>%
    mutate(incidence_rate = (observed_total / total_population) * 100) %>%
    select(County, incidence_rate)
  
  # Forrelation matrix (excluding County column)
  cor_matrix <- round(cor(df_factor %>% select(-County), use = "complete.obs"), 3)
  print(cor_matrix)
  ################## median_age gender_ratio education_level median_income ################
  # median_age           1.000        0.220          -0.373        -0.132                  #
  # gender_ratio         0.220        1.000          -0.477        -0.265                  #
  # education_level     -0.373       -0.477           1.000         0.429                  #
  # median_income       -0.132       -0.265           0.429         1.000                  #
  ##########################################################################################
  library(corrplot)
  
  # plot for correlation
  corrplot(
    cor_matrix,
    type = "lower",
    col = colorRampPalette(c("red", "white", "green"))(200),
    method = "square",
    diag = FALSE
  )
  
  # Bartlett's test of sphericity
  factor_vars <- df_factor %>%
    select(-County)
    n_obs <- nrow(factor_vars)
  
  scaled_factor_vars <- scale(factor_vars)
   
  # Bartlett's test
  bartlett_result <- cortest.bartlett(cor(scaled_factor_vars, use = "complete.obs"), n = n_obs)
  print(bartlett_result)
  
  
  # KMO
  KMO(r = cor(scaled_factor_vars))
  
  # scree plot
  scree(cor(scaled_factor_vars),factors = T, pc=T)
  
  # eigen value
  data.frame(factor = 1:ncol(scaled_factor_vars), eigen = eigen(cor(factor_vars))$values)
  
 # factor     eigen
 #  1      1 1.9785714
 #  2      2 0.8710690
 #  3      3 0.7136046
 #  4      4 0.4367549
  
  # Parallel Analysis 
  fa.parallel(scaled_factor_vars, fa = "fa", fm = "pa")
  
  # Variance explained
  result = fa(r = scaled_factor_vars,nfactors = 1,fm = 'pa',rotate = 'none')
  result$Vaccounted
  
  # PA1
  #SS loadings    1.4942798
  #Proportion Var 0.3735699
  
  # Communalities
  data.frame(communality = result$communality)
  
  # Communality
  # median_age        0.2586987
  # gender_ratio      0.2911646
  # education_level   0.7717270
  # median_income     0.3088721
  
  fa_varimax = fa(r = scaled_factor_vars,nfactors = 1,fm = 'pa',rotate = 'varimax')
  print(fa_varimax$loadings,cut=0.15)

  # Factor loadings diagram
  fa.diagram(fa_varimax$loadings, sort = T)
  
  # Factor 1 appears to reflect a latent dimension of social resources and opportunity.
  # It is positively associated with higher levels of education (loading: +0.878) and median income (+0.492),
  # while negatively associated with gender ratio (−0.539) and median age (−0.419).
  # This suggests that counties with more educated, higher-income, relatively younger populations — and possibly a higher proportion of women — score higher on this factor. These characteristics are often linked to greater access to health-related resources, awareness, and preventive care.
  # Therefore, Factor 1 can be interpreted as a Social Opportunity and Health Access Factor, capturing the socioeconomic and demographic conditions that may influence cancer outcomes such as incidence or survivorship.
  
  
  # Save factor scores (explicitly name the column as "Factor1" to avoid errors)
  factor_scores <- data.frame(
    County = df_factor$County,
    Factor1 = fa_varimax$scores[, 1]   # Extract the first factor score and label it "Factor1"
  )
  
  # Merge factor scores with cancer incidence rate data
  df_model <- left_join(factor_scores, df_incidence_rate, by = "County") %>%
    na.omit()
  
  # Run linear regression: incidence rate ~ factor score
  lm_model <- lm(incidence_rate ~ Factor1, data = df_model)
  
   # Check linear relationship using original independent variables
  summary(lm_model)
  
  # Fit regression model using education and income separately
  df_model_direct <- df_merged %>%
    select(County, education_level, median_income, observed_total, total_population) %>%
    mutate(incidence_rate = (observed_total / total_population) * 100) %>%
    na.omit()
  
  lm_direct <- lm(incidence_rate ~ education_level + median_income, data = df_model_direct)
  summary(lm_direct)
 
   # Evaluate model accuracy with RMSE
  set.seed(42)
  train_idx <- sample(1:nrow(scaled_factor_vars), size = 0.8 * nrow(scaled_factor_vars))
  train_data <- df_model[train_idx, ]
  test_data <- df_model[-train_idx, ]
  
  # Fit the model on training data
  lm_model <- lm(incidence_rate ~ Factor1, data = train_data)
  
  # Predict on test set
  test_data$predicted <- predict(lm_model, newdata = test_data)
  
  # Compute RMSE
  rmse <- sqrt(mean((test_data$incidence_rate - test_data$predicted)^2))
  print(paste("RMSE on test set:", round(rmse, 4)))