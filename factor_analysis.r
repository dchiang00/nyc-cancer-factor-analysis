# import libraries
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(viridis)
library(psych)

# read data
df_county <- read.csv('2. GeoID_County_Mapping.csv')
df_cancer <- read.csv('1. NYSDOH_CancerMapping_Data.csv')
df_pop <- read.csv('5. County_Population_by_Characteristics.csv')
df_edu <- read.csv('3. Education_by_County.csv')
df_income <- read.csv('4. Income_by_County.csv')

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

  print(na_counts_per_col)
  # We identify there were several N/A values, but it is in variables that
  # we will use. Therefore we choose to delete and ignore them
  
  # clean cancer dataset:
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
  
  # clean population dataset:
  #   keep columns of interest
  #   remove the string county, filter to latest year
  colnames(df_pop)[colnames(df_pop) == "COUNTY"] <- "County_ID"
  colnames(df_pop)[colnames(df_pop) == "CTYNAME"] <- "County"
  
  df_pop <- df_pop %>%
    mutate(County = str_remove(County, " County")) %>% 
    filter(YEAR == 12) %>% 
    select(-c(SUMLEV, STATE, County_ID, STNAME, YEAR))
  
  # clean county dataset:
  #   get distinct geo_id and county names
  colnames(df_county)[colnames(df_county) == "GEOID.For.County"] <- "geoid_county"
  
  df_county <- df_county %>% 
    select(geoid_county, County) %>% 
    distinct()
  
  # clean education dataset:
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
  
  # clean income dataset:
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
  
  # join datasets to get cancer, county, population, income, and education level
  df_cancer <- merge(x = df_cancer, y = df_county, by = "geoid_county", all = TRUE)
  df_cancer <- merge(x = df_cancer, y = df_pop, by = "County", all = TRUE)
  df_cancer <- merge(x = df_cancer, y = df_edu, by = "County", all = TRUE)
  df_cancer <- merge(x = df_cancer, y = df_income, by = "County", all = TRUE)
  
  # clean cancer dataset for observed_total only
  
  df_cancer <- df_cancer %>% 
    select(observed_Total, geoid_county) %>% 
    group_by(geoid_county) %>% 
    summarise(observed_total = sum(observed_Total, na.rm = TRUE))
  
  # clean population dataset: extract median age and gender ratio
 
  df_pop <- df_pop %>%
    select(County, POPESTIMATE, POPEST_MALE, POPEST_FEM, MEDIAN_AGE_TOT) %>%
    group_by(County) %>%
    summarise(
      total_population = mean(POPESTIMATE, na.rm = TRUE),
      median_age = mean(MEDIAN_AGE_TOT, na.rm = TRUE),
      gender_ratio = mean(POPEST_MALE / POPEST_FEM, na.rm = TRUE)
    )
  
 
  # clean education dataset and calculate education_level index

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
 
  
  # merge all datasets into a single dataset for analysis
  df_merged <- df_cancer %>%
    left_join(df_county, by = "geoid_county") %>%
    left_join(df_pop, by = "County") %>%
    left_join(df_edu, by = "County") %>%
    left_join(df_income, by = "County")
  
  colnames(df_merged)
  
  # extract columns for factor analysis
  df_factor<-df_merged %>%
    select(County,median_age,gender_ratio,education_level,median_income)
  
  head(df_factor)
  
  # extract the incidence rate by county
  df_incidence_rate <- df_merged %>%
    mutate(incidence_rate = (observed_total / total_population) * 100) %>%
    select(County, incidence_rate)
  
  # correlation matrix (excluding County column)
  cor_matrix <- round(cor(df_factor %>% select(-County), use = "complete.obs"), 3)
  print(cor_matrix)
  
  
  
# Bartlett's test of sphericity
  
  factor_vars <- df_factor %>% select(-County)
   n_obs <- nrow(factor_vars)
  
  # Bartlett's test
  bartlett_result <- cortest.bartlett(cor(factor_vars, use = "complete.obs"), n = n_obs)
  print(bartlett_result)
  
  
  #KMO
  KMO(r = cor(factor_vars))
  
  #scree plot
  scree(cor(factor_vars),factors = T, pc=T)
  
  #eigen value
  data.frame(factor = 1:ncol(factor_vars), eigen = eigen(cor(factor_vars))$values)
  
  #variance explained
  result = fa(r = factor_vars,nfactors = 2,fm = 'pa',rotate = 'none')
  result$Vaccounted
  
  data.frame(communality = result$communality)
  
  print(result$loadings, cut=0)
  
  result$Vaccounted
  
  #Factor 1 appears to reflect a latent dimension of social resources and opportunity.
  #It is positively associated with higher levels of education (loading: +0.878) and median income (+0.492),
  #while negatively associated with gender ratio (−0.539) and median age (−0.419).
  #This suggests that counties with more educated, higher-income, relatively younger populations — and possibly a higher proportion of women — score higher on this factor. These characteristics are often linked to greater access to health-related resources, awareness, and preventive care.
  #Therefore, Factor 1 can be interpreted as a Social Opportunity and Health Access Factor, capturing the socioeconomic and demographic conditions that may influence cancer outcomes such as incidence or survivorship.
  
