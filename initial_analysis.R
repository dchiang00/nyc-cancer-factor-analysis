# import libraries
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(viridis)
library(psych)

# read data
df_county <- read.csv('Data/GeoID_County_Mapping.csv')
df_cancer <- read.csv('Data/NYSDOH_CancerMapping_Data.csv')
df_pop <- read.csv('Data/County_Population_by_Characteristics.csv')
df_edu <- read.csv('Data/Education_by_County.csv')
df_income <- read.csv('Data/Income_by_County.csv')

# basic data exploration for datasets
str(df_county)
str(df_cancer)
str(df_pop)
str(df_edu)
str(df_income)

# count NA values in each dataframe, per variable
na_counts_per_col <- list(
  df_county = colSums(is.na(df_county)),
  df_cancer = colSums(is.na(df_cancer)),
  df_pop = colSums(is.na(df_pop)),
  df_edu = colSums(is.na(df_edu)),
  df_income = colSums(is.na(df_income))
)

# We identify there were several N/A values, but it is in variables that
# we will use. Therefore we choose to delete and ignore them
print(na_counts_per_col)

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

# EDA and visualizations to explore variables

# slect relevant observed cancer columns
cancer_cols <- c("observed_Bladder", "observed_Bone", "observed_Brain", 
                 "observed_Breast", "observed_Colorectal", "observed_Esophagus", 
                 "observed_Kidney", "observed_Larynx")

# convert data into long format for visualization
long_data <- df_cancer %>%
  select(County, all_of(cancer_cols)) %>%
  pivot_longer(cols = -County, names_to = "Cancer_Type", values_to = "Cases")

# create a heatmap of observed cancer cases by county
ggplot(long_data, aes(x = Cancer_Type, y = County, fill = Cases)) +
  geom_tile(color = "white") +
  scale_fill_viridis(option = "magma", direction = -1) +  # Use a color gradient
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Heatmap of Observed Cancer Cases by County",
       x = "Cancer Type", y = "County",
       fill = "Observed Cases")

# create a bar chart of median income by county
ggplot(df_cancer, aes(x = reorder(County, -median_income), y = median_income, fill = median_income)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis(option = "magma", direction = -1) +  # Use a color gradient
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Median Income by County",
       x = "County", y = "Median Income",
       fill = "Income Level")
# initial factor analysis
df_no_county <- df_cancer[,-1:-2]
df_no_county <- as.data.frame(scale(df_no_county))

# suitability for factor analysis
cortest.bartlett(cor(df_no_county),n = nrow(df_no_county))
KMO(r = cor(df_no_county))

# Overall MSA from Cortest Bartlett test returning 0.5, indicated the data
# is not yet suitable for factor analysis. In our next factor_analysis.r code,
# we will run our next iteration with featuer engineering to better prepare our data
# for factor analysis

# determine the number of factors
scree(cor(df_no_county),factors = T, pc=T)
data.frame(factor = 1:ncol(df_no_county), eigen = eigen(cor(df_no_county))$values)
