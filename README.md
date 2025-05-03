# Factor Analysis of the Drivers of Cancer Prevalence in New York

## Overview

This project investigates the latent socioeconomic and demographic patterns influencing cancer incidence across New York counties. Using Exploratory Factor Analysis (EFA) and regression modeling, we identify key variables that affect regional disparities in cancer outcomes. Our analysis reveals a latent factor capturing higher education, income, younger median age, and a greater proportion of women, which is significantly associated with lower cancer incidence.

## Research Questions

1. What underlying socioeconomic and demographic factors contribute to variations in cancer prevalence across New York counties?
2. Do higher social status and education reduce cancer incidence, or are there structural barriers that limit health equity?
3. Can factor analysis uncover hidden patterns beyond simple variable correlations?

## Data Sources

Our final dataset includes 152 variables drawn from five public datasets:

- **NY State Dept. of Health Cancer Mapping (2011–2015)**: Observed cancer cases by type at block group level.
- **GEOID to County Mapping**: Matches geographic identifiers to counties.
- **US Census Education Data**: Educational attainment levels.
- **US Census Income Data**: Median household income.
- **US Census Population Characteristics**: Demographics by age and gender.

Note: Data was aggregated and cleaned at the county level, representing 62 counties in NY State.

## Data Cleaning and Preprocessing

- Standardized county names and identifiers.
- Consolidated variables into key indicators:
  - `median_age`
  - `gender_ratio` (male-to-female)
  - `education_level` (weighted education index)
  - `median_income`

## Methodology

### 1. Exploratory Factor Analysis (EFA)

- Assessed dataset suitability with:
  - Correlation Matrix
  - Bartlett’s Test (χ² = 36.56, p < 0.001)
  - KMO = 0.65
- Scree Plot and Parallel Analysis suggested **1 latent factor**.
- Extracted factor explained **37.4% of total variance**.
- Highest loading: `education_level (0.921)`
- Interpretation: **"Socioeconomically Advantaged Young Women Group"**

### 2. Regression Analysis

- Used latent factor as independent variable for predicting cancer incidence rate.
- Model:  
  ```
  cancer_incidence_rate = β0 + β1 * latent_factor + ε
  ```
  - β1 = -0.173 (p < 0.01)
  - R² = 0.115
- Compared to baseline model using raw education and income, the latent factor showed stronger predictive power.

### 3. Model Accuracy

- **RMSE** on test set: **0.6688**
- Given incidence rates range from 2–5%, this is acceptable in social science modeling.

## Key Findings

- Higher education and youth demographics are more predictive of lower cancer incidence than income alone.
- Latent factor modeling outperforms individual predictors.
- Structural advantages embedded in education and gender composition drive lower incidence.

## Recommendations

1. **Invest in Health Education**: Especially for less-educated counties.
2. **Design Holistic Interventions**: Use aggregate demographic profiles for targeted public health strategies.
3. **Prioritize Structurally Disadvantaged Counties**: Direct healthcare support where composite scores are low.

## Future Work

- Incorporate additional variables to extract multiple latent dimensions.
- Explore causal pathways and intervention strategies at a more granular level.

## References

- Griffiths et al. (2006). [OEM Article](https://doi.org/10.1136/oem.2005.023671)  
- Hastert et al. (2019). [Journal of Cancer Survivorship](https://doi.org/10.1007/s11764-019-00764-y)  
- Juang et al. (2025). [Scientific Reports](https://doi.org/10.1038/s41598-025-19812-7)  
- Miller et al. (2022). [CA: A Cancer Journal for Clinicians](https://doi.org/10.3322/caac.21731)  
- Singh & Jemal (2017). [Journal of Environmental and Public Health](https://doi.org/10.1155/2017/2819372)  
- Tavakol & Wetzel (2020). [IJME Article](https://doi.org/10.5116/ijme.5f96.0f4)
