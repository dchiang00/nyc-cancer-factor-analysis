# Factor Analysis of the Drivers of Cancer Prevalence in New York

---

## Introduction

Cancer remains a leading cause of mortality in New York State, with its prevalence shaped by socioeconomic and demographic factors. While prior research highlights disparities in cancer incidence and survival, most studies focus on racial differences at a national level, often using regression-based models to examine direct causal links. However, fewer studies explore whether higher social status—measured by income and education—translates into better survival outcomes for chronic diseases like cancer.

This study addresses a fundamental equity question: **Does higher socioeconomic status truly improve survival, or do structural barriers limit health equity?** Using Exploratory Factor Analysis (EFA), we will identify latent socioeconomic and demographic patterns influencing cancer survival disparities across New York counties. We will then construct a linear regression model to quantify the impact of these factors on survival rates, offering insights into how social mobility intersects with health outcomes.

---

## Research Questions and Hypotheses

This project seeks to explore the following research questions:

- What underlying socioeconomic and demographic factors contribute to variations in cancer prevalence and survival across New York counties?
- Are individuals with higher social status truly at an advantage in surviving chronic diseases, or do structural healthcare barriers limit their survival gains?
- Does factor analysis reveal hidden systemic patterns that challenge the assumption that economic and educational success guarantees better health outcomes?

By addressing these questions, we aim to go beyond simple correlations and critically examine whether social mobility genuinely impacts cancer survival, shedding light on broader questions of health equity and structural disparities in healthcare access.

---

## Literature Review

Much of the existing research has focused on national-level data, often emphasizing racial disparities as a primary explanatory factor for socioeconomic influences on cancer outcomes.

- **Miller et al. (2022)** highlight significant racial disparities in cancer survivorship due to barriers in healthcare access and systemic inequities.
- **Singh and Jemal (2017)** analyzed six decades of data, demonstrating that the correlation between SES and cancer mortality has intensified—lower-income groups now experience significantly higher rates due to socioeconomic disadvantages.
- **Ramsey et al. (2016)** provide evidence that financial insolvency is a strong predictor of early mortality.

These findings reinforce the critical role of socioeconomic disparities in cancer outcomes; however, most studies have primarily focused on race as the key demographic factor, without accounting for regional or local variations.

Unlike previous research, this study emphasizes **EFA** to examine whether latent variables significantly influence cancer mortality at the county level within New York State.

---

## Data Collection

Our data is combined from five separate datasets found through the US Census Bureau and NY State Data. Each dataset is then aggregated into county-level summaries that is then combined and cleaned into one complete dataset for analysis (see Data Preparation Efforts):

1. **New York State Dept. of Health (DOH) Cancer Mapping Data (2011–2015)**  
   - Dataset of cancer occurrences across 13,514 GEOID (DOH block groups).  
   - Includes expected cancer rates based on 2005–2009 data.  
   - Indicates whether the block group is in an elevated cancer region.

2. **New York State GEOID to County Mapping**  
   - Dataset appended with additional research that maps the GEOID found in the Cancer Mapping Data to NY counties.

3. **US Census Bureau New York County Education Data**  
   - Dataset of educational attainment levels as a percentage of county population, across NY counties.

4. **US Census Bureau New York County Income Data**  
   - Dataset of household income across NY counties.  
   - Includes 122 variables; only **median income** is selected.

5. **US Census Bureau New York County Population by Characteristics**  
   - Dataset of New York county populations, divided by age group and gender.  
   - Contains data collected across 12 years from 2009; only the 12th year is used.

---

## Data Cleaning and Preprocessing

To prepare the dataset for analysis, we performed extensive data cleaning and preprocessing to ensure accuracy, consistency, and usability. Our efforts focused on standardizing formats, resolving inconsistencies, and integrating multiple sources into a single, structured dataset.

### Cleaning and Standardization

- **Cancer Data:**  
  - Extracted the first five digits from the `Dohregion` column to create a county-level geographic identifier (`geoid_county`).  
  - Retained relevant columns such as observed and expected cancer cases.  
  - Aggregated data at the county level.

- **Population Data:**  
  - Standardized column names.  
  - Removed unnecessary text (e.g., "county" from county names).  
  - Filtered for the most recent year’s data.

- **Education and Income Data:**  
  - Reformatted county identifiers.  
  - Processed the `Geographic Area Name` column to derive county names.  
  - Aggregated data to obtain education attainment percentages and median household income.

### Merging Process

After cleaning, datasets were merged sequentially:

1. Linked cancer data to county mapping.
2. Added population data.
3. Added education data.
4. Added income data.

The final dataset includes **62 counties** and **152 variables**, covering cancer prevalence, demographics, education, and income. While our initial analysis focuses on five primary variables, we will incorporate additional ones to refine our factor analysis and regression model.

---

## Methodology and Analysis Plan

To assess the suitability of our dataset for factor analysis, we will first examine the correlation matrix among income, education, and cancer cases. **Bartlett’s Test of Sphericity** will be conducted to confirm significant correlations between variables (p < 0.05). We will also compute the **Kaiser-Meyer-Olkin (KMO)** score, aiming for a value above 0.6 to validate the adequacy of factor extraction.

The optimal number of factors will be determined using:

- **Scree Plot**
- **Eigenvalue Criterion** (eigenvalues > 1)
- Minimum 60% of total variance explained

Factor extraction will be performed using **Maximum Likelihood Estimation (MLE)**. We anticipate that socioeconomic factors (e.g., income and education) will cluster together, while regional and demographic variables may form distinct factors representing geographic disparities in cancer prevalence.

Following factor extraction, we will employ a **regression model** to assess the impact of these latent factors on mortality differences among counties. This analysis will help quantify the extent to which socioeconomic and regional disparities contribute to variations in mortality outcomes.

---

## Conclusion

This study aims to uncover structural inequalities influencing cancer survival rates in New York State. Through **EFA**, we will identify hidden patterns and validate their impact using a **regression model**, challenging the notion that economic and educational success alone guarantees better health outcomes.

---

## References

- Tavakol, M., & Wetzel, A. (2020). *Factor analysis: A means for theory and instrument development in support of construct validity*. International Journal of Medical Education, 11, 245–247. https://doi.org/10.5116/ijme.5f96.0f4a

- Miller, K. D., Nogueira, L., Devasia, T., Mariotto, A. B., Yabroff, K. R., Jemal, A., Kramer, J., & Siegel, R. L. (2022). *Cancer treatment and survivorship statistics, 2022*. CA: A Cancer Journal for Clinicians, 72(5), 409–436. https://doi.org/10.3322/caac.21731

- Ramsey, S. D., Bansal, A., Fedorenko, C. R., Blough, D. K., Overstreet, K. A., Shankaran, V., & Newcomb, P. (2016). *Financial insolvency as a risk factor for early mortality among patients with cancer*. Journal of Clinical Oncology, 34(9), 980–986. https://doi.org/10.1200/JCO.2015.64.6620

- Singh, G. K., & Jemal, A. (2017). *Socioeconomic and racial/ethnic disparities in cancer mortality, incidence, and survival in the United States, 1950–2014: Over six decades of changing patterns and widening inequalities*. Journal of Environmental and Public Health, 2017, 1–19. https://doi.org/10.1155/2017/2819372