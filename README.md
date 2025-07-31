# Racial, Age, and Sex Disparities in Suicide Mortality in Santa Clara County

## Project Overview

This project analyzes suicide deaths in Santa Clara County, California, from 2018 to 2023 using Medical Examiner-Coroner data combined with U.S. Census American Community Survey (ACS) estimates. 
It quantifies suicide rates across racial/ethnic, age, and gender groups; maps geographic disparities; recodes contributing conditions and causes of death; and fits statistical models to estimate trends and group differences.
The paper for which this code was written is currently under review at the *American Journal of Preventive Medicine Focus.*

The Medical Examiner data can be downloaded [here](https://data.sccgov.org/Health/Medical-Examiner-Coroner-Full-dataset/s3fb-yrjp/about_data)

![Suicide Trend](images/suicide-trend.png)

## Data Sources

- **Death Records**  
  Source: Santa Clara County Medical Examiner-Coroner  
  File: `Medical_Examiner-Coroner__Full_dataset.csv`  
  Content: Manner and cause of death, age, race, gender, date and location of death, etc.

- **Population Estimates**  
  Source: U.S. Census Bureau, 2022 ACS 5-Year Estimates  
  Accessed via the `tidycensus` R package  
  Variables: Age-by-race-by-gender breakdowns for White, Black, Hispanic, and Asian groups

- **Geospatial Data**  
  Description: City-level geographic boundaries for mapping suicide rates (not incorporated into the paper, but possible)

## Key Analytical Components

### 1. Data Cleaning & Transformation
- Excluded records with non-binary gender or missing/invalid age or race.
- Recoded `Race`, `Gender`, and `Age` into standardized analytic categories.
- Extracted and categorized `Cause of Death` and `Other Significant Condition` using keyword-based parsing.
- Recoded incident locations into domains such as Residential, Open Space, Hotel/Motel, etc.

### 2. Population Denominators
- Queried ACS variables for age, race, and sex at place and county levels.
- Aggregated estimates into custom age bands: `5-14`, `15-19`, ..., `≥ 65`.
- Merged with death records to compute age- and race-adjusted suicide rates.

### 3. Descriptive Analysis & Visualization
- Time-series plots of suicide counts and moving averages (monthly and yearly).
- Highlighted key policy periods: COVID mandates, eviction moratorium, etc.
- Geographic maps of city-level suicide rates by race using `ggplot2` and `tmap`.
- Cross-tabulated suicide counts by location type, race, age, and cause.

### 4. Statistical Modeling
- Fit negative binomial models to estimate suicide trends (2018–2023) across subgroups.
- Modeled suicide counts using offsets for population denominators.
- Applied `emmeans` to compare marginal rates and assess interactions.
- Conducted segmented regression and Davies tests to identify breakpoints in time trends.

### 5. Hypothesis Testing
- **Difference-in-Proportions Z-tests** for gender differences in methods of suicide.
- **Fisher’s Exact Tests** for age distribution by gender within racial groups.
- **T-tests** and **Mann-Whitney U Tests** for age-at-death comparisons by group.

## Sample Outputs
- City-level maps of suicide rates by race
- Time-series plots with COVID policy overlays
- Summary tables by race, gender, age, location type, and mental health history
- Regression output tables with exponentiated coefficients and confidence intervals
- Tables exported to Word using `officer` and `flextable`

## Dependencies

This analysis uses the following R packages:

```r
library(dplyr)
library(readr)
library(lubridate)
library(tidyr)
library(tidycensus)
library(ggplot2)
library(sf)
library(tmap)
library(zoo)
library(MASS)
library(segmented)
library(sandwich)
library(lmtest)
library(emmeans)
library(broom)
library(gtsummary)
library(officer)
library(flextable)
