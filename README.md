# Case Studies Portfolio - Quantitative Analysis & Forecasting

This repository contains a collection of analytical case studies covering descriptive statistics, distribution comparison, regression modeling, and financial forecasting. The projects demonstrate applied statistical reasoning, reproducible workflows, and model comparison across classical and machine-learning approaches.

The case studies progress from foundational statistical analysis to predictive modeling and financial time-series forecasting.

---

## Repository Structure

| Project | Focus Area | Key Methods | Tools |
|----------|------------|------------|--------|
| **Descriptive Demographic Analysis** | Exploratory Data Analysis | Summary statistics, visualization | R |
| **Distribution Comparison Study** | Statistical distribution testing | One-way ANOVA, multiple-testing correction | R |
| **Regression Modeling Case Study** | Linear regression & interpretation | Best-subset regression (AIC/BIC) | R (IRkernel/Jupyter) |
| **Equity Premium Forecasting (Baseline)** | Financial time-series forecasting | AR models, OLS predictive regression | R |
| **Equity Premium Forecasting (ML)** | Machine learning in finance | Regression trees, random forests | R |
| **Forecast Comparison Study** | Model evaluation | Rolling-window forecasts, Diebold-Mariano test | R |

---

## Data

All datasets live in the shared `data/` folder at the repository root. Each project reads from `../data/<filename>` relative to its own folder.

| File | Description | Used by |
|------|-------------|---------|
| `census2001_2021.csv` | UN demographic data: life expectancy and infant mortality by country/region, 2001 and 2021 | P1 |
| `SwimmingTimes.csv` | European Championship 200 m swim finishing times across five stroke categories | P2 |
| `bodymeasurements.csv` | Body measurement dataset (height, weight, and body-part circumferences) for regression modelling | P3 |
| `PredictorData2022.xlsx - Monthly.csv` | Goyal-Welch equity-premium predictors at monthly frequency, through 2022 | P4, P5, P6 |
| `PredictorData2022.xlsx - Quarterly.csv` | Same predictors aggregated to quarterly frequency | P4, P6 |
| `PredictorData2022.xlsx - Annual.csv` | Same predictors aggregated to annual frequency | P4, P6 |

---

## Detailed Project Overview

### 1. Descriptive Demographic Analysis
- Exploratory analysis of life expectancy (male, female, combined) and infant mortality rate
- Histograms, pair-plot correlations, and boxplots by region/subregion
- 2001 vs. 2021 scatter-plot comparison
- Implemented in R with both script (`ICSProject1.R`) and R Markdown (`ICSProject1.Rmd`)

### 2. Comparison of Multiple Distributions
- One-way ANOVA to test for differences in 200 m finishing times across five stroke categories
- Pairwise t-tests with Bonferroni and Holm multiple-testing corrections
- Assumptions checked via QQ plots and Levene's test
- Reproducible workflow in R / R Markdown

### 3. Regression Modeling Case Study
- Best-subset regression to predict height from body measurements
- AIC and BIC used for model selection; full and reduced models compared
- Residual diagnostics (QQ plot, residual-vs-fitted) reported
- Implemented in R using an IRkernel Jupyter Notebook (`ICSProject3.ipynb`)  
  *(Note: the kernel is R, not Python — the notebook requires IRkernel installed)*

### 4. Equity Premium Forecasting (Baseline)
- Excess return series derived from Goyal-Welch predictors at monthly, quarterly, and annual frequencies
- AR(p) model order selected by AIC; full-sample fitted values compared to actual returns
- OLS predictive regressions using each predictor individually and all predictors jointly
- Backward and forward stepwise selection (AIC) for predictor reduction

### 5. Equity Premium Forecasting Using Machine Learning
- Monthly data only; regression trees and random forests as forecasting models
- Cross-validated model selection on training split; evaluated on held-out test set
- Two forecast scenarios: (a) lagged excess returns only, (b) all lagged predictors
- Permutation-based variable importance reported for both model families  
  *(Folder name is `Project5-Forecasting-The-Equity-Premium-Using-ML`)*

### 6. Equity Premium Forecasting — Comparative Study
- Rolling/expanding-window one-step-ahead forecasts across annual, quarterly, and monthly frequencies
- Models compared: AR/linear, backward-selected OLS, forward-selected OLS, regression tree, random forest, single-predictor OLS
- Diebold-Mariano pairwise test; p-value heatmaps produced for each frequency

---

## Core Competencies Demonstrated

- Exploratory Data Analysis (EDA)
- Statistical inference and hypothesis testing
- Linear regression modeling and diagnostics
- Financial time-series forecasting
- Machine-learning model implementation
- Model comparison and evaluation
- Reproducible research workflows

---

## Technical Stack

- **R / RStudio**
- R Markdown for reproducible reporting
- **R (IRkernel)** via Jupyter Notebook for Project 3
- Structured analytical documentation (PDF reports)

---

## Reproducibility

Each project folder contains:

- Source code (`.R`, `.Rmd`, or `.ipynb`)
- Final report documentation (PDF)

All datasets are in the shared `data/` folder at the repository root.

To reproduce analyses:

1. Navigate to the project folder.
2. Install required packages (see script headers).
3. Run the script or notebook (working directory should be the project folder).
4. Review the report for methodology and interpretation.

---

## Notes

These projects were completed as part of structured quantitative analysis coursework and are presented to document applied statistical and predictive modeling capabilities.
All datasets used in these analyses are included in the shared `data/` folder for full reproducibility.
