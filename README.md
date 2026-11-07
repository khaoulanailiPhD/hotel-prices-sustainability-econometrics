# Empirical Econometric Analysis of Hotel Prices and Sustainability

## Overview

This project presents an empirical econometric analysis investigating the relationship between sustainability certifications, customer review topics and hotel prices.

The analysis uses R to implement a reproducible workflow covering data preparation, econometric estimation, heterogeneous effects and statistical inference.

## Research Objectives

- Examine the relationship between sustainability certification and hotel prices.
- Analyse the association between customer review dimensions and hotel prices.
- Investigate heterogeneous relationships across hotel categories and certification status.
- Estimate average marginal effects using counterfactual predictions.
- Assess statistical uncertainty using bootstrap procedures.

## Methodology

The empirical analysis includes:

1. **Data preparation:** Data cleaning, sample selection, variable construction and missing-value management.
2. **Descriptive analysis:** Summary statistics and sample validation.
3. **Econometric modelling:** Multiple Ordinary Least Squares (OLS) regression models.
4. **Interaction effects:** Analysis of heterogeneous relationships between review topics, hotel stars and sustainability certification.
5. **Statistical inference:** Nested-model F-tests and heteroskedasticity-robust standard errors (HC2).
6. **Marginal effects:** Counterfactual predictions and Average Marginal Effect (AME) estimation.
7. **Bootstrap inference:** Bootstrap standard errors and 95% confidence intervals, including percentile and BCa methods.

## Tools and Technologies

- **Programming:** R
- **Data management:** dplyr, readxl, readr
- **Econometrics:** lm, anova, lmtest, sandwich
- **Statistical inference:** boot
- **Results presentation:** stargazer

## Repository Structure

```text
hotel-prices-sustainability-econometrics/
│
├── README.md
├── Empirical_Analysis_Hotel_Prices.R
├── results/
│   ├── model_comparison.csv
│   └── AME_bootstrap_results.csv
└── .gitignore
```

## Reproducibility

The R script is organized into modular sections, from data import and preparation to model estimation and result export.

To reproduce the analysis:

1. Install the required R packages.
2. Provide access to the underlying dataset.
3. Configure the data input path.
4. Run the R script sequentially.

**Data availability:** The underlying dataset is not included in this repository. Access to the original data is subject to applicable data access and redistribution restrictions.

## Author

**Khaoula Naili**

PhD in Economics | Econometrics | Natural Language Processing | Data Science

Research interests: Applied Econometrics, Economics of Law, NLP Applications in Economics and Sustainability.
