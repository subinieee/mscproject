# MSc Project: Machine Learning Methods for Cancer Survival Prediction

## Status: Archive

This repository contains scripts used for my MSc dissertation and is
provided as an **archival record of the computational analyses
performed**.\
It is not maintained as a reusable software package, and scripts may
contain environment-specific paths or assumptions.

------------------------------------------------------------------------

## Dissertation

**Machine Learning Methods for Cancer Survival Prediction**\
Subin Choi\
UCL Cancer Institute, MSc Cancer, University College London, 2019

This project investigated statistical, Bayesian, and machine learning
approaches for modelling cancer survival and genomic mutation data.

------------------------------------------------------------------------

## Dissertation context

Cancer survival prediction presents unique challenges due to censored
observations, high-dimensional genomic predictors, and complex
biological relationships.

This dissertation evaluated three complementary modelling approaches:

-   Bayesian Multivariate Regression (BMR) using the Saddlepoint
    Signature method
-   Cox proportional hazards regression
-   Random Survival Forest (RSF)

The goal was to assess their ability to model survival outcomes and
identify meaningful predictors from genomic and clinical data.

------------------------------------------------------------------------

## Repository structure

    mscproject/
    │
    ├── Data/                    # Synthetic survival datasets
    ├── Data formatting/         # Data preprocessing and preparation scripts
    ├── Survival curves/         # Cox regression and Kaplan–Meier survival analysis
    ├── Random Forest/           # Random Survival Forest modelling scripts
    ├── Autoit for SaddlePoint/  # Automation scripts for Saddlepoint Signature (BMR)
    └── README.md

------------------------------------------------------------------------

## Methods implemented

### Bayesian Multivariate Regression (BMR) using Saddlepoint Signature

Bayesian Multivariate Regression is a regularised extension of multivariate Cox regression used to model survival outcomes and identify predictive features.

The method combines:

- Cox multivariate regression as the underlying survival model
- Ridge (L2) regularisation to reduce overfitting
- Bayesian inference to estimate regression coefficients probabilistically
- Recursive Backward Elimination (RBE) for iterative feature selection

This approach improves prediction accuracy and model stability, particularly in high-dimensional datasets.

The final model produces regression coefficients and a multivariate risk score that combines multiple predictors into a single survival prediction measure.

Saddlepoint Signature software was used to perform Bayesian Multivariate Regression (BMR) and to generate synthetic survival datasets with controlled variations in dimensionality, noise, censoring, and negative control variables. These datasets were used to evaluate model robustness and predictive performance under realistic data constraints.

AutoIt scripts in this repository were used to automate execution of Saddlepoint Signature.

Relevant directory:

    Autoit for SaddlePoint/

------------------------------------------------------------------------

### Cox proportional hazards model

The Cox proportional hazards model was used as a classical survival
analysis baseline.

Key features:

-   Models time-to-event survival outcomes
-   Provides interpretable regression coefficients
-   Widely used in clinical survival analysis

Relevant directory:

    Survival curves/

------------------------------------------------------------------------

### Random Survival Forest (RSF)

Random Survival Forest was used as a machine learning survival modelling
approach.

Key features:

-   Captures nonlinear relationships between predictors and survival
-   Handles high-dimensional genomic data
-   Models complex feature interactions
-   Provides variable importance measures

Relevant directory:

    Random Forest/

------------------------------------------------------------------------

## Synthetic datasets

The `Data/` directory contains synthetic survival datasets generated using the Saddlepoint Signature software to evaluate modelling approaches under controlled statistical conditions.

These datasets were designed to simulate key challenges encountered in cancer survival and genomic data analysis.

The simulation scenarios were defined using the following constraint parameters:

- **Number of features (P):** total number of predictor variables, including both true predictive features and noise features  
- **Number of true features (X):** number of predictor variables that have a true association with survival outcome  
- **Missing rate (NA%):** proportion of missing values in the dataset  
- **End-of-trial censoring rate (EOT%):** proportion of samples whose survival time is censored at the end of the study period  

These parameters allowed systematic evaluation of model performance under varying levels of complexity and data constraints.

---

### Dimensionality

Dimensionality refers to the number of predictor variables (P) relative to the number of samples.

High-dimensional datasets, where the number of features is large, are common in genomic studies and increase the risk of overfitting. These synthetic datasets include different dimensional settings to evaluate how modelling methods perform under varying feature-space complexity.

---

### Noise and true features

The datasets include both:

- **True features (X):** predictors with a real association with survival outcome  
- **Noise features:** predictors with no true association with survival outcome  

Noise features simulate irrelevant variables commonly present in real-world datasets. This allows evaluation of the model’s ability to distinguish meaningful predictors from irrelevant ones and assess robustness to overfitting.

---

### Missing data

Missing data were introduced at controlled rates (NA%) to simulate incomplete observations, which are common in clinical and genomic datasets.

This allows evaluation of how modelling methods handle missing information and whether missingness affects predictive performance.

---

### Censoring

End-of-trial censoring (EOT%) was introduced to simulate realistic survival analysis conditions.

Censoring occurs when the exact survival time is not observed, for example when a patient is still alive at the end of the study period.

This ensures the synthetic datasets reflect the fundamental statistical properties of survival data.

---

### Purpose of synthetic datasets

These synthetic datasets provide a controlled framework for evaluating the performance, stability, and feature selection behaviour of Bayesian Multivariate Regression, Cox proportional hazards regression, and Random Survival Forest methods under varying statistical constraints.

------------------------------------------------------------------------

## Data processing

Scripts for data preparation and formatting can be found in:

    Data formatting/

These scripts were used to prepare datasets prior to modelling.

------------------------------------------------------------------------

## Repository purpose

This repository serves as a record of the computational work performed
during the MSc dissertation, including:

-   Survival analysis scripts
-   Machine learning survival modelling
-   Bayesian multivariate regression workflows
-   Automation scripts for running Saddlepoint Signature (Bayesian Multivariate Regression)
-   Data preprocessing scripts

This repository is intended for documentation and reference purposes.

------------------------------------------------------------------------

## Author

Subin Choi\
UCL Cancer Institute\
University College London\
MSc Cancer, 2019
