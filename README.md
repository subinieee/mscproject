# MSc Project --- Machine Learning Methods for Cancer Survival Prediction

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
    Signature method\
-   Cox proportional hazards regression\
-   Random Survival Forest (RSF)

The goal was to assess their ability to model survival outcomes and
identify meaningful predictors from genomic and clinical data.

------------------------------------------------------------------------

## Repository structure

    mscproject/
    │
    ├── Data/                     # Synthetic survival datasets
    ├── Data formatting/         # Data preprocessing and preparation scripts
    ├── Survival curves/         # Cox regression and Kaplan–Meier survival analysis
    ├── Random Forest/           # Random Survival Forest modelling scripts
    ├── Autoit for SaddlePoint/  # Automation scripts for Saddlepoint Signature (BMR)
    └── README.md

------------------------------------------------------------------------

## Methods implemented

### Bayesian Multivariate Regression (BMR) using Saddlepoint Signature

Bayesian Multivariate Regression was used to model mutation signature
contributions and multivariate genomic relationships.

Key features:

-   Probabilistic estimation of regression parameters
-   Models multiple genomic variables simultaneously
-   Accounts for uncertainty in mutation signature estimation
-   Provides interpretable estimates of mutational processes

The Saddlepoint Signature method was used to efficiently perform
Bayesian inference.\
AutoIt scripts in this repository were used to automate execution of
Saddlepoint Signature.

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

The `Data/` directory contains synthetic survival datasets generated to
evaluate modelling approaches under controlled conditions. These
datasets were designed to reflect key statistical challenges encountered
in cancer survival and genomic data analysis.

The datasets vary across several important factors:

### Dimensionality

Dimensionality refers to the number of predictor variables relative to
the number of samples.

Cancer genomic datasets often contain many predictors, which can make
modelling difficult. These synthetic datasets allow evaluation of model
performance under different dimensional settings.

------------------------------------------------------------------------

### Noise

Noise refers to variability in predictor variables that does not contain
meaningful information about survival outcomes.

Noise simulates biological variability and irrelevant features commonly
present in real datasets. This allows evaluation of model robustness.

------------------------------------------------------------------------

### Censoring

Censoring occurs when the exact survival time is not observed for all
individuals, such as when a patient is still alive at the end of the
study period.

Censoring is a fundamental characteristic of survival data, and these
synthetic datasets simulate realistic censoring scenarios.

------------------------------------------------------------------------

### Negative control variables

Negative control variables are predictors intentionally generated to
have no true relationship with survival outcome.

These variables allow evaluation of whether models can correctly
distinguish between informative and irrelevant predictors.

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
