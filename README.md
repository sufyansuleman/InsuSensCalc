# InsuSensCalc

<!-- badges: start -->
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/InsuSensCalc)](https://cran.r-project.org/package=InsuSensCalc)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/grand-total/InsuSensCalc)](https://cran.r-project.org/package=InsuSensCalc)
[![GitHub release downloads](https://img.shields.io/github/downloads/sufyansuleman/InsuSensCalc/total.svg)](https://github.com/sufyansuleman/InsuSensCalc/releases)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

## Overview

InsuSensCalc provides reproducible calculators for a wide range of insulin sensitivity indices derived from common clinical and research measurements (fasting, OGTT, adipose tissue, tracer studies and DXA). It is designed to be:

- **Reliable**: implements validated formulas used in published research.
- **Lightweight**: minimal dependencies and straightforward installation.
- **Reproducible**: consistent naming and outputs so results can be compared across studies.

## Who should use this package?

- Clinical and translational researchers working with metabolic or endocrine data.
- Data analysts processing OGTT, fasting, tracer or DXA datasets.
- Anyone needing standardised insulin sensitivity index calculations in R.

## Quick start

Install from CRAN or GitHub and run a quick example:

```r
install.packages("InsuSensCalc") # or remotes::install_github("sufyansuleman/InsuSensCalc")
library(InsuSensCalc)
data(example_data)
res <- isi_calculator(example_data, category = c("fasting", "ogtt"))
head(res)
```

## Introduction

`InsuSensCalc` is an R package for computing insulin sensitivity indices from fasting, OGTT, adipose tissue, tracer, and DXA data. It provides a single, reproducible workflow to calculate multiple established insulin sensitivity measures used in metabolic research.

## About this package

This package was developed as part of the study available at https://pubmed.ncbi.nlm.nih.gov/38635292/. The package was used to support the analyses reported in that work, and it is intended for researchers who need a consistent, easy-to-use calculator for insulin sensitivity indices.

## Key features

- Calculates validated insulin sensitivity indices from fasting, OGTT, adipose, tracer, and DXA inputs.
- Allows category-specific or full multi-category calculation in a single call.
- Uses standard clinical and research variable names to support reproducible workflows.
- Includes a citation section so users can properly acknowledge the package and underlying study.

> Note: This README is the package landing page. No additional pkgdown website is included at this time.

## Setup

### Prerequisites

Load the necessary R libraries:

```{r setup-libraries}
library(dplyr)
library(magrittr)
library(tibble)
library(tidyr)
```

If missing any, install via `install.packages("package_name")`.

### Installing InsuSensCalc

Install from CRAN or GitHub:

```{r install-cran}
install.packages("InsuSensCalc")
```

Or directly from GitHub:

```{r install-github}
remotes::install_github("sufyansuleman/InsuSensCalc")
```

## Understanding the `isi_calculator` Function

### Function Overview

`isi_calculator` takes a dataframe as input, calculating insulin sensitivity indices for:

### Input Parameters

- `data`: Your dataframe.
- `category`: Indices categories to calculate. Options: `"fasting"`, `"ogtt"`, `"adipo"`, `"tracer_dxa"`.


## What it calculates

The `isi_calculator` function computes various insulin sensitivity (IS) indices across different data categories.

### Categories and Indices


- **Fasting** (fasting)
- **OGTT** (ogtt)
- **Adipose tissue (adipo)**
- **Tracer and DXA (tracer_dxa)**

### Fasting Measurements

Indices calculated from fasting measurements are essential for assessing baseline insulin sensitivity and resistance.

- **Fasting Insulin**: Inversed to represent IS.
- **Raynaud Index**: An IS index.
- **HOMA-IR_inv**: Inversed to represent IS.
- **FIRI**: Fasting Insulin Resistance Index, inversed to represent IS.
- **QUICKI**: Quantitative Insulin Sensitivity Check Index, an IS index.
- **Belfiore basal**: An IS index.
- **Insulin to Glucose Ratio**: Inversed to represent IS.
- **Insulin Sensitivity Index basal**: An IS index.
- **Bennett Index**: An IS index.
- **HOMA-IR-inv (Revised)**: Revised HOMA-IR, inversed to represent IS Index.

### OGTT Measurements

OGTT-based indices are derived from glucose and insulin responses to an oral glucose tolerance test, offering insights into dynamic insulin sensitivity.

- **Insulin Sensitivity Index**: IS at 120 min.
- **Cederholm Index**: Insulin sensitivity based on the OGTT.
- **Gutt Index**: Insulin sensitivity based on the OGTT.
- **Matsuda ISI AUC**: Based on AUC for glucose and insulin at 0, 30, 120 minutes.
- **Matsuda ISI**: Based on row means for glucose and insulin at 0, 30, 120 minutes.
- **IG_ratio_120_inv**: Insulin to Glucose Ratio at 120, inversed to represent IS.
- **Avignon_Si0**: Avignon Index at 0 min.
- **Avignon_Si120**: Avignon Index at 120 min.
- **Avignon_Sim**: Avignon Index mean of the two Avignon indices.
- **Modified_stumvoll**: Modified Stumvoll Index.
- **Stumvoll_Demographics**: Stumvoll Index with demographics, age, and BMI.
- **Glu_Auc_Mean**: Mean Glucose AUC.
- **Insu_Auc_Mean**: Mean Insulin AUC.
- **BigttSI**: An IS index.
- **Ifc_inv**: Insulin fold change, inversed to represent IS.
- **HIRI_inv**: Hepatic Insulin Resistance Index, inversed to represent IS.
- **Belfiore_ISI_gly**: IS index based on OGTT.

### Adipose Tissue Measurements

Adipose tissue-related indices focus on the interactions between insulin sensitivity and adipose tissue metrics.

- **Revised_QUICKI**: Revised QUICK Index.
- **VAI_Men_inv**: Visceral Adiposity Index for Men, inversed to represent IS.
- **VAI_Women_inv**: Visceral Adiposity Index for Women, inversed to represent IS.
- **TG_HDL_C_inv**: TG to HDL-C ratio, inversed to represent IS.
- **TyG_inv**: Triglyceride-based Index, inversed to represent IS.
- **LAP_Men_inv**: Lipid Accumulation Product for Men, inversed to represent IS.
- **LAP_Women_inv**: Lipid Accumulation Product for Women, inversed to represent IS.
- **McAuley_index**: McAuley Index.
- **Adipo_inv**: Adipose Insulin Resistance Index, inversed to represent IS.
- **Belfiore_inv_FFA**: Belfiore Index with FFA, inversed to represent IS.

### Tracer and DXA Measurements

These specialized indices involve tracer and dual-energy X-ray absorptiometry (DXA) measurements, providing advanced assessments of insulin resistance.

- **LIRI_inv**: Liver Insulin Resistance Index, inversed to represent IS.
- **Lipo_inv**: Lipolysis Index, inversed to represent IS.
- **ATIRI_inv**: Adipose Tissue Insulin Resistance Index, inversed to represent IS.


## Data Preparation

Structure your data with the following columns and units:

- `age`: Years
- `sex`: 1 (male) or 0/2 (female)
- `I0`: Fasting insulin (pmol/L)
- `G0`: Fasting glucose (mmol/L)
- `I30`, `I120`: Insulin at 30 and 120 mins post-glucose (pmol/L)
- `G30`, `G120`: Glucose at 30 and 120 mins post-glucose (mmol/L)
- `HDL_c`: HDL cholesterol (mmol/L)
- `FFA`: Free fatty acids (mmol/L)
- `waist`: Waist circumference (cm)
- `weight`: Weight (kg)
- `bmi`: Body Mass Index (kg/m^2)
- `TG`: Triglycerides (mmol/L)
- `rate_palmitate`, `rate_glycerol`: Tracer rates (arbitrary units)
- `fat_mass`: Fat mass (kg)

Column names are case sensitive and must match exactly.

## Example Usage

Calculate indices with your formatted data:

```{r calculate-indices}
# Define or load your dataframe 'your_data'

result <- isi_calculator(your_data, "fasting")
result <- isi_calculator(your_data, "ogtt")
result <- isi_calculator(your_data, category = c("fasting", "ogtt"))

result <- isi_calculator(your_data, category = c("fasting", "ogtt", "adipo", "tracer_dxa"))
print(result)
```

## Citation

If you use `InsuSensCalc` in your research, please cite the study that motivated this package and mention the package in your methods section. This package was developed from and used in the published study:

- PubMed: https://pubmed.ncbi.nlm.nih.gov/38635292/

Suggested citation language:

> Suleman S, et al. InsuSensCalc package for insulin sensitivity index calculation. Study available at PubMed PMID 38635292. Please cite the package and the underlying study when using it in your research.

You can also use the built-in citation helper from R:

```r
citation("InsuSensCalc")
```

## Interpreting the Output

The resulting dataframe lists calculated indices. Missing data points will not have the corresponding index value. Output columns are raw, non-standardized values of the indices that may require normalization or standardization prior to any statistical analysis or visualization.

## Conclusion

The InsuSensCalc package's `isi_calculator` function offers a detailed methodology for insulin sensitivity analysis, supporting a range of data types for metabolic health research. Please cite this package and the associated study when using InsuSensCalc in your publications. The package will be updated with new indices and features in the future.
