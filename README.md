# InsuSensCalc

## Introduction

The `isi_calculator` from the InsuSensCalc package is designed to compute surrogate insulin sensitivity indices across various categories: fasting measures, oral glucose tolerance tests (OGTT), adipose tissue metrics, and tracer and DXA assessments. This guide covers from setup and data preparation to execution and output interpretation. 

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

Or directly from a repository:

```{r install-github}
devtools::install_github("username/InsuSensCalc")
```

## Understanding the `isi_calculator` Function

### Function Overview

`isi_calculator` takes a dataframe as input, calculating insulin sensitivity indices for:

### Input Parameters

- `data`: Your dataframe.
- `category`: Indices categories to calculate. Options: `"fasting"`, `"ogtt"`, `"adipo"`, `"tracer_dxa"`.


## What it calcualtes

The `isi_calculator` function computes various insulin sensitivity (IS) indices across different data categories.

## Categories and Indices


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

Column names are case sensitiivty and must match exactly.

## Example Usage

Calculate indices with your formatted data:

```{r calculate-indices}
# Define or load your dataframe 'your_data'

result <- isi_calculator(your_data, "fasting")
result <- isi_calculator(your_data, "ogtt")
result <- isi_calculator(your_data, category = c("fasting", "ogtt"))
OR

result <- isi_calculator(your_data, category = c("fasting", "ogtt", "adipo", "tracer_dxa"))
print(result)
```

## Interpreting the Output

The resulting dataframe lists calculated indices. Missing data points will not have the corresponding index value.Output columns are raw non standardized values of the indices that may require normalization or standardaization prior to any statistiical analysis or visalization.

## Conclusion

The InsuSensCalc package's `isi_calculator` function offers a detailed methodology for insulin sensitivity analysis, supporting a range of data types for metabolic health research. The package will updated with new indices and features in the future.
