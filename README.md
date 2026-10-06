# InsuSensCalc: Insulin Sensitivity Calculator (ISC)

<!-- badges: start -->
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/InsuSensCalc)](https://cran.r-project.org/package=InsuSensCalc)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/grand-total/InsuSensCalc)](https://cran.r-project.org/package=InsuSensCalc)
[![GitHub release downloads](https://img.shields.io/github/downloads/sufyansuleman/InsuSensCalc/total.svg)](https://github.com/sufyansuleman/InsuSensCalc/releases)
[![ISAT web app](https://img.shields.io/badge/ISAT-web%20app-2a9d8f.svg)](https://sufyansuleman.github.io/isat/)
[![ISAT CLI](https://img.shields.io/badge/ISAT-command%20line-264653.svg)](https://github.com/sufyansuleman/isat/releases/latest)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![DOI](https://zenodo.org/badge/780001797.svg)](https://doi.org/10.5281/zenodo.20647742)
<!-- badges: end -->

## Overview

InsuSensCalc (**Insu**lin **Sens**itivity **Calc**ulator, or **ISC** for short) provides reproducible calculators for a wide range of insulin sensitivity indices derived from common clinical and research measurements (fasting, OGTT, adipose tissue, tracer studies and DXA). It is designed to be:

- **Reliable**: implements validated formulas used in published research.
- **Lightweight**: minimal dependencies and straightforward installation.
- **Reproducible**: consistent naming and outputs so results can be compared across studies.

## Three ways to use this calculator

The same insulin sensitivity indices are available as a browser app, a command-line tool and
this R package. Pick whichever fits how you work; you do not need R to use the first two.

| Interface | Best for | Where to get it |
|---|---|---|
| **ISAT web app** | one-off calculations, exploring a dataset, teaching. Nothing to install, runs entirely in your browser, and your data are never uploaded. Handles up to 100,000 rows per file. | <https://sufyansuleman.github.io/isat/> |
| **ISAT command line** (`isat`) | batch jobs, HPC and SLURM arrays, automated pipelines. A standalone binary for Linux x86-64 and ARM64 that needs no R, Python or Node and no root. | [Latest release](https://github.com/sufyansuleman/isat/releases/latest) |
| **InsuSensCalc** (this R package) | working inside an existing R or tidyverse analysis. | [CRAN](https://cran.r-project.org/package=InsuSensCalc) |

### Using the command-line tool

```bash
curl -L https://github.com/sufyansuleman/isat/releases/latest/download/isat-linux-x64.tar.gz | tar xz
./isat --help
```

```bash
isat check cohort.csv
isat calculate cohort.csv -o results.csv
isat merge res_*.csv -o results.csv
isat transform results.csv -o rint.csv --method rint --within-sex --input cohort.csv
```

Chunked for a SLURM array job:

```bash
isat calculate cohort.csv -o res_${SLURM_ARRAY_TASK_ID}.csv --chunk ${SLURM_ARRAY_TASK_ID}/20
```

A single-file alternative, `isat.mjs`, runs under Node.js 20 or newer. Source for both the web
app and the command-line tool is at <https://github.com/sufyansuleman/isat>
(DOI [10.5281/zenodo.23190906](https://doi.org/10.5281/zenodo.23190906)).

### How the three relate

The web app and the command-line tool are version-matched: both are ISAT 0.1.0, built from one
codebase and one calculation engine, and every build checks automatically that they produce
byte-identical output for the same input and settings.

**ISAT is the reference implementation.** This R package is its predecessor. Where the formulas
agree, ISAT reproduces InsuSensCalc 0.1.0. Four indices were deliberately corrected in ISAT to
match their published definitions and so will differ from this package: **QUICKI**, **FIRI**,
**Gutt** and the **Belfiore** indices. Every difference is listed in `validation/legacy-differences.md`
in the ISAT repository and on the web app's Methods page. If you need results that match the
published definitions of those four, use ISAT.

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

> Note: This README is the R package's landing page; there is no separate pkgdown
> documentation site for it. The ISAT web app linked above is a calculator, not
> documentation for this package.

## Setup

### Prerequisites

Load the necessary R libraries:

```r
library(dplyr)
library(magrittr)
library(tibble)
```

If missing any, install via `install.packages("package_name")`.

### Installing InsuSensCalc

Install from CRAN or GitHub:

```r
install.packages("InsuSensCalc")
```

Or directly from GitHub:

```r
remotes::install_github("sufyansuleman/InsuSensCalc")
```

## Understanding the `isi_calculator` Function

### Function Overview

The package has a single main function, `isi_calculator`, where *isi* stands for **Insulin Sensitivity Indices**. It takes a dataframe as input and returns the same data with the requested insulin sensitivity indices appended as new columns. Indices are grouped by the type of measurement they are derived from (fasting, OGTT, adipose tissue, tracer/DXA), and you choose which groups to compute with the `category` argument.

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
- **FIRI**: Fasting Insulin Resistance Index, inversed to represent IS. Glucose in mmol/L (corrected in 0.2.0).
- **QUICKI**: Quantitative Insulin Sensitivity Check Index, an IS index. Uses log base 10 as published (corrected in 0.2.0).
- **Belfiore basal**: An IS index.
- **Insulin to Glucose Ratio**: Inversed to represent IS.
- **Insulin Sensitivity Index basal**: An IS index.
- **Bennett Index**: An IS index.
- **HOMA-IR-inv (Revised)**: Standard HOMA-IR via the mg/dL ÷405 form; numerically equals HOMA-IR_inv.

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
- **Avignon_Sim**: Avignon Index, the weighted mean of the two Avignon indices (Avignon 1999).
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


## Where these indices come from

Every index in this package is taken from a published source. This section records, for each
one, which paper first defined it and which population that paper studied. The package does
not re-derive any formula.

Two points are worth stating plainly before the table.

**All of these indices were derived in Europe, the United States, New Zealand or Mexico.**
No index in this package was derived in an Asian, African, Middle Eastern or South Asian
cohort. That is the clearest and most consequential pattern here, and it is not a criticism
of the original papers; it reflects where insulin clamp studies have historically been done.

**Most of the source papers do not report the ethnic composition of their samples at all.**
Where a paper does report it, the table quotes it. Where it does not, the table says "Not
reported" rather than inferring it from the country. Four derivations are explicitly not
white European: HIRI (Mexican American), the basal insulin sensitivity index (Pima Indian),
the Gutt Index (Black and White), and LAP (multi-ethnic US). Readers who assume the whole
set is white European will be wrong about those four.

### Derivation sources

| Index or indices | Primary reference | Country | Cohort or setting | N | Ethnic composition as reported |
|---|---|---|---|---|---|
| HOMA-IR_inv, HOMA-IR-inv (Revised) | Matthews et al. 1985, *Diabetologia* 28:412 (doi:10.1007/BF00280883) | UK (Oxford) | Modelling study validated against clamp and IVGTT | Not reported in abstract | Not reported |
| Raynaud Index | Raynaud et al. 1999, *Diabetes Care* 22:1003 (research letter) | France (Montpellier) | Not described | Not reported | Not reported |
| FIRI | Duncan et al. 1995, *Lancet* 346:120 (research letter) | UK | Not a derivation cohort study | Not reported | Not reported |
| QUICKI | Katz et al. 2000, *J Clin Endocrinol Metab* 85:2402 | USA (NIH Bethesda, with Indiana validation) | Clinical research volunteers | 56 derivation, 35 validation | Not reported |
| Belfiore basal, Belfiore_ISI_gly, Belfiore_inv_FFA | Belfiore et al. 1998, *Mol Genet Metab* 63:134 | Italy (Catania) | Normal, overweight, IGT and NIDDM subjects and relatives | Not reported in abstract | Not reported |
| Insulin Sensitivity Index basal | Hanson et al. 2000, *Am J Epidemiol* 151:190 | USA (Arizona) | Gila River Indian Community, 1982 to 1997 | 1,731 | **Pima Indian (Native American)** |
| Bennett Index | **No primary reference identified.** Secondary sources attribute the formula to Anderson et al. 1995, *Am J Epidemiol* 142:724 (IRAS methodologic substudy, USA, n=55) | USA, if the attribution holds | IRAS substudy | 55 | Not reported for the substudy |
| Insulin Sensitivity Index (120 min) | **Unverified.** One review attributes it to Sluiter et al. 1976, *Diabetes* 25:245 | Netherlands (Groningen) | Not established | Not established | Not reported |
| Cederholm Index | Cederholm and Wibell 1990, *Diabetes Res Clin Pract* 10:167 | Sweden (Uppsala) | NGT, IGT and NIDDM groups, 75 g OGTT | Not reported in abstract | Not reported |
| Gutt Index | Gutt et al. 2000, *Diabetes Res Clin Pract* 47:177 | USA (Miami) | Community volunteers aged 17 to 66 | 135 | **Black and White: 22 Black / 28 White men, 31 Black / 54 White women** |
| Avignon_Si0, Avignon_Si120, Avignon_Sim | Avignon et al. 1999, *Int J Obes* 23:512 | France (Montpellier) | OGTT plus insulin-modified FSIVGTT | 47 (31 NGT, 10 IGT, 6 T2D) | Not reported |
| Modified_stumvoll, Stumvoll_Demographics | Equations: Stumvoll et al. 2001, *Diabetes Care* 24:796. Parent dataset: Stumvoll et al. 2000, *Diabetes Care* 23:295 | Germany (Tuebingen-led multicentre) | Nondiabetic volunteers, clamp plus OGTT | 104 | Not reported |
| Matsuda ISI, Matsuda ISI AUC | Matsuda and DeFronzo 1999, *Diabetes Care* 22:1462 | USA (San Antonio) | Varying glucose tolerance, OGTT plus clamp | 153 | Not reported |
| BigttSI | Hansen et al. 2007, *Diabetes Care* 30:257 | Denmark (Steno Diabetes Center) | Participants with both OGTT and IVGTT | 258 | Not reported |
| HIRI_inv | Abdul-Ghani et al. 2007, *Diabetes Care* 30:89 | USA (San Antonio) | NGT and IGT subjects, OGTT plus labelled-glucose clamp | 155 | **Mexican American, stated in the abstract** |
| Revised_QUICKI | Perseghin et al. 2001, *J Clin Endocrinol Metab* 86:4776 | Italy (Milan) | Nonobese volunteers, with and without a T2D first-degree relative | 57 | Not reported |
| VAI_Men_inv, VAI_Women_inv | Amato et al. 2010, *Diabetes Care* 33:920 | Italy (Palermo, Sicily) | AlkaMeSy primary-care database; model fitted on 315 subjects with BMI 20 to 30 | 1,498 | Not reported in the 2010 paper. The descriptor "Caucasian Sicilian" comes from the separate 2011 cut-off paper (*Lipids Health Dis* 10:183, n=1,764) |
| TG_HDL_C_inv | **No primary derivation reference.** Earliest citable use as an IR marker: McLaughlin et al. 2003, *Ann Intern Med* 139:802 | USA (Stanford) | Non-diabetic overweight volunteers, insulin suppression test | 258 | Not reported |
| TyG_inv | Simental-Mendia et al. 2008, *Metab Syndr Relat Disord* 6:299. Clamp validation: Guerrero-Romero et al. 2010, *J Clin Endocrinol Metab* 95:3347 (separate cohort, n=99) | **Mexico** (Durango; validation in Guadalajara) | Population-based cluster sample of apparently healthy adults aged 18 to 65 | 748 | Not reported |
| LAP_Men_inv, LAP_Women_inv | Kahn 2005, *BMC Cardiovasc Disord* 5:26 | USA | NHANES III (1988 to 1994). The 65 and 58 cm waist offsets come from the weighted 18 to 24 year subgroup | 9,180 | **76.0% non-Hispanic white, 10.5% non-Hispanic black, 5.3% Mexican American, 8.1% other. Race-ethnicity was explicitly modelled** |
| McAuley_index | McAuley et al. 2001, *Diabetes Care* 24:460 | New Zealand (Dunedin) | Normoglycaemic general-population volunteers, euglycaemic clamp | 178 | Not reported |
| Adipo_inv | **No primary derivation reference.** Validation: Sondergaard et al. 2017, *J Clin Endocrinol Metab* 102:1193 | USA (Mayo Clinic) | Clamp and tracer study | 25 | Not reported |
| LIRI_inv | Vangipurapu et al. 2011, *Diabetologia* 54:540 | Europe (multicentre RISC / EGIR-RISC) | Non-diabetic participants with endogenous glucose production measured by tracer and clamp | 368 | Not reported |
| ATIRI_inv | Fabbrini et al. 2012, *J Lipid Res* 53:321 | USA (St. Louis) | Obese non-diabetic subjects, clamp plus palmitate tracer | 47 | Not reported |
| Lipolysis Index (Lipo_inv) | **No primary reference identified** | Not established | Not established | Not established | Not reported |

Fasting Insulin, Insulin to Glucose Ratio, Insulin fold change, Glu_Auc_Mean and Insu_Auc_Mean
are arithmetic transformations of the input measurements rather than published indices, so no
derivation source applies.

### How these entries were checked

Each row was checked against the source record, and most against the paper's own abstract.
Full texts behind publisher paywalls could not be opened, so sample sizes and ethnic
composition are missing for several entries where that information sits only in a Methods
section. Those cells say "Not reported in abstract", which means the information was not
accessible rather than that the paper omits it.

Three rows are weaker than the rest and are labelled as such in the table: the Bennett Index
has no identifiable primary paper, the 120-minute insulin sensitivity index rests on a single
secondary attribution, and the lipolysis index has no established source. Contributions that
resolve any of these are welcome via an [issue](https://github.com/sufyansuleman/InsuSensCalc/issues).

### Known differences between this package and the original publications

Recorded here for transparency. These are implementation notes, not errors in the source
papers. Four of these points (QUICKI, FIRI, Gutt and Belfiore) are corrected in ISAT, the
reference implementation described above; see `validation/legacy-differences.md` in the
[ISAT repository](https://github.com/sufyansuleman/isat).

**Corrected in 0.2.0.** Two formulas were fixed and now differ numerically from 0.1.0.
`Quicki` previously used the natural logarithm instead of log base 10, and `Firi` was
returned as a resistance index without the inversion its documentation claimed, with
glucose in mg/dL rather than mmol/L. Existing results can be converted exactly:

```r
Quicki_0.2.0 <- Quicki_0.1.0 * 2.302585
Firi_0.2.0   <- -(Firi_0.1.0 / 18)
```

The remaining differences below are unchanged in this package.

- **LIRI** as published uses the insulin area under the curve from 0 to 120 minutes. This
  package uses the mean of the 0 and 30 minute insulin values.
- **HIRI** as published uses glucose and insulin areas under the curve from 0 to 30 minutes.
  This package uses a trapezoid-free approximation of them.
- **Belfiore** indices as published use areas normalised to a reference mean. This package
  uses unnormalised areas.
- **HOMA-IR_inv** and **HOMA-IR-inv (Revised)** are algebraically identical and will always
  return the same value.
- **Avignon_Sim** depends on the mean of the rows you pass in, so its value changes if you
  subset your data. Interpret it only within a fixed dataset.

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

```r
# Define or load your dataframe 'your_data'

result <- isi_calculator(your_data, "fasting")
result <- isi_calculator(your_data, "ogtt")
result <- isi_calculator(your_data, category = c("fasting", "ogtt"))

result <- isi_calculator(your_data, category = c("fasting", "ogtt", "adipo", "tracer_dxa"))
print(result)
```

## Citation

If you use `InsuSensCalc` in your research, please cite both the package and the underlying study.

### Citing the package

Suleman, S. (2025). InsuSensCalc: An R package for computing insulin sensitivity indices. Zenodo. <https://doi.org/10.5281/zenodo.20647742>

Or in BibTeX:

```bibtex
@software{suleman_insusensecalc_2025,
  author    = {Suleman, Sufyan},
  title     = {InsuSensCalc: An R package for computing insulin sensitivity indices},
  year      = {2025},
  publisher = {Zenodo},
  doi       = {10.5281/zenodo.20647742},
  url       = {https://doi.org/10.5281/zenodo.20647742}
}
```

### Citing the underlying study

This package was developed from and used in the published study:

- PubMed: <https://pubmed.ncbi.nlm.nih.gov/38635292/>

> Suleman S, et al. InsuSensCalc package for insulin sensitivity index calculation. Study available at PubMed PMID 38635292. Please cite the package and the underlying study when using it in your research.

You can also use the built-in citation helper from R:

```r
citation("InsuSensCalc")
```

## Interpreting the Output

The resulting dataframe lists calculated indices. Missing data points will not have the corresponding index value. Output columns are raw, non-standardized values of the indices that may require normalization or standardization prior to any statistical analysis or visualization.

## Conclusion

The InsuSensCalc package's `isi_calculator` function offers a detailed methodology for insulin sensitivity analysis, supporting a range of data types for metabolic health research. Please cite this package and the associated study when using InsuSensCalc in your publications. The package will be updated with new indices and features in the future.
