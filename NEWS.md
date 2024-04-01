# InsuSensCalc 0.0.1 (2024-04-02)

- Initial release of the `InsuSensCalc`, featuring the `isi_calculator` function.

## New Features

- **`isi_calculator` Function**: A comprehensive tool for calculating surrogate insulin sensitivity indices based on various measurements, including fasting, Oral Glucose Tolerance Test (OGTT), and lipid (adipose) values. This function supports a wide range of indices calculations, making it a versatile tool for research in metabolic health and diabetes.

### Capabilities

- Calculates indices using **fasting** glucose and insulin levels, including:
  - Fasting Insulin Sensitivity
  - HOMA-IR (and its inverse)
  - QUICKI
  - And several other fasting-related indices.

- Incorporates **OGTT** (0 min, 30 min, 120 min post-glucose load) values for:
  - Gutt Index
  - Matsuda Index
  - Insulin Sensitivity Index at 120 min
  - And more, adapting calculations based on available time points.

- Utilizes **lipid** (adipo) measurements like triglycerides and HDL cholesterol for indices such as:
  - Visceral Adiposity Index (VAI) for Men and Women (inversed)
  - Lipid Accumulation Product (LAP)
  - TyG Index (inversed)
  - And other adipose-related indices.

### Flexible Input

- The function accepts a dataframe containing the necessary variables for calculation and a character vector specifying the categories of indices to calculate (`"fasting"`, `"ogtt"`, `"adipo"`), allowing users to customize the scope of their analysis.

### User-friendly

- Includes comprehensive documentation and examples to facilitate easy use and integration into research workflows.

This release lays the foundation for robust and flexible insulin sensitivity analysis within the R ecosystem, catering to a wide array of research needs in metabolic health.
