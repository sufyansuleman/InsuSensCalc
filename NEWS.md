# InsuSensCalc 0.1.0 (2026-06-09)

## Formula corrections (change computed values)

Several index formulas were verified against their original publications and
corrected. **These fixes change the numeric output of the affected indices**, so
results from 0.1.0 are not directly comparable to 0.0.1 for these columns.

- **`VAI_Men_inv` / `VAI_Women_inv`**: fixed the denominator grouping so waist is
  divided by the full `(constant + slope * BMI)` term, and switched triglycerides
  and HDL-C to mmol/L as required by the original constants (Amato 2010,
  <doi:10.2337/dc09-1825>). Previous values were substantially off.
- **`LAP_Men_inv` / `LAP_Women_inv`**: triglycerides are now used in mmol/L
  (Kahn 2005, <doi:10.1186/1471-2261-5-26>); the previous mg/dL values were
  inflated by ~88.6x.
- **`McAuley_index`**: triglycerides are now used in mmol/L (McAuley 2001,
  <doi:10.2337/diacare.24.3.460>).
- **`Avignon_Sim`**: now computed as `(0.137 * Si0 + Si120) / 2`, the weighted
  mean defined by Avignon 1999 (<doi:10.1038/sj.ijo.0800864>). The previous
  expression returned an invalid, dataset-wide collapsed value.
- **`Matsuda_ISI`**: the mean glucose and insulin terms are now computed in the
  same converted units (mg/dL and µU/mL) as the fasting terms, fixing a
  unit mismatch (Matsuda & DeFronzo 1999, <doi:10.2337/diacare.22.9.1462>).
- **`Avignon_Sim`**: now uses the data-driven weighting `w = mean(Si120) /
  mean(Si0)`, i.e. `(w * Si0 + Si120) / 2`, matching the source publication's
  Table 2. Note this index therefore depends on the full sample.
- **`BigttSi`**: corrected the 30-minute insulin coefficient from `0.000565` to
  `0.000556` (Hansen 2007, <doi:10.2337/dc06-1240>).
- **`Homa_IR_inv`**: now uses glucose in mmol/L, giving the standard HOMA-IR
  (Matthews 1985, <doi:10.1007/BF00280883>); the previous mg/dL values were
  ~18x too large.

## Maintenance

- `Homa_IR_inv` and `HOMA_IR_rev_inv` are both retained as separate columns;
  after the units fix they return the same standard HOMA-IR value (computed via
  mmol/L / 22.5 and mg/dL / 405 respectively).
- Removed `library()` calls from package source; imports are handled via
  NAMESPACE. Dropped the unused `tidyr` dependency.
- Documentation now states the correct triglyceride (`* 88.57`) and HDL-C
  (`* 38.67`) unit-conversion factors, and corrects a reversed insulin
  conversion comment.
- Added a citation for the package's source publication (Suleman 2024,
  <doi:10.1210/clinem/dgae275>) to the `DESCRIPTION` field.

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

- Utilizes **lipid** (adipo) measurements like triglycerides, free fatty acids and HDL cholesterol for indices such as:
  - Visceral Adiposity Index (VAI) for Men and Women (inversed)
  - Lipid Accumulation Product (LAP)
  - ATIRI_inv
  
- Utilizes **tracer and dxa based data** measurements like rate of glycerol, palmitate and dxa based fat mass for indices such as:
  - LIRI_inv
  - Lipo_inv
  - TyG Index (inversed)
  - And other adipose-related indices.

### Flexible Input

- The function accepts a dataframe containing the necessary variables for calculation and a character vector specifying the categories of indices to calculate (`"fasting"`, `"ogtt"`, `"adipo"`), allowing users to customize the scope of their analysis.

### User-friendly

- Includes comprehensive documentation and examples to facilitate easy use and integration into research workflows.

This release lays the foundation for robust and flexible insulin sensitivity analysis within the R ecosystem, catering to a wide array of research needs in metabolic health.
