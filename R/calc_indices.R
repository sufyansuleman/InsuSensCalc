
# Load necessary libraries
library(dplyr)
library(magrittr)
library(tibble) # Make sure to load the tibble package
library(tidyr)



#' Insulin Sensitivity Indices Calculator
#'
#' Calculates surrogate insulin sensitivity indices based on fasting, OGTT, and lipid (adipo) values values.
#'
#' @param data A dataframe with variables for calculating indices.
#' The variables include measurements of insulin and glucose at fasting (0 minutes), 30 minutes, and 120 minutes after an oral glucose tolerance test, along with triglycerides, HDL cholesterol, and other necessary parameters.
#' The variable names in the input dataframe should match those specified in the documentation (see `?example_data`) to ensure accurate index calculations. If the names differ, the function will return an error.
#' If your dataframe is missing a variable required for a specific index calculation, the function will not compute any indices for that category.
#' To address this, you can add a column for the missing variable filled with simulated values or "NA" to allow the calculation of other indices.
#' If a variable column exists but contains missing values, the 'isi_calculator()' function will internally set these values to "NA" and proceed to calculate the remaining indices.
#' It will return "NA" for any index that required the missing variable.
#' 
#' @param category Specify categories of indices to calculate through a character vector.
#' If your data includes only fasting insulin and glucose measurements, use the "fasting" category.
#' For calculations involving Oral Glucose Tolerance Test (OGTT) values, select the "ogtt" category; if 30-minute values are absent, the function will compute indices using only the 0 and 120-minute measurements.
#' To incorporate lipid measurements such as triglycerides (TG), free fatty acids (FFA), and HDL cholesterol (HDL-C), choose the "adipo" category.
#' Both the "ogtt" and "adipo" categories also require anthropometric data, including age, sex, weight, body mass index (BMI), and waist circumference.
#' To calculate indices across all categories, either leave the argument empty or specify a list of desired categories, for example, `c("fasting", "ogtt", "adipo", "tracer_dxa")`.
#' 
#' @details
#' It requires specific columns in the data for each category:
#' - \code{fasting}: \code{"G0", "I0"}
#' - \code{ogtt}: \code{"G0", "I0", "G120", "I120", "G30", "I30", "age", "sex", "bmi", "weight"}
#' - \code{adipo}: \code{"G0", "I0", "G120", "I120", "G30", "I30", "age", "sex", "bmi", "weight", "TG", "HDL_c", "FFA", "waist"}
#' - \code{tracer_dxa}: This category includes all of the columns required for \code{adipo} 
#'   plus specific tracer and DXA measures: \code{"rate_palmitate", "rate_glycerol", "fat_mass"}.
#'   Ensure that the data frame contains these columns when selecting this category for accurate calculation.
#'   
#' It also performs the following unit conversions as part of the calculations:
#' - Glucose: Converts from mmol/L to mg/dL using the formula `value * 18`.
#' - Insulin: Converts from pmol/L to µU/ml using the formula `value / 6`.
#' - Triglycerides: Converts from mmol/L to mg/dL using the formula `value * 88`.
#' - HDL cholesterol: Converts from mmol/L to mg/dL using the formula `value * 38`.
#' 
#' Additionally, for the calculation of Belfiore_inv_FFA, the function converts 
#' Free Fatty Acids (FFA) values to Area Under Curve (AUC) as part of the preprocessing.
#'
#' Supported options for \code{category} are "fasting", "ogtt", "adipo", and "tracer_dxa". 
#' Specific indices calculated for each category are detailed within each category section.
#'
#' \describe{
#'   \item{\strong{fasting}:}{
#'     Indices based on \strong{fasting} measurements.
#'     \describe{
#'       \item{- \strong{Fasting Insulin}: Inversed to represent IS}{}
#'       \item{- \strong{Raynaud Index}: An IS index}{}
#'       \item{- \strong{HOMA-IR_inv}: Inversed to represent IS}{}
#'       \item{- \strong{FIRI}: Fasting Insulin Resistance Index: Inversed to represent IS}{}
#'       \item{- \strong{QUICKI}: Quantitative Insulin Sensitivity Check Index: IS index}{}
#'       \item{- \strong{Belfiore basal}:IS index}{}
#'       \item{- \strong{Insulin to Glucose Ratio}: Inversed to represent IS}{}
#'       \item{- \strong{Insulin Sensitivity Index basal}: IS index}{}
#'       \item{- \strong{Bennett Index}: An IS index}{}
#'       \item{- \strong{HOMA-IR-inv (Revised)}: Revised HOMA-IR, Inversed to represent IS Index}{}
#'     }
#'   }
#'   \item{\strong{ogtt}:}{
#'     Indices based on \strong{OGTT} measurements.
#'     \describe{
#'       \item{- \strong{Insulin Sensitivity Index}: IS at 120 min}{}
#'       \item{- \strong{Cederholm Index}: Insulin sensitivity based on the OGTT}{}
#'       \item{- \strong{Gutt Index}: Insulin sensitivity based on the OGTT}{}
#'       \item{- \strong{Matsuda ISI AUC }: Based on AUC for glucose and insulin at 0, 30, 120 minutes}{}
#'       \item{- \strong{Matsuda ISI}: Based on row means for glucose and insulin at 0, 30, 120 minutes}{}
#'       \item{- \strong{IG_ratio_120_inv}: Insulin to Glucose Ratio at 120: Inversed to represent IS}{}
#'       \item{- \strong{Avignon_Si0}: Avignon Index at 0 min}{}
#'       \item{- \strong{Avignon_Si120}: Avignon Index at 120 min}{}
#'       \item{- \strong{Avignon_Sim}: Avignon Index mean of the two avignon indices}{}
#'       \item{- \strong{Modified_stumvoll}: Modified Stumvoll Index}{}
#'       \item{- \strong{Stumvoll_Demographics}: Stumvoll Index with Demographics, age and bmi }{}
#'       \item{- \strong{Glu_Auc_Mean}: Mean Glucose AUC, not really intermediate product}{}
#'       \item{- \strong{Insu_Auc_Mean}: Mean Insulin AUC, not really intermediate product}{}
#'       \item{- \strong{BigttSI}:An IS index}{}
#'       \item{- \strong{Ifc_inv}: Insulin fold change, Inversed to represent IS}{}
#'       \item{- \strong{HIRI_inv}: Hepatic Insulin Resistance Index: Inversed to represent IS}{}
#'       \item{- \strong{Belfiore_ISI_gly}:IS index based on OGTT}{}
#'     }
#'   }
#'   \item{\strong{adipo}:}{
#'     Indices based on \strong{adipose} tissue measurements.
#'     \describe{
#'       \item{- \strong{Revised_QUICKI}: Revised QUICK Index}{}
#'       \item{- \strong{VAI_Men_inv}: Visceral Adiposity Index for Men: Inversed to represent IS}{}
#'       \item{- \strong{VAI_Women_inv}: Visceral Adiposity Index for Women: Inversed to represent IS}{}
#'       \item{- \strong{TG_HDL_C_inv}: TG to HDL-C ratio: Inversed to represent IS}{}
#'       \item{- \strong{TyG_inv}: Triglyceride based Index: Inversed to represent IS}{}
#'       \item{- \strong{LAP_Men_inv}: Lipid Accumulation Product for Men: Inversed to represent IS}{}
#'       \item{- \strong{LAP_Women_inv}: Lipid Accumulation Product for Women: Inversed to represent IS}{}
#'       \item{- \strong{McAuley_index}: McAuley Index}{}
#'       \item{- \strong{Adipo_inv}: Adipose Insulin Resistance Index: Inversed to represent IS}{}
#'       \item{- \strong{Belfiore_inv_FFA}: Belfiore Index with FFA: Inversed to represent IS}{}
#'     }
#'   }
#'   \item{\strong{tracer_dxa}:}{
#'     Special indices involving \strong{tracer and DXA} measurements.
#'     \describe{
#'       \item{- \strong{LIRI_inv}: Liver Insulin Resistance Index: Inversed to represent IS}{}
#'       \item{- \strong{Lipo_inv}: Lipolysis Index: Inversed to represent IS}{}
#'       \item{- \strong{ATIRI_inv}: Adipose Tissue Insulin Resistance Index: Inversed to represent IS}{}
#'     }
#'   }
#' }
#' The calculation of most indices follows established formulas documented in the references, with units and other details conforming to the standards set forth in the literature. Although not all original references are explicitly provided, they were consulted individually for each index calculation.
#' 
#' References:
#' \itemize{
#'   \item Gastaldelli (2022). <doi.org/10.1002/oby.23503>
#'   \item Lorenzo (2010). <doi.org/10.1210/jc.2010-1144>
#' }

#' @keywords internal


#' @return This function returns a dataframe with Insulin Sensitivity indices calculated for the chosen categories.
#' The output values are raw and have not undergone any normalization or transformation.
#' For subsequent analyses, particularly statistical testing and visualization, it's advisable to normalize these values due to their varying scales.
#'
#' @import dplyr
#' @import magrittr
#' @import tibble
#'
#' @examples
#' data(example_data)
#' # Example usage of the isi_calculator function
#' # Run the isi_calculator function with the sample data
#' # run for each category separately
#' result <- isi_calculator(example_data, category = "fasting")
#' result <- isi_calculator(example_data, category = "ogtt")
#' result <- isi_calculator(example_data, category = "adipo")
#' result <- isi_calculator(example_data, category = "tracer_dxa")
#' # OR all four together if you all the required columns
#' result <- isi_calculator(example_data, category = c("adipo", "ogtt", "fasting", "tracer_dxa"))
#' # View the results
#' print(result)
#' # use ?example_data to see the sample data column names and description
#' 
#' @keywords internal
#' 
#' @export
isi_calculator <- function(data, category = c("fasting", "ogtt", "adipo", "tracer_dxa")) {
  # Ensure 'data' is a dataframe
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a dataframe.")
  }
  
  category <- tolower(category)
  
  # Determine which calculations to perform
  perform_fasting <- "fasting" %in% category
  perform_ogtt <- "ogtt" %in% category
  perform_adipo <- "adipo" %in% category
  perform_tracer_dxa <- "tracer_dxa" %in% category
  
  required_columns_fasting <- c("G0", "I0")
  required_columns_ogtt <- c("G0", "I0", "G120", "I120", "G30", "I30", "age", "sex", "bmi", "weight")
  required_columns_adipo <- c("G0", "I0", "G120", "I120", "G30", "I30", "age", "sex", "bmi", "weight", "TG", "HDL_c", "FFA", "waist")
  required_columns_tracer_dxa <- c("G0", "I0", "G120", "I120", "G30", "I30", "age", "sex", "bmi", "weight", "TG", "HDL_c", "FFA", "waist", "rate_palmitate", "rate_glycerol", "fat_mass")
  
  # Fasting-based calculations
  if (perform_fasting) {
    missing_cols_fasting <- setdiff(required_columns_fasting, names(data))
    if (length(missing_cols_fasting) > 0) {
      warning(paste("Missing columns for fasting calculations:", paste(missing_cols_fasting, collapse = ", ")))
    } else {
      message("Calculating fasting indices...")
      
      data <- data %>%
        dplyr::mutate(
          # Unit conversion
          I0_microU_ml = if_else(!is.na(I0), I0 / 6, NA_real_), # Convert pmol/L to microU/ml (pmol/L * 6 = microU/ml)
          G0_mg_dl = if_else(!is.na(G0), G0 * 18, NA_real_), # Convert mmol/L to mg/dL (mmol/L * 18 = mg/dL)
          Fasting_inv = -1*(I0_microU_ml), # Fasting Insulin Sensitivity
          Raynaud = 40 / I0_microU_ml, # Raynaud Index
          Homa_IR_inv = -1*((G0_mg_dl * I0_microU_ml) / 22.5), # HOMA-IR (Revised) IS Index
          Firi = (G0_mg_dl * I0_microU_ml) / 25, # Fasting Insulin Resistance Index which is IS
          Quicki = 1 / (log(G0_mg_dl) + log(I0_microU_ml)), # Quantitative Insulin Sensitivity Check Index
          Belfiore_basal = 2 / ((I0_microU_ml * G0) + 1), # Belfiore Index
          Ig_ratio_basal = -1*(I0_microU_ml / G0), # Insulin to Glucose Ratio converted to IS
          Isi_basal = 10000 / (G0_mg_dl * I0_microU_ml), # Insulin Sensitivity Index basal
          Bennett = 1 / (log(I0_microU_ml) * log(G0_mg_dl)), # Bennett Index
          HOMA_IR_rev_inv = -1*((I0_microU_ml * G0_mg_dl) / 405) # HOMA-IR (Revised) IS Index
        ) # 10 fasting based indices
      
    }
    
  }
  
  # OGTT-based calculations
  if (perform_ogtt) {
    missing_cols_ogtt <- setdiff(required_columns_ogtt, names(data))
    if (length(missing_cols_ogtt) > 0) {
      warning(paste("Missing columns for ogtt calculations:", paste(missing_cols_ogtt, collapse = ", ")))
    } else {
      message("Calculating ogtt indices...")
      
      data <- data %>%
        dplyr::mutate(
          I0_microU_ml = if_else(!is.na(I0), I0 / 6, NA_real_), # Convert pmol/L to microU/ml (pmol/L * 6 = microU/ml)
          G0_mg_dl = if_else(!is.na(G0), G0 * 18, NA_real_), # Convert mmol/L to mg/dL (mmol/L * 18 = mg/dL)
          I30_microU_ml = if_else(!is.na(I30), I30 / 6, NA_real_), # Convert pmol/L to microU/ml (pmol/L * 6 = microU/ml)
          G30_mg_dl = if_else(!is.na(G30), G30 * 18, NA_real_), # Convert mmol/L to mg/dL (mmol/L * 18 = mg/dL)
          I120_microU_ml = if_else(!is.na(I120), I120 / 6, NA_real_), # Convert pmol/L to microU/ml (pmol/L * 6 = microU/ml)
          G120_mg_dl = if_else(!is.na(G120), G120 * 18, NA_real_), # Convert mmol/L to mg/dL (mmol/L * 18 = mg/dL)
          I_AUC = if_else(!is.na(I0) & !is.na(I30) & !is.na(I120),
                          (1/2) * ((I30 + I0) * 30 + (I120 + I30) * 90), NA_real_), # Insulin AUC
          G_AUC = if_else(!is.na(G0) & !is.na(G30) & !is.na(G120),
                          (1/2) * ((G30 + G0) * 30 + (G120 + G30) * 90), NA_real_), # Glucose AUC
          I_mean = tidyr::replace_na(rowMeans(cbind(I0, I30, I120), na.rm = TRUE), NA_real_), # Mean insulin
          G_mean = tidyr::replace_na(rowMeans(cbind(G0, G30, G120), na.rm = TRUE), NA_real_), # Mean glucose
          
          # OGTT-based calculations here
          Isi_120 = 10000 / (G120_mg_dl * I120_microU_ml), # Insulin Sensitivity Index 120
          Ig_ratio_120 = -1*(I120_microU_ml / G120), # Insulin to Glucose Ratio converted to IS
          Cederholm_index = (75000 + (G0_mg_dl - G120_mg_dl) * 1.15 *180 * 0.19 * weight) / (120 * ((G0_mg_dl + G120_mg_dl) / 2) * log(I0_microU_ml + I120_microU_ml)),
          Gutt_index = (75000 + (G0_mg_dl - G120_mg_dl) * 0.19 * weight) / (120 * ((G0_mg_dl + G120_mg_dl) / 2) * log((I0_microU_ml + I120_microU_ml) / 2)), # Gutt Index
          Avignon_Si0 = 1e8 / ((G0_mg_dl * I0_microU_ml) * weight * 150), # Avignon Index at 0 min 
          Avignon_Si120 = 1e8 / ((G120_mg_dl * I120_microU_ml) * weight * 150), # Avignon Index at 120 min
          Avignon_Sim = (mean(c(Avignon_Si120, Avignon_Si0), na.rm = TRUE) * ((Avignon_Si0 + Avignon_Si120) / 2)), # Avignon Index mean
          Modified_stumvoll = 0.156 - (0.0000459 * I120) - (0.000321 * I0) - (0.00541 * G120), # Modified Stumvoll Index
          Stumvoll_Demographics = 0.222 - (0.00333 * bmi) - (0.0000779 * I120) - (0.000422 * age), # Stumvoll Index with Demographics
          Glu_Auc_Mean = ((15 * G0_mg_dl + 60 * G30_mg_dl + 45 * G120_mg_dl) / 120), # Mean Glucose AUC
          Insu_Auc_Mean = ((15 * I0_microU_ml + 60 * I30_microU_ml + 45 * I120_microU_ml) / 120), # Mean Insulin AUC
          Matsuda_Auc = 10000 / (sqrt(G0_mg_dl * I0_microU_ml * Glu_Auc_Mean * Insu_Auc_Mean)), # Matsuda Index
          Matsuda_ISI = 10000 / (sqrt(G0_mg_dl * I0_microU_ml * G_mean * I_mean)), # Matsuda Index
          BigttSi = exp(4.90 - (0.00402 * I0) - (0.000565 * I30) - (0.00127 * I120) - (0.152 * G0) - (0.00871 * G30) - (0.0373 * G120) - if_else(sex == 1, 0.145, 0) - (0.0376 * bmi)), # BIGTT-Si
          Ifc_inv = -1*(log(I120 / I0)), # Insulinogenic Index converted to IS
          HIRI_inv = -1 * (((G0_mg_dl + G30_mg_dl) / 100 / 2) * ((I0_microU_ml + I30_microU_ml) / 2)), # Hepatic Insulin Resistance Index converted to IS
          Belfiore_isi_gly = (2 / ((I_AUC * G_AUC) + 1))  # Belfiore ISI Glycemia
        ) # 17 OGTT based indices
    }
    
  }
  # Adipo-related calculations
  if (perform_adipo) {
    missing_cols_adipo <- setdiff(required_columns_adipo, names(data))
    if (length(missing_cols_adipo) > 0) {
      warning(paste("Missing columns for adipo calculations:", paste(missing_cols_adipo, collapse = ", ")))
    } else {
      message("Calculating adipo indices...")
      
      data <- data %>%
        dplyr::mutate(
          # Convert units for insulin and glucose measurements
          I0_microU_ml = if_else(!is.na(I0), I0 / 6, NA_real_), # Convert pmol/L to microU/ml
          G0_mg_dl = if_else(!is.na(G0), G0 * 18, NA_real_), # Convert mmol/L to mg/dL
          I30_microU_ml = if_else(!is.na(I30), I30 / 6, NA_real_),
          G30_mg_dl = if_else(!is.na(G30), G30 * 18, NA_real_),
          I120_microU_ml = if_else(!is.na(I120), I120 / 6, NA_real_),
          G120_mg_dl = if_else(!is.na(G120), G120 * 18, NA_real_),
          TG_mg_dl = if_else(!is.na(TG), TG * 88.57, NA_real_), # Convert mmol/L to mg/dL for Triglycerides
          HDL_c_mg_dl = if_else(!is.na(HDL_c), HDL_c * 38.67, NA_real_), # Convert mmol/L to mg/dL for HDL cholesterol
          
          # Area Under Curve (AUC) for Insulin and FFA
          I_AUC = if_else(!is.na(I0) & !is.na(I30) & !is.na(I120),
                          (1/2) * ((I30 + I0) * 30 + (I120 + I30) * 90), NA_real_),
          FFA_AUC = if_else(!is.na(FFA), 
                            (1/2) * ((FFA + FFA) * (120 - 0)), NA_real_), # Assuming FFA values are consistent and FFA0, FFA30, and FFA120 are not provided
          
          
          # Adipose-related calculations here
          Revised_QUICKI = 1 / (log10(I0_microU_ml) + log10(G0_mg_dl) + log10(FFA)),
          VAI_Men_inv = -1 * ((waist / 39.68 + (1.88 * bmi)) * (TG_mg_dl / 1.03) * (1.31 / HDL_c_mg_dl)),
          VAI_Women_inv = -1 * ((waist / 36.58 + (1.89 * bmi)) * (TG_mg_dl / 0.81) * (1.52 / HDL_c_mg_dl)),
          TG_HDL_C_inv = -1 * (TG_mg_dl / HDL_c_mg_dl),
          TyG_inv = -1 * (log(TG_mg_dl * G0_mg_dl / 2)),
          LAP_Men_inv = -1 * ((waist - 65) * TG_mg_dl),
          LAP_Women_inv = -1 * ((waist - 58) * TG_mg_dl),
          McAuley_index = exp(2.63 - 0.28 * log(I0_microU_ml) - 0.31 * log(TG_mg_dl)),
          Adipo_inv = -1 * (FFA * I0_microU_ml),
          Belfiore_inv_FFA = -1 * (2 / ((I_AUC * FFA_AUC) + 1))
        ) # 10 Adipo indices
      
    }
  }
  
  # Tracer and DXA realted calculations
  if (perform_tracer_dxa) {
    missing_cols_tracer_dxa <- setdiff(required_columns_tracer_dxa, names(data))
    if (length(missing_cols_tracer_dxa) > 0) {
      warning(paste("Missing columns for adipo calculations:", paste(missing_cols_tracer_dxa, collapse = ", ")))
    } else {
      message("Calculating tracer daxa indices...")
      
      data <- data %>%
        dplyr::mutate(
          # Convert units for insulin and glucose measurements
          I0_microU_ml = if_else(!is.na(I0), I0 / 6, NA_real_), # Convert pmol/L to microU/ml
          G0_mg_dl = if_else(!is.na(G0), G0 * 18, NA_real_), # Convert mmol/L to mg/dL
          I30_microU_ml = if_else(!is.na(I30), I30 / 6, NA_real_),
          G30_mg_dl = if_else(!is.na(G30), G30 * 18, NA_real_),
          I120_microU_ml = if_else(!is.na(I120), I120 / 6, NA_real_),
          G120_mg_dl = if_else(!is.na(G120), G120 * 18, NA_real_),
          TG_mg_dl = if_else(!is.na(TG), TG * 88.57, NA_real_), # Convert mmol/L to mg/dL for Triglycerides
          HDL_c_mg_dl = if_else(!is.na(HDL_c), HDL_c * 38.67, NA_real_), # Convert mmol/L to mg/dL for HDL cholesterol
          I_AUC = if_else(!is.na(I0) & !is.na(I30) & !is.na(I120),
                          (1/2) * ((I30 + I0) * 30 + (I120 + I30) * 90), NA_real_),
          FFA_AUC = if_else(!is.na(FFA), 
                            (1/2) * ((FFA + FFA) * (120 - 0)), NA_real_), # Assuming FFA values are consistent and FFA0, FFA30, and FFA120 are not provided
          FM_kg = if_else(!is.na(fat_mass), fat_mass, NA_real_), # Assuming fat_mass is already in kg
          Ra_glycerol = if_else(!is.na(rate_glycerol), rate_glycerol, NA_real_),
          Ra_palmitate = if_else(!is.na(rate_palmitate), rate_palmitate, NA_real_),
          
          
          # Adipose-related calculations here
          LIRI_inv = -1 * (-0.091 + log10((I0_microU_ml + I30_microU_ml) / 2 * 6) * 0.4 + log10(FM_kg / weight * 100) * 0.346 - log10(HDL_c_mg_dl) * 0.408 + log10(bmi) * 0.435),
          Lipo_inv = -1 * (Ra_glycerol * I0_microU_ml),
          ATIRI_inv = -1 * (Ra_palmitate * I0_microU_ml)
        ) # 3 tracer and dxa indices
    }
  }
  
  # Optionally, remove calculation columns you don't want to return
  data <- data %>%
    select(
      -any_of(c("I0_microU_ml", "G0_mg_dl", "I30_microU_ml", "G30_mg_dl", "I120_microU_ml", "G120_mg_dl", "TG_mg_dl", "HDL_c_mg_dl", "I_AUC", "G_AUC", "I_mean", "G_mean", "FFA_AUC", "FM_kg", "Ra_glycerol", "Ra_palmitate"
      )))
  
  
  return(data)
}
