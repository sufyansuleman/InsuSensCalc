
# Load necessary libraries
library(dplyr)
library(magrittr)
library(tibble) # Make sure to load the tibble package


#' Insulin Sensitivity Indices Calculator
#'
#' Calculates surrogate insulin sensitivity indices based on fasting, OGTT, and lipid (adipo) values values.
#'
#' @param data A dataframe containing the required variables for calculation. This includes insulin
#' and glucose measurements at fasting (0 min), 30 min, and 120 min post-glucose load, as well as
#' triglycerides, HDL cholesterol, and other metabolic parameters as needed.
#'
#' @param categories A character vector specifying which categories of indices to calculate.
#' If you have only fasting insulin and glucose values choose "fasting" category
#' If you also have OGTT based vlaues choose "ogtt" category, if you have 0, 30, 120 it calcualte indices based on these values, if 30 min values are missing then it will only calcualte the indices based on 0 and 120 min values.
#' If you have lipid values like TG, FFA and HDL-C then choose "adipo" category.
#' OGTT and adipo categories also use anthropometric value ssuch as age, sex, weight, bmi and waist.
#' If you want to calculate indices for all categories then use c("fasting", "ogtt", "adipo") as input.
#' 
#' Supported options are "fasting", "ogtt", and "adipo". Specific indices calculated for each category are:
#' \itemize{
#'   \item{\strong{fasting}:}{
#'     \itemize{
#'       \item{Fasting Insulin Sensitivity: Inversed to represent IS}
#'       \item{Raynaud Index: an IS index}
#'       \item{HOMA-IR_inv: Inversed to represent IS}
#'       \item{FRI: Fasting Insulin Resistance Index: Inversed to represent IS}
#'       \item{QUICKI: Quantitative Insulin Sensitivity Check Index: IS index}
#'       \item{Belfiore basal index : IS index}
#'       \item{Insulin to Glucose Ratio: Inversed to represent IS}
#'       \item{Insulin Sensitivity Index basal :IS index}
#'       \item{Bennett Index : IS index}
#'       \item{HOMA-IR-inv (Revised) : Revised HOMA-IR inversed to represent IS Index}
#'     }
#'   }
#'   \item{\strong{ogtt}:}{
#'     \itemize{
#'       \item{Insulin Sensitivity Index at 120 min}
#'       \item{Gutt Index: Insulin sensitivity based on the OGTT}
#'       \item{Avignon Index at 0 and 120 min: A composite measure considering glucose and insulin levels}
#'       \item{Matsuda Index based on AUC for glucose and insulin at 0, 30, 120 minutes }
#'       \item{Isi_120: Insulin Sensitivity Index 120}
#'       \item{Ig_ratio_120_inv: Insulin to Glucose Ratio at 120 : Inversed to represent IS}
#'       \item{Gutt_index: Gutt Index : IS index}
#'       \item{Avignon_Si0: Avignon Index at 0 min}
#'       \item{Avignon_Si120: Avignon Index at 120 min}
#'       \item{Avignon_Sim: Avignon Index mean}
#'       \item{Modified_stumvoll: Modified Stumvoll Index}
#'       \item{Stumvoll_Demographics: Stumvoll Index with Demographics}
#'       \item{Glu_Auc_Mean: Mean Glucose AUC}
#'       \item{Insu_Auc_Mean: Mean Insulin AUC}
#'       \item{Matsuda_Auc: Matsuda Index}
#'       \item{BigttSi: BIGTT-Si}
#'       \item{Ifc_inv: Insulin fold change: Inversed to represent IS}
#'       \item{HIRI_inv: Hepatic Insulin Resistance Index: Inversed to represent IS}
#'     }
#'   }
#'   \item{\strong{adipo}:}{
#'     \itemize{
#'       \item{Revised_QUICKI: Revised QUICK Index}
#'       \item{VAI_Men_inv: Visceral Adiposity Index for Men: Inversed to represent IS}
#'       \item{VAI_Women_inv: Visceral Adiposity Index for Women : Inversed to represent IS}
#'       \item{TG_HDL_C_inv: TG to HDL-C ratio converted to IS :Inversed to represent IS}
#'       \item{TyG_inv: TyG Index converted to IS : Inversed to represent IS}
#'       \item{LAP_Men_inv: Lipid Accumulation Product for Men : Inversed to represent IS}
#'       \item{LAP_Women_inv: Lipid Accumulation Product for Women : Inversed to represent IS}
#'       \item{McAuley_index: McAuley Index}
#'       \item{LIRI_inv: Liver Insulin Resistance Index converted to IS : Inversed to represent IS}
#'       \item{Adipo_inv: Adipose Insulin Resistance Index converted to IS : Inversed to represent IS}
#'       \item{Lipo_inv: Lipolysis Index converted to IS : Inversed to represent IS}
#'       \item{ATIRI_inv: Adipose Tissue Insulin Resistance Index converted to IS : Inversed to represent IS}
#'       \item{Belfiore_inv_FFA: Belfiore Index for FFA converted to IS : Inversed to represent IS}
#'     }
#'   }
#' }
#' 
#' The calculation of most indices follows established formulas documented in the references, with units and other details conforming to the standards set forth in the literature. Although not all original references are explicitly provided, they were consulted individually for each index calculation.
#' 
#' References:
#' \itemize{
#'   \item Amalia Gastaldelli (2022). (PubMed: \url{https://onlinelibrary.wiley.com/doi/10.1002/oby.23503})
#'   \item Carlos Lorenzo (2010). (PubMed: \url{https://academic.oup.com/jcem/article/95/11/5082/2835314})
#' }

#' @keywords internal


#' @return A data frame containing the IS indices calculated for the specified category/ies.
#' The values are returned raw without any kind of normalization or transformation.
#' For further processing especially the statistical testing and visualization, it is recommended to normalize the values.
#' As most of them are on different scales
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
#' result <- isi_calculator(example_data, categories = "fasting")
#' result <- isi_calculator(example_data, categories = "ogtt")
#' result <- isi_calculator(example_data, categories = "adipo")
#' # OR all three together
#' result <- isi_calculator(example_data, categories = c("adipo", "ogtt", "fasting"))
#' # View the results
#' print(result)
#' # use ?isi_calculator to see the full documentation of the function
#' # use ?example_data to see the sample data description
#' 
#' @keywords internal
#' 
#' @export
isi_calculator <- function(data, categories = c("fasting", "ogtt", "adipo")) {
  # Ensure 'data' is a dataframe
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a dataframe.")
  }
  
  # Convert categories to lowercase to standardize the input handling
  categories <- tolower(categories)
  
  # Determine which calculations to perform
  perform_fasting <- "fasting" %in% categories
  perform_ogtt <- "ogtt" %in% categories
  perform_adipo <- "adipo" %in% categories
  
  
  # Fasting-based calculations
  if (perform_fasting ) {
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
      )
        
  }
  
  # OGTT-based calculations
  if (perform_ogtt ) {
    message("Calculating ogtt indices...")
    data <- data %>%
      dplyr::mutate(
        I0_microU_ml = if_else(!is.na(I0), I0 / 6, NA_real_), # Convert pmol/L to microU/ml (pmol/L * 6 = microU/ml)
        G0_mg_dl = if_else(!is.na(G0), G0 * 18, NA_real_), # Convert mmol/L to mg/dL (mmol/L * 18 = mg/dL)
        I30_microU_ml = if_else(!is.na(I30), I30 / 6, NA_real_), # Convert pmol/L to microU/ml (pmol/L * 6 = microU/ml)
        G30_mg_dl = if_else(!is.na(G30), G30 * 18, NA_real_), # Convert mmol/L to mg/dL (mmol/L * 18 = mg/dL)
        I120_microU_ml = if_else(!is.na(I120), I120 / 6, NA_real_), # Convert pmol/L to microU/ml (pmol/L * 6 = microU/ml)
        G120_mg_dl = if_else(!is.na(G120), G120 * 18, NA_real_), # Convert mmol/L to mg/dL (mmol/L * 18 = mg/dL)
        
        # OGTT-based calculations here
        Isi_120 = 10000 / (G120_mg_dl * I120_microU_ml), # Insulin Sensitivity Index 120
        Ig_ratio_120 = -1*(I120_microU_ml / G120), # Insulin to Glucose Ratio converted to IS
        Gutt_index = (75000 + (G0_mg_dl - G120_mg_dl) * 0.19 * weight) / (120 * ((G0_mg_dl + G120_mg_dl) / 2) * log((I0_microU_ml + I120_microU_ml) / 2)), # Gutt Index
        Avignon_Si0 = 1e8 / ((G0_mg_dl * I0_microU_ml) * weight * 150), # Avignon Index at 0 min 
        Avignon_Si120 = 1e8 / ((G120_mg_dl * I120_microU_ml) * weight * 150), # Avignon Index at 120 min
        Avignon_Sim = (mean(c(Avignon_Si120, Avignon_Si0), na.rm = TRUE) * ((Avignon_Si0 + Avignon_Si120) / 2)), # Avignon Index mean
        Modified_stumvoll = 0.156 - (0.0000459 * I120) - (0.000321 * I0) - (0.00541 * G120), # Modified Stumvoll Index
        Stumvoll_Demographics = 0.222 - (0.00333 * bmi) - (0.0000779 * I120) - (0.000422 * age), # Stumvoll Index with Demographics
        Glu_Auc_Mean = ((15 * G0_mg_dl + 60 * G30_mg_dl + 45 * G120_mg_dl) / 120), # Mean Glucose AUC
        Insu_Auc_Mean = ((15 * I0_microU_ml + 60 * I30_microU_ml + 45 * I120_microU_ml) / 120), # Mean Insulin AUC
        Matsuda_Auc = 10000 / (sqrt(G0_mg_dl * I0_microU_ml * Glu_Auc_Mean * Insu_Auc_Mean)), # Matsuda Index
        BigttSi = exp(4.90 - (0.00402 * I0) - (0.000565 * I30) - (0.00127 * I120) - (0.152 * G0) - (0.00871 * G30) - (0.0373 * G120) - if_else(sex == 1, 0.145, 0) - (0.0376 * bmi)), # BIGTT-Si
        Ifc_inv = -1*(log(I120 / I0)), # Insulinogenic Index converted to IS
        HIRI_inv = -1 * (((G0_mg_dl + G30_mg_dl) / 100 / 2) * ((I0_microU_ml + I30_microU_ml) / 2)) # Hepatic Insulin Resistance Index converted to IS
      )
  }
  
  # Adipose-related calculations
  if (perform_adipo) {
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
        
        # Additional unit conversions and calculations
        FM_kg = if_else(!is.na(fat_mass), fat_mass, NA_real_), # Assuming fat_mass is already in kg
        Ra_glycerol = if_else(!is.na(rate_glycerol), rate_glycerol, NA_real_),
        Ra_palmitate = if_else(!is.na(rate_palmitate), rate_palmitate, NA_real_),
        
        
        # Adipose-related calculations here
        # Revised QUICK Index, assuming FFA is measured in mmol/L and represents FFA0
        Revised_QUICKI = 1 / (log10(I0_microU_ml) + log10(G0_mg_dl) + log10(FFA)),
        
        # Visceral Adiposity Index for Men and Women, corrected variable names
        VAI_Men_inv = -1 * ((waist / 39.68 + (1.88 * bmi)) * (TG_mg_dl / 1.03) * (1.31 / HDL_c_mg_dl)),
        VAI_Women_inv = -1 * ((waist / 36.58 + (1.89 * bmi)) * (TG_mg_dl / 0.81) * (1.52 / HDL_c_mg_dl)),
        
        # TG to HDL-C ratio converted to Insulin Sensitivity
        TG_HDL_C_inv = -1 * (TG_mg_dl / HDL_c_mg_dl),
        
        # TyG Index converted to Insulin Sensitivity
        TyG_inv = -1 * (log(TG_mg_dl * G0_mg_dl / 2)),
        
        # Lipid Accumulation Product for Men and Women, corrected variable names
        LAP_Men_inv = -1 * ((waist - 65) * TG_mg_dl),
        LAP_Women_inv = -1 * ((waist - 58) * TG_mg_dl),
        
        # McAuley Index
        McAuley_index = exp(2.63 - 0.28 * log(I0_microU_ml) - 0.31 * log(TG_mg_dl)),
        
        # Hepatic Insulin Resistance Index converted to Insulin Sensitivity
        HIRI_inv = -1 * (((G0_mg_dl + G30_mg_dl) / 100 / 2) * ((I0_microU_ml + I30_microU_ml) / 2)),
        
        # Liver Insulin Resistance Index converted to Insulin Sensitivity
        LIRI_inv = -1 * (-0.091 + log10((I0_microU_ml + I30_microU_ml) / 2 * 6) * 0.4 + log10(FM_kg / weight * 100) * 0.346 - log10(HDL_c_mg_dl) * 0.408 + log10(bmi) * 0.435),
        
        # Adipose Insulin Resistance Index converted to Insulin Sensitivity
        Adipo_inv = -1 * (FFA * I0_microU_ml),
        
        # Lipolysis Index converted to Insulin Sensitivity, assuming Ra_glycerol represents rate_glycerol
        Lipo_inv = -1 * (Ra_glycerol * I0_microU_ml),
        
        # Adipose Tissue Insulin Resistance Index converted to Insulin Sensitivity, assuming Ra_palmitate represents rate_palmitate
        ATIRI_inv = -1 * (Ra_palmitate * I0_microU_ml),
        
        # Belfiore Index for FFA converted to Insulin Sensitivity
        Belfiore_inv_FFA = -1 * (2 / ((I_AUC * FFA_AUC) + 1))
      )

  }
  
  # Optionally, remove calculation columns you don't want to return
  data <- data %>%
    select(
      -any_of(c("I0_microU_ml", "G0_mg_dl", "I30_microU_ml", "G30_mg_dl", "I120_microU_ml", "G120_mg_dl", "TG_mg_dl", "HDL_c_mg_dl", "I_AUC", "FFA_AUC", "FM_kg", "Ra_glycerol", "Ra_palmitate"
    )))

  
  return(data)
}
