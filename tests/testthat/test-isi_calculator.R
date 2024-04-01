# File: test-isi_calculator.R

# Load necessary libraries
library(testthat)
library(dplyr)  # If you're using dplyr functions like mutate, select, etc.
library(InsuSensCalc)

# Load your package (replace 'InsulinMetrics' with the name of your package)
#library(InsulinMetrics)

# Load the example_data using system.file to ensure the path is correctly located within the package
load("~/R_packages/InsuSensCalc/data/example_data.rda")


# Start defining your tests
test_that("isi_calculator function works correctly", {
  # Example test cases
  
  # Test 1: Ensure function works with a dataframe input
  data <- data.frame(example_data)  # Replace ... with appropriate test data
  result <- isi_calculator(data)
  expect_true(is.data.frame(result), "Result should be a dataframe")
  
  # Test 2: Test specific output for fasting category
  data <- data.frame(example_data)  # Replace ... with appropriate test data
  result <- isi_calculator(data, categories = "fasting")
  # Add more expect_ statements to verify specific output
  
  # Test 3: Test specific output for ogtt category
  data <- data.frame(example_data)  # Replace ... with appropriate test data
  result <- isi_calculator(data, categories = "ogtt")
  # Add more expect_ statements to verify specific output
  
  # Test 4: Test specific output for adipo category
  data <- data.frame(example_data)  # Replace ... with appropriate test data
  result <- isi_calculator(data, categories = "adipo")
  # Add more expect_ statements to verify specific output
})
