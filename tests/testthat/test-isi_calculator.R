# File: test-isi_calculator.R

# Load necessary libraries
library(testthat)
library(dplyr)  # If you're using dplyr functions like mutate, select, etc.
library(InsuSensCalc)

# Load your package (replace 'InsulinMetrics' with the name of your package)
#library(InsulinMetrics)

# Load the example_data 
data(example_data.rda)


# Start defining your tests
test_that("isi_calculator function works correctly", {
  # Example test cases
  
  # Test 1: Ensure function works with a dataframe input
  data <- data.frame(example_data)  
  result <- isi_calculator(data)
  expect_true(is.data.frame(result), "Result should be a dataframe")
  
  # Test 2: Test specific output for fasting category
  data <- data.frame(example_data)  
  result <- isi_calculator(data, category = "fasting")
 
  
  # Test 3: Test specific output for ogtt category
  data <- data.frame(example_data)  
  result <- isi_calculator(data, category = "ogtt")

  
  # Test 4: Test specific output for adipo category
  data <- data.frame(example_data)  
  result <- isi_calculator(data, category = "adipo")
 
  
  # Test 4: Test specific output for tracer_dxa category
  data <- data.frame(example_data)  
  result <- isi_calculator(data, category = "tracer_dxa")
  
})
