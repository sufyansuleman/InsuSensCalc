# File: test-isi_calculator.R

library(testthat)
library(InsuSensCalc)

data(example_data)

# Start defining your tests

test_that("isi_calculator returns a dataframe for default categories", {
  result <- isi_calculator(example_data)
  expect_true(is.data.frame(result))
  expect_gt(nrow(result), 0)
})

test_that("fasting category produces expected fasting index columns", {
  result <- isi_calculator(example_data, category = "fasting")
  expect_true(all(c("Fasting_inv", "Homa_IR_inv", "Quicki", "Isi_basal") %in% names(result)))
})

test_that("ogtt category produces expected OGTT index columns", {
  result <- isi_calculator(example_data, category = "ogtt")
  expect_true(all(c("Isi_120", "Gutt_index", "Matsuda_ISI", "Belfiore_isi_gly") %in% names(result)))
})

test_that("adipo and tracer_dxa categories produce adipose and tracer indices", {
  result_adipo <- isi_calculator(example_data, category = "adipo")
  expect_true(all(c("Revised_QUICKI", "TyG_inv", "McAuley_index") %in% names(result_adipo)))

  result_tracer <- isi_calculator(example_data, category = "tracer_dxa")
  expect_true(all(c("LIRI_inv", "Lipo_inv", "ATIRI_inv") %in% names(result_tracer)))
})
