# File: test-isi_na.R
# Tests for missing-column and missing-value (NA) handling.

library(testthat)
library(InsuSensCalc)

data(example_data)

test_that("a missing required column warns and skips that category", {
  d <- example_data
  d$I0 <- NULL
  expect_warning(
    res <- isi_calculator(d, category = "fasting"),
    "Missing columns for fasting"
  )
  # category skipped: none of its indices are added
  expect_false("Fasting_inv" %in% names(res))
  expect_false("Quicki" %in% names(res))
})

test_that("an NA input yields NA only for the affected row's index", {
  d <- example_data
  d$I0[1] <- NA
  res <- suppressMessages(isi_calculator(d, category = "fasting"))
  expect_true(is.na(res$Fasting_inv[1]))
  expect_false(is.na(res$Fasting_inv[2]))
  # other rows still computed for an index that uses I0
  expect_false(is.na(res$Quicki[2]))
})

test_that("NA triglycerides nullify only TG-dependent adipo indices", {
  d <- example_data
  d$TG[1] <- NA
  res <- suppressMessages(isi_calculator(d, category = "adipo"))
  expect_true(is.na(res$LAP_Men_inv[1]))
  expect_true(is.na(res$McAuley_index[1]))
  expect_true(is.na(res$TyG_inv[1]))
  # Revised_QUICKI does not use TG, so it is unaffected
  expect_false(is.na(res$Revised_QUICKI[1]))
})
