# File: test-isi_values.R
# Numeric regression tests pinning the formula corrections made in 0.1.0 and a
# spread of unchanged indices. Values are computed from the bundled example_data.

library(testthat)
library(InsuSensCalc)

data(example_data)

res <- suppressMessages(
  isi_calculator(example_data,
                 category = c("fasting", "ogtt", "adipo", "tracer_dxa"))
)

test_that("corrected indices match expected reference values (row 1)", {
  expect_equal(res$VAI_Men_inv[1],   -1.980838, tolerance = 1e-5)
  expect_equal(res$VAI_Women_inv[1], -3.021989, tolerance = 1e-5)
  expect_equal(res$LAP_Men_inv[1],   -45,       tolerance = 1e-6)
  expect_equal(res$LAP_Women_inv[1], -57.6,     tolerance = 1e-6)
  expect_equal(res$McAuley_index[1],  11.074265, tolerance = 1e-5)
  expect_equal(res$Avignon_Sim[1],    8.988255,  tolerance = 1e-5)
  expect_equal(res$Matsuda_ISI[1],    43.050691, tolerance = 1e-5)
  expect_equal(res$BigttSi[1],        13.548285, tolerance = 1e-5)
})

test_that("VAI uses the grouped denominator and mmol/L lipids (Amato 2010)", {
  r <- example_data[1, ]
  expected_men <- -((r$waist / (39.68 + 1.88 * r$bmi)) *
                      (r$TG / 1.03) * (1.31 / r$HDL_c))
  expected_women <- -((r$waist / (36.58 + 1.89 * r$bmi)) *
                        (r$TG / 0.81) * (1.52 / r$HDL_c))
  expect_equal(res$VAI_Men_inv[1],   expected_men,   tolerance = 1e-9)
  expect_equal(res$VAI_Women_inv[1], expected_women, tolerance = 1e-9)
})

test_that("both HOMA-IR columns equal the standard HOMA-IR (mmol/L)", {
  r <- example_data[1, ]
  expect_equal(res$Homa_IR_inv[1], -((r$G0 * (r$I0 / 6)) / 22.5), tolerance = 1e-9)
  expect_true("HOMA_IR_rev_inv" %in% names(res))
  expect_equal(res$Homa_IR_inv, res$HOMA_IR_rev_inv, tolerance = 1e-9)
})

test_that("LAP and McAuley use triglycerides in mmol/L", {
  r <- example_data[1, ]
  expect_equal(res$LAP_Men_inv[1],   -((r$waist - 65) * r$TG), tolerance = 1e-9)
  expect_equal(res$LAP_Women_inv[1], -((r$waist - 58) * r$TG), tolerance = 1e-9)
  expect_equal(res$McAuley_index[1],
               exp(2.63 - 0.28 * log(r$I0 / 6) - 0.31 * log(r$TG)),
               tolerance = 1e-9)
})

test_that("Avignon_Sim is the data-driven weighted mean of Si0 and Si120 (Suleman 2024 Table 2)", {
  w <- mean(res$Avignon_Si120, na.rm = TRUE) / mean(res$Avignon_Si0, na.rm = TRUE)
  expect_equal(res$Avignon_Sim,
               (w * res$Avignon_Si0 + res$Avignon_Si120) / 2,
               tolerance = 1e-9)
})

test_that("a spread of unchanged indices remain stable (row 1)", {
  expect_equal(res$Fasting_inv[1],  -1.166667,  tolerance = 1e-5)
  expect_equal(res$Raynaud[1],       34.285714, tolerance = 1e-5)
  expect_equal(res$Homa_IR_inv[1],  -0.259259,  tolerance = 1e-5)
  expect_equal(res$Quicki[1],        0.214871,  tolerance = 1e-5)
  expect_equal(res$Isi_120[1],       8.547009,  tolerance = 1e-5)
  expect_equal(res$LIRI_inv[1],     -0.822550,  tolerance = 1e-5)
  expect_equal(res$Lipo_inv[1],     -0.583333,  tolerance = 1e-5)
  expect_equal(res$ATIRI_inv[1],    -0.233333,  tolerance = 1e-5)
})
