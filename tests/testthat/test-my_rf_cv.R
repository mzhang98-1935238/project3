# within test-my_rf_cv.R
test_that("my_rf_cv works mathematically", {
  expect_equal(my_rf_cv(2), 125000, tolerance = 20000)
})
test_that("non-numeric input throws error", {
  expect_error(my_rf_cv("2"))
})
