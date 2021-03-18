# within test-my_rf_cv.R
test_that("non-numeric input throws error", {
  expect_error(my_rf_cv("2"))
})
