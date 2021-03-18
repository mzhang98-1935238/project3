# within test-my_knn_cv.R
test_that("non-dataframe input throws error", {
  data <- na.omit(my_penguins)
  data$species <- as.numeric(
    factor(data$species,levels = c('Adelie', 'Chinstrap', 'Gentoo'),
           labels = c(1, 2, 3)))
  expect_error(my_knn_cv(1, data[, 1], 1, 5))
  expect_error(my_knn_cv(data[, 3:6], data[, 1], x, 5))
})
