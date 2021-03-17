#' Random Forest Cross-Validation
#'
#' This function implements \code{randomForest} to perform cross-validation.
#'
#' @param k Number of folds.
#'
#' @return A numeric with the cross-validation error
#'
#' @importFrom stats na.omit predict
#' @importFrom dplyr filter
#' @importFrom dplyr %>%
#' @importFrom randomForest randomForest
#'
#' @examples
#' my_rf_cv(2)
#' my_rf_cv(5)
#' my_rf_cv(10)
#'
#' @export
my_rf_cv <- function(k) {
  if (is.numeric(k) == FALSE) {
    stop("input k is non-numeric")
  }
  # omit NAs in penguins
  omit_penguins <- na.omit(my_penguins)
  # assign observations to folds 1,...,k with equal probability
  fold <- sample(rep(1:k, length = nrow(omit_penguins)))
  data <- cbind(omit_penguins[3:6], fold)
  pred_mat <- matrix(NA, nrow(data), 1)
  for (i in 1:k) {
    data_train <- data %>% filter(fold != i)
    data_test <- data %>% filter(fold == i)
    model <- randomForest(body_mass_g ~ bill_length_mm + bill_depth_mm + flipper_length_mm, data = data_train, ntree = 100)
    pred_mat[fold == i, 1] <- predict(model, data_test[, 1:3])
  }
  cv_err <- mean((pred_mat - data$body_mass_g)^2)
  return(cv_err)
}
