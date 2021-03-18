#' k-Nearest Neighbors Cross-Validation
#'
#' This function implements \code{knn} to perform cross-validation.
#'
#' @param train Input data frame.
#' @param cl True class value of your training data.
#' @param k_nn Integer representing the number of neighbors.
#' @param k_cv Integer representing the number of folds.
#' @keywords cross-validation
#'
#' @return A list with objects:
#'   \item{class}{A vector of the predicted class for all observations.}
#'   \item{cv_err}{A numeric with the cross-validation misclassification error.}
#'
#' @importFrom class knn
#'
#' @examples
#' data <- na.omit(my_penguins)
#' data$species <- as.numeric(factor(data$species,
#'   levels = c('Adelie', 'Chinstrap', 'Gentoo'), labels = c(1, 2, 3)))
#' my_knn_cv(data[, 3:6], data[, 1], 1, 5)
#' my_knn_cv(data[, 3:6], data[, 1], 5, 5)
#'
#' @export
my_knn_cv <- function(train, cl, k_nn, k_cv) {
  if (is.data.frame(train) == FALSE) {
    stop("Input train is not a dataframe")
  }
  if (!is.numeric(c(k_nn, k_cv))) {
    stop("k_nn and k_cv are non-numeric")
  }
  # assign observations to folds 1,...,k with equal probability
  fold <- sample(rep(1:k_cv, length = nrow(train)))
  data <- cbind(train, cl, fold)
  # empty vector to store output of knn(), training and test data
  pred_class <- vector()
  # initialize a numeric vector to store misclassification rate per iteration
  cv_rate <- numeric()
  # initialize a numeric vector to store training error per iteration
  train_err <- numeric()

  for (i in 1:k_cv) {
    # training and test data
    data_train <- data %>% filter(fold != i)
    data_test <- data %>% filter(fold == i)

    # training and test cl
    cl_train <- data_train[, 5]
    cl_test <- data_test[, 5]

    # predict training data base on training data
    prediction_train <- knn(data_train[, 1:4], data_train[, 1:4], cl_train, k_nn)
    # predict test data base on training data
    prediction_test <- knn(data_train[, 1:4], data_test[, 1:4], cl_train, k_nn)

    # save the predicted test data to the vector of predicted class
    pred_class[fold == i] <- prediction_test
    # calculate misclassification rate
    cv_rate[i] <- mean(prediction_test != unlist(cl_test))
    # calculate training error
    train_err[i] <- mean(prediction_train != unlist(cl_train))
  }
  # store class as the output of knn() with full data
  class <- cbind(data[, 1:4], as.data.frame(pred_class))
  # compute misclassification rate
  cv_rate <- sum(cv_rate) / k_cv
  # compute misclassification error
  cv_err <- mean((pred_class - data$species)^2)
  # compute training error
  train_err <- sum(train_err) / k_cv
  # return a list with objects
  return(list(as.vector(class), cv_err, train_err))
}
