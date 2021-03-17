#' Linear Model
#'
#' This function fits a linear model.
#'
#' @param formula A formula class object, similar to \code{lm()}.
#' @param data Input data frame.
#'
#' @return A table similar to the coefficent table from summary() with
#'   rows for each coefficient (including the Intercept) and columns for the
#'   estimate, standard error, t value, and \code{Pr(>|t|)}
#'
#' @importFrom stats model.frame model.matrix model.response pt
#'
#' @examples
#' my_lm(lifeExp ~ gdpPercap + continent, my_gapminder)
#'
#' @export
my_lm <- function(formula, data) {
  X <- model.matrix(formula, data) # model matrix
  model_frame <- model.frame(formula, data) # model frame
  Y <- model.response(model_frame) # model response
  beta_hat <- solve((t(X) %*% X)) %*% t(X) %*% Y # linear regression coefficients
  df <- nrow(data) - nrow(beta_hat) # degree of freedom
  var <- sum((Y - X %*% beta_hat)^2/df) # sample variance
  se <- diag(sqrt(abs(var * solve(t(X) %*% X)))) # standard error of coefficients
  t_val <- (beta_hat - 0)/se # test statistic
  p_val <- 2 * pt(abs(t_val), df, lower.tail = FALSE) # area under t distribution curve, *2 as distribution is symmetric
  table <- cbind(beta_hat, as.matrix(se), t_val, p_val)
  colnames(table) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  return (table)
}
