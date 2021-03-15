#' T-Test
#'
#' This function performs a one sample t-test.
#'
#' @param x The numeric vector of data.
#' @param alternative Character string specifying the alternative hypothesis.
#'   This should only accept "two.sided", "less", or "greater".
#' @param mu A number indicating the null hypothesis value of the mean.
#'
#' @return A list with elements:
#'   \item{test_stat}{The numeric test statistic.}
#'   \item{df}{The degrees of freedom.}
#'   \item{alternative}{The value of the parameter alternative.}
#'   \item{p_val}{The numeric p-value.}
#'
#' @examples
#' my_t.test(my_gapminder$lifeExp, "two.sided", 60)
#' my_t.test(my_gapminder$lifeExp, "less", 60)
#' my_t.test(my_gapminder$lifeExp, "greater", 60)
#'
#' @export
my_t.test <- function(x, alternative, mu) {
  # if alternative != "two.sided" | "less" | "greater", throw informative error
  if (!alternative %in% c("two.sided", "less", "greater")) {
    stop("specification of alternative hypothesis is unidentifiable")
  }
  se <- sd(x) / sqrt(length(x)) # standard error = sd/sqrt(n)
  # t value = (mean - mu)/se
  test_stat <- (mean(x) - mu)/se
  df <- length(x) - 1 # degree of freedom = sample size - 1
  # calculation of p value given test statistic
  if (alternative == "two.sided") {
    p_val <- 2 * pt(-abs(test_stat), df)
  } else if (alternative == "less") {
    p_val <- pt(-abs(test_stat), df, lower.tail = FALSE)
  } else {
    p_val <- pt(-abs(test_stat), df, lower.tail = TRUE)
  }
  # return a list with elements test statistic, df, parameter alternative, and p value
  list <- c(format(round(test_stat, 4), nsmall = 4), df, alternative, signif(p_val, 4))
  return (list)
}
