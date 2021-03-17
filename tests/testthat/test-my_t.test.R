# within test-my_t.test.R
test_that("my_t.test works mathematically", {
  a <- t.test(my_gapminder$lifeExp, alternative = "two.sided", mu = 60)
  b <- t.test(my_gapminder$lifeExp, alternative = "less", mu = 60)
  c <- t.test(my_gapminder$lifeExp, alternative = "greater", mu = 60)

  expect_equal(my_t.test(my_gapminder$lifeExp, "two.sided", 60),
               c(as.numeric(a$statistic), as.numeric(a$parameter), a$alternative, a$p.value),
               tolerance = 1e-10)
  expect_equal(my_t.test(my_gapminder$lifeExp, "less", 60),
               c(as.numeric(b$statistic), as.numeric(b$parameter), b$alternative, b$p.value),
               tolerance = 1e-10)
  expect_equal(my_t.test(my_gapminder$lifeExp, "greater", 60),
               c(as.numeric(c$statistic), as.numeric(c$parameter), c$alternative, c$p.value),
               tolerance = 1e-10)
})
test_that("unlisted input 'alternative' throws an error", {
  expect_error(my_t.test(my_gapminder$lifeExp, "large", 60))
})
