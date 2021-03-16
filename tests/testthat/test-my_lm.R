# within test-my_t.test.R
test_that("my_t.test works mathematically", {
  expect_equal(my_lm(lifeExp ~ gdpPercap + continent, my_gapminder),
               summary(lm(lifeExp ~ gdpPercap + continent, my_gapminder))$coefficients)
})
test_that("dataset does not contain the variables in the formula", {
  expect_error(my_lm(lifeExp ~ gdpPercap + continent, my_penguins))
})
