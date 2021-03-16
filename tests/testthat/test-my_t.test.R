# within test-my_t.test.R
test_that("my_t.test works mathematically", {
  expect_equal(my_t.test(my_gapminder$lifeExp, "two.sided", 60),
               c("-1.6795", "1703", "two.sided", "0.09323"))
  expect_equal(my_t.test(my_gapminder$lifeExp, "less", 60),
               c("-1.6795", "1703", "less", "0.9534"))
  expect_equal(my_t.test(my_gapminder$lifeExp, "greater", 60),
               c("-1.6795", "1703", "greater", "0.04661"))
})
test_that("unlisted input 'alternative' throws an error", {
  expect_error(my_t.test(my_gapminder$lifeExp, "large", 60))
})
