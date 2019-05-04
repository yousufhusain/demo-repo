### Test for Private Checker Functions
library(testthat)
source("/Users/yousufhusain/Documents/hw-stat133/workout03/binomial/R/All Functions.R")

context("Tests for Checkers")

#check_prob()
test_that("check_prob will work with valid values", {
  expect_true(check_prob(0.7))
  expect_true(check_prob(1))
})

test_that("check_prob will fail with invalid values", {
  expect_error(check_prob(-5))
  expect_error(check_prob(5))
})

test_that("check_prob will fail with invalid types/lengths", {
  expect_error(check_prob("stat"))
  expect_error(check_prob(c("x", "y")))
})

#check_trials()
test_that("check_trials will work with valid values", {
  expect_true(check_trials(8))
  expect_true(check_trials(10))
})

test_that("check_trials will fail with invalid values", {
  expect_error(check_trials(-5))
  expect_error(check_trials(1.5))
})

test_that("check_trials will fail with invalid types/lengths", {
  expect_error(check_trials("stat"))
  expect_error(check_trials(c("a", 5)))
})

#check_success()
test_that("check_success will work with valid values", {
  expect_true(check_success(1,2))
  expect_true(check_success(1:2, 3))
})

test_that("check_success will fail with invalid types/lengths", {
  expect_error(check_success("stat",2))
  expect_error(check_success(-5, 3))
})

test_that("check_success will fail where successes > trials", {
  expect_error(check_success(2, 1))
  expect_error(check_success(5, 1:3))
})

context("Tests for Summary Measures")

#aux_mean()
test_that("aux_mean will produce the valid output", {
  expect_equal(aux_mean(10, 0.5), 5)
  expect_equal(aux_mean(15, 0.6), 9)
  expect_equal(aux_mean(20, 0.5), 10)
})

test_that("aux_mean will produce the valid length", {
  expect_length(aux_mean(10, 0.5), 1)
})

test_that("aux_mean will produce the valid type", {
  expect_type(aux_mean(10, 0.5), "double")
})


#aux_variance()
test_that("aux_variance will produce the valid output", {
  expect_equal(aux_variance(10, 0.5), 2.5)
  expect_equal(aux_variance(15, 0.6), 3.6)
  expect_equal(aux_variance(20, 0.5), 5)
})

test_that("aux_variance will produce the valid length", {
  expect_length(aux_variance(10, 0.5), 1)
})

test_that("aux_variance will produce the valid type", {
  expect_type(aux_variance(10, 0.5), "double")
})

#aux_mode()
test_that("aux_mode will produce the valid output", {
  expect_equal(aux_mode(10, 0.5), 5)
  expect_equal(aux_mode(15, 0.6), 9)
  expect_equal(aux_mode(20, 0.5), 10)
})

test_that("aux_mode will produce the valid length", {
  expect_length(aux_mode(10, 0.5), 1)
})

test_that("aux_mode will produce the valid type", {
  expect_type(aux_mode(10, 0.5), "integer")
})

#aux_skewness
test_that("aux_skewness will produce the valid output", {
  expect_equal(round(aux_skewness(10, 0.5), 5), 0)
  expect_equal(round(aux_skewness(10, 0.4), 5), 0.1291)
  expect_equal(round(aux_skewness(10, 0.3), 5), 0.27603)
})

test_that("aux_skewness will produce the avlid length", {
  expect_length(aux_skewness(10, 0.5), 1)
})

test_that("aux_skewness will produce the valid type", {
  expect_type(aux_skewness(10, 0.5), "double")
})

#aux_kurtosis
test_that("aux_kurtosis will produce the valid output", {
  expect_equal(round(aux_kurtosis(10, 0.3), 5), -0.12381)
  expect_equal(round(aux_kurtosis(20, 0.5), 5), -0.1)
  expect_equal(round(aux_kurtosis(30, 0.8), 5), 0.00833)
})

test_that("aux_kurtosis will produce the valid length", {
  expect_length(aux_kurtosis(10, 0.5), 1)
})

test_that("aux_kurtosis will produce the valid type", {
  expect_type(aux_kurtosis(10, 0.5), "double")
})

context("Tests for Binomial")

#bin_choose()
test_that("bin_choose will produce the valid output", {
  expect_equal(bin_choose(10, 1), 10)
  expect_equal(bin_choose(15, 2), 105)
  expect_equal(bin_choose(20, 3), 1140)
})

test_that("bin_choose will fail where k > n", {
  expect_error(bin_choose(1, 10))
})

test_that("bin_choose will produce the valid length", {
  expect_length(bin_choose(10, 1:5), 5)
})

#bin_probability()
test_that("bin_probability will produce the valid output", {
  expect_equal(bin_probability(0:2, 5, 0.5), c(0.03125, 0.15625, 0.31250))
  expect_equal(bin_probability(2, 5, 0.5), 0.3125)
  expect_equal(bin_probability(55, 100, 0.45), 0.01075277)
})

test_that("bin_probability will fail where success > trials", {
  expect_error(bin_probability(10, 4, 0.5))
})

test_that("bin_probability will fail with invalid values", {
  expect_error(bin_probability(-10, 4, 0.5))
})

#bin_distribution()
test_that("bin_distribution will produce the valid length", {
  expect_length(bin_distribution(10, 0.3)$success, 11)
})

test_that("bin_distribution will fail with invalid values", {
  expect_error(bin_distribution(-10, 0.5))
  expect_error(bin_distribution(10, -0.5))
})

test_that("bin_distribution will produce the valid class", {
  expect_is(bin_distribution(10, 0.3), c("bindis", "data.frame"))
})

#bin_cumulative()
test_that("bin_cumulative will produce the valid length", {
  expect_length(bin_cumulative(10, 0.3)$success, 11)
})

test_that("bin_cumulative will fail with invalid values", {
  expect_error(bin_cumulative(-10, 5))
  expect_error(bin_cumulative(10, -5))
})

test_that("bin_cumulative will produce the valid class", {
  expect_is(bin_cumulative(10, 0.3), c("bincum", "data.frame"))
})

