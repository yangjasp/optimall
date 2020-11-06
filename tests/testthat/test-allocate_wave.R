context("test-allocate_wave")

library(dplyr)
library(optimall)

data <- data.frame("strata" = c(rep("a", times = 15), rep("b", times = 15), rep("c", times = 12)),
                   "y" = c(rnorm(30, sd = 1), rnorm(12, sd = 2)),
                   "key" = rbinom(42, 1, 0.2))
data$key[c(1,16,31)] <- 1 #To make sure no group gets zero in wave2a

test_that("the output of allocate_wave is as expected",{
  output <- allocate_wave(data = data, strata = "strata", wave2a = "key", y = "y",nsample = 20)
  expect_equal(output$nsample_total, optimum_allocation(data = data, strata = "strata", y = "y", nsample = sum(data$key) + 20)$stratum_size) #Only works if no oversampling in wave2a
  expect_equal(sum(output$n_to_sample), 20)
  expect_equal(sum(output$n_to_sample) + sum(output$nsample_prior), sum(output$nsample_total))
})

test_that("y must be numeric, as it has to be for optimal_allocation",{
  data2 <- data
  data2$y <- as.character(data2$y)
  expect_error(allocate_wave(data = data2, strata = "strata", wave2a = "key", y = "y",nsample = 20), "'y' must be numeric.")
})

test_that("nsample cannot be larger than npop - n_sampled_prior",{
  expect_error(allocate_wave(data = data, strata = "strata", wave2a = "key", y = "y",nsample = 39), "Total sample size across waves, taken as nsampled in wave2a + nsample, is larger than the population size.", fixed = TRUE)
})
