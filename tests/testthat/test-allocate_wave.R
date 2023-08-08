context("test-allocate_wave")

library(dplyr)
library(optimall)

data <- data.frame(
  "strata" = c(
    rep("a", times = 15),
    rep("b", times = 15),
    rep("c", times = 12)
  ),
  "y" = c(rnorm(30, sd = 1), rnorm(12, sd = 2)),
  "key" = rbinom(42, 1, 0.2)
)
data$key[c(1, 16, 31)] <- 1 # To make sure no group gets zero in already_sampled

test_that("the output of allocate_wave is as expected", {
  output <- allocate_wave(
    data = data, strata = "strata",
    already_sampled = "key", y = "y", nsample = 20
  )
  expect_equal(
    output$nsample_actual,
    optimum_allocation(
      data = data, strata = "strata",
      y = "y",
      nsample = sum(data$key) + 20
    )$stratum_size
  )
  # Only works if no oversampling in already_sampled
  expect_equal(sum(output$n_to_sample), 20)
  expect_equal(
    sum(output$n_to_sample) + sum(output$nsample_prior),
    sum(output$nsample_actual)
  )
})

test_that("the output is as expected with allocation_method = Neyman", {
  output <- allocate_wave(
    data = data, strata = "strata",
    already_sampled = "key", y = "y", nsample = 20, allocation_method = "Neyman"
  )
  expect_equal(
    output$nsample_actual,
    optimum_allocation(
      data = data, strata = "strata",
      y = "y", method = "Neyman",
      nsample = sum(data$key) + 20
    )$stratum_size
  )
  # Only works if no oversampling in already_sampled
  expect_lt(sum(output$n_to_sample), 22)
  expect_gt(sum(output$n_to_sample), 18)
  expect_equal(
    sum(output$n_to_sample) + sum(output$nsample_prior),
    sum(output$nsample_actual)
  )
})

test_that("If there is oversampling, allocate_wave does keeps strata
          at least as large as they were in prior samples and total
          sample sizes add up properly", {
  data$key <- c(
    rep(1, times = 13), 0, 0,
    rep(0, times = 10),
    rep(1, times = 5),
    rep(0, times = 8),
    rep(1, times = 4)
  )
  # total of 42. stratum a has been oversampled.
  # Total prior nsample is 22.
  output_over <- allocate_wave(
    data = data,
    strata = "strata", already_sampled = "key",
    y = "y", nsample = 8
  )
  expect_equal(sum(output_over$nsample_actual), 30)
  expect_equal(any(output_over$nsample_actual < c(13, 5, 4)), FALSE)
  expect_equal(
    output_over$n_to_sample + output_over$nsample_prior,
    output_over$nsample_actual
  )
  output_over_simple <- allocate_wave(
    data = data, strata = "strata",
    already_sampled = "key", y = "y",
    nsample = 8, method = "simple"
  )
  # and for simple method
  expect_equal(sum(output_over_simple$nsample_actual), 30)
  expect_equal(any(output_over_simple$nsample_actual < c(13, 5, 4)), FALSE)
  expect_equal(
    output_over_simple$n_to_sample +
      output_over_simple$nsample_prior,
    output_over_simple$nsample_actual
  )
})

test_that("the output of allocate_wave matches the output of
          optimum_allocation where it should", {
  data2 <- data
  data2$key2 <- c(rep(1, times = 10), rbinom(32, 1, 0.2))
  output_wave <- allocate_wave(
    data = data2, strata = "strata",
    already_sampled = "key2", y = "y", nsample = 12,
    detailed = TRUE
  )
  output_opt <- optimum_allocation(
    data = data2, strata = "strata",
    y = "y",
    nsample = sum(data2$key2 == 1) + 12
  )
  expect_equal(output_wave$nsample_optimal, output_opt$stratum_size)
  expect_equal(output_wave$sd, output_opt$sd)
  expect_equal(
    sum(output_wave$nsample_actual),
    sum(output_opt$stratum_size)
  )
  expect_equal(
    all((output_wave$nsample_prior +
      output_wave$n_to_sample) ==
      output_wave$nsample_actual),
    TRUE
  )
})

test_that("y must be numeric, as it has to be for optimum_allocation", {
  data2 <- data
  data2$y <- as.character(data2$y)
  expect_error(
    allocate_wave(
      data = data2, strata = "strata",
      already_sampled = "key", y = "y", nsample = 20
    ),
    "'y' must be numeric."
  )
})

test_that("nsample cannot be larger than npop - n_sampled_prior", {
  expect_error(allocate_wave(
    data = data, strata = "strata",
    already_sampled = "key", y = "y", nsample = 39
  ),
  "Total sample size across waves, taken as",
  fixed = TRUE
  )
})

test_that("error if data available for less than two samples", {
  data3 <- data %>%
    dplyr::mutate(y2 = ifelse(strata == "a", NA, y))
  expect_error(allocate_wave(
    data = data3, strata = "strata",
    already_sampled = "key", y = "y2", nsample = 15
  ),
  "Function requires at least two observations",
  fixed = TRUE
  )
})

test_that("detailed = TRUE gives all columns of interest", {
  output <- allocate_wave(
    data = data, strata = "strata",
    already_sampled = "key", y = "y", nsample = 20,
    detailed = TRUE
  )
  expect_equal(
    names(output),
    c(
      "strata", "npop", "nsample_optimal", "nsample_actual",
      "nsample_prior", "n_to_sample", "sd"
    )
  )
})

test_that("basic errors work", {
  expect_error(
    allocate_wave(
      data = data, strata = "strata",
      already_sampled = "key", y = "y_wrong", nsample = 20
    ),
    "'y' must be a character string"
  )
  expect_error(
    allocate_wave(
      data = data, strata = "strata",
      already_sampled = "key_wrong", y = "y", nsample = 20
    ),
    "'already_sampled' must be a character string"
  )
  expect_error(
    allocate_wave(
      data = data, strata = "strata_wrong",
      already_sampled = "key", y = "y", nsample = 20
    ),
    "'strata' must be a character string"
  )
  bad_data <- data
  bad_data$key_wrong <- rep(c(1, 2, 3), times = dim(bad_data)[1] / 3)
  expect_error(
    allocate_wave(
      data = bad_data, strata = "strata",
      already_sampled = "key_wrong", y = "y", nsample = 20
    ),
    "has a binary indicator for whether each unit"
  )
  bad_data$key[3] <- NA
  expect_error(
    allocate_wave(
      data = bad_data, strata = "strata",
      already_sampled = "key", y = "y", nsample = 20
    ),
    "cannot contain NAs"
  )
})
