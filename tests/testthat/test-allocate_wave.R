context("test-allocate_wave")

library(dplyr)
library(optimall)

data <- data.frame(
  "strata" = c(
    rep("a", times = 15),
    rep("b", times = 15),
    rep("c", times = 12)
  ),
  "y" = c(rnorm(30, sd = 1), rnorm(12, sd = 1.5)),
  "key" = rbinom(42, 1, 0.2)
)
data$key[c(1, 16, 31)] <- 1 # To make sure no group gets zero in already_sampled

test_that("the output of allocate_wave is as expected", {
  output <- allocate_wave(
    data = data, strata = "strata",
    already_sampled = "key", y = "y", nsample = 15
  )
  expect_equal(
    output$nsample_actual,
    optimum_allocation(
      data = data, strata = "strata",
      y = "y",
      nsample = sum(data$key) + 15
    )$stratum_size
  )
  # Only works if no oversampling in already_sampled
  expect_equal(sum(output$n_to_sample), 15)
  expect_equal(
    sum(output$n_to_sample) + sum(output$nsample_prior),
    sum(output$nsample_actual)
  )
})

test_that("the output is as expected with allocation_method = Neyman", {
  output <- allocate_wave(
    data = data, strata = "strata",
    already_sampled = "key", y = "y", nsample = 15, allocation_method = "Neyman",
    detailed = TRUE
  )
  expect_equal(
    output$nsample_optimal,
    optimum_allocation(
      data = data, strata = "strata",
      y = "y", method = "Neyman",
      nsample = sum(data$key) + 15
    )$stratum_size
  )
  # Only works if no oversampling in already_sampled
  expect_lt(sum(output$n_to_sample), 17)
  expect_gt(sum(output$n_to_sample), 13)
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

#####
##### Tests for multiple y columns
data <- data.frame(
  "strata" = c(
    rep("a", times = 15),
    rep("b", times = 15),
    rep("c", times = 12)
  ),
  "y1" = c(rnorm(30, sd = 1), rnorm(12, sd = 1.5)),
  "y2" = c(rnorm(30, sd = 1.5), rnorm(12, sd = 2.3)),
  "key" = rbinom(42, 1, 0.2)
)
data$key[c(1, 16, 31)] <- 1 # To make sure no group gets zero in already_sampled


test_that("the output of allocate_wave is as expected", {
  expect_error(allocate_wave(
    data = data, strata = "strata",
    already_sampled = "key", y = c("y1","y2"), nsample = 15
  ),"Must provide a vector of 'weights")

  expect_error(allocate_wave(
    data = data, strata = "strata",
    already_sampled = "key", y = c("y1","y2"),
    weights = c(1,2), nsample = 15
  ),"Must provide a vector of 'weights")

  output <- allocate_wave(
    data = data, strata = "strata",
    already_sampled = "key", y = c("y1","y2"),
    weights = c(0.5,0.5),
    nsample = 15, detailed = TRUE
  )
  expect_equal(
    output$nsample_actual,
    optimum_allocation(
      data = data, strata = "strata",
      y = c("y1","y2"), weights = c(0.5,0.5),
      nsample = sum(data$key) + 15
    )$stratum_size
  )
  # Only works if no oversampling in already_sampled
  expect_equal(sum(output$n_to_sample), 15)
  expect_equal(
    sum(output$n_to_sample) + sum(output$nsample_prior),
    sum(output$nsample_actual)
  )
})

#####
##### Tests for multiple y columns with oversampling
set.seed(1)
data <- data.frame(
  "strata" = c(
    rep("a", times = 15),
    rep("b", times = 15),
    rep("c", times = 12)
  ),
  "y1" = c(rnorm(30, sd = 1), rnorm(12, sd = 1.5)),
  "y2" = c(rnorm(30, sd = 1.5), rnorm(12, sd = 2.3)),
  "key" = rbinom(42, 1, 0.2)
)
data$key[c(1, 2, 3,4,5,6,7, 8, 9,
           16, 31)] <- 1 # To make sure no group gets zero in already_sampled


test_that("the output of allocate_wave is as expected", {
  output <- allocate_wave(
    data = data, strata = "strata",
    already_sampled = "key", y = c("y1","y2"),
    weights = c(0.5,0.5),
    nsample = 5, detailed = TRUE
  )
  expect_equal(
    output$nsample_optimal,
    optimum_allocation(
      data = data, strata = "strata",
      y = c("y1","y2"), weights = c(0.5,0.5),
      nsample = sum(data$key) + 5
    )$stratum_size
  )
})

# Test WrightIII
test_that("allocate_wave passes WrightIII named bounds through in simple no-oversampling case", {
  set.seed(1)

  data <- data.frame(
    strata = rep(c("A", "B", "C"), each = 10),
    y = c(
      rnorm(10, 0, 1),
      rnorm(10, 0, 2),
      rnorm(10, 0, 3)
    ),
    already_sampled = c(
      rep(1, 2), rep(0, 8),
      rep(1, 2), rep(0, 8),
      rep(1, 2), rep(0, 8)
    )
  )

  lower <- c(C = 2, A = 2, B = 2)
  upper <- c(B = 8, C = 9, A = 7)

  out <- allocate_wave(
    data = data,
    strata = "strata",
    y = "y",
    already_sampled = "already_sampled",
    nsample = 6,
    allocation_method = "WrightIII",
    lower = lower,
    upper = upper,
    method = "simple",
    detailed = TRUE
  )

  sizes <- setNames(out$nsample_actual, as.character(out$strata))

  expect_equal(sum(out$n_to_sample), 6)
  expect_true(all(sizes[names(lower)] >= lower))
  expect_true(all(sizes[names(upper)] <= upper))
})

test_that("allocate_wave WrightIII works with unnamed constant bounds", {
  set.seed(2)

  data <- data.frame(
    strata = rep(c("A", "B", "C"), each = 10),
    y = rnorm(30),
    already_sampled = c(
      rep(1, 1), rep(0, 9),
      rep(1, 1), rep(0, 9),
      rep(1, 1), rep(0, 9)
    )
  )

  out <- allocate_wave(
    data = data,
    strata = "strata",
    y = "y",
    already_sampled = "already_sampled",
    nsample = 6,
    allocation_method = "WrightIII",
    lower = c(1, 1, 1),
    upper = c(10, 10, 10),
    method = "simple"
  )

  expect_equal(sum(out$n_to_sample), 6)
  expect_true(all(out$nsample_actual >= 1))
  expect_true(all(out$nsample_actual <= 10))
})

test_that("allocate_wave WrightIII errors when both lower and upper are missing", {
  set.seed(3)

  data <- data.frame(
    strata = rep(c("A", "B", "C"), each = 10),
    y = rnorm(30),
    already_sampled = c(
      rep(1, 1), rep(0, 9),
      rep(1, 1), rep(0, 9),
      rep(1, 1), rep(0, 9)
    )
  )

  expect_error(
    allocate_wave(
      data = data,
      strata = "strata",
      y = "y",
      already_sampled = "already_sampled",
      nsample = 6,
      allocation_method = "WrightIII",
      method = "simple"
    ),
    "requires.*lower.*upper"
  )
})

test_that("allocate_wave WrightIII subsets named bounds correctly in simple oversampling case", {
  data <- data.frame(
    strata = c(
      rep("A", 10),
      rep("B", 10),
      rep("C", 10)
    ),
    y = c(
      rep(1, 10),      # low variance
      1:10,            # moderate variance
      c(1, 5, 1, 5, 1, 5, 1, 5, 1, 5)  # moderate/high
    ),
    already_sampled = c(
      rep(1, 8), rep(0, 2),   # A heavily pre-sampled
      rep(1, 1), rep(0, 9),
      rep(1, 1), rep(0, 9)
    )
  )

  lower <- c(A = 1, B = 1, C = 1)
  upper <- c(A = 10, B = 10, C = 10)

  out <- allocate_wave(
    data = data,
    strata = "strata",
    y = "y",
    already_sampled = "already_sampled",
    nsample = 4,
    allocation_method = "WrightIII",
    lower = lower,
    upper = upper,
    method = "simple"
  )

  expect_equal(sum(out$n_to_sample), 4)
  expect_true(all(out$n_to_sample >= 0))
})

test_that("allocate_wave WrightIII subsets named bounds correctly in iterative oversampling case", {
  data <- data.frame(
    strata = c(
      rep("A", 10),
      rep("B", 10),
      rep("C", 10),
      rep("D", 10)
    ),
    y = c(
      rep(1, 10),
      1:10,
      c(1, 5, 1, 5, 1, 5, 1, 5, 1, 5),
      c(2, 8, 2, 8, 2, 8, 2, 8, 2, 8)
    ),
    already_sampled = c(
      rep(1, 8), rep(0, 2),   # likely oversampled relative to optimum
      rep(1, 1), rep(0, 9),
      rep(1, 1), rep(0, 9),
      rep(1, 1), rep(0, 9)
    )
  )

  lower <- c(A = 1, B = 1, C = 1, D = 1)
  upper <- c(A = 10, B = 10, C = 10, D = 10)

  out <- allocate_wave(
    data = data,
    strata = "strata",
    y = "y",
    already_sampled = "already_sampled",
    nsample = 4,
    allocation_method = "WrightIII",
    lower = lower,
    upper = upper,
    method = "iterative"
  )

  expect_equal(sum(out$n_to_sample), 4)
  expect_true(all(out$n_to_sample >= 0))
})

test_that("allocate_wave WrightIII with named bounds is invariant to name order", {
  set.seed(4)

  data <- data.frame(
    strata = rep(c("A", "B", "C"), each = 12),
    y = c(
      rnorm(12, 0, 1),
      rnorm(12, 0, 2),
      rnorm(12, 0, 3)
    ),
    already_sampled = c(
      rep(1, 2), rep(0, 10),
      rep(1, 2), rep(0, 10),
      rep(1, 2), rep(0, 10)
    )
  )

  lower1 <- c(A = 2, B = 2, C = 2)
  upper1 <- c(A = 9, B = 10, C = 11)

  lower2 <- c(C = 2, A = 2, B = 2)
  upper2 <- c(B = 10, C = 11, A = 9)

  out1 <- allocate_wave(
    data = data,
    strata = "strata",
    y = "y",
    already_sampled = "already_sampled",
    nsample = 6,
    allocation_method = "WrightIII",
    lower = lower1,
    upper = upper1,
    method = "simple"
  )

  out2 <- allocate_wave(
    data = data,
    strata = "strata",
    y = "y",
    already_sampled = "already_sampled",
    nsample = 6,
    allocation_method = "WrightIII",
    lower = lower2,
    upper = upper2,
    method = "simple"
  )

  expect_equal(
    out1$n_to_sample[match(out2$strata, out1$strata)],
    out2$n_to_sample
  )
})

test_that("allocate_wave WrightIII supports multiple y variables", {
  set.seed(5)

  data <- data.frame(
    strata = rep(c("A", "B", "C"), each = 10),
    y1 = c(
      rnorm(10, 0, 1),
      rnorm(10, 0, 2),
      rnorm(10, 0, 3)
    ),
    y2 = c(
      rnorm(10, 0, 2),
      rnorm(10, 0, 1),
      rnorm(10, 0, 2.5)
    ),
    already_sampled = c(
      rep(1, 2), rep(0, 8),
      rep(1, 2), rep(0, 8),
      rep(1, 2), rep(0, 8)
    )
  )

  lower <- c(A = 2, B = 2, C = 2)
  upper <- c(A = 10, B = 10, C = 10)

  out <- allocate_wave(
    data = data,
    strata = "strata",
    y = c("y1", "y2"),
    already_sampled = "already_sampled",
    nsample = 6,
    allocation_method = "WrightIII",
    weights = c(0.5, 0.5),
    lower = lower,
    upper = upper,
    method = "simple"
  )

  expect_equal(sum(out$n_to_sample), 6)
  expect_true(all(out$n_to_sample >= 0))
})

