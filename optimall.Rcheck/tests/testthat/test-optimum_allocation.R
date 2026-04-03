context("test-optimum_allocation")

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

test_that("Neyman Allocation works", {
  nsd_vec <- c(
    length(data[data$strata == "a", "y"]) *
      sd(data[data$strata == "a", "y"]),
    length(data[data$strata == "b", "y"]) *
      sd(data[data$strata == "b", "y"]),
    length(data[data$strata == "c", "y"]) *
      sd(data[data$strata == "c", "y"])
  )

  expect_equal(
    optimum_allocation(
      data = data, strata = "strata",
      y = "y", method = "Neyman"
    )$n_sd,
    round(nsd_vec, digits = 2)
  )
  expect_equal(
    optimum_allocation(
      data = data, strata = "strata",
      y = "y",
      method = "Neyman"
    )$stratum_fraction,
    round(nsd_vec / sum(nsd_vec), digits = 2)
  )
})

test_that("WrightI and WrightII work", {
  expect_equal(
    optimum_allocation(
      data = data, strata = "strata",
      y = "y", nsample = 10,
      method = "WrightI"
    )$stratum_size,
    optimum_allocation(
      data = data, strata = "strata",
      y = "y", nsample = 10,
      method = "WrightII"
    )$stratum_size)

  expect_equal(
    optimum_allocation(
    data = data, strata = "strata",
    y = "y", nsample = 10,
    method = "WrightII"
  )$stratum_size,
    optimum_allocation(
      data = data, strata = "strata",
      y = "y", nsample = 10,
      method = "Neyman"
    )$stratum_size
  )
  # Should agree in this simple case
  expect_equal(
    sum(optimum_allocation(
      data = data, strata = "strata",
      y = "y", nsample = 15,
      method = "WrightII"
    )$stratum_size),
    15
  )
})

test_that("Output agrees whether input is matrix, df, or tibble", {
  data_mat <- as.matrix(data.frame(
    "strata" = c(
      rep(1, times = 15),
      rep(2, times = 15),
      rep(3, times = 12)
    ),
    "y" = data$y, "key" = data$key
  ))
  data_tib <- dplyr::as_tibble(data)

  expect_equal(
    optimum_allocation(
      data = data, strata = "strata",
      y = "y", nsample = 10
    )$stratum_size,
    optimum_allocation(
      data = data_mat, strata = "strata",
      y = "y", nsample = 10
    )$stratum_size)

  expect_equal(
    optimum_allocation(
    data = data_mat, strata = "strata",
    y = "y", nsample = 10
  )$stratum_size,
    optimum_allocation(
      data = data_tib, strata = "strata",
      y = "y", nsample = 10
    )$stratum_size
  )
})

test_that("optimum_allocation prints error message when 'y'
          is not numeric", {
  data2 <- dplyr::mutate(data, y = as.factor(y))
  expect_error(
    optimum_allocation(
      data = data2, y = "y",
      strata = "strata", nsample = 10
    ),
    "'y' must be numeric."
  )
})

test_that("'nsample' argument of optimum_allocation can't be less than
          or equal to zero, but it can be larger than the population of
          the dataset if the method is Neyman", {
  expect_error(
    optimum_allocation(
      data = data, y = "y",
      strata = "strata",
      method = "WrightII",
      nsample = 0
    ),
    "'nsample' is too small for this method"
  )
  expect_error(
    optimum_allocation(
      data = data, y = "y",
      strata = "strata",
      method = "WrightII",
      nsample = 50
    ),
    "'nsample' is larger than population size"
  )
})

test_that("multiple strings in  the 'strata' argument lead to the
          creation of new strata based on their interaction", {
  data$strata2 <- rbinom(42, 1, 0.5)
  names(data)[names(data) == "strata"] <- "strata3"
  expect_equal(
    as.character(optimum_allocation(
      data = data,
      strata = c(
        "strata3",
        "strata2"
      ),
      y = "y",
      nsample = 30
    )$strata),
    c("a.0", "b.0", "c.0", "a.1", "b.1", "c.1")
  )
})

test_that("Error if not enough non-NA observations in a stratum", {
  data3 <- data %>%
    dplyr::mutate(y = ifelse(strata == "a", NA, y))
  expect_error(
    optimum_allocation(
      data = data3, strata = "strata",
      y = "y", method = "Neyman",
      allow.na = TRUE
    ),
    "Function requires at least two observations per stratum"
  )
})

##  Tests for simple version with N_h and sd_h

short_data <- data.frame(
  strata = c("a", "b", "c"),
  size = c(15, 15, 12),
  sd = c(
    sd(data[data$strata == "a", "y"]),
    sd(data[data$strata == "b", "y"]),
    sd(data[data$strata == "c", "y"])
  )
)
nsd_vec <- short_data$size * short_data$sd

test_that("Neyman Allocation works", {
  expect_equal(
    optimum_allocation(
      data = short_data, strata = "strata",
      sd_h = "sd",
      N_h = "size", method = "Neyman"
    )$n_sd,
    round(nsd_vec, digits = 2)
  )
  expect_equal(
    optimum_allocation(
      data = short_data, strata = "strata",
      sd_h = "sd",
      N_h = "size",
      method = "Neyman"
    )$stratum_fraction,
    round(nsd_vec / sum(nsd_vec), digits = 2)
  )
})

test_that("WrightI and WrightII work", {
  expect_equal(
    optimum_allocation(
      data = short_data, strata = "strata",
      N_h = "size", sd_h = "sd", nsample = 10,
      method = "WrightI"
    )$stratum_size,
    optimum_allocation(
      data = short_data, strata = "strata",
      N_h = "size", sd_h = "sd", nsample = 10,
      method = "WrightII"
    )$stratum_size)
  expect_equal(
    optimum_allocation(
    data = short_data, strata = "strata",
    N_h = "size", sd_h = "sd", nsample = 10,
    method = "WrightII"
    )$stratum_size,
    optimum_allocation(
      data = data, strata = "strata",
      y = "y", nsample = 10,
      method = "Neyman"
    )$stratum_size
  )
  # Should agree in this simple case
  expect_equal(
    sum(optimum_allocation(
      data = data, strata = "strata",
      y = "y", nsample = 15,
      method = "WrightII"
    )$stratum_size),
    15
  )
})

test_that("Errors work for sd_h and N_h version", {
  short_data3 <- short_data
  short_data3$y <- c(34, 20, 30)
  expect_error(
    optimum_allocation(short_data3,
      strata = "strata",
      y = "y",
      sd_h = "sd",
      N_h = "size"
    ),
    "One and only one of"
  )
  expect_error(
    optimum_allocation(short_data3,
      strata = "strata",
      y = "y",
      N_h = "size"
    ),
    "If 'sd_h' is NULL, 'N_h' should also be NULL"
  )
  new_row <- c("b", 15, 0.953, 25)
  short_data3 <- rbind(short_data3, new_row)
  short_data3$size <- as.numeric(short_data3$size)
  short_data3$sd <- as.numeric(short_data3$sd)
  expect_error(
    optimum_allocation(short_data3,
      strata = "strata",
      sd_h = "sd",
      N_h = "size"
    ),
    "data must only contain one row per stratum"
  )
})

test_that("Output agrees whether input is matrix, df, or tibble", {
  data_mat <- as.matrix(data.frame(
    "strata" = c(1, 2, 3),
    "size" = c(15, 15, 12),
    "sd" =
      c(
        sd(data[data$strata == "a", "y"]),
        sd(data[data$strata == "b", "y"]),
        sd(data[data$strata == "c", "y"])
      )
  ))
  data_tib <- dplyr::as_tibble(short_data)

  expect_equal(
    optimum_allocation(
      data = short_data, strata = "strata",
      N_h = "size",
      sd_h = "sd",
      nsample = 10
    )$stratum_size,
    optimum_allocation(
      data = data_mat, strata = "strata",
      N_h = "size",
      sd_h = "sd",
      nsample = 10
    )$stratum_size)
  expect_equal(optimum_allocation(
    data = data_mat, strata = "strata",
    N_h = "size",
    sd_h = "sd",
    nsample = 10
  )$stratum_size,
    optimum_allocation(
      data = data_tib, strata = "strata",
      N_h = "size",
      sd_h = "sd",
      nsample = 10
    )$stratum_size
  )
})

test_that("'nsample' argument of optimum_allocation can't be less than
          or equal to zero, but it can be larger than the population of
          the dataset if the method is Neyman", {
  expect_error(
    optimum_allocation(
      data = short_data,
      N_h = "size",
      sd_h = "sd",
      strata = "strata",
      method = "WrightII",
      nsample = 0
    ),
    "'nsample' is too small for this method"
  )
  expect_error(
    optimum_allocation(
      data = short_data,
      N_h = "size",
      sd_h = "sd",
      strata = "strata",
      method = "WrightII",
      nsample = 50
    ),
    "'nsample' is larger than population size"
  )
})

test_that("multiple strings in  the 'strata' argument lead to the
          creation of new strata based on their interaction", {
  short_data4 <- rbind(short_data, short_data)
  short_data4$strata2 <- c(0, 1, 0, 1, 0, 1)
  expect_equal(
    as.character(optimum_allocation(
      data = short_data4,
      strata = c("strata", "strata2"),
      N_h = "size", sd_h = "sd",
      nsample = 30
    )$strata),
    c("a.0", "b.0", "c.0", "a.1", "b.1", "c.1")
  )
})

# Test WrightIII

test_that("WrightIII algorithm works with named lower/upper bounds for y input", {
  data <- data.frame(
    strata = rep(c("A", "B", "C"), each = 5),
    y = c(
      1, 2, 3, 4, 5,
      10, 11, 12, 13, 14,
      20, 22, 24, 26, 28
    )
  )

  lower <- c(C = 3, A = 1, B = 2)   # deliberately out of order
  upper <- c(B = 4, C = 5, A = 3)   # deliberately out of order

  out <- optimum_allocation(
    data = data,
    strata = "strata",
    y = "y",
    nsample = 9,
    method = "WrightIII",
    lower = lower,
    upper = upper
  )

  sizes <- setNames(out$stratum_size, as.character(out$strata))

  expect_equal(sum(out$stratum_size), 9)
  expect_true(all(sizes[names(lower)] >= lower))
  expect_true(all(sizes[names(upper)] <= upper))
})

test_that("WrightIII matches WrightI for y input when lower = 1 and upper = N_h", {
  data <- data.frame(
    strata = rep(c("A", "B", "C"), each = 6),
    y = c(
      1, 2, 3, 4, 5, 6,
      2, 4, 6, 8, 10, 12,
      3, 6, 9, 12, 15, 18
    )
  )

  out_I <- optimum_allocation(
    data = data,
    strata = "strata",
    y = "y",
    nsample = 8,
    method = "WrightI"
  )

  Nh <- table(data$strata)
  lower <- setNames(rep(1, length(Nh)), names(Nh))
  upper <- setNames(as.integer(Nh), names(Nh))

  out_III <- optimum_allocation(
    data = data,
    strata = "strata",
    y = "y",
    nsample = 8,
    method = "WrightIII",
    lower = lower,
    upper = upper
  )

  expect_equal(
    out_III$stratum_size[match(out_I$strata, out_III$strata)],
    out_I$stratum_size
  )
})

test_that("WrightIII matches WrightII for y input when lower = 2 and upper = N_h", {
  data <- data.frame(
    strata = rep(c("A", "B", "C"), each = 6),
    y = c(
      1, 2, 3, 4, 5, 6,
      2, 4, 6, 8, 10, 12,
      3, 6, 9, 12, 15, 18
    )
  )

  out_II <- optimum_allocation(
    data = data,
    strata = "strata",
    y = "y",
    nsample = 9,
    method = "WrightII"
  )

  Nh <- table(data$strata)
  lower <- setNames(rep(2, length(Nh)), names(Nh))
  upper <- setNames(as.integer(Nh), names(Nh))

  out_III <- optimum_allocation(
    data = data,
    strata = "strata",
    y = "y",
    nsample = 9,
    method = "WrightIII",
    lower = lower,
    upper = upper
  )

  expect_equal(
    out_III$stratum_size[match(out_II$strata, out_III$strata)],
    out_II$stratum_size
  )
})

test_that("WrightIII works with sd_h/N_h input and respects bounds", {
  data <- data.frame(
    strata = c("A", "B", "C"),
    N_h = c(5, 6, 7),
    sd_h = c(0.4, 10, 2)
  )

  lower <- c(C = 2, A = 1, B = 2)
  upper <- c(B = 4, C = 5, A = 3)

  out <- optimum_allocation(
    data = data,
    strata = "strata",
    sd_h = "sd_h",
    N_h = "N_h",
    nsample = 8,
    method = "WrightIII",
    lower = lower,
    upper = upper
  )

  sizes <- setNames(out$stratum_size, as.character(out$strata))

  expect_equal(sum(out$stratum_size), 8)
  expect_true(all(sizes[names(lower)] >= lower))
  expect_true(all(sizes[names(upper)] <= upper))
})

test_that("WrightIII matches WrightI for sd_h/N_h input when lower = 1 and upper = N_h", {
  data <- data.frame(
    strata = c("A", "B", "C"),
    N_h = c(6, 6, 6),
    sd_h = c(1, 4, 2)
  )

  out_I <- optimum_allocation(
    data = data,
    strata = "strata",
    sd_h = "sd_h",
    N_h = "N_h",
    nsample = 8,
    method = "WrightI"
  )

  lower <- setNames(rep(1, nrow(data)), data$strata)
  upper <- setNames(data$N_h, data$strata)

  out_III <- optimum_allocation(
    data = data,
    strata = "strata",
    sd_h = "sd_h",
    N_h = "N_h",
    nsample = 8,
    method = "WrightIII",
    lower = lower,
    upper = upper
  )

  expect_equal(
    out_III$stratum_size[match(out_I$strata, out_III$strata)],
    out_I$stratum_size
  )
})

test_that("WrightIII matches WrightII for sd_h/N_h input when lower = 2 and upper = N_h", {
  data <- data.frame(
    strata = c("A", "B", "C"),
    N_h = c(6, 6, 6),
    sd_h = c(1, 4, 2)
  )

  out_II <- optimum_allocation(
    data = data,
    strata = "strata",
    sd_h = "sd_h",
    N_h = "N_h",
    nsample = 9,
    method = "WrightII"
  )

  lower <- setNames(rep(2, nrow(data)), data$strata)
  upper <- setNames(data$N_h, data$strata)

  out_III <- optimum_allocation(
    data = data,
    strata = "strata",
    sd_h = "sd_h",
    N_h = "N_h",
    nsample = 9,
    method = "WrightIII",
    lower = lower,
    upper = upper
  )

  expect_equal(
    out_III$stratum_size[match(out_II$strata, out_III$strata)],
    out_II$stratum_size
  )
})

test_that("WrightIII works for A-optimal allocation with multiple y variables", {
  data <- data.frame(
    strata = rep(c("A", "B", "C"), each = 6),
    y1 = c(
      1, 2, 2, 2, 5, 2,
      5, 11, 12, 13, 14, 15,
      2, 4, 6, 8, 10, 12
    ),
    y2 = c(
      6, 5, 5, 3, 2, 3,
      1, 3, 5, 7, 9, 11,
      4, 5, 6, 7, 8, 9
    )
  )

  lower <- c(C = 2, A = 1, B = 2)
  upper <- c(B = 5, C = 6, A = 4)

  out <- optimum_allocation(
    data = data,
    strata = "strata",
    y = c("y1", "y2"),
    weights = c(0.9, 0.1),
    nsample = 9,
    method = "WrightIII",
    lower = lower,
    upper = upper
  )

  sizes <- setNames(out$stratum_size, as.character(out$strata))

  expect_equal(sum(out$stratum_size), 9)
  expect_true(all(sizes[names(lower)] >= lower))
  expect_true(all(sizes[names(upper)] <= upper))
})

test_that("WrightIII matches WrightII in A-optimal y case when lower = 2 and upper = N_h", {
  data <- data.frame(
    strata = rep(c("A", "B", "C"), each = 6),
    y1 = c(
      1, 2, 3, 4, 5, 6,
      10, 11, 12, 13, 14, 15,
      2, 4, 6, 8, 10, 12
    ),
    y2 = c(
      6, 5, 4, 3, 2, 1,
      1, 3, 5, 7, 9, 11,
      4, 5, 6, 7, 8, 9
    )
  )

  out_II <- optimum_allocation(
    data = data,
    strata = "strata",
    y = c("y1", "y2"),
    weights = c(0.5, 0.5),
    nsample = 9,
    method = "WrightII"
  )

  Nh <- table(data$strata)
  lower <- setNames(rep(2, length(Nh)), names(Nh))
  upper <- setNames(as.integer(Nh), names(Nh))

  out_III <- optimum_allocation(
    data = data,
    strata = "strata",
    y = c("y1", "y2"),
    weights = c(0.5, 0.5),
    nsample = 9,
    method = "WrightIII",
    lower = lower,
    upper = upper
  )

  expect_equal(
    out_III$stratum_size[match(out_II$strata, out_III$strata)],
    out_II$stratum_size
  )
})

test_that("WrightIII works for A-optimal allocation with multiple sd_h variables", {
  data <- data.frame(
    strata = c("A", "B", "C"),
    N_h = c(6, 6, 6),
    sd1 = c(1, 4, 2),
    sd2 = c(3, 1, 5)
  )

  lower <- c(C = 2, A = 1, B = 2)
  upper <- c(B = 5, C = 6, A = 4)

  out <- optimum_allocation(
    data = data,
    strata = "strata",
    sd_h = c("sd1", "sd2"),
    weights = c(0.5, 0.5),
    N_h = "N_h",
    nsample = 9,
    method = "WrightIII",
    lower = lower,
    upper = upper
  )

  sizes <- setNames(out$stratum_size, as.character(out$strata))

  expect_equal(sum(out$stratum_size), 9)
  expect_true(all(sizes[names(lower)] >= lower))
  expect_true(all(sizes[names(upper)] <= upper))
})

test_that("WrightIII errors when both lower and upper are missing", {
  data <- data.frame(
    strata = rep(c("A", "B", "C"), each = 5),
    y = rnorm(15)
  )

  expect_error(
    optimum_allocation(
      data = data,
      strata = "strata",
      y = "y",
      nsample = 9,
      method = "WrightIII"
    ),
    "requires lower and"
  )
})

test_that("WrightIII errors when lower/upper lengths do not match number of strata", {
  data <- data.frame(
    strata = rep(c("A", "B", "C"), each = 5),
    y = rnorm(15)
  )

  expect_error(
    optimum_allocation(
      data = data,
      strata = "strata",
      y = "y",
      nsample = 9,
      method = "WrightIII",
      lower = c(A = 1, B = 2),
      upper = c(A = 4, B = 4)
    ),
    "'lower' must have length equal to the number of strata"
  )
})

test_that("WrightIII allows unnamed constant lower/upper in y case", {
  data <- data.frame(
    strata = rep(c("A", "B", "C"), each = 5),
    y = rnorm(15)
  )

  out <- optimum_allocation(
    data = data,
    strata = "strata",
    y = "y",
    nsample = 9,
    method = "WrightIII",
    lower = c(2, 2, 2),
    upper = c(5, 5, 5)
  )

  expect_equal(sum(out$stratum_size), 9)
})

test_that("WrightIII errors for non-constant unnamed lower in y case", {
  data <- data.frame(
    strata = rep(c("A", "B", "C"), each = 5),
    y = rnorm(15)
  )

  expect_error(
    optimum_allocation(
      data = data,
      strata = "strata",
      y = "y",
      nsample = 9,
      method = "WrightIII",
      lower = c(1, 2, 2),
      upper = c(5, 5, 5)
    ),
    "must take the same value"
  )
})

test_that("WrightIII errors when upper names do not match strata", {
  data <- data.frame(
    strata = rep(c("A", "B", "C"), each = 5),
    y = rnorm(15)
  )

  expect_error(
    optimum_allocation(
      data = data,
      strata = "strata",
      y = "y",
      nsample = 9,
      method = "WrightIII",
      lower = c(A = 1, B = 2, C = 2),
      upper = c(A = 4, B = 4, D = 4)
    ),
    "names must match strata names"
  )
})

test_that("WrightIII errors when bounds are infeasible", {
  data <- data.frame(
    strata = rep(c("A", "B", "C"), each = 5),
    y = rnorm(15)
  )

  expect_error(
    optimum_allocation(
      data = data,
      strata = "strata",
      y = "y",
      nsample = 4,
      method = "WrightIII",
      lower = c(A = 2, B = 2, C = 2),
      upper = c(A = 5, B = 5, C = 5)
    ),
    "infeasible given the lower and upper bounds"
  )
})

test_that("WrightIII errors when upper exceeds population size", {
  data <- data.frame(
    strata = rep(c("A", "B", "C"), each = 5),
    y = rnorm(15)
  )

  expect_error(
    optimum_allocation(
      data = data,
      strata = "strata",
      y = "y",
      nsample = 9,
      method = "WrightIII",
      lower = c(A = 1, B = 1, C = 1),
      upper = c(A = 10, B = 5, C = 5)
    ),
    "All upper bounds must be"
  )
})

test_that("WrightIII errors when lower < 1", {
  data <- data.frame(
    strata = rep(c("A", "B", "C"), each = 5),
    y = rnorm(15)
  )

  expect_error(
    optimum_allocation(
      data = data,
      strata = "strata",
      y = "y",
      nsample = 9,
      method = "WrightIII",
      lower = c(A = 0, B = 1, C = 1),
      upper = c(A = 10, B = 5, C = 5)
    ),
    "All lower bounds must be"
  )
})

test_that("WrightIII sd_h unnamed lower follows data order", {
  data <- data.frame(
    strata = c("B", "A", "C"),   # intentionally out of order
    N_h = c(5, 5, 5),
    sd_h = c(1, 2, 3)
  )

  lower <- c(1, 2, 3)  # should map B=1, A=2, C=3
  upper <- c(5, 5, 5)

  out <- optimum_allocation(
    data = data,
    strata = "strata",
    sd_h = "sd_h",
    N_h = "N_h",
    nsample = 8,
    method = "WrightIII",
    lower = lower,
    upper = upper
  )

  sizes <- setNames(out$stratum_size, as.character(out$strata))

  expect_true(sizes["B"] >= 1)
  expect_true(sizes["A"] >= 2)
  expect_true(sizes["C"] >= 3)
})

test_that("WrightIII allows non-constant unnamed lower in sd_h case", {
  data <- data.frame(
    strata = c("A", "B", "C"),
    N_h = c(5, 6, 7),
    sd_h = c(1, 4, 2)
  )

  lower <- c(1, 2, 3)
  upper <- c(5, 6, 7)

  out <- optimum_allocation(
    data = data,
    strata = "strata",
    sd_h = "sd_h",
    N_h = "N_h",
    nsample = 8,
    method = "WrightIII",
    lower = lower,
    upper = upper
  )

  expect_equal(sum(out$stratum_size), 8)
})

####
#### Tests for A-optimal allocation

data <- data.frame(
  "strata" = c(
    rep("a", times = 15),
    rep("b", times = 15),
    rep("c", times = 12)
  ),
  "y1" = c(rnorm(30, sd = 1), rnorm(12, sd = 2)),
  "y2" = c(rnorm(30, sd = 3), rnorm(12, sd = 5)),
  "key" = rbinom(42, 1, 0.2)
)
weights <- c(0.3,0.7)

test_that("A-optimal allocation works as intended when 'y' is supplied", {
  nsd_vec <- c(
    length(data[data$strata == "a", "y1"]) *
      sqrt(weights[1]*var(data[data$strata == "a", "y1"]) +
         weights[2]*var(data[data$strata == "a", "y2"])),
    length(data[data$strata == "b", "y1"]) *
      sqrt(weights[1]*var(data[data$strata == "b", "y1"]) +
         weights[2]*var(data[data$strata == "b", "y2"])),
    length(data[data$strata == "c", "y1"]) *
      sqrt(weights[1]*var(data[data$strata == "c", "y1"]) +
         weights[2]*var(data[data$strata == "c", "y2"]))
  )

  expect_equal(
    optimum_allocation(
      data = data, strata = "strata",
      y = c("y1","y2"), weights = weights, method = "Neyman"
    )$sd_y1,
    round(c(sd(data[data$strata == "a", "y1"]),
            sd(data[data$strata == "b", "y1"]),
            sd(data[data$strata == "c", "y1"])), digits = 2)
  )
  expect_equal(
    optimum_allocation(
      data = data, strata = "strata",
      y = c("y1","y2"), weights = weights, method = "Neyman"
    )$n_sd,
    round(nsd_vec, digits = 2)
  )
  expect_equal(
    optimum_allocation(
      data = data, strata = "strata",
      y = c("y1","y2"), weights = weights,
      method = "Neyman"
    )$stratum_fraction,
    round(nsd_vec / sum(nsd_vec), digits = 2)
  )
  expect_equal(
    optimum_allocation(
      data = data, strata = "strata", weights = weights,
      y = c("y1","y2"),  nsample = 10,
      method = "WrightI"
    )$stratum_size,
    optimum_allocation(
      data = data, strata = "strata",
      y = c("y1","y2"), nsample = 10, weights = weights,
      method = "WrightII"
    )$stratum_size)
  expect_error(
    optimum_allocation(
      data = data, strata = "strata",
      y = c("y1","y2"), nsample = 10, weights = 0.5,
      method = "WrightII"),
    "Must provide a vector of 'weights"
  )
  expect_error(optimum_allocation(
    data = data, strata = "strata",
    y = c("y1","y2"),
    method = "Neyman"
  ), "Must provide a vector of 'weights'")
  expect_error(optimum_allocation(
    data = data, strata = "strata",
    y = c("y1","y2"), weights = c(0.5, 0.6),
    method = "Neyman"
  ), "Must provide a vector of 'weights'")
     })


test_that("A-optimal allocation works as intended when 'sd_h' is supplied", {
  short_data <- data %>%
    dplyr::group_by(strata) %>%
    dplyr::summarise(
      size = dplyr::n(),
      sd1 = sd(y1, na.rm = TRUE),
      sd2 = sd(y2, na.rm = TRUE)
    ) %>%
    dplyr::mutate(
      sd_sum = sqrt(weights[1] * sd1^2 + weights[2] * sd2^2),
      tot = size * sd_sum,
      frac = tot / sum(tot)
    )

  res <- optimum_allocation(
    data = short_data,
    strata = c("strata"),
    N_h = "size", sd_h = c("sd1", "sd2"), weights = weights,
    nsample = 30
  )
  res_old <- optimum_allocation(
    data = data, strata = "strata", weights = weights,
    y = c("y1","y2"),  nsample = 30
  )
  expect_equal(
    res$n_sd, res_old$n_sd
  )
  expect_equal(
    res$stratum_size, res_old$stratum_size
  )

  expect_equal(
    res$sd1, res_old$sd_y1
  )

  expect_error(
    optimum_allocation(
      data = short_data,
      strata = c("strata"),
      N_h = "size", sd_h = c("sd1", "sd2"), weights = 0.5,
      nsample = 30
    ),
    "Must provide a vector of 'weights'"
  )
})
