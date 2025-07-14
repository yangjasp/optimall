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
