context("test-optimum_allocation")

library(dplyr)
library(optimall)

data <- data.frame("strata" = c(rep("a", times = 15),
                                rep("b", times = 15),
                                rep("c", times = 12)),
              "y" = c(rnorm(30, sd = 1), rnorm(12, sd = 2)),
              "key" = rbinom(42, 1, 0.2))

test_that("Neyman Allocation works", {
  nsd_vec <- c(length(data[data$strata == "a","y"])*
                 sd(data[data$strata == "a","y"]),
               length(data[data$strata == "b","y"])*
                 sd(data[data$strata == "b","y"]),
               length(data[data$strata == "c","y"])*
                 sd(data[data$strata == "c","y"]))

  expect_equal(optimum_allocation(data = data, strata = "strata",
                                  y = "y", method = "Neyman")$n_sd,
               round(nsd_vec,digits = 2))
  expect_equal(optimum_allocation(data = data, strata = "strata",
                                  y = "y",
                                  method = "Neyman")$stratum_fraction,
               round(nsd_vec/sum(nsd_vec), digits = 2))
})

test_that("WrightI and WrightII work", {
  expect_equal(optimum_allocation(data = data, strata = "strata",
                                  y = "y", nsample = 10,
                                  method = "WrightI")$stratum_size,
               optimum_allocation(data = data, strata = "strata",
                                  y = "y", nsample = 10,
                                  method = "WrightII")$stratum_size,
               optimum_allocation(data = data, strata = "strata",
                                  y = "y", nsample = 10,
                                  method = "Neyman")$stratum_size)
  # Should agree in this simple case
  expect_equal(sum(optimum_allocation(data = data, strata = "strata",
                                      y = "y", nsample = 15,
                                      method = "WrightII")$stratum_size),
               15)
})

test_that("Output agrees whether input is matrix, df, or tibble",{
  data_mat <- as.matrix(data.frame("strata" = c(rep(1, times = 15),
                                                rep(2, times = 15),
                                                rep(3, times = 12)),
                                   "y" = data$y,"key" = data$key))
  data_tib <- dplyr::as_tibble(data)

  expect_equal(optimum_allocation(data = data, strata = "strata",
                                  y = "y", nsample = 10)$stratum_size,
               optimum_allocation(data = data_mat, strata = "strata",
                                  y = "y", nsample = 10)$stratum_size,
               optimum_allocation(data = data_tib, strata = "strata",
                                  y = "y", nsample = 10)$stratum_size)
})

test_that("optimum_allocation prints error message when 'y'
          is not numeric",{
  data2 <- dplyr::mutate(data, y = as.factor(y))
  expect_error(optimum_allocation(data = data2, y = "y",
                                  strata = "strata", nsample = 10),
               "'y' must be numeric.")

})

test_that("'nsample' argument of optimum_allocation can't be less than
          or equal to zero, but it can be larger than the population of
          the dataset if the method is Neyman",{
  expect_error(optimum_allocation(data = data, y = "y",
                                  strata = "strata",
                                  method = "WrightII",
                                  nsample = 0),
               "'nsample' is too small for this method")
  expect_error(optimum_allocation(data = data, y = "y",
                                  strata = "strata",
                                  method = "WrightII",
                                  nsample = 50),
               "'nsample' is larger than population size")
})

test_that("multiple strings in  the 'strata' argument lead to the
          creation of new strata based on their interaction",{
  data$strata2 <- rbinom(42, 1, 0.5)
  names(data)[names(data)== "strata"] <- "strata3"
  expect_equal(as.character(optimum_allocation(data = data,
                                               strata = c("strata3",
                                                          "strata2"),
                                               y = "y",
                                               nsample = 30)$strata),
               c("a.0","b.0","c.0","a.1","b.1","c.1"))
})
