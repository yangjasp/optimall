context("test-split_strata")

library(dplyr)
library(optimall)
library(stats)

data_split <- data.frame("strata" = c(rep("a", times = 15), rep("b", times = 15), rep("c", times = 12)),
                   "split_var" = c(rnorm(30, sd = 1), rnorm(12, sd = 2)),
                   "strata2" = rep(c(rep(0, times = 7), rep(1, times = 7)), times = 3))

test_that("strata_split produces a dataframe with the same number of rows as input and one more column called 'new_strata' with only the specified strata changed",{
  expect_equal(dim(split_strata(data = data_split, strata = "strata", split = "a", split_var = "split_var", split_at = 0, type = "value")),c(42,4))
  expect_equal(dim(split_strata(data = data_split, strata = "strata", split = NULL, split_var = "split_var", split_at = 0, type = "value")),c(42,4))
  expect_equal(sort(unique(split_strata(data = data_split, strata = "strata", split = "a", split_var = "split_var", split_at = 0, type = "value")$new_strata)),c(paste("a.split_var_(0,", round(max(filter(data_split, strata == "a")$split_var), digits = 2), "]", sep = ""), paste("a.split_var_[", round(min(filter(data_split, strata == "a")$split_var), digits = 2), ",0]", sep = ""),  "b","c"))
})

test_that("splits occur at correct global quantile values",{
  median1 <- strsplit(dplyr::filter(split_strata(data = data_split, strata = "strata", split = "a", split_var = "split_var", split_at = 0.5, type = "global quantile"), split_var < stats::median(split_var), old_strata == "a")$new_strata[1],
           split = "[", fixed = TRUE)[[1]][2] #Extract median from strata name
  expect_equal(substr(median1, start = 7, stop = nchar(median1) - 1 ),
               as.character(round(stats::median(data_split$split_var), digits = 2)))
  expect_equal(as.vector(table(dplyr::filter(split_strata(data = data_split, strata = "strata", split = "a", split_var = "split_var", split_at = 0.5, type = "global quantile"), new_strata %in% c("b","c") == FALSE)$new_strata)),
               as.vector(table(dplyr::filter(data_split, strata == "a")$split_var <= median(data_split$split_var))))
})

test_that("splits occur at correct local quantile values",{
 median2 <- strsplit(dplyr::filter(split_strata(data = data_split, strata = "strata", split = "a", split_var = "split_var", split_at = 0.5, type = "local quantile"), split_var < stats::median(data_split[data_split$strata == "a",]$split_var), old_strata == "a")$new_strata[1],
                     split = "[", fixed = TRUE)[[1]][2]
 expect_equal(substr(median2, start = 7, stop = nchar(median2) - 1 ),
              as.character(round(stats::median(data_split[data_split$strata == "a",]$split_var), digits = 2)))
 expect_equal(as.vector(table(dplyr::filter(split_strata(data = data_split, strata = "strata", split = "a", split_var = "split_var", split_at = 0.5, type = "local quantile"), new_strata %in% c("b","c") == FALSE)$new_strata)),
              as.vector(table(dplyr::filter(data_split, strata == "a")$split_var <= median(dplyr::filter(data_split, strata  == "a")$split_var))))
})

test_that("splits occur at correct categorical split",{
  data_split$split_var2 <- rep(c(rep("alpha", times = 7), rep("beta", times = 7)), times = 3)
  expect_equal(sort(unique(split_strata(data = data_split, strata = "strata", split = "a", split_var = "split_var2", split_at = "alpha", type = "categorical")$new_strata)),
               c("a.split_var2_0","a.split_var2_1", "b","c"))
  expect_equal(sort(as.character(unique(split_strata(data = data_split, strata = "strata", split = NULL, split_var = "split_var2", split_at = "alpha", type = "categorical")$new_strata))),
               c("a.split_var2_0","a.split_var2_1", "b.split_var2_0","b.split_var2_1","c.split_var2_0","c.split_var2_1"))
})

test_that("strata_split can define prior strata based on an interaction of multiple columns",{
  expect_equal(length(unique(split_strata(data = data_split, strata = c("strata","strata2"), split = "a.0", split_var = "split_var", split_at = 0, type = "value")$new_strata)), 7)
})

test_that("when type is a quantile, input must be between 0 and 1 or else an error occurs",{
  expect_error(split_strata(data = data_split, strata = "strata", split = "a", split_var = "split_var", split_at = 1.3, type = "global quantile"), "'probs' outside [0,1]", fixed = TRUE)
})

test_that("when a 'value' outside of the range of values is given, a warning comes up",{
  expect_warning(split_strata(data = data_split, strata = "strata", split = "a", split_var = "split_var", split_at = 5, type = "value"), "value(s) of 'split_at' are outside of the range of values in 'split'", fixed =  TRUE)
})

