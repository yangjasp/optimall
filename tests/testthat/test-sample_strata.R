context("test-sample_strata")

library(dplyr)
library(optimall)

set.seed(2343)
data <- data.frame("strata" = c(rep("a", times = 15), rep("b", times = 15), rep("c", times = 12)),
                   "y" = c(rnorm(30, sd = 1), rnorm(12, sd = 2)),
                   "key" = rbinom(42, 1, 0.2),
                   "id" = seq(1:42))
data$key[c(1,16,31)] <- 1 #To make sure no group gets zero in wave2a

df <- allocate_wave(data = data, strata = "strata", wave2a = "key", y = "y", nsample = 15)

set.seed(384)
sampled_data <- sample_strata(data1 = data, strata1 = "strata", id = "id", data2 = df, wave2a = "key",strata2 = "strata", n_allocated = "n_to_sample" )

test_that("samples are properly allocated",{
  expect_equal(sum(sampled_data$sample_indicator == 1), 15)
  expect_equal(sort(as.vector(table(sampled_data[sampled_data$sample_indicator== 1,]$strata))), sort(df$n_to_sample))
})

test_that("no samples in wave2a are again sampled",{
  expect_equal(any(sampled_data[sampled_data$sample_indicator ==1,]$id %in%
                     data[data$key == 1,]$id), FALSE)
})

test_that("same thing with new seed yields different sample",{
  set.seed(6589)
  sampled_data2 <- sample_strata(data1 = data, strata1 = "strata", id = "id", data2 = df, wave2a = "key",strata2 = "strata", n_allocated = "n_to_sample" )
  expect_equal(length(setdiff(sampled_data[sampled_data$sample_indicator ==1,]$id, sampled_data2[sampled_data2$sample_indicator ==1,]$id)) > 0, TRUE)
})
