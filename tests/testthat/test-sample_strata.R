context("test-sample_strata")

library(dplyr)
library(optimall)

set.seed(2343)
data <- data.frame("strata" = c(rep("a", times = 15),
                                rep("b", times = 15),
                                rep("c", times = 12)),
                   "y" = c(rnorm(30, sd = 1), rnorm(12, sd = 2)),
                   "key" = rbinom(42, 1, 0.2),
                   "id" = seq(1:42))
data$key[c(1,16,31)] <- 1 #To make sure no group gets zero in wave2a

df <- allocate_wave(data = data, strata = "strata",
                    wave2a = "key", y = "y", nsample = 15)

set.seed(384)
sampled_data <- sample_strata(data1 = data, strata1 = "strata",
                              id = "id", data2 = df,
                              wave2a = "key",strata2 = "strata",
                              n_allocated = "n_to_sample" )

test_that("samples are properly allocated",{
  expect_equal(sum(sampled_data$sample_indicator == 1), 15)
  expect_equal(sort(as.vector(
    table(sampled_data[sampled_data$sample_indicator== 1,]$strata))),
    sort(df$n_to_sample))
})

test_that("no samples in wave2a are again sampled",{
  expect_equal(any(sampled_data[sampled_data$sample_indicator ==1,]$id %in%
                     data[data$key == 1,]$id), FALSE)
})

test_that("same thing with new seed yields different sample",{
  set.seed(6589)
  sampled_data2 <- sample_strata(data1 = data, strata1 = "strata",
                                 id = "id", data2 = df,
                                 wave2a = "key",strata2 = "strata",
                                 n_allocated = "n_to_sample" )
  expect_equal(length(setdiff(
    sampled_data[sampled_data$sample_indicator ==1,]$id,
    sampled_data2[sampled_data2$sample_indicator ==1,]$id)) > 0,
    TRUE)
})

test_that("works if wave2a is NULL",{
  data2 <- data
  data2$key <- rep(0, times = 42)
  df2 <- optimum_allocation(data = data2, strata = "strata",
                            y = "y", nsample = 15)
  set.seed(384)
  sampled_data2 <- sample_strata(data1 = data2, strata1 = "strata",
                                 id = "id", data2 = df2,
                                 wave2a = NULL,strata2 = "strata",
                                 n_allocated = "stratum_size" )
  expect_equal(sum(sampled_data2$sample_indicator == 1), 15)
  expect_equal(sort(as.vector(table(
    sampled_data2[sampled_data2$sample_indicator== 1,]$strata))),
    sort(df2$stratum_size))
})

test_that("Error messages displayed as necessary",{
  x <- c(1,2,3,4,5)
  expect_error(sampled_data <- sample_strata(data1 = x,
                                             strata1 = "strata",
                                             id = "id", data2 = df,
                                             wave2a = "key",
                                             strata2 = "strata",
                                             n_allocated = "n_to_sample"),
               "'data1' and 'data2' must be a dataframe")
  expect_error(sampled_data <- sample_strata(data1 = data,
                                             strata1 = "strata_wrong",
                                             id = "id", data2 = df,
                                             wave2a = "key",
                                             strata2 = "strata",
                                             n_allocated = "n_to_sample"),
               "'strata1' and 'id' must be strings matching a column")
  expect_error(sampled_data <- sample_strata(data1 = data,
                                             strata1 = "strata",
                                             id = "id", data2 = df,
                                             wave2a = "key",
                                             strata2 = "strata_wrong",
                                             n_allocated = "n_to_sample"),
               "'strata2' and 'n_allocated' must be strings matching")
  df_extra_row <- rbind(df,df[3,])
  expect_error(sampled_data <- sample_strata(data1 = data,
                                             strata1 = "strata",
                                             id = "id",
                                             data2 = df_extra_row,
                                             wave2a = "key",
                                             strata2 = "strata",
                                             n_allocated = "n_to_sample"),
               "'data2' may only contain one row per stratum")
  expect_error(sampled_data <- sample_strata(data1 = data,
                                             strata1 = "strata",
                                             id = "id",
                                             data2 = df,
                                             wave2a = "key_wrong",
                                             strata2 = "strata",
                                             n_allocated = "n_to_sample"),
               "If not NULL, 'wave2a' must be a character string")
  data$three_key <- rep(c(1,2,3), times = dim(data)[1]/3)
  expect_error(sampled_data <- sample_strata(data1 = data,
                                             strata1 = "strata",
                                             id = "id",
                                             data2 = df,
                                             wave2a = "three_key",
                                             strata2 = "strata",
                                             n_allocated = "n_to_sample"),
               "has a binary indicator for whether")
  data$wrong_two_key <- rep(c(2,3), times = dim(data)[1]/2)
  expect_error(sampled_data <- sample_strata(data1 = data,
                                             strata1 = "strata",
                                             id = "id",
                                             data2 = df,
                                             wave2a = "wrong_two_key",
                                             strata2 = "strata",
                                             n_allocated = "n_to_sample"),
               "'wave2a' column must contain '1'")
  data$wrong_two_key <- c(rep(1, times = dim(data)[1] - 6), 0, 0, 0,
                          0, 0, 0)
  expect_error(sampled_data <- sample_strata(data1 = data,
                                             strata1 = "strata",
                                             id = "id",
                                             data2 = df,
                                             wave2a = "wrong_two_key",
                                             strata2 = "strata",
                                             n_allocated = "n_to_sample"),
               "Total sample size across waves, taken as")



  df_wrong_strata_name <- df %>%
    dplyr::mutate(strata_new = case_when(strata == "a" ~ "a",
                                         strata == "b" ~ "b",
                                         strata == "c" ~ "c_2"))
  expect_error(sampled_data <- sample_strata(data1 = data,
                                             strata1 = "strata",
                                             id = "id",
                                             data2 = df_wrong_strata_name,
                                             wave2a = "key",
                                             strata2 = "strata_new",
                                             n_allocated = "n_to_sample"),
               "strata names in 'data2' must all match strata")
})

test_that("works if input is a matrix",{
    sampled_data_mat <- sample_strata(data1 = data.matrix(data),
                                  strata1 = "strata",
                                  id = "id",
                                  data2 = data.matrix(df),
                                  wave2a = "key",
                                  strata2 = "strata",
                                  n_allocated = "n_to_sample")
    expect_equal(sum(sampled_data_mat$sample_indicator),15)
})
