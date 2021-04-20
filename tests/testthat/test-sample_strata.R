context("test-sample_strata")

library(dplyr)
library(optimall)

set.seed(2343)
data <- data.frame(
  "strata" = c(
    rep("a", times = 15),
    rep("b", times = 15),
    rep("c", times = 12)
  ),
  "y" = c(rnorm(30, sd = 1), rnorm(12, sd = 2)),
  "key" = rbinom(42, 1, 0.2),
  "id" = seq(1:42)
)
data$key[c(1, 16, 31)] <- 1 # To make sure no group gets zero in already_sampled

df <- allocate_wave(
  data = data, strata = "strata",
  already_sampled = "key", y = "y", nsample = 15
)

set.seed(384)
sampled_data <- sample_strata(
  data = data, strata = "strata",
  id = "id", design_data = df,
  already_sampled = "key", design_strata = "strata",
  n_allocated = "n_to_sample"
)

test_that("samples are properly allocated", {
  expect_equal(sum(sampled_data$sample_indicator == 1), 15)
  expect_equal(
    sort(as.vector(
      table(sampled_data[sampled_data$sample_indicator == 1, ]$strata)
    )),
    sort(df$n_to_sample)
  )
})

test_that("no samples in already_sampled are again sampled", {
  expect_equal(any(sampled_data[sampled_data$sample_indicator == 1, ]$id %in%
    data[data$key == 1, ]$id), FALSE)
})

test_that("same thing with new seed yields different sample", {
  set.seed(6589)
  sampled_design_data <- sample_strata(
    data = data, strata = "strata",
    id = "id", design_data = df,
    already_sampled = "key", design_strata = "strata",
    n_allocated = "n_to_sample"
  )
  expect_equal(
    length(setdiff(
      sampled_data[sampled_data$sample_indicator == 1, ]$id,
      sampled_design_data[sampled_design_data$sample_indicator == 1, ]$id
    )) > 0,
    TRUE
  )
})

test_that("works if already_sampled is NULL", {
  design_data <- data
  design_data$key <- rep(0, times = 42)
  df2 <- optimum_allocation(
    data = design_data, strata = "strata",
    y = "y", nsample = 15
  )
  set.seed(384)
  sampled_design_data <- sample_strata(
    data = design_data, strata = "strata",
    id = "id", design_data = df2,
    already_sampled = NULL, design_strata = "strata",
    n_allocated = "stratum_size"
  )
  expect_equal(sum(sampled_design_data$sample_indicator == 1), 15)
  expect_equal(
    sort(as.vector(table(
      sampled_design_data[sampled_design_data$sample_indicator == 1, ]$strata
    ))),
    sort(df2$stratum_size)
  )
})

test_that("Error messages displayed as necessary", {
  x <- c(1, 2, 3, 4, 5)
  expect_error(
    sampled_data <- sample_strata(
      data = x,
      strata = "strata",
      id = "id", design_data = df,
      already_sampled = "key",
      design_strata = "strata",
      n_allocated = "n_to_sample"
    ),
    "'data' and 'design_data' must be a dataframe"
  )
  expect_error(
    sampled_data <- sample_strata(
      data = data,
      strata = "strata_wrong",
      id = "id", design_data = df,
      already_sampled = "key",
      design_strata = "strata",
      n_allocated = "n_to_sample"
    ),
    "'strata' and 'id' must be strings matching a column"
  )
  expect_error(
    sampled_data <- sample_strata(
      data = data,
      strata = "strata",
      id = "id", design_data = df,
      already_sampled = "key",
      design_strata = "strata_wrong",
      n_allocated = "n_to_sample"
    ),
    "'design_strata' and 'n_allocated' must be strings matching"
  )
  df_extra_row <- rbind(df, df[3, ])
  expect_error(
    sampled_data <- sample_strata(
      data = data,
      strata = "strata",
      id = "id",
      design_data = df_extra_row,
      already_sampled = "key",
      design_strata = "strata",
      n_allocated = "n_to_sample"
    ),
    "'design_data' may only contain one row per stratum"
  )
  expect_error(
    sampled_data <- sample_strata(
      data = data,
      strata = "strata",
      id = "id",
      design_data = df,
      already_sampled = "key_wrong",
      design_strata = "strata",
      n_allocated = "n_to_sample"
    ),
    "If not NULL, 'already_sampled' must be a character string"
  )
  data$three_key <- rep(c(1, 2, 3), times = dim(data)[1] / 3)
  expect_error(
    sampled_data <- sample_strata(
      data = data,
      strata = "strata",
      id = "id",
      design_data = df,
      already_sampled = "three_key",
      design_strata = "strata",
      n_allocated = "n_to_sample"
    ),
    "has a binary indicator for whether"
  )
  data$wrong_two_key <- rep(c(2, 3), times = dim(data)[1] / 2)
  expect_error(
    sampled_data <- sample_strata(
      data = data,
      strata = "strata",
      id = "id",
      design_data = df,
      already_sampled = "wrong_two_key",
      design_strata = "strata",
      n_allocated = "n_to_sample"
    ),
    "'already_sampled' column must contain '1'"
  )
  data$wrong_two_key <- c(
    rep(1, times = dim(data)[1] - 6), 0, 0, 0,
    0, 0, 0
  )
  expect_error(
    sampled_data <- sample_strata(
      data = data,
      strata = "strata",
      id = "id",
      design_data = df,
      already_sampled = "wrong_two_key",
      design_strata = "strata",
      n_allocated = "n_to_sample"
    ),
    "Total sample size across waves, taken as"
  )



  df_wrong_strata_name <- df %>%
    dplyr::mutate(strata_new = case_when(
      strata == "a" ~ "a",
      strata == "b" ~ "b",
      strata == "c" ~ "c_2"
    ))
  expect_error(
    sampled_data <- sample_strata(
      data = data,
      strata = "strata",
      id = "id",
      design_data = df_wrong_strata_name,
      already_sampled = "key",
      design_strata = "strata_new",
      n_allocated = "n_to_sample"
    ),
    "strata names in 'design_data' must all match strata"
  )
})

test_that("works if input is a matrix", {
  sampled_data_mat <- sample_strata(
    data = data.matrix(data),
    strata = "strata",
    id = "id",
    design_data = data.matrix(df),
    already_sampled = "key",
    design_strata = "strata",
    n_allocated = "n_to_sample"
  )
  expect_equal(sum(sampled_data_mat$sample_indicator), 15)
})

test_that("returns error if n_allocated is not a whole number", {
  df$n_to_sample <- c(1.5, 2, 4)
  expect_error(
    sample_strata(
      data = data, strata = "strata",
      id = "id", design_data = df,
      already_sampled = "key", design_strata = "strata",
      n_allocated = "n_to_sample"
    ),
    "must specify a numeric column"
  )
  df$n_to_sample <- c("a", "b", "c")
  expect_error(
    sample_strata(
      data = data, strata = "strata",
      id = "id", design_data = df,
      already_sampled = "key", design_strata = "strata",
      n_allocated = "n_to_sample"
    ),
    "must specify a numeric column"
  )
})
