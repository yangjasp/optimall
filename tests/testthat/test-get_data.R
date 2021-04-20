context("test-get_data")

library(optimall)
library(dplyr)

# Make multiwave object and add things into slots
MySurvey <- new_multiwave(phases = 2, waves = c(1, 3))

# To write overall metadata
MySurvey@metadata <-
  list(title = "Maternal Weight Survey")

test_that("metadata access works", {
  # overall metadata
  expect_equal(
    get_data(MySurvey, phase = NA, slot = "metadata"),
    MySurvey@metadata
  )

  # To access Phase 1 metadata
  MySurvey@phases$phase1$metadata <-
    list(title = "Maternal Weight Survey Phase 1")

  expect_equal(
    get_data(MySurvey, phase = 1, slot = "metadata"),
    MySurvey@phases$phase1$metadata
  )

  # To access Phase 2metadata

  MySurvey@phases$phase2@metadata <-
    list(title = "Maternal Weight Survey Phase 2")

  expect_equal(
    get_data(MySurvey, phase = 2, slot = "metadata"),
    MySurvey@phases$phase2@metadata
  )


  # To access Phase 2, Wave 1 metadata

  MySurvey@phases$phase2@waves$wave1@metadata <-
    list(title = "Maternal Weight Survey Phase 2, Wave 1")

  expect_equal(
    get_data(MySurvey, phase = 2, wave = 1, slot = "metadata"),
    MySurvey@phases$phase2@waves$wave1@metadata
  )
})

test_that("Writing metadata slots works", {
  get_data(MySurvey, phase = NA, slot = "metadata") <- list(
    title = "test1"
  )
  expect_equal(MySurvey@metadata$title, "test1")

  get_data(MySurvey, phase = 1, slot = "metadata") <- list(
    title = "test2"
  )
  expect_equal(MySurvey@phases$phase1$metadata$title, "test2")

  get_data(MySurvey, phase = 2, wave = 1, slot = "metadata") <- list(
    title = "test3"
  )
  expect_equal(MySurvey@phases$phase2@waves$wave1@metadata$title, "test3")
})

test_that("Design, samples, sampled_data, data work", {

  # Set up

  test <- new_multiwave(phases = 2, waves = c(1, 3))

  set.seed <- 345
  iris <- data.frame(
    id = c(1:60),
    Species = rep(c("A", "B", "C"), times = 20),
    Sepal.Length = rnorm(60, 3, 0.7)
  )
  iris$Sepal.Width <- iris$Sepal.Length + rnorm(60, 0, 0.5)

  get_data(test, phase = 1, slot = "data") <-
    dplyr::select(iris, -Sepal.Width)

  get_data(test, phase = 2, slot = "metadata") <- list(
    strata = "Species",
    design_strata = "strata",
    id = "id",
    n_allocated = "n_to_sample"
  )

  get_data(test, phase = 2, wave = 1, slot = "design") <-
    data.frame(
      strata = unique(iris$Species),
      n_to_sample = c(5, 5, 5)
    )

  set.seed(123)
  test <- apply_multiwave(test,
    phase = 2,
    wave = 1, "sample_strata"
  ) # get samples

  samples <- get_data(test, phase = 2, wave = 1, slot = "samples")

  get_data(test, phase = 2, wave = 1, slot = "sampled_data") <-
    dplyr::select(iris, id, Sepal.Width)[samples, ]

  test <- merge_samples(test,
    phase = 2, wave = 1, id = "id",
    sampled_ind = "already_sampled_ind"
  )

  test <- apply_multiwave(test,
    phase = 2, wave = 2, fun = "allocate_wave",
    y = "Sepal.Width",
    already_sampled = "already_sampled_ind", nsample = 30,
    detailed = TRUE
  )

  expect_equal(
    test@phases$phase1$data,
    get_data(test, phase = 1, slot = "data")
  )

  expect_equal(
    test@phases$phase2@waves$wave2@design,
    get_data(test, phase = 2, wave = 2, slot = "design")
  )

  expect_equal(
    test@phases$phase2@waves$wave1@data,
    get_data(test, phase = 2, wave = 1, slot = "data")
  )
  expect_equal(
    test@phases$phase2@waves$wave1@samples,
    get_data(test, phase = 2, wave = 1, slot = "samples")
  )
  expect_equal(
    test@phases$phase2@waves$wave1@sampled_data,
    get_data(test, phase = 2, wave = 1, slot = "sampled_data")
  )

  # and that writing them with get_data worked
  expect_equal(
    test@phases$phase2@waves$wave1@design,
    data.frame(
      strata = unique(iris$Species),
      n_to_sample = c(5, 5, 5)
    )
  )

  expect_equal(
    test@phases$phase2@waves$wave1@sampled_data,
    dplyr::select(iris, id, Sepal.Width)[samples, ]
  )

  expect_equal(
    test@phases$phase1$data,
    dplyr::select(iris, -Sepal.Width)
  )
})

test_that("errors work when invalid slot is accessed", {
  expect_error(
    get_data(MySurvey, phase = NA, slot = "data"),
    "must specify a phase unless getting overall metadata"
  )

  expect_error(
    get_data(MySurvey, phase = 2, wave = NA, slot = "data"),
    "must specify wave number unless"
  )
  expect_error(
    get_data(MySurvey, phase = NA, slot = "data") <-
      data.frame(),
    "must specify a phase unless getting overall metadata"
  )
  expect_error(
    get_data(MySurvey, phase = 2, wave = NA, slot = "data") <-
      data.frame(),
    "must specify wave number unless"
  )
})
