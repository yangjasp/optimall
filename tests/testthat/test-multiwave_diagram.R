context("test-multiwave_diagram")

library(optimall)
library(dplyr)


test_that("multiwave_diagram works", {
  # Create Survey

  MySurvey <- multiwave(phases = 2, waves = c(1, 3))
  set_mw(MySurvey, phase = 1, slot = "data") <-
    dplyr::select(iris, -Sepal.Width)

  set_mw(MySurvey, phase = 2, slot = "metadata") <- list(
    strata = "Species",
    strata2 = "strata",
    id = "id",
    n_allocated = "n_to_sample"
  )

  set_mw(MySurvey, phase = 2, wave = 1, slot = "design") <-
    data.frame(strata = unique(iris$Species), n_to_sample = c(5, 5, 5))
  set.seed(123)
  # MySurvey <- apply_multiwave(MySurvey, phase = 2, wave = 1,
  #                           fun = "sample_strata")

  #  samples <- get_mw(MySurvey, phase = 2, wave = 1, slot = "samples")

  # set_mw(MySurvey, phase = 2, wave = 1, slot = "sampled_data") <-
  #  dplyr::select(iris, id, Sepal.Width)[samples,]
  # MySurvey <- merge_samples(MySurvey, phase = 2, wave = 1, id = "id",
  #                         phase_sample_ind = "already_sampled_ind")
  # MySurvey <- apply_multiwave(MySurvey, phase = 2, wave = 2,
  #                           fun = "allocate_wave", strata = "Species",
  #                          y = "Sepal.Width",
  #                         wave2a = "already_sampled_ind",
  #                        nsample = 30, detailed = TRUE)
  multiwave_diagram(MySurvey)
  expect_visible(multiwave_diagram(MySurvey))
})
