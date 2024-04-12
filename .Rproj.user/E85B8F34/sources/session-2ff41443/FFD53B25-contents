context("test-summary.multiwave")

library(optimall)

MySurvey <- multiwave(phases = 2, waves = c(1, 2))
set_mw(MySurvey, phase = 1, slot = "data") <-
  data.frame(a = c(1, 2, 3, 4), b = c(2, 3, 4, 5))
set_mw(MySurvey, phase = 2, wave = 2, slot = "data") <-
  data.frame(a = c(1, 2, 3), b = c(2, 3, 4))
set_mw(MySurvey, phase = 2, wave = 2, slot = "sampled_data") <-
  data.frame(a = c(1, 2, 3), b = c(2, 3, 4))

test_that("summary works", {
  expect_output(summary(MySurvey), "object with 2 phases containing 1, 2 wav")
  expect_output(summary(MySurvey), "4 obs. of 2 vars: a, b")
  expect_output(summary(MySurvey), "3 obs. of 2 vars: a, b")
})

MySurvey <- multiwave(phases = 2, waves = c(1, 2))
set_mw(MySurvey, phase = 1, slot = "data") <-
  data.frame(a = c(1, 2, 3, 4), b = c(2, 3, 4, 5))
set_mw(MySurvey, phase = 2, wave = 2, slot = "data") <-
  data.frame(a = c(1, 2, 3), b = c(2, 3, 4))
set_mw(MySurvey, phase = 2, wave = 2, slot = "sampled_data") <-
  data.frame(a = c(1, 2, 3), b = c(2, 3, 4))
set_mw(MySurvey, phase = 2, wave = 2, slot = "design") <-
  data.frame(a = c(1, 2, 3), b = c(2, 3, 4), c = c(6, 7, 8))
set_mw(MySurvey, phase = 2, wave = 2, slot = "samples") <-
  list(ids = c("4", "7", "11"), probs = c(.3,.3,.3))

test_that("summary works with design and samples", {
  expect_output(summary(MySurvey), "object with 2 phases containing 1, 2 wav")
  expect_output(summary(MySurvey), "3 obs. of 3 vars: a, b")
  expect_output(summary(MySurvey), "list of length 2")
})
