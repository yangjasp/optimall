context("test-summary.multiwave")

MySurvey <- new_multiwave(phases = 2, waves = c(1,2))
get_data(MySurvey, phase = 1, slot = "data") <-
  data.frame(a = c(1,2,3,4), b = c(2,3,4,5))
get_data(MySurvey, phase = 2, wave = 2, slot = "data") <-
  data.frame(a = c(1,2,3), b = c(2,3,4))

test_that("summary works", {
  expect_output(summary(MySurvey), "object with 2 phases containing 1, 2 wav")
  expect_output(summary(MySurvey), "4 observations of 2 vars: a, b")
  expect_output(summary(MySurvey), "3 observations of 2 vars: a, b")

})
