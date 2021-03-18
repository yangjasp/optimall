context("test-multiwave_diagram")

#Create Survey

MySurvey <- new_multiwave(phases = 2, waves = c(1,3))

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
