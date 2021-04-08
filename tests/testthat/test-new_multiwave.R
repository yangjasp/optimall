context("test-new_multiwave")

library(optimall)
library(dplyr)

test <- new_multiwave(phases = 2, waves = c(1, 3))

test_that("new_multiwave makes object with proper slots", {
  expect_equal(length(test@phases), 2)
  expect_equal(length(test@phases$phase2@waves), 3)
})

test_that("initiating with metadata and phase1 data works", {
  test <- new_multiwave(
    phases = 2, waves = c(1, 3),
    phase1 = data.frame(
      a = c("a", "b", "c"),
      b = c(1, 2, 3)
    ),
    metadata = list(title = "title")
  )
  expect_equal(get_data(test, phase = 1), data.frame(
    a = c("a", "b", "c"),
    b = c(1, 2, 3)
  ))
  expect_equal(
    get_data(test, phase = NA, slot = "metadata"),
    list(title = "title")
  )
})

test_that("errors work", {
  # Too many waves in phase 1
  expect_error(
    new_multiwave(phases = 2, waves = c(2, 3)),
    "phase 1 can only contain"
  )
  expect_error(
    new_multiwave(phases = 2, waves = c(2, 3, 4)),
    "must be a numeric vector with length"
  )
  expect_error(
    new_multiwave(phases = 0, waves = c(2, 3, 4)),
    "must be a numeric value"
  )
})

test_that("single phase is possible", {
  test <- new_multiwave(phases = 1, waves = c(1))
  expect_equal(length(test@phases), 1)
})
