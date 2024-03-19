context("test-set_mw")

library(optimall)
library(dplyr)


# Make multiwave object and add things into slots
MySurvey <- multiwave(phases = 2, waves = c(1, 3))

# To write overall metadata
set_mw(MySurvey, slot = "metadata") <-
  list(title = "Maternal Weight Survey")

test_that("metadata access works", {
  # overall metadata
  expect_equal(
    get_mw(MySurvey, phase = NA, slot = "metadata"),
    MySurvey@metadata
  )

  # To access Phase 1 metadata
  set_mw(MySurvey, phase = 1, slot = "metadata") <-
    list(title = "Maternal Weight Survey Phase 1")

  expect_equal(
    get_mw(MySurvey, phase = 1, slot = "metadata"),
    MySurvey@phases$phase1$metadata
  )

  # To access Phase 2 metadata

  set_mw(MySurvey, phase = 2, slot = "metadata") <-
    list(title = "Maternal Weight Survey Phase 2")

  expect_equal(
    get_mw(MySurvey, phase = 2, slot = "metadata"),
    MySurvey@phases$phase2@metadata
  )


  # To access Phase 2, Wave 1 metadata

  set_mw(MySurvey, phase = 1, wave = 1, slot = "metadata") <-
    list(title = "Maternal Weight Survey Phase 2, Wave 1")

  expect_equal(
    get_mw(MySurvey, phase = 2, wave = 1, slot = "metadata"),
    MySurvey@phases$phase2@waves$wave1@metadata
  )
})

## See test-get_mw for other tests

