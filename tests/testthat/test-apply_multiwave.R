context("test-apply_multiwave")

library(optimall)
library(dplyr)

set.seed <- 345
iris <- data.frame(
  id = c(1:60),
  Species = rep(c("A", "B", "C"), times = 20),
  Sepal.Length = rnorm(60, 3, 0.7)
)
iris$Sepal.Width <- iris$Sepal.Length + rnorm(60, 0, 0.5)

# 1. Optimum_allocation

test_that("optimum_allocation runs with args provided", {
  MySurvey <- new_multiwave(phases = 2, waves = c(1, 3))
  get_data(MySurvey, phase = 1, slot = "data") <-
    dplyr::select(iris, -Sepal.Width)
  MySurvey <- apply_multiwave(MySurvey,
    phase = 2, wave = 1,
    fun = "optimum_allocation", strata = "Species", nsample = 15,
    y = "Sepal.Length",
    method = "WrightII",
    ndigits = 2,
    allow.na = FALSE
  )
  expect_equal(
    dim(get_data(MySurvey,
      phase = 2, wave = 1,
      slot = "design"
    )),
    c(3, 6)
  )
  expect_equal(
    sum(get_data(MySurvey,
      phase = 2, wave = 1,
      slot = "design"
    )$stratum_size),
    15
  )
  MySurvey <- apply_multiwave(MySurvey,
    phase = 2, wave = 1,
    fun = "optimum_allocation", strata = "Species",
    y = "Sepal.Length",
    method = "Neyman"
  )
  expect_equal(
    dim(get_data(MySurvey,
      phase = 2, wave = 1,
      slot = "design"
    )),
    c(3, 5)
  )
})

test_that("optimum_allocation runs with args in metadata", {
  MySurvey <- new_multiwave(phases = 2, waves = c(1, 3))
  get_data(MySurvey, phase = 1, slot = "data") <-
    dplyr::select(iris, -Sepal.Width)
  get_data(MySurvey, phase = NA, slot = "metadata") <- list(
    strata = "Species",
    nsample = 15,
    y = "Sepal.Length",
    method = "WrightII",
    ndigits = 2,
    allow.na = FALSE
  )
  MySurvey <- apply_multiwave(MySurvey,
    phase = 2, wave = 1,
    fun = "optimum_allocation"
  )
  expect_equal(
    dim(get_data(MySurvey,
      phase = 2, wave = 1,
      slot = "design"
    )),
    c(3, 6)
  )
  expect_equal(
    sum(get_data(MySurvey,
      phase = 2, wave = 1,
      slot = "design"
    )$stratum_size),
    15
  )

  # but metadata in phase overrides
  get_data(MySurvey, phase = 2, slot = "metadata") <- list(
    strata = "Species",
    nsample = 20,
    y = "Sepal.Length",
    method = "WrightII", ndigits = 2,
    allow.na = FALSE
  )
  MySurvey <- apply_multiwave(MySurvey,
    phase = 2, wave = 1,
    fun = "optimum_allocation"
  )
  expect_equal(
    dim(get_data(MySurvey,
      phase = 2, wave = 1,
      slot = "design"
    )),
    c(3, 6)
  )
  expect_equal(
    sum(get_data(MySurvey,
      phase = 2, wave = 1,
      slot = "design"
    )$stratum_size),
    20
  )

  # And wave overrides that
  get_data(MySurvey,
    phase = 2, wave = 1,
    slot = "metadata"
  ) <- list(
    strata = "Species",
    nsample = 30,
    y = "Sepal.Length",
    method = "WrightII", ndigits = 2,
    allow.na = FALSE
  )
  MySurvey <- apply_multiwave(MySurvey,
    phase = 2, wave = 1,
    fun = "optimum_allocation"
  )
  expect_equal(
    dim(get_data(MySurvey,
      phase = 2, wave = 1,
      slot = "design"
    )),
    c(3, 6)
  )
  expect_equal(
    sum(get_data(MySurvey,
      phase = 2, wave = 1,
      slot = "design"
    )$stratum_size),
    30
  )
})

test_that("optimum_allocation works for wave 2 also", {
  MySurvey <- new_multiwave(phases = 2, waves = c(1, 3))
  get_data(MySurvey, phase = 2, wave = 1, slot = "data") <- iris
  MySurvey <- apply_multiwave(MySurvey,
    phase = 2, wave = 2,
    fun = "optimum_allocation", strata = "Species",
    nsample = 15,
    y = "Sepal.Length",
    method = "WrightII",
    ndigits = 2,
    allow.na = FALSE
  )
  expect_equal(
    dim(get_data(MySurvey,
      phase = 2, wave = 2,
      slot = "design"
    )),
    c(3, 6)
  )
})

# 2. Allocate_wave

test_that("allocate_wave runs with args provided", {
  MySurvey <- new_multiwave(phases = 2, waves = c(1, 3))
  get_data(MySurvey, phase = 1, slot = "data") <-
    dplyr::select(iris, -Sepal.Width)

  get_data(MySurvey, phase = 2, slot = "metadata") <- list(
    strata = "Species",
    design_strata = "strata",
    id = "id",
    n_allocated = "n_to_sample"
  )

  get_data(MySurvey, phase = 2, wave = 1, slot = "design") <-
    data.frame(strata = unique(iris$Species), n_to_sample = c(5, 5, 5))
  set.seed(123)
  MySurvey <- apply_multiwave(MySurvey,
    phase = 2, wave = 1,
    fun = "sample_strata"
  )

  samples <- get_data(MySurvey, phase = 2, wave = 1, slot = "samples")

  get_data(MySurvey, phase = 2, wave = 1, slot = "sampled_data") <-
    dplyr::select(iris, id, Sepal.Width)[samples, ]
  MySurvey <- merge_samples(MySurvey,
    phase = 2, wave = 1, id = "id",
    sampled_ind = "already_sampled_ind"
  )
  MySurvey <- apply_multiwave(MySurvey,
    phase = 2, wave = 2,
    fun = "allocate_wave", strata = "Species",
    y = "Sepal.Width",
    already_sampled = "already_sampled_ind",
    nsample = 30, detailed = TRUE
  )
  expect_equal(
    dim(get_data(MySurvey,
      phase = 2, wave = 2,
      slot = "design"
    )),
    c(3, 7)
  )
  expect_equal(
    sum(get_data(MySurvey,
      phase = 2, wave = 1,
      slot = "design"
    )$n_to_sample),
    15
  )
})

test_that("allocate_wave runs with args in metadata", {
  MySurvey <- new_multiwave(phases = 2, waves = c(1, 3))
  get_data(MySurvey, phase = 1, slot = "data") <-
    dplyr::select(iris, -Sepal.Width)

  get_data(MySurvey, phase = 2, slot = "metadata") <- list(
    strata = "Species",
    design_strata = "strata",
    id = "id",
    n_allocated = "n_to_sample"
  )

  get_data(MySurvey, phase = 2, wave = 1, slot = "design") <-
    data.frame(strata = unique(iris$Species), n_to_sample = c(5, 5, 5))
  set.seed(123)
  MySurvey <- apply_multiwave(MySurvey,
    phase = 2, wave = 1,
    fun = "sample_strata"
  )

  samples <- get_data(MySurvey, phase = 2, wave = 1, slot = "samples")

  get_data(MySurvey, phase = 2, wave = 1, slot = "sampled_data") <-
    dplyr::select(iris, id, Sepal.Width)[samples, ]
  MySurvey <- merge_samples(MySurvey,
    phase = 2, wave = 1, id = "id",
    sampled_ind = "already_sampled_ind"
  )
  get_data(MySurvey, phase = NA, slot = "metadata") <- list(
    y = "Sepal.Width",
    already_sampled = "already_sampled_ind",
    strata = "Species",
    nsample = 30, detailed = TRUE
  )
  get_data(MySurvey, phase = 2, wave = 2, slot = "metadata") <- list()
  get_data(MySurvey, phase = 2, slot = "metadata") <- list()
  MySurvey <- apply_multiwave(MySurvey,
    phase = 2, wave = 2,
    fun = "allocate_wave"
  )
  expect_equal(
    dim(get_data(MySurvey,
      phase = 2, wave = 2,
      slot = "design"
    )),
    c(3, 7)
  )
  expect_equal(
    sum(get_data(MySurvey,
      phase = 2, wave = 2,
      slot = "design"
    )$n_to_sample),
    30
  )
  # but metadata in phase overrides
  get_data(MySurvey, phase = 2, slot = "metadata") <- list(
    y = "Sepal.Width",
    already_sampled = "already_sampled_ind",
    strata = "Species",
    nsample = 32, detailed = TRUE
  )
  MySurvey <- apply_multiwave(MySurvey,
    phase = 2, wave = 2,
    fun = "allocate_wave"
  )
  expect_equal(
    dim(get_data(MySurvey,
      phase = 2, wave = 2,
      slot = "design"
    )),
    c(3, 7)
  )
  expect_equal(
    sum(get_data(MySurvey,
      phase = 2, wave = 2,
      slot = "design"
    )$n_to_sample),
    32
  )

  # And wave overrides that
  get_data(MySurvey,
    phase = 2, wave = 2,
    slot = "metadata"
  ) <- list(
    y = "Sepal.Width",
    already_sampled = "already_sampled_ind",
    strata = "Species",
    nsample = 33, detailed = TRUE
  )
  MySurvey <- apply_multiwave(MySurvey,
    phase = 2, wave = 2,
    fun = "allocate_wave"
  )
  expect_equal(
    dim(get_data(MySurvey,
      phase = 2, wave = 2,
      slot = "design"
    )),
    c(3, 7)
  )
  expect_equal(
    sum(get_data(MySurvey,
      phase = 2, wave = 2,
      slot = "design"
    )$n_to_sample),
    33
  )
})

test_that("error if phase = 1", {
  MySurvey <- new_multiwave(phases = 2, waves = c(1, 3))
  get_data(MySurvey, phase = 1, slot = "data") <-
    dplyr::select(iris, -Sepal.Width)
  expect_error(
    apply_multiwave(MySurvey,
      phase = 1, wave = 1,
      fun = "allocate_wave"
    ),
    "Allocate wave cannot be performed in Phase 1"
  )
})

test_that("errors if args are not specified", {
  MySurvey <- new_multiwave(phases = 2, waves = c(1, 3))
  get_data(MySurvey, phase = 1, slot = "data") <-
    dplyr::select(iris, -Sepal.Width)

  get_data(MySurvey, phase = 2, slot = "metadata") <- list(
    strata = "Species",
    design_strata = "strata",
    id = "id",
    n_allocated = "n_to_sample"
  )

  get_data(MySurvey, phase = 2, wave = 1, slot = "design") <-
    data.frame(strata = unique(iris$Species), n_to_sample = c(5, 5, 5))
  set.seed(123)
  MySurvey <- apply_multiwave(MySurvey,
    phase = 2, wave = 1,
    fun = "sample_strata"
  )

  samples <- get_data(MySurvey, phase = 2, wave = 1, slot = "samples")

  get_data(MySurvey, phase = 2, wave = 1, slot = "sampled_data") <-
    dplyr::select(iris, id, Sepal.Width)[samples, ]
  MySurvey <- merge_samples(MySurvey,
    phase = 2, wave = 1, id = "id",
    sampled_ind = "already_sampled_ind"
  )
  get_data(MySurvey, phase = 2, slot = "metadata") <- list()

  expect_error(
    apply_multiwave(MySurvey,
      phase = 2, wave = 2,
      fun = "allocate_wave",
      y = "Sepal.Width",
      already_sampled = "already_sampled_ind",
      nsample = 30, detailed = TRUE
    ),
    "must be specified or available in metadata"
  )

  expect_error(
    apply_multiwave(MySurvey,
      phase = 2, wave = 2,
      fun = "allocate_wave",
      y = "Sepal.Width", strata = "Species",
      nsample = 30, detailed = TRUE
    ),
    "must be specified or available in metadata"
  )


  expect_error(
    apply_multiwave(MySurvey,
      phase = 2, wave = 2,
      fun = "allocate_wave",
      y = "Sepal.Width",
      already_sampled = "already_sampled_ind",
      strata = "Species", detailed = TRUE
    ),
    "must be specified or available in metadata"
  )

  expect_error(
    apply_multiwave(MySurvey,
      phase = 2, wave = 2,
      fun = "allocate_wave",
      strata = "Species",
      already_sampled = "already_sampled_ind",
      nsample = 30, detailed = TRUE
    ),
    "must be specified or available in metadata"
  )
})

# 3. sample_strata

test_that("sample_strata works with specified args", {
  MySurvey <- new_multiwave(phases = 2, waves = c(1, 3))
  get_data(MySurvey, phase = 1, slot = "data") <-
    dplyr::select(iris, -Sepal.Width)

  get_data(MySurvey, phase = 2, wave = 1, slot = "design") <-
    data.frame(strata = unique(iris$Species), n_to_sample = c(5, 5, 5))
  set.seed(123)
  MySurvey <- apply_multiwave(MySurvey,
    phase = 2, wave = 1,
    fun = "sample_strata", strata = "Species",
    design_strata = "strata",
    id = "id",
    n_allocated = "n_to_sample"
  )
  expect_equal(length(
    get_data(MySurvey, phase = 2, wave = 1, slot = "samples")
  ), 15)
})

test_that("sample_strata works with args specified in metadata", {
  MySurvey <- new_multiwave(phases = 2, waves = c(1, 3))
  get_data(MySurvey, phase = 1, slot = "data") <-
    dplyr::select(iris, -Sepal.Width)

  get_data(MySurvey, phase = NA, slot = "metadata") <- list(
    strata = "Species",
    design_strata = "strata",
    id = "id",
    n_allocated = "n_to_sample"
  )

  get_data(MySurvey, phase = 2, wave = 1, slot = "design") <-
    data.frame(strata = unique(iris$Species), n_to_sample = c(5, 5, 5))
  set.seed(123)
  MySurvey <- apply_multiwave(MySurvey,
    phase = 2, wave = 1,
    fun = "sample_strata"
  )
  expect_equal(length(
    get_data(MySurvey, phase = 2, wave = 1, slot = "samples")
  ), 15)

  # only need to specify strata once if it is same in both

  get_data(MySurvey, phase = NA, slot = "metadata") <- list(
    strata = "Species",
    id = "id",
    n_allocated = "n_to_sample"
  )

  get_data(MySurvey, phase = 2, wave = 1, slot = "design") <-
    data.frame(Species = unique(iris$Species), n_to_sample = c(5, 5, 5))
  set.seed(123)
  MySurvey <- apply_multiwave(MySurvey,
    phase = 2, wave = 1,
    fun = "sample_strata"
  )
  expect_equal(length(
    get_data(MySurvey, phase = 2, wave = 1, slot = "samples")
  ), 15)

  # but metadata in phase overrides
  get_data(MySurvey, phase = 2, slot = "metadata") <- list(
    strata = "Species",
    design_strata = "strata",
    id = "id",
    n_allocated = "n_to_sample"
  )

  get_data(MySurvey, phase = 2, wave = 1, slot = "design") <-
    data.frame(strata = unique(iris$Species), n_to_sample = c(6, 5, 5))
  set.seed(123)
  MySurvey <- apply_multiwave(MySurvey,
    phase = 2, wave = 1,
    fun = "sample_strata"
  )
  expect_equal(length(
    get_data(MySurvey, phase = 2, wave = 1, slot = "samples")
  ), 16)

  # again, only need to specify strata once if it is same in both

  get_data(MySurvey, phase = 2, slot = "metadata") <- list(
    strata = "Species",
    id = "id",
    n_allocated = "n_to_sample"
  )

  get_data(MySurvey, phase = 2, wave = 1, slot = "design") <-
    data.frame(Species = unique(iris$Species), n_to_sample = c(6, 5, 5))
  set.seed(123)
  MySurvey <- apply_multiwave(MySurvey,
    phase = 2, wave = 1,
    fun = "sample_strata"
  )
  expect_equal(length(
    get_data(MySurvey, phase = 2, wave = 1, slot = "samples")
  ), 16)

  # And wave overrides that
  get_data(MySurvey, phase = 2, wave = 1, slot = "metadata") <- list(
    strata = "Species",
    design_strata = "strata",
    id = "id",
    n_allocated = "n_to_sample"
  )

  get_data(MySurvey, phase = 2, wave = 1, slot = "design") <-
    data.frame(strata = unique(iris$Species), n_to_sample = c(6, 6, 5))
  set.seed(123)
  MySurvey <- apply_multiwave(MySurvey,
    phase = 2, wave = 1,
    fun = "sample_strata"
  )
  expect_equal(length(
    get_data(MySurvey, phase = 2, wave = 1, slot = "samples")
  ), 17)

  # only need to specify strata once if it is same in both

  get_data(MySurvey, phase = 2, wave = 1, slot = "metadata") <- list(
    strata = "Species",
    id = "id",
    n_allocated = "n_to_sample"
  )

  get_data(MySurvey, phase = 2, wave = 1, slot = "design") <-
    data.frame(Species = unique(iris$Species), n_to_sample = c(7, 5, 5))
  set.seed(123)
  MySurvey <- apply_multiwave(MySurvey,
    phase = 2, wave = 1,
    fun = "sample_strata"
  )
  expect_equal(length(
    get_data(MySurvey, phase = 2, wave = 1, slot = "samples")
  ), 17)
})

test_that("errors in sample_strata if args don't match", {
  MySurvey <- new_multiwave(phases = 2, waves = c(1, 3))
  get_data(MySurvey, phase = 1, slot = "data") <-
    dplyr::select(iris, -Sepal.Width)

  # If empty design
  get_data(MySurvey, phase = 2, wave = 1, slot = "design") <-
    data.frame()
  set.seed(123)
  expect_error(
    apply_multiwave(MySurvey,
      phase = 2, wave = 1,
      fun = "sample_strata", strata = "Species",
      design_strata = "strata",
      id = "id",
      n_allocated = "n_to_sample"
    ),
    "of specified wave must be filled"
  )

  # If empty data
  get_data(MySurvey, phase = 1, slot = "data") <-
    data.frame()

  get_data(MySurvey, phase = 2, wave = 1, slot = "design") <-
    data.frame(strata = unique(iris$Species), n_to_sample = c(5, 5, 5))


  expect_error(
    apply_multiwave(MySurvey,
      phase = 2, wave = 1,
      fun = "sample_strata", strata = "Species",
      design_strata = "strata",
      id = "id",
      n_allocated = "n_to_sample"
    ),
    "of previous wave must contain data to be used"
  )

  get_data(MySurvey, phase = 1, slot = "data") <-
    dplyr::select(iris, -Sepal.Width)

  expect_error(
    apply_multiwave(MySurvey,
      phase = 2, wave = 1,
      fun = "sample_strata", strata = NULL,
      design_strata = "strata",
      id = "id",
      n_allocated = "n_to_sample"
    ),
    "must be specified"
  )
  expect_error(
    apply_multiwave(MySurvey,
      phase = 2, wave = 1,
      fun = "sample_strata", strata = "Species",
      design_strata = "wrong",
      id = "id",
      n_allocated = "n_to_sample"
    ),
    "must be a column name"
  )
  expect_error(
    apply_multiwave(MySurvey,
      phase = 2, wave = 1,
      fun = "sample_strata", strata = "Species",
      design_strata = "strata", id = NULL,
      n_allocated = "n_to_sample"
    ),
    "must be specified or available"
  )
  expect_error(
    apply_multiwave(MySurvey,
      phase = 2, wave = 1,
      fun = "sample_strata", strata = "Species",
      design_strata = "strata",
      already_sampled = NULL,
      id = "id",
      n_allocated = "wrong"
    ),
    "must be a column name"
  )
  expect_error(
    apply_multiwave(MySurvey,
      phase = 2, wave = 1,
      fun = "sample_strata", strata = "Species",
      design_strata = "strata",
      already_sampled = "wrong",
      id = "id"
    ),
    "must be a column name"
  )
})

# 4. merge_samples

test_that("merge_samples works when args are specified within it", {
  MySurvey <- new_multiwave(phases = 2, waves = c(1, 3))

  get_data(MySurvey, phase = 1, slot = "data") <-
    dplyr::select(iris, -Sepal.Width)

  get_data(MySurvey, phase = 2, slot = "metadata") <- list(
    strata = "Species",
    design_strata = "strata",
    id = "id",
    n_allocated = "n_to_sample"
  )


  get_data(MySurvey, phase = 2, wave = 1, slot = "design") <-
    data.frame(
      strata = unique(iris$Species),
      n_to_sample = c(5, 5, 5)
    )

  set.seed(123)
  MySurvey <- apply_multiwave(MySurvey,
    phase = 2,
    wave = 1, "sample_strata"
  ) # get samples

  samples <- get_data(MySurvey, phase = 2, wave = 1, slot = "samples")

  get_data(MySurvey, phase = 2, wave = 1, slot = "sampled_data") <-
    dplyr::select(iris, id, Sepal.Width)[samples, ]

  MySurvey <- apply_multiwave(MySurvey,
    phase = 2, wave = 1,
    fun = "merge_samples", id = "id",
    sampled_ind = "already_sampled_ind"
  )

  expect_equal(
    dim(MySurvey@phases$phase2@waves$wave1@data),
    c(60, 5)
  )
  expect_equal(length(
    MySurvey@phases$phase2@waves$wave1@data$`already_sampled_ind`[
      MySurvey@phases$phase2@waves$wave1@data$`already_sampled_ind` == 1
    ]
  ), 15)
  expect_equal(length(
    MySurvey@phases$phase2@waves$wave1@data$`already_sampled_ind`[
      !is.na(MySurvey@phases$phase2@waves$wave1@data$Sepal.Width)
    ]
  ), 15)
})

test_that("merge_samples works with args specifies in metadata", {
  MySurvey <- new_multiwave(phases = 2, waves = c(1, 3))

  get_data(MySurvey, phase = 1, slot = "data") <-
    dplyr::select(iris, -Sepal.Width)

  get_data(MySurvey, phase = NA, slot = "metadata") <- list(
    strata = "Species",
    design_strata = "strata",
    id = "id",
    n_allocated = "n_to_sample"
  )


  get_data(MySurvey, phase = 2, wave = 1, slot = "design") <-
    data.frame(
      strata = unique(iris$Species),
      n_to_sample = c(5, 5, 5)
    )

  set.seed(123)
  MySurvey <- apply_multiwave(MySurvey,
    phase = 2,
    wave = 1, "sample_strata"
  ) # get samples

  samples <- get_data(MySurvey, phase = 2, wave = 1, slot = "samples")

  get_data(MySurvey, phase = 2, wave = 1, slot = "sampled_data") <-
    dplyr::select(iris, id, Sepal.Width)[samples, ]

  get_data(MySurvey, phase = NA, slot = "metadata") <- list(
    id = "id",
    sampled_ind = "already_sampled_ind"
  )

  MySurvey <- apply_multiwave(MySurvey,
    phase = 2, wave = 1,
    fun = "merge_samples"
  )

  expect_equal(
    dim(MySurvey@phases$phase2@waves$wave1@data),
    c(60, 5)
  )
  expect_equal(length(
    MySurvey@phases$phase2@waves$wave1@data$`already_sampled_ind`[
      MySurvey@phases$phase2@waves$wave1@data$`already_sampled_ind` == 1
    ]
  ), 15)
  expect_equal(length(
    MySurvey@phases$phase2@waves$wave1@data$`already_sampled_ind`[
      !is.na(MySurvey@phases$phase2@waves$wave1@data$Sepal.Width)
    ]
  ), 15)

  # But phase metadata overrides it
  get_data(MySurvey, phase = 2, slot = "metadata") <- list(
    id = "id",
    sampled_ind = "already_sampled_ind1"
  )

  MySurvey <- apply_multiwave(MySurvey,
    phase = 2, wave = 1,
    fun = "merge_samples"
  )

  expect_equal(
    dim(MySurvey@phases$phase2@waves$wave1@data),
    c(60, 5)
  )
  expect_equal(length(
    MySurvey@phases$phase2@waves$wave1@data$`already_sampled_ind1`[
      MySurvey@phases$phase2@waves$wave1@data$`already_sampled_ind1` == 1
    ]
  ), 15)
  expect_equal(length(
    MySurvey@phases$phase2@waves$wave1@data$`already_sampled_ind1`[
      !is.na(MySurvey@phases$phase2@waves$wave1@data$Sepal.Width)
    ]
  ), 15)

  # But wave metadata overrides it
  get_data(MySurvey, phase = 2, wave = 1, slot = "metadata") <- list(
    id = "id",
    sampled_ind = "already_sampled_ind2"
  )

  MySurvey <- apply_multiwave(MySurvey,
    phase = 2, wave = 1,
    fun = "merge_samples"
  )

  expect_equal(
    dim(MySurvey@phases$phase2@waves$wave1@data),
    c(60, 5)
  )
  expect_equal(length(
    MySurvey@phases$phase2@waves$wave1@data$`already_sampled_ind2`[
      MySurvey@phases$phase2@waves$wave1@data$`already_sampled_ind2` == 1
    ]
  ), 15)
  expect_equal(length(
    MySurvey@phases$phase2@waves$wave1@data$`already_sampled_ind2`[
      !is.na(MySurvey@phases$phase2@waves$wave1@data$Sepal.Width)
    ]
  ), 15)
})
