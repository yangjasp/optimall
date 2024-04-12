context("test-merge_samples")

library(dplyr)
library(optimall)

# Make multiwave_object
test <- multiwave(phases = 2, waves = c(1, 3))

set.seed <- 345
iris <- data.frame(
  id = c(1:60),
  Species = rep(c("A", "B", "C"), times = 20),
  Sepal.Length = rnorm(60, 3, 0.7)
)
iris$Sepal.Width <- iris$Sepal.Length + rnorm(60, 0, 0.5)

set_mw(test, phase = 1, slot = "data") <-
  dplyr::select(iris, -Sepal.Width)

set_mw(test, phase = 2, slot = "metadata") <- list(
  strata = "Species",
  design_strata = "strata",
  id = "id",
  n_allocated = "n_to_sample"
)


set_mw(test, phase = 2, wave = 1, slot = "design") <-
  data.frame(
    strata = unique(iris$Species),
    n_to_sample = c(5, 5, 5),
    probs = c(.25, .25, .25)
  )

set.seed(0123)
test <- apply_multiwave(test,
  phase = 2,
  wave = 1, "sample_strata", probs = "probs"
) # get samples

samples <- get_mw(test, phase = 2, wave = 1, slot = "samples")$ids

set_mw(test, phase = 2, wave = 1, slot = "sampled_data") <-
  dplyr::select(iris, id, Sepal.Width)[samples, ]

# Testing Wave 1
test <- merge_samples(test,
  phase = 2, wave = 1, id = "id", include_probs = TRUE
)


test_that("merge_samples merges data with sampled data
          when column doesn't already exist", {
  expect_equal(
    dim(test@phases$phase2@waves$wave1@data),
    c(60, 7)
  )
  expect_equal(length(
    test@phases$phase2@waves$wave1@data$`sampled_phase2`[
      test@phases$phase2@waves$wave1@data$`sampled_phase2` == 1
    ]
  ), 15)
  expect_equal(length(
    test@phases$phase2@waves$wave1@data$`sampled_phase2`[
      !is.na(test@phases$phase2@waves$wave1@data$Sepal.Width)
    ]
  ), 15)
})

# Testing Wave 2
samples2 <- sample(c(1:60)[-as.numeric(samples)], 15)
# No duplicates
set_mw(test, phase = 2, wave = 2, slot = "samples") <-
  list(ids = samples2) # No duplicates
set_mw(test, phase = 2, wave = 2, slot = "sampled_data") <-
  dplyr::select(iris, id, Sepal.Width)[samples2, ]
test1 <- merge_samples(test,
  phase = 2, wave = 2, id = "id"
)

test_that("merge_samples merges data with sampled data
          when column already exists", {
  expect_equal(
    dim(test1@phases$phase2@waves$wave1@data),
    c(60, 7)
  )
  expect_equal(length(
    test1@phases$phase2@waves$wave2@data$`sampled_phase2`[
      test1@phases$phase2@waves$wave2@data$`sampled_phase2` == 1
    ]
  ), 30)
  expect_equal(length(
    test1@phases$phase2@waves$wave2@data$`sampled_phase2`[
      !is.na(test1@phases$phase2@waves$wave2@data$Sepal.Width)
    ]
  ), 30)
})

test_that("warning if id already has value for what has been sampled", {
  temp <- test1
  set_mw(temp, phase = 2, wave = 2, slot = "sampled_data") <-
    dplyr::select(iris, id, Sepal.Width)[samples2 + as.numeric(samples[1]), ]
  # Make dup
  set_mw(temp, phase = 2, wave = 2, slot = "samples") <-
    list(ids = c(samples2, samples[1])) # Make dup
  expect_warning(
    merge_samples(temp,
      phase = 2, wave = 2, id = "id"
    ),
    "have non-NA values already"
  )
})

test_that("warning if new ids are in sampled data", {
  temp <- test1
  set_mw(temp, phase = 2, wave = 2, slot = "sampled_data") <-
    rbind(
      dplyr::select(iris, id, Sepal.Width)[samples2, ],
      c(61, 3.5)
    )
  # Make dup
  set_mw(temp, phase = 2, wave = 2, slot = "samples") <-
    list(ids = c(samples2, "61")) # Make dup
  expect_warning(
    temp <- merge_samples(temp,
      phase = 2, wave = 2, id = "id"
    ),
    "sampled_data is introducing new ids"
  )
  # temp <- merge_samples(temp, phase = 2, wave = 2, id = "id")
  expect_equal(nrow(get_mw(temp, 2, 2, "data")), 61) # one extra row
})

test_that("error if one of wave's 'samples' slots is empty, but
          design_data still has sampled", {
  temp <- test1
  set_mw(temp, phase = 2, wave = 2, slot = "sampled_data") <-
    dplyr::select(iris, id, Sepal.Width)[samples2, ]
  # Make dup
  set_mw(temp, phase = 2, wave = 2, slot = "samples") <-
    list() # forget to specify samples
  expect_warning(
    temp <- merge_samples(temp,
      phase = 2, wave = 2, id = "id"),
    "slots of waves in this phase are"
  )
  set_mw(temp, phase = 2, wave = 2, slot = "samples") <-
    list(ids = c(3,4,5)) # specify wrong samples
  expect_warning(
    temp <- merge_samples(temp,
                          phase = 2, wave = 2, id = "id"),
    "they do not match"
  )
})

test_that("arguments can be specified in metadata", {
  temp <- test1
  set_mw(temp, phase = NA, wave = NA, "metadata") <- list(
    id = "id", include_probs = FALSE
    )
  set_mw(temp, phase = 2, wave = 2, slot = "samples") <-
    list(ids = samples2)
  temp <- merge_samples(temp, phase = 2, wave = 2)
  expect_equal(dim(get_mw(temp, 2, 2, "data")), c(60, 8))
  expect_equal(length(dplyr::filter(get_mw(temp, 2, 2, "data"),
                                    !is.na(sampling_prob))$id),
               15)
  expect_equal(length(dplyr::filter(get_mw(temp, 2, 2, "data"),
                                    sampled_phase2 == 1)$id),
                      30)
  expect_equal(length(dplyr::filter(get_mw(temp, 2, 2, "data"),
                                    sampled_wave2.1 == 1)$id) +
                 length(dplyr::filter(get_mw(temp, 2, 2, "data"),
                                      sampled_wave2.2 == 1)$id),
               30)

  expect_equivalent(get_mw(temp, 2, 2, "data")$sampled_phase2,
                    get_mw(temp, 2, 2, "data")$sampled_wave2.1 +
                    get_mw(temp, 2, 2, "data")$sampled_wave2.2)
  temp <- test1
  set_mw(temp, phase = 2, wave = 2, "metadata") <- list(
    id = "id",
    phase_sample_ind = "already_sampled_phase2_ind"
  )
  temp <- merge_samples(temp, phase = 2, wave = 2)
  expect_equal(dim(get_mw(temp, 2, 2, "data")), c(60, 9))
  temp <- test1
  set_mw(temp, phase = 2, wave = NA, "metadata") <- list(
    id = "id",
    phase_sample_ind = "already_sampled2_phase2_ind"
  )
  temp <- merge_samples(temp, phase = 2, wave = 2)
  expect_equal(dim(get_mw(temp, 2, 2, "data")), c(60, 9))
  expect_true("already_sampled2_phase2_ind2" %in% names(get_mw(temp,
                                                            2, 2, "data")))
})

test_that("Errors as necessary", {
  temp <- test1
  set_mw(temp, phase = 2, wave = 2, slot = "sampled_data") <-
    dplyr::select(iris, id, Sepal.Width)[samples2, ]
  # Make dup
  set_mw(temp, phase = 2, wave = 2, slot = "samples") <-
    list(ids = NULL) # forget to specify samples
  expect_error(
    merge_samples(temp,
      phase = 0, wave = 2, id = "id"),
    "must be a numeric value specifying a valid phase"
  )
  expect_error(
    merge_samples(temp,
      phase = 2, wave = 5, id = "id"),
    "must be a numeric value specifying a valid wave"
  )
  expect_error(
    merge_samples(temp,
      phase = 2, wave = 2, id = "id",
      phase_sample_ind = 3
    ),
    "must be a character value"
  )
  expect_error(
    merge_samples(temp,
                  phase = 2, wave = 2, id = "id",
                  wave_sample_ind = 3
    ),
    "must be FALSE or a character value"
  )
  expect_error(
    merge_samples(temp,
      phase = 2, wave = 2, id = "wrong",
      phase_sample_ind = 3
    ),
    "must be a character value"
  )
  expect_error(
    merge_samples(temp,
                  phase = 2, wave = 2, id = "id",
                  include_probs = 3
    ),
    "must be TRUE, FALSE"
  )
})

test_that("New wave and phase column names work", {
  temp <- test1
  set_mw(temp, phase = 2, wave = 2, slot = "sampled_data") <-
    dplyr::select(iris, id, Sepal.Width)[samples2, ]

  temp <- merge_samples(temp,
                  phase = 2, wave = 2, id = "id",
                  wave_sample_ind = FALSE, include_probs = FALSE)
  expect_false("sampled_wave2" %in% names(get_mw(temp, phase = 2, wave = 2)))
  temp <- merge_samples(temp,
                        phase = 2, wave = 2, id = "id",
                        wave_sample_ind = "mytestwave",
                        phase_sample_ind = "mytestphase"
                        )
  expect_equal(sort(dplyr::filter(get_mw(temp, phase = 2, wave = 2),
                             mytestphase2 == 1)$id),
               sort(c(
                 get_mw(temp, phase = 2, wave = 1, slot = "samples")$id,
                 get_mw(temp, phase = 2, wave = 2, slot = "samples")$id
               )))
  expect_equivalent(sort(dplyr::filter(get_mw(temp, phase = 2, wave = 2),
                                          mytestwave2.2 == 1)$id),
               sort(get_mw(temp, phase = 2, wave = 2, slot = "samples")$ids))

})

# Set up include_probs
temp <- test1
temp <- apply_multiwave(temp, phase = 2, wave = 2,
                        fun = "allocate_wave", id = "id",
                        y = "Sepal.Width",
                        already_sampled = "sampled_phase2",
                        allocation_method = "WrightII",
                        nsample = 15)
set_mw(temp, phase = 2, wave = 2, slot = "design") <-
  get_mw(temp, phase = 2, wave = 2, slot = "design") %>%
  dplyr::mutate(probs = n_to_sample/(npop-nsample_prior))
temp <- apply_multiwave(temp, phase = 2, wave = 2,
                        fun = "sample_strata", id = "id",
                        already_sampled = "sampled_phase2",
                        probs = "probs")
set_mw(temp, phase = 2, wave = 2, slot = "sampled_data") <-
  dplyr::select(iris, id, Sepal.Width)[get_mw(temp, 2, 2, "samples")$ids, ]

test_that("include_probs works", {
  temp <- merge_samples(temp,
                        phase = 2, wave = 2, id = "id",
                        include_probs = TRUE)
  expect_true("sampled_wave2.2" %in% names(get_mw(temp, phase = 2, wave = 2)))
  expect_true("sampling_prob" %in% names(get_mw(temp, phase = 2, wave = 2)))
  new_data <- dplyr::filter(get_mw(temp, phase = 2, wave = 2),
                            sampled_wave2.2 == 1)
  if(
    length(unique(new_data$sampling_prob)) ==
    length(unique(get_mw(temp, phase = 2, wave = 2, "design")$Species))){
  expect_equal(as.numeric(table(new_data$Species)), get_mw(temp, phase = 2,
                                              wave = 2, "design")$n_to_sample)
  expect_equal(sort(as.numeric(table(new_data$sampling_prob))),
               sort(get_mw(temp, phase = 2, wave = 2, "design")$n_to_sample))
  }
  expect_equal(sort(new_data$sampling_prob),
               sort(get_mw(temp, phase = 2, wave = 2, "samples")$probs))
})

test_that("include_probs arguments can be specified in metadata", {
  temp <- merge_samples(temp,
                        phase = 2, wave = 2, id = "id",
                        include_probs = TRUE)
  set_mw(temp, phase = NA, wave = NA, "metadata") <- list(
    include_probs = FALSE
  )
  temp <- merge_samples(temp,
                        phase = 2, wave = 2, id = "id")
  expect_equal(nrow(dplyr::filter(get_mw(temp, 2, 2), !is.na(sampling_prob))),
               15)
  set_mw(temp, phase = 2, wave = NA, "metadata") <- list(
    include_probs = TRUE
  )

  temp <- merge_samples(temp,
                        phase = 2, wave = 2, id = "id")
  expect_equal(nrow(dplyr::filter(get_mw(temp, 2, 2), !is.na(sampling_prob))), 30)

  set_mw(temp, phase = 2, wave = 2, "metadata") <- list(
    include_probs = TRUE
  )

  temp <- merge_samples(temp,
                        phase = 2, wave = 2, id = "id")
  expect_equal(nrow(dplyr::filter(get_mw(temp, 2, 2), !is.na(sampling_prob))), 30)
})

test_that("include_probs correctly overwrites sampling_prob if necessary", {
  set_mw(temp, phase = 2, wave = 1) <-
    get_mw(temp, phase = 2, wave = 1) %>%
    dplyr::mutate(sampling_prob = 0.0001)
  temp <- merge_samples(temp,
                        phase = 2, wave = 2, id = "id",
                        include_probs = TRUE)
  expect_equal(nrow(dplyr::filter(get_mw(temp, 2, 2), !is.na(sampling_prob))),
               60)
  expect_equal(nrow(dplyr::filter(get_mw(temp, 2, 2), sampling_prob == 0.0001)),
               45)
})

test_that("include_probs warns if no probs in samples slot", {

  temp <- apply_multiwave(temp, phase = 2, wave = 2,
                          fun = "sample_strata", id = "id", strata = "Species",
                          already_sampled = "sampled_phase2",
                          probs = NULL)
  set_mw(temp, phase = 2, wave = 2, slot = "sampled_data") <-
    dplyr::select(iris, id, Sepal.Width)[get_mw(temp, 2, 2, "samples")$ids, ]
  expect_warning(merge_samples(temp,
                        phase = 2, wave = 2, id = "id",
                        include_probs = TRUE), "include_probs is TRUE")

})
