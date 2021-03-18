context("test-merge_samples")

library(dplyr)
library(optimall)

#Make multiwave_object
test <- new_multiwave(phases = 2, waves = c(1,3))

get_data(test, phase = 1, slot = "data") <-
  dplyr::select(iris, -Sepal.Width)

get_data(test, phase = 2, slot = "metadata") <- list(
  strata = "Species",
  strata2 = "strata",
  id = "id",
  n_allocated = "n_to_sample"
)


get_data(test, phase = 2, wave = 1, slot = "design") <-
  data.frame(strata = unique(iris$Species),
             n_to_sample = c(5,5,5))

set.seed(123)
test <- apply_multiwave(test, phase = 2,
                        wave = 1, "sample_strata") #get samples

samples <- get_data(test, phase = 2, wave = 1, slot = "samples")

get_data(test, phase = 2, wave = 1, slot = "sampled_data") <-
  dplyr::select(iris, id, Sepal.Width)[samples,]

# Testing Wave 1
test <- merge_samples(test, phase = 2, wave = 1, id = "id",
                      sampled_ind = "already_sampled_ind")


test_that("merge_samples merges data with sampled data
          when column doesn't already exist", {
  expect_equal(dim(test@phases$phase2@waves$wave1@data),
               c(150,10))
  expect_equal(length(
    test@phases$phase2@waves$wave1@data$`already_sampled_ind`[
      test@phases$phase2@waves$wave1@data$`already_sampled_ind` == 1
    ]), 15)
  expect_equal(length(
    test@phases$phase2@waves$wave1@data$`already_sampled_ind`[
      !is.na(test@phases$phase2@waves$wave1@data$Sepal.Width)
      ]), 15)
})

#Testing Wave 2
samples2 <- sample(c(1:150)[-as.numeric(samples)], 15)
# No duplicates
get_data(test, phase = 2, wave = 2, slot = "samples") <-
  as.character(samples2) #No duplicates
get_data(test, phase = 2, wave = 2, slot = "sampled_data") <-
  dplyr::select(iris, id, Sepal.Width)[samples2,]
test1 <- merge_samples(test, phase = 2, wave = 2, id = "id",
                       sampled_ind = "already_sampled_ind")

test_that("merge_samples merges data with sampled data
          when column already exists", {
            expect_equal(dim(test1@phases$phase2@waves$wave1@data),
                         c(150,10))
            expect_equal(length(
              test1@phases$phase2@waves$wave2@data$`already_sampled_ind`[
                test1@phases$phase2@waves$wave2@data$`already_sampled_ind` == 1
                ]), 30)
            expect_equal(length(
              test1@phases$phase2@waves$wave2@data$`already_sampled_ind`[
                !is.na(test1@phases$phase2@waves$wave2@data$Sepal.Width)
                ]), 30)
          })

test_that("warning if id already has value for what has been sampled",{
  temp <- test1
  get_data(temp, phase = 2, wave = 2, slot = "sampled_data") <-
    dplyr::select(iris, id, Sepal.Width)[samples2 + as.numeric(samples[1]),]
        #Make dup
  get_data(temp, phase = 2, wave = 2, slot = "samples") <-
    c(as.character(samples2), samples[1]) #Make dup
  expect_warning(merge_samples(temp, phase = 2, wave = 2, id = "id",
                             sampled_ind = "already_sampled_ind"),
               "have non-NA values already")

})

test_that("warning if new ids are in sampled data",{
  temp <- test1
  get_data(temp, phase = 2, wave = 2, slot = "sampled_data") <-
    rbind(dplyr::select(iris, id, Sepal.Width)[samples2,],
          c(160, 3.5))
  #Make dup
  get_data(temp, phase = 2, wave = 2, slot = "samples") <-
    c(as.character(samples2), "160") #Make dup
  expect_warning(temp <- merge_samples(temp, phase = 2, wave = 2, id = "id",
                               sampled_ind = "already_sampled_ind"),
                 "sampled_data is introducing new ids")
  #temp <- merge_samples(temp, phase = 2, wave = 2, id = "id",
                        #sampled_ind = "already_sampled_ind")
  expect_equal(nrow(get_data(temp, 2, 2, "data")),151) #one extra row
})

test_that("warning if one of wave's 'samples' slots is empty",{
  temp <- test1
  get_data(temp, phase = 2, wave = 2, slot = "sampled_data") <-
    dplyr::select(iris, id, Sepal.Width)[samples2,]
  #Make dup
  get_data(temp, phase = 2, wave = 2, slot = "samples") <-
    character(0) #forget to specify samples
  expect_warning(temp <- merge_samples(temp, phase = 2, wave = 2, id = "id",
                                       sampled_ind = "already_sampled_ind"),
                 "slots of previous waves in this phase are")
})

test_that("arguments can be specified in metadata",{
  temp <- test1
  get_data(temp, phase = 2, wave = 2, "metadata") <- list(
    id = "id",
    sampled_ind = "already_sampled_ind"
  )
  temp <- merge_samples(temp, phase = 2, wave = 2)
  expect_equal(dim(get_data(temp, 2, 2, "data")),c(150,10))
  temp <- test1
  get_data(temp, phase = 2, wave = NA, "metadata") <- list(
    id = "id",
    sampled_ind = "already_samples_ind"
  )
  temp <- merge_samples(temp, phase = 2, wave = 2)
  expect_equal(dim(get_data(temp, 2, 2, "data")),c(150,10))
  temp <- test1
  get_data(test1, phase = NA, wave = NA, "metadata") <- list(
    id = "id",
    sampled_ind = "already_sampled_ind"
  )
  temp <- merge_samples(temp, phase = 2, wave = 2)
  expect_equal(dim(get_data(temp, 2, 2, "data")),c(150,10))
  expect_equal("already_sampled_ind" %in% names(
    get_data(temp, phase = 2, wave = 2, slot = "data")),
    TRUE
  )

})
