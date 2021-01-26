context("test-merge_strata")

library(dplyr)
library(optimall)

#Make data

set.seed(354)
data <- data.frame("strata" = rep(c("A","B","C"),times = 10),
                   "y" = rnorm(30))

test_that("merge_strata produces a dataframe with the same number of
          rows as input and one more column called 'new_strata' with
          only the specified strata changed",{
  output <- merge_strata(data,strata = "strata",
                         merge = c("A","B"),name = "new")
  expect_equal(all(dplyr::select(output, -new_strata) == data),TRUE)
  expect_equal(as.vector(table(output$new_strata)),c(10,20))
  expect_equal(unique(dplyr::filter(output,
                                    old_strata %in%
                                      c("A","B"))$new_strata), "new")
})

test_that("error messages properly address issues with inputs",{
  expect_error(merge_strata(data,strata = "wrong",
                            merge = c("A","B"),name = "new"),
               "'strata' must be a string or vector of strings")
  expect_error(merge_strata(data,strata = "strata",
                            merge = c("A","D"),name = "new"),
               "names in 'merge' must each exactly match at least")
})

test_that("error messages properly address issues with inputs",{
  expect_error(merge_strata(data,strata = "wrong",
                            merge = c("A","B"),name = "new"),
               "'strata' must be a string or vector of strings")
  expect_error(merge_strata(data,strata = "strata",
                            merge = c("A","D"),name = "new"),
               "names in 'merge' must each exactly match at least")
})

test_that("giving NULL name works",{
  expect_equal(unique((merge_strata(data,
                                    "strata",c("A","B"))$new_strata)),
               c("A.B","C"))
})
