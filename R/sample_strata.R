#' Select Sampling Units based on Stratified Random Sampling
#'
#' Given one dataframe indicating which stratum each population
#' unit belongs to and a second specifying the n allocated to each
#' stratum, \code{sample_strata} selects the units to sample by
#' taking a random sample of the desired size within each
#' stratum. The second dataframe specifying the n allocated to each
#' stratum is typically the output of \code{allocate_wave} or
#' \code{optimum_allocation}.
#'
#' If some units have already been sampled, \code{sample_strata} will avoid
#' sampling them again.
#' @param data1 A data frame or matrix with one row for each
#' sampling unit in the population, one column specifying each
#' unit's stratum, and one column with a unique identifier for each
#' unit.
#' @param strata1 a character string specifying the name of column
#' in `data1` which indicates the stratum that each unit belongs to.
#' @param id a character string specifying the name of the column
#' in `data1` which uniquely identifies each unit.
#' @param wave2a a character sting specifying the name of the
#' column in \code{data1} which indicates (1/0 or Y/N) whether a
#' unit has already been sampled in a prior wave. Defaults to NULL
#' which means that none have been sampled yet.
#' @param data2 a dataframe or matrix with one row for each stratum
#' that subdivides the population, one column specifying the
#' stratum name, and one column indicating the number of samples
#' allocated to each stratum. The outputs of both \code{allocate_wave} and
#' \code{optimum_allocation} are in this format.
#' @param strata2 a character string specifying the name of the
#' column in \code{data2} which indicates the stratum. Defaults to "strata".
#' @param n_allocated a character string specifying the name of the
#' column in \code{data2} which indicates the n allocated to each
#' stratum. Defaults to "n_to_sample".
#' @export
#' @return returns a {data1} as a dataframe with a new column named
#' "sample_indicator" containing a binary (1/0) indicator of
#' whether each unit should be sampled.
#' @importFrom magrittr %>%

sample_strata <- function(data1, strata1, id, wave2a = NULL,
                          data2, strata2 = "strata",
                          n_allocated = "n_to_sample", phase, wave) {
  UseMethod("sample_strata", data1)

}
