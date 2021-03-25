#' Select Sampling Units based on Stratified Random Sampling
#'
#' Requires two dataframes or matrices: \code{data1} with a column
#' \code{strata1} which specifies stratum membership for each unit in
#' the population and a second dataframe \code{data2} with one row per
#' strata level with a column \code{strata2} that indicates the unique
#' levels of \code{strata1} and \code{n_allocated} that specifies the
#' number to be sampled from each stratum.
#' \code{sample_strata} selects the units to sample by
#' selecting a random sample of the desired size within each
#' stratum. The second dataframe can be the output of \code{allocate_wave}
#' or \code{optimum_allocation}.
#' @param data1 A data frame or matrix with one row for each
#' sampling unit in the population, one column specifying each
#' unit's stratum, and one column with a unique identifier for each
#' unit.
#' @param strata1 a character string specifying the name of column
#' in `data1` which indicates stratum membership.
#' @param id a character string specifying the name of the column
#' in `data1` that uniquely identifies each unit.
#' @param wave2a a character sting specifying the name of the
#' column in \code{data1} which indicates (1/0 or Y/N) whether a
#' unit has already been sampled in a prior wave. Defaults to NULL
#' which means that none have been sampled yet.
#' @param data2 a dataframe or matrix with one row for each stratum
#' that subdivides the population, one column specifying the
#' stratum name, and one column indicating the number of samples
#' allocated to each stratum.
#' @param strata2 a character string specifying the name of the
#' column in \code{data2} that contains the stratum levels.
#' Defaults to "strata".
#' @param n_allocated a character string specifying the name of the
#' column in \code{data2} that indicates the n allocated to each
#' stratum. Defaults to "n_to_sample".
#' @export
#' @return returns a {data1} as a dataframe with a new column named
#' "sample_indicator" containing a binary (1/0) indicator of
#' whether each unit should be sampled.
#' @importFrom magrittr %>%
#' @examples
#' # Define a design dataframe
#' design <- data.frame(strata = c("setosa", "virginica", "versicolor"),
#' n_to_sample = c(5, 5, 5))
#'
#' # Make sure there is an id column
#' iris$id <- 1:nrow(iris)
#'
#' # Run
#' sample_strata(data1 = iris, strata1 = "Species", id = "id",
#' data2 = design, strata2 = "strata", n_allocated = "n_to_sample")
#'
#' # If some units had already been samples
#' iris$already_sampled <- rbinom(nrow(iris), 1, 0.25)
#'
#' sample_strata(data1 = iris, strata1 = "Species", id = "id",
#' wave2a = "already_sampled",
#' data2 = design, strata2 = "strata", n_allocated = "n_to_sample")

sample_strata <- function(data1, strata1, id, wave2a = NULL,
                          data2, strata2 = "strata",
                          n_allocated = "n_to_sample") {
  if (is.matrix(data1) | is.matrix(data2)) {
    data1 <- as.data.frame(data1)
    data2 <- as.data.frame(data2)
  }
  if (is.data.frame(data1) == FALSE | is.data.frame(data2) == FALSE) {
    stop("'data1' and 'data2' must be a dataframe or matrix with named columns")
  }
  if (any(c(strata1, id) %in% names(data1) == FALSE)) {
    stop("'strata1' and 'id' must be strings matching a column name of 'data1'")
  }
  if (any(c(strata2, n_allocated) %in% names(data2) == FALSE)) {
    stop("'strata2' and 'n_allocated' must be strings matching a
         column name of 'data2'")
  }
  if (length(unique(data2[, strata2])) != length(data2[, strata2])) {
    stop("'data2' may only contain one row per stratum")
  }
  if (any(data2[, strata2] %in% data1[, strata1] == FALSE)) {
    stop("strata names in 'data2' must all match strata names in 'data1'.")
  }
  if (is.numeric(data2[,n_allocated]) == FALSE) {
    stop("'n_allocated' must specify a numeric column in 'data2'
    containing only whole number values")
  }
  if (is.numeric(data2[,n_allocated]) == TRUE &
      any(data2[,n_allocated]%%1 != 0)){
    stop("'n_allocated' must specify a numeric column in 'data2' containing only
         whole number values")
  }
  nsample <- sum(data2[, n_allocated])
  if (is.null(wave2a) == FALSE) {
    if (wave2a %in% names(data1) == FALSE) {
      stop("If not NULL, 'wave2a' must be a character string matching
      a column name of 'data1'.")
    }
    if (length(table(data1[, wave2a])) != 2) {
      stop("'wave2a' must be a character string matching a column
           in 'data1' that has a binary indicator for whether each
           unit was already sampled.")
    }
    if (("Y" %in% data1[, wave2a] == FALSE & 1 %in%
         data1[, wave2a] == FALSE) | any(is.na(data1[, wave2a]))) {
      stop("'wave2a' column must contain '1' (numeric) or 'Y'
           (string) as indicators that a unit was sampled in a
           previous wave and cannot contain NAs")
    }
    if (nsample + sum(data1[, wave2a] == "Y") +
        sum(data1[, wave2a] == 1) > length(data1[, wave2a])) {
      stop("Total sample size across waves, taken as nsampled in
           wave2a + n to allocate in this sample, is larger than
           the population size.")
    }
  }
  sampled_ids <- list()
  if (is.null(wave2a) == TRUE) {
    for (i in seq_len(nrow(data2))) {
      stratum <- data2[, strata2][i]
      strata_data <- data1[data1[, strata1] == stratum, c(id, strata1)]
      sampled_ids[[i]] <- sample(x = strata_data[, id],
                                 size = data2[, n_allocated][i])
    }
  }
  if (is.null(wave2a) == FALSE) {
    for (i in seq_len(nrow(data2))) {
      stratum <- data2[, strata2][i]
      strata_data <- data1[
        data1[, strata1] == stratum
        & data1[, wave2a] != "Y"
        & data1[, wave2a] != 1,
        c(id, strata1)
        ]
      sampled_ids[[i]] <- sample(x = strata_data[, id],
                                 size = data2[, n_allocated][i])
    }
  }
  sampled_ids <- unlist(sampled_ids)
  names(data1)[names(data1) == id] <- "id"
  output_df <- data1 %>%
    dplyr::mutate(sample_indicator = ifelse(id %in% sampled_ids, 1, 0))
  return(output_df)

}
