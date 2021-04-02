#' Select Sampling Units based on Stratified Random Sampling
#'
#' Requires two dataframes or matrices: \code{data} with a column
#' \code{strata} which specifies stratum membership for each unit in
#' the population and a second dataframe \code{design_data} with one #' row per strata level with a column \code{design_strata} that
#' indicates the unique levels of \code{strata} in \code{data} and
#' \code{n_allocated} that specifies the
#' number to be sampled from each stratum.
#' \code{sample_strata} selects the units to sample by
#' selecting a random sample of the desired size within each
#' stratum. The second dataframe can be the output of \code{allocate_wave}
#' or \code{optimum_allocation}.
#' @param data A data frame or matrix with one row for each
#' sampling unit in the population, one column specifying each
#' unit's stratum, and one column with a unique identifier for each
#' unit.
#' @param strata a character string specifying the name of column
#' in `data` which indicates stratum membership.
#' @param id a character string specifying the name of the column
#' in `data` that uniquely identifies each unit.
#' @param wave2a a character sting specifying the name of the
#' column in \code{data} which indicates (1/0 or Y/N) whether a
#' unit has already been sampled in a prior wave. Defaults to NULL
#' which means that none have been sampled yet.
#' @param design_data a dataframe or matrix with one row for each stratum
#' that subdivides the population, one column specifying the
#' stratum name, and one column indicating the number of samples
#' allocated to each stratum.
#' @param design_strata a character string specifying the name of the
#' column in \code{design_data} that contains the stratum levels.
#' Defaults to "strata".
#' @param n_allocated a character string specifying the name of the
#' column in \code{design_data} that indicates the n allocated to each
#' stratum. Defaults to "n_to_sample".
#' @export
#' @return returns {data} as a dataframe with a new column named
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
#' sample_strata(data = iris, strata = "Species", id = "id",
#' design_data = design, design_strata = "strata", n_allocated = "n_to_sample")
#'
#' # If some units had already been samples
#' iris$already_sampled <- rbinom(nrow(iris), 1, 0.25)
#'
#' sample_strata(data = iris, strata = "Species", id = "id",
#' wave2a = "already_sampled",
#' design_data = design, design_strata = "strata", n_allocated = "n_to_sample")

sample_strata <- function(data, strata, id, wave2a = NULL,
                          design_data, design_strata = "strata",
                          n_allocated = "n_to_sample") {
  if (is.matrix(data) | is.matrix(design_data)) {
    data <- as.data.frame(data)
    design_data <- as.data.frame(design_data)
  }
  if (is.data.frame(data) == FALSE | is.data.frame(design_data) == FALSE) {
    stop("'data' and 'design_data' must be a dataframe or matrix
         with named columns")
  }
  if (any(c(strata, id) %in% names(data) == FALSE)) {
    stop("'strata' and 'id' must be strings matching a column name of
         'data'")
  }
  if (any(c(design_strata, n_allocated) %in% names(design_data) == FALSE)) {
    stop("'design_strata' and 'n_allocated' must be strings matching a
         column name of 'design_data'")
  }
  if (length(unique(design_data[, design_strata])) !=
      length(design_data[, design_strata])) {
    stop("'design_data' may only contain one row per stratum")
  }
  if (any(design_data[, design_strata] %in% data[, strata] == FALSE)) {
    stop("strata names in 'design_data' must all match strata names
         in 'data'.")
  }
  if (is.numeric(design_data[,n_allocated]) == FALSE) {
    stop("'n_allocated' must specify a numeric column in 'design_data'
    containing only whole number values")
  }
  if (is.numeric(design_data[,n_allocated]) == TRUE &
      any(design_data[,n_allocated]%%1 != 0)){
    stop("'n_allocated' must specify a numeric column in 'design_data'
    containing only whole number values")
  }
  nsample <- sum(design_data[, n_allocated])
  if (is.null(wave2a) == FALSE) {
    if (wave2a %in% names(data) == FALSE) {
      stop("If not NULL, 'wave2a' must be a character string matching
      a column name of 'data'.")
    }
    if (length(table(data[, wave2a])) != 2) {
      stop("'wave2a' must be a character string matching a column
           in 'data' that has a binary indicator for whether each
           unit was already sampled.")
    }
    if (("Y" %in% data[, wave2a] == FALSE & 1 %in%
         data[, wave2a] == FALSE) | any(is.na(data[, wave2a]))) {
      stop("'wave2a' column must contain '1' (numeric) or 'Y'
           (string) as indicators that a unit was sampled in a
           previous wave and cannot contain NAs")
    }
    if (nsample + sum(data[, wave2a] == "Y") +
        sum(data[, wave2a] == 1) > length(data[, wave2a])) {
      stop("Total sample size across waves, taken as nsampled in
           wave2a + n to allocate in this sample, is larger than
           the population size.")
    }
  }
  sampled_ids <- list()
  if (is.null(wave2a) == TRUE) {
    for (i in seq_len(nrow(design_data))) {
      stratum <- design_data[, design_strata][i]
      strata_data <- data[data[, strata] == stratum, c(id, strata)]
      sampled_ids[[i]] <- sample(x = strata_data[, id],
                                 size = design_data[, n_allocated][i])
    }
  }
  if (is.null(wave2a) == FALSE) {
    for (i in seq_len(nrow(design_data))) {
      stratum <- design_data[, design_strata][i]
      strata_data <- data[
        data[, strata] == stratum
        & data[, wave2a] != "Y"
        & data[, wave2a] != 1,
        c(id, strata)
        ]
      sampled_ids[[i]] <- sample(x = strata_data[, id],
                                 size = design_data[, n_allocated][i])
    }
  }
  sampled_ids <- unlist(sampled_ids)
  names(data)[names(data) == id] <- "id"
  output_df <- data %>%
    dplyr::mutate(sample_indicator = ifelse(id %in% sampled_ids, 1, 0))
  return(output_df)

}
