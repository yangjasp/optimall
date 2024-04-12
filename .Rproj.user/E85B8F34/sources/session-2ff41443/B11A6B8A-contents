#' Select Sampling Units based on Stratified Random Sampling
#'
#' Requires two dataframes or matrices: \code{data} with a column
#' \code{strata} which specifies stratum membership for each unit in
#' the population and a second dataframe \code{design_data} with one
#' row per strata level with a column \code{design_strata} that
#' indicates the unique levels of \code{strata} in \code{data} and
#' \code{n_allocated} that specifies the
#' number to be sampled from each stratum.
#' \code{sample_strata} selects the units to sample by
#' selecting a random sample of the desired size within each
#' stratum. The second dataframe can be the output of \code{allocate_wave()}
#' or \code{optimum_allocation()}.
#' @param data A data frame or matrix with one row for each
#' sampling unit in the population, one column specifying each
#' unit's stratum, and one column with a unique identifier for each
#' unit.
#' @param strata a character string specifying the name of column
#' in `data` which indicates stratum membership.
#' @param id a character string specifying the name of the column
#' in `data` that uniquely identifies each unit.
#' @param already_sampled a character sting specifying the name of the
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
#' @param probs a character string specifying the name of the column in
#' in \code{design_data} that indicates the sampling probability for each
#' stratum, or a formula indicating how the sampling probabilities can be
#' computed. From existing columns.
#' If specified, a new column containing the sampling probability
#' attached to each sampled unit will be created in the outputted
#' dataframe. This column will be named "sampling_prob". Defaults to NULL.
#' @param wave A numeric value or character string indicating the
#' sampling wave. If specified, the input is appended to
#' "sample_indicator" in the new the sample indicator column name
#' (as long as such columns name do not already exist in \code{data}).
#' Defaults to NULL. This argument does not
#' apply when \code{sample_strata()} is called inside \code{allocate_wave()}.
#' @param warn_prob_overwrite Logical indicator for whether warning should
#' be printed if \code{probs} is specified and a "sampling_prob" columns is
#' going to be overwritten. Defaults to TRUE. If function is called inside
#' \code{apply_multiwave()}, then defaults to FALSE
#' @export
#' @return returns \code{data} as a dataframe with a new column named
#' "sample_indicator" containing a binary (1/0) indicator of
#' whether each unit should be sampled. If \code{wave} argument is
#' specified, then the given input is appended to the name "sample_indicator".
#' If \code{probs} argument is specified, then the dataframe will also contain
#' a new column named "sampling_prob" holding the sampling probabilities for
#' each sampled element.
#' @importFrom magrittr %>%
#' @examples
#' # Define a design dataframe
#' design <- data.frame(
#'   strata = c("setosa", "virginica", "versicolor"),
#'   npop = c(50, 50, 50),
#'   n_to_sample = c(5, 5, 5)
#' )
#'
#' # Make sure there is an id column
#' iris$id <- 1:nrow(iris)
#'
#' # Run
#' sample_strata(
#'   data = iris, strata = "Species", id = "id",
#'   design_data = design, design_strata = "strata",
#'   n_allocated = "n_to_sample"
#' )
#'
#' # To include probs as a formula
#' sample_strata(
#'   data = iris, strata = "Species", id = "id",
#'   design_data = design, design_strata = "strata",
#'   n_allocated = "n_to_sample", probs = ~n_to_sample/npop
#' )
#'
#' # If some units had already been sampled
#' iris$already_sampled <- rbinom(nrow(iris), 1, 0.25)
#'
#' sample_strata(
#'   data = iris, strata = "Species", id = "id",
#'   already_sampled = "already_sampled",
#'   design_data = design, design_strata = "strata",
#'   n_allocated = "n_to_sample"
#' )
sample_strata <- function(data, strata, id, already_sampled = NULL,
                          design_data, design_strata = "strata",
                          n_allocated = "n_to_sample", probs = NULL,
                          wave = NULL, warn_prob_overwrite = TRUE) {
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
  if (is.numeric(design_data[, n_allocated]) == FALSE) {
    stop("'n_allocated' must specify a numeric column in 'design_data'
    containing only whole number values")
  }
  if (is.numeric(design_data[, n_allocated]) == TRUE &
    any(design_data[, n_allocated] %% 1 != 0)) {
    stop("'n_allocated' must specify a numeric column in 'design_data'
    containing only whole number values")
  }
  nsample <- sum(design_data[, n_allocated])

  if(is.null(probs) == FALSE){
    if (is(probs, "formula")){
      # Check if variables exist in the dataframe
      if (!all(all.vars(probs) %in% names(design_data))) {
        stop("Variables in probs formula must exist in design_data.")
      }

      # Evaluate the formula and create the new column
      probs_expr <- as.character(probs)[[2]]
      design_data <- design_data %>%
        dplyr::mutate(probs := !!rlang::parse_expr(probs_expr))
      probs <- "probs"
    }
    if (probs %in% names(design_data) == FALSE) {
      stop("If not NULL, 'probs' must be a character string matching
      a column name of 'design_data'.")
    }
    if (is.numeric(design_data[, probs]) == FALSE) {
      stop("If not NULL,
           'probs' must specify a numeric column in 'design_data'")
    }
  }

  if (is.null(already_sampled) == FALSE) {
    if (already_sampled %in% names(data) == FALSE) {
      stop("If not NULL, 'already_sampled' must be a character string matching
      a column name of 'data'.")
    }
    if (length(table(data[, already_sampled])) != 2) {
      stop("'already_sampled' must be a character string matching a column
           in 'data' that has a binary indicator for whether each
           unit was already sampled.")
    }
    if (("Y" %in% data[, already_sampled] == FALSE & 1 %in%
      data[, already_sampled] == FALSE) | anyNA(data[, already_sampled])) {
      stop("'already_sampled' column must contain '1' (numeric) or 'Y'
           (string) as indicators that a unit was sampled in a
           previous wave and cannot contain NAs")
    }
    if (nsample + sum(data[, already_sampled] == "Y") +
      sum(data[, already_sampled] == 1) > length(data[, already_sampled])) {
      stop("Total sample size across waves, taken as nsampled in
           already_sampled + n to allocate in this sample, is larger than
           the population size.")
    }
  }
  sampled_ids <- list()
  if (is.null(already_sampled) == TRUE) {
    for (i in seq_len(nrow(design_data))) {
      stratum <- design_data[, design_strata][i]
      strata_data <- data[data[, strata] == stratum, c(id, strata)]
      sampled_ids[[i]] <- sample(
        x = strata_data[, id],
        size = design_data[, n_allocated][i]
      )
    }
  }
  if (is.null(already_sampled) == FALSE) {
    for (i in seq_len(nrow(design_data))) {
      stratum <- design_data[, design_strata][i]
      strata_data <- data[
        data[, strata] == stratum
        & data[, already_sampled] != "Y"
        & data[, already_sampled] != 1,
        c(id, strata)
      ]
      sampled_ids[[i]] <- sample(
        x = strata_data[, id],
        size = design_data[, n_allocated][i]
      )
    }
  }
  sampled_ids <- unlist(sampled_ids)
  names(data)[names(data) == id] <- "id"

  ## Generate sample_indicator column, possibly with "wave" arg appended

  new_col_name <- paste0("sample_indicator", wave)
  new_col_name <- ifelse(new_col_name %in% names(data),
                         "sample_indicator",
                         new_col_name)

  output_df <- data %>%
    dplyr::mutate(!!new_col_name := ifelse(id %in% sampled_ids, 1, 0))

  ## If 'probs' specified, add sampling_prob column for sampled units only

  if(!is.null(probs)){
    if("sampling_prob" %in% names(output_df)){
      if(warn_prob_overwrite == TRUE){
      warning("Overwriting prior 'sampling_prob' column with
              new sampling probs" )
    }
      output_df <- output_df[,!(names(output_df) == "sampling_prob")]
    }
    temp <- design_data[,c(design_strata, probs)]
    names(temp) <- c(strata, "sampling_prob")
    sampling_prob <- NULL # to avoid global var note, ensure next line works
    output_df <- output_df %>%
      dplyr::left_join(temp, by = strata) %>%
      dplyr::mutate(sampling_prob = ifelse(id %in% sampled_ids,
                                              sampling_prob, NA))
  }

  return(output_df)
}
