#' Merge Strata
#'
#' Merges multiple pre-defined sampling strata into a single stratum.
#' @param data a dataframe or matrix with one row for each sampling
#' unit, one column, \code{strata},
#' specifying each unit's current stratum, and any
#' other relevant columns.
#' @param strata a character string specifying the name of the
#' column that defines each unit's current strata.
#' @param merge the names of the strata to be merged, exactly as
#' they appear in \code{strata}.
#' @param name a character name for the new stratum. Defaults to
#' NULL, which pastes the old strata names together to create the
#' new stratum name.

#' @examples
#' x <- merge_strata(iris,
#'   strata = "Species",
#'   merge = c("virginica", "versicolor"), name = "v_species"
#' )
#' @export
#' @return Returns the input dataframe with a new column named
#' 'new_strata' that holds the name of the stratum that each sample
#' belongs to after the merge. The column containing the previous
#' strata names is retained and given the name 'old_strata'.


merge_strata <- function(data, strata, merge, name = NULL) {
  old_strata <- NULL # bind global vars as necessary
  if (is.matrix(data)) {
    data <- as.data.frame(data)
  }
  if (is.data.frame(data) == FALSE) {
    stop("'data' must be a dataframe or matrix with named columns")
  }
  if (all(strata %in% names(data)) == FALSE) {
    stop("'strata' must be a string or vector of strings matching
         column names of data.")
  }
  if (all(strata != "old_strata") & "old_strata" %in% names(data)) {
    data <- dplyr::select(data, -old_strata)
    # fixes error from assigning duplicate names
  }
  if (length(strata) != 1) {
    stop("'strata' should specify only one column. To create
         strata from multiple columns, use 'split_strata'.")
  }
  if (is.null(merge) == TRUE) {
    stop("'merge' is NULL. Expecting names of strata to merge.")
  }
  names(data)[names(data) == strata] <- "old_strata"
  if (all(merge %in% data$old_strata) == FALSE) {
    stop("names in 'merge' must each exactly match at least one
         value of strata names in 'strata'.")
  }
  if (is.null(name)) {
    new_strata <- ifelse(data$old_strata %in% merge,
      glue::glue_collapse(merge, sep = "."),
      as.character(data$old_strata)
    )
  }
  if (is.null(name) == FALSE) {
    new_strata <- ifelse(data$old_strata %in% merge,
      name,
      as.character(data$old_strata)
    )
  }
  data <- as.data.frame(cbind(new_strata, data))
}
