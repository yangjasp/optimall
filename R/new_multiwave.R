#' Initialize a Multiwave Object
#'
#' Creates an Object of Class \code{Multiwave} with the specified number
#' of phases and waves. All contents will be NULL upon initialization,
#' but the object contains a framework for contents to be added to
#' during the survey design and sample collection process. Currently,
#' multiwave objects may only have one wave in Phase 1.
#'
#' @param phases A numeric value specifying the number of phases in the
#' survey design.
#' @param waves A vector of numeric values specifying the number of waves in
#' each phase of the survey design. Length must match the number of
#' \code{phases} and the first
#' @param metadata A list containing the survey metadata.
#' Defaults to an empty list.
#' @param phase1 A dataframe containing the phase 1 data of the survey.
#' Defaults to an empty dataframe.
#' @return Returns an object of class \code{Multiwave} that stores all
#' relevant data from the survey design in an organized and easy-to-access
#' manner. See package vignettes or class documentation for more information.
#' @examples
#'
#' # Initialize a multiwave object for a two-phase sampling design that will
#' # sample over three waves in the second phase
#' multiwave_object <- new_multiwave(phases = 2, waves = c(1, 3))
#'
#' # If we already have the phase 1 data and want to add a title to the survey
#' # metadata, we can initialize the object with these included.
#'
#' library(datasets)
#' multiwave_object <- new_multiwave(
#'   phases = 2, waves = c(1, 3),
#'   metadata = list(title = "my two-phase survey"), phase1 = iris
#' )
#' @export

new_multiwave <- function(phases, waves,
                          metadata = list(), phase1 = data.frame()) {
  if (!is.double(phases) | is.double(phases) & phases < 1) {
    stop("'phases' must be a numeric value >= 1")
  }
  if (!is.double(waves) | is.double(waves) & length(waves) != phases) {
    stop("'waves' must be a numeric vector with length matching
         the number of phases specified in 'phases'")
  }
  if (waves[1] != 1) {
    stop("phase 1 can only contain one wave")
  }
  # build list of metadata and waves for each specified phase
  if (phases == 1) {
    phases_list <- list(phase1 = list(
      "metadata" = list(),
      data = phase1
    ))
  } else if (phases > 1) {
    phases_list <- list()
    phase_names <- vector()
    for (i in 2:phases) {
      phase_names[i - 1] <- paste0("phase", as.character(i))
      n_waves <- waves[i]
      waves_list <- list()
      wave_names <- vector()
      for (j in 1:n_waves) {
        wave_names[j] <- paste0("wave", as.character(j))
        waves_list[[j]] <- new("Wave")
      }
      names(waves_list) <- wave_names
      the_phase <- new("Phase", waves = waves_list)
      phases_list[[i]] <- the_phase
    }
    phases_list[[1]] <- list(
      "metadata" = list(),
      data = phase1
    )
    names(phases_list) <- c("phase1", phase_names)
  }
  output <- new("Multiwave", metadata = metadata, phases = phases_list)
  return(output)
}
