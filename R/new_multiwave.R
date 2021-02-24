#' Initialize a Multi-Wave Object
#'
#' Creates an Object of Class "Multiwave" with the specified number
#' of phases and waves. All contents will be NULL upon initialization,
#' but the object contains a framework for contents to be added to
#' during the survey design and sample collection process.
#'
#' @param phases A numeric value specifying the number of phases in the survey design.
#' @param waves A vector of numeric values specifying the number of waves in each phase
#' of the survey design. Length must match the number of \code{phases} and the first
#' phase must only have one wave.
#' @return Returns an object of class "multi-wave" that stores all relevant data from the survey design in an organized and easy-to-access manner. See package vignettes for more information.
#'

new_multiwave <- function(phases, waves,
                          metadata = list(), phase1 = data.frame()){
  if(!is.double(phases) | is.double(phases) & phases < 1){
    stop("'phases' must be a numeric value >= 1")
  }
  if(!is.double(waves) | is.double(waves) & length(waves) != phases){
    stop("'waves' must be a numeric vector with length matching
         the number of phases specified in 'phases'")
  }
  #build list of metadata and waves for each specified phase
  if(phases == 1){
    phases_list <- list(phase1 = list("metadata" = list(),
                                      data = phase1))
  } else if(phases > 1){
    phases_list <- list()
    phase_names <- vector()
    for (i in 2:phases){
      phase_names[i-1] <- paste0("phase", as.character(i))
      n_waves <- waves[i]
      waves_list <- list()
      wave_names <- vector()
      for (j in 1:n_waves){
        wave_names[j] <- paste0("wave", as.character(j))
        waves_list[[j]] <- new("wave")
      }
      names(waves_list) <- wave_names
      the_phase <- new("phase", waves = waves_list)
      phases_list[[i]] <- the_phase
    }
    phases_list[[1]] <- list("metadata" = list(),
                             data = phase1)
    names(phases_list) <- c("phase1", phase_names)
  }
  output <- new("multiwave", metadata = metadata, phases = phases_list)
  return(output)
}
