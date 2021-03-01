#' Allocate Wave Method for Multiwave
#' @title Allocate Wave Method for Multiwave
#' @include multiwave.R phase.R wave.R allocate_wave.R
NULL

#setGeneric("allocate_wave", function(data, phase, wave,
  #                                   strata = NULL,
   #                                  y = NULL, wave2a = NULL,
   #                                  nsample = NULL,
   #                                  method = "iterative",
   #                                  detailed = FALSE){
  #cat("generic dispatch\n")
  #standardGeneric("allocate_wave")
#}, useAsDefault=optimall::allocate_wave)

allocate_wave.Multiwave <- function(data, phase, wave,
                                    strata = NULL,
                                    y = NULL, wave2a = NULL,
                                    nsample = NULL,
                                    method = "iterative",
                                    detailed = FALSE){
  x <- data
  data <- x@phases[[phase]]@waves[[wave]]@data
  wave_md <- x@phases[[phase]]@waves[[wave]]@metadata
  phase_md <- x@phases[[phase]]@metadata
  survey_md <- x@metadata

  # Check for args to allocate_wave in the metadata. Start in wave and move up
  if (is.null(strata)){
    if("strata" %in% names(wave_md) & class(waves_md$strata) == "character"){
      strata <- waves_md$strata
    } else if("strata" %in% names(phase_md) &
              class(phase_md$strata) == "character"){
      strata <- phase_md$strata
    } else if("strata" %in% names(survey_md) &
              class(survey_md$strata) == "character"){
      strata <- phase_md$strata
    } else {
      stop("'strata' must be specified or available in metadata")
    }
  }
  if (is.null(y)){
    if("y" %in% names(wave_md) & class(waves_md$y) == "character"){
      y <- waves_md$y
    } else if("y" %in% names(phase_md) &
              class(phase_md$y) == "character"){
      y <- phase_md$y
    } else if("y" %in% names(survey_md) &
              class(survey_md$y) == "character"){
      y <- phase_md$y
    } else {
      stop("'y' must be specified or available in metadata")
    }
  }
  if (is.null(wave2a)){
    if("wave2a" %in% names(wave_md) & class(waves_md$wave2a) == "character"){
      wave2a <- waves_md$wave2a
    } else if("wave2a" %in% names(phase_md) &
              class(phase_md$wave2a) == "character"){
      wave2a <- phase_md$wave2a
    } else if("wave2a" %in% names(survey_md) &
              class(survey_md$wave2a) == "character"){
      wave2a <- phase_md$wave2a
    } else {
      stop("'wave2a' must be specified or available in metadata")
    }
  }

  output <- optimall::allocate_wave(data = data, y = y,
                          wave2a = wave2a, strata = strata,
                          nsample = nsample, method = method,
                          detailed= detailed)

  x_updated <- x
  x_updated@phases[[phase]]@waves[[wave]]@design <- output
  return(x_updated)
}

setMethod("allocate_wave.Multiwave", signature(data = "Multiwave"),
          allocate_wave.Multiwave)
