#' @describeIn allocate_wave Allocate Wave Method for Multiwave object
#' A method for \code{allocate_wave} that can be directly
#' implemented on a multiwave object. This method requres that the data in
#' the previous wave of the same phase is present, as it uses that
#' data to determine the sample allocation for the current wave.
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

allocate_wave.Multiwave <- function(data,
                                    strata = NULL,
                                    y = NULL, wave2a = NULL,
                                    nsample = NULL,
                                    method = "iterative",
                                    detailed = FALSE,
                                    phase, wave){
  x <- data
  if((phase == 2 | phase == "phase2") & (wave == 1 | wave == "wave1")){
    data <- x@phases$phase1$data
  } else if(wave == 1 | wave == "wave1"){
    data <- x@phases[[phase - 1]]@waves[[
      length(x@phases[[phase - 1]]@waves)]]@data
  } else if(wave != 1 ){
    data <- x@phases[[phase]]@waves[[
      wave - 1]]@data
  } else{
    stop("Allocate wave cannot be performed in Phase 1")
  }

  if (nrow(data) == 0){
    stop("'data' slot of previous wave must contain data to be used
         for sample allocation calculations")
  }
  wave_md <- x@phases[[phase]]@waves[[wave]]@metadata
  phase_md <- x@phases[[phase]]@metadata
  survey_md <- x@metadata

  # Check for args to allocate_wave in the metadata. Start in wave and move up
  if (is.null(strata)){
    if("strata" %in% names(wave_md) & class(wave_md$strata) == "character"){
      strata <- wave_md$strata
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
    if("y" %in% names(wave_md) & class(wave_md$y) == "character"){
      y <- wave_md$y
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
    if("wave2a" %in% names(wave_md) & class(wave_md$wave2a) == "character"){
      wave2a <- wave_md$wave2a
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

  if (is.null(method)){
    if("method" %in% names(wave_md) & class(wave_md$method) == "character"){
      method <- wave_md$method
    } else if("method" %in% names(phase_md) &
              class(phase_md$method) == "character"){
      method <- phase_md$method
    } else if("method" %in% names(survemethod_md) &
              class(survemethod_md$method) == "character"){
      method <- phase_md$method
    } else {
      stop("'method' must be specified or available in metadata")
    }
  }

  if (is.null(nsample)){
    if("nsample" %in% names(wave_md) & class(wave_md$nsample) == "numeric"){
      nsample <- wave_md$nsample
    } else if("nsample" %in% names(phase_md) &
              class(phase_md$nsample) == "numeric"){
      nsample <- phase_md$nsample
    } else if("nsample" %in% names(survey_md) &
              class(survey_md$nsample) == "numeric"){
      nsample <- phase_md$nsample
    } else {
      stop("'nsample' must be specified or available in metadata")
    }
  }

  if (is.null(detailed)){
    if("detailed" %in% names(wave_md) & class(wave_md$detailed) == "logical"){
      detailed <- wave_md$detailed
    } else if("detailed" %in% names(phase_md) &
              class(phase_md$detailed) == "logical"){
      detailed <- phase_md$detailed
    } else if("detailed" %in% names(survey_md) &
              class(survey_md$detailed) == "logical"){
      detailed <- phase_md$detailed
    } else {
      stop("'detailed' must be specified or available in metadata")
    }
  }

  output <- allocate_wave(data = data, y = y,
                          wave2a = wave2a, strata = strata,
                          nsample = nsample, method = method,
                          detailed= detailed)

  x_updated <- x
  x_updated@phases[[phase]]@waves[[wave]]@design <- output
  return(x_updated)
}

setMethod("allocate_wave", c(data = "Multiwave"),
          allocate_wave.Multiwave)
