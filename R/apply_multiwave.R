#' Apply a basic optimall function to a Multiwave Object
#'
#' Given a specified phase and wave of an object of class multiwave,
#' \code{apply_multiwave} applies one of four \code{optimall} functions
#' and returns an updated multiwave object with the output of the applied
#' function in its specified slot.
#'
#' @param x An Object of class \code{"multiwave"}
#' @param phase A numeric or character value specifying the phase of
#' \code{multiwave} where the desired output should be placed.
#' @param wave A numeric or character value specifying the wave of \code{phase}
#'  in \code{multiwave} where the output should be placed.
#' @param fun A character value specifying the name of the \code{optimall}
#' function to apply. The four available functions are:
#' \code{optimum_allocation},
#' \code{allocate_wave}, \code{sample_strata}, and \code{merge_samples}.
#' \itemize{
#' \item \code{optimum_allocation}: Uses the \code{data} from the previous wave (or previous phase if \code{wave = 1}) to determine the optimum sampling allocation for the specified wave. If used, the output multiwave object contains an updated \code{"design"} slot in the specified wave.
#' \item \code{allocate_wave}: Uses the \code{data} from the previous wave (or previous phase if \code{wave = 1}) to determine the optimum sampling allocation for the specified wave. If used, the output multiwave object contains an updated \code{"design"} slot in the specified wave.
#' \item \code{sample_strata}: Uses the \code{data} from the previous wave (or previous phase if \code{wave = 1}) and \code{design} from current wave to generate a vector of ids to sample for the current wave. If used, the output multiwave object contains an updated \code{"samples"} slot in the specified wave.
#' \item \code{merge_samples}: Uses the \code{data} from the previous wave (or
#' previous phase if \code{wave = 1}) and \code{sampled_data} from the
#' specified wave to generate the final, merged data for the current wave.
#' If used, the output multiwave object contains an updated \code{"data"}
#' slot in the specified wave. Note that \code{merge_samples} is already a
#' method for multiwave objects, so calling
#' it through \code{apply_optimall} is the exact same as calling it on its own.}
#' See documentation of these functions for more details on the specific uses and arguments.
#' @param ... Optional arguments to be given to \code{fun}. Not necessary if the arguments are already provided as named values in the wave, phase, or overall metadata in the multiwave object. Arguments provided here will override specifications in the metadata if provided in both places.
#' @return The inputted multiwave object with one slot updated to include the
#' output of the specified function.
#'
#' Note that the phase and wave arguments specify where the function
#' \emph{output} should be placed. \code{apply_multiwave} will determine where
#' to get the input dataframes from (returning an error if those slots are
#' empty or invalid) given the specified wave for the output. For example, if
#' \code{phase = 2, wave = 2, function = "allocate_wave"}, the data to
#' determine the optimum allocation will be taken from the previous wave
#' (phase 2, wave 1) and the output multiwave object will have an updated
#' \code{"design"} slot of phase 2, wave 2.
#'
#' @include get_data.R phase.R wave.R multiwave.R optimum_allocation.R
#' @include allocate_wave.R merge_samples.R sample_strata.R
#'
setGeneric("apply_multiwave", function(x, phase, wave, fun, ...)
  standardGeneric("apply_multiwave"))

setMethod("apply_multiwave", c(x = "Multiwave"),
          function(x, phase, wave, fun, ...){
  #Get function args
  arguments <- list(...)

  #optimum_allocation
  if (fun == "optimum_allocation"){
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
      stop("Optimum Allocation cannot be performed in Phase 1")
    }

    if (nrow(data) == 0){
      stop("'data' slot of previous wave must contain data to be used
         for sample allocation calculations")
    }
    wave_md <- x@phases[[phase]]@waves[[wave]]@metadata
    phase_md <- x@phases[[phase]]@metadata
    survey_md <- x@metadata

    # Check for args to optimum_allocation in the metadata.
    #Start in wave and move up
    if (is.null(arguments$strata)){
      if("strata" %in% names(wave_md) & class(wave_md$strata) == "character"){
        strata <- wave_md$strata
      } else if("strata" %in% names(phase_md) &
                class(phase_md$strata) == "character"){
        strata <- phase_md$strata
      } else if("strata" %in% names(survey_md) &
                class(survey_md$strata) == "character"){
        strata <- phase_md$strata
      } else {
        stop("'strata' must be a character vector specified or
             available in metadata")
      }
    } else{
      strata <- arguments$strata
    }

    if (is.null(arguments$y)){
      if("y" %in% names(wave_md) & class(wave_md$y) == "character"){
        y <- wave_md$y
      } else if("y" %in% names(phase_md) &
                class(phase_md$y) == "character"){
        y <- phase_md$y
      } else if("y" %in% names(survey_md) &
                class(survey_md$y) == "character"){
        y <- phase_md$y
      } else {
        stop("'y' must be a character value specified or available
             in metadata")
      }
    } else{
      y <- arguments$y
    }

    if (is.null(arguments$method)){
      if("method" %in% names(wave_md) & class(wave_md$method) == "character"){
        method <- wave_md$method
     } else if("method" %in% names(phase_md) &
               class(phase_md$method) == "character"){
       method <- phase_md$method
      } else if("method" %in% names(survemethod_md) &
                class(survemethod_md$method) == "character"){
        method <- phase_md$method
      } else {
        method <- "WrightII"
      }
    } else{
      method <- arguments$method
    }

    if (is.null(arguments$nsample)){
      if("nsample" %in% names(wave_md) & class(wave_md$nsample) == "numeric"){
        nsample <- wave_md$nsample
      } else if("nsample" %in% names(phase_md) &
                class(phase_md$nsample) == "numeric"){
        nsample <- phase_md$nsample
      } else if("nsample" %in% names(survey_md) &
                class(survey_md$nsample) == "numeric"){
        nsample <- phase_md$nsample
      } else {
        nsample <- NULL
      }
    } else{
      nsample <- arguments$nsample
    }

    if (is.null(arguments$ndigits)){
      if("ndigits" %in% names(wave_md) & class(wave_md$ndigits) == "numeric"){
        ndigits <- wave_md$ndigits
      } else if("ndigits" %in% names(phase_md) &
                class(phase_md$ndigits) == "numeric"){
        ndigits <- phase_md$ndigits
      } else if("ndigits" %in% names(survey_md) &
                class(survey_md$ndigits) == "numeric"){
        ndigits <- phase_md$ndigits
      } else {
        ndigits <- 2
      }
    } else{
      ndigits <- arguments$ndigits
    }

    if (is.null(arguments$allow.na)){
      if("allow.na" %in% names(wave_md) &
         class(wave_md$allow.na) == "logical"){
        allow.na <- wave_md$allow.na
      } else if("allow.na" %in% names(phase_md) &
                class(phase_md$allow.na) == "logical"){
        allow.na <- phase_md$allow.na
      } else if("allow.na" %in% names(survey_md) &
                class(survey_md$allow.na) == "logical"){
        allow.na <- phase_md$allow.na
      } else {
        allow.na <- FALSE
      }
    } else{
      allow.na <- arguments$allow.na
    }

    output <- optimum_allocation(data = data, y = y,
                                strata = strata,
                                nsample = nsample, method = method)

    x_updated <- x
    x_updated@phases[[phase]]@waves[[wave]]@design <- output
    return(x_updated)
    }

  #allocate_wave
  if (fun == "allocate_wave"){
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
    if (is.null(arguments$strata)){
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
    } else {
      strata <- arguments$strata
    }
    if (is.null(arguments$y)){
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
    } else{
      y <- arguments$y
    }
    if (is.null(arguments$wave2a)){
      if("wave2a" %in% names(wave_md) & class(wave_md$wave2a) == "character"){
        wave2a <- wave_md$wave2a
      } else if("wave2a" %in% names(phase_md) &
                class(phase_md$wave2a) == "character"){
        wave2a <- phase_md$wave2a
      } else if("wave2a" %in% names(survey_md) &
                class(survey_md$wave2a) == "character"){
        wave2a <- phase_md$wave2a
      } else {
        stop("'wave2a' must be specified or available in metadata. If no
             samples have been taken yet, use 'optimum_allocation'")
      }
    } else{
      wave2a <- arguments$wave2a
    }

    if (is.null(arguments$method)){
      if("method" %in% names(wave_md) & class(wave_md$method) == "character"){
        method <- wave_md$method
      } else if("method" %in% names(phase_md) &
                class(phase_md$method) == "character"){
        method <- phase_md$method
      } else if("method" %in% names(survemethod_md) &
                class(survemethod_md$method) == "character"){
        method <- phase_md$method
      } else {
        method <- "iterative"
      }
    } else{
      method <- arguments$method
    }

    if (is.null(arguments$nsample)){
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
    } else{
      nsample <- arguments$nsample
    }

    if (is.null(arguments$detailed)){
      if("detailed" %in% names(wave_md) & class(wave_md$detailed) == "logical"){
        detailed <- wave_md$detailed
      } else if("detailed" %in% names(phase_md) &
                class(phase_md$detailed) == "logical"){
        detailed <- phase_md$detailed
      } else if("detailed" %in% names(survey_md) &
                class(survey_md$detailed) == "logical"){
        detailed <- phase_md$detailed
      } else {
        detailed <- FALSE
      }
    } else{
      detailed = arguments$detailed
    }

    output <- allocate_wave(data = data, y = y,
                            wave2a = wave2a, strata = strata,
                            nsample = nsample, method = method,
                            detailed= detailed)

    x_updated <- x
    x_updated@phases[[phase]]@waves[[wave]]@design <- output
    return(x_updated)
  }
    # sample_strata
    if(fun == "sample_strata"){
      #Get data1
      if((phase == 2 | phase == "phase2") & (wave == 1 | wave == "wave1")){
        data1 <- x@phases$phase1$data
      } else if(wave == 1 | wave == "wave1"){
        data1 <- x@phases[[phase - 1]]@waves[[
          length(x@phases[[phase - 1]]@waves)]]@data
      } else if(wave != 1 ){
        data1 <- x@phases[[phase]]@waves[[
          wave - 1]]@data
      } else{
        stop("sample strata cannot be performed in Phase 1")
      }

      #Get data2

      data2 <- x@phases[[phase]]@waves[[wave]]@design
      if (nrow(data2) == 0){
        stop("'design' slot of specified wave must be filled with valid
         design dataframe")
      }

      if (nrow(data1) == 0){
        stop("'data' slot of previous wave must contain data to be used
         for sample allocation calculations")
      }
      wave_md <- x@phases[[phase]]@waves[[wave]]@metadata
      phase_md <- x@phases[[phase]]@metadata
      survey_md <- x@metadata

      # Check for args to sample_strata in the metadata.
      # Start in wave and move up.

      # strata1. Can be "strata1" or "strata" in metadata
      if (is.null(arguments$strata1)){
        if("strata1" %in% names(wave_md) & class(wave_md$strata1)
           == "character"){
          strata1 <- wave_md$strata1
        } else if("strata1" %in% names(phase_md) &
                  class(phase_md$strata1) == "character"){
          strata1 <- phase_md$strata1
        } else if("strata1" %in% names(survey_md) &
                  class(survey_md$strata1) == "character"){
          strata1 <- phase_md$strata1
        } else if("strata" %in% names(wave_md) &
                  class(wave_md$strata) == "character"){
          strata1 <- wave_md$strata
        } else if("strata" %in% names(phase_md) &
                  class(phase_md$strata) == "character"){
          strata1 <- phase_md$strata
        } else if("strata" %in% names(survey_md) &
                  class(survey_md$strata) == "character"){
          strata1 <- phase_md$strata
        } else {
          stop("'strata1' or 'strata' must be specified or
               available in metadata")
        }
      } else if (!(arguments$strata1 %in% names(data1))){
        stop("'strata1' must be a column name of the dataframe in the
         'data' slot of the previous wave")
      } else{
        strata1 <- arguments$strata1
      }

      # strata2. Can be "strata2" or "strata" in metadata. Will check in
      #that order.
      if (is.null(arguments$strata2)){
        if("strata2" %in% names(wave_md) & class(wave_md$strata2)
           == "character"){
          strata2 <- wave_md$strata2
        } else if("strata2" %in% names(phase_md) &
                  class(phase_md$strata2) == "character"){
          strata2 <- phase_md$strata2
        } else if("strata2" %in% names(survey_md) &
                  class(survey_md$strata2) == "character"){
          strata2 <- phase_md$strata2
        } else if("strata" %in% names(wave_md) & class(wave_md$strata) == "character"){
          strata2 <- wave_md$strata
        } else if("strata" %in% names(phase_md) &
                  class(phase_md$strata) == "character"){
          strata2 <- phase_md$strata
        } else if("strata" %in% names(survey_md) &
                  class(survey_md$strata) == "character"){
          strata2 <- phase_md$strata
        } else {
          strata2 <- "strata"
        }
      } else{
        strata2 <- arguments$strata2
      }

      if (!(strata2 %in% names(data2))){
        stop("'strata2' must be a column name of the dataframe in the
         'data' slot of the previous wave.")
      }


      # Now id
      if (is.null(arguments$id)){
        if("id" %in% names(wave_md) & class(wave_md$id) == "character"){
          id <- wave_md$id
        } else if("id" %in% names(phase_md) &
                  class(phase_md$id) == "character"){
          id <- phase_md$id
        } else if("id" %in% names(survey_md) &
                  class(survey_md$id) == "character"){
          id <- phase_md$id
        } else {
          stop("'id' must be specified or available in metadata")
        }
      } else{
        id <- arguments$id
      }

      if (!(id %in% names(data1))){
        stop("'id' must be a column name of the dataframe in the
        'data' slot of the previous wave.")
      }

      # wave2a
      if (is.null(arguments$wave2a)){
        if("wave2a" %in% names(wave_md) &
           class(wave_md$wave2a) == "character"){
          wave2a <- wave_md$wave2a
        } else if("wave2a" %in% names(phase_md) &
                  class(phase_md$wave2a) == "character"){
          wave2a <- phase_md$wave2a
        } else if("wave2a" %in% names(survey_md) &
                  class(survey_md$wave2a) == "character"){
          wave2a <- phase_md$wave2a
        } else {
          wave2a <- NULL
        }
      } else{
        wave2a <- arguments$wave2a
      }

      if (!is.null(wave2a)){
        if(!(wave2a %in% names(data1))){
          stop("'wave2a' must be a column name of the dataframe in the
          'data' slot of the previous wave.")
        }
      }

      # n_allocated
      if (is.null(arguments$n_allocated)){
        if("n_allocated" %in% names(wave_md) & class(wave_md$n_allocated) == "character"){
          n_allocated <- wave_md$n_allocated
        } else if("n_allocated" %in% names(phase_md) &
                  class(phase_md$n_allocated) == "character"){
          n_allocated <- phase_md$n_allocated
        } else if("n_allocated" %in% names(survey_md) &
                  class(survey_md$n_allocated) == "character"){
          n_allocated <- phase_md$n_allocated
        } else {
          n_allocated <- "n_to_sample"
          if (!(n_allocated %in% names(data2))){
            n_allocated <- "n_to_sample"
          }
        }
      } else{
        n_allocated <- arguments$n_allocated
      }

      if (!(n_allocated %in% names(data2))){
        stop("'n_allocated' must be a column name of the dataframe in the
        'design' slot of the specified wave.")
      }

      output <- sample_strata(data = data1, id = id,
                              strata1 = strata1, wave2a = wave2a,
                              data2 = data2, strata2 = strata2,
                              n_allocated = n_allocated)

      x_updated <- x
      x_updated@phases[[phase]]@waves[[wave]]@samples <-
        as.character(dplyr::filter(output, sample_indicator == 1)$id)
      return(x_updated)
    }

  # merge_samples

  if(fun == "merge_samples"){
    if (is.null(arguments$id)){
      if("id" %in% names(wave_md) & class(wave_md$id) == "character"){
        id <- wave_md$id
      } else if("id" %in% names(phase_md) &
                class(phase_md$id) == "character"){
        id <- phase_md$id
      } else if("id" %in% names(survey_md) &
                class(survey_md$id) == "character"){
        id <- phase_md$id
      } else {
        stop("'id' must be specified or available in metadata")
      }
    } else{
      id <- arguments$id
    }

    if (is.null(arguments$sampled_ind)){
      if("sampled_ind" %in% names(wave_md) & class(wave_md$sampled_ind) == "character"){
        sampled_ind <- wave_md$sampled_ind
      } else if("sampled_ind" %in% names(phase_md) &
                class(phase_md$sampled_ind) == "character"){
        sampled_ind <- phase_md$sampled_ind
      } else if("sampled_ind" %in% names(survey_md) &
                class(survey_md$sampled_ind) == "character"){
        sampled_ind <- phase_md$sampled_ind
      } else {
        sampled_ind = "sampled_ind"
      }
    } else{
      sampled_ind <- arguments$sampled_ind
    }


    x_updated <- merge_samples(x = x, phase = phase, wave = wave, id = id,
                  sampled_ind = sampled_ind)
    return(x_updated)
  }
})


