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
#' \item \code{optimum_allocation}: Uses the \code{data} from
#' the previous wave
#' (or previous phase if \code{wave = 1}) to determine the optimum sampling
#' allocation for the specified wave. If used, the output multiwave
#' object contains an updated \code{"design"} slot in the specified wave.
#' \item \code{allocate_wave}: Uses the \code{data} from the previous wave
#' (or previous phase if \code{wave = 1}) to determine the optimum sampling
#' allocation for the specified wave. If used, the outputted multiwave object
#' contains an updated \code{"design"} slot in the specified wave.
#' The default argument when \code{allocate_wave} is applied in a \code{apply_multiwave()} is \code{detailed = TRUE}.
#' \item \code{sample_strata}: Uses the \code{data} from the previous wave
#' (or previous phase if \code{wave = 1}) and \code{design}
#' from current wave to generate a vector of ids to sample for the current
#' wave. Note that the \code{wave} argument of the standalone
#' \code{sample_strata()} function does not apply here,
#'  If used, the output multiwave object contains an updated
#' \code{"samples"} slot in the specified wave.
#' \item \code{merge_samples}: Uses the \code{data} from the previous wave (or
#' previous phase if \code{wave = 1}) and \code{sampled_data} from the
#' specified wave to generate the final, merged data for the current wave.
#' If used, the output multiwave object contains an updated \code{"data"}
#' slot in the specified wave. Note that \code{merge_samples} is already a
#' method for multiwave objects, so calling
#' it through \code{apply_multiwave} is the exact same as calling it on its
#' own.}
#' See documentation of these functions for more details on the
#' specific uses and arguments.
#' @param ... Optional arguments to be given to \code{fun}. Not necessary if
#' the arguments are already provided as named values in the wave, phase,
#' or overall metadata in the multiwave object. Arguments provided here
#' will override specifications in the metadata if provided in both places.
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
#' @examples
#'
#' library(datasets)
#'
#' MySurvey <- multiwave(phases = 2, waves = c(1, 3))
#' set_mw(MySurvey, phase = 1, slot = "data") <-
#'   dplyr::select(datasets::iris, -Sepal.Width)
#'
#' # Get Design by applying optimum_allocation
#' MySurvey <- apply_multiwave(MySurvey,
#'   phase = 2, wave = 1,
#'   fun = "optimum_allocation", strata = "Species",
#'   y = "Sepal.Length",
#'   nsample = 15,
#'   method = "WrightII"
#' )
#'
#' # or, we can establish function args in the metadata
#' set_mw(MySurvey, phase = 2, slot = "metadata") <- list(
#'   strata = "Species",
#'   nsample = 15,
#'   y = "Sepal.Length",
#'   method = "WrightII"
#' )
#'
#' # which allows the function to be run without specifying the args
#' MySurvey <- apply_multiwave(MySurvey,
#'   phase = 2, wave = 1,
#'   fun = "optimum_allocation"
#' )
#' @include get_mw.R set_mw.R phase.R wave.R multiwave.R optimum_allocation.R
#' @include allocate_wave.R merge_samples.R sample_strata.R
#' @importFrom magrittr %>%
#' @export

setGeneric("apply_multiwave", function(x, phase, wave, fun, ...) {
  standardGeneric("apply_multiwave")
})

#' @name apply_multiwave
#' @aliases apply_multiwave,Multiwave-method

setMethod(
  "apply_multiwave", c(x = "Multiwave"),
  function(x, phase, wave, fun, ...) {
    # Get function args
    arguments <- list(...)

    # optimum_allocation
    if (fun == "optimum_allocation") {
      if ((phase == 2 | phase == "phase2") & (wave == 1 | wave == "wave1")) {
        data <- x@phases$phase1$data
      } else if (wave == 1 | wave == "wave1") {
        data <- x@phases[[phase - 1]]@waves[[
        length(x@phases[[phase - 1]]@waves)]]@data
      } else if (wave != 1) {
        data <- x@phases[[phase]]@waves[[
        wave - 1]]@data
      } else {
        stop("Optimum Allocation cannot be performed in Phase 1")
      }

      if (nrow(data) == 0) {
        stop("'data' slot of previous wave must contain data to be used
         for sample allocation calculations")
      }
      wave_md <- x@phases[[phase]]@waves[[wave]]@metadata
      phase_md <- x@phases[[phase]]@metadata
      survey_md <- x@metadata

      # Check for args to optimum_allocation in the metadata.
      # Start in wave and move up
      if (is.null(arguments$strata)) {
        if ("strata" %in% names(wave_md) &
            inherits(wave_md$strata, "character")) {
          strata <- wave_md$strata
        } else if ("strata" %in% names(phase_md) &
            inherits(phase_md$strata,"character")) {
          strata <- phase_md$strata
        } else if ("strata" %in% names(survey_md) &
            inherits(survey_md$strata, "character")) {
          strata <- survey_md$strata
        } else {
          stop("'strata' must be a character vector specified or
             available in metadata")
        }
      } else {
        strata <- arguments$strata
      }

      if (is.null(arguments$y)) {
        if ("y" %in% names(wave_md) &
            inherits(wave_md$y, "character")) {
          y <- wave_md$y
        } else if ("y" %in% names(phase_md) &
            inherits(phase_md$y, "character")) {
          y <- phase_md$y
        } else if ("y" %in% names(survey_md) &
            inherits(survey_md$y, "character")) {
          y <- survey_md$y
        } else {
          stop("'y' must be a character value specified or available
             in metadata")
        }
      } else {
        y <- arguments$y
      }

      if (is.null(arguments$method)) {
        if ("method" %in% names(wave_md) &
            inherits(wave_md$method, "character")) {
          method <- wave_md$method
        } else if ("method" %in% names(phase_md) &
            inherits(phase_md$method, "character")) {
          method <- phase_md$method
        } else if ("method" %in% names(survey_md) &
            inherits(survey_md$method, "character")) {
          method <- survey_md$method
        } else {
          method <- "WrightII"
        }
      } else {
        method <- arguments$method
      }

      if (is.null(arguments$nsample)) {
        if ("nsample" %in% names(wave_md) &
            inherits(wave_md$nsample, "numeric")) {
          nsample <- wave_md$nsample
        } else if ("nsample" %in% names(phase_md) &
            inherits(phase_md$nsample, "numeric")) {
          nsample <- phase_md$nsample
        } else if ("nsample" %in% names(survey_md) &
            inherits(survey_md$nsample, "numeric")) {
          nsample <- survey_md$nsample
        } else {
          nsample <- NULL
        }
      } else {
        nsample <- arguments$nsample
      }

      if (is.null(arguments$ndigits)) {
        if ("ndigits" %in% names(wave_md) &
            inherits(wave_md$ndigits, "numeric")) {
          ndigits <- wave_md$ndigits
        } else if ("ndigits" %in% names(phase_md) &
            inherits(phase_md$ndigits, "numeric")) {
          ndigits <- phase_md$ndigits
        } else if ("ndigits" %in% names(survey_md) &
            inherits(survey_md$ndigits, "numeric")) {
          ndigits <- phase_md$ndigits
        } else {
          ndigits <- 2
        }
      } else {
        ndigits <- arguments$ndigits
      }

      if (is.null(arguments$allow.na)) {
        if ("allow.na" %in% names(wave_md) &
            inherits(wave_md$allow.na, "logical")) {
          allow.na <- wave_md$allow.na
        } else if ("allow.na" %in% names(phase_md) &
            inherits(phase_md$allow.na, "logical")) {
          allow.na <- phase_md$allow.na
        } else if ("allow.na" %in% names(survey_md) &
            inherits(survey_md$allow.na, "logical")) {
          allow.na <- phase_md$allow.na
        } else {
          allow.na <- FALSE
        }
      } else {
        allow.na <- arguments$allow.na
      }

      output <- optimum_allocation(
        data = data, y = y,
        strata = strata,
        nsample = nsample, method = method
      )

      x_updated <- x
      x_updated@phases[[phase]]@waves[[wave]]@design <- output
      return(x_updated)
    }

    # allocate_wave
    if (fun == "allocate_wave") {
      if (phase == 1) {
        stop("Allocate wave cannot be performed in Phase 1")
      } else if ((phase == 2 | phase == "phase2") &
        (wave == 1 | wave == "wave1")) {
        data <- x@phases$phase1$data
      } else if (wave == 1 | wave == "wave1") {
        data <- x@phases[[phase - 1]]@waves[[
        length(x@phases[[phase - 1]]@waves)]]@data
      } else if (wave != 1) {
        data <- x@phases[[phase]]@waves[[
        wave - 1]]@data
      }

      if (nrow(data) == 0) {
        stop("'data' slot of previous wave must contain data to be used
         for sample allocation calculations")
      }
      wave_md <- x@phases[[phase]]@waves[[wave]]@metadata
      phase_md <- x@phases[[phase]]@metadata
      survey_md <- x@metadata

      # Check for args to allocate_wave in the metadata.
      # Start in wave and move up
      if (is.null(arguments$strata)) {
        if ("strata" %in% names(wave_md) &
            inherits(wave_md$strata, "character")) {
          strata <- wave_md$strata
        } else if ("strata" %in% names(phase_md) &
            inherits(phase_md$strata, "character")) {
          strata <- phase_md$strata
        } else if ("strata" %in% names(survey_md) &
            inherits(survey_md$strata, "character")) {
          strata <- survey_md$strata
        } else {
          stop("'strata' must be specified or available in metadata")
        }
      } else {
        strata <- arguments$strata
      }
      if (is.null(arguments$y)) {
        if ("y" %in% names(wave_md) &
            inherits(wave_md$y, "character")) {
          y <- wave_md$y
        } else if ("y" %in% names(phase_md) &
            inherits(phase_md$y, "character")) {
          y <- phase_md$y
        } else if ("y" %in% names(survey_md) &
            inherits(survey_md$y, "character")) {
          y <- survey_md$y
        } else {
          stop("'y' must be specified or available in metadata")
        }
      } else {
        y <- arguments$y
      }
      if (is.null(arguments$already_sampled)) {
        if ("already_sampled" %in% names(wave_md) &
            inherits(wave_md$already_sampled, "character")) {
          already_sampled <- wave_md$already_sampled
        } else if ("already_sampled" %in% names(phase_md) &
            inherits(phase_md$already_sampled, "character")) {
          already_sampled <- phase_md$already_sampled
        } else if ("already_sampled" %in% names(survey_md) &
            inherits(survey_md$already_sampled, "character")) {
          already_sampled <- survey_md$already_sampled
        } else {
          stop("'already_sampled' must be specified or available in metadata.
          If no samples have been taken yet, use 'optimum_allocation'")
        }
      } else {
        already_sampled <- arguments$already_sampled
      }

      if (is.null(arguments$method)) {
        if ("method" %in% names(wave_md) &
            inherits(wave_md$method, "character")) {
          method <- wave_md$method
        } else if ("method" %in% names(phase_md) &
            inherits(phase_md$method, "character")) {
          method <- phase_md$method
        } else if ("method" %in% names(survey_md) &
            inherits(survey_md$method, "character")) {
          method <- survey_md$method
        } else {
          method <- "iterative"
        }
      } else {
        method <- arguments$method
      }

      if (is.null(arguments$allocation_method)) {
        if ("allocation_method" %in% names(wave_md) &
            inherits(wave_md$allocation_method, "character")) {
          allocation_method <- wave_md$allocation_method
        } else if ("allocation_method" %in% names(phase_md) &
                   inherits(phase_md$allocation_method, "character")) {
          allocation_method <- phase_md$allocation_method
        } else if ("allocation_method" %in% names(survey_md) &
                   inherits(survey_md$allocation_method, "character")) {
          allocation_method <- survey_md$allocation_method
        } else {
          allocation_method <- "WrightII"
        }
      } else {
        allocation_method <- arguments$allocation_method
      }

      if (is.null(arguments$nsample)) {
        if ("nsample" %in% names(wave_md) &
            inherits(wave_md$nsample, "numeric")) {
          nsample <- wave_md$nsample
        } else if ("nsample" %in% names(phase_md) &
            inherits(phase_md$nsample, "numeric")) {
          nsample <- phase_md$nsample
        } else if ("nsample" %in% names(survey_md) &
            inherits(survey_md$nsample, "numeric")) {
          nsample <- survey_md$nsample
        } else {
          stop("'nsample' must be specified or available in metadata")
        }
      } else {
        nsample <- arguments$nsample
      }

      if (is.null(arguments$detailed)) {
        if ("detailed" %in% names(wave_md) &
            inherits(wave_md$detailed, "logical")) {
          detailed <- wave_md$detailed
        } else if ("detailed" %in% names(phase_md) &
            inherits(phase_md$detailed, "logical")) {
          detailed <- phase_md$detailed
        } else if ("detailed" %in% names(survey_md) &
            inherits(survey_md$detailed, "logical")) {
          detailed <- survey_md$detailed
        } else {
          detailed <- TRUE
        }
      } else {
        detailed <- arguments$detailed
      }

      output <- allocate_wave(
        data = data, y = y,
        already_sampled = already_sampled, strata = strata,
        nsample = nsample, method = method,
        allocation_method = allocation_method,
        detailed = detailed
      )

      x_updated <- x
      x_updated@phases[[phase]]@waves[[wave]]@design <- output
      return(x_updated)
    }
    # sample_strata
    if (fun == "sample_strata") {
      # Get data
      if ((phase == 2 | phase == "phase2") & (wave == 1 | wave == "wave1")) {
        data <- x@phases$phase1$data
      } else if (wave == 1 | wave == "wave1") {
        data <- x@phases[[phase - 1]]@waves[[
        length(x@phases[[phase - 1]]@waves)]]@data
      } else if (wave != 1) {
        data <- x@phases[[phase]]@waves[[
        wave - 1]]@data
      } else {
        stop("sample strata cannot be performed in Phase 1")
      }

      # Get design_data

      design_data <- x@phases[[phase]]@waves[[wave]]@design
      if (nrow(design_data) == 0) {
        stop("'design' slot of specified wave must be filled with valid
         design dataframe")
      }

      if (nrow(data) == 0) {
        stop("'data' slot of previous wave must contain data to be used
         for sample allocation calculations")
      }
      wave_md <- x@phases[[phase]]@waves[[wave]]@metadata
      phase_md <- x@phases[[phase]]@metadata
      survey_md <- x@metadata

      # Check for args to sample_strata in the metadata.
      # Start in wave and move up.

      # strata (in data). Can be "strata" in metadata
      if (is.null(arguments$strata)) {
        if ("strata" %in% names(wave_md) &
            inherits(wave_md$strata, "character")) {
          strata <- wave_md$strata
        } else if ("strata" %in% names(phase_md) &
            inherits(phase_md$strata, "character")) {
          strata <- phase_md$strata
        } else if ("strata" %in% names(survey_md) &
            inherits(survey_md$strata, "character")) {
          strata <- survey_md$strata
        } else {
          stop("'strata' must be specified or
               available in metadata")
        }
      } else if (!(arguments$strata %in% names(data))) {
        stop("'strata' must be a column name of the dataframe in the
         'data' slot of the previous wave")
      } else {
        strata <- arguments$strata
      }

      # design_strata. Can be "design_strata" or "strata" in metadata.
      # Will check in that order.

      if (is.null(arguments$design_strata)) {
        if ("design_strata" %in% names(wave_md) &
            inherits(wave_md$design_strata, "character")) {
          design_strata <- wave_md$design_strata
        } else if ("design_strata" %in% names(phase_md) &
            inherits(phase_md$design_strata, "character")) {
          design_strata <- phase_md$design_strata
        } else if ("design_strata" %in% names(survey_md) &
            inherits(survey_md$design_strata, "character")) {
          design_strata <- survey_md$design_strata
        } else if ("strata" %in% names(wave_md) &
            inherits(wave_md$strata, "character")) {
          design_strata <- wave_md$strata
        } else if ("strata" %in% names(phase_md) &
            inherits(phase_md$strata, "character")) {
          design_strata <- phase_md$strata
        } else if ("strata" %in% names(survey_md) &
            inherits(survey_md$strata, "character")) {
          design_strata <- survey_md$strata
        } else {
          design_strata <- "strata"
        }
      } else {
        design_strata <- arguments$design_strata
      }

      if (!(design_strata %in% names(design_data))) {
        stop("'design_strata' must be a column name of the dataframe in the
         'design' slot of the specified wave.")
      }

      # Probs
      if (is.null(arguments$probs)) {
        if ("probs" %in% names(wave_md) &
            inherits(wave_md$probs, "character")) {
          probs <- wave_md$probs
        } else if ("probs" %in% names(phase_md) &
                   inherits(phase_md$probs, "character")) {
          probs <- phase_md$probs
        } else if ("probs" %in% names(survey_md) &
                   inherits(survey_md$probs, "character")) {
          probs <- survey_md$probs
        } else {
          probs <- NULL
        }
      } else if (is.character(arguments$probs)) {
        if(!(arguments$probs %in% names(design_data))){
            stop("'probs' must be a formula or column name of the 'design_data'
            slot of the specified wave")
        }
      }
      probs <- arguments$probs



      # Now id
      if (is.null(arguments$id)) {
        if ("id" %in% names(wave_md) & inherits(wave_md$id, "character")) {
          id <- wave_md$id
        } else if ("id" %in% names(phase_md) &
            inherits(phase_md$id, "character")) {
          id <- phase_md$id
        } else if ("id" %in% names(survey_md) &
            inherits(survey_md$id,  "character")) {
          id <- survey_md$id
        } else {
          stop("'id' must be specified or available in metadata")
        }
      } else {
        id <- arguments$id
      }

      if (!(id %in% names(data))) {
        stop("'id' must be a column name of the dataframe in the
        'data' slot of the previous wave.")
      }

      # already_sampled
      if (is.null(arguments$already_sampled)) {
        if ("already_sampled" %in% names(wave_md) &
            inherits(wave_md$already_sampled, "character")) {
          already_sampled <- wave_md$already_sampled
        } else if ("already_sampled" %in% names(phase_md) &
            inherits(phase_md$already_sampled, "character")) {
          already_sampled <- phase_md$already_sampled
        } else if ("already_sampled" %in% names(survey_md) &
            inherits(survey_md$already_sampled, "character")) {
          already_sampled <- survey_md$already_sampled
        } else {
          already_sampled <- NULL
        }
      } else {
        already_sampled <- arguments$already_sampled
      }

      if (!is.null(already_sampled)) {
        if (!(already_sampled %in% names(data))) {
          stop("'already_sampled' must be a column name of the dataframe in the
          'data' slot of the previous wave.")
        }
      }

      # n_allocated
      if (is.null(arguments$n_allocated)) {
        if ("n_allocated" %in% names(wave_md) &
            inherits(wave_md$n_allocated, "character")) {
          n_allocated <- wave_md$n_allocated
        } else if ("n_allocated" %in% names(phase_md) &
            inherits(phase_md$n_allocated, "character")) {
          n_allocated <- phase_md$n_allocated
        } else if ("n_allocated" %in% names(survey_md) &
            inherits(survey_md$n_allocated, "character")) {
          n_allocated <- survey_md$n_allocated
        } else {
          n_allocated <- "n_to_sample"
          if (!(n_allocated %in% names(design_data))) {
            n_allocated <- "n_to_sample"
          }
        }
      } else {
        n_allocated <- arguments$n_allocated
      }

      if (!(n_allocated %in% names(design_data))) {
        stop("'n_allocated' must be a column name of the dataframe in the
        'design' slot of the specified wave.")
      }


      output <- sample_strata(
        data = data, id = id,
        strata = strata, already_sampled = already_sampled,
        design_data = design_data,
        design_strata = design_strata,
        n_allocated = n_allocated,
        probs = probs,
        wave = NULL,
        warn_prob_overwrite = FALSE
      )

      x_updated <- x
      sample_indicator <- NULL
      samps <- dplyr::filter(output, sample_indicator == 1)
      x_updated@phases[[phase]]@waves[[wave]]@samples$ids <-
        samps$id
      if(!(is.null(probs))){
        x_updated@phases[[phase]]@waves[[wave]]@samples$probs <-
        samps$sampling_prob
      } else{
        x_updated@phases[[phase]]@waves[[wave]]@samples$probs <- c()
      } # To ensure incorrect/old probs are removed

      return(x_updated)
    }

    # merge_samples
    if (fun == "merge_samples") {
      wave_md <- x@phases[[phase]]@waves[[wave]]@metadata
      phase_md <- x@phases[[phase]]@metadata
      survey_md <- x@metadata
      if (is.null(arguments$id)) {
        if ("id" %in% names(wave_md) &
            inherits(wave_md$id, "character")) {
          id <- wave_md$id
        } else if ("id" %in% names(phase_md) &
            inherits(phase_md$id, "character")) {
          id <- phase_md$id
        } else if ("id" %in% names(survey_md) &
            inherits(survey_md$id, "character")) {
          id <- survey_md$id
        } else {
          stop("'id' must be specified or available in metadata")
        }
      } else {
        id <- arguments$id
      }

      if (is.null(arguments$phase_sample_ind)) {
        if ("phase_sample_ind" %in% names(wave_md) &
            inherits(wave_md$phase_sample_ind, "character")) {
          phase_sample_ind <- wave_md$phase_sample_ind
        } else if ("phase_sample_ind" %in% names(phase_md) &
            inherits(phase_md$phase_sample_ind, "character")) {
          phase_sample_ind <- phase_md$phase_sample_ind
        } else if ("phase_sample_ind" %in% names(survey_md) &
            inherits(survey_md$phase_sample_ind, "character")) {
          phase_sample_ind <- survey_md$phase_sample_ind
        } else {
          phase_sample_ind <- "phase_sample_ind"
        }
      } else {
        phase_sample_ind <- arguments$phase_sample_ind
      }

      if (is.null(arguments$wave_sample_ind)) {
        if ("wave_sample_ind" %in% names(wave_md) &
            inherits(wave_md$wave_sample_ind, "character")) {
          wave_sample_ind <- wave_md$wave_sample_ind
        } else if ("wave_sample_ind" %in% names(phase_md) &
                   inherits(phase_md$wave_sample_ind, "character")) {
          wave_sample_ind <- phase_md$wave_sample_ind
        } else if ("wave_sample_ind" %in% names(survey_md) &
                   inherits(survey_md$wave_sample_ind, "character")) {
          wave_sample_ind <- survey_md$wave_sample_ind
        } else {
          wave_sample_ind <- "wave_sample_ind"
        }
      } else {
        wave_sample_ind <- arguments$wave_sample_ind
      }

      # Get include_probs if given include_probs is NULL
      if(is.null(arguments$include_probs)){
        if ("include_probs" %in% names(wave_md)){
          include_probs <- wave_md$include_probs
        } else if ("include_probs" %in% names(phase_md)) {
          include_probs <- phase_md$include_probs
        } else if ("include_probs" %in% names(survey_md)) {
          include_probs <- survey_md$include_probs
        } else{
          include_probs <- FALSE
        }
      } else{
        include_probs <- arguments$include_probs
        }


      x_updated <- merge_samples(
        x = x, phase = phase, wave = wave, id = id,
        phase_sample_ind = phase_sample_ind,
        wave_sample_ind = wave_sample_ind,
        include_probs = include_probs
      )
      return(x_updated)
    }
  }
)
