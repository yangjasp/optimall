#' Generate IDs to sample in Multiwave Object
#' @export
#' @include multiwave.R phase.R wave.R sample_strata.R
sample_strata.Multiwave <- function(data1, phase, wave,
                                    strata1 = NULL, id = NULL,
                                    wave2a = NULL,
                                    data2 = NULL, strata2 = NULL,
                                    n_allocated = NULL){
  x <- data1

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

  # Check for args to allocate_wave in the metadata.
  # Start in wave and move up.

  # strata1. Can be "strata1" or "strata" in metadata
  if (is.null(strata1)){
    if("strata1" %in% names(wave_md) & class(wave_md$strata1)
            == "character"){
      strata1 <- wave_md$strata1
    } else if("strata1" %in% names(phase_md) &
              class(phase_md$strata1) == "character"){
      strata1 <- phase_md$strata1
    } else if("strata1" %in% names(survey_md) &
              class(survey_md$strata1) == "character"){
      strata1 <- phase_md$strata1
    } else if("strata" %in% names(wave_md) & class(wave_md$strata) == "character"){
      strata1 <- wave_md$strata
    } else if("strata" %in% names(phase_md) &
              class(phase_md$strata) == "character"){
      strata1 <- phase_md$strata
    } else if("strata" %in% names(survey_md) &
              class(survey_md$strata) == "character"){
      strata1 <- phase_md$strata
    } else {
      stop("'strata' must be specified or available in metadata")
    }
  } else if (!(strata1 %in% names(data1))){
    stop("'strata' must be a column name of the dataframe in the
         'data' slot of the previous wave")
  }

  # strata2. Can be "strata2" or "strata" in metadata. Will check in
  #that order.
  if (is.null(strata2)){
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
      stop("'strata' must be specified or available in metadata")
    }
  } else if (!(strata1 %in% names(data1))){
    stop("'strata' must be a column name of the dataframe in the
         'data' slot of the previous wave.")
  }


  # Now id
  if (is.null(id)){
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
  } else if (!(id %in% names(data1))){
    stop("'id' must be a column name of the dataframe in the
         'data' slot of the previous wave.")
  }

  # wave2a
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
      wave2a <- NULL
    }
  } else if (!(wave2a %in% names(data1))){
    stop("'wave2a' must be a column name of the dataframe in the
         'data' slot of the previous wave.")
  }

  # n_allocated
  if (is.null(n_allocated)){
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
      stop("'n_allocated', the name of the column in 'data2' that
      indicates how many samples should be allocated to each stratum,
           must be specified or available in metadata")
      }
    }
  } else if (!(n_allocated %in% names(data2))){
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
