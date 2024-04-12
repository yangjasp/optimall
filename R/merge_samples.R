#' Merge Sampled Data based on IDs
#'
#' In an object of class \code{"Mutiwave"}, \code{merge_samples} creates
#' a dataframe in the \code{"data"} slot of the specified wave by merging
#' the dataframe in the \code{"sampled data"} slot with the dataframe in
#' the \code{"data"} slot of the previous wave.
#'
#'
#' Columns in \code{"sampled_data"} that do not match names of the
#' \code{"data"} from the previous wave will be added as new columns in
#' the output dataframe. All ids that do not appear in
#' \code{"sampled_data"} will receive NA values for these new variables.
#'
#' @param x an object of class \code{"Multiwave"}.
#' @param phase A numeric value specifying the phase of the
#' Multiwave object that
#'  the specified wave is in. Cannot be phase 1.
#' @param wave A numeric value specifying the wave of the Multiwave
#'  object that the merge should be
#' performed in. This wave must have a valid dataframe in the
#' \code{"sampled data"} slot. The previous wave, taken as the final
#' wave of the previous phase if \code{wave} = 1, must have a valid
#' dataframe in the \code{"data"} slot.
#' @param id A character value specifying the name of the column holding unit
#' ids. Taken from wave, phase, or overall metadata (searched for in that
#' order) if \code{NULL}. Defaults to \code{NULL}.
#' @param phase_sample_ind a character value specifying the name of the column
#' that should hold the indicator of whether each unit has already been sampled
#' in the current \emph{phase}. The specified phase number will be appended
#' to the end of the given character name. Defaults to "sampled_phase".
#' @param wave_sample_ind a character value specifying the name of the column
#' that should hold the indicator of whether each unit has already been sampled
#' in the current \emph{wave}. The specified phase and wave numbers separated
#' by "." will be appended o the end of the given character name.
#' If FALSE, no such column is created. Defaults to "sampled_wave".
#' @param include_probs A logical value. If TRUE, looks for "probs" in
#' the \code{design_data} slot and includes the corresponding sampling
#' probability for each element sampled in the current wave in the merged data
#' in a column named "sampling_prob". If this column already exists, it keeps
#' the existing column and adds (or replaces) the values for units sampled in
#' the current wave. Returns an error if specified but
#' \code{wave_sample_wave} is FALSE.
#' Defaults to NULL, which looks for "probs" argument in
#' metadata and does not create (or add to existing) "sampling_prob" column if
#' none is found.
#' @details
#' If a column name in the \code{"sampled_data"} matches a column name in
#' the \code{"data"} slot of the previous wave, these columns will be
#' merged into one column with the same name in the output dataframe.
#' For ids that have non-missing values in both columns of the merge,
#' the value from \code{"sampled_data"} will overwrite the previous value
#' and a warning will be printed. All ids present in the \code{"data"} from the
#' previous wave but missing from \code{"sampled_data"} will be given NA values
#' for the newly merged variables.
#'
#' If columns with the name produced by \code{phase_sample_ind} or
#' \code{wave_sample_ind} already exist, they will be overwritten.
#'
#'
#' @return A Multiwave object with the merged dataframe in the
#' \code{"data"} slot of the specified wave.
#' @export
#' @include multiwave.R phase.R wave.R
#' @aliases merge_samples,Multiwave-method
#'
#' @examples
#' library(datasets)
#' iris <- data.frame(iris, id = 1:150)
#'
#' MySurvey <- multiwave(phases = 2, waves = c(1, 3))
#' set_mw(MySurvey, phase = 1, slot = "data") <-
#'   data.frame(dplyr::select(iris, -Sepal.Width))
#' set_mw(MySurvey, phase = 2, wave = 1, slot = "sampled_data") <-
#'   dplyr::select(iris, id, Sepal.Width)[1:40, ]
#' set_mw(MySurvey, phase = 2, wave = 1, slot = "samples") <-
#'    list(ids = 1:40)
#' MySurvey <- merge_samples(MySurvey, phase = 2, wave = 1, id = "id")
#' @importFrom rlang :=
setGeneric("merge_samples", function(x, phase, wave,
                                     id = NULL,
                                     phase_sample_ind = "sampled_phase",
                                     wave_sample_ind = "sampled_wave",
                                     include_probs = NULL) {
  standardGeneric("merge_samples")
})
setMethod(
  "merge_samples", c(x = "Multiwave"),
  function(x, phase, wave, id = NULL,
           phase_sample_ind = NULL,
           wave_sample_ind = NULL,
           include_probs = NULL) {
    if (!is.numeric(phase) |
      !(phase %in% c(seq_len(length(x@phases))) & phase > 1)) {
      stop("'phase' must be a numeric value specifying a valid phase
      in 'x'")
    }
    if (!is.numeric(wave) |
      !(wave %in% c(seq_len(length(x@phases[[phase]]@waves))))) {
      stop("'wave' must be a numeric value specifying a valid wave in
      'phase' in 'x'")
    }

    # Get previous wave data.

    if ((phase == 2 | phase == "phase2") & (wave == 1 | wave == "wave1")) {
      previous_wave_data <- x@phases$phase1$data
    } else if (wave == 1 | wave == "wave1") {
      previous_wave_data <- x@phases[[phase - 1]]@waves[[
      length(x@phases[[phase - 1]]@waves)]]@data
    } else if (wave != 1) {
      previous_wave_data <- x@phases[[phase]]@waves[[
      wave - 1]]@data
    }

    # Get id if given id is NULL
    if (is.null(id)) {
      if ("id" %in% names(x@phases[[phase]]@waves[[wave]]@metadata) # &
        # inherits(x@phases[[phase]]@waves[[wave]]@metadata$id,
        # "character")
      ) {
        id <- x@phases[[phase]]@waves[[wave]]@metadata$id
      } else if ("id" %in% names(x@phases[[phase]]@metadata) # &
        # inherits(x@phases[[phase]]@waves[[wave]]@metadata$id,
        # "character")
      ) {
        id <- x@phases[[phase]]@metadata$id
      } else if ("id" %in% names(x@metadata) # &
        # inherits(x@phases[[phase]]@waves[[wave]]@metadata$id,
        # "character")
      ) {
        id <- x@metadata$id
      } else {
        stop("Could not find character element with name
      'id' in metadata. Add
           or specify in function call.")
      }
    }

    if (!(id %in% names(x@phases[[phase]]@waves[[wave]]@sampled_data)) |
      !(id %in% names(previous_wave_data))) {
      stop("'id' must be a character value specifying the name of the
         column that holds unit ids in both 'sampled_data' of this wave
         and 'data' of the previous wave")
    }

    # Print warning about ids if necessary
    if (!(all(
      x@phases[[phase]]@waves[[wave]]@sampled_data[[id]] %in%
        previous_wave_data[[id]]
    ))) {
      warning("sampled_data is introducing new ids to the data")
    }

    # Get include_probs if given include_probs is NULL
    if(is.null(include_probs)){
      if ("include_probs" %in% names(x@phases[[phase]]@waves[[wave]]@metadata)){
        include_probs <- x@phases[[phase]]@waves[[wave]]@metadata$include_probs
      } else if ("include_probs" %in% names(x@phases[[phase]]@metadata)) {
        include_probs <- x@phases[[phase]]@metadata$include_probs
      } else if ("include_probs" %in% names(x@metadata)) {
        include_probs <- x@metadata$include_probs
      } else{
        include_probs <- FALSE
      }
    }

    # Error if include_probs is not logical
    if(!is.logical(include_probs)){
      stop("'include_probs' must be TRUE, FALSE, or NULL")
    }

    # Get phase_sample_ind if given phase_sample_ind is NULL
    if (is.null(phase_sample_ind)) {
      if ("phase_sample_ind" %in% names(x@phases[[phase]]@waves[[wave]]@metadata)
      ) {
        phase_sample_ind <-
          x@phases[[phase]]@waves[[wave]]@metadata$phase_sample_ind
      } else if ("phase_sample_ind" %in% names(x@phases[[phase]]@metadata)) {
        phase_sample_ind <- x@phases[[phase]]@metadata$phase_sample_ind
      } else if ("phase_sample_ind" %in% names(x@metadata)) {
        phase_sample_ind <- x@metadata$phase_sample_id
      } else {
       phase_sample_ind <- "sampled_phase"
      }
    }

    # Get wave_sample_ind if given wave_sample_ind is NULL
    if (is.null(wave_sample_ind)) {
      if ("wave_sample_ind" %in% names(x@phases[[phase]]@waves[[wave]]@metadata)
      ) {
        wave_sample_ind <-
          x@phases[[phase]]@waves[[wave]]@metadata$wave_sample_ind
      } else if ("wave_sample_ind" %in% names(x@phases[[phase]]@metadata)) {
        wave_sample_ind <- x@phases[[phase]]@metadata$wave_sample_ind
      } else if ("wave_sample_ind" %in% names(x@metadata)) {
        wave_sample_ind <- x@metadata$wave_sample_id
      } else {
        wave_sample_ind <- "sampled_wave"
      }
    }

    # Require wave_sample_ind to exist if include_probs is true, so wave is
    # always attached to probability
    if(include_probs == TRUE & wave_sample_ind == FALSE){
      stop("'wave_sample_ind must be specified if 'include_probs' is TRUE.")
    }

    # Errors if given wave ind or phase ind are not characaters
    if (!is.character(phase_sample_ind) | length(phase_sample_ind) != 1) {
      stop("'phase_sampled_ind' must be a character value specifying the desired
    name of the column in the newly merged data that should hold a
    binary indicator for whether each unit has been sampled in the current
    phase.")
    }
    if (!is.character(wave_sample_ind) & wave_sample_ind != FALSE |
        length(wave_sample_ind) != 1) {
      stop("'wave_sampled_ind' must be FALSE or a character value specifying the
      desired name of the column in the newly merged data that should hold a
      binary indicator for whether each unit has been sampled in the current
           wave.")
    }

    # Perform merge
    output_data <-
      dplyr::full_join(previous_wave_data,
        x@phases[[phase]]@waves[[wave]]@sampled_data,
        by = id
      )

    # Fix duplicate columns
    dup_cols <- names(previous_wave_data)[
      names(previous_wave_data) %in% names(
        x@phases[[phase]]@waves[[wave]]@sampled_data
      )
    ]
    dup_cols <- dup_cols[dup_cols != id]


    if (length(dup_cols > 0)) {
      for (i in seq_along(dup_cols)) {
        # Warning if non-NA for both values for any row
        if (any(!is.na(sort(previous_wave_data[
          previous_wave_data[, id] %in%
            x@phases[[phase]]@waves[[wave]]@sampled_data[, id],
          dup_cols[i]
        ])) &
          !is.na(sort(x@phases[[phase]]@waves[[wave]]@sampled_data[
            , dup_cols[i]
          ])))) {
          warning("Some units in 'sampled_data' have non-NA values already in
                columns that are being merged. The old values will be
                overwritten by the values in 'sampled_data' for these
                units.")
        }

        new_col_name <- as.character(dup_cols[i])
        new_col_namex <- paste0(new_col_name, ".x")
        new_col_namey <- paste0(new_col_name, ".y")
        new_col_name_sym <- enquo(new_col_name)
        new_col_namex_sym <- enquo(new_col_namex)
        new_col_namey_sym <- enquo(new_col_namey)
        output_data <- output_data %>%
          dplyr::mutate(!!new_col_name := dplyr::coalesce(
            !!sym(new_col_namey),
            !!sym(new_col_namex)
          )) %>%
          dplyr::select(
            -paste0(new_col_name, ".x"),
            -paste0(new_col_name, ".y")
          )
      }
    }

    # Add indicator for already sampled in current phase

    already_sampled_phase_ids <- list()
    warn_empty <- FALSE

    for (i in seq_len(wave)) {
      already_sampled_phase_ids[[i]] <- get_mw(x,
        phase = phase,
        wave = i,
        slot = "samples"
      )$ids

      if (length(
        get_mw(x,
              phase = phase,
              wave = i,
              slot = "samples"
        )$ids) == 0) {
        warn_empty <- TRUE
      }
    }

    if(warn_empty == TRUE){
      warning("some 'samples' slots of waves in this phase are
            empty. The `sampled_ind` column of the newly merged data may
            be inaccurate.")
    }


    already_sampled_phase_ids <- unlist(already_sampled_phase_ids)

    phase_col_name <- paste0(phase_sample_ind, phase)

    output_data[, phase_col_name] <-
      ifelse(output_data[,id] %in% already_sampled_phase_ids, 1, 0)

    # Add indicator for sampled in current wave

    if(wave_sample_ind != FALSE){
      wave_col_name <- paste0(wave_sample_ind, phase, ".", wave)

      output_data[, wave_col_name] <-
        ifelse(output_data[,id] %in% get_mw(x,
                                          phase = phase,
                                          wave = wave,
                                          slot = "samples")$ids, 1, 0)

      if(all(sort(unique(get_mw(x,
               phase = phase,
               wave = wave,
               slot = "samples")$ids)) !=
         sort(unique(x@phases[[phase]]@waves[[wave]]@sampled_data[,id])))){
        warning("ids in 'samples' slot were used to create sample indicator,
                but they do not match ids in 'sampled_data'")
      }
    }

    # Get probs from design_data if include_probs is TRUE.
    if(include_probs == TRUE){
      if(length(x@phases[[phase]]@waves[[wave]]@samples$probs) > 0){
        if(length(x@phases[[phase]]@waves[[wave]]@samples$probs) !=
           length(x@phases[[phase]]@waves[[wave]]@samples$ids)){
          stop(" 'probs' and 'ids' in 'samples'
               slot should be the same length.")
        } else{
          probs_df <-
            data.frame(x@phases[[phase]]@waves[[wave]]@samples$ids,
                       x@phases[[phase]]@waves[[wave]]@samples$probs)
          names(probs_df) <- c(id, "sampling_prob")
          # First if 'sampling_probs' is already a column in data, write probs
          # from 'design_data' only for new samples, leaving any old values
          if("sampling_prob" %in% names(output_data)){
            temp1 <- output_data[,c(id, "sampling_prob")]
            temp2 <- dplyr::left_join(temp1, probs_df, by = id)
            updated_probs <- dplyr::coalesce(temp2$sampling_prob.y,
                                             temp2$sampling_prob.x)
            output_data$sampling_prob <- updated_probs
          } else{
            output_data <- output_data %>%
              dplyr::left_join(probs_df, by = id)
          }
        }
      } else{
        warning("include_probs is TRUE, but no 'probs' found in samples slot.")
      }
    }

    # Add output_data to data slot of current wave
    set_mw(x, phase = phase, wave = wave, slot ="data") <- output_data
    return(x)
  }
)
