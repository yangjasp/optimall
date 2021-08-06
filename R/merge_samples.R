#' Merge Sampled Data based on IDs
#'
#' In an object of class \code{"Mutiwave"}, \code{merge_samples} creates
#' a dataframe in the \code{"data"} slot of the specified wave by merging
#' the dataframe in the \code{"sampled data"} slot with the dataframe in
#' the \code{"data"} slot of the previous wave.
#'
#' If a column name in the \code{"sampled data"} matches a column name in
#' the \code{"data"} slot of the previous wave, these columns will be
#' merged into one column with the same name in the output dataframe.
#' For ids that have non-missing values in both columns of the merge,
#' the value from
#' \code{"sampled_data"} will overwrite the previous value and a warning
#' will be printed. All ids present in the \code{"data"} from the previous
#' wave but missing from \code{"sampled_data"} will be given NA values
#' for the newly merged variables
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
#' @param sampled_ind a character value specifying the name of the column that
#' should hold the indicator of whether each unit has already been sampled in
#' the current phase.
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
#' MySurvey <- new_multiwave(phases = 2, waves = c(1, 3))
#' get_data(MySurvey, phase = 1, slot = "data") <-
#'   data.frame(dplyr::select(iris, -Sepal.Width))
#' get_data(MySurvey, phase = 2, wave = 1, slot = "sampled_data") <-
#'   dplyr::select(iris, id, Sepal.Width)[1:40, ]
#' MySurvey <- merge_samples(MySurvey, phase = 2, wave = 1, id = "id")
#' @importFrom rlang :=
setGeneric("merge_samples", function(x, phase, wave,
                                     id = NULL,
                                     sampled_ind = "already_sampled_ind") {
  standardGeneric("merge_samples")
})
setMethod(
  "merge_samples", c(x = "Multiwave"),
  function(x, phase, wave, id = NULL,
           sampled_ind = "already_sampled_ind") {
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
    if (!is.character(sampled_ind) | length(sampled_ind) != 1) {
      stop("'sampled_ind' must be a character value specifying the desired
    name of the column in the newly merged data that should hold a
    binary indicator for whether each unit has been sampled in the current
    wave.")
    }

    # Get previous wave data

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

    # Add indicator for already sampled

    already_sampled_ids <- list()

    for (i in seq_len(wave)) {
      already_sampled_ids[[i]] <- get_data(x,
        phase = phase,
        wave = i,
        slot = "samples"
      )
    }

    if (any(sapply(already_sampled_ids, length) == 0)) {
      warning("some 'samples' slots of previous waves in this phase are
            empty. The `sampled_ind` column of the newly merged data may
            be inaccurate.")
    }

    already_sampled_ids <- unlist(already_sampled_ids)

    output_data[, sampled_ind] <-
      ifelse(output_data[,id] %in% already_sampled_ids, 1, 0)

    get_data(x, phase = phase, wave = wave) <- output_data
    return(x)
  }
)
