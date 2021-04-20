#' Split Strata
#'
#' Splits pre-defined sampling strata based on values of a
#' continuous or categorical variable.
#'
#' For splits on continuous variables, the new strata are defined
#' on left-open intervals. The only exception is the first interval,
#' which must include the overall minimum value. The names of the newly
#' created strata for a split generated
#' from a continuous value are the `split_var` column name with
#' the range of values defining that stratum appended to the
#' old strata name. For a categorical split, the new strata names
#' are the `split_var` column name appended to the
#' 1/0 logical flag specifying whether the unit is in \code{split at},
#' all appended to the old strata name.
#' If the `split_var` column name is long,
#' the user can specify a value for `trunc` to prevent the new
#' strata names from being inconveniently long.
#'
#' @param data a dataframe or matrix with one row for each
#' sampling unit, one column specifying each unit's current
#' stratum, one column containing the continuous or categorical
#' values that will define the split, and any other relevant
#' columns.
#' @param strata a character string specifying the name of the
#' column that defines each unit's current strata.
#' @param split the name of the stratum or strata to be split,
#' exactly as they appear in \code{strata}. Defaults to NULL,
#' which indicates that all strata in \code{strata} will be split.
#' @param split_var a character string specifying the name of the
#' column that should be used to define the strata splits.
#' @param split_at the percentile, value, or name(s) which
#' \code{split_var} should be split at. The interpretation of
#' this input depends on \code{type}. For \code{"quantile"} types,
#' input must be between \code{0} and \code{1}. Defaults to
#' \code{0.5} (median). For \code{"categorical"} type, the
#' input should be a vector of values or names in \code{split_var}
#' that define the new stratum.
#' @param type a character string specifying how the function
#' should interpret the \code{split_at} argument. Must be one of:
#' \itemize{
#' \item \code{"global quantile"}, the default, splits the strata
#' at the quantiles specified in \code{split_at} defined along
#' the entire, unfiltered \code{split_var} column.
#' \item \code{"local quantile"} splits the strata at the
#' quantiles specified in \code{split_at} defined along the
#' filtered \code{split_var} column which only includes units in
#' the stratum being split.
#' \item \code{"value"} splits the strata at the values specified
#' in \code{split_at} along \code{split_var} column.
#' \item \code{"categorical"} splits the strata into two new
#' strata, one that contains each unit where \code{split_var}
#' matches an input of \code{split_at}, and a second that contains
#' every other unit.
#' }
#' @param trunc A numeric or character value specifying how the
#' name of the \code{split_var} should be truncated when naming
#' the new strata. If numeric, the new strata name will only
#' include the first 'n' characters of the \code{split_var} name.
#' If character, the specified string will be used to name the new
#' strata instead of the \code{split_var} name. Defaults to
#' \code{NULL}, which creates the new strata name using the entire
#' name of the \code{split_var} column.
#' @examples
#' x <- split_strata(iris, "Sepal.Length",
#'   strata = c("Species"),
#'   split = "setosa", split_var = "Sepal.Width",
#'   split_at = c(0.5), type = "global quantile"
#' )
#'
#' # You can split at more than one quantile in one call.
#' # The above call splits the "setosa" stratum into three of equal size
#' x <- split_strata(iris, "Sepal.Length",
#'   strata = c("Species"),
#'   split = "setosa", split_var = "Sepal.Width", split_at = c(0.33, 0.66),
#'   type = "local quantile"
#' )
#'
#' # Manually select split values with type = "value"
#' x <- split_strata(iris, "Sepal.Length",
#'   strata = "Species",
#'   split = "setosa", split_var = "Sepal.Width",
#'   split_at = c(3.1, 3.8), type = "value"
#' )
#'
#' # Perform a categorical split.
#' iris$strata <- rep(c(rep(1, times = 25), rep(0, times = 25)), times = 3)
#' x <- split_strata(iris, "Sepal.Length",
#'   strata = "strata",
#'   split = NULL, split_var = "Species",
#'   split_at = c("virginica", "versicolor"), type = "categorical"
#' )
#' # Splits each initial strata 1 and 2 into one stratum with "virginia"
#' # and "versicolor" species and one stratum with all of the other species
#' # not specified in the split_at argument.
#' @export
#' @return Returns the input dataframe with a new column named
#' 'new_strata' that holds the name of the stratum that each
#' sample belongs to after the split. The column containing the
#' previous strata names is retained and given the name "old_strata".
#' @importFrom magrittr %>%

split_strata <- function(data, strata, split = NULL,
                         split_var, type = "global quantile",
                         split_at = .5, trunc = NULL) {
  old_strata <- split_variable <- split_var_updated <- NULL
  # bind global vars as necessary
  if (is.matrix(data)) {
    data <- as.data.frame(data)
  }
  if (is.data.frame(data) == FALSE) {
    stop("'data' must be a dataframe or matrix with named columns")
  }
  if (split_var %in% names(data) == FALSE) {
    stop("'split_var' must be a string matching a column name of 'data'")
  }
  if (all(strata %in% names(data)) == FALSE) {
    stop("'Strata' must be a string or vector of strings
         matching column names of data.")
  }
  if (all(strata != "old_strata") & "old_strata" %in% names(data)) {
    data <- dplyr::select(data, -old_strata)
    # fixes error from assigning duplicate names
  }
  if (length(strata) > 1) {
    strata_q <- enquo(strata)
    strata_interact <- data %>%
      dplyr::select(!!strata_q)
    strata_interact <- interaction(strata_interact)
    data <- cbind(data, strata_interact)
    data <- dplyr::select(data, -!!strata_q)
    names(data)[names(data) == "strata_interact"] <- "old_strata"
  } else {
    names(data)[names(data) == strata] <- "old_strata"
  }
  if (is.null(split) == FALSE) {
    # split <- match.arg(split, unique(data[,"old_strata"]))
    if (all(split %in% data[, "old_strata"]) == FALSE) {
      stop(paste(
        paste("'", split, "'", sep = ""),
        "does not match any value in 'strata'"
      ))
    }
  } else if (is.null(split) == TRUE) {
    split <- sort(unique(data[, "old_strata"]))
  }

  # Check for a column that is sorted to preserve order later.
  test <- vector()
  for (i in seq_len(ncol(data))) {
    test[i] <- is.unsorted(data[, i])
    if (is.na(test[i])) {
      test[i] <- TRUE
    }
    if (test[i] == FALSE) {
      break
    }
  }
  if (any(test == FALSE)) {
    sort_by <- names(data)[which(test == FALSE)]
  } else {
    sort_by <- NULL
  }

  names(data)[names(data) == split_var] <- "split_variable"
  data$old_strata <- as.character(data$old_strata)
  type <- match.arg(
    type,
    c(
      "global quantile", "local quantile",
      "value", "categorical"
    )
  )
  if (type %in% c(
    "global quantile", "local quantile", "value",
    "categorical"
  ) == FALSE) {
    stop("'type' must be one of 'global quantile',
         'local quantile', 'value', 'categorical'")
  }
  if (length(trunc) > 1) {
    stop("'trunc' must be a single numeric or character value
         specifying how the name of 'split_var' should be used in
         the new strata names")
  }

  # Create new_name to use for new strata names.
  if (is.null(trunc) == FALSE) {
    if (is.numeric(trunc) & trunc > 0) {
      new_name <- substr(split_var, 1, trunc)
    } else if (is.numeric(trunc) & trunc < 0) {
      new_name <- substr(
        split_var, nchar(split_var) + trunc + 1,
        nchar(split_var)
      )
    } else if (is.character(trunc)) {
      new_name <- trunc
    } else {
      stop("'trunc' must be a single numeric or character value
           specifying how the name of 'split_var' should be used
           in the new strata names")
    }
  } else if (is.null(trunc) == TRUE) {
    new_name <- split_var
  } else {
    stop("'trunc' must be a single numeric or character value
         specifying how the name of 'split_var' should be used in
         the new strata names")
  }

  if (type %in% c("global quantile", "local quantile", "value") &
    length(split) <= 1) {
    if (is.numeric(data$split_variable) == FALSE) {
      stop("'split_var' must be a column of 'data' holding numeric
           values. If you want to split on a categorical variable,
           use type = 'categorical'.")
    }
    if (type == "global quantile" |
      (is.null(split) & type == "local quantile")) {
      cut_point <- sort(stats::quantile(
        data[, "split_variable"],
        split_at
      )) # Find cut points
    }
    if (type == "local quantile" & is.null(split) == FALSE) {
      cut_point <- sort(stats::quantile(
        data[data$old_strata == split, "split_variable"], split_at
      ))
    }
    if (type == "value") {
      cut_point <- sort(split_at)
      if (is.null(split) == FALSE) {
        if (any(split_at < min(data[
          data$old_strata == split,
          "split_variable"
        ])) |
          any(split_at > max(data[
            data$old_strata == split,
            "split_variable"
          ]))) {
          warning("value(s) of 'split_at' are outside of the range
                  of values in 'split'")
        }
      }
      if (is.null(split)) {
        if (any(split_at < min(data$split_variable)) |
          any(split_at > max(data$split_variable))) {
          warning("value(s) of 'split_at' are outside of the range
                  of values in 'split'")
        }
      }
    }
    if (is.null(split) == FALSE) {
      data_filtered <- data %>%
        dplyr::filter(old_strata == split)
    }
    if (is.null(split) == TRUE) {
      data_filtered <- data
    }

    if (length(cut_point) == 1) {
      data_filtered <- data_filtered %>%
        dplyr::mutate(
          split_var_updated =
            ifelse(split_variable <= cut_point[1],
              paste(new_name,
                paste("[", round(min(data_filtered$split_variable),
                  digits = 2
                ), ",",
                round(cut_point[1], digits = 2), "]",
                sep = ""
                ),
                sep = "_"
              ),
              paste(new_name,
                paste("(", round(cut_point[1],
                  digits = 2
                ), ",",
                round(max(data_filtered$split_variable),
                  digits = 2
                ), "]",
                sep = ""
                ),
                sep = "_"
              )
            )
        )
    }
    if (length(cut_point) > 1) {
      cut_point <- c(
        min(data_filtered$split_variable),
        cut_point,
        max(data_filtered$split_variable)
      )
      data_filtered$split_var_updated <- data_filtered$split_variable
      data_filtered <- data_filtered %>%
        dplyr::mutate(
          split_var_updated =
            ifelse(split_variable <= cut_point[2],
              paste(new_name,
                paste("[", round(cut_point[1], digits = 2),
                  ",", round(cut_point[2], digits = 2), "]",
                  sep = ""
                ),
                sep = "_"
              ),
              "other"
            )
        )
      for (i in 3:length(cut_point)) {
        data_filtered <- data_filtered %>%
          dplyr::mutate(
            split_var_updated =
              ifelse(split_variable > cut_point[1] &
                split_variable > cut_point[i - 1] &
                split_variable <= cut_point[i],
              paste(new_name,
                paste("(",
                  round(cut_point[i - 1],
                    digits = 2
                  ),
                  ",",
                  round(cut_point[i],
                    digits = 2
                  ), "]",
                  sep = ""
                ),
                sep = "_"
              ),
              split_var_updated
              )
          )
      }
    }
    new_strata <- interaction(dplyr::select(
      data_filtered, old_strata,
      split_var_updated
    ))
    small_df <- cbind(new_strata, data_filtered)
    small_df <- dplyr::select(small_df, -split_var_updated)
  }

  if (type %in% c("global quantile", "local quantile", "value") &
    length(split) > 1) {
    if (is.numeric(data$split_variable) == FALSE) {
      stop(strwrap("'split_var' must be a column of 'data' holding
      numeric values. If you want to split on a categorical variable,
      use type = 'categorical'.", prefix = " ", initial = ""))
    }
    if (type == "global quantile") {
      cut_point_list <- list()
      for (i in seq_along(split)) {
        cut_point_list[[i]] <- sort(stats::quantile(
          data[, "split_variable"],
          split_at
        )) # Find cut points
      }
    }
    if (type == "local quantile") {
      get_cuts <- function(x) {
        sort(stats::quantile(data[
          data$old_strata == x,
          "split_variable"
        ], split_at))
      }
      cut_point_list <- lapply(split, get_cuts)
    }
    if (type == "value") {
      cut_point_list <- list()
      for (i in seq_along(split)) {
        cut_point_list[[i]] <- sort(split_at)
        # Find cut points, which are the same everywhere here.
      }
      # Warning if some cut points are outside of limits of some strata
      if (any(split_at < min(data[
        data$old_strata %in% split,
        "split_variable"
      ])) |
        any(split_at > max(data[
          data$old_strata %in% split,
          "split_variable"
        ]))) {
        warning(strwrap(paste0("value(s) of 'split_at' are outside of
                               the range of values in ", split),
          prefix = " ", initial = ""
        ))
      }
    }
    # Now perform splits. Each element of split gets the process
    # run over it.
    data_filtered_list <- list()
    for (j in seq_along(split)) {
      if (is.null(split[j]) == FALSE) {
        data_filtered <- data %>%
          dplyr::filter(old_strata == split[j])
      }
      if (is.null(split) == TRUE) { # shouldn't happen here
        data_filtered <- data
      }

      if (length(cut_point_list[[j]]) == 1) {
        cut_point <- cut_point_list[[j]]
        data_filtered <- data_filtered %>%
          dplyr::mutate(
            split_var_updated =
              ifelse(split_variable <= cut_point[1],
                paste(new_name,
                  paste("[", round(min(data_filtered$split_variable),
                    digits = 2
                  ), ",",
                  round(cut_point[1], digits = 2), "]",
                  sep = ""
                  ),
                  sep = "_"
                ),
                paste(new_name,
                  paste("(", round(cut_point[1],
                    digits = 2
                  ), ",",
                  round(max(data_filtered$split_variable),
                    digits = 2
                  ), "]",
                  sep = ""
                  ),
                  sep = "_"
                )
              )
          )
      }

      if (length(cut_point_list[[j]]) > 1) {
        cut_point <- cut_point_list[[j]]
        cut_point <- c(
          min(data_filtered$split_variable),
          cut_point,
          max(data_filtered$split_variable)
        )
        data_filtered$split_var_updated <- data_filtered$split_variable
        data_filtered <- data_filtered %>%
          dplyr::mutate(
            split_var_updated =
              ifelse(split_variable <= cut_point[2],
                paste(new_name,
                  paste("[", round(cut_point[1], digits = 2), ",",
                    round(cut_point[2], digits = 2), "]",
                    sep = ""
                  ),
                  sep = "_"
                ),
                "other"
              )
          )
        for (i in 3:length(cut_point)) {
          data_filtered <- data_filtered %>%
            dplyr::mutate(
              split_var_updated =
                ifelse(split_variable > cut_point[1] &
                  split_variable > cut_point[i - 1] &
                  split_variable <= cut_point[i],
                paste(new_name,
                  paste("(",
                    round(cut_point[i - 1],
                      digits = 2
                    ), ",",
                    round(cut_point[i],
                      digits = 2
                    ), "]",
                    sep = ""
                  ),
                  sep = "_"
                ),
                split_var_updated
                )
            )
        }
      }
      data_filtered_list[[j]] <- data_filtered
    }
    data_filtered_df <- dplyr::bind_rows(data_filtered_list)
    new_strata <- interaction(dplyr::select(
      data_filtered_df,
      old_strata,
      split_var_updated
    ))
    small_df <- cbind(new_strata, data_filtered_df)
    small_df <- dplyr::select(small_df, -split_var_updated)
  }


  if (type == "categorical") {
    data$split_variable <- as.character(data$split_variable)
    if (is.null(split) == TRUE) {
      data_filtered <- data
    } else {
      data_filtered <- dplyr::filter(data, old_strata %in% split)
    }
    data_filtered <- data_filtered %>%
      dplyr::mutate(
        split_var_updated =
          ifelse(split_variable %in% split_at,
            paste0(new_name, "_1"),
            paste0(new_name, "_0")
          )
      )
    new_strata <- interaction(dplyr::select(
      data_filtered,
      old_strata,
      split_var_updated
    ))
    data_filtered <- cbind(new_strata, data_filtered)
    small_df <- dplyr::select(data_filtered, -split_var_updated)
  }
  if (is.null(split)) {
    output_df <- small_df
  }
  if (is.null(split) == FALSE & length(split) == 1) {
    output_df <- data %>%
      dplyr::filter(old_strata != split) %>%
      dplyr::mutate(new_strata = old_strata)
    output_df <- rbind(output_df, small_df)
  }
  if (is.null(split) == FALSE & length(split) > 1) {
    output_df <- data %>%
      dplyr::filter(old_strata %in% split == FALSE) %>%
      dplyr::mutate(new_strata = old_strata)
    output_df <- rbind(output_df, small_df)
  }
  names(output_df)[names(output_df) == "split_variable"] <- split_var
  column_names_other <- names(output_df)[names(output_df) !=
    "old_strata" &
    names(output_df) !=
      "new_strata"]
  column_names_other <- enquo(column_names_other)
  output_df <- dplyr::select(
    output_df, new_strata,
    old_strata, !!column_names_other
  )
  if (is.numeric(output_df$new_strata) == FALSE) {
    output_df$new_strata <- as.character(output_df$new_strata)
  }
  if (is.null(sort_by) == FALSE) {
    output_df <- dplyr::arrange(output_df, !!sym(sort_by))
  }
  return(output_df)
}
