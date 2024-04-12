#' Adaptive Multi-Wave Sampling
#'
#' Determines the adaptive optimum sampling allocation for a new sampling
#' wave based on results from previous waves. Using Neyman or
#' Wright (2014) allocation, \code{allocate_wave} calculates the
#' optimum allocation for the \emph{total} number of samples
#' across waves, determines how many were allocated to each strata
#' in previous waves, and allocates the remaining samples to make
#' up the difference.
#'
#' If the optimum sample size in a stratum is smaller than the
#' amount it was allocated in previous waves, that strata has been
#' \emph{oversampled}. When oversampling occurs,
#' \code{allocate_wave} "closes" the oversampled strata and
#' re-allocates the remaining samples optimally among the open
#' strata. Under these circumstances, the total sampling
#' allocation is no longer optimal, but \code{optimall} will
#' output the \emph{most} optimal allocation possible for the next wave.
#' @param data A data frame or matrix with one row for each
#' sampling unit, one column specifying each unit's stratum,
#' one column holding the value of the continuous variable for
#' which the variance should be minimized, and one column
#' containing a binary indicator, \code{already_sampled},
#' specifying whether each unit has already been sampled.
#' @param strata A character string or vector of character strings
#' specifying the name of columns that indicate the stratum that
#' each unit belongs to.
#' @param y A character string specifying the name of the
#' continuous variable for which the variance should be minimized.
#' @param already_sampled A character string specifying the name of a
#' column that contains a binary (\code{Y}/\code{N} or \code{1}
#' /\code{0}) indicator specifying whether each unit has already
#' been sampled in a previous wave.
#' @param nsample The desired sample size of the next wave.
#' @param allocation_method A character string specifying the method of
#' optimum sample allocation to use. For details see
#' \code{optimum_allocation()}. Defaults to \code{WrightII} which is more exact
#' than \code{Neyman} but may run slower.
#' @param method A character string specifying the method to be
#' used if at least one group was oversampled. Must be one of:
#' \itemize{
#' \item \code{"iterative"}, the default, will require a longer
#' runtime but may be a more precise method of handling oversampled
#' strata. If there are multiple oversampled strata, this method
#' closes strata and re-calculates optimum allocation one by one.
#' \item \code{"simple"} closes all oversampled together and
#' re-calculates optimum allocation on the rest of the strata only
#' once. In certain cases where many strata have been oversampled
#' in prior waves, it is possible that this method will output a
#' negative value in n_to_sample. When this occurs, the function
#' will print a warning, and it is recommended that the user
#' re-runs the allocation with the 'iterative' method.
#' }
#' @param detailed A logical value indicating whether the output
#' dataframe should include details about each stratum including
#' the true optimum allocation without the constraint of
#' previous waves of sampling
#' and stratum standard deviations. Defaults to FALSE, unless called within
#' \code{apply_multiwave()}.
#' These details are all available from
#' \code{optimum_allocation()}.
#' @examples
#' # Create dataframe with a column specifying strata, a variable of interest
#' # and an indicator for whether each unit was already sampled
#' set.seed(234)
#' mydata <- data.frame(Strata = c(rep(1, times = 20),
#'                                 rep(2, times = 20),
#'                                 rep(3, times = 20)),
#'                      Var = c(rnorm(20, 1, 0.5),
#'                              rnorm(20, 1, 0.9),
#'                              rnorm(20, 1.5, 0.9)),
#'                      AlreadySampled = rep(c(rep(1, times = 5),
#'                                             rep(0, times = 15)),
#'                                           times = 3))
#'
#' x <- allocate_wave(
#'   data = mydata, strata = "Strata",
#'   y = "Var", already_sampled = "AlreadySampled",
#'   nsample = 20, method = "simple"
#' )
#' @export
#' @references McIsaac MA, Cook RJ. Adaptive sampling in two-phase designs:
#' a biomarker study for progression in arthritis. Statistics in medicine.
#' 2015 Sep 20;34(21):2899-912.
#' @references Reilly, M., & Pepe, M. S. (1995). A mean score method for
#' missing and auxiliary covariate data in regression models.
#' Biometrika, 82(2), 299-314.
#' @references  Wright, T. (2014). A Simple Method of Exact Optimal
#' Sample Allocation under Stratification with any Mixed
#' Constraint Patterns, Research Report Series (Statistics #2014-07),
#' Center for Statistical Research and Methodology, U.S. Bureau
#' of the Census, Washington, D.C.
#' @return Returns a dataframe with one row for each stratum and
#' columns specifying the stratum name ("strata"), population stratum size
#' (\code{"npop"}), cumulative sample in that strata
#' (\code{"nsample_actual"}), prior number sampled in that
#' strata (\code{"nsample_prior"}), and the optimally allocated
#' number of units in each strata for the next wave (\code{"n_to_sample"}).
#' @importFrom rlang enquo
#' @importFrom rlang sym
#' @importFrom magrittr %>%

allocate_wave <- function(data,
                          strata,
                          y, already_sampled,
                          nsample,
                          allocation_method = c("WrightII", "WrightI",
                                                "Neyman"),
                          method = c("iterative","simple"),
                          detailed = FALSE) {
  key <- stratum_size <- wave1_size <- npop <- difference <-
    nsample_prior <- n_to_sample <- nsample_actual <-
    nsample_optimal <- sd <- NULL # bind global vars as necessary
  if (is.matrix(data)) {
    data <- data.frame(data)
  }
  if (is.data.frame(data) == FALSE) {
    stop("Input data must be a dataframe or matrix with named columns.")
  }
  if (all(strata %in% names(data)) == FALSE) {
    stop("'strata' must be a character string or vector of
    strings matching column names of data.")
  }
  if (y %in% names(data) == FALSE) {
    stop("'y' must be a character string matching a column name of data.")
  }
  if (already_sampled %in% names(data) == FALSE) {
    stop("'already_sampled' must be a character string matching a column name of
           data.")
  }
  if (inherits(detailed, "logical") == FALSE) {
    stop("'detailed' must be a logical value.")
  }
  if (length(table(data[, already_sampled])) != 2) {
    stop("'already_sampled' must be a character string matching a column in
         'data' that has a binary indicator for whether each unit
         was already sampled. If no units have been sampled yet,
         use 'optimum_allocation'.")
  }
  if (("Y" %in% data[, already_sampled] == FALSE & 1 %in%
    data[, already_sampled] == FALSE) | anyNA(data[, already_sampled])) {
    stop("'already_sampled' column must contain '1' (numeric) or 'Y'
         (character) as indicators that a unit was sampled in a
         previous wave and cannot contain NAs. If no units have
         been sample, use 'optimum_allocation.")
  }
  if (nsample + sum(data[, already_sampled] == "Y") +
    sum(data[, already_sampled] == 1) > length(data[, y])) {
    stop("Total sample size across waves, taken as nsampled in
         already_sampled + nsample, is larger than the population size.")
  }
  allocation_method <- match.arg(allocation_method)
  method <- match.arg(method)
  # Find the total sample size and optimally allocate that
  nsampled <- sum(data[, already_sampled] == "Y" | data[, already_sampled] == 1)
  output1 <- optimall::optimum_allocation(
    data = data,
    strata = strata,
    y = y,
    nsample = nsample + nsampled,
    method = allocation_method,
    allow.na = TRUE
  )
  # Optimal for total sample size

  # Create groups from strata argument and determine the prior
  # sample size for each
  y <- enquo(y)
  strata <- enquo(strata)
  key_q <- enquo(already_sampled)
  wave1_df <- data %>%
    dplyr::select(!!strata, !!y, !!key_q)
  group <- interaction(dplyr::select(wave1_df, !!strata))
  wave1_df <- cbind(group, wave1_df)
  wave1_df <- dplyr::select(wave1_df, 1, !!y, !!key_q)
  # Only columns of interest
  names(wave1_df) <- c("group", "y", "key")
  wave1_summary <- wave1_df %>%
    dplyr::group_by(group) %>%
    dplyr::summarize(wave1_size = sum(key == 1 | key == "Y"))

  names(output1)[1] <- "group"
  comp_df <- dplyr::inner_join(output1, wave1_summary, by = "group")
  comp_df <- dplyr::mutate(comp_df,
    difference = stratum_size - wave1_size,
    n_avail = npop - wave1_size
  )

  # For the simple case in which no strata have been oversampled
  if (all(comp_df$difference >= 0)) {
    comp_df <- comp_df %>%
      dplyr::rename(
        nsample_optimal = stratum_size,
        nsample_prior = wave1_size,
        n_to_sample = difference
      ) %>%
      dplyr::mutate(nsample_actual = nsample_prior + n_to_sample)
    if (detailed == FALSE) {
      comp_df <- comp_df %>%
        dplyr::select(
          "strata" = group, npop, nsample_actual,
          nsample_prior, n_to_sample
        )
    } else if (detailed == TRUE) {
      comp_df <- comp_df %>%
        dplyr::select(
          "strata" = group, npop, nsample_optimal,
          nsample_actual, nsample_prior,
          n_to_sample, sd
        )
    }
    return(comp_df)
  }

  # If some Strata have been oversampled. Basic, non-iterative method.
  if (any(comp_df$difference < 0) & method == "simple") {
    temp <- dplyr::filter(comp_df, difference <= 0)
    n_oversampled <- -sum(temp$difference)
    closed_groups <- (temp$group)
    nsampled_in_closed_groups <- sum(temp$wave1_size)

    open_groups <- dplyr::filter(comp_df, difference > 0)$group
    open_df <- wave1_df %>%
      dplyr::filter(group %in% open_groups)
    open_output <- optimall::optimum_allocation(
      data = open_df,
      strata = "group",
      y = "y",
      nsample = nsample + nsampled - nsampled_in_closed_groups,
      method = allocation_method,
      allow.na = TRUE
    )
    names(open_output)[1] <- "group"
    open_output <- dplyr::inner_join(open_output, wave1_summary, by = "group")
    open_output <- dplyr::mutate(
      open_output,
      difference = stratum_size - wave1_size,
      n_avail = npop - wave1_size
    )

    open_output <- open_output %>%
      dplyr::rename(
        nsample_optimal = stratum_size,
        nsample_prior = wave1_size
      ) %>%
      dplyr::mutate(
        n_to_sample = difference,
        nsample_actual = nsample_prior + n_to_sample
      ) %>%
      dplyr::select(
        "strata" = group,
        npop,
        nsample_actual,
        nsample_prior,
        n_to_sample
      )

    closed_output <- temp %>%
      dplyr::rename(
        nsample_optimal = stratum_size,
        nsample_prior = wave1_size
      ) %>%
      dplyr::mutate(
        n_to_sample = 0,
        nsample_actual = nsample_prior
      ) %>%
      dplyr::select(
        "strata" = group,
        npop,
        nsample_actual,
        nsample_prior,
        n_to_sample
      )

    output_df <- rbind(closed_output, open_output)
    if (detailed == TRUE) {
      output_df <- dplyr::inner_join(
        output_df,
        dplyr::select(output1,
          "nsample_optimal" = stratum_size,
          sd,
          "strata" = group
        ),
        by = "strata"
      )
      output_df <- dplyr::select(
        output_df, strata, npop, nsample_optimal,
        nsample_actual, nsample_prior, n_to_sample, sd
      )
    }
    output_df <- dplyr::arrange(output_df, strata)
    if (any(output_df$n_to_sample < 0)) {
      warning("The simple method yielded strata with negative
              n_to_sample values due to many groups being
              oversampled in prior waves. Switching to
              method = 'iterative'.")
      did_simple_work <- FALSE
      method <- "iterative"
      rm(output_df, closed_output, open_output, closed_groups, open_groups)
    } else {
      return(output_df)
    }
  }
  # Now, iterative method

  if (any(comp_df$difference < 0) & method == "iterative") {
    closed_groups_df <- data.frame()

    while (any(comp_df$difference < 0)) {
      # Find most oversampled group. Add that group to the closed strata.
      closed_groups_df <- rbind(
        closed_groups_df,
        dplyr::filter(
          comp_df,
          difference ==
            min(difference)
        )
      )
      nsampled_in_closed_groups <- sum(closed_groups_df$wave1_size)
      closed_groups <- (closed_groups_df$group)

      # Filter comp_df, remove the smallest group
      open_groups_names <- dplyr::filter(
        comp_df,
        difference !=
          min(difference)
      )$group
      open_df <- wave1_df %>%
        dplyr::filter(group %in% open_groups_names)

      # Run optimal allocation on this filtered df of open groups
      outputn <- optimall::optimum_allocation(
        data = open_df, strata = "group", y = "y",
        nsample = nsample + nsampled - nsampled_in_closed_groups,
        method = allocation_method,
        allow.na = TRUE
      )

      # Re-join with (cleaned) input data to  get new differences
      names(outputn)[1] <- "group"
      comp_df <- dplyr::inner_join(outputn, wave1_summary, by = "group")
      comp_df <- dplyr::mutate(comp_df,
        difference = stratum_size - wave1_size,
        n_avail = npop - wave1_size
      )
    }
    open_output <- comp_df %>%
      dplyr::rename(
        nsample_optimal = stratum_size,
        nsample_prior = wave1_size
      ) %>%
      dplyr::mutate(
        n_to_sample = difference,
        nsample_actual = nsample_prior + n_to_sample
      ) %>%
      dplyr::select(
        "strata" = group, npop, nsample_actual, nsample_prior,
        n_to_sample
      )

    closed_output <- closed_groups_df %>%
      dplyr::rename(
        nsample_optimal = stratum_size,
        nsample_prior = wave1_size
      ) %>%
      dplyr::mutate(
        n_to_sample = 0,
        nsample_actual = nsample_prior
      ) %>%
      dplyr::select(
        "strata" = group, npop, nsample_actual, nsample_prior,
        n_to_sample
      )
    output_df <- rbind(closed_output, open_output)
    if (detailed == TRUE) {
      output_df <- dplyr::inner_join(
        output_df,
        dplyr::select(output1,
          "nsample_optimal" = stratum_size,
          sd,
          "strata" = group
        ),
        by = "strata"
      )
      output_df <- dplyr::select(
        output_df, strata, npop, nsample_optimal,
        nsample_actual, nsample_prior, n_to_sample, sd
      )
    }
    output_df <- dplyr::arrange(output_df, strata)
    return(output_df)
  }
}
