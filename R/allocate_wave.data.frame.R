#' allocate_wave data frame
#' @export
#' @importFrom rlang enquo
#' @importFrom rlang sym
#' @importFrom magrittr %>%
#' @include  allocate_wave.R

allocate_wave.data.frame <- function(data, strata, y, wave2a,
                                     nsample, method = "iterative",
                                     detailed = FALSE, phase, wave) {
  key <- stratum_size <- wave1_size <- npop <- difference <-
    nsample_prior <- n_to_sample <- nsample_total <-
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
  if (wave2a %in% names(data) == FALSE) {
    stop("'wave2a' must be a character string matching a column name of data.")
  }
  if (class(detailed) != "logical") {
    stop("'detailed' must be a logical value.")
  }
  if (length(table(data[, wave2a])) != 2) {
    stop("'wave2a' must be a character string matching a column in
         'data' that has a binary indicator for whether each unit
         was already sampled. If no units have been sampled yet,
         use 'optimum_allocation'.")
  }
  if (("Y" %in% data[, wave2a] == FALSE & 1 %in%
       data[, wave2a] == FALSE) | any(is.na(data[, wave2a]))) {
    stop("'wave2a' column must contain '1' (numeric) or 'Y'
         (character) as indicators that a unit was sampled in a
         previous wave and cannot contain NAs. If no units have
         been sample, use 'optimum_allocation.")
  }
  if (nsample + sum(data[, wave2a] == "Y") +
      sum(data[, wave2a] == 1) > length(data[, y])) {
    stop("Total sample size across waves, taken as nsampled in
         wave2a + nsample, is larger than the population size.")
  }
  method <- match.arg(method, c("simple", "iterative"))
  # Find the total sample size and optimally allocate that
  nsampled <- sum(data[, wave2a] == "Y" | data[, wave2a] == 1)
  output1 <- optimall::optimum_allocation(data = data,
                                          strata = strata,
                                          y = y,
                                          nsample = nsample + nsampled,
                                          allow.na = TRUE)
  # Optimal for total sample size

  # Create groups from strata argument and determine the prior
  # sample size for each
  y <- enquo(y)
  strata <- enquo(strata)
  key_q <- enquo(wave2a)
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
      dplyr::mutate(nsample_total = nsample_prior + n_to_sample)
    if (detailed == FALSE) {
      comp_df <- comp_df %>%
        dplyr::select("strata" = group, npop, nsample_total,
                      nsample_prior, n_to_sample)
    } else if (detailed == TRUE) {
      comp_df <- comp_df %>%
        dplyr::select("strata" = group, npop, nsample_optimal,
                      nsample_total, nsample_prior,
                      n_to_sample, sd)
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
      allow.na = TRUE)
    names(open_output)[1] <- "group"
    open_output <- dplyr::inner_join(open_output, wave1_summary, by = "group")
    open_output <- dplyr::mutate(
      open_output,
      difference = stratum_size - wave1_size,
      n_avail = npop - wave1_size)

    open_output <- open_output %>%
      dplyr::rename(
        nsample_optimal = stratum_size,
        nsample_prior = wave1_size
      ) %>%
      dplyr::mutate(
        n_to_sample = difference,
        nsample_total = nsample_prior + n_to_sample
      ) %>%
      dplyr::select(
        "strata" = group,
        npop,
        nsample_total,
        nsample_prior,
        n_to_sample)

    closed_output <- temp %>%
      dplyr::rename(
        nsample_optimal = stratum_size,
        nsample_prior = wave1_size
      ) %>%
      dplyr::mutate(
        n_to_sample = 0,
        nsample_total = nsample_prior
      ) %>%
      dplyr::select(
        "strata" = group,
        npop,
        nsample_total,
        nsample_prior,
        n_to_sample)

    output_df <- rbind(closed_output, open_output)
    if (detailed == TRUE) {
      output_df <- dplyr::inner_join(output_df,
                                     dplyr::select(output1,
                                                   "nsample_optimal" = stratum_size,
                                                   sd, "strata" = group
                                     ),
                                     by = "strata"
      )
      output_df <- dplyr::select(
        output_df, strata, npop, nsample_optimal,
        nsample_total, nsample_prior, n_to_sample, sd
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
      closed_groups_df <- rbind(closed_groups_df,
                                dplyr::filter(comp_df,
                                              difference ==
                                                min(difference)))
      nsampled_in_closed_groups <- sum(closed_groups_df$wave1_size)
      closed_groups <- (closed_groups_df$group)

      # Filter comp_df, remove the smallest group
      open_groups_names <- dplyr::filter(comp_df,
                                         difference !=
                                           min(difference))$group
      open_df <- wave1_df %>%
        dplyr::filter(group %in% open_groups_names)

      # Run optimal allocation on this filtered df of open groups
      outputn <- optimall::optimum_allocation(
        data = open_df, strata = "group", y = "y",
        nsample = nsample + nsampled - nsampled_in_closed_groups,
        allow.na = TRUE)

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
        nsample_total = nsample_prior + n_to_sample
      ) %>%
      dplyr::select(
        "strata" = group, npop, nsample_total, nsample_prior,
        n_to_sample)

    closed_output <- closed_groups_df %>%
      dplyr::rename(
        nsample_optimal = stratum_size,
        nsample_prior = wave1_size
      ) %>%
      dplyr::mutate(
        n_to_sample = 0,
        nsample_total = nsample_prior
      ) %>%
      dplyr::select(
        "strata" = group, npop, nsample_total, nsample_prior,
        n_to_sample)
    output_df <- rbind(closed_output, open_output)
    if (detailed == TRUE) {
      output_df <- dplyr::inner_join(output_df,
                                     dplyr::select(output1,
                                                   "nsample_optimal" = stratum_size,
                                                   sd, "strata" = group
                                     ),
                                     by = "strata"
      )
      output_df <- dplyr::select(
        output_df, strata, npop, nsample_optimal,
        nsample_total, nsample_prior, n_to_sample, sd
      )
    }
    output_df <- dplyr::arrange(output_df, strata)
    return(output_df)
  }
  else {
    stop("'Method' must be a character string that matches or
         partially matches one of 'simple' or 'iterative'.")
  }
}
