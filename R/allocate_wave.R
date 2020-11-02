#' Adaptive Multi-Wave Sampling
#'
#' Determines the optimal sampling allocation for a new sampling wave based on results from previous waves.
#' @param data A data frame or matrix with one row for each sampling unit, one column specifying each unit's stratum, one column holding the value of the continuous variable for which the variance should be minimized, and one column containing a key specifying if each unit has already been sampled.
#' @param strata a character string or vector of character strings specifying the name of columns which indicate the stratum that each unit belongs to.
#' @param y a character string specifying the name of the continuous variable for which the variance should be minimized.
#' @param wave2a a character string specifying the name of a column that contains a binary (\code{Y}/\code{N} or \code{1}/\code{0}) indicator specifying whether each unit has already been sampled in a previous wave.
#' @param nsample The desired sample size of the next wave.
#' @param method a character string specifying the method to be used if at least one group was oversampled. Must be one of \code{"simple"} or \code{"iterative"}. Defaults to \code{"iterative"}, which requires a longer runtime but is a more precise method of handling oversampled strata.
#' @export
#' @return Returns a dataframe with one row for each stratum and columns specifying the stratum name, population stratum size (\code{"npop"}), cumulative sample in that strata (\code{"nsampled_total"}), prior number sampled in that strata (\code{"nsampled_prior"}), and the optimally allocated number of units in each strata for the next wave (\code{"n_to_sample"}).

allocate_wave <- function(data, strata, y, wave2a, nsample, method = "iterative"){
  if (is.matrix(data)) {
    data <- data.frame(data)
    }
  if (is.data.frame(data) == FALSE) {
    stop("Input data must be a dataframe or matrix with named columns.")
  }
  if (all(strata %in% names(data)) == FALSE) {
    stop("'Strata' must be a string or vector of strings matching column names of data.")
  }
  if (y %in% names(data) == FALSE) {
    stop("'y' must be a character string matching a column name of data.")
  }
  if (wave2a %in% names(data) == FALSE) {
    stop("'wave2a' must be a character string matching a column name of data.")
  }
  if (length(table(data[,wave2a])) != 2) {
    stop("'wave2a' must be a character string matching a column in 'data' that has a binary indicator for whether each unit was already sampled.")
  }
  if (("Y" %in% data[,wave2a] == FALSE & 1 %in% data[,wave2a] == FALSE) | any(is.na(data[,wave2a]))){
    stop("'wave2a' column must contain '1' (numeric) or 'Y' (string) as indicators that a unit was sampled in a previous wave and cannot contain NAs")
  }
  if(nsample + sum(data[,wave2a] == "Y") + sum(data[,wave2a] == 1) > length(data[,y])){
    stop("Total sample size across waves, taken as nsampled in wave2a + nsample, is larger than the population size.")
  }
  method <- match.arg(method, c("simple","iterative"))
 # Find the total sample size and optimally allocate that
  nsampled <- sum(data[,wave2a] == "Y" | data[,wave2a] == 1)
  output1 <- optimall::optimal_allocation(data = data, strata = strata, y = y, nsample = nsample + nsampled, allow.na = TRUE) #Optimal for total sample size

 #Create groups from strata argument and determine the prior sample size for each
  y <- enquo(y)
  strata <- enquo(strata)
  key_q <- enquo(wave2a)
  wave1_df <- data %>%
    dplyr::select(!!strata, !!y, !!key_q)
  group <- interaction(dplyr::select(wave1_df, !!strata))
  wave1_df <- cbind(group,wave1_df)
  wave1_df <- dplyr::select(wave1_df, 1, !!y, !!key_q ) #Only columns of interest
  names(wave1_df) <- c("group","y","key")
  wave1_summary <- wave1_df %>%
    dplyr::filter(key == 1 | key  == "Y") %>%
    dplyr::group_by(group) %>%
    dplyr::summarize(wave1_size = n())

  names(output1)[1] <- "group"
  comp_df <- dplyr::inner_join(output1, wave1_summary, by = "group")
  comp_df <- dplyr::mutate(comp_df, difference =  stratum_size - wave1_size,
                    n_avail = nsample - wave1_size)

 #For the simple case in which no strata have been oversampled
  if(all(comp_df$difference >= 0) & all(comp_df$n_avail > comp_df$difference)){
    comp_df <- comp_df %>%
      dplyr::rename(n_optimal = stratum_size,
             nsample_prior = wave1_size,
             n_to_sample = difference) %>%
      dplyr::mutate(nsample_total = nsample_prior + n_to_sample) %>%
      dplyr::select(group, npop, nsample_total, nsample_prior, n_to_sample)
    return(comp_df)
  }

  #If some Strata have been oversampled. Basic, non-iterative method.
  if(any(comp_df$difference <0) & method == "simple"){
    temp <- dplyr::filter(comp_df, difference <= 0)
    n_oversampled <- -sum(temp$difference)
    closed_groups <- (temp$group)
    nsampled_in_closed_groups <- sum(temp$wave1_size)

    open_groups <- dplyr::filter(comp_df, difference > 0)$group
    open_df <- wave1_df %>%
      filter(group %in% open_groups)
    open_output <- optimall::optimal_allocation(data = open_df, strata = "group",y = "y", nsample = nsample + nsampled - nsampled_in_closed_groups, allow.na = T)
    names(open_output)[1] <- "group"
    open_output <- dplyr::inner_join(open_output, wave1_summary, by = "group")
    open_output <- dplyr::mutate(open_output, difference =  stratum_size - wave1_size, n_avail = nsample - wave1_size)

    open_output <- open_output %>%
      dplyr::rename(n_optimal = stratum_size,
                    nsample_prior = wave1_size) %>%
      dplyr::mutate(n_to_sample = difference,
                    nsample_total = nsample_prior + n_to_sample) %>%
      dplyr::select(group, npop, nsample_total, nsample_prior, n_to_sample)

    closed_output <- temp %>%
      dplyr::rename(n_optimal = stratum_size,
                    nsample_prior = wave1_size) %>%
      dplyr::mutate(n_to_sample = 0,
                    nsample_total = nsample_prior) %>%
      dplyr::select(group, npop, nsample_total, nsample_prior, n_to_sample)

    output_df <- rbind(closed_output, open_output)
    return(output_df)
  }
  #Now, iterative method

  if(any(comp_df$difference <0) & method == "iterative"){
    closed_groups_df <- data.frame()

    while (any(comp_df$difference < 0)){
    #Find most oversampled group. Add that group to the closed strata.
    closed_groups_df <- rbind(closed_groups_df, dplyr::filter(comp_df, difference == min(difference)))
    nsampled_in_closed_groups <- sum(closed_groups_df$wave1_size)
    closed_groups <- (closed_groups_df$group)

    #Filter comp_df, remove the smallest group
    open_groups_names <- dplyr::filter(comp_df, difference != min(difference))$group
    open_df <- wave1_df %>%
      dplyr::filter(group %in% open_groups_names)

    #Run optimal allocation on this filtered df of open groups
    outputn <- optimall::optimal_allocation(data = open_df, strata = "group",y = "y", nsample = nsample + nsampled - nsampled_in_closed_groups, allow.na = T)

    #Re-join with (cleaned) input data to  get new differences
    names(outputn)[1] <- "group"
    comp_df <- dplyr::inner_join(outputn, wave1_summary, by = "group")
    comp_df <- dplyr::mutate(comp_df, difference =  stratum_size - wave1_size,
                             n_avail = nsample - wave1_size)

    }
    open_output <- comp_df %>%
      dplyr::rename(n_optimal = stratum_size,
                    nsample_prior = wave1_size) %>%
      dplyr::mutate(n_to_sample = difference,
                    nsample_total = nsample_prior + n_to_sample) %>%
      dplyr::select(group, npop, nsample_total, nsample_prior, n_to_sample)

    closed_output <- closed_groups_df %>%
      dplyr::rename(n_optimal = stratum_size,
                    nsample_prior = wave1_size) %>%
      dplyr::mutate(n_to_sample = 0,
                    nsample_total = nsample_prior) %>%
      dplyr::select(group, npop, nsample_total, nsample_prior, n_to_sample)
    output_df <- rbind(closed_output, open_output)
    return(output_df)
  }
  else {
    stop("'Method' must be a character string that matches or partially matches one of 'simple' or 'iterative'.")
  }
}
