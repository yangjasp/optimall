#' Adaptive Multi-Wave Sampling
#'
#' Determines the optimal sampling allocation for a new sampling wave based on results from previous waves.
#' @param data A data frame or matrix with one row for each sampled unit, one column specifying each unit's stratum, one column holding the value of the continuous variable for which the variance should be minimized, and one column containing a key specifying if that unit has already been sampled.
#' @param strata a character string or vector of character strings specifying the name of columns which indicate the stratum that each unit belongs to.
#' @param y a character string specifying the name of the continuous variable for which the variance should be minimized.
#' @param key a character string specifying the name of the column that contains the binary (Y/N or 1/0) indicator specifying if each unit has already been sampled.
#' @param n The desired sample size of the next wave.
#' @export
#' @return Returns a dataframe with the n allocated to each strata for the next sampling wave.

allocate_wave <- function(data, strata, y, key, n){
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
  if (key %in% names(data) == FALSE) {
    stop("'key' must be a character string matching a column name of data.")
  }
  if (length(table(data[,key])) != 2) {
    stop("'key' must be a character string matching a column in 'data' that has a binary indicator for whether each unit was already sampled.")
  }
  if (("Y" %in% data[,key] == FALSE & 1 %in% data[,key] == FALSE) | any(is.na(data[,key]))){
    stop("'key' column must contain '1' (numeric) or 'Y' (string) as indicators that a unit was sampled in a previous wave and cannot contain NAs")
  }
  nsampled <- sum(data[,key] == "Y" | data[,key] == 1)
  output1 <- optimall::optimal_allocation(data = data, strata = strata, y = y, nsample = n + nsampled, allow.na = TRUE) #Optimal for total sample size

  y <- enquo(y)
  strata <- enquo(strata)
  key_q <- enquo(key)
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
                    n_avail = n - wave1_size)

  if(all(comp_df$difference >= 0) & all(comp_df$n_avail > comp_df$difference)){
    comp_df <- comp_df %>%
      dplyr::rename(n_optimal = stratum_size,
             nsample_prior = wave1_size,
             nsample = difference) %>%
      dplyr::mutate(nsample_total = nsample_prior + nsample)
      dplyr::select(group, n, nsample_total, nsample_prior, nsample)
    return(comp_df)
  }
  if(any(comp_df$difference <0)){
    temp <- dplyr::filter(comp_df, difference <= 0)
    n_oversampled <- -sum(temp$difference)
    closed_groups <- (temp$group)
    nsampled_in_closed_groups <- sum(temp$wave1_size)

    open_groups <- dplyr::filter(comp_df, difference > 0)$group
    open_df <- wave1_df %>%
      filter(group %in% open_groups)
    open_output <- optimall::optimal_allocation(data = open_df, strata = "group",y = "y", nsample = n + nsampled - nsampled_in_closed_groups)
    names(open_output)[1] <- "group"
    open_output <- dplyr::inner_join(open_output, wave1_summary, by = "group")
    open_output <- dplyr::mutate(open_output, difference =  stratum_size - wave1_size, n_avail = n - wave1_size)

    open_output <- open_output %>%
      dplyr::rename(n_optimal = stratum_size,
                    nsample_prior = wave1_size) %>%
      dplyr::mutate(nsample = difference,
                    nsample_total = nsample_prior + nsample) %>%
      dplyr::select(group, n, nsample_total, nsample_prior, nsample)

    closed_output <- temp %>%
      dplyr::rename(n_optimal = stratum_size,
                    nsample_prior = wave1_size) %>%
      dplyr::mutate(nsample = 0,
                    nsample_total = nsample_prior) %>%
      dplyr::select(group, n, nsample_total, nsample_prior, nsample)

    output_df <- rbind(closed_output, open_output)
    return(output_df)
  }

}
