#' Optimum Allocation
#'
#' Determines the optimum sampling fraction and sample size for each stratum in a stratified random sample according to Neyman Allocation or Exact Optimum Sample Allocation (Wright 2014).
#' @param data A data frame or matrix with one row for each sampled unit, one column specifying each unit's stratum, and one column holding the value of the continuous variable for which the variance should be minimized.
#' @param strata a character string or vector of character strings specifying the name(s) of columns which specify the stratum that each unit belongs to. If multiple column names are provided, each unique combination of values in these columns is taken to define one stratum.
#' @param y a character string specifying the name of the continuous variable for which the variance should be minimized.
#' @param nsample the desired total sample size. Defaults to NULL.
#' @param method a character string specifying the method of optimum sample allocation to use. Must be one of \code{"Neyman"}, \code{"WrightI"} or \code{"WrightII"}. Defaults to \code{"WrightII"}.
#' @param ndigits a numeric value specifying the number of digits to round the stratum fraction to.
#' @param allow.na logical input specifying whether y should be allowed to have NA values. Defaults to \code{FALSE}.
#' @examples
#' optimum_allocation(data = iris, strata = "Species", y = "Sepal.Length",
#' nsample = 100, method = "WrightII")
#' @export
#' @references Wright, T. (2014). A simple method of exact optimal sample allocation under stratification with any mixed constraint patterns. Statistics, 07.
#' @return Returns a data frame with the n allocated to each strata or the sampling fractions if nsample is NULL.

optimum_allocation <- function(data, strata, y, nsample = NULL,
                               ndigits = 2, method = "WrightII", allow.na = FALSE) {
  if (is.matrix(data) | tibble::is_tibble(data)) {
    data <- data.frame(data)
  }
  if (is.data.frame(data) == FALSE) {
    stop("Input data must be a dataframe or matrix with named columns.")
  }
  if (all(strata %in% names(data)) == FALSE) {
    stop("'Strata' must be a string or vector of strings matching column names of data.")
  }
  if (any(is.na(data[,strata]))) {
    stop("Columns specifying strata contain NAs")
  }
  if (y %in% names(data) == FALSE) {
    stop("'y' must be a character string matching a column name of data.")
  }
  if (is.numeric(data[,y]) == FALSE) {
    stop("'y' must be numeric.")
  }
  method <- match.arg(method, c("WrightI","WrightII","Neyman"))
  y <- enquo(y)
  strata <- enquo(strata)
  output_df <- data %>%
    dplyr::select(!!strata, !!y)
  strata <- interaction(dplyr::select(output_df,-!!y))
  output_df <- cbind(strata,output_df)
  output_df <- output_df[,c(1,ncol(output_df))] #Only columns of interest
  names(output_df) <- c("strata","y")
  if (min(dplyr::count(output_df,strata)[,"n"]) < 2){
    stop("Function requires at least two observations per stratum")
  }
  if (allow.na == FALSE & sum(is.na(output_df)) >= 1) {
    stop("Data contains NAs. If this is intentional, set allow.na to TRUE.")
  }
  if (method == "Neyman"){
    output_df <- output_df %>%
      dplyr::group_by(strata) %>%
      dplyr::summarize(n = n(),
                       sd = sd(y, na.rm = T),
                       n_sd = sd(y, na.rm = T) * n()) %>%
      dplyr::mutate(stratum_fraction = round(n_sd / sum(n_sd),
                                                    digits = ndigits),
                    sd = round(sd, digits = 2),
                    n_sd = round(n_sd, digits = 2))
    names(output_df)[names(output_df) == "n"] <- "npop"
      if (is.null(nsample)) {
        return(as.data.frame(output_df))
      }
      else {
        output_df <- output_df %>%
          dplyr::mutate(stratum_size = round(nsample * n_sd / sum(n_sd),
                                             digits = 0),
                        sd = round(sd, digits = 2),
                        n_sd = round(n_sd, digits = 2))
        return(as.data.frame(output_df))
      }
    }
    else if (method == "WrightI"){
      if (is.null(nsample)){
        stop("This method requires a fixed nsample. Try method = 'Neyman' for exact sampling fractions.")
      }
      else {
        n_strata <- length(unique(strata))
        n_minus_H <- nsample - n_strata
        if (n_minus_H <= 0){
          stop("'nsample' is too small for this method.")
        }
        output_df <- output_df %>%
          dplyr::group_by(strata) %>%
          dplyr::summarize(n = n(),
                           sd = sd(y, na.rm = T),
                           n_sd = sd(y, na.rm = T) * n())
        if (nsample > sum(output_df$n)){
          stop("'nsample' is larger than population size")
        }
        priority_array <- list()
        for (i in 1:n_strata){
          priority_array[[i]] <- c(rep(output_df[i,"n_sd"],
                                       times = min(output_df[i,"n"] - 1,n_minus_H)),
                                   rep(NULL,
                                       times = ifelse(n_minus_H > (output_df[i,"n"] - 1),
                                                      n_minus_H - (output_df[i,"n"] - 1),
                                                      0)))
          #All rows are same length, but zeroes so that n_sample won't be larger than n_strata
          names(priority_array)[[i]] <- paste0("n_sd",as.character(i))
        }
        suppressMessages(Wright_output <- bind_rows(priority_array))
        Wright_output[is.na(Wright_output)] <- 0
        mult_vec <- vector()
        for (i in 1:(n_minus_H+1)){
          mult_vec[i] <- 1/(sqrt(i*(i+1)))
        }
        for (i in 1:ncol(Wright_output)){
          Wright_output[,i] <- Wright_output[,i]*mult_vec[i]
        }
        cutoff <- (sort(unlist(Wright_output, use.names = FALSE),
                        decreasing = T))[n_minus_H]
        stratum_size <- rowSums(Wright_output >= cutoff) + 1
        final_output <- cbind(output_df[,c("strata","n","sd","n_sd")], stratum_size)
        final_output <- final_output %>%
          dplyr::mutate(stratum_fraction = round(stratum_size / nsample,
                                                 digits = ndigits),
                        sd = round(sd, digits = 2),
                        n_sd = round(n_sd, digits = 2))
        final_output <- final_output[c("strata","n","sd","n_sd","stratum_fraction","stratum_size")]
        names(final_output)[names(final_output) == "n"] <- "npop"
        return(final_output)

      }
    }
    else if (method == "WrightII"){
      if (is.null(nsample)){
        stop("This method requires a fixed nsample. Try method = 'Neyman' for exact sampling fractions.")
      }
      else {
        n_strata <- length(unique(strata))
        n_minus_2H <- nsample - 2*n_strata
        if (n_minus_2H <= 0){
          stop("'nsample' is too small for this method.")
        }
        output_df <- output_df %>%
          dplyr::group_by(strata) %>%
          dplyr::summarize(n = n(),
                           sd = sd(y, na.rm = T),
                           n_sd = sd(y, na.rm = T) * n())
        if (nsample > sum(output_df$n)){
          stop("'nsample' is larger than population size")
        }
        priority_array <- list()
        for (i in 1:n_strata){
          priority_array[[i]] <- c(rep(output_df[i,"n_sd"],
                                       times = min(output_df[i,"n"] - 2,n_minus_2H)),
                                   rep(NULL,
                                       times = ifelse(n_minus_2H > (output_df[i,"n"] - 2),
                                                      n_minus_2H - (output_df[i,"n"] - 2),
                                                      0)))
          #All rows are same length, but zeroes so that n_sample won't be larger than n_strata
          names(priority_array)[[i]] <- paste0("n_sd",as.character(i))
        }
        suppressMessages(Wright_output <- bind_rows(priority_array))
        Wright_output[is.na(Wright_output)] <- 0
        mult_vec <- vector()
        for (i in 2:(n_minus_2H+1)){
          mult_vec[i-1] <- 1/(sqrt(i*(i+1)))
        }
        for (i in 1:ncol(Wright_output)){
          Wright_output[,i] <- Wright_output[,i]*mult_vec[i]
        }
        cutoff <- (sort(unlist(Wright_output, use.names = FALSE),
                        decreasing = T))[n_minus_2H]
        stratum_size <- rowSums(Wright_output >= cutoff) + 2
        final_output <- cbind(output_df[,c("strata","n","sd","n_sd")], stratum_size)
        final_output <- final_output %>%
          dplyr::mutate(stratum_fraction = round(stratum_size / nsample,
                                                        digits = ndigits),
                        sd = round(sd, digits = 2),
                        n_sd = round(n_sd, digits = 2))
        final_output <- final_output[c("strata","n","sd","n_sd","stratum_fraction","stratum_size")]
        names(final_output)[names(final_output) == "n"] <- "npop"
        return(final_output)

      }
    }
    else {
      stop("'Method' must be a character string that matches or partially matches one of 'WrightI','WrightII', or 'Neyman'.")
    }
}
