#' Optimal Allocation
#'
#' Determines the optimal sampling fraction and sample size for each stratum in a stratified random sample according to Neyman Allocation or Exact Optimum Sample Allocation (Wright 2014).
#' @param data A data frame or matrix with one row for each sampled unit, one column specifying each unit's stratum, and one column holding the value of the continuous variable for which the variance should be minimized.
#' @param strata a character string specifying the name of column indicating the stratum that each unit belongs to.
#' @param y a character string specifying the name of the continuous variable for which the variance should be minimized.
#' @param nsample the desired total sample size. Defaults to NULL
#' @param method a character string specifying the method of optimal sample allocation to use. Must be one of "Neyman" or "WrightII".
#' @param ndigits a numeric value specifying the number of digits to round the stratum fraction to.
#' @examples
#' optimal_allocation(data = iris, strata = "Species", y = "Sepal.Length",
#' nsample = 100, method = "WrightII")
#' @export
#' @references Wright, T. (2014). A simple method of exact optimal sample allocation under stratification with any mixed constraint patterns. Statistics, 07.
#' @return Returns a data frame with the n allocated to each strata or the sampling fractions if nsample is NULL.

optimal_allocation <- function(data, strata, y, nsample = NULL,
                               ndigits = 2, method = "WrightII") {
  if (is.matrix(data)) {
    data <- data.frame(data)
  }
  if (is.data.frame(data) == FALSE) {
    stop("Input data must be a dataframe or matrix with named columns.")
  }
  else if (strata %in% names(data) == FALSE) {
    stop("'Strata' must be a character string matching a column name of data.")
  }
  else if (y %in% names(data) == FALSE) {
    stop("'y' must be a character string matching a column name of data.")
  }
  else {
    output_df <- data %>%
      dplyr::select(!!sym(strata), !!sym(y))
    if (sum(is.na(output_df)) >= 1) {
      stop("Data contains NAs")
    }
    if (method == "Neyman"){
      output_df <- output_df %>%
        dplyr::group_by(!!sym(strata)) %>%
        dplyr::summarize(n = n(),
                         sd = sd(!!sym(y)),
                         n_sd = sd(!!sym(y)) * n()) %>%
        dplyr::mutate(stratum_fraction = round(n_sd / sum(n_sd), digits = ndigits),
                      sd = round(sd, digits = 2),
                      n_sd = round(n_sd, digits = 2))
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
    else if (method == "WrightII"){
      if (is.null(nsample)){
        stop("This method requires a fixed nsample. Try method = 'Neyman'.")
      }
      else {
        n_strata <- length(unique(data[,strata]))
        n_minus_2H <- nsample - 2*n_strata
        output_df <- output_df %>%
          dplyr::group_by(!!sym(strata)) %>%
          dplyr::summarize(n = n(),
                           sd = sd(!!sym(y)),
                           n_sd = sd(!!sym(y)) * n())
        #priority_array <- matrix(0,nrow = n_strata,ncol = n_minus_2H,
        #dimnames = list(unique()))
        priority_array <- list()
        for (i in 1:n_strata){
          priority_array[[i]] <- rep(output_df[i,"n_sd"],times = n_minus_2H)
          names(priority_array)[[i]] <- paste0("n_sd",as.character(i))
        }
        suppressMessages(Wright_output <- bind_rows(priority_array))
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
        final_output <- cbind(output_df[,c(strata,"n","sd","n_sd")], stratum_size)
        final_output <- final_output %>%
          dplyr::mutate(stratum_fraction = round(stratum_size / nsample,
                                                 digits = ndigits),
                        sd = round(sd, digits = 2),
                        n_sd = round(n_sd, digits = 2))
        final_output <- final_output[c(strata,"n","sd","n_sd","stratum_fraction","stratum_size")]
        return(final_output)

      }
    }
    else {
      stop("'method' must be one of 'Neyman' or 'WrightII'")
    }
  }
}
