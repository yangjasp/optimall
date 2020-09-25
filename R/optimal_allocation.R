#' Optimal Allocation
#'
#' Determines the optimal sampling fraction and sample size for each stratum in a stratified random sample according to Neyman Allocation or Exact Optimum Sample Allocation (Wright 2014).
#' @param data A data frame or matrix with one row for each sampled unit, one column specifying each unit's stratum, and one column holding the value of the continuous variable for which the variance should be minimized.
#' @param strata a character string or vector of character strings specifying the name of columns which indicate the stratum that each unit belongs to.
#' @param y a character string specifying the name of the continuous variable for which the variance should be minimized.
#' @param nsample the desired total sample size. Defaults to NULL
#' @param method a character string specifying the method of optimal sample allocation to use. Must be one of "Neyman", "WrightI" or "WrightII". Defaults to "WrightII".
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
  if (all(strata %in% names(data)) == FALSE) {
    stop("'Strata' must be a character string matching a column name of data.")
  }
  if (y %in% names(data) == FALSE) {
    stop("'y' must be a character string matching a column name of data.")
  }
  if (any(grepl(method,c("WrightI","WrightII","Neyman"))) == FALSE)
    stop("'Method' must be a character string that matches or partially matches one of 'WrightI','WrightII', or 'Neyman'")
  method <- match.arg(method, c("WrightI","WrightII","Neyman"))
  y <- enquo(y)
  strata <- enquo(strata)
  output_df <- data %>%
    dplyr::select(!!strata, !!y)
  group <- interaction(dplyr::select(output_df,-!!y))
  output_df <- cbind(group,output_df)
  output_df <- output_df[,c(1,ncol(output_df))] #Only columns of interest
  names(output_df) <- c("group","y")
  if (sum(is.na(output_df)) >= 1) {
    stop("Data contains NAs")
  }
  if (method == "Neyman"){
    output_df <- output_df %>%
      dplyr::group_by(group) %>%
      dplyr::summarize(n = n(),
                       sd = sd(y),
                       n_sd = sd(y) * n()) %>%
      dplyr::mutate(stratum_fraction = round(n_sd / sum(n_sd),
                                                    digits = ndigits),
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
    else if (method == "WrightI"){
      if (is.null(nsample)){
        stop("This method requires a fixed nsample. Try method = 'Neyman' for exact sampling fractions.")
      }
      else {
        n_strata <- length(unique(group))
        n_minus_H <- nsample - n_strata
        if (n_minus_H <= 0){
          stop("nsample is too small for this method.")
        }
        output_df <- output_df %>%
          dplyr::group_by(group) %>%
          dplyr::summarize(n = n(),
                           sd = sd(y),
                           n_sd = sd(y) * n())
        priority_array <- list()
        for (i in 1:n_strata){
          priority_array[[i]] <- rep(output_df[i,"n_sd"],times = n_minus_H)
          names(priority_array)[[i]] <- paste0("n_sd",as.character(i))
        }
        suppressMessages(Wright_output <- bind_rows(priority_array))
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
        final_output <- cbind(output_df[,c("group","n","sd","n_sd")], stratum_size)
        final_output <- final_output %>%
          dplyr::mutate(stratum_fraction = round(stratum_size / nsample,
                                                 digits = ndigits),
                        sd = round(sd, digits = 2),
                        n_sd = round(n_sd, digits = 2))
        final_output <- final_output[c("group","n","sd","n_sd","stratum_fraction","stratum_size")]
        return(final_output)

      }
    }
    else if (method == "WrightII"){
      if (is.null(nsample)){
        stop("This method requires a fixed nsample. Try method = 'Neyman' for exact sampling fractions.")
      }
      else {
        n_strata <- length(unique(group))
        n_minus_2H <- nsample - 2*n_strata
        if (n_minus_2H <= 0){
          stop("nsample is too small for this method.")
        }
        output_df <- output_df %>%
          dplyr::group_by(group) %>%
          dplyr::summarize(n = n(),
                           sd = sd(y),
                           n_sd = sd(y) * n())
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
        final_output <- cbind(output_df[,c("group","n","sd","n_sd")], stratum_size)
        final_output <- final_output %>%
          dplyr::mutate(stratum_fraction = round(stratum_size / nsample,
                                                        digits = ndigits),
                        sd = round(sd, digits = 2),
                        n_sd = round(n_sd, digits = 2))
        final_output <- final_output[c("group","n","sd","n_sd","stratum_fraction","stratum_size")]
        return(final_output)

      }
    }
    else {
      stop("'Method' must be a character string that matches or partially matches one of 'WrightI','WrightII', or 'Neyman'")
    }
}
