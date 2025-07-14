#' Optimum Allocation
#'
#' Determines the optimum sampling fraction and sample size for
#' each stratum in a stratified random sample, which
#' minimizes the variance of the sample mean according to Neyman
#' Allocation or Exact Optimum Sample Allocation (Wright 2014).
#' @param data A data frame or matrix with at least one column specifying
#' each unit's stratum, and either 1) a second column holding the value of the
#' continuous variable for which the sample mean variance should be minimized
#' (\code{y}) or 2) two columns: one holding the the within-stratum
#' standard deviation for the variable of interest (\code{sd_h}) and another
#' holding the stratum sample sizes (\code{N_h}).
#' If \code{data} contains a column \code{y} holding values for
#' the variable of interest, then \code{data} should have one row for
#' each sampled unit. If \code{data} holds \code{sd_h} and \code{N_h}, the
#' within-stratum standard deviations and population sizes, then \code{data}
#' should have one row per stratum.
#' Other columns are allowed but will be ignored.
#' @param strata a character string or vector of character strings
#' specifying the name(s) of columns which specify the stratum
#' that each unit belongs to. If multiple column names are
#' provided, each unique combination of values in these columns
#' is taken to define one stratum.
#' @param y a character string specifying the name of the
#' continuous variable for which the variance should be minimized.
#' Defaults to \code{NULL} and should be left as \code{NULL} when \code{data}
#' holds stratum standard deviations and sample sizes instead of individual
#' sampling units. If a character vector of length > 1 is supplied, then
#' function performs A-optimal allocation to minimize the sum of variances.
#' @param sd_h a character string specifying the name of the
#' column holding the within-stratum standard deviations for each stratum.
#' Defaults to \code{NULL} and should be left as \code{NULL} when \code{data}
#' holds individual sampling units. If a character vector of length > 1 is
#' supplied, the function performs A-optimal allocation to minimize the sum
#' of variances.
#' @param N_h a character string specifying the name of the
#' column holding the population stratum sizes for each stratum.
#' Defaults to \code{NULL} and should be left as \code{NULL} when \code{data}
#' holds individual sampling units.
#' @param nsample the desired total sample size. Defaults to \code{NULL}.
#' @param method a character string specifying the method of
#' optimum sample allocation to use. Must be one of:
#' \itemize{
#' \item \code{"WrightII"}, the default, uses Algorithm II from
#' Wright (2014) to determine the optimum allocation of a fixed
#' sample size across the strata. It requires that at least two
#' samples are allocated to each stratum.
#' \item \code{"WrightI"} uses Wright's Algorithm I to determine
#' the optimum sample allocation. It only requires that at least
#' one sample is allocated to each stratum, and can therefore
#' lead to a biased variance estimate.
#' \item \code{"Neyman"} uses the standard method of Neyman
#' Allocation to determine the optimum sample allocation. When
#' \code{nsample = NULL}, the optimal sampling fraction is calculated
#' and returned. When a numeric value is specified for \code{nsample},
#' then the number allocated to each stratum is the optimal sampling
#' fraction times \code{nsample} rounded to the nearest integer,
#' which may no longer be optimall.
#' }
#' @param weights A numeric vector of length matching the length of \code{y} or
#' \code{sd_h} that is only applicable if these lengths are > 1. In this case,
#' the values must sum to 1 and correspond to the weights of each variables
#' of interest in A-optimal allocation.
#' @param ndigits a numeric value specifying the number of digits
#' to which the standard deviation and stratum fraction should be rounded.
#' Defaults to 2.
#' @param allow.na logical input specifying whether y should
#' be allowed to have NA values. Defaults to \code{FALSE}.
#' @details If a character vector of length > 1 is supplied for \code{y} or
#' \code{sd_h}, then function performs A-optimal allocation to minimize the
#' sum of variances.
#' @examples
#' optimum_allocation(
#'   data = iris, strata = "Species", y = "Sepal.Width",
#'   nsample = 40, method = "WrightII"
#' )
#'
#' # Or if input data is summary of strata sd and N:
#' iris_summary <- data.frame(
#'   strata = unique(iris$Species),
#'   size = c(50, 50, 50),
#'   sd = c(0.3791, 0.3138, 0.3225)
#' )
#'
#' optimum_allocation(
#'   data = iris_summary, strata = "strata",
#'   sd_h = "sd", N_h = "size",
#'   nsample = 40, method = "WrightII"
#' )
#'
#' # A-optimal allocation to minimize the sum of variances if a vector is
#' # supplied for y or sd_h
#' optimum_allocation(
#'   data = iris, strata = "Species", y = c("Sepal.Width", "Sepal.Length"),
#'   weights = c(0.5,0.5),
#'   nsample = 40, method = "WrightII"
#' )
#'
#' iris_summary2 <- data.frame(
#'   strata = unique(iris$Species),
#'   size = c(50, 50, 50),
#'   sd1 = c(0.3791, 0.3138, 0.3225),
#'   sd2 = c(0.3525, 0.5162, 0.6359)
#' )
#'
#' optimum_allocation(
#'   data = iris_summary2, strata = "strata",
#'   sd_h = c("sd1", "sd2"), weights = c(0.5,0.5),
#'   N_h = "size",
#'   nsample = 40, method = "WrightII"
#' )
#' @export
#' @references Wright, T. (2014). A Simple Method of Exact Optimal
#' Sample Allocation under Stratification with any Mixed
#' Constraint Patterns, Research Report Series (Statistics #2014-07),
#' Center for Statistical Research and Methodology, U.S. Bureau
#' of the Census, Washington, D.C.
#' @return Returns a data frame with the number of samples allocated to each
#' stratum, or just the sampling fractions if nsample is NULL.
#' @importFrom magrittr %>%

optimum_allocation <- function(data, strata,
                               y = NULL,
                               sd_h = NULL,
                               N_h = NULL,
                               nsample = NULL,
                               ndigits = 2,
                               method = c("WrightII", "WrightI", "Neyman"),
                               weights = NULL,
                               allow.na = FALSE) {
  n_sd <- sd <- n <- npop <- stratum_fraction <- NULL
  # bind local vars as necessary
  if (is.matrix(data) | tibble::is_tibble(data)) {
    data <- data.frame(data)
  }
  if (is.data.frame(data) == FALSE) {
    stop("Input data must be a dataframe or matrix with named columns.")
  }
  if (all(strata %in% names(data)) == FALSE) {
    stop("'Strata' must be a string or vector of strings matching
         column names of data.")
  }
  if (anyNA(data[, strata])) {
    stop("Columns specifying strata contain NAs")
  }
  if ((is.null(y) & is.null(sd_h)) |
    (!is.null(y) & !is.null(sd_h))) {
    stop("One and only one of 'y' or 'sd_h' must be provided. If 'sd_h' is
    provided, then 'N_h' must be as well. Use ?optimum_allocation for help")
  }

  if(length(y) == 1 & !is.null(y) | length(sd_h) == 1){
  # Start if y is given and sd is NULL
  if (is.null(sd_h)) {
    if (!is.null(N_h)) {
      stop("If 'sd_h' is NULL, 'N_h' should also be NULL.")
    }

    if (y %in% names(data) == FALSE) {
      stop("'y' must be a character string matching a column name of data.")
    }
    if (is.numeric(data[, y]) == FALSE) {
      stop("'y' must be numeric.")
    }
    method <- match.arg(method)
    y <- enquo(y)
    strata <- enquo(strata)
    output_df <- data %>%
      dplyr::select(!!strata, !!y)
    strata <- interaction(dplyr::select(output_df, -!!y))
    output_df <- cbind(strata, output_df)
    output_df <- output_df[, c(1, ncol(output_df))] # Only columns of interest
    names(output_df) <- c("strata", "y")
    if (allow.na == FALSE & sum(is.na(output_df)) >= 1) {
      stop("Data contains NAs. If this is intentional, set allow.na to TRUE.")
    }
    if (min(dplyr::count(
      dplyr::filter(output_df, !(is.na(y))), strata
    )[, "n"]) < 2 |
      length(unique(dplyr::filter(output_df, !(is.na(y)))$strata)) !=
        length(unique(output_df$strata))) {
      stop("Function requires at least two observations per stratum")
    }
    if (method == "Neyman") {
      output_df <- output_df %>%
        dplyr::group_by(strata) %>%
        dplyr::summarize(
          n = dplyr::n(),
          sd = stats::sd(y, na.rm = TRUE),
          n_sd = stats::sd(y, na.rm = TRUE) * dplyr::n()
        )
      output_df <- (as.data.frame(output_df))
      names(output_df)[names(output_df) == "n"] <- "npop"
      if (is.null(nsample)) {
        output_df <- dplyr::arrange(output_df, strata) %>%
          dplyr::mutate(
            stratum_fraction = round(n_sd / sum(n_sd),
                                     digits = ndigits
            ),
            sd = round(sd, digits = ndigits),
            n_sd = round(n_sd, digits = ndigits)
          )
        return(output_df)
      }
      else {
        output_df <- output_df %>%
          dplyr::mutate(
            stratum_fraction = round(n_sd / sum(n_sd),
                                     digits = ndigits
            ),
            stratum_size = round(nsample * n_sd / sum(n_sd),
              digits = 0
            ),
            sd = round(sd, digits = ndigits),
            n_sd = round(n_sd, digits = ndigits)
          )
        output_df <- dplyr::arrange(output_df, strata)
        return(output_df)
      }
    }
    else if (method == "WrightI") {
      if (is.null(nsample)) {
        stop("This method requires a fixed nsample. Try method =
             'Neyman' for exact sampling fractions.")
      }
      else {
        n_strata <- length(unique(strata))
        n_minus_H <- nsample - n_strata
        if (n_minus_H <= 0) {
          stop("'nsample' is too small for this method.")
        }
        output_df <- output_df %>%
          dplyr::group_by(strata) %>%
          dplyr::summarize(
            n = dplyr::n(),
            sd = stats::sd(y, na.rm = TRUE),
            n_sd = stats::sd(y, na.rm = TRUE) * dplyr::n()
          )
        if (nsample > sum(output_df$n)) {
          stop("'nsample' is larger than population size")
        }
        priority_array <- list()
        for (i in 1:n_strata) {
          priority_array[[i]] <- c(
            rep(output_df[i, "n_sd"],
              times = min(output_df[i, "n"] - 1, n_minus_H)
            ),
            rep(NULL,
              times = ifelse(n_minus_H > (output_df[i, "n"] - 1),
                n_minus_H - (output_df[i, "n"] - 1),
                0
              )
            )
          )
          # All rows are same length, but zeroes so that n_sample
          # won't be larger than n_strata
          names(priority_array)[[i]] <- paste0("n_sd", as.character(i))
        }
        suppressMessages(Wright_output <- dplyr::bind_rows(priority_array))
        Wright_output[is.na(Wright_output)] <- 0
        mult_vec <- vector()
        for (i in 1:(n_minus_H + 1)) {
          mult_vec[i] <- 1 / (sqrt(i * (i + 1)))
        }
        for (i in seq_len(ncol(Wright_output))) {
          Wright_output[, i] <- Wright_output[, i] * mult_vec[i]
        }
        cutoff <- (sort(unlist(Wright_output, use.names = FALSE),
          decreasing = TRUE
        ))[n_minus_H]
        stratum_size <- rowSums(Wright_output >= cutoff) + 1
        final_output <- cbind(
          output_df[, c("strata", "n", "sd", "n_sd")], stratum_size
        )
        final_output <- final_output %>%
          dplyr::mutate(
            stratum_fraction = round(stratum_size / nsample,
              digits = ndigits
            ),
            sd = round(sd, digits = ndigits),
            n_sd = round(n_sd, digits = ndigits)
          )
        final_output <- final_output[c(
          "strata", "n", "sd", "n_sd", "stratum_fraction",
          "stratum_size"
        )]
        names(final_output)[names(final_output) == "n"] <- "npop"
        final_output <- dplyr::arrange(final_output, strata)
        return(final_output)
      }
    }
    else if (method == "WrightII") {
      if (is.null(nsample)) {
        stop("This method requires a fixed nsample. Try method =
            'Neyman' for exact sampling fractions.")
      }
      else {
        n_strata <- length(unique(strata))
        n_minus_2H <- nsample - 2 * n_strata
        if (n_minus_2H <= 0) {
          stop("'nsample' is too small for this method.")
        }
        output_df <- output_df %>%
          dplyr::group_by(strata) %>%
          dplyr::summarize(
            n = dplyr::n(),
            sd = stats::sd(y, na.rm = TRUE),
            n_sd = stats::sd(y, na.rm = TRUE) * dplyr::n()
          )
        if (nsample > sum(output_df$n)) {
          stop("'nsample' is larger than population size")
        }
        priority_array <- list()
        for (i in 1:n_strata) {
          priority_array[[i]] <- c(
            rep(output_df[i, "n_sd"],
              times = min(output_df[i, "n"] - 2, n_minus_2H)
            ),
            rep(output_df[i, "n_sd"] * 0,
              times = ifelse(n_minus_2H > (output_df[i, "n"] - 2),
                n_minus_2H - (output_df[i, "n"] - 2),
                0
              )
            )
          )
          # All rows are same length, but zeroes so that n_sample
          # won't be larger than n_strata. Zero instead of NULL so
          # entries in list aren't empty when n=2.
          names(priority_array)[[i]] <- paste0("n_sd", as.character(i))
        }
        suppressMessages(Wright_output <- dplyr::bind_rows(priority_array))
        Wright_output[is.na(Wright_output)] <- 0
        mult_vec <- vector()
        for (i in 2:(n_minus_2H + 1)) {
          mult_vec[i - 1] <- 1 / (sqrt(i * (i + 1)))
        }
        for (i in seq_len(ncol(Wright_output))) {
          Wright_output[, i] <- Wright_output[, i] * mult_vec[i]
        }
        cutoff <- (sort(unlist(Wright_output, use.names = FALSE),
          decreasing = TRUE
        ))[n_minus_2H]
        stratum_size <- rowSums(Wright_output >= cutoff) + 2
        final_output <- cbind(
          output_df[, c("strata", "n", "sd", "n_sd")], stratum_size
        )
        final_output <- final_output %>%
          dplyr::mutate(
            stratum_fraction = round(stratum_size / nsample,
              digits = ndigits
            ),
            sd = round(sd, digits = ndigits),
            n_sd = round(n_sd, digits = ndigits)
          )
        final_output <- final_output[
          c(
            "strata", "n", "sd", "n_sd", "stratum_fraction",
            "stratum_size"
          )
        ]
        names(final_output)[names(final_output) == "n"] <- "npop"
        final_output <- dplyr::arrange(final_output, strata)
        return(final_output)
      }
    }
    # End if y is given and sd/N is NULL
  }

  # Start if sd/N is given and y is NULL
  if (is.null(y)) {
    if (sd_h %in% names(data) == FALSE | N_h %in% names(data) == FALSE) {
      stop("'sd_h' and 'N_h' must be character strings
           matching column names of 'data'.")
    }
    if (is.numeric(data[, sd_h]) == FALSE |
      is.numeric(data[, N_h]) == FALSE) {
      stop("'sd_h' and 'N_h' must be numeric.")
    }
    method <- match.arg(method)
    sd_h <- enquo(sd_h)
    N_h <- enquo(N_h)
    strata <- enquo(strata)
    output_df <- data %>%
      dplyr::select(!!strata, !!N_h, !!sd_h)
    strata <- interaction(dplyr::select(output_df, -!!N_h, -!!sd_h))
    output_df <- cbind(strata, output_df)
    output_df <- output_df[, c(1, ncol(output_df) - 1, ncol(output_df))]
    # Only columns of interest
    names(output_df) <- c("strata", "N_h", "sd_h")
    if (any(table(output_df$strata) > 1)) {
      stop("If using 'sd_h' and 'N_h' instead of 'y',
      data must only contain one row per stratum.")
    }
    if (sum(is.na(output_df)) >= 1) {
      stop("Data cannot contain NAs.")
    }
    if (method == "Neyman") {
      output_df <- output_df %>%
        dplyr::mutate(
          n_sd = N_h * sd_h,
          n = N_h
        )
      output_df <- (as.data.frame(output_df))
      names(output_df)[names(output_df) == "n"] <- "npop"
      if (is.null(nsample)) {
        output_df <- dplyr::arrange(output_df, strata) %>%
          dplyr::mutate(
            stratum_fraction = round(n_sd / sum(n_sd),
              digits = ndigits
            ),
            sd = round(sd_h, digits = ndigits),
            n_sd = round(n_sd, digits = ndigits))
        output_df <- dplyr::select(
          output_df,
          strata, npop, sd, n_sd,
          stratum_fraction
        )
        return(output_df)
      }
      else {
        output_df <- output_df %>%
          dplyr::mutate(
            stratum_size = round(nsample * n_sd / sum(n_sd),
              digits = 0
            ),
            sd = round(sd_h, digits = ndigits),
            n_sd = round(n_sd, digits = ndigits)
          )
        output_df <- dplyr::arrange(output_df, strata)
        output_df <- dplyr::select(
          output_df,
          strata, npop, sd, n_sd,
          stratum_fraction, stratum_size
        )
        return(output_df)
      }
    }
    else if (method == "WrightI") {
      if (is.null(nsample)) {
        stop("This method requires a fixed nsample. Try method =
           'Neyman' for exact sampling fractions.")
      }
      else {
        n_strata <- length(unique(strata))
        n_minus_H <- nsample - n_strata
        if (n_minus_H <= 0) {
          stop("'nsample' is too small for this method.")
        }
        output_df <- output_df %>%
          dplyr::mutate(
            n = N_h,
            sd = sd_h,
            n_sd = n * sd
          )
        output_df <- tibble::as_tibble(dplyr::select(
          output_df,
          strata, n, sd, n_sd
        ))
        if (nsample > sum(output_df$n)) {
          stop("'nsample' is larger than population size")
        }
        priority_array <- list()
        for (i in 1:n_strata) {
          priority_array[[i]] <- c(
            rep(output_df[i, "n_sd"],
              times = min(output_df[i, "n"] - 1, n_minus_H)
            ),
            rep(NULL,
              times = ifelse(n_minus_H > (output_df[i, "n"] - 1),
                n_minus_H - (output_df[i, "n"] - 1),
                0
              )
            )
          )
          # All rows are same length, but zeroes so that n_sample
          # won't be larger than n_strata
          names(priority_array)[[i]] <- paste0("n_sd", as.character(i))
        }
        suppressMessages(Wright_output <- dplyr::bind_rows(priority_array))
        Wright_output[is.na(Wright_output)] <- 0
        mult_vec <- vector()
        for (i in 1:(n_minus_H + 1)) {
          mult_vec[i] <- 1 / (sqrt(i * (i + 1)))
        }
        for (i in seq_len(ncol(Wright_output))) {
          Wright_output[, i] <- Wright_output[, i] * mult_vec[i]
        }
        cutoff <- (sort(unlist(Wright_output, use.names = FALSE),
          decreasing = TRUE
        ))[n_minus_H]
        stratum_size <- rowSums(Wright_output >= cutoff) + 1
        final_output <- cbind(
          output_df[, c("strata", "n", "sd", "n_sd")], stratum_size
        )
        final_output <- final_output %>%
          dplyr::mutate(
            stratum_fraction = round(stratum_size / nsample,
              digits = ndigits
            ),
            sd = round(sd, digits = ndigits),
            n_sd = round(n_sd, digits = ndigits)
          )
        final_output <- final_output[c(
          "strata", "n", "sd", "n_sd", "stratum_fraction",
          "stratum_size"
        )]
        names(final_output)[names(final_output) == "n"] <- "npop"
        final_output <- dplyr::arrange(final_output, strata)
        return(final_output)
      }
    }
    else if (method == "WrightII") {
      if (is.null(nsample)) {
        stop("This method requires a fixed nsample. Try method =
           'Neyman' for exact sampling fractions.")
      }
      else {
        n_strata <- length(unique(strata))
        n_minus_2H <- nsample - 2 * n_strata
        if (n_minus_2H <= 0) {
          stop("'nsample' is too small for this method.")
        }
        output_df <- output_df %>%
          dplyr::mutate(
            n = N_h,
            sd = sd_h,
            n_sd = n * sd
          )
        output_df <- tibble::as_tibble(dplyr::select(
          output_df,
          strata, n, sd, n_sd
        ))
        if (nsample > sum(output_df$n)) {
          stop("'nsample' is larger than population size")
        }
        priority_array <- list()
        for (i in 1:n_strata) {
          priority_array[[i]] <- c(
            rep(output_df[i, "n_sd"],
              times = min(output_df[i, "n"] - 2, n_minus_2H)
            ),
            rep(output_df[i, "n_sd"] * 0,
              times = ifelse(n_minus_2H > (output_df[i, "n"] - 2),
                n_minus_2H - (output_df[i, "n"] - 2),
                0
              )
            )
          )
          # All rows are same length, but zeroes so that n_sample
          # won't be larger than n_strata. Zero instead of NULL so
          # entries in list aren't empty when n=2.
          names(priority_array)[[i]] <- paste0("n_sd", as.character(i))
        }
        suppressMessages(Wright_output <- dplyr::bind_rows(priority_array))
        Wright_output[is.na(Wright_output)] <- 0
        mult_vec <- vector()
        for (i in 2:(n_minus_2H + 1)) {
          mult_vec[i - 1] <- 1 / (sqrt(i * (i + 1)))
        }
        for (i in seq_len(ncol(Wright_output))) {
          Wright_output[, i] <- Wright_output[, i] * mult_vec[i]
        }
        cutoff <- (sort(unlist(Wright_output, use.names = FALSE),
          decreasing = TRUE
        ))[n_minus_2H]
        stratum_size <- rowSums(Wright_output >= cutoff) + 2
        final_output <- cbind(
          output_df[, c("strata", "n", "sd", "n_sd")], stratum_size
        )
        final_output <- final_output %>%
          dplyr::mutate(
            stratum_fraction = round(stratum_size / nsample,
              digits = ndigits
            ),
            sd = round(sd, digits = ndigits),
            n_sd = round(n_sd, digits = ndigits)
          )
        final_output <- final_output[
          c(
            "strata", "n", "sd", "n_sd", "stratum_fraction",
            "stratum_size"
          )
        ]
        names(final_output)[names(final_output) == "n"] <- "npop"
        final_output <- dplyr::arrange(final_output, strata)
        return(final_output)
      }
    }
    # End if sd is given and y is NULL
  }} # This ends case where y or sd_h is length one. Typical Neyman allocation

  ####
  #### Begin case where y or sd_h is length > 1
  if(length(y) > 1 | length(sd_h) > 1) {

    # Check for weights
    if(is.null(weights) | sum(weights) != 1){
      stop("Must provide a vector of 'weights' which sum to 1
      if 'y' or 'sd_h' is a vector of length > 1.")
    }

    ## First, if y is supplied
    if (!is.null(y)) {
      if (!is.null(N_h)) {
        stop("If 'y' is given then 'sd_h' and 'N_h' should be NULL.")
      }
      if (all(y %in% names(data)) == FALSE) {
        stop("'y' must be a character string or vector of strings matching
             column names of data.")
      }
      if (!all(sapply(data[, y, drop = FALSE], is.numeric))) {
        stop("All columns specified in 'y' must be numeric.")
      }
      if(length(weights) != length(y)) {
        stop("If 'y' is a vector, then 'weights' must be a vector of the same
             length.")
      }
      method <- match.arg(method)
      P <- length(y)
      if(is.factor(data[,strata])){
        data[,strata] <- droplevels(data[,strata]) # Ignore empty levels
      }
      N_h <- table(data[,strata]) # before removing NAs
      data <- data[!is.na(data[,y[1]]),]
      var_list <- list()
      a_var_list <- list()
      for (i in seq_along(y)){
        variances <- tapply(data[,y[i]], data[,strata], stats::var)
        var_list[[i]] <- variances
        a_var_list[[i]] <- weights[i]*variances
      }
      sums <- rowSums(t(dplyr::bind_rows(a_var_list))) # Gives sum per stratum
      df <- data.frame("strata" = names(N_h), "N_h" = as.vector(N_h),
                       "sqrt_a_var_sum" = sqrt(sums))

      # Now run Neyman on this df
      output <- optimum_allocation(df, strata = "strata", sd_h = "sqrt_a_var_sum",
                                   N_h = "N_h", nsample = nsample,
                                   method = method)

      # Add sd for each var of interest to output df
      output <- output[, names(output)[names(output) != "sd"]]

      sd_list <- lapply(var_list, function(x) round(sqrt(x), ndigits))
      sd_df <- as.data.frame(sd_list)
      sd_df$stratum <- rownames(sd_df)
      sd_df <- sd_df[, c("stratum", setdiff(names(sd_df), "stratum"))]
      names(sd_df) <- c("strata", paste0("sd_", y))

      output <- dplyr::left_join(
        sd_df,
        output,
        by = "strata"
      )

      # arrange appropriately
      non_sd_cols <- setdiff(names(output), names(sd_df)[names(sd_df) != "strata"])
      new_order <- c(non_sd_cols[1:2], names(sd_df)[names(sd_df) != "strata"],
                     non_sd_cols[-(1:2)])
      output <- output[, new_order]
      return(output)
    }

    # Else, if sd_h is given and y is null
    else {
      if (any(sd_h %in% names(data) == FALSE)) {
        stop("'sd_h' must be character strings or vector of strings
             matching column names of data.")
      }
      if (length(N_h) > 1 | N_h %in% names(data) == FALSE) {
        stop("'N_h' must be a character string
             matching a column name of data.")
      }
      if (!all(sapply(data[, sd_h, drop = FALSE], is.numeric))) {
        stop("All columns specified in 'sd_h' must be numeric.")
      }
      if (is.numeric(as.vector(data[, N_h])) == FALSE) {
        stop("N_h' must be numeric.")
      }
      if(length(weights) != length(sd_h)) {
        stop("If 'sd_h' is a vector, then it and 'weights'
        must all be vectors of the same length.")
      }
      if (any(table(data[,strata]) > 1)) {
        stop("If using 'sd_h' and 'N_h' instead of 'y',
      data must only contain one row per stratum.")
      }
      P <- length(sd_h)
      method <- match.arg(method)
      df <- data[, c(strata, N_h, sd_h)]
      strata <- interaction(df[,strata])
      df <- cbind(strata, df[,c(N_h, sd_h)])
      # Only columns of interest
      if (any(table(df$strata) > 1)) {
        stop("If using 'sd_h' and 'N_h' instead of 'y',
      data must only contain one row per stratum.")
      }
      if (sum(is.na(df)) >= 1) {
        stop("Data cannot contain NAs.")
      }
      # Now multiply weights by variances.
      weighted_vars <- sweep(df[, sd_h]^2, 2, weights, FUN = "*")
      sums <- rowSums(weighted_vars)
      df <- data.frame("strata" = df$strata, "N_h" = as.vector(data[,N_h]),
                       "sqrt_a_var_sum" = sqrt(sums))

      # Now run Neyman on this df
      output <- optimum_allocation(df, strata = "strata", sd_h = "sqrt_a_var_sum",
                                   N_h = "N_h", nsample = nsample,
                                   method = method)

      # Add sd for each var of interest
      output <- output[, names(output)[names(output) != "sd"]]
      output <- dplyr::left_join(
        cbind("strata" = df$strata, data[, sd_h]),
        output,
        by = "strata"
      )

      # arrange appropriately
      non_sd_cols <- setdiff(names(output), sd_h)
      new_order <- c(non_sd_cols[1:2], sd_h, non_sd_cols[-(1:2)])
      output <- output[, new_order]
      output[,sd_h] <- round(output[,sd_h], ndigits)
      return(output)
    }
  }
}
