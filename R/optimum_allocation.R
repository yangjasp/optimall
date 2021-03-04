#' Optimum Allocation
#'
#' Determines the optimum sampling fraction and sample size for
#' each stratum in a stratified random sample according to Neyman
#' Allocation or Exact Optimum Sample Allocation (Wright 2014).
#' @param data A data frame or matrix with one row for each
#' sampled unit, one column specifying each unit's stratum, and
#' one column holding the value of the continuous variable for
#' which the variance should be minimized.
#' @param strata a character string or vector of character strings
#' specifying the name(s) of columns which specify the stratum
#' that each unit belongs to. If multiple column names are
#' provided, each unique combination of values in these columns
#' is taken to define one stratum.
#' @param y a character string specifying the name of the
#' continuous variable for which the variance should be minimized.
#' @param nsample the desired total sample size. Defaults to NULL.
#' @param method a character string specifying the method of
#' optimum sample allocation to use. Must be one of:
#' \itemize{
#' \item \code{"WrightII"}, the default, uses Algorithm II from
#' Wright (2014) to determine the optimum allocation of a fixed
#' sample size across the strata. It requires that at least two
#' samples are allocated to each stratum, and it is recommended
#' because it is the only algorithm that requires an unbiased
#' estimate of the variance and always produces the global
#' optimum allocation.
#' \item \code{"WrightI"} uses Wright's Algorithm I to determine
#' the optimum sample allocation. It only requires that at least
#' one sample is allocated to each stratum, and can therefore
#' lead to a biased variance estimate. It is not recommended
#' except for in very specific cases.
#' \item \code{"Neyman"} uses the standard method of Neyman
#' Allocation to determine the optimum sample allocation. Its
#' calculated stratum sample sizes are frequently non-integer
#' values, but the output rounds these to the nearest integer.
#' It is best used when nsample is `NULL` because it will output
#' exact sampling fractions.
#' }
#' @param ndigits a numeric value specifying the number of digits
#' to round the standard deviation and stratum fraction to.
#' Defaults to 2.
#' @param allow.na logical input specifying whether y should
#' be allowed to have NA values. Defaults to \code{FALSE}.
#' @examples
#' optimum_allocation(
#'   data = iris, strata = "Species", y = "Sepal.Length",
#'   nsample = 100, method = "WrightII"
#' )
#' @export
#' @references Wright, T. (2014). A simple method of exact optimal
#' sample allocation under stratification with any mixed
#' constraint patterns. Statistics, 07.
#' @return Returns a data frame with the n allocated to each
#' strata or the sampling fractions if nsample is NULL.
#' @importFrom magrittr %>%

optimum_allocation <- function(data, strata, y, nsample = NULL,
                               ndigits = 2, method = "WrightII",
                               allow.na = FALSE, phase, wave) {
  UseMethod("optimum_allocation", data)
}
