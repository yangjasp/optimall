% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/optimum_allocation.R
\name{optimum_allocation}
\alias{optimum_allocation}
\title{Optimum Allocation}
\usage{
optimum_allocation(
  data,
  strata,
  y = NULL,
  sd_h = NULL,
  N_h = NULL,
  nsample = NULL,
  ndigits = 2,
  method = c("WrightII", "WrightI", "Neyman"),
  allow.na = FALSE
)
}
\arguments{
\item{data}{A data frame or matrix with at least one column specifying
each unit's stratum, and either 1) a second column holding the value of the
continuous variable for which the sample mean variance should be minimized
(\code{y}) or 2) two columns: one holding the the within-stratum
standard deviation for the variable of interest (\code{sd_h}) and another
holding the stratum sample sizes (\code{N_h}).
If \code{data} contains a column \code{y} holding values for
the variable of interest, then \code{data} should have one row for
each sampled unit. If \code{data} holds \code{sd_h} and \code{N_h}, the
within-stratum standard deviations and population sizes, then \code{data}
should have one row per stratum.
Other columns are allowed but will be ignored.}

\item{strata}{a character string or vector of character strings
specifying the name(s) of columns which specify the stratum
that each unit belongs to. If multiple column names are
provided, each unique combination of values in these columns
is taken to define one stratum.}

\item{y}{a character string specifying the name of the
continuous variable for which the variance should be minimized.
Defaults to \code{NULL} and should be left as \code{NULL} when \code{data}
holds stratum standard deviations and sample sizes instead of individual
sampling units.}

\item{sd_h}{a character string specifying the name of the
column holding the within-stratum standard deviations for each stratum.
Defaults to \code{NULL} and should be left as \code{NULL} when \code{data}
holds individual sampling units.}

\item{N_h}{a character string specifying the name of the
column holding the population stratum sizes for each stratum.
Defaults to \code{NULL} and should be left as \code{NULL} when \code{data}
holds individual sampling units.}

\item{nsample}{the desired total sample size. Defaults to \code{NULL}.}

\item{ndigits}{a numeric value specifying the number of digits
to which the standard deviation and stratum fraction should be rounded.
Defaults to 2.}

\item{method}{a character string specifying the method of
optimum sample allocation to use. Must be one of:
\itemize{
\item \code{"WrightII"}, the default, uses Algorithm II from
Wright (2014) to determine the optimum allocation of a fixed
sample size across the strata. It requires that at least two
samples are allocated to each stratum.
\item \code{"WrightI"} uses Wright's Algorithm I to determine
the optimum sample allocation. It only requires that at least
one sample is allocated to each stratum, and can therefore
lead to a biased variance estimate.
\item \code{"Neyman"} uses the standard method of Neyman
Allocation to determine the optimum sample allocation. When
\code{nsample = NULL}, the optimal sampling fraction is calculated
and returned. When a numeric value is specified for \code{nsample},
then the number allocated to each stratum is the optimal sampling
fraction times \code{nsample} rounded to the nearest integer,
which may no longer be optimall.
}}

\item{allow.na}{logical input specifying whether y should
be allowed to have NA values. Defaults to \code{FALSE}.}
}
\value{
Returns a data frame with the number of samples allocated to each
stratum, or just the sampling fractions if nsample is NULL.
}
\description{
Determines the optimum sampling fraction and sample size for
each stratum in a stratified random sample, which
minimizes the variance of the sample mean according to Neyman
Allocation or Exact Optimum Sample Allocation (Wright 2014).
}
\examples{
optimum_allocation(
  data = iris, strata = "Species", y = "Sepal.Length",
  nsample = 40, method = "WrightII"
)

# Or if input data is summary of strata sd and N:
iris_summary <- data.frame(
  strata = unique(iris$Species),
  size = c(50, 50, 50),
  sd = c(0.3791, 0.3138, 0.3225)
)

optimum_allocation(
  data = iris_summary, strata = "strata",
  sd_h = "sd", N_h = "size",
  nsample = 40, method = "WrightII"
)
}
\references{
Wright, T. (2014). A Simple Method of Exact Optimal
Sample Allocation under Stratification with any Mixed
Constraint Patterns, Research Report Series (Statistics #2014-07),
Center for Statistical Research and Methodology, U.S. Bureau
of the Census, Washington, D.C.
}
