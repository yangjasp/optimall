#'Define Classes for Multi-Wave Sampling Organization
#'
#'\pkg{optimall} defines three S4 classes for organizing the
#'multi-wave sampling workflow:
#'\itemize{
#'\item{\code{wave}:}
#'}
#' @slot metadata A list containing the metadata for the wave.
#' @slot design a dataframe specifying the design of the wave. Is often the output of \code{allocate_wave}.
#' @slot samples A character vector containing the ids of the units sampled in the wave.
#' @slot sampled_data A dataframe holding the data, with ids, \emph{collected} in this wave of sampling
#' @slot data A dataframe holding the updated full data set with all of the Phase 1 sampling units including the samples collected in this wave.
#' @data
#' @import methods
#' @export

setClass("wave", slots = list(metadata = "list",
                                      design = "data.frame",
                                      samples = "character",
                                      sampled_data = "data.frame",
                                      data = "data.frame"))
