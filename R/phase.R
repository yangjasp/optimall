#'Define Classes for Multi-Wave Sampling Organization
#'
#'\pkg{optimall} defines three S4 classes for organizing the
#'multi-wave sampling workflow:
#'\itemize{
#'\item{\code{wave}:}
#'}
#'
#' @slot metadata A list containing the phase metadata
#' @slot waves A list of objects of class \code{wave}, each element
#' representing one wave of the phase
#' @import methods
#' @export

setClass("phase", slots = list(metadata = "list",
                               waves = "list"))
