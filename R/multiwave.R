#'Define Classes for Multi-Wave Sampling Organization
#'
#'\pkg{optimall} defines three S4 classes for organizing the
#'multi-wave sampling workflow:
#'\itemize{
#'\item{\code{wave}:}
#'}
#'
#' @import methods
#' @export multiwave
#' @exportClass multiwave

multiwave <- setClass("multiwave", slots = list(metadata = "list",
                                           phases = "list"))

