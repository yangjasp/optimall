#' Phase Class for Multi-Wave Sampling Organization
#'
#' \pkg{optimall} defines three S4 classes for organizing the
#' multi-wave sampling workflow: \code{Wave}, \code{Phase},
#' and \code{Multiwave}.
#' An object of class \code{Multiwave} holds metadata and a list of objects of
#' class \code{Phase}, which in turn holds metadata and a list of
#' objects of class
#' \code{Wave}. These three object classes are used together to organize the
#' workflow of multi-wave sampling designs.
#'
#' @slot metadata A list containing the phase metadata
#' @slot waves A list of objects of class \code{Wave}, each element
#' representing one wave of the phase
#' @import methods
#' @export Phase
#' @exportClass Phase

Phase <- setClass("Phase", slots = list(
  metadata = "list",
  waves = "list"
))
