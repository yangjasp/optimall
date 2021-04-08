#' Multiwave Class for Multi-Wave Sampling Organization
#'
#' \pkg{optimall} defines three S4 classes for organizing the
#' multi-wave sampling workflow: \code{Wave}, \code{Phase}, and
#' \code{Multiwave}.
#' An object of class \code{Multiwave} holds metadata and a list of objects of
#' class \code{Phase}, which in turn holds metadata and a list of
#' objects of class
#' \code{Wave}. These three object classes are used together to organize the
#' workflow of multi-wave sampling designs.
#'
#' @slot metadata A list of elements that describe the entire survey.
#' The list is empty upon initialization of the multiwave object, but the user
#' may add anything to it as they see fit. It may include a "title".
#' @slot phases A list of objects of class \code{Phase} (see other class
#' documentation).
#' @import methods
#' @export Multiwave
#' @exportClass Multiwave

Multiwave <- setClass("Multiwave", slots = list(
  metadata = "list",
  phases = "list"
))
