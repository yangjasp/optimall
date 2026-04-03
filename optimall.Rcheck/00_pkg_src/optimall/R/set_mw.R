#' Write Slots of a Multiwave Object
#'
#' \code{set_mw} is used to assign values (write to) slots of
#' \code{Multiwave} class objects. It is used to set values of
#' multiwave (mw) objects.
#' @param x an object of class \code{'Multiwave'}
#' @param phase a numeric value specifying the phase that should be accessed.
#' To access the overall metadata, set \code{phase = NA}. Defaults to 1.
#' @param wave a numeric value specifying the wave that should be accessed.
#' Ta access phase metadata, set \code{wave = NA}. Defaults to \code{NA}.
#' @param slot a character value specifying the name of the slot to be
#' accessed. Must be one of \code{"metadata"}, \code{"design"},
#' \code{"samples"}, \code{"sampled_data"}, \code{"data"}. Defaults to
#' \code{"data"}. See class documentation or package vignettes for more
#'  information about slots.
#'
#' @name set_mw<-
#'
#' @examples
#' # Intiate multiwave object
#' MySurvey <- multiwave(phases = 2, waves = c(1, 3))
#'
#' # To write overall metadata
#' set_mw(MySurvey, phase = NA, slot = "metadata") <-
#'  list(title = "Maternal Weight Survey")
#'
#' # To write Phase 2 metadata
#' set_mw(MySurvey, phase = 2, slot = "metadata") <-
#'  list(strata = "mystrata", id = "id")
#'
#' @export
#' @include multiwave.R phase.R wave.R
NULL

#' assign value to slot of a multiwave object
#' @param value value to assign to specified slot
#' @aliases set_mw<-,Multiwave-method
#' @export
#'
setGeneric("set_mw<-", function(x, phase = 1, wave = NA,
                                  slot = c("data", "design",
                                           "metadata", "samples",
                                           "sampled_data"),
                                  value) {
  standardGeneric("set_mw<-")
})

setMethod("set_mw<-", c(x = "Multiwave"), function(x, phase = 1, wave = NA,
                                                     slot = c("data",
                                                              "design",
                                                              "metadata",
                                                              "samples",
                                                              "sampled_data"),
                                                     value) {
  slot <- match.arg(slot)
  if (is.na(phase) & is.na(wave) & slot == "metadata") {
    x@metadata <- value
  } else if (is.na(phase)) {
    stop("must specify a phase unless getting overall metadata")
  } else if (phase != 1 & phase != "phase1" & is.na(wave) == TRUE
             & !is.na(phase) & slot != "metadata") {
    stop("must specify wave number unless getting phase 1 or
         survey metadata")
  } else if ((phase == 1 | phase == "phase1") & slot %in% c("data",
                                                            "metadata")) {
    x@phases$phase1[[slot]] <- value
  } else if ((phase > 1 | (!is.na(phase) & phase != "phase1")) &
             is.na(wave) & slot == "metadata") {
    x@phases[[phase]]@metadata <- value
  } else if ((phase > 1 | (!is.na(phase) & phase != "phase1")) &
             slot == "data") {
    x@phases[[phase]]@waves[[wave]]@data <- value
  } else if ((phase > 1 | (!is.na(phase) & phase != "phase1")) &
             slot == "metadata") {
    x@phases[[phase]]@waves[[wave]]@metadata <- value
  } else if ((phase > 1 | (!is.na(phase) & phase != "phase1")) &
             slot == "samples") {
    x@phases[[phase]]@waves[[wave]]@samples <- value
  } else if ((phase > 1 | (!is.na(phase) & phase != "phase1")) &
             slot == "sampled_data") {
    x@phases[[phase]]@waves[[wave]]@sampled_data <- value
  } else if ((phase > 1 | (!is.na(phase) & phase != "phase1")) &
             slot == "design") {
    x@phases[[phase]]@waves[[wave]]@design <- value
  }
  validObject(x)
  x
})
