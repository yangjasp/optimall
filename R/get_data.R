#' Access and Write Slots of a Multiwave Object
#'
#' \code{get_data} is the accessor function for objects of
#' class \code{Multiwave}. It can be used to access or write slots.
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
#' @return If accessing a multiwave object slot, returns the specified slot.
#'
#' @name get_data
#'
#' @examples
#' # Intiate multiwave object
#' MySurvey <- new_multiwave(phases = 2, waves = c(1, 3))
#'
#' # To access overall metadata
#' get_data(MySurvey, phase = NA, slot = "metadata")
#'
#' # To write overall metadata
#' get_data(MySurvey, phase = NA, slot = "metadata") <- list(
#'   title = "Maternal Weight Survey"
#' )
#'
#' # To access Phase 2 metadata
#' get_data(MySurvey, phase = 2, slot = "metadata")
#'
#' # To access Phase 2, Wave 2 design
#' get_data(MySurvey, phase = 2, wave = 2, slot = "design")
#' @export
#' @include multiwave.R phase.R wave.R
NULL

#' @aliases get_data,Multiwave-method
#' @describeIn get_data
#' access slot of multiwave object
#' @export
setGeneric("get_data", function(x, phase = 1, wave = NA,
                                slot = c("data", "design",
                                         "metadata", "samples",
                                         "sampled_data")) {
  standardGeneric("get_data")
})

setMethod("get_data", c(x = "Multiwave"), function(x, phase = 1,
                                                   wave = NA,
                                                   slot = c("data",
                                                            "design",
                                                            "metadata",
                                                            "samples",
                                                            "sampled_data")){
  if (inherits(x, "Multiwave")  == FALSE) {
    stop("'x' must be an object of class 'Multiwave'")
  }
  slot <- match.arg(slot)
  if (is.na(phase) & is.na(wave) & slot == "metadata") {
    x@metadata
  } else if (is.na(phase)) {
    stop("must specify a phase unless getting overall metadata")
  } else if (phase != 1 & phase != "phase1" & is.na(wave) == TRUE
  & !is.na(phase) & slot != "metadata") {
    stop("must specify wave number unless getting phase 1 or
         survey metadata")
  } else if ((phase == 1 | phase == "phase1") & slot %in% c("data",
                                                            "metadata")) {
    x@phases$phase1[[slot]]
  } else if ((phase > 1 | (!is.na(phase) & phase != "phase1")) &
    is.na(wave) & slot == "metadata") {
    x@phases[[phase]]@metadata
  } else if ((phase > 1 | (!is.na(phase) & phase != "phase1")) &
    slot == "data") {
    x@phases[[phase]]@waves[[wave]]@data
  } else if ((phase > 1 | (!is.na(phase) & phase != "phase1")) &
    slot == "metadata") {
    x@phases[[phase]]@waves[[wave]]@metadata
  } else if ((phase > 1 | (!is.na(phase) & phase != "phase1")) &
    slot == "samples") {
    x@phases[[phase]]@waves[[wave]]@samples
  } else if ((phase > 1 | (!is.na(phase) & phase != "phase1")) &
    slot == "sampled_data") {
    x@phases[[phase]]@waves[[wave]]@sampled_data
  } else if ((phase > 1 | (!is.na(phase) & phase != "phase1")) &
    slot == "design") {
    x@phases[[phase]]@waves[[wave]]@design
  }
  else {
    stop("unable to find selection in 'x': invalid selection")
  }
})

#' @describeIn get_data
#' assign value to slot of a multiwave object
#' @param value value to assign to specified slot
#' @aliases get_data<-,Multiwave-method
#' @export
#'
setGeneric("get_data<-", function(x, phase = 1, wave = NA,
                                  slot = c("data", "design",
                                           "metadata", "samples",
                                           "sampled_data"),
value) {
  standardGeneric("get_data<-")
})

setMethod("get_data<-", c(x = "Multiwave"), function(x, phase = 1, wave = NA,
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
