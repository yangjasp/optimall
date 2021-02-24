#' Access and Write Slots of a Multiwave Object
#'
#' \code{get_data} is the accessor function for objects of class \code{Multiwave}.
#' @name get_data
#' @export
#' @include multiwave.R phase.R wave.R
NULL

get_data <- function(x, phase = 1, wave = NA, slot = "data"){
  if (class(x) != "Multiwave"){
    stop("'x' must be an object of class 'Multiwave'")
  }
  if (is.na(phase) & is.na(wave) & slot == "metadata"){
    x@metadata
  } else if(is.na(phase)){
    stop("must specify a phase unless getting overall metadata")
  } else if (phase != 1 & phase != "phase1" & is.na(wave) == TRUE
      & !is.na(phase) & slot != "metadata"){
      stop("must specify wave number unless getting phase 1 or
         survey metadata")
  } else if ((phase == 1| phase == "phase1") & slot %in% c("data","metadata")){
    x@phases$phase1[[slot]]
  } else if ((phase > 1 | (!is.na(phase) & phase != "phase1")) &
             is.na(wave) & slot == "metadata"){
    x@phases[[phase]]@metadata
  } else if((phase > 1 | (!is.na(phase) & phase != "phase1")) &
            slot == "data"){
    x@phases[[phase]]@waves[[wave]]@data
  } else if((phase > 1 | (!is.na(phase) & phase != "phase1")) &
            slot == "metadata"){
    x@phases[[phase]]@waves[[wave]]@metadata
  } else if((phase > 1 | (!is.na(phase) & phase != "phase1")) &
            slot == "samples"){
      x@phases[[phase]]@waves[[wave]]@samples
  } else if((phase > 1 | (!is.na(phase) & phase != "phase1")) &
             slot == "sampled_data"){
    x@phases[[phase]]@waves[[wave]]@sampled_data
  } else if((phase > 1 | (!is.na(phase) & phase != "phase1")) &
            slot == "design"){
    x@phases[[phase]]@waves[[wave]]@design
  }
  else {
    stop("unable to find selection in 'x': invalid selection")
  }
}

setGeneric("get_data<-", function(x, phase = 1, wave = NA, slot = "data",
                                  value)
  standardGeneric("get_data<-"))
setMethod("get_data<-", c(x = "Multiwave"), function(x, phase = 1, wave = NA,
                                              slot = "data", value){
  if (class(x) != "Multiwave"){
    stop("'x' must be an object of class 'multiwave'")
  }
  if (is.na(phase) & is.na(wave) & slot == "metadata"){
    x@metadata <- value
  } else if(is.na(phase)){
    stop("must specify a phase unless getting overall metadata")
  } else if (phase != 1 & phase != "phase1" & is.na(wave) == TRUE
             & !is.na(phase) & slot != "metadata"){
    stop("must specify wave number unless getting phase 1 or
         survey metadata")
  } else if ((phase == 1| phase == "phase1") & slot %in% c("data","metadata")){
    x@phases$phase1[[slot]] <- value
  } else if ((phase > 1 | (!is.na(phase) & phase != "phase1")) &
             is.na(wave) & slot == "metadata"){
    x@phases[[phase]]@metadata <- value
  } else if((phase > 1 | (!is.na(phase) & phase != "phase1")) &
            slot == "data"){
    x@phases[[phase]]@waves[[wave]]@data <- value
  } else if((phase > 1 | (!is.na(phase) & phase != "phase1")) &
            slot == "metadata"){
    x@phases[[phase]]@waves[[wave]]@metadata <- value
  } else if((phase > 1 | (!is.na(phase) & phase != "phase1")) &
            slot == "samples"){
    x@phases[[phase]]@waves[[wave]]@samples <- value
  } else if((phase > 1 | (!is.na(phase) & phase != "phase1")) &
            slot == "sampled_data"){
    x@phases[[phase]]@waves[[wave]]@sampled_data <- value
  } else if((phase > 1 | (!is.na(phase) & phase != "phase1")) &
            slot == "design"){
    x@phases[[phase]]@waves[[wave]]@design <- value
  }
  validObject(x)
  x
})
