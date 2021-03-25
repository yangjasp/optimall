#' Method for summary for class Multiwave
#' @param object object of class "Multiwave"
#' @aliases summary,Multiwave-method
#' @export

setMethod("summary", "Multiwave", function(object) {
  n_waves <- vector()
  for (i in (seq_along(object@phases))){
    n_waves[i] <- ifelse(names(object@phases)[[i]] == "phase1",
                         1,
                         length(object@phases[[i]]@waves))
  }
  n_waves[1] <- 1
  cat("A 'Multiwave' object with", length(object@phases),
      "phases containing",
  paste(n_waves,collapse = ", "), "waves respectively: \n",
  "\n",
  "Phase 1: \n",
  " data: ", as.character(nrow(object@phases$phase1$data)),
  "observations of", as.character(ncol(object@phases$phase1$data)), "vars:",
  paste(names(object@phases$phase1$data)
        [1:min(5, length(names(object@phases$phase1$data)))],
        collapse = ", "),
  ifelse(length(names(object@phases$phase1$data)) > 5, "... \n","\n"))

  #Add info from other phases
  if(length(object@phases) > 1){
    phase_data <- vector(mode = "character")
    for (i in 2:length(object@phases)){
      title <- paste0("Phase ",as.character(i),": \n")
      wave_data <- list()
      for (j in seq_len(length(object@phases[[i]]@waves))){
        info <- if (nrow(object@phases[[i]]@waves[[j]]@data) == 0 &
                         ncol(object@phases[[i]]@waves[[j]]@data) == 0){
          c("NA \n"," \n")
          } else{
            c(as.character(nrow(object@phases[[i]]@waves[[j]]@data)),
            "observations of",
            as.character(ncol(object@phases[[i]]@waves[[j]]@data)),
            "vars:",
            paste(names(object@phases[[i]]@waves[[j]]@data)
                                  [1:min(5,length(names(object@phases[[i]]
                                                      @waves[[j]]@data)))],
                  collapse = ", "),
            ifelse(length(names(object@phases[[i]]@waves[[j]]@data)) > 5,
                   "... \n","\n")," \n")
            }
        wave_data[[j]] <- c(paste0("Wave ",j,": \n", " data:"),info)
      }
      cat(title, paste(unlist(wave_data),
                       collapse = " "))
    }
    #cat(paste(phase_data, collapse = " "))
  }
  })
