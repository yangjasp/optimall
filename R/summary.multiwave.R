#' Method for summary for class Multiwave
#' @param object object of class "Multiwave"
#' @return Prints a summary of the specified multiwave object in the console.
#' @aliases summary,Multiwave-method
#' @export

setMethod("summary", "Multiwave", function(object) {
  n_waves <- vector()
  for (i in (seq_along(object@phases))) {
    n_waves[i] <- ifelse(names(object@phases)[[i]] == "phase1",
      1,
      length(object@phases[[i]]@waves)
    )
  }
  n_waves[1] <- 1
  cat(
    "A 'Multiwave' object with", length(object@phases),
    "phases containing",
    paste(n_waves, collapse = ", "), "waves respectively:\n",
    "\n"
  )
  cat(
    "Overall Metadata: list of length", paste0(
      length(object@metadata),
      ":"
    ),
    paste(names(object@metadata)
    [1:min(5, length(names(object@metadata)))],
    collapse = ", "
    ),
    ifelse(length(names(object@metadata)) > 5, "... \n", "\n"), "\n"
  )

  cat(
    "Phase 1: \n",
    "metadata: list of length", paste0(
      length(object@phases$phase1$metadata),
      ":"
    ),
    paste(names(object@phases$phase1$metadata)
    [1:min(5, length(names(object@phases$phase1$metadata)))],
    collapse = ", "
    ),
    ifelse(length(names(object@phases$phase1$metadata)) > 5, "... \n", "\n"),
    "data: df with", as.character(nrow(object@phases$phase1$data)),
    "obs. of", as.character(ncol(object@phases$phase1$data)), "vars:",
    paste(names(object@phases$phase1$data)
    [1:min(5, length(names(object@phases$phase1$data)))],
    collapse = ", "
    ),
    ifelse(length(names(object@phases$phase1$data)) > 5, "... \n", "\n"),
    "\n"
  )

  # Add info from other phases
  if (length(object@phases) > 1) {
    phase_data <- vector(mode = "character")
    for (i in 2:length(object@phases)) {
      title <- paste0("Phase ", as.character(i), ": \n")
      wave_data <- list()
      data_list <- list()
      design_list <- list()
      samples_list <- list()
      sampled_data_list <- list()
      metadata_list <- list()
      for (j in seq_len(length(object@phases[[i]]@waves))) {
        data_list[[j]] <- if (nrow(object@phases[[i]]@waves[[j]]@data) == 0 &
          ncol(object@phases[[i]]@waves[[j]]@data) == 0) {
          paste0(" NA \n", " \n")
        } else {
          paste(" df with",
            as.character(nrow(object@phases[[i]]@waves[[j]]@data)),
            "obs. of",
            as.character(ncol(object@phases[[i]]@waves[[j]]@data)),
            "vars:",
            paste(names(object@phases[[i]]@waves[[j]]@data)
            [1:min(5, length(names(object@phases[[i]]
              @waves[[j]]@data)))],
            collapse = ", "
            ),
            ifelse(length(names(object@phases[[i]]@waves[[j]]@data)) > 5,
              "... \n", "\n"
            ), " \n",
            sep = " "
          )
        }
        metadata_list[[j]] <- paste(
          "list of length",
          paste0(
            length(object@phases[[i]]@waves[[j]]@metadata),
            ":"
          ), "\n"
        )
        design_list[[j]] <- if (
          nrow(object@phases[[i]]@waves[[j]]@design) == 0 &
            ncol(object@phases[[i]]@waves[[j]]@design) == 0) {
          paste0(" NA \n")
        } else {
          paste(" df with",
            as.character(nrow(object@phases[[i]]@waves[[j]]@design)),
            "obs. of",
            as.character(ncol(object@phases[[i]]@waves[[j]]@design)),
            "vars:",
            paste(names(object@phases[[i]]@waves[[j]]@design)
            [1:min(5, length(names(object@phases[[i]]
              @waves[[j]]@design)))],
            collapse = ", "
            ),
            ifelse(length(names(object@phases[[i]]@waves[[j]]@design)) > 5,
              "... \n", "\n"
            ),
            sep = " "
          )
        }
        samples_list[[j]] <-
          if (length(object@phases[[i]]@waves[[j]]@samples) == 0) {
            paste0(" NA \n")
          } else {
            paste(" vector of length",
              as.character(
                length(object@phases[[i]]@waves[[j]]@samples)
              ), "\n",
              sep = " "
            )
          }
        sampled_data_list[[j]] <-
          if (nrow(object@phases[[i]]@waves[[j]]@sampled_data) == 0 &
            ncol(object@phases[[i]]@waves[[j]]@sampled_data) == 0) {
            paste0(" NA \n")
          } else {
            paste(" df with",
              as.character(nrow(
                object@phases[[i]]@waves[[j]]@sampled_data
              )),
              "obs. of",
              as.character(ncol(
                object@phases[[i]]@waves[[j]]@sampled_data
              )),
              "vars:",
              paste(names(object@phases[[i]]@waves[[j]]@sampled_data)
              [1:min(5, length(names(object@phases[[i]]
                @waves[[j]]@sampled_data)))],
              collapse = ", "
              ),
              ifelse(length(
                names(object@phases[[i]]@waves[[j]]@sampled_data)
              ) > 5,
              "... \n", "\n"
              ),
              sep = " "
            )
          }
        wave_data[[j]] <- c(paste0(
          "Wave ", j, ": \n",
          " metadata: ",
          paste(unlist(metadata_list[[j]])),
          " design:",
          paste(unlist(design_list[[j]])),
          " sample IDs:",
          paste(unlist(samples_list[[j]])),
          " sampled data:",
          paste(unlist(samples_list[[j]])),
          " data:",
          paste(unlist(data_list[[j]]))
        ))
      }
      cat(title, paste(unlist(wave_data),
        collapse = " "
      ))
    }
    # cat(paste(phase_data, collapse = " "))
  }
})
