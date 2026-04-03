#' Print Summary Diagram of Multiwave Object
#'
#' Takes a multiwave object as input and plots a diagram of its structure
#' in the plotting window using \code{grViz()} from the \code{DiagrammeR}
#' package. Red boxes indicate slots that have not yet been
#' filled, blue boxes indicate that the slot is filled.
#'
#' @param x An object of class \code{multiwave}.
#' @param height The height in pixels of the diagram. Defaults to \code{NULL}
#' , which produces default height.
#' @param width The width in pixels of the diagram. Defaults to \code{NULL},
#' which produces the default width.
#' @return Returns an object of class \code{htmlwidget}
#' displaying the structure of the \code{x}.
#' @examples
#' MySurvey <- multiwave(phases = 2, waves = c(1, 3))
#' multiwave_diagram(MySurvey)
#' @export
multiwave_diagram <- function(x, height = NULL, width = NULL) {
  if (inherits(x, "Multiwave") == FALSE) {
    stop("'x' must be an object of class Multiwave")
  }

  # Install DiagrammeR if it is not already installed
  if (! requireNamespace("DiagrammeR", quietly = TRUE)) {
    stop("Please install DiagrammeR: install.packages('DiagrammeR').
         Otherwise, try 'summary()'",
         call. = FALSE)}

  title_char <- n_phases <- n_waves <- metadata_rec <- design_rec <-
    samples_rec <-
    sampled_data_rec <- data_rec <- full_slot_color <- empty_slot_color <-
    overall_md_color <- overall_md_text <- p1_md_color <- p1_md_text <-
    p1_data_color <- p1_data_text <- default_rec <- recs <- n_waves_prev <-
    title <- start <- title_w <- md_color <- md_text <-
    des_color <- des_text <- sampled_df_color <- sampled_df_text <-
    samples_color <- samples_text <- data_color <- data_text <-
    edges <- edges_vec <- phases_recs <- phases_recs_indices <- recs_vec <-
    recs_vec_short <- wave_edges <- NULL

  # Find title if there is one in total metadata
  if ("title" %in% names(x@metadata) &
    is.character(x@metadata$title)) {
    title_char <- paste(
      "rec1 [label = <<b>", x@metadata$title,
      "</b>>, fillcolor = lightgray, style = filled]"
    )
  } else {
    title_char <- "rec1 [label = <<b>Multiwave Object</b>>,
  fillcolor = lightgray, style = filled]"
  }

  # Find number of phases and number of waves per phase
  n_phases <- length(x@phases)
  n_waves <- vector()
  for (i in (seq_along(x@phases))) {
    n_waves[i] <- ifelse(names(x@phases)[[i]] == "phase1",
      1,
      length(x@phases[[i]]@waves)
    )
  }
  n_waves[1] <- 1

  # Write grViz code for these phases and waves.
  # Start by making rectangles for wave
  metadata_rec <-
    "[label = <<b>Metadata</b>"
  design_rec <- "[label = <<b>Design</b>"
  samples_rec <- "[label = <<b>Sample IDs</b>"
  sampled_data_rec <- "[label = <<b>Sampled Data</b>"
  data_rec <- "[label = <<b>Data</b>"

  # And set possible fillcolor attachments (will later select one of them)
  full_slot_color <- ", fillcolor = lightblue, style = filled]"
  empty_slot_color <- ", fillcolor = lightcoral, style = filled]"

  # Code boxes for phase 1 and overall metadata (default)
  overall_md_color <-
    ifelse(length(x@metadata) == 0,
      empty_slot_color,
      full_slot_color
    )
  overall_md_text <-
    ifelse(length(x@metadata) == 0,
      ">",
      paste(
        "<br/> list of length", as.character(length(x@metadata)),
        ">"
      )
    )
  p1_md_color <-
    ifelse(length(x@phases$phase1$metadata) == 0,
      empty_slot_color,
      full_slot_color
    )
  p1_md_text <-
    ifelse(length(x@phases$phase1$metadata) == 0,
      ">",
      paste(
        "<br/> list of length",
        as.character(length(x@phases$phase1$metadata)),
        ">"
      )
    )
  p1_data_color <-
    ifelse(nrow(x@phases$phase1$data) == 0,
      empty_slot_color,
      full_slot_color
    )
  p1_data_text <-
    ifelse(length(x@phases$phase1$data) == 0,
      ">",
      paste(
        "<br/> df with",
        as.character(nrow(x@phases$phase1$data)),
        "obs. of",
        as.character(ncol(x@phases$phase1$data)),
        "vars>"
      )
    )

  default_rec <- c(
    paste(
      "rec2 [label = <<b>Metadata</b>",
      overall_md_text,
      overall_md_color
    ),
    paste(
      "rec3 [label = <<b>Phase 1</b>>,",
      "fillcolor = burlywood, style = filled]"
    ),
    paste(
      "rec4 [label = <<b>Metadata</b>",
      p1_md_text,
      p1_md_color
    ),
    paste(
      "rec5 [label = <<b>Data</b>",
      p1_data_text,
      p1_data_color
    )
  )
  # Now write boxes for n_phases. Phase 1 is already coded in default
  if (n_phases > 1) {
    recs <- list()
    # Find number of waves in previous phase so know where to start
    # each phase
    n_waves_prev <- n_waves[-length(n_waves)]
    n_waves_prev[1] <- 0
    for (i in 2:n_phases) {
      # Make Phase title, metadata
      title <- paste0("Phase ", as.character(i))
      recs[n_waves_prev[i - 1] * 6 + 1 * (i - 1)] <-
        paste0(
          "[label = <<b>",
          title,
          "</b>>, fillcolor = burlywood, style = filled]"
        )
      recs[n_waves_prev[i - 1] * 6 + 2 * (i - 1)] <-
        paste(
          "[label = <<b>Metadata</b>",
          ifelse(length(x@phases$phase2@metadata) == 0,
            ">, fillcolor = lightcoral, style = filled]",
            paste(
              "<br/> list of length",
              as.character(length(x@phases$phase2@metadata)),
              "> fillcolor = lightblue, style = filled]"
            )
          )
        )
      start <- n_waves_prev[i - 1] * 6 + 2 * (i - 1)
      for (j in seq_len(n_waves[i])) {
        title_w <- paste0("Wave ", as.character(j))
        recs[[(j - 1) * 6 + start + 1]] <-
          paste0(
            "[label = <<b>",
            title_w,
            "</b>>, fillcolor = burlywood, style = filled]"
          )
        # Check if each element of wave is complete and define color and
        # description based on this
        md_color <-
          ifelse(length(x@phases[[i]]@waves[[j]]@metadata) == 0,
            empty_slot_color,
            full_slot_color
          )
        md_text <-
          ifelse(length(x@phases[[i]]@waves[[j]]@metadata) == 0,
            ">",
            paste(
              "<br/> list of length",
              as.character(length(x@phases[[i]]@waves[[j]]@metadata)),
              ">"
            )
          )
        des_color <-
          ifelse(nrow(x@phases[[i]]@waves[[j]]@design) == 0,
            empty_slot_color,
            full_slot_color
          )
        des_text <-
          ifelse(nrow(x@phases[[i]]@waves[[j]]@design) == 0,
            ">",
            paste(
              "<br/> df with",
              as.character(nrow(x@phases[[i]]@waves[[j]]@design)),
              "obs. of",
              as.character(ncol(x@phases[[i]]@waves[[j]]@design)),
              "vars>"
            )
          )
        samples_color <-
          ifelse(length(x@phases[[i]]@waves[[j]]@samples$ids) == 0,
            empty_slot_color,
            full_slot_color
          )
        samples_text <-
          ifelse(length(x@phases[[i]]@waves[[j]]@samples$ids) == 0,
            ">",
            paste(
              "<br/> vector of length",
              as.character(length(x@phases[[i]]@waves[[j]]@samples$ids)),
              ">"
            )
          )

        sampled_df_color <-
          ifelse(nrow(x@phases[[i]]@waves[[j]]@sampled_data) == 0,
            empty_slot_color,
            full_slot_color
          )
        sampled_df_text <-
          ifelse(nrow(x@phases[[i]]@waves[[j]]@sampled_data) == 0,
            ">",
            paste(
              "<br/> df with",
              as.character(nrow(x@phases[[i]]@waves[[j]]@
              sampled_data)),
              "obs. of",
              as.character(ncol(x@phases[[i]]@waves[[j]]@
              sampled_data)),
              "vars>"
            )
          )
        data_color <-
          ifelse(length(x@phases[[i]]@waves[[j]]@data) == 0,
            empty_slot_color,
            full_slot_color
          )

        data_text <-
          ifelse(nrow(x@phases[[i]]@waves[[j]]@data) == 0,
            ">",
            paste(
              "<br/> df with",
              as.character(nrow(x@phases[[i]]@waves[[j]]@data)),
              "obs. of",
              as.character(ncol(x@phases[[i]]@waves[[j]]@data)),
              "vars>"
            )
          )

        recs[[(j - 1) * 6 + start + 2]] <- paste(metadata_rec, md_text,
                                                 md_color)
        recs[[(j - 1) * 6 + start + 3]] <- paste(design_rec, des_text,
                                                 des_color)
        recs[[(j - 1) * 6 + start + 4]] <- paste(
          samples_rec, samples_text,
          samples_color
        )
        recs[[(j - 1) * 6 + start + 5]] <- paste(
          sampled_data_rec,
          sampled_df_text,
          sampled_df_color
        )
        recs[[(j - 1) * 6 + start + 6]] <- paste(data_rec, data_text,
                                                 data_color)
      }
    }
    recs_vec <- unlist(recs)
    for (i in seq_along(recs_vec)) {
      recs_vec[i] <- paste0("rec", as.character(i + 5), " ", recs_vec[i])
    }
  } else {
    recs_vec <- c(" ")
  }

  # Write edge definitions. Phase 1 is already coded in default
  if (n_phases > 1) {
    edges <- list()

    # Connect phase headers to overall object
    phases_recs_indices <- which(grepl("Phase", recs_vec))
    phases_recs <- recs_vec[grepl("Phase", recs_vec)]
    phases_recs <- sub(" .*", "", phases_recs)
    recs_vec_short <- sub(" .*", "", recs_vec)
    edges[[1]] <- paste("rec1 ->", paste(phases_recs, collapse = " "))

    # Connect phase headers to wave headers and metadata
    for (i in 2:n_phases) {
      waves_recs_indices <- which(grepl("Wave", recs_vec))
      waves_recs_indices <- waves_recs_indices[
        waves_recs_indices %in% c((n_waves_prev[i - 1] * 6 + 2 * (i - 1)):
        (n_waves[i] * 6 + 2 * (i - 1)))
      ]

      edges[[i]] <- paste(
        phases_recs[i - 1], "->", "{",
        paste(recs_vec_short[waves_recs_indices[1] - 1],
          recs_vec_short[waves_recs_indices],
          collapse = " "
        ),
        "}"
      )
    }

    # connect wave parts to wave
    for (i in 2:n_phases) {
      waves_recs_indices <- which(grepl("Wave", recs_vec))
      waves_recs_indices <- waves_recs_indices[
        waves_recs_indices %in% c((n_waves_prev[i - 1] * 6 + 2 * (i - 1)):
        (n_waves[i] * 6 + 2 * (i - 1)))
      ]
      edges[[n_phases + (i - 1)]] <- paste(paste(
        "{",
        recs_vec_short
        [waves_recs_indices],
        "} -> {"
      ))
      wave_edges <- list()
      for (j in seq_along(waves_recs_indices)) {
        edges[[n_phases + n_waves_prev[i - 1] + j]] <-
          paste(
            recs_vec_short[waves_recs_indices[j]],
            "-> {",
            recs_vec_short[waves_recs_indices[j] + 1],
            recs_vec_short[waves_recs_indices[j] + 2],
            recs_vec_short[waves_recs_indices[j] + 3],
            recs_vec_short[waves_recs_indices[j] + 4],
            recs_vec_short[waves_recs_indices[j] + 5],
            "}"
          )
      }
    }

    # Combine into vec
    edges_vec <- unlist(edges)
  }

  # Use glue to write text input to be passed to Diagrammer::GrViz
  diagram <- glue::glue("digraph {{
                  graph [layout = dot, rankdir = LR]
                  node [shape = rectangle, fixedsize= true, width = 4.5,
                  height = 1.5, fontname = Helvetica, fontsize = 20]
                  {title_char}
                  {default_rec}
                  {recs_vec}
                  # edge definitions with node ids
                  rec1 -> rec2
                  rec1 -> rec3 -> {{rec4 rec5}}
                  {edges_vec}
                  }}", title_char  = paste(title_char, collapse = " "),
                  default_rec = paste(default_rec, collapse = " "),
                  recs_vec = paste(recs_vec, collapse = " "),
                  edges_vec =  paste(edges_vec, collapse = " "))

  # Make diagram
  output <- DiagrammeR::grViz(diagram, width = width, height = height)

  # Print
  return(output)
}
