#' Visualize Strata
#'
#' Launches an R Shiny application locally. This app can be used to interactively split strata and determine how the results affect optimum allocation of a fixed number of samples.
#' @export
optimall_shiny <- function() {
  appDir <- system.file("shiny-app", "optimall_shiny", "thoroughapp.R", package = "optimall")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `optimall`.", call. = FALSE)
    }
  shiny::runApp(appDir, display.mode = "normal")
}
