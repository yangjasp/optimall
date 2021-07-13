#' Visualize Strata
#'
#' Launches an R Shiny application locally. This app can be used to
#' interactively split strata and determine how the results affect
#' optimum allocation of a fixed number of samples.
#' @param ... Optional arguments to pass to \code{shiny::runApp}.
#' \code{display.mode} is already set to normal.
#' @return Launches an R Shiny application locally.
#' @export
optimall_shiny <- function(...) {
  appDir <- system.file("shiny-app", "optimall_shiny",
    "app_draft_4.R",
    package = "optimall"
  )
  if (appDir == "") {
    stop("Could not find directory.
         Try re-installing `optimall`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal", ...)
}
