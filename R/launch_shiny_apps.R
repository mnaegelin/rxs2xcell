#' @import shiny
#' @import bslib
#' @export
#'
# dependencies:
# full: shiny, bslib,
# expicit: plotly, DT, dplyr, tidyr, ggplot2, stringr, shinyalert, sass, shinyjs
launch_coverage_app <- function() {
  app_dir <- system.file("shiny_YTE", package = "rxs2xcell")
  if (app_dir == "") {
    stop("Could not find the Shiny app directory. Reinstall the package.")
  }
  #pkgload::load_all(app_dir)
  shiny::runApp(app_dir)
}

#' @export
launch_metadata_app <- function() {
  app_dir <- system.file("shiny_meta", package = "rxs2xcell")
  if (app_dir == "") {
    stop("Could not find the Shiny app directory. Reinstall the package.")
  }
  shiny::runApp(app_dir)
}
