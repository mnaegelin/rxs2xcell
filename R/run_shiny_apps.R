run_coverage_app <- function() {
  app_dir <- system.file("shiny_YTE", package = "rxs2xcell")
  if (app_dir == "") {
    stop("Could not find the Shiny app directory. Reinstall the package.")
  }
  shiny::runApp(app_dir)
}

run_metadata_app <- function() {
  app_dir <- system.file("shiny_meta", package = "rxs2xcell")
  if (app_dir == "") {
    stop("Could not find the Shiny app directory. Reinstall the package.")
  }
  shiny::runApp(app_dir)
}
