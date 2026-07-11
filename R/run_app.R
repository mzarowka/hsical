#' Run the hsical Shiny App
#'
#' Launches the hyperspectral image calibration tool.
#'
#' @param ... Additional arguments passed to [shiny::runApp()]
#'
#' @return No return value, called for side effects (launches Shiny app)
#' @export
#'
#' @examples
#' if (interactive()) {
#'   run_app()
#' }
run_app <- function(...) {
  app_dir <- system.file("app", package = "hsical")

  if (app_dir == "") {
    cli::cli_abort(
      "Could not find the app directory. Try reinstalling {.pkg hsical}."
    )
  }

  shiny::runApp(app_dir, ...)
}
