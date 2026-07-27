#' Launch the Leggett+ Shiny application
#'
#' @return Invisibly returns `NULL`. Called for its side effect of starting a
#'   Shiny application.
#' @export
#' @examples
#' if (interactive()) {
#'   launch_leggett_app()
#' }
launch_leggett_app <- function() {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required to run the app. Please install it.")
  }
  app_dir <- system.file("shiny-examples/LeggettApp", package = "LeggettPlus")
  shiny::runApp(app_dir, display.mode = "normal")
}
