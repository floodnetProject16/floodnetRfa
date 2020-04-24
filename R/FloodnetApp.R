#' Floodnet App
#'
#' A simple function used to run the Shiny GUI from the floodnetRfa package
#'
#' @return None
#' @export
#'
#' @examples
#'
#' \dontrun{
#'   FloodnetToolbox()
#' }
FloodnetApp <- function(){

	NULL
  ## Start the shinyApp
	shiny::runApp(appDir = system.file('shinyApp/app.R', package = 'floodnetRfa'), launch.browser = TRUE)
}

