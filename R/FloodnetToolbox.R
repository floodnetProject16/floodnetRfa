#' Run Shiny Toolbox GUI
#'
#' A simple function used to run the Shiny GUI from the floodnetRfa package isntallation
#'
#' @return None
#' @export
#'
#' @examples
#' FloodnetToolbox()
FloodnetToolbox <- function() {

	###################################
	## Start the shinyApp
	###################################

	shiny::runApp(appDir = system.file('shinyApp/app.R', package = 'floodnetRfa'))
}
