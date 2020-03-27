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

  ## Start the shinyApp
	runApp(appDir = system.file('shinyApp/app.R', package = 'floodnetRfa'))
}

