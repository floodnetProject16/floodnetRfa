#' Floodnet App
#'
#' A simple function used to start launch a Graphical User Interface (GUI)
#' inside a the default web browser.
#' Alternatively, if a path is passed in argument it create a script that will
#' launch the GUI from the system terminal.
#'
#' @param shortcut File path of the launching script.
#'
#' @import shiny
#' @import shinydashboard
#' @import shinyjs
#' @import shinyFiles
#' @import DT
#' @import floodnetRfa
#' @import gridExtra
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'   FloodnetApp()
#'
#'   #On Windows
#'   floodnetApp("C:/Users/JohnDoe/Desktop/FloodnetApp.cmd")
#'
#'   #On Unix
#'   floodnetApp("/home/JohnDoe/Desktop/FloodnetApp.sh")
#' }
FloodnetApp <- function(shortcut=NULL){

	if(!is.null(shortcut)){

		rbin <- ifelse(.Platform$OS.type == "windows", "Rscript.exe", "Rscript")
		rbin <- normalizePath(paste(R.home("bin"), rbin, sep = '/'))
		script <- paste0('call "', rbin, '" -e "floodnetRfa::FloodnetApp()"')

		f <- file(shortcut)
		writeLines(script, f)
		close(f)

	} else {
	  ## Start the shinyApp
		shiny::runApp(appDir = system.file('shinyApp/app.R', package = 'floodnetRfa'), launch.browser = TRUE)

	}
}

