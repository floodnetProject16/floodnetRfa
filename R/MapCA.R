#' Simple map of Canada
#'
#' Return a simple map of Canada using the grammar of graphics \code{ggplot2}.
#'
#' @param polygon.args Parameters of the displayed polygons.
#'   See \link[ggplot2]{geom_polygon}.
#'
#' @param xlab,ylab Labels of the axes.
#'
#' @param ... Other parameters passed to \link[ggplot2]{ggplot}.
#'
#' @seealso \link{floodnetGraphics}, \link[ggplot2]{geom_polygon}.
#'
#' @import ggplot2
#' @export
#'
#' @examples
#'
#' library(ggplot2)
#'
#' MapCA(polygon.args = list(fill = 'white', colour = 'black'))
#'
#' xd <- DemoData('descriptor')
#'
#' MapCA() +
#'   geom_point(data = xd, aes(x = lon, y = lat), colour = 'orange') +
#'   coord_cartesian(xlim = c(-90,-50), ylim = c(42,60))
#'
MapCA <- function(..., polygon.args = NULL,
									xlab = 'Longitude', ylab = 'Latitude'){

	## Create a base map
	plt <- ggplot(...)

	if(is.null(polygon.args))
		polygon.args <- list(fill = '#737373')

	polygon.args$data <- get("map_ca")
	polygon.args$mapping <- aes(x = .data$long, y = .data$lat, group = .data$group)

	plt <- plt + do.call(geom_polygon, polygon.args)
	plt <- plt + xlab(xlab) + ylab(ylab)

	return(plt)
}
