#' Simple map of Canada
#'
#' Return a simple map of Canada using the grammar of graphics \code{ggplot2}.
#'
#' @param polygon.args Parameters of the displayed polygons.
#'   See \link[ggplot2]{geom_polygon}.
#'
#' @param ... Other parameters passed to \link[ggplot2]{ggplot}.
#'
#' @seealso \link{floodnetGraphics}, \link[ggplot2]{geom_polygon}.
#'
#' @export
#'
#' @examples
#'
#' library(ggplot2)
#'
#' MapCA() +
#'   geom_point(data = gaugedSites, aes(x = lon, y = lat), colour = 'orange')
#'
MapCA <- function(..., polygon.args = NULL){

	## Create a base map
	plt <- ggplot(...)

	if(is.null(polygon.args))
		polygon.args <- list(fill = '#737373')

	polygon.args$data <- get("map_ca")
	polygon.args$mapping <- aes(x = .data$long, y = .data$lat, group = .data$group)

	plt <- plt + do.call(geom_polygon, polygon.args)

	return(plt)
}
