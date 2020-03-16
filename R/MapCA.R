#' @export
#' @rdname floodnetGraphics
MapCA <- function(..., polygon.args = NULL){

	## Create a base map
	plt <- ggplot(...)

	if(is.null(polygon.args))
		polygon.args <- list(fill = '#737373')

	polygon.args$data <- map_ca
	polygon.args$mapping <- aes(x = long, y = lat, group = group)

	plt <- plt + do.call(geom_polygon, polygon.args)

	return(plt)
}
