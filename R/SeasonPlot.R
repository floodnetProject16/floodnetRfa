#' @export
#' @rdname floodnetGraphics
SeasonPlot <- function(... , xlab = 'Timing', ylab = 'Regularity'){

	## Starting of the month in radian
  month.theta <- c(0.00,0.53,1.02,1.55,2.07,2.60,3.12,3.65,4.18,4.70,5.23,5.75)

  plt <- ggplot(...) +
  	coord_polar(theta = 'x', start = -pi/2, direction = -1) +
		scale_x_continuous(xlab, breaks = month.theta, labels = month.abb, limits = c(0, 2*pi)) +
		scale_y_continuous(ylab, limits = c(0,1))

	return(plt)
}
