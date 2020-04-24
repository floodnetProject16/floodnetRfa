#' Seasonality plot
#'
#' Create a canvas to represent the timing and the regularity of seasonal measures
#' in a polar coordinate system.
#'
#' @param xlab,ylab Axis labels.
#'
#' @param ... Other parameters passed to \link{ggplot}.
#'
#' @import ggplot2
#' @export
#'
#' @examples
#'
#' library(CSHShydRology)
#' library(ggplot2)
#'
#' ## Basic plot
#'
#' SeasonPlot()
#'
#' ## Plot with points
#'
#' xd <- DemoData('region')
#' stat.season <- as.data.frame(SeasonStat(date~site, xd))
#'
#' SeasonPlot() +
#'   geom_point(data = stat.season, aes(x = angle, y = radius),
#'              colour = 'blue') +
#'   theme_bw()
#'
#'
SeasonPlot <- function(... , xlab = 'Timing', ylab = 'Regularity'){

	## Starting of the month in radian
  month.theta <- c(0.00,0.53,1.02,1.55,2.07,2.60,3.12,3.65,4.18,4.70,5.23,5.75)

  plt <- ggplot(...) +
  	coord_polar(theta = 'x', start = -pi/2, direction = -1) +
		scale_x_continuous(xlab, breaks = month.theta, labels = month.abb, limits = c(0, 2*pi)) +
		scale_y_continuous(ylab, limits = c(0,1))

	return(plt)
}
