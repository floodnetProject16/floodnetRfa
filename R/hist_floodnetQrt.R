#' @export
#' @import ggplot2
hist.floodnetRoi <-
	function(x, histogram.args = NULL,
					 line.args = NULL,
					 xlab = 'Residuals (log)',
					 ylab = 'Density', ...){

	## Create the plot
	xd <- data.frame(res = log(x$data[,1]) - log(x$data[,2]))

	plt  <- ggplot(xd, aes(x = .data$res))

	## Add the bars
	if(is.null(histogram.args))
		histogram.args <- list()

	if(is.null(histogram.args$fill))
		histogram.args$fill <- '#969696'

	if(is.null(histogram.args$colour))
		histogram.args$colour <- '#525252'

	## Sturges rule
	if(is.null(histogram.args$bins))
		histogram.args$bins <- round(1 + 3.322 * log(length(x)))

	histogram.args$mapping = aes(y = .data$..density..)

	plt <- plt + do.call(geom_histogram, histogram.args)

  ## Add the density
	if(is.null(line.args))
		line.args <- list()

	if(is.null(line.args$colour))
		line.args$colour <- '#f8766d'

	if(is.null(line.args$size))
		line.args$size <- 1.25

	mu <- mean(xd$res)
	sig <- sd(xd$res)
	line.args$fun = function(z) dnorm(z, mean = mu, sd = sig)

	plt <- plt + do.call(stat_function, line.args)

	plt <- plt + xlab(xlab) + ylab(ylab)

	return(plt)
}
