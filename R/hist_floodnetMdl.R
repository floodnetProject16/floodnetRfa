#' @export
#' @import ggplot2
#' @rdname floodnetGraphics
hist.floodnetMdl <-
	function(x, histogram.args = NULL,
					 line.args = NULL,
					 xlab = NULL,
					 ylab = 'Density'){

	# Select customize axis depending of the method
	if(x$method == 'amax'){
		xd <- data.frame(obs = x$obs)
		sa.form <- sec_axis(~ . )
		bnd <- NULL
		obs.lab <- 'Observations'
		Fdens <- function(z) CSHShydRology::dAmax(z, x$param[,1], x$distr)

	} else if(x$method == 'pool_amax'){
		xd <- data.frame(obs = x$obs/x$param[1,1])
		bnd <- NULL
		sa.form <- sec_axis(~ . * x$param[1,1], name = '(original)')
		obs.lab <- 'Observations (scaled)'
		Fdens <- function(z) CSHShydRology::dAmax(z, x$param[-1,1], x$distr)

	} else if(x$method == 'pot'){
		bnd <- 0
		xd <- data.frame(obs = x$obs - x$thresh)
		sa.form <- sec_axis(~ . + x$thresh, name = '(original)')
		obs.lab <- 'Observations (centered)'
		Fdens <- function(z) CSHShydRology::dgpa(z, x$param[1,1], x$param[2,1])

	} else if(x$method == 'pool_pot'){
		xd <- data.frame(obs = (x$obs - x$thresh)/x$param[1,1])
		bnd <- 0
		sa.form <- sec_axis(~ . * x$param[1,1] + x$thresh, name = '(original)')
		obs.lab <- 'Observations (centered-scaled)'
		Fdens <- function(z) CSHShydRology::dgpa(z, x$param[3,1], x$param[4,1])

	}

	if(!is.null(xlab))
		obs.lab <- xlab

	## Create the plot
	plt  <- ggplot(xd, aes(x = obs))

	## Add the bars
	if(is.null(histogram.args))
		histogram.args = list(bins = 10, fill = '#969696', colour = '#525252')

	histogram.args$mapping = aes(y = ..density..)
	histogram.args$boundary = bnd

	plt <- plt + do.call(geom_histogram, histogram.args)

  ## Add the density
	if(is.null(line.args))
		line.args <- list(colour = '#f8766d', size = 1.25)

	line.args$fun = Fdens

	plt <- plt + do.call(stat_function, line.args)

	plt <- plt +
		scale_y_continuous(ylab) +
		scale_x_continuous(obs.lab, sec.axis = sa.form)

	return(plt)
}
