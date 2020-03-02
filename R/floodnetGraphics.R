#' Plotting functions for floodnet outputs.
#'
#' Generate diagnostic plots based on the output of a floodnet function
#' \link{floodnetMdl}). Includes the following graphics: Return level plot,
#' comparison of the flood quantiles, comparison of the coefficient of
#' variation, L-moment ratio diagram and histogram, seasonal plot and
#' a basic map of Canada.
#'
#' @name floodnetGraphics
#'
#' @param x Input data.
#'
#' @param type Plot type. \code{'r'} - Return level, \code{'q'} - compare quantile,
#'   \code{'cv'} - compare cv, \code{'l'} - L-moment ratio.
#'
#' @param ... Other arguments pass to the respective geometry of \code{ggplot2}.
#'   Must be a list of arguments.
#'
#' @seealso \link[ggplot2]{ggplot}, \link[ggplot2]{geom_point},
#'   \link[ggplot2]{geom_path}, \link[ggplot2]{geom_ribbon}
#'
#' @details
#'
#' All functions use \code{ggplot2} to create a basic plot that can be further
#' customized. The default arguments of the used geometry can be overide by
#' passing a list of new arguments. For example,
#' \code{point.args = list(colour = 'red')}
#' change the color of a set of points that was created by \code{geom_point}.
#'
#' The main graphcis are produced by the method \code{plot} (and \code{hist}).
#' The function \code{SeasonPlot} and \code{MapCA} create basic canvas for
#' creating a seasonality plot or a map of Canada.
#'
#' @examples
#'
#' \dontrun{
#' x <- rnorm(10)
#'
#' }
#'
#'
#' @export
#' @import ggplot2
plot.floodnetMdl <- function(x, type = 'r', ...){

	if(type == 'r'){
		plt <- .PlotReturnLevel(x, ...)

	} else if(type == 'q'){
		plt <- .PlotCompareQ(x)

	} else if(type == 'cv'){
		plt <- .PlotCompareCv(x, ...)

	} else if(type == 'l'){
		plt <- .PlotLRatio(x, ...)
	}

	return(plt)
}

#' @export
#' @import ggplot2
#' @rdname floodnetGraphics
plot.floodnetMdls <- function(x, type = 'q', ...){

	if(type == 'q'){
		plt <- .PlotCompareQ(x, ...)

	} else if(type == 'cv'){
		plt <- .PlotCompareCv(x, ...)

	}

	return(plt)
}

##############################################################################
.PlotReturnLevel <-
	function(x, point.args = NULL,
					 line.args = NULL,
					 ribbon.args = NULL,
					 xlab = 'Return periods',
					 ylab = 'Return levels'){

	xd <- x$rlevels

	Fz <- function(z) -log(-log(z))
	xd$z <- Fz(xd$prob)

	nobs <- length(x$obs)
	pts <- data.frame(z = Fz(seq(nobs)/(nobs+1)), obs = x$obs)

	xbreaks <- c(1.2, 1.5, 2, 5,10,20,50,100,200,500,1000,2000,5000,10000)
	zbreaks <- Fz(1 - 1 / xbreaks)


	###########################################
	## Create the graphic
	###########################################

	plt <- ggplot()

	## Add confidence intervals

	if(is.null(ribbon.args))
		ribbon.args <- list(fill = '#969696', alpha = 0.3)

	ribbon.args$data = xd
	ribbon.args$mapping = aes(x = z, ymin = lower, ymax = upper)

	plt <- plt + do.call(geom_ribbon, ribbon.args)

	## Add observations
	if(is.null(point.args))
	  point.args <- list(colour = 'black')

	point.args$data = pts
	point.args$mapping = aes(x = z, y = obs)

	plt <- plt + do.call(geom_point,point.args)

	## Add return level curve

	if(is.null(line.args))
	  line.args <- list(colour = '#f8766d', size = 1.25)

	line.args$data = xd
	line.args$mapping = aes(x = z, y = pred)

	plt <- plt + do.call(geom_line, line.args)

	## Customize the xy-axis
	plt <- plt +
			scale_x_continuous(xlab, breaks = zbreaks, labels = xbreaks) +
			scale_y_continuous(ylab)

	return(plt)
}

################################################################################
.PlotCompareQ <-
	function(x,
					 xlab = 'Return periods',
					 ylab = 'Return levels'){

	## Verify input type
	if(!is.list(x))
		stop('The input x must be a list of fitted models.')

	## Limit the plot to the comparison of 4 models
	nmdl <- length(x)
	if(nmdl > 4){
		warning('Only the first four models will be compared.')
		x <- x[1:4]
	}

	# Merge all the results in one table
	xd <- as.data.frame(x)

	## Create name for the compared models
	xd$model <- paste(xd$method, xd$distribution, sep = '_')

	if(length(unique(xd$model)) < length(x))
		stop('All models must be unique (site + method + distr).')

	## Pivot the table
	xd <- xd[order(xd$variable,xd$model, xd$period),]

	qua <- xd[xd$variable == 'quantile' ,c('model','period','value')]
	lb <- xd[xd$variable == 'lower' , 'value']
	ub <- xd[xd$variable == 'upper' , 'value']

	qua <- cbind(qua, lb = lb, ub = ub)

	## determine position in the x-axis
	Fclass <- function(z) as.integer(as.factor(z))
	delta <- Fclass(qua$model)/(nmdl+1)
	delta <- delta - min(delta) - diff(range(delta))/2

	qua$x <- Fclass(qua$period) + delta

	per <- unique(qua$period)

	## Return a plot of the flood quantile qith error bars.
	plt <- ggplot(data = qua, aes(x = x, y = value, fill = model)) +
		geom_crossbar(aes(x = x, ymin = lb, ymax = ub)) +
		scale_x_continuous(xlab, breaks = seq_along(per),
											 labels = per) +
		scale_y_continuous(ylab)

	return(plt)
}

################################################################################
.PlotCompareCv <-
	function(x,
					 line.args = NULL,
					 point.args = NULL,
					 xlab = 'Return periods',
					 ylab = 'Coefficient of variation'){

	## Verify input type
	if(!is.list(x))
		stop('The input x must be a list of fitted models.')

	## Limit the plot to the comparison of 4 models
	nmdl <- length(x)
	if(nmdl > 4){
		warning('Only the first four model will be compared.')
		x <- x[1:4]
	}

	## Merge all the results in one table
	xd <- as.data.frame(x)

	## Create name for the compared models
	xd$model <- paste(xd$method, xd$distribution, sep = '_')

	if(length(unique(xd$model)) < length(x))
		stop('All models mustbe unique (site + method + distr).')

	## Pivot the table
	xd <- xd[order(xd$variable, xd$model, xd$period),]

	cv <- xd[xd$variable == 'se' , c('model','period','value')]
	cv$value <- cv$value / xd[xd$variable == 'quantile' , 'value']
	cv$x <- as.integer(as.factor(cv$period))

	per <- unique(cv$period)

	## Set default parameters
	if(is.null(line.args))
		line.args <- list(size = 1.25)

	if(is.null(point.args))
		point.args <- list(size = 2.5)

	## Make the graph
	plt <- ggplot(data = cv, aes(x = x, y = value, col = model))

	plt <- plt + do.call(geom_line, line.args)
	plt <- plt + do.call(geom_point, point.args)

	plt <- plt +
		scale_x_continuous(xlab, breaks = seq_along(per), labels = per) +
		scale_y_continuous(ylab)

	return(plt)
}

###############################################################################
.PlotLRatio <-
	function(x, point.args = NULL,
					 average.args = NULL,
					 line.args = NULL,
					 xlab = 'L-Skewness',
					 ylab = 'L-Kurtosis'){

	if(x$method != 'pool_amax')
		stop('Must be an AMAX regional model.')

	## Determine the theoricial curve of the common distribution
	theo.lsk <- seq(min(x$lmom[,4]), max(x$lmom[,4]), len = 50)
	theo.curve <- CSHShydRology::LmomDiag(x = theo.lsk, plot = FALSE,
																				distr = c('gev','glo','gno','pe3'))

	## Melt the table
	theo <- expand.grid(x = theo.lsk, distrib = colnames(theo.curve))
	theo <- data.frame(theo, y = theo.curve[seq_along(theo.curve)])

	## Regional L-moments
	rlmom <- apply(x$lmom[,4:5], 2, weighted.mean, x$lmom[,1])
	rlmom <- data.frame( LSK = c(x$lmom[1,4], rlmom[1]),
											 LKUR = c(x$lmom[1,5], rlmom[2]),
											 site = c('Target','Average'))

	## Make the graph

	plt <- ggplot(data = theo, aes( x = x, y = y, colour = distrib))

	## Add distribution curves

	if(is.null(line.args)){
		line.args = list(size = 1)
	}

	plt <- plt + do.call(geom_line, line.args)

	if(is.null(point.args)){
		point.args = list(colour = 'black')
	}

	## Add neighbor's L-moments
	point.args$data <- x$lmom[-1,]
	point.args$mapping <- aes(x = LSK, y = LKUR)

	plt <- plt + do.call(geom_point, point.args)

	## Add target and avarage L-moments

	if(is.null(average.args)){
		average.args = list(colour = '#d73027', size = 2)
	}

	average.args$data <- rlmom
	average.args$mapping <- aes(x = LSK, y = LKUR, shape = site)

	plt <- plt + do.call(geom_point, average.args)

	plt <- plt + xlab(xlab) + ylab(ylab)

	return(plt)

}


