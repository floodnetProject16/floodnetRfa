#' Floodnet Plotting functions.
#'
#' Generate diagnostic plots based on the output of a modeling function
#' \link{floodnetMdl}). Includes the following graphics: Return level plot,
#' comparison of the flood quantiles, comparison of the coefficient of
#' variation, L-moment ratio diagram and histogram.#'
#' @name floodnetGraphics
#'
#' @param x Input data.
#'
#' @param type Plot type. \code{'r'} - Return level, \code{'q'} - compare quantile,
#'   \code{'cv'} - compare cv, \code{'l'} - L-moment ratio.
#'
#' @param xlab,ylab,fill,shape,colour Labels of the XY-axis or legend.
#'
#' @param caption Logical.
#'   Should additional information be displayed in the caption.
#'
#' @param point.args,line.args,histogram.args,ribbon.args,
#'   Parameters passed to the respective geometry.
#'
#' @param average.args Same as point.args for the average point.
#'
#' @param ... Other parameters.
#'
#' @seealso \link[ggplot2]{ggplot}, \link[ggplot2]{geom_point},
#'   \link[ggplot2]{geom_path}, \link[ggplot2]{geom_ribbon}
#'
#' @details
#'
#' All functions use \code{ggplot2} to create a basic plot that can be further
#' customized. The default arguments of the used geometry can be overridden by
#' passing a list of new arguments. For example,
#' \code{point.args = list(colour = 'red')}
#' change the colour of a set of points that was created by \code{geom_point}.
#'
#'
#' @export
#' @import ggplot2
#'
#' @examples
#'
#' library(ggplot2)
#'
#' ## Create a random data
#' set.seed(2)
#' xd <- SequenceData(365*30, freq = 'days', sdate = '2000-01-01')
#' an <- CSHShydRology::ExtractAmax(value~date,xd)
#'
#' ## Return level Plot
#' fit.an <- an$value %>%
#'   SequenceData() %>%
#'   FloodnetAmax()
#'
#' plot(fit.an)
#'
#' # QQ-plot
#' fit.pot <- FloodnetPot(xd, area = 200, u = 200)
#' plot(fit.pot, type = 'qq')
#'
#' ## Histogram
#' hist(fit.pot)
#'
#' ## Sen's slope
#' plot(fit.an, 't')
#'
#' ## Comparing flood quantiles and coefficient of variation
#' m2 <- CompareModels(fit.an, fit.pot)
#' plot(m2) + labs(fill = 'Method')
#' plot(m2, type = 'cv')
#'
#' ## L-moment ratio diagram
#' DemoData('region') %>%
#'   FloodnetPool(target = '01AF007', tol.H = Inf, verbose = FALSE) %>%
#'   plot(type = 'l') + labs(shape = 'Site', colour = 'Distr.')
#'
#'
plot.floodnetMdl <- function(x, type = 'r', ...){

	arg <- list(...)
	arg$x <- x

	arg <- lapply(arg, .CorrectColorArgs)

	f <- switch(type,
							"h" = hist.floodnetMdl,
							"qq" = .PlotQQ,
							"r" = .PlotReturnLevel,
							"l" = .PlotLRatio,
							"t" = .PlotLinTrend)


	return(do.call(f, arg))
}

#' @export
#' @import ggplot2
#' @rdname floodnetGraphics
plot.floodnetMdls <- function(x, type = 'q', ...){

	arg <- list(...)
	arg$x <- x

	arg <- lapply(arg, .CorrectColorArgs)

	f <- switch(type,
							"q" = .PlotCompareQ ,
							"cv" = .PlotCompareCv)

	return(do.call(f, arg))
}

##############################################################################
#' @rdname floodnetGraphics
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
	pts <- data.frame(z = Fz(seq(nobs)/(nobs+1)), obs = sort(x$obs))

	xbreaks <- c(1.2, 1.5, 2, 5,10,20,50,100,200,500,1000,2000,5000,10000)
	zbreaks <- Fz(1 - 1 / xbreaks)


	###########################################
	## Create the graphic
	###########################################

	plt <- ggplot()

	## Add confidence intervals

	if(is.null(ribbon.args))
		ribbon.args <- list()

	if(is.null(ribbon.args$fill))
		ribbon.args$fill <- '#969696'

	if(is.null(ribbon.args$alpha))
		ribbon.args$alpha <- 0.3

	ribbon.args$data = xd
	ribbon.args$mapping = aes(x = .data$z, ymin = .data$lower, ymax = .data$upper)

	plt <- plt + do.call(geom_ribbon, ribbon.args)

	## Add observations
	if(is.null(point.args))
		point.args <- list()

	if(is.null(point.args$colour))
	  point.args$colour <- 'black'

	point.args$data <- pts
	point.args$mapping <- aes(x = .data$z, y = .data$obs)

	plt <- plt + do.call(geom_point,point.args)

	## Add return level curve

	if(is.null(line.args))
	  line.args <- list()

	if(is.null(line.args$colour))
		line.args$colour <- '#f8766d'

	if(is.null(line.args$size))
		line.args$size <- 1.25

	line.args$data <- xd
	line.args$mapping <- aes(x = .data$z, y = .data$pred)

	plt <- plt + do.call(geom_line, line.args)

	## Customize the xy-axis
	plt <- plt +
			scale_x_continuous(xlab, breaks = zbreaks, labels = xbreaks) +
			scale_y_continuous(ylab)

	return(plt)
	}

##############################################################################
#' @rdname floodnetGraphics
.PlotQQ <-
	function(x, point.args = NULL,
					 line.args = NULL,
					 ribbon.args = NULL,
					 xlab = 'Theoretical quantiles',
					 ylab = 'Sample quantiles'){

	xd <- x$rlevels

	## Extrat the proper quantile function
	if(x$method == 'amax'){
		qf <- function(z) CSHShydRology::qAmax(z, x$param[,1], x$distr)

	} else if(x$method == 'pot'){
		qf <- function(z) CSHShydRology::qgpa(z, x$param[1,1], x$param[2,1])

	} else if(x$method == 'pool_amax'){
		qf <- function(z) CSHShydRology::qAmax(z, x$param[-1,1], x$distr) * x$param[1,1]

	} else if(x$method == 'pool_pot'){
		qf <- function(z) {
			q <- CSHShydRology::qgpa(z, x$param[3,1], x$param[4,1])
			return(q * x$param[1,1] + x$thresh)
		}
	}

	## Evaluate the sample and theoretical quantiles
	nobs <- length(x$obs)
	pobs <- seq_along(x$obs) / (nobs+1)

  xd$z <- qf(xd$prob)
	pts <- data.frame(obs = sort(x$obs), z = qf(pobs))

	## Remove extra space on the right.
	## For The return level plot, we wanted that predicted return period be
	## present, not here
	bid <- min(which(xd$prob > max(pobs)) + 1, nrow(xd))
	xd <- xd[seq(bid),]

	###########################################
	## Create the graphic
	###########################################

	plt <- ggplot()

	## Add confidence intervals

	if(is.null(ribbon.args))
		ribbon.args <- list()

	if(is.null(ribbon.args$fill))
		ribbon.args$fill <- '#969696'

	if(is.null(ribbon.args$alpha))
		ribbon.args$alpha <- 0.3

	ribbon.args$data <- xd
	ribbon.args$mapping <- aes(x = .data$z, ymin = .data$lower, ymax = .data$upper)

	plt <- plt + do.call(geom_ribbon, ribbon.args)

	## Add observations
	if(is.null(point.args))
		point.args <- list()

	if(is.null(point.args$colour))
	  point.args$colour <- 'black'

	point.args$data <- pts
	point.args$mapping <- aes(x = .data$z, y = .data$obs)

	plt <- plt + do.call(geom_point,point.args)

	## Add return level curve

	if(is.null(line.args))
	  line.args <- list()

	if(is.null(line.args$colour))
		line.args$colour <- '#f8766d'

	if(is.null(line.args$size))
		line.args$size <- 1.25

	line.args$data <- xd
	line.args$mapping <- aes(x = .data$z, y = .data$pred)

	plt <- plt + do.call(geom_line, line.args)

	## Customize the xy-axis
	xl <- range(pts$z)
	xl <- xl + diff(xl) * c(-0.05, 0.05)

	yl <- range(x$obs)
	yl <- yl + diff(xl) * c(-0.05, 0.05)

	plt <- plt + xlab(xlab) + ylab(ylab)

	return(plt)
}

################################################################################
#' @rdname floodnetGraphics
.PlotCompareQ <-
	function(x,
					 xlab = 'Return periods',
					 ylab = 'Return levels',
					 fill = ''){

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
	plt <- ggplot(data = qua, aes(x = .data$x, y = .data$value, fill = .data$model)) +
		geom_crossbar(aes(x = .data$x, ymin = .data$lb, ymax = .data$ub)) +
		scale_x_continuous(xlab, breaks = seq_along(per),
											 labels = per) +
		scale_y_continuous(ylab) + labs(fill = fill)

	return(plt)
}

################################################################################
#' @rdname floodnetGraphics
.PlotCompareCv <-
	function(x,
					 line.args = NULL,
					 point.args = NULL,
					 xlab = 'Return periods',
					 ylab = 'Coefficient of variation',
					 colour = ''){

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
		line.args = list()

	if(is.null(line.args$size))
		line.args$size <- 1.25

	if(is.null(point.args))
		point.args = list()

	if(is.null(point.args$size))
		point.args$size <- 2.5

	## Make the graph
	plt <- ggplot(data = cv, aes(x = .data$x, y = .data$value, col = .data$model))

	plt <- plt + do.call(geom_line, line.args)
	plt <- plt + do.call(geom_point, point.args)

	plt <- plt +
		scale_x_continuous(xlab, breaks = seq_along(per), labels = per) +
		scale_y_continuous(ylab) +
		labs(colour = colour)

	return(plt)
}

###############################################################################
#' @rdname floodnetGraphics
.PlotLRatio <-
	function(x, point.args = NULL,
					 average.args = NULL,
					 line.args = NULL,
					 xlab = 'L-Skewness',
					 ylab = 'L-Kurtosis',
					 colour = '',
					 shape = ''){

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

	plt <- ggplot(data = theo, aes( x = .data$x, y = .data$y, colour = .data$distrib))

	## Add distribution curves

	if(is.null(line.args))
		line.args = list()

	if(is.null(line.args$size))
		line.args$size <- 1

	plt <- plt + do.call(geom_line, line.args)

	## Add points
	if(is.null(point.args))
		point.args = list()

	if(is.null(point.args$colour))
		point.args$colour <- 'black'

	## Add neighbor's L-moments
	point.args$data <- x$lmom[-1,]
	point.args$mapping <- aes(x = .data$LSK, y = .data$LKUR)

	plt <- plt + do.call(geom_point, point.args)

	## Add target and avarage L-moments

	if(is.null(average.args))
		average.args <- list()

	if(is.null(average.args$colour))
		average.args$colour <- '#d73027'

	if(is.null(average.args$size))
		average.args$size <- 2

	average.args$data <- rlmom
	average.args$mapping <- aes(x = .data$LSK, y = .data$LKUR, shape = .data$site)

	plt <- plt + do.call(geom_point, average.args)

	plt <- plt + labs(x= xlab, y = ylab, shape = shape, colour = colour)

	return(plt)

}

##############################################################################
#' @rdname floodnetGraphics
.PlotLinTrend <-
	function(x, point.args = NULL,
					 line.args = NULL, xlab = NULL, ylab = NULL, caption = TRUE){


	xd.pt <- data.frame(Time = x$time, Observations = x$obs)
	xd.slp <- data.frame(x = range(x$time),
										 	 y = .SenSlope(x$time, x$obs)[3:4])

	plt <- ggplot()

	if(is.null(point.args))
		points.args <- list()

	point.args$data <- xd.pt
	point.args$mapping <- aes(x = .data$Time, y = .data$Observations)

	## Add Sen's slope
	if(is.null(line.args))
	  line.args <- list()

	if(is.null(line.args$colour))
		line.args$colour <- '#f8766d'

	if(is.null(line.args$size))
		line.args$size <- 1.25

	line.args$data <- xd.slp
	line.args$mapping <- aes(x = .data$x, y = .data$y)

	plt <- plt + do.call(geom_point, point.args)
	plt <- plt + do.call(geom_line, line.args)


	# Set axis label
	if(is.null(xlab))
		xlab <- 'Time'

	if(is.null(ylab))
		ylab <- 'Observations'

	plt <- plt + xlab(xlab) + ylab(ylab)

	## Add p-values message if needed
	if(caption){

		if(x$method %in% c('amax','pool_amax')){
			trend.msg <- paste0('p-value : Mann-Kendall (', round(x$trend[1], 3),
											 '), Pettitt (', round(x$trend[2], 3), ')')

		} else {
			trend.msg <- paste0('p-value : Mann-Kendall (', round(x$trend[1], 3),
											 '), Logistic (', round(x$trend[2], 3), ')')
		}

		plt <- plt + labs(caption = trend.msg)
	}

	return(plt)
}

############################################################################
.SenSlope <- function(x,y){

	x <- as.numeric(x)

	## Extract all combinations
	cid <- utils::combn(length(y),2)
	xc <- matrix(x[cid], nrow = 2)
	yc <- matrix(y[cid], nrow = 2)

	## Estimate formula y = m * x + b
	m <- median((yc[2,] - yc[1,]) / (xc[2,] - xc[1,]))
	b <- mean(y - m * x)

	# Evaluate the first and last point of the slope
	y = m * range(x) + b

	return(c(slope = m, intercept = b, y0 = y[1], y1 = y[2]))
}

###############################################################################
## ggplot accept the argument syntax color or colour.
## This function convert the color arguments into colour
.CorrectColorArgs <- function(l){

	if(is.list(l)){
		if(!is.null(l$color)){
	 	  l$colour <- l$color
	  	l$color <- NULL
		}
	}

	return(l)
}
