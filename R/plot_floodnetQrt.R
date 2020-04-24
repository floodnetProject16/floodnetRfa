#' @export
#' @import ggplot2
#' @rdname floodnetGraphics
plot.floodnetRoi <- function(x, type = 'f', ...){

	if(type == 'h'){
		plt <- hist.floodnetRoi(x, ...)

	} else if(type == 'f'){
		plt <- .QrtFit(x, ...)

	} else if(type == 'r'){
		plt <- .QrtFit(x, ..., res = TRUE)

	} else if(type == 'cv'){
		plt <- .QrtCv(x, ...)

	}

	return(plt)
}

######################################################################
## Graphic of the at-site versus fitted
.QrtFit <- function(x, point.args = NULL, abline.args = NULL,
										xlab = 'At-site quantiles (log)',
										ylab = NULL, res = FALSE){

	xd <- log(x$data)

	if(res)
		xd[,2] <- xd[,1] - xd[,2]

	plt <- ggplot(xd, aes(x = .data$.data$atsite, y = .data$.data$pred))

	## Add points
	if(is.null(point.args))
		point.args <- list()

	if(is.null(point.args$size))
		point.args$size <- 2

	plt <- plt + do.call(geom_point, point.args)

	## Add the line
	if(is.null(abline.args))
		abline.args <- list()

	if(is.null(abline.args$colour))
		abline.args$colour <- '#f8766d'

	if(is.null(abline.args$size))
		abline.args$size <- 2

	if(res){
		abline.args$slope <- 0
	} else{
		abline.args$slope <- 1
	}

	abline.args$intercept <- 0

	plt <- plt + do.call(geom_abline, abline.args)

	## Add axis labels

	if(is.null(ylab)){
		ylab <- ifelse(res, 'Residuals (log)', 'Predicted quantiles (log)')
	}

	plt <- plt + xlab(xlab) + ylab(ylab)

	return(plt)
}

.QrtCv <- function(x, point.args = NULL, line.args = NULL,
										xlab = 'Pooling group size',
										ylab1 = 'Skill - MAD (log)', ylab2 = 'Nash-Sutcliffe (log)',
									  colour = ''){

	## Verify that cross-validation was performed on several candidates
	if(is.null(x$cv))
		stop('Missing cross validation information.')

	## transform criterion to a commun scale
	skl.mean <- mean(x$cv[,3])
	skl.sd <- sd(x$cv[,3])
	nsh.mean <- mean(x$cv[,2])
	nsh.sd <- sd(x$cv[,2])

	Fscale <- function(z) (z-nsh.mean) * skl.sd / nsh.sd + skl.mean
	Finv <- function(z) (z-skl.mean) * nsh.sd / skl.sd + nsh.mean

	## Melt data
	xd <- expand.grid(size = x$cv$size, Criterion = c('Skill', 'Nash') )
	xd <- data.frame(xd, value = c(x$cv[,3], Fscale(x$cv[,2])))

	## Create plot
	plt <- ggplot(xd, aes(x = .data$size, y = .data$value, colour = .data$Criterion))

	## Add points
	if(is.null(point.args))
		point.args <- list()

	plt <- plt + do.call(geom_point, point.args)

	## Add points
	if(is.null(line.args))
		line.args <- list()

	plt <- plt + do.call(geom_line, line.args)

	## Create a second axis
	plt <- plt + scale_y_continuous(ylab1, sec.axis = sec_axis(~ Finv(.), name = ylab2))

	plt <- plt + labs(x = xlab, colour = colour)

	return(plt)
}



