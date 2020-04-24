###############################################################################
#' Floodnet model output
#'
#' All Floodnet modeling functions create a standardized model output that can be used
#' for printing results or generating graphics.
#'
#' @name floodnetMdl
#'
#' @param x,object Output of a modeling function. Such as \link{FloodnetAmax}.
#'
#' @param type Type of output to be returned:
#'   parameter \code{'p'} or quantile \code{'q'}.
#'
#' @param print.it Logical. Should the results be displayed.
#'
#' @param row.names Optional vector of names for the rows.
#'
#' @param optional Logical. Should \link[base]{make.names} be applied to row names.
#'
#' @param ... Other parameters.
#'
#' @seealso \link{FloodnetAmax},\link{FloodnetPot}, \link{FloodnetPool},
#'   \link{floodnetGraphics}, \link{hist.floodnetMdl}.
#'
#' @export
#'
summary.floodnetMdl <- function(object, ...){

	print(object)

	cat('\n\nQuantiles:\n')
	print(as.matrix(object$quantile), digits = 4)

	cat('\nParameters:\n')
	print(as.matrix(object$param), digits = 4)

}

#' @export
#' @rdname floodnetMdl
summary.floodnetMdls <- function(object, ...){
	lapply(object, summary)

}
#' @export
#' @rdname floodnetMdl
print.floodnetMdl <- function(x, ...){

	cat('\nFlood Frequency Analysis\n')
	cat('\nMethod:', x$method)

	if(length(x$site) == 1){
		cat('\nSite:', x$site)
	} else {
		cat('\nNb. site:', length(x$site))
	}

	cat('\nDistribution:', x$distr)
	cat('\nReturn Period:', x$period)
}

#' @export
#' @rdname floodnetMdl
print.floodnetRoi <- function(x, ...){

	cat('\nPredictions at ungauged sites\n')
	cat('\nMethod:', x$method)

	if(length(x$site) == 1){
		cat('\nSite:', x$site)
	} else {
		cat('\nNb. site:', length(x$site))
	}

	cat('\nPool size:', x$size)
	cat('\nReturn Period:', x$period,'\n')

	cat('\nQuantiles:\n')
	print(x$quantile, digits = 4)

}

#' @export
#' @rdname floodnetMdl
summary.floodnetRoi <- function(object, print.it = TRUE, ...){

	## Compute various residuals
	res <- object$data[,1] - object$data[,2]
	rres <- 1 - object$data[,2] / object$data[,1]

	lx <- log(object$data)
	lres <- lx[,1] - lx[,2]

	stat <- data.frame(rmse = sqrt(mean(res^2)),
										 mad = mean(abs(res)),
										 rrmse = sqrt(mean(rres^2)),
										 rmad = mean(abs(rres)),
										 lmse = sqrt(mean(lres^2)),
										 lmad = mean(abs(lres)),
										 nash = 1-sum(lres^2)/sum((lx[,1]-mean(lx[,1]))^2),
										 skill = 1-sum(abs(lres))/sum(abs(lx[,1]-mean(lx[,1]))))
	if(print.it){

		if(!is.null(object$cv)){

			cv <- object$cv
			colnames(cv) <- c('size','nash','skill')

			cat('\nBest Cross Validation Scores:\n')
			nprint <- pmin(5, nrow(cv))
			sid <- order(cv$skill, decreasing = TRUE)[1:nprint]
			print(cv[sid,], digits = 4)
		}

		cat('\nCross Validation Scores:\n')
		print(stat, digits = 4)
	}

	invisible(stat)
}

