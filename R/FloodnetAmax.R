#' Estimate flood quantiles using annual maxima
#'
#' Return the flood quantile, standard deviation,
#' of an at-site frequency analysis of floods based on annual
#' maxima.
#'
#' @param x Hydrometric data. Vector of annual maxima.
#'
#' @param period Return period for which the flood quantiles are estimated.
#'
#' @param distr Distribution. One of \code{'gev'},\code{'gno'},\code{'glo'}
#'   or \code{'pe3'}
#'
#' @param nsim Number of bootstrap samples used for inference.
#'
#' @param level Confidence level.
#'
#' @param out.model Logical. Should the model be output. This corresponds to the
#'   output of \link{FitAmax}. Otherwise only the estimated flood quantiles are
#'   returned.
#'
#' @param verbose Logical. Should message and warnings be returned.
#'
#' @details
#'
#' Estimation is carried out by L-moments and a best distribution is
#' selected using Akaike Information Criterion (AIC) among the
#' Generalized Extreme Value (GEV), Generalized logistic (GLO), the
#' Generalized Normal (GNO) and the Pearson type 3 distributions.
#' A preference is given to the GEV when the AIC of the other distributions is not
#' greater than at least two AIC.
#' Inference on the estimated flood quantiles is performed by parametric bootstraps.
#'
#' If \code{verbose == TRUE}, a Mann-Kendall and a Pettitt's test are used
#' to verify the presence of trend and change points in the data.
#' If the data fails one of the them at significance level 0.05,
#' a warning is issued.
#'
#' @seealso \link{AmaxData}, \link{floodnetMdl}.
#'
#' @references
#'
#' Helsel, D. R., & Hirsch, R. M. (2002). Statistical Methods in Water Resources.
#'    In Techniques of Water-Resources Investigations of the United States
#'    Geological Survey.
#'    Retrieved from http://water.usgs.gov/pubs/twri/twri4a3/
#'
#' Hosking, J. R. M., & Wallis, J. R. (1997). Regional frequency analysis:
#'   an approach based on L-moments. Cambridge Univ Pr.
#'
#' @import CSHShydRology
#' @export
#'
#' @examples
#'
#'  an <- DemoData('amax')
#'  fit <- FloodnetAmax(an, period = c(20,50))
#'  summary(fit)
#'
#'
FloodnetAmax <- function(
	  x,
	  period = c(2,5,10,20,50,100),
	  distr = NULL,
	  nsim = 2000,
	  level = 0.95,
	  out.model = FALSE,
	  verbose = TRUE){

	MINSIM <- 500
	x <- as.data.frame(x)
	alpha <- 1 - level
	period.p <- 1 - 1 / period

	####################################
	## Verify the structure of the input
	####################################
	msg <- 'The input must be in a proper format.'

 	if(ncol(x) != 3)
 		stop(msg)

	if(!methods::is(x[,2],'Date'))
		stop(msg)

	if(length(unique(x[,1])) != 1 )
		stop(msg)

	colnames(x) <- c('site','date','value')
	site <- as.character(x[1,1])

  ## remove missing
	xd <- stats::na.omit(x[,3])
	nac <- stats::na.action(xd)

	if(!is.null(nac)){
		dd <- x[-nac,2]
	} else {
		dd <- x[,2]
	}

	xd <- as.numeric(xd)

	###########################
	## Trend test
	###########################

	mk <- Kendall::MannKendall(xd)$sl
	pt <- trend::pettitt.test(xd)$p.value

	##Sort the observation
	oid <- order(xd)
	xd <- xd[oid]
	dd <- dd[oid]

	############################################
	## Print warnings
	############################################

	if(verbose){

		if(length(xd) < 20)
		  warning('\nThere is less than 20 observations.')

	  if(mk < 0.05){
		  warning('\nThere may be a trend in the data.')
	  }

	  if(pt < 0.05){
  	   warning('\nThere may be a change point in the data.')
	  }

	  if(nsim < MINSIM)
	  	warning('\nBootstrap of the flood quantiles will not be performed with ',
	  				  'less than ', MINSIM ,' simulations.')

	}

	############################################
	## Fitting model
	############################################

	## Fit the distribution
	if(is.null(distr))
	  distr <- c('gev','glo','gno', 'pe3')

	fit <- FitAmax(xd, distr = distr, method = 'lmom', varcov = TRUE,
								 nsim = MINSIM, tol.gev = 2)

	## GOF test
	ad <- .adtest(xd, CSHShydRology::pAmax, fit$para, fit$distr, verbose = verbose)

	if(nsim >= MINSIM){
  	hat <- predict(fit, p = period.p, ci = 'boot', alpha = alpha, nsim = nsim,
	  							 out.matrix = TRUE)

  	para.se <- apply(hat$para, 2, sd)

  	qua <- data.frame(pred = hat$pred[,1],
  											se = apply(hat$qua, 2, sd),
  											hat$pred[,2:3])

  } else{
	  qua <- predict(fit, p = period.p, se = TRUE, ci = 'delta', alpha = alpha)

	  para.se <- diag(fit$varcov)
  }

	para <- data.frame(param = fit$para, se = para.se)
	rownames(para) <- names(fit$para)

	##############################################
	## Pre-compute data for the return level plots
	##############################################

	nobs <- length(xd)

	Fz <- function(z) -log(-log(z))
	zmin <- 1/(nobs+1)
	zmax <- max(nobs/(nobs+1), max(period.p))

	rlevel.p <- seq(Fz(zmin),Fz(zmax), len = 30)
	rlevel.p <- exp(-exp(-rlevel.p))

	rlevel <- cbind(prob = rlevel.p,
									predict(fit, p = rlevel.p, ci = 'delta', alpha = alpha))

	############################################
	## Build the final output
	############################################

	ans <- list(site = site,
							method = 'amax',
							distr  = fit$distr,
							period = period,
							quantile = qua,
							param = para,
							obs = xd,
							time = dd,
							rlevels = rlevel,
							thresh = 0,
							ppy = 1,
							trend = c(mk,pt),
							gof = ad)

	class(ans) <- 'floodnetMdl'

	if(out.model)
		ans$fit <- fit

	return(ans)
}

################################################################
## Wrapping of the function ad.test
## Earlier version were not allowing argument "estimated = TRUE"
##
.adtest <- function(x, qfun, ..., verbose = TRUE){
  ans <- try(goftest::ad.test(x, qfun, ..., estimated = TRUE), silent = TRUE)

	if(methods::is(ans, 'try-error')){

		if(verbose)
			warning('Goodness-of-fit test was performed without considering parameter estimation')

		ans <- goftest::ad.test(x, CSHShydRology::pAmax, ...)
	}

  return(ans$p.value)
}
