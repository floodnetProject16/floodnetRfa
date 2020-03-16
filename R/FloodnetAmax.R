#' Estimate flood quantiles using using annual maxima
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
#' @param instant Logical, should the instantaneous peaks be used.
#'
#' @param nsim Number of bootstrap samples used for inference.
#'
#' @param level Confidence level.
#'
#' @param out.model Logical. Should the model be output. This correspond to the
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
#' greater than at least two.
#' Inference on the estimated flood quantiles is performed by parametric bootstrap.
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
#' @import CSHShydRology stats
#' @export
#'
#' @examples
#'
#' \dontrun{
#' 	## Path the HYDAT database
#'  db <- DB_HYDAT
#'
#'  ## Read Amax data
#'  x <- AmaxData('01AD002', db)
#'
#'  ## Performing the analysis
#'  FloodnetAmax(x, period = c(20,50), nsim = 200, verbose = FALSE)
#' }
#'
FloodnetAmax <- function(x, ...){

	msg <- 'Must provide the input x in a proper format.'

	if(class(x) == 'numeric'){
		return(.FloodnetAmaxOne(x, ...))

	} else if(class(x) %in% c('matrix','data.frame')){

		## Verify the structure of the input
  	if(all(colnames(x) != c('site','year','value')))
  		stop(msg)

		nsite <- length(unique(x[,1]))

		if(nsite == 1){
			return(.FloodnetAmaxOne(x[,3], ..., site = as.character(x[1,1])))
		}

		## Perform the FFA to every sites
		xlst <- split(x[,3], as.character(x[,1]))
		flst <- lapply(xlst, .FloodnetAmaxOne, ...)

		## Set the site ID
		for(ii in seq(nsite))
			flst[[ii]]$site <- names(xlst)[ii]

		## Return the result in the form of table
		flst <- lapply(flst, as.data.frame)

		ans <- do.call(rbind, flst)
		rownames(ans) <- NULL

		return(ans)

	} else{
  	stop(msg)
  }

}

.FloodnetAmaxOne <- function(x,
					 period = c(2,5,10,20,50,100),
					 distr = NULL,
					 instant = FALSE,
					 nsim = 2000,
					 level = 0.95,
					 out.model = FALSE,
					 verbose = TRUE,
					 site = 'site'){

	MINSIM <- 200
	alpha = 1 - level

	## Probabilities associated with the flood quantiles
  period.p <- 1 - 1 / period

	xd <- as.numeric(na.omit(x))

	############################################
	## Perform verification
	############################################

	if(verbose){

		if(length(xd) < 20)
		  warning('\nThere is less than 20 observations.')

	   mk <- Kendall::MannKendall(xd)$sl

	  if(mk < 0.05){
	  	is.trend <- TRUE
		  warning('\nThere may be a trend in the data.')
	  }

	   pt <- trend::pettitt.test(xd)$p.value

	  if(pt < 0.05){
	  	is.trend <- TRUE
  	   warning('\nThere may be a change point in the data.')
	  }

	  if(nsim < MINSIM)
	  	warning('\n Bootstrap of the flood quantiles will not be performed for',
	  				  'less than ', MINSIM ,' simulations.')

	}

	############################################
	## Fitting
	############################################

	## Fit the distribution
	if(is.null(distr))
	  distr <- c('gev','glo','gno', 'pe3')

	fit <- FitAmax(xd, distr = distr, method = 'lmom', varcov = TRUE,
								 nsim = MINSIM, tol.gev = 2)

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
							quantile = qua,
							param = para,
							method = 'amax',
							distr  = fit$distr,
							thresh = 0,
							ppy = 1,
							period = period,
							rlevels = rlevel,
							obs = sort(xd))

	class(ans) <- 'floodnetMdl'

	if(out.model)
		ans$fit <- fit

	return(ans)
}

