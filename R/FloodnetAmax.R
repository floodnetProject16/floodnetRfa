#' Estimate flood quantiles using using annual maxima
#'
#' Return the flood quantile, standard deviation,
#' of an at-site frequency analysis of floods based on annual
#' maxima.
#'
#' @param site Station ID for HYDAT or a station name for identification.
#'
#' @param db Path of the HYDAT database.
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
#' @seealso \link{AmaxData}, \link{FloodnetPot} and \link{FloodnetPool}.
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
#' ## Assuming that the HYDAT database was already downloaded
#' db <- "/pathToDB/HYDAT.sqlite"
#' FloodnetAmax(site = '01AD002', db = db, period = c(20,50))
#' }
#'
FloodnetAmax <-
	function(site = NULL,
					 db = NULL,
					 x = NULL,
					 period = c(2,5,10,20,50,100),
					 distr = NULL,
					 instant = FALSE,
					 nsim = 2000,
					 level = 0.95,
					 out.model = FALSE,
					 verbose = TRUE){

	alpha = 1- level

	## Probabilities associated with the flood quantiles
  period.p <- 1-1/period

	############################################
	## Reading the data
	############################################

	## ERROR in input
  if(is.null(db) & is.null(x))
		stop('Must provide an input data.')

  ## Using HYDAT
  if(!is.null(db)){

		if(verbose)
		  cat('\n[Reading HYDAT database]')

		## open a connection to the database
	  con <- RSQLite::dbConnect(RSQLite::SQLite(), db)

	  ## Extract the Annual maxima
	  if(instant){
	 	  xd <- HYDAT::AnnualInstantaneousPeakData(con, get_flow = TRUE, site)
	 	  xd <- xd[xd$peak_code == 'MAXIMUM', 'peak']
	  } else {
      xd <- HYDAT::AnnualPeakData(con, get_flow = TRUE, site)
      xd <- xd[xd$peak == 'MAXIMUM', 'value']
	  }

    RSQLite::dbDisconnect(con)

	## USING adata.frame
	} else {

		xd <- as.numeric(x)

    ## If the user have not specify a station id
    if(is.null(site))
      site <- 'site'
  }

	xd <- na.omit(xd)

	############################################
	## Perform verification
	############################################

	if(verbose){

		if(length(xd) < 20)
		  warning('\nThere is less than 20 observations.')

	   mk <- Kendall::MannKendall(xd)$sl

	  if(mk < 0.05)
		  warning('\nThere may be a trend in the data.')

	   pt <- trend::pettitt.test(xd)$p.value

	  if(pt < 0.05)
	   warning('\nThere may be a change point in the data.')
	}

	############################################
	## Fitting and prediction
	############################################

	if(verbose)
	  cat('\n[Estimating the parameters of the best distribution]')

	## Fit the distribution
	if(is.null(distr))
	  distr <- c('gev','glo','gno', 'pe3')

	fit <- FitAmax(xd, distr = distr, method = 'lmom',
								 varcov = out.model, tol.gev = 2)

	if(verbose)
	  cat('\n[Estimating the flood quantiles (bootstrap)]\n')

	if(nsim > 1){
  	hat <- predict(fit, p = period.p, ci = 'boot', alpha = alpha, nsim = nsim,
	  							 out.matrix = TRUE)

  	ans <- replicate(4,
	    data.frame(site = site,
			  			   method = 'amax',
				  		   distribution = fit$distr,
					  	   period = period,
						     variable = 'quantile',
						     value= hat$pred[,1]),
	    simplify = FALSE)

	  ans[[2]]$variable <- 'se'
	  ans[[3]]$variable <- 'lower'
	  ans[[4]]$variable <- 'upper'

	  ans[[2]]$value <- apply(hat$qua,2,sd)
	  ans[[3]]$value <- hat$pred[,2]
	  ans[[4]]$value <- hat$pred[,3]

	  ans <- do.call(rbind,ans)

  } else{
	  hat <- predict(fit, p = period.p)

	  ans <- data.frame(site = site,
			  			   method = 'amax',
				  		   distribution = fit$distr,
					  	   period = period,
						     variable = 'quantile',
						     value= hat)
	}


	if(out.model)
		ans <- list(fit = fit, qua = ans)

	return(ans)
}
