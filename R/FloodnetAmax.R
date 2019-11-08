#' Estimate flood quantiles using using annual maxima
#'
#' Return the results of an at-site frequency analysis of floods based on annual
#' maxima. Estimation is carried out by L-moments and a best distribution is
#' selected using Akaike Information Criterion (AIC) among the
#' Generalized Extreme Value (GEV), Generalized logistic (GLO), the
#' Generalized Normal (GNO) and the Pearson type 3 distributions. A preference is
#' given to the GEV when the AIC of the other distributions is not
#' greater than at least two points.
#' Inference on the estimated flood quantiles is performed by parametric bootstrap.
#'
#' @author Martin Durocher <mduroche@@uwaterloo.ca>
#'
#' @param period Return period for which the flood quantiles are estimated.
#'
#' @param x Path of a CSV file or data.frame.
#'
#' @param site Station ID for HYDAT or a station name for identification.
#'
#' @param db Path of the HYDAT database.
#'
#' @param maximum Logical. Should the maxima (or minima) be modeled.
#'
#' @param instant Logical, should the instantaneous peaks be used.
#'
#' @param nsim Number of bootstrap samples used for inference.
#'
#' @param alpha Probability outside the confidence interval.
#'
#' @param out.model Logical. Should the model be output. This correspond to the
#'   output of \link{FitAmax}. Otherwise only the estimated flood quantiles are
#'   returned.
#'
#' @param verbose Logical. Should message and warnings be returned.
#'
#' @details
#'
#' If `x` is a path, it is assumed to point to a CSV file, otherwise
#' it must be a data.frame.
#' In both cases, there must be at least one column
#' called `Max` (or `Min` if low flow are modeled).
#' The name of each column is not case sensitive.
#' On the website of the Water Survey of Canada, it is suggested to
#' download the the Annual Extremes Data.
#'
#' @export
#'
#' @import CSHShydRology
#'
#' @examples
#'
#' \dontrun{
#' ## Assuming that the HYDAT database was already downloaded
#' db <- "/pathToDB/HYDAT.sqlite"
#' FloodnetAmax(period = c(20,50), site = '01AD002', db = db)
#' }
#'
FloodnetAmax <-
	function(period = c(2,5,10,20,50,100),
					 x = NULL,
					 site = NULL,
					 db = NULL,
					 distr = NULL,
					 instant = FALSE,
					 nsim = 2000,
					 alpha = 0.05,
					 out.model = FALSE,
					 verbose = TRUE){

	## Probability associate with the flood quantiles
  p <- 1-1/period

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

	hat <- predict(fit, p = p, ci = 'boot', alpha = alpha, nsim = nsim,
								 out.matrix = TRUE)

	ans <-
		replicate(4,
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

	ans[[2]]$value <- apply(hat$qua, 2, sd)
	ans[[3]]$value <- hat$pred[,2]
	ans[[3]]$value <- hat$pred[,3]

	ans <- do.call(rbind,ans)

	if(out.model)
		ans <- list(fit = fit, qua = ans)

	return(ans)
}
