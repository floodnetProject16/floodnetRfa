#' Pool frequency analysis using annual maximum floods
#'
#' Return the results of a regional flood frequency analysis based on annual
#' maxima and pooling groups.
#' Estimation is carried out by the L-moment algoritms.
#'
#'
#' @author Martin Durocher <mduroche@@uwaterloo.ca>
#'
#' @param period Return periods for which the flood quantiles are estimated.
#'
#' @param target Station ID for HYDAT.
#'
#' @param sites List of station ID for HYDAT that may be used
#'  for the regional analysis.
#'
#' @param db Path of the HYDAT database.
#'
#' @param x Path of a CSV file or data.frame containing the flow data.
#'
#' @param distr Regional distribution.
#'
#' @param size Initial size of the pooling groups.
#'
#' @param nsim Number of bootstrap samples used for inference.
#'
#' @param alpha Probability outside the confidence interval.
#'
#' @param out.model Logical. Should the model be output. This correspond to the
#'   output of \link{FitRegLmom}. Otherwise only the estimated flood quantiles are
#'   returned.
#'
#' @param verbose Logical. Should message and warnings be returned.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' ## Assuming that the HYDAT database is available
#' db <- "/pathToDB/HYDAT.sqlite"
#' FloodnetPoolAmax(period = c(20,50), target = '01AD002', db = db)
#' }
#'
FloodnetPoolAmax <-
	function(period = c(2,5,10,20,50,100),
					 target,
					 sites = NULL,
					 db = NULL,
					 x = NULL,
					 distr = NULL,
					 size = 25,
					 tol.H = 2,
					 nsim = 2000,
					 alpha = 0.05,
					 out.model = FALSE,
					 verbose = TRUE){


  #######################################
  ## Reading hydrometric data
  #######################################

  ## ERROR in input
  if(is.null(db) & is.null(x))
		stop('Must provide an input data.')

  ## Using HYDAT
  if(!is.null(db)){

  	if(is.null(sites))
  		stop('A list of sites must be provided.')

		if(verbose)
		  cat('\n[Reading HYDAT database]')

  	## open a connection to the database
	  con <- RSQLite::dbConnect(RSQLite::SQLite(), db)

	  ## Extract the Annual maxima
    xd <- HYDAT::AnnualPeakData(con, get_flow = TRUE, sites)
    xd <- xd[xd$peak == 'MAXIMUM', ]
    xd$date <- with(xd, as.Date(paste(year,month,day,sep = '/')))

    ## Standardize the dataset format
    xd <- xd[,c(1, 8, 6, 3)]
    colnames(xd) <- c('site','date', 'value', 'year')

    RSQLite::dbDisconnect(con)

  } else {

    xd <- na.omit(as.data.frame(x))

    if(ncol(xd) != 3)
    	stop('Wrong number of columns in the input')

    colnames(xd) <- c('site','date','value')

    if(class(xd$date) != 'Date')
    	stop('The second column of x must be a Date')

    xd$year <- as.integer(format(xd$date, '%Y'))

    ## Order
    xd <- xd[order(xd[,1], xd[,2]), ]

    ## Verify that the target is in the data
    sites <- unique(xd$site)
    if(!(target %in% sites))
    	stop('The target must be in the provided sites')

  }

  xd <- na.omit(xd)

  #######################################
  ## forming pooling groups
  #######################################
  if(verbose)
  	cat('\n[Forming the pooling group]')

  ## Compute seasonal distances
  season <- SeasonStat(date ~ site, xd)
  season.dist <- DistSeason(radius~angle,season)
  colnames(season.dist) <- rownames(season.dist) <- rownames(season)

  ## Transform data to a wide format
  xd <- DataWide(value ~ site + year, xd)

  ## Find an initial pooling group
  xd <- FindNearest(xd, season.dist[target,  colnames(season.dist)], n = size)

  #####################################
  ## Estimate flood quantile
  #####################################

 	## Fit a regional model and verified homogeneity
  suppressWarnings(fit <- try(FitRegLmom(xd, distr = distr), silent = TRUE))

  if(class(fit) == 'try-error')
  	stop('Error in fitting the regional model')

  ## If the distribution is passed, keep.it fixed in the updating process
 	distr.fix <- ifelse(is.null(distr), FALSE, TRUE)

 	## Update the pooling group base on its heterogeneity
  suppressWarnings(
  	fit <- PoolRemove(fit, verbose = FALSE, nmin = 1, tol = tol.H,
  										ntot.min = 5 * max(period),  distr.fix = distr.fix))

  if(verbose){

  	## Verify heterogenous measure H
  	het <- fit$stat[1]

  	if(het > 2)
  		warning('The heterogenous measure for the pooling group is H > 2.')

  }

  if(verbose)
  	cat('\n[Estimating the flood quantile]\n')

  ## Evaluate the average intersite-correlation
  pool.sites <- rownames(fit$lmom)
  icor <- Intersite(xd[,pool.sites])$para

  ## predict flood quantile
  p <- 1-1/period
  hat <- predict(fit, p,  corr = icor, ci = TRUE, nsim = nsim, alpha = alpha)

  ans <-
		replicate(4,
	    data.frame(site = target,
			  			   method = 'pool_amax',
				  		   distribution = fit$distr,
					  	   period = period,
						     variable = 'quantile',
						     value = hat[,1]),
	    simplify = FALSE)

	ans[[2]]$variable <- 'se'
	ans[[3]]$variable <- 'lower'
	ans[[4]]$variable <- 'upper'

	ans[[2]]$value <- hat[,2]
	ans[[3]]$value <- hat[,3]
	ans[[4]]$value <- hat[,4]

	ans <- do.call(rbind,ans)
	rownames(ans) <- NULL

	if(out.model)
		ans <- list(fit = fit, qua = ans)

	return(ans)

}
