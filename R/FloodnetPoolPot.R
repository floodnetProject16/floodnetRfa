#' Pool frequency analysis using peaks over threshold
#'
#' Return the results of a regional flood frequency analysis based on
#' peaks over threshold and pooling groups.
#' Estimation is carried out by the L-moment algoritms.
#'
#' @author Martin Durocher <mduroche@@uwaterloo.ca>
#'
#' @param period Return periods for which the flood quantiles are estimated.
#'
#' @param target Station ID for HYDAT.
#'
#' @param sites List of station ID for HYDAT that may be used
#'  for the regional analysis. Must be a data.frame with three columns.
#'  Respectively:  site, threshold and drainage area.
#'
#' @param db Path of the HYDAT database.
#'
#' @param x Path of a CSV file or data.frame containing the flow data.
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
#' @import CSHShydRology
#' @export
#'
#' @examples
#'
#' \dontrun{
#' ## Assuming that the HYDAT database is available
#' db <- "/pathToDB/HYDAT.sqlite"
#' FloodnetPoolPot(period = c(20,50), target = '01AD002', db = db)
#' }
#'
FloodnetPoolPot <-
	function(period = c(2,5,10,20,50,100),
					 target,
					 sites,
					 db = NULL,
					 x = NULL,
					 nsim = 2000,
					 alpha = 0.05,
					 size = 25,
					 tol.H = 2,
					 tol.year = 346,
					 out.model = FALSE,
					 verbose = TRUE){

	## order by sites
	sites <- sites[order(sites[,1]),]
	sname <- as.character(sites[,1])

	if(ncol(sites) != 3)
		stop('The threshold and the drainage area must be provided for each site.')

  #######################################
  ## Reading annual data
  #######################################

  if(is.null(db) & is.null(x))
		stop('Must provide an input data.')

  if(!is.null(db)){

	  ## Extract the Annual maxima
	  con <- RSQLite::dbConnect(RSQLite::SQLite(), db)
    an <- HYDAT::AnnualPeakData(con, get_flow = TRUE, sname)
    RSQLite::dbDisconnect(con)

    ## Standardize the dataset format
    an <- an[an$peak == 'MAXIMUM', ]
    an$date <- with(an, as.Date(paste(year,month,day,sep = '/')))
    an <- an[ ,c(1, 8, 6, 3)]
    colnames(an) <- c('station','date', 'value', 'year')

    an <- na.omit(an)

  } else {

  	## Sort and format the input
    xd <- na.omit(data.frame(x))

    if(ncol(xd) != 3)
    	stop('Wrong number of columns in the input')

    colnames(xd) <- c('station','date','value')

    ## Verify that the date format
    if(class(xd$date) != 'Date')
    	stop('The second column of x must be a Date')

    ## Verify the correspondance in the list of sites
    xstation <- unique(xd$station)
    cond1 <- all(xstation %in% sname)
    cond2 <- all(sname %in% xstation)

    if(!cond1 | !cond2)
    	stop('The list of sites does not match those of the daily data.')

    xd <- xd[order(xd[,1], xd[,2]), ]

    an <- ExtractAmax(value ~ station + date, xd, ylab = 'year', tol = tol.year)

  }

  #################################
  ## Finding the pooling group
  #################################

  if(verbose)
  	cat('\n[Forming the pooling group]')

  ## Compute seasonal distances
  season <- SeasonStat(date ~ station, an)

  season.dist <- DistSeason(radius~angle,season)
  colnames(season.dist) <- rownames(season.dist) <- rownames(season)

  ## Find an initial pooling
  pool.id <- sort(order(season.dist[target, ])[1:size])
  sites <- sites[pool.id,]

  #######################################
  ## Extracting the peaks
  #######################################
  if(verbose)
  	cat('\n[Extracting the peaks]')

  if(is.null(db)){

  	## Filter the pooling groups
  	xd <- xd[xd$station %in% sites$station, ]
    out <- ExtractPeaks(xd, sites, tol = tol.year, sorted = TRUE)

  } else{
    out <- DailyPeaksData(db, sites, tol = tol.year)
  }

  ## Transform data to a wide format
  colnames(out$peaks) <- c('station','date','value')
  xw <- DataWide(value ~ station, out$peaks, row.names = FALSE)

  ## make sure the target is the first columns in xw
  cname <- unique(c(target,colnames(xw)))
  xw <- xw[,cname]

  #######################################
  ## Fit the model and verify homogeneity
  #######################################
  if(verbose)
  	cat('\n[Verifying homogeneity]')

  ## Fit a regional model and verified homogeneity
  fit <- FitRegLmom(xw, type = 'pot')
  fit <- PoolRemove(fit, method = 'H1', tol = tol.H,
  									ntot.min = 5 * max(period) , nmin = 1, distr.fix = TRUE,
  									verbose = FALSE)

  if(verbose){

  	## Verify heterogenous measure H
    het <- fit$stat[1]

    if(het > 2)
    	warning('The heterogenous measure for the pooling group is H > 2.')
  }


  if(verbose)
  	cat('\n[Estimating flood quantiles]')

  # Rate of peaks per years
  ppy <- sum(is.finite(xw[,1]))/out$nyear[target]
  prb <- 1 - 1/(ppy * period)
  hat <- predict(fit, prb,  corr = 0, ci = TRUE, nsim = nsim, alpha = alpha)

  ## Reformat the output
  ans <-
		replicate(4,
	    data.frame(site = target,
			  			   method = 'pool_pot',
				  		   distribution = 'gpa',
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





