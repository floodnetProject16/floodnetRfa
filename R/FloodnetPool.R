#' Pool frequency analysis using annual maximum floods
#'
#' Return the results of a regional flood frequency analysis based on annual
#' maxima and pooling groups.
#'
#' @param x Hydrometric data of the form: site, year, value.
#'
#' @param target Station ID of the target.
#'
#' @param type Type of regional models to be fitted.
#'   See \link[CSHShydRology]{FitPoolMle}.
#'
#' @param period Return periods for which the flood quantiles are estimated.
#'
#' @param distr Regional distribution.
#'
#' @param tol.H Heterogeneity measure. Stopping criterion representing the
#'    minimal heterogeneity level accepted for a pooling group.
#'
#' @param nsim Number of bootstrap samples used for inference.
#'
#' @param alpha Probability outside the confidence interval.
#'
#' @param corr Coefficient of correlation used for simulation.
#'   The same value is assumed between each pair of sites.
#'
#' @param out.model Logical. Should the model be output. This correspond to the
#'   output of \link{FitRegLmom}. Otherwise only the estimated flood quantiles are
#'   returned.
#'
#' @param verbose Logical. Should message and warnings be returned.
#'
#' @details
#'
#' Estimation is carried out by the L-moment algoritms.
#' If not provided the distance between sites is evaluated using the distance
#' between the regularity and timing of the annual flood peaks.
#' Confidence intervals and standard deviation are evaluated by a
#' parametric bootstrap.
#' An empirical matrix of intersite correlation is estimated and used to simulate
#' from a multivariate normal distribution.
#'
#' @import CSHShydRology stats
#' @export
#'
#' @examples
#'
#' \dontrun{
#' ## Assuming that a downloaded version of HYDAT database is available
#' db <- "extdata/Hydat.sqlite3"
#' target <- '01AF009'
#' supreg <- with(gaugedSites, station[supreg_km12 == 11])
#'
#' an <- AmaxData(sites = supreg, target = target, db = db)
#'
#' FloodnetPoolAmax(an, target, period = c(20,50))
#' }
#'
FloodnetPool <-
	function(x, target,
					 period = c(2,5,10,20,50,100),
					 distr = NULL,
					 tol.H = 2,
					 nsim = 2000,
					 alpha = 0.05,
					 corr = NULL,
					 out.model = FALSE,
					 verbose = TRUE){

	if(class(x) == 'peaksdata'){
	  nyear <- x$nyear
		npeak <- x$npeak
		thresh <- x$thresh
		sites <- x$sites
    x <- x$peaks
    type <- 'pot'
    distr <- 'gpa'
    metho <- 'pool_pot'

  } else {
    type <- 'amax'
    metho <- 'pool_amax'
    x <- as.data.frame(x)
  }

	if(!(target %in% x[,1]))
		stop('The target must be in the hydrmetric data')

  ## Transform data to a wide format
	if(type == 'pot'){
    colnames(x) <- c('station','date','value')
    xw <- DataWide(value ~ station, x, row.names = FALSE)

	} else {
		colnames(x) <- c('station','year','value')
    xw <- DataWide(value ~ station + year, x)
	}

  ## make sure the target is the first columns in xw
  cname <- unique(c(target,colnames(xw)))
  xw <- xw[,cname]

 	## Fit a regional model and verified homogeneity
  fit <- FitRegLmom(xw, distr = distr, type = type)

  ## If the distribution is passed, keep.it fixed in the updating process
 	distr.fix <- ifelse(is.null(distr), FALSE, TRUE)

 	## Update the pooling group base on its heterogeneity
 	if(verbose)
  	cat('\n[Updating pooling group]')

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

  ## Evaluate the average intersite-correlation and exceeding probability
  ## if not specified
  if(!is.null(corr)){

  	if(length(corr) != 1)
  		stop('The correlation coefficient must be a scalar.')

  	if(any(corr > 1 | corr < 0))
  		stop('The correlation coefficient must be between 0 and 1.')

  } else if(type == 'amax'){
   	pool.sites <- rownames(fit$lmom)
    suppressWarnings(corr <- Intersite(xw[,pool.sites])$para)
  } else if(type == 'pot'){
    corr <- 0
  }

  ## Compute the exceeding probability
  if(type == 'amax'){
    p <- 1-1/period
  } else if(type == 'pot'){
  	tid <- which(sites == target)
    ppy <- npeak[tid]/nyear[tid]
    u <- thresh[tid]
    p <- 1-1/(ppy*period)
  }


  if(nsim > 1){
    hat <- try(predict(fit, p,  corr = corr, ci = TRUE,
    									 nsim = nsim, alpha = alpha), silent = TRUE)

    if(class(hat) == 'try-error')
    	stop('Model fail to predict flood quantiles.')

    if(type == 'pot')
    	hat[,c(1,3,4)] <- hat[,c(1,3,4)] + u


    ans <- replicate(4,
	    data.frame(site = target,
			  			   method = metho,
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

  } else{
    hat <- predict(fit, p)

    if(type == 'pot')
    	hat <- hat + u

    ans <- data.frame(site = target,
			  	            method = metho,
				  		        distribution = fit$distr,
					  	        period = period,
						          variable = 'quantile',
						          value = hat)
  }


	if(out.model)
		ans <- list(fit = fit, qua = ans)

	return(ans)

}
