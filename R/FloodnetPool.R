#' Pool frequency analysis using annual maximum floods
#'
#' Return the results of a regional flood frequency analysis based on annual
#' maxima and pooling groups.
#'
#' @param x Hydrometric data of the form: site, year, value.
#'
#' @param target site ID of the target.
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
#' @param level Confidence level.
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
#' Estimation is carried out by the L-moment algorithms.
#' See \link{FitRegLmom} and \link{PoolRemove} for more details.
#' If not provided the distance between sites is evaluated using the distance
#' between the regularity and timing of the annual flood peaks.
#' Confidence intervals and standard deviation are evaluated by a
#' parametric bootstrap.
#' An intersite correlation is estimated and used to simulate
#' from a multivariate normal distribution.
#'
#' @seealso \link{FitRegLmom}, \link{PoolRemove}.
#'
#' @import CSHShydRology stats
#' @export
#'
#' @examples
#'
#' \dontrun{
#'	## Path the HYDAT database
#'  db <- DB_HYDAT
#'
#'  ## Compute distance
#'  coord <- gaugedSites[,c('lon','lat')]
#'  rownames(coord) <- gaugedSites$station
#'
#'  ## Read Amax data
#'  atl <- with(gaugedSites, station[substr(station,1,2) == '01'])
#'  x <- AmaxData(atl, db, target = '01AF009', size = 15)
#'
#'  ## Performing AMAX analysis using L-moments
#'  FloodnetPool(x, '01AF009', distr = 'gev', period = c(20,50), nsim = 30)
#'
#'  ## Read Pot data
#'  info <- gaugedSites[, c('station','auto','area')]
#'  xd <- DailyPeaksData(info, db, target = '01AF009',
#'  							size = 15, distance = dist(coord))
#'
#'  ## Performing POT analysis using L-moments
#'  FloodnetPool(xd, '01AF009', period = c(20,50), nsim = 30)
#' }
#'
FloodnetPool <-
	function(x, target,
					 period = c(2,5,10,20,50,100),
					 distr = NULL,
					 tol.H = 2,
					 nsim = 2000,
					 level = 0.95,
					 corr = NULL,
					 out.model = FALSE,
					 verbose = TRUE){

	alpha <- 1 - level

	dtype <- attr(x,'dtype')
	dtype <- ifelse(is.null(dtype), '', dtype)

	#############################################################
	## Verify the type of input and transform the data in a matrix
	###############################################################

	if(dtype == 'peaksdata'){
		thresh <- attr(x,'meta')$thresh
		ppy <- attr(x,'meta')$ppy
		sites <- rownames(attr(x,'meta'))
    type <- 'pot'
    distr <- 'gpa'

  } else {
    type <- 'amax'
    x <- as.data.frame(x)
    thresh <- 0
    ppy <- 1
  }

	if(!(target %in% as.character(x[,1])))
		stop('The target must be in the hydrmetric data')

  ## Transform data to a wide format
 if(type == 'amax') {
		x$date <- as.integer(format(x$date, '%Y'))
    xw <- DataWide(value ~ site + date, x)
 } else {
    xw <- DataWide(value ~ site, x, row.names = FALSE)
	}

  ## make sure the target is the first columns in xw
  cname <- unique(c(target,colnames(xw)))
  xw <- xw[,cname]

  ###############################################
  ## Fit the regional model
  ################################################

 	## Fit a regional model and verified homogeneity
  fit <- FitRegLmom(xw, distr = distr, type = type)

  ## If the distribution is passed, keep.it fixed in the updating process
 	distr.fix <- ifelse(is.null(distr), FALSE, TRUE)

 	## Update the pooling group base on its heterogeneity
 	if(verbose)
  	cat('\n[Updating pooling group]\n')

  suppressWarnings(
  	fit <- PoolRemove(fit, verbose = verbose, nmin = 1, tol = tol.H,
  										ntot.min = 5 * max(period),  distr.fix = distr.fix))

  if(verbose){

  	## Verify heterogenous measure H
  	het <- fit$stat[1]

  	if(het > 2)
  		warning('The heterogenous measure for the pooling group is H > 2.')

  }

  ## Evaluate the average intersite-correlation and exceeding probability
  ## if not specified
  if(!is.null(corr)){

  	if(length(corr) != 1)
  		stop('The correlation coefficient must be a scalar.')

  	if(any(corr > 1 | corr < 0))
  		stop('The correlation coefficient must be between 0 and 1.')

  } else if(type == 'amax'){
   	xw <- xw[, rownames(fit$lmom)]
   	nn <- crossprod(!is.na(xw))
  	nn <- nn[lower.tri(nn)]
  	cc <- cor(xw, use = 'pairwise.complete.obs')
  	cc <- cc[lower.tri(cc)]
    corr <- weighted.mean(cc, w = nn, na.rm = TRUE)

  } else if(type == 'pot'){
    corr <- 0
  }

  ## Compute the exceeding probability
  if(type == 'amax'){
    period.p <- 1-1/period
  } else if(type == 'pot'){
  	tid <- which(sites == target)
    ppy <- ppy[tid]
    u <- thresh[tid]
    period.p <- 1 - 1 / (ppy * period)
  }

  #########################################################
  ## Predict flood quantiles
  #########################################################

	## Include more return period for the return level plots
  obs <- sort(as.numeric(stats::na.omit(xw[,1])))
	nobs <- length(obs)

	if(type == 'pot')
		obs <- obs + u

	Fz <- function(z) -log(-log(z))
	zmin <- 1/(nobs+1)
	zmax <- max(nobs/(nobs+1), max(period.p))

	rlevel.p <- seq(Fz(zmin),Fz(zmax), len = 50)
	rlevel.p <- exp(-exp(-rlevel.p))

	prob <- unique(c(period.p, rlevel.p))

	## Predict all flood quantile
	boot <- suppressWarnings(try(
  	predict(fit, prob,  corr = corr, ci = TRUE, nsim = nsim,
  					alpha = alpha, out.matrix = TRUE),
  	silent = TRUE ))

	 if(is(hat,'try-error'))
  	 stop('Model fail to predict flood quantiles.')

	hat <- boot$pred

	## Compute the standard error of the model parameters.
	para.se <- apply(boot$para, 2, sd)

	# split selected return period from return level plot
	op <- order(prob)
	rlevel<- cbind(prob = prob[op], hat[op, -2])
	hat <- hat[seq_along(period.p),]

	## Smooth the boundary of the return level that are obtained by simulation
	rlevel$lower <- fitted(lm(lower ~ poly(Fz(prob), 10), rlevel))
	rlevel$upper <- fitted(lm(upper ~ poly(Fz(prob), 10), rlevel))

	## Add back the threshold to the POT data
  if(type == 'pot'){
   	hat[,c(1,3,4)] <- hat[,c(1,3,4)] + u
		rlevel[,2:4] <- rlevel[,2:4] + u
  }

	############################################
	## Build the final output
	############################################

	para0 <- c(fit$lmom[1,1],fit$para)
	names(para0) <- c('IF', names(fit$para))

	ans <- list(site = target,
							quantile = hat,
							param = data.frame(param = para0, se = para.se),
							method = paste0('pool_',type),
							distr  = fit$distr,
							thresh = thresh[1],
							ppy = ppy[1],
							period = period,
							lmom = data.frame(nrec = fit$nrec, fit$lmom),
							rlevels = rlevel,
							obs = obs)

	class(ans) <- 'floodnetMdl'

	if(out.model)
		ans$fit <- fit

	return(ans)

}
