#' @import CSHShydRology stats utils
#' @export
#' @rdname FloodnetPool
FloodnetPoolMle <- function(x, target,
					 period = c(2,5,10,20,50,100),
					 distr,
					 type = 'mean',
					 nsim = 2000,
					 level = 0.95,
					 corr = NULL,
					 out.model = FALSE,
					 verbose = TRUE){

	alpha <- 1 - level

	if(class(x) == 'peaksdata'){
	  nyear <- x$nyear
		npeak <- x$npeak
		thresh <- x$thresh
		sites <- x$sites
    x <- x$peaks
    model <- 'pot'
    distr <- 'gpa'

  } else {
    model <- 'amax'
    x <- as.data.frame(x)
  }

	metho <- paste('pool',model, type, sep = '_')

	if(!(target %in% x[,1]))
		stop('The target must be in the hydrmetric data')

  ## Transform data to a wide format
	if(model == 'pot'){
    colnames(x) <- c('station','date','value')
    xw <- DataWide(value ~ station, x, row.names = FALSE)

	} else if(model == 'amax'){
		colnames(x) <- c('station','year','value')
    xw <- DataWide(value ~ station + year, x)
	}

	## Make sure that the target is the first columns
	xw <- xw[,unique(c(target,colnames(xw)))]

	## Fit a distribution for each margin
	margin <- FitPoolMargin(xw, distr = distr)

	## Compute the exceeding probability associated with the return period
	if(model == 'amax'){
		p <- 1-1/period
	} else if(model == 'pot'){
		tid <- which(sites == target)
    ppy <- npeak[tid]/nyear[tid]
    u <- thresh[tid]
	  p <- 1-1/(ppy*period)
	}

	if(type == 'shape' & model == 'amax')
  	GetIndx <- function(z) z$index[,1]
  else
  	GetIndx <- function(z) z$index[1]

	## Fit the model
  fit <- FitPoolMle(xw, distr = distr, type = type)
  hat <- predict(fit, p, index = GetIndx(fit))

  if(model == 'pot')
    	hat <- hat + u

  ## Evaluate the average intersite-correlation
  if(!is.null(corr)){

  	if(length(corr) != 1)
  		stop('The correlation coefficient must be a scalar.')

  	if(any(corr > 1 | corr < 0))
  		stop('The correlation coefficient must be between 0 and 1.')

  } else if(model == 'amax'){
      suppressWarnings(corr <- Intersite(xw)$para)
  } else if(model == 'pot'){
      corr <- 0
  }


  ## Bootstrap
  if(nsim > 1){

  	qboot <- matrix(0, nsim, length(hat))
    bar <- txtProgressBar()

    for(ii in 1:nsim){

      if(verbose)
    	  setTxtProgressBar(bar, ii/nsim)

  	  sim <- simulate(margin, corr = corr[1])
  	  f <- FitPoolMle(sim, distr = distr, type = type)
      qboot[ii,] <- predict(f, p = p, index = GetIndx(f))
    }

    if(model == 'pot')
      qboot <- qboot + u

    ans <- replicate(4,
	    data.frame(site = target,
			  			   method = metho,
				  		   distribution = distr,
					  	   period = period,
						     variable = 'quantile',
						     value = hat),
	    simplify = FALSE)

	  ans[[2]]$variable <- 'se'
	  ans[[3]]$variable <- 'lower'
	  ans[[4]]$variable <- 'upper'

	  ans[[2]]$value <- apply(qboot, 2, sd)
	  ans[[3]]$value <- apply(qboot, 2, quantile, alpha/2)
	  ans[[4]]$value <- apply(qboot, 2, quantile, 1-alpha/2)

	  ans <- do.call(rbind,ans)
	  rownames(ans) <- NULL

	} else{

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
