################################################################################
#' Prediction of flood quantiles at ungauged sites using Region of Influence
#'
#' Return the flood quantiles, root mean square error, relative bias, lower and upper
#' bound of the 95% confidence interval estimated provided with a combination of
#' local regression and kriging technique.
#'
#' @param x Dataset containing the hydrometric data.
#'
#' @param target Descriptors of the target sites. A data.frame where the first
#'   column is the name of the basin.
#'
#' @param target.coord Coordinates of the target sites.
#'
#' @param sites Descriptors of gauged sites. A data.frame where the first
#'   column is the name of the basin.
#'
#' @param sites.coord Coordinates of the gauged sites.
#'
#' @param size Size of the region of influence.
#'
#' @param period Return period to predict. Must be a single value.
#'
#' @param distr Distribution of the gauged sites. Can a common distribution or
#'   a vector of the individual distributions.
#'
#' @param corr Correlation matrix or coefficient that represent
#'   the intersite correlation. If no specified the average coefficient
#'   of correlation is used between for all pairs.
#'
#' @param nsim Number of bootstrap samples.
#'
#' @param out.model Logical. Should the model be returned.
#'
#' @param verbose Should progress and message be display during the procedure.
#'
#' @details
#'
#' The function uses a quantile regression technique (QRT) to predict flood
#' quantiles of a given return period at ungauged sites.
#' The function first perform at-site flood frequency analysis of the annual
#' maximum discharges extracted from the HYDAT database (or provided data).
#' At-site estimates of the flood quantile are evaluated by the L-moments method.
#' The resulting flood quantiles are fed to the QRT model to predict the
#' flood quantiles at ungauged sites according to its basin characteristics.
#' The QRT model uses a local regression method to evaluate flood quantiles at
#' specific target locations.
#' If coordinates are provided, simple kriging is additionally used to improve
#' the prediction by extracting further information
#' from the spatially correlated residuals.
#'
#' If the size of the region of influence (ROI) is not provided,
#' a value is automatically selected.
#' If the dataset include fewer than 30 sites, all sites will beincluded by default,
#' otherwise ROI sizes of 20 and more are tried by steps of 5. The one
#' associated with the lowest mean absolute prediction error
#' (based on leave-one-out cross-validation) is finally selected
#'
#' Please note that the distance between sites is the Euclidean distance and
#' that proper projection of the coordinates is required for input.
#'
#' A mix of parametric bootstraps for the at-site distributions and residual
#' bootstraps for the QRT model is used to evaluate model uncertainty.
#'
#' @references
#'
#' Durocher, M., Burn, D. H., & Mostofi Zadeh, S. (2018). A nationwide regional
#'   flood frequency analysis at ungauged sites using ROI/GLS with copulas and
#'   super regions. Journal of Hydrology, 567, 191–202.
#'   https://doi.org/10.1016/j.jhydrol.2018.10.011
#'
#' Durocher, M., Burn, D. H., Zadeh, S. M., & Ashkar, F. (2019). Estimating
#'   flood quantiles at ungauged sites using nonparametric regression methods
#'   with spatial components. Hydrological Sciences Journal, 64(9), 1056–1070.
#'   https://doi.org/10.1080/02626667.2019.1620952
#'
#' @import CSHShydRology stats utils
#' @export
#'
#' @examples
#'
#' \dontrun{
#' 	## Path the HYDAT database
#'  db <- DB_HYDAT
#'
#'	## Extract catchment descriptors
#'	xd <- with(descriptors,
#'  data.frame(
#'  	site = station,
#'    area = log(area),
#'    map  = log(map_ws),
#'    wb   = log(.01 + wb),
#'    stream = log(.01 + stream),
#'  	elev = elev_ws,
#'  	slope = log(.01 + slope)))
#'
#'	## Put the target site apart
#'	target.id <- (xd$site == '01AF009')
#'
#'  target <- xd[target.id,]
#'  xd <- xd[-target.id,]
#'
#'  ## Fit the model
#'  FloodnetRoi(target = target, sites = xd, db = db,
#'						period = 100, size = 30, nsim = 30)
#'
#' }
#'
FloodnetRoi <- function(
	x,
	sites,
	target,
	sites.coord = NULL,
	target.coord = NULL,
	size = 20,
	period = 100,
	distr = 'gev',
	corr = 0,
	nsim = 0,
	out.model = FALSE,
	verbose = TRUE){


	#########################################
	## Verification
	##########################################

	if(verbose)
   	bar <- txtProgressBar()

	## Reorganize target an sites information
	target <- .ToDF(target)
  sites <- .ToDF(sites)

	## Extract stations names
	sname <- as.character(sites[,1])
	tname <- as.character(target[,1])
	sites <- sites[,-1]
	target <- target[,-1]

	if(any(colnames(sites) != colnames(target)))
    stop('Sites and target should have the same columns names')

  if(any(tname %in% sname))
  	stop('The target cannot not be in the gauged sites')

	## Scale the descriptors
	sites <- scale(sites)
	target <- scale(target, center = attr(sites, 'scaled:center'),
				          scale = attr(sites, 'scaled:scale'))

	## Verify that kriging was asked.
	do.krig <- ifelse(all(!is.null(target.coord),
												!is.null(sites.coord)), TRUE, FALSE)

	## Verify that sites ID match
	if(do.krig){

		msg <- 'The sites ID in the input data and the coord data do not match.'

		target.coord <- .ToDF(target.coord)
    sites.coord <- .ToDF(sites.coord)

		if(nrow(sites) != nrow(sites.coord))
      stop(msg)

		if(nrow(target) != nrow(target.coord))
			stop(msg)
	}

	if(length(distr) == 1)
		distr <- rep(distr, nrow(sites))

	if(length(as.numeric(period)) != 1)
		stop("Only one return period must be passed")

	period.p <- 1-1/period

	##################################################
	## Verify the hydrometric data
	##################################################

	## Remove missing from hydrometric data
  an <- na.omit(as.data.frame(x))

	## Order and Filter
  an <- an[order(an[,1], an[,2]), ]
  an <- an[an[,1] %in% sname, ]

  if(ncol(an) != 3)
   	stop('Wrong number of columns in the input')

  ## Reformat column names
  colnames(an) <- c('site','date','value')

  if(!methods::is(an$date,'Date'))
    	stop('The second column of x must be a Date')

  an$year <- as.integer(format(an$date, '%Y'))

	###########################################
	## Estimate at-site flood quantiles
	##########################################

  ## Evaluate the at-site flood quantile
  an.lst <- split(an$value, an$site)
  an.para <- mapply(fAmax, an.lst, distr, SIMPLIFY = FALSE)
  an.qua <- mapply(qAmax, an.para, distr,
  								 MoreArgs = list(p = period.p))

  #####################################
  ## Organize the input to use formulas
  #####################################

  xsite <- data.frame(y = log(an.qua), phy = sites)
  xtarget <- data.frame(y = 0, phy = target)

  phy.name <- colnames(xsite)[seq(2,ncol(xsite))]
  phy.form <- as.formula(paste0('y~', paste(phy.name, collapse = '+')))

  sim.form <- as.formula(paste0('~', paste(phy.name, collapse = '+')))

  if(do.krig){
    xsite <- data.frame(xsite, krig = sites.coord)
   	xtarget <- data.frame(xtarget, krig = target.coord)

   	krig.name <- colnames(xsite)[seq(2+length(phy.name),ncol(xsite))]
    krig.form <- as.formula(paste0('~', paste(krig.name, collapse = '+')))
  }

  #################################################################
  ## Automatically select the number of sites in the pooling groups
  #################################################################

  do.cv <- length(size) > 1
  if(do.cv){

  	if(verbose)
		  cat('\n[Finding the ROI size]\n')


    if(do.krig){
      suppressWarnings(
  	  cv <- CvRoi(x = xsite, phy = phy.form, similarity = sim.form,
  		  					kriging = krig.form, model = 'Exp',
  		  					nk = size, fold = 10, verbose = FALSE))
    } else {
    	suppressWarnings(
      cv <- CvRoi(x = xsite, phy = phy.form, similarity = sim.form,
      						nk = size, fold = 10, verbose = FALSE))
    }

    size <- cv$nk[which.min(cv$mad)]

  }

  ####################
  ## Fitting the model
  ####################

  if(do.krig){
  	fit <- suppressWarnings(FitRoi(x = xsite, xnew = xtarget, nk = size,
  							phy = phy.form, similarity = sim.form, kriging = krig.form))

  } else{
    fit <- suppressWarnings(FitRoi(x = xsite, xnew = xtarget, nk = size,
  							phy = phy.form, similarity = sim.form))
  }

  ##############################################
  # Bootstrapping
  ###############################################

  if(nsim > 1){

  	if(verbose)
		  cat('\n[Bootstrapping]\n')

    ## Verify the input intersite correlation
    if(any(corr > 1 | corr < 0))
  		  stop('The correlation coefficient must be between 0 and 1.')

  	if(all(corr == 0)){
  	  nocorr <- TRUE

  	} else {

  		if(length(corr) == 1){
    	  corr <- matrix(corr, nrow(sites), nrow(sites))
    	  diag(corr) <- 1
  		}

    	demi <- chol(corr)
    	nocorr <- FALSE
    }

    ## At-site info for simulation
    an.size <- sapply(an.lst,length)
    an.nsite <- nrow(sites)
    an.nmax <- max(an.size)

    ## allocate memory
    xsite0 <- xsite
    qboot <- matrix(0,nsim, nrow(target))

    ## Make sure that nsim is even
    if(nsim %% 2 != 0)
    	nsim <- nsim + 1

    res <- residuals(fit, xsite, fold = 10)

    ## Use a balance bootstrap to sample the residuals
    ## It also assumed a symmetric distribution.
    res.boot <- split(sample(rep(c(res,-res),nsim/2)),
    										rep(1:nsim,length(res)))

    for(ii in 1:nsim){

    	if(verbose)
    		setTxtProgressBar(bar, ii/nsim)

      ##------- At site simulation ----------##
      if(nocorr){
        ## Simulate directly uniform variable
        u <- lapply(an.size, runif)
      } else {
        ## Simulate from multivariate normal
        z <- matrix(rnorm(an.nsite * an.nmax), an.nsite, an.nmax)
        z <- pnorm(crossprod(demi, z))
        u <- lapply(1:an.nsite, function(jj) z[jj, 1:an.size[jj]])
      }

      oo <- mapply(qAmax, u, an.para, distr)
      pp <- mapply(fAmax, oo, distr, SIMPLIFY = FALSE)
      qq <- log(mapply(qAmax, pp, distr, MoreArgs = list(p = period.p)))
      xsite0$y <- qq + res.boot[[ii]]

      ## Fit the model
      if(do.krig){
  	    ff <- suppressWarnings(FitRoi(x = xsite0, xnew = xtarget, nk = size,
  							phy = phy.form, similarity = sim.form, kriging = krig.form))

      } else{
        ff <- suppressWarnings(FitRoi(x = xsite0, xnew = xtarget, nk = size,
  							phy = phy.form, similarity = sim.form))
      }

      qboot[ii,] <- ff$pred
    }

    qboot <- exp(qboot)

    qua <- data.frame(quantile = exp(fit$pred),
    						 se = apply(qboot, 2, sd),
    						 lower = apply(qboot, 2, quantile, 0.025),
    						 upper = apply(qboot, 2, quantile, 0.975))

  } else {
    qua <- data.frame(quantile = 	exp(fit$pred))
  }

  rownames(qua) <- tname

	xdata <- data.frame(atsite = an.qua, pred = exp(predict(fit, xsite, 10)))
	rownames(xdata) <- sname

	ans <- list(site = tname, method = 'qrt', period = period,
							quantile = qua, size = size, data = xdata)

	class(ans) <- 'floodnetRoi'

	if(do.cv){
		cv <- cv[ ,c(1,4,7)]
		colnames(cv) <- c('size','nsh','skill')
		ans$cv <- cv
	}

	if(out.model)
		ans$fit <- fit

	return(ans)
}

## Function that transform different format into a data.frame
.ToDF <- function(x){

	if(is.data.frame(x))
		ans <- x
	else if(is.vector(x))
		ans <- as.data.frame(as.list(x))
	else
		ans <- as.data.frame(x)

	return(ans)
}


