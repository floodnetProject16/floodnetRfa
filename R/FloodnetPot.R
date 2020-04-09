#' Estimating flood quantiles using peaks over threshold
#'
#' Return the flood quantiles, standard deviation, lower and upper bounds
#' of the confidence interval based on a Peaks Over Threshold (POT) model.
#'
#' @param x Hydrometric data. Dataset having three columns: site, date, value.
#'
#' @param period Return period for which the flood quantiles are estimated.
#'
#' @param u Threshold value provided by the user.
#'
#' @param area Drainage area. Use to determine the minimal separating time
#'   between flood events. The default value corresponds to 15 days.
#'
#' @param nsim Number of bootstrap samples used for inference.
#'
#' @param level Confidence level.
#'
#' @param tol.year Number of days necessary to consider a year complete. Otherwise the data is removed.
#'
#' @param out.model Logical. Should the model be output. This corresponds to the
#'   output of \link{FitPot}.
#'
#' @param verbose Logical. Should message and warnings be output.
#'
#' @details
#'
#' Estimation is carried out by maximum likelihood using the Generalized
#' Pareto distribution. If not provided, a best candidate threshold is searched.
#' It corresponds to the lowest
#' threshold where the goodness-of-fit test of Anderson-Darling has
#' a p-value greater than 0.25.
#' In addition, the selected threshold is forced to have (in
#' average) fewer than 2.5 peaks per year and at least 30 peaks. If
#' no such threshold is found, the threshold with the maximum p-value is used instead.
#' The declustering technique for extracting the flood peaks
#' follows the recommendations of the Water Ressources council of the United States.
#' In particular, flood peaks must be separated by at least 4 + log(A) days, where
#' A is the drainage area of the basin in kilometers squared.
#' Inference on the estimated flood quantiles is performed by parametric bootstraps.
#'
#' If \code{verbose == TRUE}, a Mann-Kendall test and logistic regression are used
#' to verify the presence of trends in the mean excess and the number of peaks per years.
#' For the logistic regression, a t-test examines the significance of the slope
#' parameter for a linear trend.
#' If the data fails one of the tests at significance level 0.05,
#' a warning is issued.
#'
#' @references
#'
#' Durocher, M., Zadeh, S. M., Burn, D. H., & Ashkar, F. (2018). Comparison of
#'   automatic procedures for selecting flood peaks over threshold based on
#'   goodness-of-fit tests. Hydrological Processes, 0(0).
#'   https://doi.org/10.1002/hyp.13223
#'
#' Durocher, M., Burn, D. H., & Ashkar, F. (2019). Comparison of estimation
#'   methods for a nonstationary index-flood model in flood frequency analysis
#'   using peaks over threshold. Water Resources Research.
#'   https://doi.org/10.1029/2019WR025305
#'
#'
#' @seealso \link{FloodnetAmax}, \link{FloodnetPool}.
#'
#' @import CSHShydRology stats
#' @export
#'
#' @examples
#'
#'  ## Performing the analysis
#'  x <- DemoData('daily')
#'  fit <- FloodnetPot(x, u = 1000, area = 14700)
#'  summary(fit)
#'
FloodnetPot <-
	function(x, u = NULL, area = 59874,
					 period = c(2,5,10,20,50,100),
					 nsim = 2000,
					 level = 0.95,
					 tol.year = 346,
					 out.model = FALSE,
					 verbose = TRUE){

	MINSIM <- 200
	alpha <- 1 - level

	############################################
	## Reading of the data
	############################################

	xd <- as.data.frame(x)

  if(ncol(xd) == 3){
  	site = as.character(xd[1,1])
  	xd <- xd[,2:3]
  }

	colnames(xd) <- c('date','value')

  ## Verify the date format
  if(class(xd$date) != 'Date' )
   	stop('Must be a valid date format.')

	## remove missing
	xd <- na.omit(xd)

	## remove incomplete years
	yy <- format(xd$date,'%Y')
	yy.full <- names(which(tapply(yy,yy, length) > tol.year))
	xd <- xd[yy %in% yy.full,]

  ## Compute the minimum separating time between peaks
	rarea <- 4 + log(area)

	############################################
	## Automatic selection of the threshold
	############################################

	u.auto <- is.null(u)

	if(u.auto){

		if(verbose){
	    cat('\n[Searching for a threshold]\n')
		  bar <- txtProgressBar()
		}

  	## preselect a set of candidate thresholds
	  xs <- sort(unique(xd$value), decreasing = TRUE)

	  ## Compute the p-value of the Anderson Darling test for all candidates
	  xiter <- seq(30, length(xs))
	  umat <- matrix(-1, length(xs), 5)

	  colnames(umat) <- c('u','ppy', 'ad', 'mrl', 'kap')

	  for(ii in xiter){

	  	if(verbose)
	  		setTxtProgressBar(bar, ii /length(xiter))

		  fit0 <- try(FitPot(value~date, xd, u = xs[ii], declust = 'wrc', r = rarea))

		  ## POT fails
      if(class(fit0) != 'fpot'){
        umat[ii,] <- c(xs[ii], 0, 0, 0, 0)

      ## IF less than 30 peaks
      } else if(fit0$nexcess < 30){
        umat[ii,] <- c(xs[ii], fit0$nexcess/fit0$nyear, 0,
        							 fit0$mrl, fit0$estimate[2])

      ## Normal condition perform the AD test
      } else{
        gof0 <- GofTest(fit0)$pvalue
        umat[ii,] <- c(xs[ii], fit0$nexcess/fit0$nyear, gof0,
        							 fit0$mrl, fit0$estimate[2])
      }

		  ## Terminate loops id PPY is greater than 2.5
      if(umat[ii,2] > 2.5)
      	break

	  }

	  ## Select the optimal thresholds
	  umat <- umat[umat[,2] > 0 & umat[,2] <= 2.5, ]

	  ufinal <- which(umat[,3] > .25)

	  if(length(ufinal)>0){
	  	ufinal <- max(ufinal)
	  } else {
	  	ufinal <- max(which(umat[,3] == max(umat[,3])))

	  	if(verbose)
		  	warning('There was no threshold that passed the Anderson-Darling test
		  					with at least a p-value of 0.25.')
	  }

	  u <- umat[ufinal,1]

	}

	############################################
	## Fitting and Performing model checking
	############################################

	fit <- FitPot(value~date, xd, u = u, declust = 'wrc', r = rarea)

	## verify the GOF of the GPA
	ad <- GofTest(fit)$pvalue

	mk <- Kendall::MannKendall(fit$excess)$sl

	## Test for trends in the probability of exceedance
	xbin <- data.frame(y = xd$date %in% fit$time, x = xd$date)
  lg <- .TrendLogis(xbin)

	if(verbose){

	  if(ad < 0.05)
	    warning('The GPA may not be a prober distribution.')

	  if(mk < 0.05)
	    warning('There may be a trend in the mean excess.')

    if(lg < 0.05)
	    warning('There may be a trend in the probability of exceedance.')

	}

	############################################
	## Evaluate flood quantiles
	############################################

	if(nsim >= MINSIM){
    hat <- predict(fit,	period, ci = 'boot', alpha = alpha, nsim = nsim,
								 out.matrix = TRUE)

		para.se <- apply(hat$para, 2, sd)

  	qua <- data.frame(pred = hat$pred[,1],
  											se = apply(hat$qua, 2, sd),
  											hat$pred[,2:3])


	} else {
	  qua <- predict(fit,	period, se = TRUE, ci = 'delta', alpha = alpha)

	  para.se <- sqrt(diag(fit$varcov))

	}

	#
	para <- data.frame(param = fit$estimate, se = para.se)
	rownames(para) <- names(fit$estimate)

	##############################################
	## Pre-compute data for the return level plots
	##############################################
	Fz <- function(z) -log(-log(z))

	## Determine range of values for the plot
	ppy <- fit$nexcess/fit$nyear
	period.p <- 1 - 1 /(ppy * period)
	zmin <- 1/(fit$nexcess+1)
	zmax <- max(fit$nexcess/(fit$nexcess+1), max(period.p))
	rlevel.p <- seq(Fz(zmin),Fz(zmax), len = 100)

	## Convert probabilities to return period
	rlevel.p <- exp(-exp(-rlevel.p))
	rlevel.t <- 1/((1-rlevel.p) * ppy)

	## Evaluate the associated return levels
	rlevel <- cbind(prob = rlevel.p,
									predict(fit, rlevel.t, ci = 'delta', alpha = alpha))

	############################################
	## Build the final output
	############################################

	ans <- list(site = site,
							method = 'pot',
							distr  = 'gpa',
							period = period,
							quantile = qua,
							param = para,
							obs = fit$excess + fit$u,
							time = fit$time,
							rlevels = rlevel,
							thresh = fit$u,
							ppy = fit$nexcess/fit$nyear,
							trend = c(mk, lg),
							gof = ad)

	class(ans) <- 'floodnetMdl'

	if(out.model){
		ans$fit <- fit

		if(u.auto)
			ans$u <- as.data.frame(umat)

	}

	return(ans)

}


## Trend in the exceedance rate
## Verify if the slope of a linear model is significant
.TrendLogis <- function(xbin){
  fit <- glm(y~x, xbin, family = quasibinomial())
  ans <- summary(fit)$coefficient[2,4]
  return(ans)
}
