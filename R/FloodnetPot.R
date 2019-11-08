#' Estimate flood quantiles using using peaks over threshold
#'
#' Return the flood quantiles estimated by (at-site) Peaks Over Threshold (POT).
#' Estimation is carried out by maximum likelihood using the Generalized
#' Pareto distribution. If not provided, a best candiate threshold is searched.
#' Apart some restrictions, it corresponds to the lowest
#' threshold that passes the goodness of fit test of Anderson-Darling with
#' a p-value greater that 0.25.
#' In addition, the selected threshold is forced to have in
#' average less than 2.5 peaks per year and at least 30 peaks. If
#' no threshold is associated to p-value greater that 0.25, the threshold
#' with the maximimum p-value is used instead.
#' The declustering technique for extracting the independend peaks
#' follows the recommendations of the Water Ressources council of United States.
#' In particular, peaks must be separated by at least 4 + log(A) days, where
#' A is the drainage area of the basin in square kilometers.
#' Inference on the estimated flood quantiles is performed by parametric bootstrap.
#'
#' @param period Return period for which the flood quantiles are estimated.
#'
#' @param x Path of a CSV file or data.frame.
#'
#' @param site Station ID for HYDAT or a station name for identification.
#'
#' @param db File name of the HYDAT database.
#'
#' @param u Threshold value provided by the user.
#'
#' @param area Drainage area.
#'
#' @param nsim Number of bootstrap samples used for inference.
#'
#' @param alpha Probability outside the confidence interval.
#'
#' @param tol.year Number of days necessary to consider a year complete.
#'
#' @param out.model Logical. Should the model be output. This correspond to the
#'   output of \link{FitPot}. Otherwise only the estimated flood quantiles are
#'   returned.
#'
#' @param verbose Logical. Should message and warnings be output.
#'
#' @export
#'
#' @import CSHShydRology
#'
#' @details
#'
#' If the HYDAT database is used as input, the the drainage area
#' is extracted from the database. If no drainage area is provided,
#' its value is approximated by a log-log relationship with mean river
#' discharge. This relationship was estimated using the 1114 sites found in the
#' dataset `gaugedSites`.
#' It has the form $log(A) <- 4.0934 + 0.9944 * log(M)$, where M is the mean
#' streamflow.
#'
#' For the CSV file, there must be at least two columns: Date and Value. The
#' names of the variables are not case sensitive. On the website of the
#' Water Survey of Canada, it is suggested to download the format
#' `Daily Data: Date-Data Format`.
#'
#' @examples
#'
#' \dontrun{
#' ## Assuming that the HYDAT database was already downloaded
#' db <- "/pathToDB/HYDAT.sqlite"
#' FloodnetPot(period = c(20,50), site = '01AD003', db = db)
#' }
#'
FloodnetPot <-
	function(period = c(2,5,10,20,50,100),
					 x = NULL,
					 site = NULL,
					 db = NULL,
					 u = NULL,
					 area = NULL,
					 nsim = 2000,
					 alpha = 0.05,
					 tol.year = 346,
					 out.model = FALSE,
					 verbose = TRUE){


	############################################
	## Reading of the data
	############################################

	if(is.null(x) & is.null(db))
		stop('Must provide an input data.')

  if(!is.null(db)){

		if(verbose)
	    cat('\n[Reading the HYDAT database]')

		## open a connection to the database
	  con <- RSQLite::dbConnect(RSQLite::SQLite(), db)

	  ## Extract the daily data
	  xd <- HYDAT::DailyHydrometricData(con, get_flow = TRUE, site)[,2:3]

	  if(is.null(area)){
	  	area <- HYDAT::StationMetadata(con, site)$drainage_area_gross

	  	if(!is.finite(area))
	  		area <- NULL
	  }

    RSQLite::dbDisconnect(con)

	## USING CSV file or data.frame
	} else {

		xd <- as.data.frame(x)
    colnames(xd) <- c('date','value')

    ## If the user have not specify a site id
    if(is.null(site))
      site <- 'site'

    ## Verify the date format
    dd <- as.Date(xd$date[1])
    yy <- as.integer(format(as.Date(as.character(xd$date[1])),'%Y'))

    ## if ddmmyy
    if(yy < 32){
    	xd$date <- as.Date(xd$date,
    										 tryFormats = c("%d-%m-%Y", "%d-%b-%Y","%d/%m/%Y"))
    ## or yymmdd
   	} else{
    	xd$date <- as.Date(xd$date,
    										 tryFormats = c("%Y-%m-%d", "%Y-%b-%d","%Y/%m/%d"))
   	}

  }

	## remove missing
	xd <- na.omit(xd)

	## remove incomplete years
	yy <- format(xd$date,'%Y')
	yy.full <- names(which(tapply(yy,yy, length) > tol.year))
	xd <- xd[yy %in% yy.full,]

	## If needed, affect a default drainage area based on log-log relationship
	## with average river discharge
	if(is.null(area)){
	  area <- exp(4.0934 + 0.9944 * log(mean(xd$value, na.rm = TRUE)) )

	  if(verbose)
		 	warning('The drainage area was approximated using the mean ',
		 					'river discharge')
	}

  ## Compute the minimum separating time between peaks
	rarea <- 4 + log(area)

	############################################
	## Automatic selection of the threshold
	############################################

	if(is.null(u)){

		if(verbose)
	    cat('\n[Searching for a threshold]')

  	## preselect a set of candidate thresholds
	  xs <- sort(unique(xd$value), decreasing = TRUE)

	  ## Compute the p-value of the Anderson Darling test for all candidates
	  xiter <- seq(30, length(xs))
	  umat <- matrix(-1, length(xs), 3)
	  for(ii in xiter){

		  fit0 <- try(FitPot(value~date, xd, u = xs[ii], declust = 'wrc', r = rarea))

		  ## POT fails
      if(class(fit0) != 'fpot'){
        umat[ii,] <- c(xs[ii], 0, 0)

      ## IF less than 30 peaks
      } else if(fit0$nexcess < 30){
        umat[ii,] <- c(xs[ii], fit0$nexcess/fit0$nyear, 0)

      ## Normal condition perform the AD test
      } else{
        gof0 <- GofTest(fit0)$pvalue
        umat[ii,] <- c(xs[ii], fit0$nexcess/fit0$nyear, gof0)
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
	## Fitting and prediction
	############################################

	if(verbose)
	    cat('\n[Fitting the POT model with u =', round(u,1), ']')

	fit <- FitPot(value~date, xd, u = u, declust = 'wrc', r = rarea)

	############################################
	## Perform model checking
	############################################

	if(verbose){

		## verify the GOF of the GPA
	  ad <- GofTest(fit)$pvalue

	  if(ad < 0.05)
	    warning('The GPA may not be a prober distribution.')

	  mk <- Kendall::MannKendall(fit$excess)$sl

	  if(mk < 0.05)
	    warning('There may be a trend in the mean excess.')

	  ## Test for trends in the probability of exceedance
	  xbin <- data.frame(y = xd$date %in% fit$time, x = xd$date)
    lg <- .TrendLogis(xbin)

    if(lg < 0.05)
	    warning('There may be a trend in the probability of exceedance.')

	}

	############################################
	## Evaluate flood quantiles
	############################################

	if(verbose)
	  cat('\n[Estimating the flood quantiles (bootstrap)]\n')

  hat <- predict(fit,	period, ci = 'boot', alpha = alpha, nsim = nsim,
								 out.matrix = TRUE)

	ans <-
		replicate(4,
	    data.frame(site = site,
			  			   method = 'pot',
				  		   distribution = 'gpa',
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


## Verify using a F-test that trend model in exceedance probability
## is better than the constant model
.TrendLogis <- function(xbin){

  ## Constant model
  fit0 <- glm(y~1, xbin, family = quasibinomial())

  ## alternative models
  fit <- vector('list', 4)
  fit[[1]] <- glm(y~x, xbin, family = quasibinomial())
  fit[[2]] <- glm(y~poly(x,2), xbin, family = quasibinomial())
  fit[[3]] <- glm(y~poly(x,3), xbin, family = quasibinomial())

  fit[[4]] <- glm(y~splines::ns(x,4), xbin, family = quasibinomial())

  ## Evaluate p-values of the F-test
  fun <- function(z) anova(fit0, z, test = 'F')[2,6]

  ## Return minimal p-value
  return(min(sapply(fit,fun)))
}
